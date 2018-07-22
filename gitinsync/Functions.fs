module Functions

open LibGit2Sharp
open Types
open System.IO

let valueOrZero (n: System.Nullable<int>) = 
   if n.HasValue then n.Value else 0

let folderName (repo: Repository) = 
    (new DirectoryInfo (repo.Info.WorkingDirectory)).Name

let withRepository folder f =
    try
        use repo = new Repository(folder)
        f repo
    with
        | :? RepositoryNotFoundException -> Error { Directory=folder; BranchName=None; Message="Not a git repository"; Comparison=None }

let trueOrError exp dir bn errorMsg =
    match exp with
    | false -> Error { Directory=dir; BranchName=bn; Message=errorMsg; Comparison=None }
    | true -> Ok ()

let ensureRepositoryHasOriginRemote (repo: Repository) =
    trueOrError (repo.Network.Remotes.["origin"] <> null) (folderName repo) (Some repo.Head.FriendlyName) "No origin remote"

let ensureRepositoryRemoteHostIsCorrect remoteMustMatch (repo: Repository) =
    trueOrError (repo.Network.Remotes.["origin"].Url.Contains(remoteMustMatch)) (folderName repo) (Some repo.Head.FriendlyName) "Not checked (external)"

let ensureRepositoryTracksOriginRemote (repo: Repository) =
    trueOrError (repo.Head.IsTracking && repo.Head.RemoteName = "origin") (folderName repo) (Some repo.Head.FriendlyName) "Not tracking origin remote"

let ensureChangesAreCommitted (repo: Repository) =
    trueOrError (not (repo.RetrieveStatus().IsDirty)) (folderName repo) (Some repo.Head.FriendlyName) "UNCOMMITTED"

let fetchChanges credentials dir (repo: Repository) =
    let remote = repo.Network.Remotes.["origin"]
    let branch = repo.Head
    let refSpecs = remote.FetchRefSpecs |> Seq.map (fun rs -> rs.Specification)
    let fetchOptions = FetchOptions (CredentialsProvider=credentials)
    try
        Commands.Fetch (repo, "origin", refSpecs, fetchOptions, null)
        Ok ()
    with
        | :? LibGit2SharpException as e ->
            match e.Message with
            | m when m.Contains("401") || m.Contains("replays") -> Error { Directory=dir; BranchName=Some branch.FriendlyName; Message="Incorrect credentials"; Comparison=None }
            | m when m.Contains("not found") -> Error { Directory=dir; BranchName=Some branch.FriendlyName; Message="Remote not found or no access"; Comparison=None }
            | m when m.Contains("timed out") -> Error { Directory=dir; BranchName=Some branch.FriendlyName; Message="Timed out accessing remote"; Comparison=None }
            | _ -> Error { Directory=dir; BranchName=Some branch.FriendlyName; Message=e.Message; Comparison=None }

let getBranchDifferences (dir: string) (branch: Branch) =
    let branchName = branch.FriendlyName
    let aheadBy = branch.TrackingDetails.AheadBy |> valueOrZero
    let behindBy = branch.TrackingDetails.BehindBy |> valueOrZero

    let status = if (aheadBy > 0 && behindBy > 0) then "MERGE REQUIRED"
                 else if aheadBy > 0 then "PUSH REQUIRED"
                 else "OK";

    { Directory=dir; Status=status; BranchName=branchName; AheadOfRemoteBy=aheadBy; BehindRemoteBy=behindBy }

let getTrackingBranchDifferences credentials folder =
    let fetchAndGetDifferences (repo: Repository) =
        let dir = folderName repo
        let branch = repo.Head
        let fetchResult = repo |> (fetchChanges credentials dir)
        match fetchResult with
        | Error e ->
            let comparison = { Directory=dir; Status=e.Message; BranchName=branch.FriendlyName; AheadOfRemoteBy=0; BehindRemoteBy=0 }
            Error { Directory=dir; BranchName=Some branch.FriendlyName; Message=e.Message; Comparison=Some(comparison) }
        | Ok _ -> Ok (getBranchDifferences dir branch)
    fetchAndGetDifferences |> withRepository folder

let ensureLocalRepositoryIsValid remoteMustMatch folder =
    let withRepository' = withRepository folder
    let ensureRepositoryRemoteHostIsCorrect' = ensureRepositoryRemoteHostIsCorrect remoteMustMatch
    result {
        do! ensureRepositoryHasOriginRemote |> withRepository'
        do! ensureRepositoryRemoteHostIsCorrect' |> withRepository'
        do! ensureRepositoryTracksOriginRemote |> withRepository'
        do! ensureChangesAreCommitted |> withRepository'
        return ()
    }
