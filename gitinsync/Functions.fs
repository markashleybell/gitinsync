module Functions

open LibGit2Sharp
open Types
open System.IO
open System
open System.Collections.Generic

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

let trueOrError repo exp errorMsg =
    let dir = folderName repo 
    let bn = Some repo.Head.FriendlyName
    match (exp repo) with
    | false -> Error { Directory=dir; BranchName=bn; Message=errorMsg; Comparison=None }
    | true -> Ok ()

let ensureRepositoryHasOriginRemote (repo: Repository) =
    trueOrError repo (fun r -> r.Network.Remotes.["origin"] <> null) "No origin remote"

let ensureRepositoryRemoteHostIsCorrect remoteMustMatch (repo: Repository) =
    trueOrError repo (fun r -> r.Network.Remotes.["origin"].Url.Contains(remoteMustMatch)) "Not checked (external)"

let ensureRepositoryTracksOriginRemote (repo: Repository) =
    trueOrError repo (fun r -> r.Head.IsTracking && repo.Head.RemoteName = "origin") "Not tracking origin remote"

let ensureChangesAreCommitted (repo: Repository) =
    trueOrError repo (fun r -> not (r.RetrieveStatus().IsDirty)) "UNCOMMITTED"

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
            | m when m.Contains("404") -> Error { Directory=dir; BranchName=Some branch.FriendlyName; Message="Remote not found/no access"; Comparison=None }
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

let isNotIgnored ignores path =
    not (ignores |> List.contains path)

let isGitRepository path =
    Directory.Exists(path + @"\.git")

let findGitRepositories (ignores: string list) path =
    let subFolders = (new DirectoryInfo(path)).EnumerateDirectories()

    let isGitRepositoryAndNotIgnored (di: DirectoryInfo) = 
        di.Name |> isNotIgnored ignores && di.FullName |> isGitRepository

    subFolders
    |> Seq.filter isGitRepositoryAndNotIgnored
    |> Seq.map (fun di -> di.FullName)

let getRepositoryDifferences remoteMustMatch getTrackingBranchDifferences callback path =
    result {
        callback path
        do! ensureLocalRepositoryIsValid remoteMustMatch path
        let! diff = path |> getTrackingBranchDifferences
        return diff
    }

let parseConfigLine (line: string) =
    line.Split(':') |> (fun a -> (a.[0].Trim(), a.[1].Trim()))

let tryLoadConfig path =
    try
        let settings = 
            File.ReadAllLines(path) 
            |> Array.map parseConfigLine 
            |> dict

        let ignores = 
            match settings.ContainsKey("ignores") with
            | true -> settings.["ignores"].Split('|') |> List.ofArray
            | false -> []

        Ok { 
            GitUsername = settings.["username"]
            GitPassword = settings.["password"]
            RemoteMustMatch = settings.["remotemustmatch"]
            Ignores = ignores
        }
    with
        | :? DirectoryNotFoundException -> Error (sprintf "Directory %s does not exist" path)
        | :? FileNotFoundException -> Error (sprintf "Configuration file not found at %s" path)
        | :? IndexOutOfRangeException -> Error (sprintf "Configuration parse error in %s" path)
        | :? KeyNotFoundException as ex -> Error (sprintf "Configuration key missing from %s" path)


let behind c =
    match c.BehindRemoteBy > 0 with 
    | true -> [sprintf "%d to pull" c.BehindRemoteBy] 
    | false -> []

let ahead c =
    match c.AheadOfRemoteBy > 0 with 
    | true -> [sprintf "%d to push" c.AheadOfRemoteBy] 
    | false -> []

let formatInfo c = 
    String.concat ", " ((ahead c) @ (behind c))

let createComparisonRow (c: LocalBranchComparison) = 
    {
        Status = c.Status
        Repository = c.Directory
        Branch = c.BranchName
        Info = formatInfo c
    }

let createErrorRow (e: PipelineError) = 
    {
        Status = e.Message
        Repository = e.Directory
        Branch = match e.BranchName with
                    | Some n -> n
                    | None -> ""
        Info = ""
    }

let outputResult r = 
    match r with
    | Ok d -> createComparisonRow d
    | Error e -> match e.Comparison with
                    | Some d -> createComparisonRow d
                    | None -> createErrorRow e
