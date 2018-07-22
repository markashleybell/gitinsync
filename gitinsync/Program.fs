open Functions
open LibGit2Sharp
open LibGit2Sharp.Handlers
open System.IO
open Types
open System.Configuration
open System.Collections.Specialized
open ConsoleTables
open System

let isNotIgnored ignores path =
    not (ignores |> List.contains path)

let isGitRepository path =
    Directory.Exists(path + @"\.git")

let findGitRepositories (ignores: string list) path =
    let subFolders = (new DirectoryInfo(path)).EnumerateDirectories()

    let isGitRepositoryAndNotIgnored (di: DirectoryInfo) = 
        di.FullName |> isNotIgnored ignores && di.FullName |> isGitRepository

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

let createConfig (nvc: NameValueCollection) =
    { 
        GitUsername = nvc.["GitUsername"]
        GitPassword = nvc.["GitPassword"]
        RemoteMustMatch = nvc.["RemoteMustMatch"]
    }

[<EntryPoint>]
let main argv = 
    let ignores = []

    let spacesToFillLine (lineContents: string) = 
        let num = Console.BufferWidth - lineContents.Length - 1
        String.concat "" [ for i in 1 .. num -> " " ]

    let cfg = createConfig ConfigurationManager.AppSettings

    let credentials = CredentialsHandler((fun _ _ _ ->
        UsernamePasswordCredentials(Username=cfg.GitUsername, Password=cfg.GitPassword) :> Credentials))
    
    let reportProgress s =
        let msg = sprintf "Checking %s" s
        Console.SetCursorPosition(0, Console.CursorTop)
        Console.Write (sprintf "%s%s" msg (spacesToFillLine msg))

    let getTrackingBranchDifferences' = getTrackingBranchDifferences credentials
    let getRepositoryDifferences' = getRepositoryDifferences cfg.RemoteMustMatch getTrackingBranchDifferences' reportProgress
    
    let gitRepositories = findGitRepositories ignores argv.[0]
    
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

    let createTableRow (c: LocalBranchComparison) = 
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
        | Ok d -> createTableRow d
        | Error e -> match e.Comparison with
                     | Some d -> createTableRow d
                     | None -> createErrorRow e

    Console.WriteLine ()

    let tableData = gitRepositories
                    |> Seq.map getRepositoryDifferences'
                    |> Seq.map outputResult
                    |> Seq.toArray

    Console.SetCursorPosition(0, Console.CursorTop)
    Console.Write (spacesToFillLine "")
    Console.WriteLine ()

    ConsoleTable
        .From(tableData)
        .Write(Format.MarkDown);

    0
