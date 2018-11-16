open Functions
open LibGit2Sharp
open LibGit2Sharp.Handlers
open ConsoleTables
open System
open System.IO

[<EntryPoint>]
let main argv =
    let path = match argv.Length with
               | 1 -> argv.[0]
               | _ -> Directory.GetCurrentDirectory()

    match tryLoadConfig (sprintf @"%s\.gitinsync" path) with
    | Ok cfg ->

        let ignores = []

        let spacesToFillLine (lineContents: string) = 
            let num = Console.BufferWidth - lineContents.Length - 1
            String.concat "" [ for _ in 1 .. num -> " " ]

        let credentials = CredentialsHandler((fun _ _ _ ->
            UsernamePasswordCredentials(Username=cfg.GitUsername, Password=cfg.GitPassword) :> Credentials))

        let updateLine s =
            Console.SetCursorPosition(0, Console.CursorTop)
            Console.Write (sprintf "%s%s" s (spacesToFillLine s))

        let getTrackingBranchDifferences' = getTrackingBranchDifferences credentials

        let gitRepositories = findGitRepositories ignores path

        let repositoryCount = gitRepositories |> Seq.length

        let reportProgress i s =
            updateLine (sprintf "Check [%d of %d]: %s" i repositoryCount s)

        let getRepositoryDifferences' i = 
            getRepositoryDifferences cfg.RemoteMustMatch getTrackingBranchDifferences' (reportProgress (i + 1))

        printfn ""
        printfn "Found %d git repositories in %s" repositoryCount path
        printfn ""

        let tableData = 
            gitRepositories
            |> Seq.mapi getRepositoryDifferences'
            |> Seq.map outputResult
            |> Seq.toArray

        printfn ""
        updateLine ""
        printf " "

        ConsoleTable
            .From(tableData)
            .Write(Format.MarkDown);

    | Error e -> printfn "%s" e

    0
