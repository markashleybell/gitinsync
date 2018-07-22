module Types

type LocalBranchComparison = {
    Directory: string;
    BranchName: string;
    Status: string;
    AheadOfRemoteBy: int;
    BehindRemoteBy: int;
}

type PipelineError = {
    Directory: string;
    BranchName: string option;
    Message: string;
    Comparison: LocalBranchComparison option;
}

type ResultBuilder() =
    member _this.Return(x) = Ok x
    member _this.Bind(x, f) = Result.bind f x

let result = ResultBuilder()

type Config = {
    GitUsername: string;
    GitPassword: string;
    RemoteMustMatch: string;
}

type OutputTableRow = {
    Status: string;
    Repository: string;
    Branch: string;
    Info: string;
}
