open System.IO
open System.Text.Json
open System.Text.Json.Serialization
open Common

module C = Common

open Common.Common

type DateTime = System.DateTime

open Argu

// [JsonSerializerContext.a JsonSerializableAttribute(typeof(DateTime))]
// type MyContext() =
//     inherit JsonSerializerContext()

module Str = String
module Dir = Directory


// Operators.typeof
[<JsonSerializable(typeof<DateTime>)>]
type MyContext(x) =
    inherit JsonSerializerContext(x)

    override this.GeneratedSerializerOptions: JsonSerializerOptions =
        base.GeneratedSerializerOptions
    override this.GetTypeInfo(``type``: System.Type): Metadata.JsonTypeInfo =
        base.GetTypeInfo(``type``)

// MyContext.

// JsonSerializer.Deserialize("", jsonTypeInfo<)

type CliArguments =
    // | Path of path: string
    | [<CliPrefix(CliPrefix.None)>] Add of name: string
    | [<CliPrefix(CliPrefix.None)>] Diff of newPath: string * oldPath: string
    | [<CliPrefix(CliPrefix.None)>] Merge of package: string
    | [<CliPrefix(CliPrefix.None)>] List
    | [<CliPrefix(CliPrefix.None)>] Test
    | [<CliPrefix(CliPrefix.None)>] Sync of newPath: string * oldPath: string
    | Path of path: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Add name ->
                "Add"
            | Diff _ -> "Diff"
            | Merge _ -> "Merge"
            | Path _ -> "Path"
            | List -> "List"
            | Sync _ -> "Sync"
            | Test -> "Test"

[<EntryPoint>]
let main argv =
    printfn "argv: %A" argv

    let parser = ArgumentParser.Create<CliArguments>(programName = "dir-diff.exe")

    let result = parser.Parse argv

    let all = result.GetAllResults()

    // if argv.Length < 1 then
    //     exit 1

    // let command = argv[0]
    // printfn "command: %s" command

    let history = Path.join Dir.current ".history"

    let add = result.TryGetResult Add
    let pathPair = result.TryGetResult Diff
    let package = result.TryGetResult Merge
    let list = result.TryGetResult List
    let sync = result.TryGetResult Sync
    let test = result.TryGetResult Test

    if add.IsSome then
        let path = Dir.current
        let dest = Path.join history add.Value
        Dir.create dest
        let whiteList = Path.join3 history ".config" "white_list.txt"

        let content =
            match whiteList |> File.exists with
            | true ->
                Some <| File.readAllLines whiteList
            | false -> None
        Diff.current (fun p _ _ ->
            let h = p |> Str.startsWith ".history" |> not
            match content with
            | Some c ->
                Array.exists (p |> Str.startsWith) c && h
            | None -> h) path dest
        exit 0

    if pathPair.IsSome then
        let history = Path.join Dir.current ".history"
        let path = Dir.current
        let newPath, oldPath = pathPair.Value
        let dest = Path.join3 history ".diff" <| newPath + "-" + oldPath
        let newPath = Path.join history newPath
        let oldPath = Path.join history oldPath
        Dir.create dest
        Diff.diff path dest newPath oldPath
        exit 0

    if package.IsSome then
        let path = Dir.current
        Diff.merge path package.Value
        exit 0

    if list.IsSome then
        let history = Path.join Dir.current ".history"
        let dirs = Directory.GetDirectories history
        for i in dirs do
            let info = new DirectoryInfo(i)
            let relativePath = Path.relativePath history i
            if relativePath |> Str.startsWith ".diff" |> not then
                printfn "path: %A writeTime: %A" relativePath <| info.LastWriteTime
        exit 0

    if sync.IsSome then
        let newPath, oldPath = sync.Value
        if Dir.exists newPath |> not then
            failwith <| newPath  + "not exists"
        Diff.sync newPath oldPath
        exit 0

    if test.IsSome then
        Diff.test
        exit 0

    printfn "%s" <| parser.PrintUsage()

    exit 1

    printfn "%A" result
    0
