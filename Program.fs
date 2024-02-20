open System.IO
open System.Text.Json
open System.Text.Json.Serialization
open Common

module C = Common

open Common.Common

type DateTime = System.DateTime

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

[<EntryPoint>]
let main argv =
    printfn "argv: %A" argv
    if argv.Length < 1 then
        exit 1

    let command = argv[0]
    printfn "command: %s" command

    let history = Path.join Dir.current ".history"

    let result =
        let message = Map [
            ("current", "current path dest");
            ("diff", "diff path dest newPath oldPath");
            ("merge", "merge path package");
            ("test", "test");
            ]
        let checkHelp argv messge =
            match argv |> Array.tryFind ((=) "help") with
            | Some _ -> printfn "dotnet run %s" messge
            | None -> ()
        match command with
        | "current" ->
            checkHelp argv <| message[command]
            if argv.Length >= 3 then
                let path = argv[1]
                let dest = argv[2]
                if Dir.exists path && Dir.exists dest then
                    Diff.current Diff.defaultPred path dest
                    0
                else
                    2
            else if history |> Dir.exists then
                let path = Dir.current
                let dest = Path.join history argv[1]
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
                0
            else
                1
        | "list" ->
            let history = Path.join Dir.current ".history"
            let dirs = Directory.GetDirectories history
            for i in dirs do
                let info = new DirectoryInfo(i)
                let relativePath = Path.relativePath history i
                if relativePath |> Str.startsWith ".diff" |> not then
                    printfn "path: %A writeTime: %A" relativePath <| info.LastWriteTime
            0
        | "diff" as c ->
            checkHelp argv <| message[command]
            if argv.Length >= 5 then
                let path = argv[1]
                let dest = argv[2]
                let newPath = argv[3]
                let oldPath = argv[4]
                Diff.diff path dest newPath oldPath
                0
            else if argv.Length >= 3 then
                let history = Path.join Dir.current ".history"
                let path = Dir.current
                let newPath = argv[1]
                let oldPath = argv[2]
                let dest = Path.join3 history ".diff" <| newPath + "-" + oldPath
                let newPath = Path.join history newPath
                let oldPath = Path.join history oldPath
                Dir.create dest
                Diff.diff path dest newPath oldPath
                0
            else
                1
        | "merge" -> // 合併
            checkHelp argv <| message[command]
            if argv.Length >= 3 then
                let path = argv[1]
                let package = argv[2]
                Diff.merge path package
                0
            else if argv.Length >= 2 then
                let path = Dir.current
                let package = argv[1]
                Diff.merge path package
                0
            else
                1
        | "test" -> // 測試
            checkHelp argv <| message[command]
            Diff.test
            0
        | "help" ->
            for i in message do
                printfn "dotnet run %s" <| i.Value
            0
        | "sync" ->
            if argv.Length >= 3 then
                let newPath = argv[1]
                let oldPath = argv[2]
                if Dir.exists newPath |> not then
                    failwith <| newPath  + "not exists"
                Diff.sync newPath oldPath
            0
        | _ ->
            printfn "Unknowed commander"
            3
    printfn "%A" result
    0
