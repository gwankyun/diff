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
                if Directory.exists path && Directory.exists dest then
                    Diff.current Diff.defaultPred path dest
                    0
                else
                    2
            else if Path.join Directory.current ".history" |> Directory.exists then
                let path = Directory.current
                let dest = Path.join3 Directory.current ".history" argv[1]
                Directory.CreateDirectory dest |> ignore
                Diff.current (fun p _ _ -> p.StartsWith(".history") |> not) path dest
                0
            else
                1
        | "list" ->
            let history = Path.join Directory.current ".history"
            let dirs = Directory.GetDirectories(history)
            for i in dirs do
                let info = new DirectoryInfo(i)
                let relativePath = Path.relativePath history i
                if relativePath |> String.startsWith ".diff" |> not then
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
                let history = Path.join Directory.current ".history"
                let path = Directory.current
                let newPath = argv[1]
                let oldPath = argv[2]
                let dest = Path.join3 history ".diff" <| newPath + "-" + oldPath
                let newPath = Path.join history newPath
                let oldPath = Path.join history oldPath
                Directory.create dest
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
                let path = Directory.current
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
                if Directory.exists newPath |> not then
                    failwith <| newPath  + "not exists"
                Diff.sync newPath oldPath
            0
        | _ ->
            printfn "Unknowed commander"
            3
    printfn "%A" result
    0
