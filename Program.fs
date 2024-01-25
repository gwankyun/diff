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

module Diff =
    let toFileMap path =
        getFileSystemEntries path
            |> Array.toSeq
            |> Seq.map (fun x -> x, Directory.exists x)
            |> Seq.map (fun (x, b) ->
                Path.relativePath path x, (b, Path.lastWriteTime x))
            |> Map.ofSeq

    let currentData path =
        // 遍歷目錄
        let state =
            path |> toFileMap

        // 最後修改時間
        let result =
            Map.maxBy (fun (_, (b1, w1)) (_, (b2, w2)) ->
                match b1, b2 with
                | _, true -> true
                | true, false -> false
                | false, false -> w1 > w2) state

        let _, (_, t) = result
        state, t

    let wrtieFile dest file json =
        File.writeAllText <| Path.join dest file <| (Json.serialize <| json)

    let current path dest =
        let stateJson, lastWriteTimeJson = currentData path

        // 寫入文件
        wrtieFile dest "state.txt" stateJson
        wrtieFile dest "lastWriteTime.txt" lastWriteTimeJson

    let currentMany pathList dest =
        let state = List.map currentData pathList

        let stateJson = List.map fst state
        let lastWriteTimeJson = List.map snd state

        // 寫入文件
        wrtieFile dest "state.txt" stateJson
        wrtieFile dest "lastWriteTime.txt" lastWriteTimeJson

    let readJson path =
        let joinPath = Path.join path
        let toJson = joinPath >> File.readAllText
        let stateJson = "state.txt" |> toJson
        let lastWriteJson = "lastWriteTime.txt" |> toJson
        stateJson, lastWriteJson

    type StateType = Map<string, bool * DateTime>
    let read path =
        let stateJson, lastWriteJson = readJson path
        Json.deserialize<StateType> stateJson, Json.deserialize<DateTime> lastWriteJson

    let readMany path =
        let stateJson, lastWriteJson = readJson path
        Json.deserialize<StateType list> stateJson, Json.deserialize<DateTime list> lastWriteJson

    let merge path package =
        Directory.copy (Path.join package "data") path
        // 刪除文件
        let deletionJson = File.readAllText <| Path.join package "deletion.json"
        let deletion = Json.deserialize<StateType> deletionJson
        deletion
        |> Map.toSeq
        |> Seq.iter (fun (k, (b, _)) ->
            let file = Path.join path k
            if b then
                Directory.deleteIfExists file true
            else
                File.deleteIfexists file)

    let diff path target newPath oldPath =
        let newState, _ = read newPath
        let oldState, _ = read oldPath
        // printfn "newState: %A" newState
        // printfn "oldState: %A" oldState
        let creation = Map.sub newState oldState
        let deletion = Map.sub oldState newState
        let modification =
            Map.diffWith (fun f (b1, d1) (b2, d2) ->
                match b1, b2 with
                | true, true -> false
                | false, false -> d1 <> d2
                |  _ -> true
                ) newState oldState

        let joinTarget = Path.join3 target "data"

        let dirAction m =
            Map.iter (fun k _ ->
                Directory.create <| joinTarget k) m

        let fileAction m =
            Map.iter (fun k _ ->
                let dest = joinTarget k
                Directory.createFor dest
                File.copy <| Path.join path k <| dest) m

        // 新增目錄文件要複製
        let creationDir, creationFile =
            creation |> Map.partition (fun _ (b, _) -> b)
        // printfn "creationDir: %A" creationDir
        creationDir |> dirAction
        creationFile |> fileAction

        // 修改的部分
        // printfn "modify: %A" modify
        let modifyDir, modifyFile =
            modification |> Map.partition (fun _ ((b1, _), _) -> b1)
        // printfn "modifyFile: %A" modifyFile
        modifyDir |> dirAction
        modifyFile |> fileAction

        // 刪除文件及目錄要保存為文件
        let deltedJosn = Json.serialize deletion
        File.writeAllText <| Path.join target "deletion.json" <| deltedJosn

    /// <summary>对比两个目录</summary>
    let compare path1 path2 =
        let m1 = toFileMap path1
        let m2 = toFileMap path2
        let c1 = Map.sub m1 m2
        let c2 = Map.sub m2 m1
        let diffPar =
            Map.diffWith (fun k (b1, d1) (b2, d2) ->
                match b1, b2 with
                | true, true -> false
                | false, false -> d1 <> d2
                | _ -> true) m1 m2
        let result = c1.IsEmpty && c2.IsEmpty && diffPar.IsEmpty
        if not result then
            printfn "%A" c1
            printfn "%A" c2
            printfn "%A" diffPar
        result

    let sync path1 path2 =
        let fileList1 = toFileMap path1
        let fileList2 = toFileMap path2
        let u = Map.union fileList1 fileList2
        for i in u do
            let key = i.Key
            let value = i.Value
            let p1 = Path.join path1 key
            let p2 = Path.join path2 key
            match value with
            | Some (false, _), Some (false, _) -> // 有新有舊，修改
                File.copy p1 p2
            | Some (false, _), Some (true, _) ->
                Directory.deleteIfExists p2 true
                File.copy p1 p2
            | Some (true, _), Some (false, _) ->
                File.deleteIfexists p2
                Directory.create p2
            | Some (true, _), Some (true, _) ->
                ()
            | Some (false, _), None -> // 有新冇舊，新增
                File.copy p1 p2
            | Some (true, _), None ->
                Directory.create p2
            | None, Some (false, _) -> // 冇新有舊，刪除
                File.deleteIfexists p2
            | None, Some (true, _) ->
                Directory.deleteIfExists p2 true
            | _ -> ()
        ()

    let test =
        // let a = Map.ofList [(1, 2); (2, 3); (3, 4)]
        // let b = Map.ofList [(1, 2); (2, 3); (3, 5)]
        // let i = Map.diffWith (fun k v1 v2 -> v1 <> v2) a b
        // printfn "i: %A" i

        let modify path =
            File.writeAllText <| Path.join path "2.txt" <| "" // 新增文件
            File.writeAllText <| Path.join3 path "5" "6.txt" <| "" // 新增文件
            Directory.create <| Path.join path "3" // 新目錄
            Directory.create <| Path.join3 path "3" "7" // 新目錄
            File.deleteIfexists <| Path.join path "3.txt" // 刪除文件
            File.deleteIfexists <| Path.join3 path "5" "51.txt" // 刪除文件
            File.writeAllText <| Path.join path "1.txt" <| "" // 修改文件
            File.writeAllText <| Path.join4 path "5" "52" "521.txt" <| "" // 修改文件
            Directory.deleteIfExists <| Path.join path "4" <| true // 刪除目錄

        let clearDirectory dir =
            Directory.deleteIfExists dir true
            Directory.create dir
        // 複製一個文件夾
        printfn "%A" Directory.current
        let currentDir = Directory.current
        let test = Path.join currentDir "test"
        let joinTest = Path.join test
        let currentPath = joinTest "current"
        let dataPath = joinTest "data"
        clearDirectory dataPath
        Directory.copy <| joinTest "base" <| dataPath
        clearDirectory currentPath
        Directory.copy dataPath currentPath
        // 記錄狀態A
        let state1 = joinTest "state1"
        clearDirectory state1
        current currentPath state1
        // 進行一些操作
        modify currentPath
        // 記錄狀態B
        let state2 = joinTest "state2"
        clearDirectory state2
        current currentPath state2
        // 從AB生成差異包
        let diffPath = joinTest "diff"
        clearDirectory diffPath
        diff currentPath diffPath state2 state1

        let zipPath = Path.join <| Directory.parent diffPath <| "patch.zip"
        File.deleteIfexists zipPath
        Directory.compress diffPath zipPath

        // 原目錄合併差異包
        merge dataPath diffPath
        // 對比兩個目錄
        printfn "compare: %A" <| compare dataPath currentPath

[<EntryPoint>]
let main argv =
    printfn "%A" argv
    if argv.Length < 1 then
        exit 1

    let command = argv[0]

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
                    Diff.current path dest
                    0
                else
                    2
            else
                1
        | "diff" as c ->
            checkHelp argv <| message[command]
            if argv.Length >= 5 then
                let path = argv[1]
                let dest = argv[2]
                let newPath = argv[3]
                let oldPath = argv[4]
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
                    failwith (newPath  + "not exists")
                Diff.sync newPath oldPath
            0
        | _ ->
            printfn "Unknowed commander"
            3
    printfn "%A" result
    0
