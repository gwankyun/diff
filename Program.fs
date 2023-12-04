open System.IO
open System.Text.Json
open Common

module C = Common

open Common.Common

type DateTime = System.DateTime

module Diff =
    let toFileMap path =
        getFileSystemEntries path
            |> Array.toSeq
            |> Seq.map (fun x -> x, Directory.exists x)
            |> Seq.map (fun (x, b) ->
                Path.relativePath path x, (b, Path.lastWriteTime x))
            |> Map.ofSeq

    let current path dest =
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

        // 轉成JSON
        let stateJson = Json.serialize state
        let _, (_, t) = result
        let lastWriteTimeJson = Json.serialize t

        // 寫入文件
        File.writeAllText <| Path.join dest "state.txt" <| stateJson
        File.writeAllText <| Path.join dest "lastWriteTime.txt" <| lastWriteTimeJson

        // printfn "%A" itemList
        // printfn "%A" result

    type StateType = Map<string, bool * DateTime>
    let read path =
        let joinPath = Path.join path
        let toJson = joinPath >> File.readAllText
        let stateJson = "state.txt" |> toJson
        let lastWriteJson = "lastWriteTime.txt" |> toJson
        Json.deserialize<StateType> stateJson, Json.deserialize<DateTime> lastWriteJson

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
        let creation, deletion, modify =
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
            modify |> Map.partition (fun _ ((b1, _), _) -> b1)
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
        let c1, c2, diffPar =
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
        match command with
        | "current" ->
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
        | "diff" ->
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
            if argv.Length >= 3 then
                let path = argv[1]
                let package = argv[2]
                Diff.merge path package
                0
            else
                1
        | "test" -> // 測試
            Diff.test
            1
        | _ -> 3
    printfn "%A" result
    0
