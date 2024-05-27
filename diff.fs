namespace Common

open Common.Common

type DateTime = System.DateTime
module Dir = Directory

module Diff =
    // 相對路徑
    let toFileMap path =
        getFileSystemEntries path
        |> Array.toSeq
        |> Seq.map (fun x -> x, Dir.exists x)
        |> Seq.map (fun (x, b) -> Path.relativePath path x, (b, Path.lastWriteTime x))
        |> Map.ofSeq

    let toFileMapWith pred path =
        toFileMap path |> Map.filter (fun p (d, t) -> pred p d t)

    let currentData pred path =
        // 遍歷目錄
        let state = path |> toFileMapWith pred

        // 最後修改時間
        let result =
            Map.maxBy
                (fun (_, (b1, w1)) (_, (b2, w2)) ->
                    match b1, b2 with
                    | _, true -> true
                    | true, false -> false
                    | false, false -> w1 > w2)
                state

        let _, (_, t) = result
        state, t

    // let currentDataWith pred path =
    //     currentData path
    //     |> Map.filter pred

    let wrtieFile dest file json =
        File.writeAllText <| Path.join dest file <| (Json.serialize <| json)

    let current pred path dest =
        let stateJson, lastWriteTimeJson = currentData pred path

        // 寫入文件
        wrtieFile dest "state.txt" stateJson
        wrtieFile dest "lastWriteTime.txt" lastWriteTimeJson

    let defaultPred a b c = true

    let currentMany pathList dest =
        let state = List.map <| currentData defaultPred <| pathList

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
        Dir.copy <| Path.join package "data" <| path
        // 刪除文件
        let deletionJson = File.readAllText <| Path.join package "deletion.json"
        let deletion = Json.deserialize<StateType> deletionJson

        deletion
        |> Map.toSeq
        |> Seq.iter (fun (k, (b, _)) ->
            let file = Path.join path k

            if b then
                Dir.deleteIfExists file true
            else
                File.deleteIfExists file)

    let diff path target newPath oldPath =
        let newState, _ = read newPath
        let oldState, _ = read oldPath
        // printfn "newState: %A" newState
        // printfn "oldState: %A" oldState
        let creation = Map.sub newState oldState
        let deletion = Map.sub oldState newState

        let modification =
            Map.diffWith
                (fun f (b1, d1) (b2, d2) ->
                    match b1, b2 with
                    | true, true -> false // 目錄不比較
                    | false, false -> d1 <> d2 // 文件比較修改時間
                    | _ -> true)
                newState
                oldState

        let joinTarget = Path.join3 target "data"

        let dirAction m =
            Map.iter (fun k _ -> Dir.create <| joinTarget k) m

        let fileAction m =
            Map.iter
                (fun k _ ->
                    let dest = joinTarget k
                    Dir.createFor dest
                    File.copy <| Path.join path k <| dest)
                m

        // 新增目錄文件要複製
        let creationDir, creationFile = creation |> Map.partition (fun _ (b, _) -> b)
        // printfn "creationDir: %A" creationDir
        creationDir |> dirAction
        creationFile |> fileAction

        // 修改的部分
        // printfn "modify: %A" modify
        let modifyDir, modifyFile = modification |> Map.partition (fun _ ((b1, _), _) -> b1)
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
            Map.diffWith
                (fun k (b1, d1) (b2, d2) ->
                    match b1, b2 with
                    | true, true -> false
                    | false, false -> d1 <> d2
                    | _ -> true)
                m1
                m2

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
            | Some(false, _), Some(false, _) -> // 有新有舊，修改
                File.copy p1 p2
            | Some(false, _), Some(true, _) ->
                Dir.deleteIfExists p2 true
                File.copy p1 p2
            | Some(true, _), Some(false, _) ->
                File.deleteIfExists p2
                Dir.create p2
            | Some(true, _), Some(true, _) -> ()
            | Some(false, _), None -> // 有新冇舊，新增
                File.copy p1 p2
            | Some(true, _), None -> Dir.create p2
            | None, Some(false, _) -> // 冇新有舊，刪除
                File.deleteIfExists p2
            | None, Some(true, _) -> Dir.deleteIfExists p2 true
            | _ -> ()

        ()

    let test =
        printfn "What?"
        // let a = Map.ofList [(1, 2); (2, 3); (3, 4)]
        // let b = Map.ofList [(1, 2); (2, 3); (3, 5)]
        // let i = Map.diffWith (fun k v1 v2 -> v1 <> v2) a b
        // printfn "i: %A" i

        let modify path =
            File.writeAllText <| Path.join path "2.txt" <| "" // 新增文件
            File.writeAllText <| Path.join3 path "5" "6.txt" <| "" // 新增文件
            Dir.create <| Path.join path "3" // 新目錄
            Dir.create <| Path.join3 path "3" "7" // 新目錄
            File.deleteIfExists <| Path.join path "3.txt" // 刪除文件
            File.deleteIfExists <| Path.join3 path "5" "51.txt" // 刪除文件
            File.writeAllText <| Path.join path "1.txt" <| "" // 修改文件
            File.writeAllText <| Path.join4 path "5" "52" "521.txt" <| "" // 修改文件
            Dir.deleteIfExists <| Path.join path "4" <| true // 刪除目錄

        let clearDirectory dir =
            Dir.deleteIfExists dir true
            Dir.create dir
        // 複製一個文件夾
        printfn "%A" Dir.current
        let currentDir = Dir.current
        let test = Path.join currentDir "test"
        let joinTest = Path.join test
        let currentPath = joinTest "current"
        let dataPath = joinTest "data"
        clearDirectory dataPath
        Dir.copy <| joinTest "base" <| dataPath
        clearDirectory currentPath
        Dir.copy dataPath currentPath
        // 記錄狀態A
        let state1 = joinTest "state1"
        clearDirectory state1
        current defaultPred currentPath state1
        // 進行一些操作
        modify currentPath
        // 記錄狀態B
        let state2 = joinTest "state2"
        clearDirectory state2
        current defaultPred currentPath state2
        // 從AB生成差異包
        let diffPath = joinTest "diff"
        clearDirectory diffPath
        diff currentPath diffPath state2 state1

        let zipPath = Path.join <| Dir.parent diffPath <| "patch.zip"
        File.deleteIfExists zipPath
        Dir.compress diffPath zipPath

        // 原目錄合併差異包
        merge dataPath diffPath
        // 對比兩個目錄
        printfn "compare: %A" <| compare dataPath currentPath
