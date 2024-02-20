namespace Common
open System.IO
open System.Text.Json
open System.IO.Compression

module Map =
    let maxBy f table =
        let maxKey = Map.maxKeyValue table
        let otherMap = table |> Map.remove (fst maxKey)
        let result =
            Map.fold (fun s k v ->
                if f s (k, v) then
                    s
                else
                    k, v)
                maxKey otherMap
        result

    /// <summary>`table1`有`table2`無的鍵</summary>
    let sub table1 table2 =
        Map.filter (fun k _ -> table2 |> Map.containsKey k |> not) table1

    let keysSet table = Map.keys table |> Set.ofSeq

    let intersect table1 table2 =
        Set.intersect <| keysSet table1 <| keysSet table2
        |> Set.toList
        |> List.map (fun k -> k, (table1[k], table2[k]))
        |> Map.ofList

    let union table1 table2 =
        let keys1 = table1 |> keysSet
        let keys2 = table2 |> keysSet
        Set.union keys1 keys2
        |> Set.toList
        |> List.map (fun k ->
            let findK = Map.tryFind k
            k, (findK table1, findK table2))
        |> Map.ofList

    let filterMap f table =
        Map.fold (fun s k v ->
            match f k v with
            | Some value -> s |> Map.add k value
            | None -> s
            ) Map.empty table

    let diffWith f table1 table2 =
        union table1 table2
        |> filterMap (fun k (o1, o2) ->
            match o1, o2 with
            | Some v1, Some v2 ->
                if f k v1 v2 then
                    Some (v1, v2)
                else
                    None
            | _ -> None)

module Common =
    let getFileSystemEntries path =
        Directory.GetFileSystemEntries(
            path,
            "*",
            SearchOption.AllDirectories)

    module Path =
        let join (a: string) b =
            Path.Join(a, b)

        let join3 (a: string) b c =
            Path.Join(a, b, c)

        let join4 (a: string) b c d =
            Path.Join(a, b, c, d)

        let lastWriteTime path =
            (new FileInfo(path)).LastWriteTime

        let joinList pathList =
            List.reduce (fun a b -> join a b) pathList

        let relativePath parent path =
            Path.GetRelativePath(parent, path)

        let directory path =
            (new FileInfo(path)).Directory

    module Json =
        let serialize value =
            JsonSerializer.Serialize value

        let deserialize<'a> (value: string) =
            JsonSerializer.Deserialize<'a> value

    let tryGetDirectory path =
        let dir = (new FileInfo(path)).Directory
        if dir <> null then
            Some dir
        else
            None

    module String =
        let countChar c s =
            s |> String.filter ((=) c) |> String.length

        let startsWith (value: string) (str: string) =
            str.StartsWith value

    module File =
        let exists path =
            File.Exists path

        let delete path =
            File.Delete path

        let deleteIfexists path =
            if exists path then
                delete path

        let copy src dest =
            if src |> exists |> not then
                invalidArg <| nameof src <| sprintf "%s not exists" src
            File.Copy(src, dest, true)

        /// <summary>寫入文件</summary>
        let writeAllText path contents =
            File.WriteAllText(path, contents)

        let readAllText path =
            File.ReadAllText path

        let readAllLines path =
            File.ReadAllLines path

    module Directory =
        let exists path =
            Directory.Exists path

        let delete path recursive =
            Directory.Delete(path, recursive)

        let deleteIfExists path recursive =
            if exists path then
                delete path recursive

        let createFor path =
            (Path.directory path).Create()

        let create path =
            Directory.CreateDirectory path |> ignore

        let copy src dest =
            let data = getFileSystemEntries src
            let sorted =
                data
                |> Array.sortWith (fun a b ->
                    let level s = s |> String.countChar '\\'
                    let la = level a
                    let lb = level b
                    match exists a, exists b with
                    | true, true -> compare lb la
                    | true, _ -> 1
                    | false, true -> -1
                    | false, false -> compare lb la)
            sorted
            |> Array.iter (fun x ->
                let rela = Path.relativePath src x
                let newPath = Path.join dest rela
                match exists x with
                | true ->
                    if exists newPath |> not then
                        createFor newPath
                        create newPath
                | false ->
                    createFor newPath
                    File.copy x newPath)

        let current =
            Directory.GetCurrentDirectory()

        let parent path =
            Directory.GetParent(path).ToString()

        let compress path dest =
            let mutable dir = new DirectoryInfo(dest)

            if dir <> null then
                dir <- dir.Parent
            
            if dir.Exists |> not then
                dir.Create()
            
            ZipFile.CreateFromDirectory(path, dest, CompressionLevel.Optimal, false)
