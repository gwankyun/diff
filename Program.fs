open System.IO
open System.Text.Json
open System.Text.Json.Serialization
open Common

module C = Common

open Common.Common

type DateTime = System.DateTime

open Argu

module Str = String
module Dir = Directory

type CliArguments =
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
                "添加當前狀態"
            | Diff _ -> "比較兩個狀態，生成補丁包"
            | Merge _ -> "合併補丁包"
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

    let history = Path.join Dir.current ".history"

    let addParam = result.TryGetResult Add
    let diffParam = result.TryGetResult Diff
    let mergeParam = result.TryGetResult Merge
    let listParam = result.TryGetResult List
    let syncParam = result.TryGetResult Sync
    let testParam = result.TryGetResult Test

    if addParam.IsSome then
        let path = Dir.current
        let dest = Path.join history addParam.Value
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

    if diffParam.IsSome then
        let history = Path.join Dir.current ".history"
        let path = Dir.current
        let newPath, oldPath = diffParam.Value
        let dest = Path.join3 history ".diff" <| newPath + "-" + oldPath
        let newPath = Path.join history newPath
        let oldPath = Path.join history oldPath
        Dir.create dest
        Diff.diff path dest newPath oldPath
        exit 0

    if mergeParam.IsSome then
        let path = Dir.current
        Diff.merge path mergeParam.Value
        exit 0

    if listParam.IsSome then
        let history = Path.join Dir.current ".history"
        let dirs = Directory.GetDirectories history
        for i in dirs do
            let info = new DirectoryInfo(i)
            let relativePath = Path.relativePath history i
            if relativePath |> Str.startsWith ".diff" |> not then
                printfn "path: %A writeTime: %A" relativePath <| info.LastWriteTime
        exit 0

    if syncParam.IsSome then
        let newPath, oldPath = syncParam.Value
        if Dir.exists newPath |> not then
            failwith <| newPath  + "not exists"
        Diff.sync newPath oldPath
        exit 0

    if testParam.IsSome then
        Diff.test
        exit 0

    printfn "%s" <| parser.PrintUsage()

    exit 1

    printfn "%A" result
    0
