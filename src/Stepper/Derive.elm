module Stepper.Derive exposing
    ( Instr
    , Operand(..)
    , TreeLine
    , disassemble
    , listing
    , sourceSpan
    , treeLines
    )

{-| Pure, total helpers that turn a State into what the panes show.
-}

import Array exposing (Array)
import Stepper.Machine exposing (Kid(..), State, memAt)
import Stepper.Trace exposing (OpInfo, Trace)


type alias Instr =
    { addr : Int, name : String, operand : Operand }


{-| Pending: a two-word opcode whose operand has not been emitted yet.
-}
type Operand
    = None
    | Pending
    | Word Int
    | Str String


opInfo : Trace -> Int -> OpInfo
opInfo trace word =
    if word < 0 then
        { name = "", words = 1 }

    else
        Array.get (remainderBy 256 word) trace.ops |> Maybe.withDefault { name = "", words = 1 }


disassemble : Trace -> State -> List Instr
disassemble trace state =
    let
        go addr acc =
            if addr >= state.codeTop then
                List.reverse acc

            else
                let
                    info =
                        opInfo trace (memAt addr state)

                    operand =
                        if info.words == 1 then
                            None

                        else if addr + 1 >= state.codeTop then
                            Pending

                        else if info.name == "PRNS" then
                            Str (stringAt trace state (memAt (addr + 1) state))

                        else
                            Word (memAt (addr + 1) state)
                in
                go (addr + info.words) ({ addr = addr, name = info.name, operand = operand } :: acc)
    in
    go 0 []


{-| The characters of the string stored downwards from `start`, up to its 0.
-}
stringAt : Trace -> State -> Int -> String
stringAt trace state start =
    stringWords trace state start |> List.map Char.fromCode |> String.fromList


stringWords : Trace -> State -> Int -> List Int
stringWords trace state start =
    let
        go j acc =
            if j < 0 || j > trace.memSize || List.length acc > trace.memSize then
                List.reverse acc

            else
                case memAt j state of
                    0 ->
                        List.reverse acc

                    word ->
                        go (j - 1) (word :: acc)
    in
    go start []


{-| Exactly the text PVM.listCode writes to a .cod file, with "\n" line ends
(Java writes the platform separator, except the bare "\n" after ASSEM).
-}
listing : Trace -> State -> String
listing trace state =
    let
        codeLen =
            state.codeTop

        wrap i =
            modBy (max 1 trace.memSize) i

        go i guard acc =
            if i < codeLen && memAt i state /= 255 && guard < trace.memSize then
                let
                    info =
                        opInfo trace (memAt i state)

                    head =
                        "  {" ++ String.padLeft 5 ' ' (String.fromInt i) ++ " } " ++ String.padRight 8 ' ' info.name

                    ( tail, last ) =
                        if info.name == "PRNS" then
                            ( " \"" ++ String.concat (List.map escape (stringWords trace state (memAt (wrap (i + 1)) state))) ++ "\""
                            , wrap (i + 1)
                            )

                        else if info.words == 2 then
                            ( " " ++ String.fromInt (memAt (wrap (i + 1)) state), wrap (i + 1) )

                        else
                            ( "", i )
                in
                go (wrap (last + 1)) (guard + 1) ((head ++ tail ++ "\n") :: acc)

            else
                List.reverse acc
    in
    "ASSEM\nBEGIN\n" ++ String.concat (go 0 0 []) ++ "END.\n"


escape : Int -> String
escape code =
    case code of
        92 ->
            "\\\\"

        34 ->
            "\\\""

        39 ->
            "\\'"

        8 ->
            "\\b"

        9 ->
            "\\t"

        10 ->
            "\\n"

        12 ->
            "\\f"

        13 ->
            "\\r"

        _ ->
            String.fromChar (Char.fromCode code)


type alias TreeLine =
    { depth : Int, label : String, open : Bool, token : Bool }


{-| An indented outline of the parse tree so far, oldest first. Nodes on the
path from the root to the production being parsed are marked open.
-}
treeLines : Trace -> State -> List TreeLine
treeLines trace state =
    let
        tokenLabel k =
            case Array.get k state.scanned of
                Just record ->
                    if record.tok.val == "" then
                        Array.get record.tok.kind trace.terminals |> Maybe.withDefault "?"

                    else
                        record.tok.val

                Nothing ->
                    "?"

        closed depth kid =
            case kid of
                KNode node ->
                    { depth = depth, label = node.prod, open = False, token = False }
                        :: List.concatMap (closed (depth + 1)) (List.reverse node.kids)

                KTok k ->
                    [ { depth = depth, label = tokenLabel k, open = False, token = True } ]

        -- the open path, root first; each open node shows its finished kids, then the next open node
        openPath depth path =
            case path of
                [] ->
                    []

                node :: rest ->
                    { depth = depth, label = node.prod, open = True, token = False }
                        :: List.concatMap (closed (depth + 1)) (List.reverse node.kids)
                        ++ openPath (depth + 1) rest
    in
    case List.reverse (state.tree.focus :: state.tree.crumbs) of
        root :: path ->
            List.concatMap (closed 0) (List.reverse root.kids) ++ openPath 0 path

        [] ->
            []


{-| Source positions [from, to) of the last consumed token and of the pending
lookahead.
-}
sourceSpan : Trace -> State -> { consumed : Maybe ( Int, Int ), lookahead : Maybe ( Int, Int ) }
sourceSpan _ state =
    let
        range record =
            ( record.tok.pos, record.tok.pos + String.length record.tok.val )

        lastConsumed k =
            case Array.get k state.scanned of
                Nothing ->
                    Nothing

                Just record ->
                    if record.consumed then
                        Just (range record)

                    else
                        lastConsumed (k - 1)
    in
    { consumed = lastConsumed (Array.length state.scanned - 1)
    , lookahead = state.pending |> Maybe.andThen (\k -> Array.get k state.scanned) |> Maybe.map range
    }
