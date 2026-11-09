module Stepper.Trace exposing
    ( Cause(..)
    , EndInfo
    , Event(..)
    , OpInfo
    , Pos
    , Severity(..)
    , Stamped
    , Sym
    , Tok
    , Trace
    , decode
    , maxEvents
    )

{-| The trace file as written by compiler/Parva/Trace.java, and its strict
decoder. The format is specified in docs/TRACE.md.
-}

import Array exposing (Array)
import Json.Decode as D exposing (Decoder)


type alias Trace =
    { file : String
    , source : String
    , maxT : Int
    , memSize : Int
    , terminals : Array String
    , types : Array String
    , ops : Array OpInfo
    , events : Array Stamped
    }


type alias OpInfo =
    { name : String, words : Int }


type alias Pos =
    { line : Int, col : Int }


type alias Tok =
    { kind : Int, val : String, line : Int, col : Int, pos : Int }


type alias Sym =
    { name : String, kind : String, type_ : String, value : Int, offset : Int }


type alias Stamped =
    { i : Int, at : Maybe Pos, event : Event }


type Event
    = Scan Tok Bool
    | Consume Tok
    | Enter String
    | Exit String
    | ScopeOpen Int
    | ScopeClose Int
    | SymInsert Sym Int Bool
    | SymFind String (Maybe Sym)
    | Check String (List String) Bool
    | Mem { addr : Int, old : Int, new : Int, cause : Cause }
    | Diag Severity Pos String
    | End EndInfo
    | Abort String


type Cause
    = Emit
    | Patch
    | FixDsp
    | StringData


type Severity
    = Syntax
    | Semantic
    | Warning


type alias EndInfo =
    { ok : Bool
    , errors : Int
    , warnings : Int
    , codeLength : Int
    , initSP : Int
    , code : Array Int
    , strings : Array Int
    , selfCheck : String
    }


maxEvents : Int
maxEvents =
    200000


decode : String -> Result String Trace
decode =
    D.decodeString trace >> Result.mapError explain



-- DECODERS


trace : Decoder Trace
trace =
    D.field "format" D.string
        |> D.andThen
            (\format ->
                if format /= "parva-trace" then
                    D.fail ("format is \"" ++ format ++ "\", not \"parva-trace\"")

                else
                    D.field "version" D.int
            )
        |> D.andThen
            (\version ->
                if version /= 1 then
                    D.fail ("version is " ++ String.fromInt version ++ "; this stepper reads version 1")

                else
                    D.map8 Trace
                        (D.field "file" D.string)
                        (D.field "source" D.string)
                        (D.field "maxT" D.int)
                        (D.field "memSize" D.int)
                        (D.field "terminals" (D.array D.string))
                        (D.field "types" (D.array D.string))
                        (D.field "ops" (D.array opInfo))
                        (D.field "events" (D.array stamped))
            )


opInfo : Decoder OpInfo
opInfo =
    D.map2 OpInfo
        (D.field "name" D.string)
        (D.field "words" D.int
            |> D.andThen
                (\words ->
                    if words == 1 || words == 2 then
                        D.succeed words

                    else
                        D.fail "words must be 1 or 2"
                )
        )


stamped : Decoder Stamped
stamped =
    D.map3 Stamped
        (D.field "i" D.int)
        (D.field "at" (D.nullable linePair))
        (D.field "ev" D.string |> D.andThen event)


linePair : Decoder Pos
linePair =
    D.list D.int
        |> D.andThen
            (\numbers ->
                case numbers of
                    [ line, col ] ->
                        D.succeed (Pos line col)

                    _ ->
                        D.fail "at must be [line, col]"
            )


event : String -> Decoder Event
event ev =
    case ev of
        "scan" ->
            D.map2 Scan (D.field "tok" tok) (D.field "pragma" D.bool)

        "consume" ->
            D.map Consume (D.field "tok" tok)

        "enter" ->
            D.map Enter (D.field "prod" D.string)

        "exit" ->
            D.map Exit (D.field "prod" D.string)

        "scope.open" ->
            D.map ScopeOpen (D.field "depth" D.int)

        "scope.close" ->
            D.map ScopeClose (D.field "depth" D.int)

        "sym.insert" ->
            D.map3 SymInsert (D.field "sym" sym) (D.field "depth" D.int) (D.field "clash" D.bool)

        "sym.find" ->
            D.field "found" D.bool
                |> D.andThen
                    (\found ->
                        if found then
                            D.map2 SymFind (D.field "name" D.string) (D.map Just (D.field "sym" sym))

                        else
                            D.map (\name -> SymFind name Nothing) (D.field "name" D.string)
                    )

        "check" ->
            D.map3 Check (D.field "rule" D.string) (D.field "args" (D.list D.string)) (D.field "ok" D.bool)

        "mem" ->
            D.map4 (\addr old new why -> Mem { addr = addr, old = old, new = new, cause = why })
                (D.field "addr" D.int)
                (D.field "old" D.int)
                (D.field "new" D.int)
                (D.field "cause" cause)

        "diag" ->
            D.map3 Diag
                (D.field "severity" severity)
                (D.map2 Pos (D.field "line" D.int) (D.field "col" D.int))
                (D.field "msg" D.string)

        "end" ->
            D.map End endInfo

        "abort" ->
            D.map Abort (D.field "reason" D.string)

        _ ->
            D.fail ("unknown event kind \"" ++ ev ++ "\"")


tok : Decoder Tok
tok =
    D.map5 Tok
        (D.field "kind" D.int)
        (D.field "val" D.string)
        (D.field "line" D.int)
        (D.field "col" D.int)
        (D.field "pos" D.int)


sym : Decoder Sym
sym =
    D.map5 Sym
        (D.field "name" D.string)
        (D.field "kind" D.string)
        (D.field "type" D.string)
        (D.field "value" D.int)
        (D.field "offset" D.int)


cause : Decoder Cause
cause =
    oneOfStrings "cause"
        [ ( "emit", Emit ), ( "patch", Patch ), ( "fixdsp", FixDsp ), ( "string", StringData ) ]


severity : Decoder Severity
severity =
    oneOfStrings "severity"
        [ ( "syntax", Syntax ), ( "semantic", Semantic ), ( "warning", Warning ) ]


oneOfStrings : String -> List ( String, a ) -> Decoder a
oneOfStrings what choices =
    D.string
        |> D.andThen
            (\s ->
                case List.filter (Tuple.first >> (==) s) choices of
                    ( _, value ) :: _ ->
                        D.succeed value

                    [] ->
                        D.fail ("unknown " ++ what ++ " \"" ++ s ++ "\"")
            )


endInfo : Decoder EndInfo
endInfo =
    D.map8 EndInfo
        (D.field "ok" D.bool)
        (D.field "errors" D.int)
        (D.field "warnings" D.int)
        (D.field "codeLength" D.int)
        (D.field "initSP" D.int)
        (D.field "code" (D.array D.int))
        (D.field "strings" (D.array D.int))
        (D.field "selfCheck" D.string)



-- ERRORS


{-| A short path-and-message description. D.errorToString would include the
offending JSON value, which for a top-level error is the whole file.
-}
explain : D.Error -> String
explain =
    let
        go path error =
            case error of
                D.Field name inner ->
                    go (path ++ "." ++ name) inner

                D.Index k inner ->
                    go (path ++ "[" ++ String.fromInt k ++ "]") inner

                D.OneOf (first :: _) ->
                    go path first

                D.OneOf [] ->
                    where_ path ++ "no alternative matched"

                D.Failure message _ ->
                    where_ path ++ message

        where_ path =
            if path == "" then
                ""

            else
                "at " ++ path ++ ": "
    in
    go ""
