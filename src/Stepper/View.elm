module Stepper.View exposing (Actions, Example, Viewer, scrollTargets, startPage, viewer)

{-| HTML only. Every jump target (next/previous of a kind, step over, step
out) is computed here from the trace and emitted as a single `goto k`.
-}

import Array exposing (Array)
import Dict
import Html exposing (Html, button, div, h1, h2, input, li, p, pre, span, strong, table, td, text, th, tr, ul)
import Html.Attributes exposing (class, classList, disabled, id, style, title, type_, value)
import Html.Events exposing (onClick, onInput)
import Stepper.Derive as Derive exposing (Instr, Operand(..))
import Stepper.Machine as M exposing (Built, State, Status(..))
import Stepper.Trace as T exposing (Cause(..), Event(..), Severity(..), Stamped, Trace)


type alias Example =
    { name : String, result : Result String ( Trace, Built ) }


type alias Viewer =
    { name : String, trace : Trace, built : Built, k : Int, rawCod : Bool }


type alias Actions msg =
    { open : String -> msg
    , pickFile : msg
    , back : msg
    , goto : Int -> msg
    , toggleRaw : msg
    }


{-| (scrolling container, marked element) pairs that Main keeps in view.
-}
scrollTargets : List ( String, String )
scrollTargets =
    [ ( "source", "src-mark" ), ( "tokens", "tok-mark" ), ( "tree", "tree-mark" ), ( "code", "code-mark" ) ]



-- START PAGE


startPage : Actions msg -> List Example -> Maybe Example -> Html msg
startPage actions examples loaded =
    div [ class "app start" ]
        [ styles
        , case loaded of
            Just { name, result } ->
                case result of
                    Err why ->
                        div [ id "problem", class "banner bad" ]
                            [ strong [] [ text ("The trace " ++ name ++ " was rejected, so it is not shown.") ]
                            , p [] [ text why ]
                            ]

                    Ok _ ->
                        text ""

            Nothing ->
                text ""
        , h1 [] [ text "Parva stepper" ]
        , p [ class "dim" ] [ text "Replays a recorded compilation one event at a time. Pick an example, or load a trace written by scripts/trace.sh." ]
        , button [ onClick actions.pickFile ] [ text "Load trace…" ]
        , h2 [] [ text "Examples" ]
        , ul [ id "examples", class "examples" ] (List.map (exampleItem actions) examples)
        ]


exampleItem : Actions msg -> Example -> Html msg
exampleItem actions example =
    case example.result of
        Ok ( trace, built ) ->
            li []
                [ button [ class "link", onClick (actions.open example.name) ]
                    [ span [ class "ok" ] [ text "✓ " ], text example.name ]
                , span [ class "dim" ] [ text ("  " ++ String.fromInt (Array.length trace.events) ++ " events" ++ outcome built) ]
                ]

        Err why ->
            li [] [ span [ class "bad" ] [ text ("✗ " ++ example.name ++ ": " ++ why) ] ]


outcome : Built -> String
outcome built =
    case Array.get (Array.length built.states - 1) built.states |> Maybe.map .status of
        Just (Finished info) ->
            if info.errors == 0 then
                ", compiles"

            else
                ", " ++ String.fromInt info.errors ++ " errors"

        Just (Aborted _) ->
            ", aborted"

        _ ->
            ""



-- VIEWER


viewer : Actions msg -> Viewer -> Html msg
viewer actions v =
    let
        n =
            Array.length v.trace.events
    in
    case Array.get v.k v.built.states of
        Nothing ->
            div [ class "app" ] [ styles, text "internal error: position out of range" ]

        Just state ->
            div [ class "app viewer" ]
                [ styles
                , abortBanner v
                , controls actions v n state
                , div [ class "grid" ]
                    [ pane "Source" "source" "src" (sourcePane v state)
                    , pane "Parse tree" "tree" "tree" (treePane v state)
                    , pane ("Code" ++ codeTitle state) "code" "code" (codePane actions v state)
                    , pane ("Tokens (" ++ String.fromInt (Array.length state.scanned) ++ " scanned)") "tokens" "tok" (tokenPane v state)
                    , pane "Scopes, checks, diagnostics" "table" "tab" (tablePane state)
                    ]
                ]


codeTitle : State -> String
codeTitle state =
    " (" ++ String.fromInt state.codeTop ++ " words)"


pane : String -> String -> String -> List (Html msg) -> Html msg
pane title_ bodyId area body =
    div [ class ("pane area-" ++ area) ]
        [ div [ class "pane-head" ] [ text title_ ]
        , div [ id bodyId, class "pane-body" ] body
        ]


abortBanner : Viewer -> Html msg
abortBanner v =
    case Array.get (Array.length v.built.states - 1) v.built.states |> Maybe.map .status of
        Just (Aborted reason) ->
            div [ class "banner bad" ] [ text ("This compilation did not finish (" ++ reason ++ "). The trace stops early.") ]

        _ ->
            text ""



-- CONTROLS


controls : Actions msg -> Viewer -> Int -> State -> Html msg
controls actions v n state =
    let
        k =
            v.k

        jump label hint target =
            button
                (title hint
                    :: (case target of
                            Just t ->
                                [ onClick (actions.goto t) ]

                            Nothing ->
                                [ disabled True ]
                       )
                )
                [ text label ]

        seek name test =
            span [ class "seek" ]
                [ jump "◀" ("previous " ++ name) (previousOf test v)
                , span [ class "dim" ] [ text name ]
                , jump "▶" ("next " ++ name) (nextOf test v)
                ]
    in
    div [ class "controls" ]
        [ div [ class "row" ]
            [ button [ onClick actions.back ] [ text "← examples" ]
            , strong [] [ text v.trace.file ]
            , jump "◀" "previous event (←)" (within n (k - 1))
            , jump "▶" "next event (→)" (within n (k + 1))
            , input
                [ type_ "range"
                , Html.Attributes.min "0"
                , Html.Attributes.max (String.fromInt n)
                , value (String.fromInt k)
                , onInput (String.toInt >> Maybe.withDefault k >> actions.goto)
                , class "slider"
                ]
                []
            , span [ id "position" ] [ text (String.fromInt k ++ " / " ++ String.fromInt n) ]
            ]
        , div [ class "row" ]
            [ seek "token" isToken
            , seek "emit" isMem
            , seek "check" isCheck
            , seek "diag" isDiag
            , jump "step over" "if the current event enters a production, jump to where it exits" (stepOver v)
            , jump "step out" "jump to the exit of the innermost open production" (stepOut v state)
            ]
        , div [ id "now", class "now" ] [ text (nowText v) ]
        ]


within : Int -> Int -> Maybe Int
within n k =
    if k >= 0 && k <= n then
        Just k

    else
        Nothing


{-| Position after the first matching event at or after the next one.
-}
nextOf : (Event -> Bool) -> Viewer -> Maybe Int
nextOf test v =
    let
        go j =
            case Array.get j v.trace.events of
                Just stamped ->
                    if test stamped.event then
                        Just (j + 1)

                    else
                        go (j + 1)

                Nothing ->
                    Nothing
    in
    go v.k


{-| Position after the last matching event before the current one.
-}
previousOf : (Event -> Bool) -> Viewer -> Maybe Int
previousOf test v =
    let
        go j =
            case Array.get j v.trace.events of
                Just stamped ->
                    if test stamped.event then
                        Just (j + 1)

                    else
                        go (j - 1)

                Nothing ->
                    Nothing
    in
    go (v.k - 2)


stepOver : Viewer -> Maybe Int
stepOver v =
    case Array.get (v.k - 1) v.trace.events |> Maybe.map .event of
        Just (Enter _) ->
            Dict.get (v.k - 1) v.built.matchExit |> Maybe.map ((+) 1)

        _ ->
            within (Array.length v.trace.events) (v.k + 1)


stepOut : Viewer -> State -> Maybe Int
stepOut v state =
    if state.tree.focus.enteredAt < 0 then
        Nothing

    else
        Dict.get state.tree.focus.enteredAt v.built.matchExit |> Maybe.map ((+) 1)


isToken : Event -> Bool
isToken event =
    case event of
        Consume _ ->
            True

        _ ->
            False


isMem : Event -> Bool
isMem event =
    case event of
        Mem _ ->
            True

        _ ->
            False


isCheck : Event -> Bool
isCheck event =
    case event of
        Check _ _ _ ->
            True

        SymFind _ _ ->
            True

        _ ->
            False


isDiag : Event -> Bool
isDiag event =
    case event of
        Diag _ _ _ ->
            True

        _ ->
            False


nowText : Viewer -> String
nowText v =
    case Array.get (v.k - 1) v.trace.events of
        Nothing ->
            "Before the first event. Press → or ▶ to start."

        Just stamped ->
            "event " ++ String.fromInt stamped.i ++ ": " ++ describe v.trace stamped ++ atText stamped.at


atText : Maybe T.Pos -> String
atText at =
    case at of
        Just pos ->
            "   (after the token at " ++ posText pos ++ ")"

        Nothing ->
            ""


posText : T.Pos -> String
posText pos =
    String.fromInt pos.line ++ ":" ++ String.fromInt pos.col


describe : Trace -> Stamped -> String
describe trace stamped =
    let
        n =
            String.fromInt

        tokText tok =
            terminal trace tok.kind ++ " " ++ quoted tok.val ++ " at " ++ n tok.line ++ ":" ++ n tok.col
    in
    case stamped.event of
        Scan tok pragma ->
            "scan "
                ++ tokText tok
                ++ (if pragma then
                        " (a pragma, so it never becomes the lookahead)"

                    else
                        ", the new lookahead"
                   )

        Consume tok ->
            "consume " ++ tokText tok

        Enter prod ->
            "enter " ++ prod

        Exit prod ->
            "exit " ++ prod

        ScopeOpen depth ->
            "open a scope (depth " ++ n depth ++ ")"

        ScopeClose depth ->
            "close a scope (depth " ++ n depth ++ ")"

        SymInsert sym depth clash ->
            "declare "
                ++ symText sym
                ++ " at depth "
                ++ n depth
                ++ (if clash then
                        ", clashing with an earlier declaration"

                    else
                        ""
                   )

        SymFind name found ->
            "look up "
                ++ name
                ++ (case found of
                        Just sym ->
                            ": " ++ symText sym

                        Nothing ->
                            ": not declared"
                   )

        Check rule args ok ->
            checkText rule args ok

        Mem m ->
            case m.cause of
                Emit ->
                    "emit mem[" ++ n m.addr ++ "] = " ++ n m.new

                Patch ->
                    "back-patch mem[" ++ n m.addr ++ "]: " ++ n m.old ++ " → " ++ n m.new

                FixDsp ->
                    "fix the DSP operand at mem[" ++ n m.addr ++ "]: " ++ n m.old ++ " → " ++ n m.new

                StringData ->
                    "string mem[" ++ n m.addr ++ "] = " ++ charText m.new

        Diag severity pos msg ->
            severityText severity ++ " at " ++ posText pos ++ ": " ++ msg

        End info ->
            "end: "
                ++ (if info.ok then
                        "compiled with no errors"

                    else
                        n info.errors ++ " errors"
                   )
                ++ ", "
                ++ n info.codeLength
                ++ " code words"

        Abort reason ->
            "abort: " ++ reason


terminal : Trace -> Int -> String
terminal trace kind =
    Array.get kind trace.terminals |> Maybe.withDefault ("kind" ++ String.fromInt kind)


quoted : String -> String
quoted s =
    "\"" ++ s ++ "\""


charText : Int -> String
charText code =
    if code == 0 then
        "0 (end of string)"

    else
        "'" ++ String.fromChar (Char.fromCode code) ++ "'"


symText : T.Sym -> String
symText sym =
    sym.name ++ " : " ++ sym.kind ++ " " ++ sym.type_


checkText : String -> List String -> Bool -> String
checkText rule args ok =
    (if ok then
        "✓ "

     else
        "✗ "
    )
        ++ rule
        ++ (if List.isEmpty args then
                ""

            else
                "(" ++ String.join ", " args ++ ")"
           )


severityText : Severity -> String
severityText severity =
    case severity of
        Syntax ->
            "syntax error"

        Semantic ->
            "semantic error"

        Warning ->
            "warning"



-- SOURCE


sourcePane : Viewer -> State -> List (Html msg)
sourcePane v state =
    let
        source =
            v.trace.source

        spans =
            Derive.sourceSpan v.trace state

        marks =
            List.filterMap identity
                [ Maybe.map (\r -> ( r, "consumed" )) spans.consumed
                , Maybe.map (\r -> ( r, "lookahead" )) spans.lookahead
                ]
                |> List.map (\( ( a, b ), cls ) -> ( ( min a (String.length source), min b (String.length source) ), cls ))
                |> List.sortBy (Tuple.first >> Tuple.first)

        markId cls =
            if cls == "lookahead" || spans.lookahead == Nothing then
                [ id "src-mark" ]

            else
                []

        go pos remaining =
            case remaining of
                [] ->
                    [ text (String.dropLeft pos source) ]

                ( ( a, b ), cls ) :: rest ->
                    text (String.slice pos (max pos a) source)
                        :: span (class cls :: markId cls)
                            [ text
                                (if b > a then
                                    String.slice a b source

                                 else
                                    "⟨EOF⟩"
                                )
                            ]
                        :: go (max pos b) rest
    in
    [ pre [ class "source" ] (go 0 marks) ]



-- TOKENS


tokenPane : Viewer -> State -> List (Html msg)
tokenPane v state =
    let
        markIndex =
            case state.pending of
                Just k ->
                    k

                Nothing ->
                    Array.length state.scanned - 1

        row k record =
            tr
                (classList
                    [ ( "lookahead", state.pending == Just k )
                    , ( "consumed", record.consumed )
                    , ( "pragma", record.pragma )
                    ]
                    :: (if k == markIndex then
                            [ id "tok-mark" ]

                        else
                            []
                       )
                )
                [ td [ class "dim" ] [ text (String.fromInt k) ]
                , td [] [ text (terminal v.trace record.tok.kind) ]
                , td [] [ text (quoted record.tok.val) ]
                , td [ class "dim" ] [ text (String.fromInt record.tok.line ++ ":" ++ String.fromInt record.tok.col) ]
                , td [ class "dim" ]
                    [ text
                        (if state.pending == Just k then
                            "lookahead"

                         else if record.pragma then
                            "pragma"

                         else if record.consumed then
                            "consumed"

                         else
                            ""
                        )
                    ]
                ]
    in
    [ table [ class "tokens" ] (Array.toList (Array.indexedMap row state.scanned)) ]



-- PARSE TREE


treePane : Viewer -> State -> List (Html msg)
treePane v state =
    let
        lines =
            Derive.treeLines v.trace state

        lastOpen =
            lines |> List.indexedMap Tuple.pair |> List.filter (Tuple.second >> .open) |> List.map Tuple.first |> List.reverse |> List.head
    in
    if List.isEmpty lines then
        [ p [ class "dim" ] [ text "Nothing parsed yet." ] ]

    else
        List.indexedMap
            (\k line ->
                div
                    (classList [ ( "tree-line", True ), ( "open", line.open ), ( "leaf", line.token ) ]
                        :: style "padding-left" (String.fromInt (line.depth * 14 + 4) ++ "px")
                        :: (if Just k == lastOpen then
                                [ id "tree-mark" ]

                            else
                                []
                           )
                    )
                    [ text
                        (if line.token then
                            quoted line.label

                         else
                            line.label
                        )
                    ]
            )
            lines



-- CODE


codePane : Actions msg -> Viewer -> State -> List (Html msg)
codePane actions v state =
    let
        toggle =
            button [ class "small", onClick actions.toggleRaw ]
                [ text
                    (if v.rawCod then
                        "show annotated"

                     else
                        "show as .cod"
                    )
                ]
    in
    if v.rawCod then
        [ toggle, pre [ id "cod", class "listing" ] [ text (Derive.listing v.trace state) ] ]

    else
        toggle
            :: div [ class "listing" ]
                (div [] [ text "ASSEM" ]
                    :: div [] [ text "BEGIN" ]
                    :: List.map (instrRow v state) (Derive.disassemble v.trace state)
                    ++ [ div [] [ text "END." ] ]
                )
            :: stringPool v state


instrRow : Viewer -> State -> Instr -> Html msg
instrRow v state instr =
    let
        words =
            case instr.operand of
                None ->
                    1

                _ ->
                    2

        covers addr =
            addr >= instr.addr && addr < instr.addr + words

        flashed =
            Maybe.map covers state.lastMem == Just True

        patched =
            case Array.get (v.k - 1) v.trace.events |> Maybe.map .event of
                Just (Mem m) ->
                    if (m.cause == Patch || m.cause == FixDsp) && m.addr == instr.addr + 1 then
                        Just m

                    else
                        Nothing

                _ ->
                    Nothing

        head =
            "  {" ++ String.padLeft 5 ' ' (String.fromInt instr.addr) ++ " } " ++ String.padRight 8 ' ' instr.name

        operand =
            case ( patched, instr.operand ) of
                ( Just m, _ ) ->
                    " " ++ String.fromInt m.old ++ " → " ++ String.fromInt m.new

                ( Nothing, None ) ->
                    ""

                ( Nothing, Pending ) ->
                    " _"

                ( Nothing, Word w ) ->
                    " " ++ String.fromInt w

                ( Nothing, Str s ) ->
                    " \"" ++ String.concat (List.map (Char.toCode >> escapeChar) (String.toList s)) ++ "\""
    in
    div
        (classList [ ( "flash", flashed ), ( "patched", patched /= Nothing ) ]
            :: (if flashed then
                    [ id "code-mark" ]

                else
                    []
               )
        )
        [ text (head ++ operand) ]


{-| The same escapes PVM.listCode uses for PRNS strings.
-}
escapeChar : Int -> String
escapeChar code =
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


{-| Strings grow downwards from memSize; each ends in a 0 word.
-}
stringPool : Viewer -> State -> List (Html msg)
stringPool v state =
    let
        -- each string: its first (highest) address and its character codes, read downwards
        collect addr acc =
            if addr < state.strBottom then
                List.reverse acc

            else
                let
                    codes =
                        takeCodes addr []
                in
                collect (addr - List.length codes - 1) (( addr, codes ) :: acc)

        takeCodes a acc =
            if a < state.strBottom || M.memAt a state == 0 then
                List.reverse acc

            else
                takeCodes (a - 1) (M.memAt a state :: acc)

        row ( start, codes ) =
            let
                flashed =
                    Maybe.map (\a -> a <= start && a >= start - List.length codes) state.lastMem == Just True
            in
            div
                (classList [ ( "flash", flashed ) ]
                    :: (if flashed then
                            [ id "code-mark" ]

                        else
                            []
                       )
                )
                [ text ("  {" ++ String.padLeft 5 ' ' (String.fromInt start) ++ " } \"" ++ String.concat (List.map escapeChar codes) ++ "\"") ]
    in
    if state.strBottom >= v.trace.memSize then
        []

    else
        [ div [ class "pool-head dim" ] [ text "strings (stored downwards from memSize)" ]
        , div [ class "listing" ] (List.map row (collect (v.trace.memSize - 1) []))
        ]



-- SCOPES, CHECKS, DIAGNOSTICS


tablePane : State -> List (Html msg)
tablePane state =
    [ h2 [] [ text "Scopes (innermost first)" ]
    , if List.isEmpty state.scopes then
        p [ class "dim" ] [ text "No scope is open." ]

      else
        table [ class "scopes" ]
            (tr [] [ th [] [ text "depth" ], th [] [ text "name" ], th [] [ text "kind" ], th [] [ text "type" ], th [] [ text "value / offset" ] ]
                :: List.concatMap scopeRows state.scopes
            )
    , h2 [] [ text "Last 20 checks" ]
    , if List.isEmpty state.checks then
        p [ class "dim" ] [ text "None yet." ]

      else
        ul [ class "checks" ]
            (state.checks
                |> List.take 20
                |> List.map
                    (\c ->
                        li [ classList [ ( "ok", c.ok ), ( "bad", not c.ok ) ] ]
                            [ text (checkText c.rule c.args c.ok ++ Maybe.withDefault "" (Maybe.map (\pos -> "   at " ++ posText pos) c.at)) ]
                    )
            )
    , h2 [] [ text "Diagnostics" ]
    , if List.isEmpty state.diags then
        p [ class "dim" ] [ text "None." ]

      else
        ul [ class "diags" ]
            (state.diags
                |> List.reverse
                |> List.map (\d -> li [ classList [ ( "bad", d.severity /= Warning ), ( "warn", d.severity == Warning ) ] ] [ text (posText d.pos ++ "  " ++ severityText d.severity ++ ": " ++ d.msg) ])
            )
    ]


scopeRows : M.Scope -> List (Html msg)
scopeRows scope =
    case List.reverse scope.syms of
        [] ->
            [ tr [] [ td [] [ text (String.fromInt scope.depth) ], td [ class "dim" ] [ text "(empty)" ], td [] [], td [] [], td [] [] ] ]

        syms ->
            List.indexedMap
                (\k sym ->
                    tr []
                        [ td []
                            [ text
                                (if k == 0 then
                                    String.fromInt scope.depth

                                 else
                                    ""
                                )
                            ]
                        , td [] [ text sym.name ]
                        , td [] [ text sym.kind ]
                        , td [] [ text sym.type_ ]
                        , td []
                            [ text
                                (if sym.kind == "const" then
                                    String.fromInt sym.value

                                 else if sym.kind == "var" then
                                    "offset " ++ String.fromInt sym.offset

                                 else
                                    ""
                                )
                            ]
                        ]
                )
                syms



-- STYLE


styles : Html msg
styles =
    Html.node "style" [] [ text css ]


css : String
css =
    """
:root { --bg:#ffffff; --fg:#1d1d1f; --dim:#6b6b70; --line:#d8d8dc; --panel:#f6f6f8;
        --tok:#fff1b8; --la:#cfe5ff; --ok:#1c7c3a; --bad:#b3261e; --warn:#8a5a00;
        --flash:#ffe08a; --open:#0b57d0; }
@media (prefers-color-scheme: dark) {
  :root { --bg:#16171a; --fg:#e6e6e9; --dim:#9a9aa2; --line:#33343a; --panel:#1e1f23;
          --tok:#5a4a12; --la:#1f3d63; --ok:#6fd38c; --bad:#ff8a80; --warn:#f0c060;
          --flash:#6b5612; --open:#8ab4ff; }
}
* { box-sizing: border-box; }
body { margin: 0; background: var(--bg); color: var(--fg); }
.app { font: 13px/1.45 ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; padding: 12px; color: var(--fg); background: var(--bg); min-height: 100vh; }
.start { max-width: 900px; }
h1 { font-size: 20px; margin: 4px 0 6px; }
h2 { font-size: 12px; margin: 10px 0 4px; text-transform: uppercase; letter-spacing: .06em; color: var(--dim); }
button { font: inherit; color: var(--fg); background: var(--panel); border: 1px solid var(--line); border-radius: 4px; padding: 2px 8px; cursor: pointer; }
button:disabled { opacity: .35; cursor: default; }
button.link { border: none; background: none; padding: 0; text-decoration: underline; }
button.small { font-size: 11px; margin-bottom: 4px; }
.dim { color: var(--dim); }
.ok { color: var(--ok); }
.bad { color: var(--bad); }
.warn { color: var(--warn); }
.examples li { margin: 2px 0; }
.banner { padding: 8px 10px; margin-bottom: 10px; border-radius: 4px; border: 1px solid var(--bad); }
.banner p { margin: 4px 0 0; }
.controls { position: sticky; top: 0; background: var(--bg); padding-bottom: 6px; border-bottom: 1px solid var(--line); margin-bottom: 8px; z-index: 1; }
.row { display: flex; flex-wrap: wrap; gap: 6px; align-items: center; margin: 3px 0; }
.slider { flex: 1 1 200px; min-width: 120px; }
.seek { display: inline-flex; gap: 3px; align-items: center; margin-right: 6px; }
.now { padding: 4px 6px; background: var(--panel); border-radius: 4px; white-space: pre-wrap; }
.grid { display: grid; gap: 8px;
        grid-template-columns: minmax(0, 1.2fr) minmax(0, 1fr) minmax(0, 1.1fr);
        grid-template-areas: "src tree code" "tok tree tab"; height: calc(100vh - 150px); min-height: 520px; }
.area-src { grid-area: src; } .area-tree { grid-area: tree; } .area-code { grid-area: code; }
.area-tok { grid-area: tok; } .area-tab { grid-area: tab; }
@media (max-width: 1000px) {
  .grid { grid-template-columns: minmax(0, 1fr); grid-template-areas: "src" "tok" "tree" "code" "tab"; height: auto; }
  .pane { height: 50vh; }
}
.pane { display: flex; flex-direction: column; min-height: 0; border: 1px solid var(--line); border-radius: 4px; }
.pane-head { padding: 3px 8px; background: var(--panel); border-bottom: 1px solid var(--line); font-weight: 600; }
.pane-body { overflow: auto; padding: 6px 8px; flex: 1; min-height: 0; }
pre { margin: 0; }
.source { white-space: pre; tab-size: 4; }
.source .consumed { background: var(--tok); outline: 1px solid var(--warn); }
.source .lookahead { background: var(--la); outline: 1px dashed var(--open); }
table { border-collapse: collapse; }
td, th { padding: 0 8px 0 0; text-align: left; white-space: pre; }
.tokens tr.consumed td { color: var(--dim); }
.tokens tr.lookahead td { background: var(--la); color: var(--fg); }
.tokens tr.pragma td { font-style: italic; color: var(--warn); }
.tree-line { white-space: pre; color: var(--dim); }
.tree-line.open { color: var(--open); font-weight: 700; }
.tree-line.leaf { color: var(--fg); }
.listing { white-space: pre; }
.listing .flash { background: var(--flash); }
.listing .patched { outline: 1px solid var(--warn); }
.pool-head { margin-top: 8px; }
.checks, .diags { list-style: none; padding: 0; margin: 0; }
"""
