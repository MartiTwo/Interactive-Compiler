module Main exposing (main)

{-| Parva Compiler Explorer

An interactive companion to Pat Terry's compilers course (CS301 PLT, Rhodes
University).

The left pane shows one Coco/R production from Parva.atg; the right pane shows
the Java that Coco/R generated from it, together with the Table and CodeGen
methods that the production's semantic actions call into. Stepping through the
production's segments highlights the corresponding code on both sides.

A phase selector re-renders BOTH panes as the compiler looked at that stage of
the course, from bare syntax up to full code generation.

Written by Martin Vosloo and Kago Songo, 2026.

-}

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, h1, node, option, p, section, select, span, text)
import Html.Attributes exposing (class, classList, disabled, selected, title, value)
import Html.Events exposing (onClick, onInput)



-- PHASES -------------------------------------------------------------------
--
-- The five stages in which the course builds the compiler up. Every piece of
-- text in the app -- grammar and Java alike -- is tagged with the phase range
-- in which it exists, and both panes are re-laid-out from those tags.


type Phase
    = Syntax
    | ErrorHandling
    | ScopeRules
    | TypeRules
    | CodeGeneration


allPhases : List Phase
allPhases =
    [ Syntax, ErrorHandling, ScopeRules, TypeRules, CodeGeneration ]


rank : Phase -> Int
rank phase =
    case phase of
        Syntax ->
            1

        ErrorHandling ->
            2

        ScopeRules ->
            3

        TypeRules ->
            4

        CodeGeneration ->
            5


phaseName : Phase -> String
phaseName phase =
    case phase of
        Syntax ->
            "Syntax"

        ErrorHandling ->
            "Error handling"

        ScopeRules ->
            "Scope rules"

        TypeRules ->
            "Kind & type rules"

        CodeGeneration ->
            "Code generation"


phaseBlurb : Phase -> String
phaseBlurb phase =
    case phase of
        Syntax ->
            "Bare EBNF. Coco/R still emits Expect and a default SynErr, because a parser cannot run without noticing that something is wrong -- but it makes no attempt to recover."

        ErrorHandling ->
            "SYNC and WEAK are added to the grammar. They generate the skip loops, ExpectWeak and WeakSeparator that let the parser carry on after an error instead of giving up."

        ScopeRules ->
            "The symbol table appears. Entry objects record what is declared; Table.openScope, closeScope, insert and find decide where each name is visible, and every variable is given a stack frame offset."

        TypeRules ->
            "Types and kinds are now checked. Attributes carry types up through the expression grammar, and isArith, isBool, assignable and compatible decide whether each use is legal."

        CodeGeneration ->
            "The back end is switched on. CodeGen calls emit PVM instructions, and Label objects stand in for branch destinations that are not yet known."



-- PHASE-CONDITIONAL TEXT ---------------------------------------------------
--
-- A Part is a fragment of text that exists over a range of phases. The upper
-- bound matters: at phase 4 "static void BasicType()" is REPLACED by
-- "static int BasicType()", it is not merely added to.


type alias Part =
    { text : String
    , from : Phase
    , until : Maybe Phase
    }


{-| Present in every phase.
-}
core : String -> Part
core body =
    Part body Syntax Nothing


{-| Appears at the given phase and stays.
-}
since : Phase -> String -> Part
since phase body =
    Part body phase Nothing


{-| Present from the start, gone by the given phase (exclusive).
-}
before : Phase -> String -> Part
before phase body =
    Part body Syntax (Just phase)


{-| Present only over the half-open range from .. until.
-}
onlyIn : Phase -> Phase -> String -> Part
onlyIn from until body =
    Part body from (Just until)


partVisible : Phase -> Part -> Bool
partVisible now part =
    let
        started =
            rank now >= rank part.from
    in
    case part.until of
        Nothing ->
            started

        Just limit ->
            started && rank now < rank limit


renderParts : Phase -> List Part -> String
renderParts now =
    List.filter (partVisible now) >> List.map .text >> String.concat


{-| The phase at which this run of parts first shows anything. Used by the
"what's new" tint.
-}
earliest : List Part -> Phase
earliest =
    List.map .from
        >> List.foldl
            (\phase best ->
                if rank phase < rank best then
                    phase

                else
                    best
            )
            CodeGeneration



-- GRAMMAR LAYOUT -----------------------------------------------------------
--
-- The grammar pane cannot be raw source with spans hidden, because phase 1 and
-- phase 5 do not share an indentation. It is rendered instead from lines of
-- fragments, each fragment carrying its own phase range and, optionally, the
-- index of the segment it belongs to.


type alias Frag =
    { owner : Maybe Int
    , space : Bool
    , parts : List Part
    }


{-| Plain text, space-separated from what precedes it, not selectable.
-}
txt : String -> Frag
txt body =
    Frag Nothing True [ core body ]


{-| Phase-varying text, not yet owned by a segment.
-}
varying : List Part -> Frag
varying =
    Frag Nothing True


{-| Attach this fragment to a segment. Several fragments may share an owner:
that is how an opening brace and its matching closing brace behave as one
segment spread across two positions.
-}
own : Int -> Frag -> Frag
own index frag =
    { frag | owner = Just index }


{-| Suppress the leading space.
-}
tight : Frag -> Frag
tight frag =
    { frag | space = False }


fragVisible : Phase -> Frag -> Bool
fragVisible now =
    .parts >> List.any (partVisible now)


type alias GrammarLine =
    { indent : Int
    , frags : List Frag
    }


gline : Int -> List Frag -> GrammarLine
gline =
    GrammarLine


grammarLineVisible : Phase -> GrammarLine -> Bool
grammarLineVisible now =
    .frags >> List.any (fragVisible now)



-- SEGMENTS -----------------------------------------------------------------


type Kind
    = Head
    | NonTerminal
    | Terminal
    | Action
    | Iterate
    | Optional
    | Alternation
    | Sync
    | Weak


kindName : Kind -> String
kindName kind =
    case kind of
        Head ->
            "Production head"

        NonTerminal ->
            "Non-terminal"

        Terminal ->
            "Terminal"

        Action ->
            "Semantic action"

        Iterate ->
            "Iteration"

        Optional ->
            "Option"

        Alternation ->
            "Alternation"

        Sync ->
            "SYNC"

        Weak ->
            "WEAK"


{-| Inclusive line range within a method's own line list.
-}
type alias Span =
    { start : Int
    , stop : Int
    }


type alias Note =
    { title : String
    , body : String
    }


type alias Segment =
    { kind : Kind
    , note : Note
    , parser : Maybe Span
    , table : List String
    , codeGen : List String
    }


{-| A segment mapping onto a range of lines in the production's own method.
-}
seg : Kind -> Int -> Int -> String -> String -> Segment
seg kind start stop noteTitle noteBody =
    { kind = kind
    , note = Note noteTitle noteBody
    , parser = Just (Span start stop)
    , table = []
    , codeGen = []
    }


callsTable : List String -> Segment -> Segment
callsTable methods segment =
    { segment | table = methods }


callsCodeGen : List String -> Segment -> Segment
callsCodeGen methods segment =
    { segment | codeGen = methods }



-- JAVA SOURCE --------------------------------------------------------------


type alias CodeLine =
    { indent : Int
    , parts : List Part
    }


{-| A line that reads the same in every phase.
-}
code : Int -> String -> CodeLine
code indent body =
    CodeLine indent [ core body ]


{-| A line whose text changes with the phase.
-}
codeP : Int -> List Part -> CodeLine
codeP =
    CodeLine


codeLineVisible : Phase -> CodeLine -> Bool
codeLineVisible now =
    .parts >> List.any (partVisible now)


type alias Method =
    { name : String
    , lines : List CodeLine
    }


findMethod : String -> List Method -> Maybe Method
findMethod wanted =
    List.filter (.name >> (==) wanted) >> List.head



-- PRODUCTIONS --------------------------------------------------------------


type alias Production =
    { name : String
    , method : String
    , lines : List GrammarLine
    , segments : Array Segment
    }


{-| Indices of the segments that exist in this phase. This is the brief's
"filtered indices of the helper arrays"; it is derived on each render rather
than stored, so it can never disagree with the current phase.
-}
selectableIndices : Phase -> Production -> List Int
selectableIndices now production =
    List.range 0 (Array.length production.segments - 1)
        |> List.filter (segmentVisible now production.lines)


segmentVisible : Phase -> List GrammarLine -> Int -> Bool
segmentVisible now lines index =
    lines
        |> List.concatMap .frags
        |> List.any (\frag -> frag.owner == Just index && fragVisible now frag)


{-| Earliest phase in which any fragment of this segment appears.
-}
segmentPhase : List GrammarLine -> Int -> Phase
segmentPhase lines index =
    lines
        |> List.concatMap .frags
        |> List.filter (.owner >> (==) (Just index))
        |> List.concatMap .parts
        |> earliest


{-| After a phase change the selected segment may no longer exist. Snap to the
nearest surviving segment at or before it.
-}
clampToVisible : Phase -> Production -> Int -> Int
clampToVisible now production current =
    let
        visible =
            selectableIndices now production
    in
    case ( visible |> List.filter (\index -> index <= current) |> List.reverse, visible ) of
        ( nearest :: _, _ ) ->
            nearest

        ( [], first :: _ ) ->
            first

        ( [], [] ) ->
            0


type Direction
    = Backward
    | Forward


stepFrom : Direction -> Phase -> Production -> Int -> Int
stepFrom direction now production current =
    let
        visible =
            selectableIndices now production
    in
    case direction of
        Forward ->
            visible
                |> List.filter (\index -> index > current)
                |> List.head
                |> Maybe.withDefault current

        Backward ->
            visible
                |> List.filter (\index -> index < current)
                |> List.reverse
                |> List.head
                |> Maybe.withDefault current


-- MODEL --------------------------------------------------------------------


type Pane
    = ParserPane
    | TablePane
    | CodeGenPane


{-| The phase at which each file joins the story. There is no symbol table
before phase 3 and no code generator before phase 5, so those tabs do not
exist yet either.
-}
paneFrom : Pane -> Phase
paneFrom pane =
    case pane of
        ParserPane ->
            Syntax

        TablePane ->
            ScopeRules

        CodeGenPane ->
            CodeGeneration


paneLabel : Pane -> String
paneLabel pane =
    case pane of
        ParserPane ->
            "Parser.java"

        TablePane ->
            "Table.java"

        CodeGenPane ->
            "CodeGen.java"


availablePanes : Phase -> List Pane
availablePanes now =
    [ ParserPane, TablePane, CodeGenPane ]
        |> List.filter (\pane -> rank now >= rank (paneFrom pane))


type Theme
    = Light
    | Dark


flipTheme : Theme -> Theme
flipTheme theme =
    case theme of
        Light ->
            Dark

        Dark ->
            Light


themeClass : Theme -> String
themeClass theme =
    case theme of
        Light ->
            "light"

        Dark ->
            "dark"


themeLabel : Theme -> String
themeLabel theme =
    case theme of
        Light ->
            "dark mode"

        Dark ->
            "light mode"


type alias Model =
    { production : Int
    , segment : Int
    , phase : Phase
    , pane : Pane
    , theme : Theme
    , showNew : Bool
    }


init : Model
init =
    { production = 0
    , segment = 0
    , phase = CodeGeneration
    , pane = ParserPane
    , theme = Light
    , showNew = False
    }


fallbackProduction : Production
fallbackProduction =
    { name = "--", method = "--", lines = [], segments = Array.empty }


productionAt : Int -> Production
productionAt index =
    productions |> Array.get index |> Maybe.withDefault fallbackProduction


currentProduction : Model -> Production
currentProduction model =
    productionAt model.production



-- UPDATE -------------------------------------------------------------------


type Msg
    = PickProduction String
    | Step Direction
    | SetPhase Phase
    | SelectPane Pane
    | ToggleTheme
    | ToggleNew


update : Msg -> Model -> Model
update msg model =
    case msg of
        PickProduction raw ->
            let
                index =
                    raw |> String.toInt |> Maybe.withDefault model.production
            in
            { model
                | production = index
                , segment = clampToVisible model.phase (productionAt index) 0
                , pane = ParserPane
            }

        Step direction ->
            { model
                | segment =
                    stepFrom direction model.phase (currentProduction model) model.segment
            }

        SetPhase phase ->
            { model
                | phase = phase
                , segment = clampToVisible phase (currentProduction model) model.segment
                , pane = keepPane phase model.pane
            }

        SelectPane pane ->
            { model | pane = pane }

        ToggleTheme ->
            { model | theme = flipTheme model.theme }

        ToggleNew ->
            { model | showNew = not model.showNew }


{-| Dropping to a phase in which the current tab does not yet exist sends the
right-hand pane back to Parser.java.
-}
keepPane : Phase -> Pane -> Pane
keepPane phase pane =
    if rank phase >= rank (paneFrom pane) then
        pane

    else
        ParserPane



-- VIEW ---------------------------------------------------------------------


view : Model -> Html Msg
view model =
    let
        production =
            currentProduction model

        visible =
            selectableIndices model.phase production

        segment =
            Array.get model.segment production.segments
    in
    div [ class ("app " ++ themeClass model.theme) ]
        [ node "style" [] [ text stylesheet ]
        , viewHeader model
        , viewToolbar model visible
        , div [ class "split" ]
            [ viewGrammarPane model production
            , viewCodePane model production segment
            ]
        , viewNote model production segment
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    div [ class "bar top" ]
        [ h1 [] [ text "Parva Compiler Explorer" ]
        , div [ class "phases" ] (List.map (viewPhasePill model.phase) allPhases)
        , div [ class "grow" ] []
        , button
            [ classList [ ( "toggle", True ), ( "on", model.showNew ) ]
            , onClick ToggleNew
            , title "Tint whatever this phase introduced"
            ]
            [ text "what's new" ]
        , button [ class "toggle", onClick ToggleTheme ] [ text (themeLabel model.theme) ]
        ]


viewPhasePill : Phase -> Phase -> Html Msg
viewPhasePill current phase =
    button
        [ classList [ ( "pill", True ), ( "on", phase == current ) ]
        , onClick (SetPhase phase)
        , title (phaseBlurb phase)
        ]
        [ span [ class "num" ] [ text (String.fromInt (rank phase)) ]
        , text (phaseName phase)
        ]


viewToolbar : Model -> List Int -> Html Msg
viewToolbar model visible =
    div [ class "bar tools" ]
        [ span [ class "dim" ] [ text "Production" ]
        , select [ onInput PickProduction ]
            (productions
                |> Array.toList
                |> List.indexedMap (viewProductionOption model.production)
            )
        , button
            [ onClick (Step Backward), disabled (List.head visible == Just model.segment) ]
            [ text "\u{25C0} prev" ]
        , button
            [ onClick (Step Forward), disabled (lastOf visible == Just model.segment) ]
            [ text "next \u{25B6}" ]
        , span [ class "counter" ]
            [ text (counterText (positionOf model.segment visible) (List.length visible)) ]
        , span [ class "grow" ] []
        , span [ class "dim blurb" ] [ text (phaseBlurb model.phase) ]
        ]


viewProductionOption : Int -> Int -> Production -> Html Msg
viewProductionOption current index production =
    option
        [ value (String.fromInt index), selected (index == current) ]
        [ text production.name ]


lastOf : List a -> Maybe a
lastOf =
    List.reverse >> List.head


positionOf : Int -> List Int -> Maybe Int
positionOf wanted visible =
    visible
        |> List.indexedMap Tuple.pair
        |> List.filter (Tuple.second >> (==) wanted)
        |> List.head
        |> Maybe.map (Tuple.first >> (+) 1)


counterText : Maybe Int -> Int -> String
counterText position total =
    case position of
        Just n ->
            "segment " ++ String.fromInt n ++ " of " ++ String.fromInt total

        Nothing ->
            "no segment in this phase"



-- GRAMMAR PANE -------------------------------------------------------------


viewGrammarPane : Model -> Production -> Html Msg
viewGrammarPane model production =
    section [ class "pane" ]
        [ div [ class "pane-head" ]
            [ span [ class "pane-title on" ] [ text "Parva.atg" ]
            , span [ class "dim" ] [ text ("PRODUCTIONS / " ++ production.name) ]
            ]
        , div [ class "src" ]
            (production.lines
                |> List.filter (grammarLineVisible model.phase)
                |> List.map (viewGrammarLine model)
            )
        ]


viewGrammarLine : Model -> GrammarLine -> Html Msg
viewGrammarLine model line =
    div [ class "line" ]
        (text (String.repeat line.indent " ")
            :: (line.frags
                    |> List.filter (fragVisible model.phase)
                    |> List.indexedMap (viewFrag model)
               )
        )


viewFrag : Model -> Int -> Frag -> Html Msg
viewFrag model position frag =
    let
        lead =
            if position == 0 || not frag.space then
                ""

            else
                " "

        shown =
            List.filter (partVisible model.phase) frag.parts
    in
    span
        [ classList
            [ ( "frag", True )
            , ( "sel", frag.owner == Just model.segment )
            , ( "new", model.showNew && earliest shown == model.phase )
            ]
        ]
        [ text (lead ++ renderParts model.phase frag.parts) ]



-- CODE PANE ----------------------------------------------------------------


viewCodePane : Model -> Production -> Maybe Segment -> Html Msg
viewCodePane model production segment =
    section [ class "pane" ]
        [ div [ class "pane-head" ]
            (availablePanes model.phase |> List.map (viewTab model segment))
        , viewPaneBody model production segment
        ]


viewTab : Model -> Maybe Segment -> Pane -> Html Msg
viewTab model segment pane =
    button
        [ classList
            [ ( "tab", True )
            , ( "on", pane == model.pane )
            , ( "flagged", paneRelevant segment pane )
            ]
        , onClick (SelectPane pane)
        ]
        [ text (paneLabel pane)
        , viewDot (paneRelevant segment pane)
        ]


viewDot : Bool -> Html Msg
viewDot flagged =
    case flagged of
        True ->
            span [ class "dot" ] [ text "\u{25CF}" ]

        False ->
            text ""


{-| Does the selected segment reach into this file?
-}
paneRelevant : Maybe Segment -> Pane -> Bool
paneRelevant segment pane =
    case ( segment, pane ) of
        ( Just found, TablePane ) ->
            not (List.isEmpty found.table)

        ( Just found, CodeGenPane ) ->
            not (List.isEmpty found.codeGen)

        _ ->
            False


viewPaneBody : Model -> Production -> Maybe Segment -> Html Msg
viewPaneBody model production segment =
    case model.pane of
        ParserPane ->
            viewParserCode model
                (segment |> Maybe.andThen .parser)
                (findMethod production.method parserSrc
                    |> Maybe.map .lines
                    |> Maybe.withDefault []
                )

        TablePane ->
            viewCallees model tableSrc (segment |> Maybe.map .table |> Maybe.withDefault [])

        CodeGenPane ->
            viewCallees model codeGenSrc (segment |> Maybe.map .codeGen |> Maybe.withDefault [])


viewParserCode : Model -> Maybe Span -> List CodeLine -> Html Msg
viewParserCode model target lines =
    div [ class "src" ]
        (lines
            |> List.indexedMap Tuple.pair
            |> List.filter (Tuple.second >> codeLineVisible model.phase)
            |> List.map (\( index, line ) -> viewCodeLine model (withinSpan target index) line)
        )


withinSpan : Maybe Span -> Int -> Bool
withinSpan target index =
    case target of
        Nothing ->
            False

        Just found ->
            index >= found.start && index <= found.stop


{-| The Table and CodeGen tabs show the bodies the selected segment actually
calls. With nothing selected in that file they fall back to the whole file, so
the tab is still worth browsing.
-}
viewCallees : Model -> List Method -> List String -> Html Msg
viewCallees model source names =
    case names of
        [] ->
            div [ class "src" ]
                (div [ class "hint" ]
                    [ text "No call from this segment. Showing the whole file." ]
                    :: List.concatMap (viewMethodBlock model False) source
                )

        _ ->
            div [ class "src" ]
                (names
                    |> List.filterMap (\name -> findMethod name source)
                    |> List.concatMap (viewMethodBlock model True)
                )


viewMethodBlock : Model -> Bool -> Method -> List (Html Msg)
viewMethodBlock model highlighted method =
    div [ class "method-head" ] [ text method.name ]
        :: (method.lines
                |> List.filter (codeLineVisible model.phase)
                |> List.map (viewCodeLine model highlighted)
           )


viewCodeLine : Model -> Bool -> CodeLine -> Html Msg
viewCodeLine model highlighted line =
    let
        shown =
            List.filter (partVisible model.phase) line.parts
    in
    div
        [ classList
            [ ( "line", True )
            , ( "sel", highlighted )
            , ( "new", model.showNew && earliest shown == model.phase )
            ]
        ]
        [ text (String.repeat line.indent " " ++ renderParts model.phase line.parts) ]



-- EXPLANATION STRIP --------------------------------------------------------


viewNote : Model -> Production -> Maybe Segment -> Html Msg
viewNote model production segment =
    case segment of
        Nothing ->
            div [ class "note" ]
                [ p [ class "note-body dim" ]
                    [ text "Nothing in this production survives at this phase." ]
                ]

        Just found ->
            div [ class "note" ]
                [ div [ class "note-head" ]
                    [ span [ class "chip" ] [ text (kindName found.kind) ]
                    , span [ class "note-title" ] [ text found.note.title ]
                    , span [ class "chip soft" ]
                        [ text
                            ("added in phase "
                                ++ String.fromInt
                                    (rank (segmentPhase production.lines model.segment))
                            )
                        ]
                    ]
                , p [ class "note-body" ] [ text found.note.body ]
                ]



-- MAIN ---------------------------------------------------------------------


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


-- STYLESHEET ---------------------------------------------------------------
--
-- Kept as one embedded stylesheet rather than inline attributes so that media
-- queries, hover states and a one-class theme switch all work from a single
-- file that can be pasted straight into Ellie.


stylesheet : String
stylesheet =
    """
*, *::before, *::after { box-sizing: border-box; }
html, body { margin: 0; padding: 0; }

.app {
  --bg:      #faf8f3;
  --panel:   #ffffff;
  --ink:     #221f1a;
  --dim:     #857d70;
  --rule:    #e2dccf;
  --hl:      #ffe9a3;
  --hlEdge:  #c08a1b;
  --accent:  #14587f;
  --onInk:   #ffffff;
  --new:     #dcefe2;
  --newEdge: #3d8f5c;

  height: 100vh;
  display: flex;
  flex-direction: column;
  overflow: hidden;
  background: var(--bg);
  color: var(--ink);
  font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, 'DejaVu Sans Mono', monospace;
  font-size: 13.5px;
  line-height: 1.6;
}

.app.dark {
  --bg:      #16181c;
  --panel:   #1f2228;
  --ink:     #e8e4db;
  --dim:     #8b9099;
  --rule:    #333841;
  --hl:      #59480f;
  --hlEdge:  #d8ab24;
  --accent:  #6fb4e2;
  --onInk:   #0f1114;
  --new:     #1a3a28;
  --newEdge: #4fb37e;
}

/* ---- bars ---- */

.bar {
  display: flex;
  align-items: center;
  gap: 10px;
  padding: 7px 14px;
  background: var(--panel);
  border-bottom: 1px solid var(--rule);
  flex-wrap: wrap;
  flex: none;
}

.bar h1 {
  margin: 0;
  font-size: 14px;
  font-weight: 700;
  letter-spacing: 0.01em;
  white-space: nowrap;
}

.grow { flex: 1 1 auto; }

.dim { color: var(--dim); }

.blurb {
  font-size: 11.5px;
  max-width: 46ch;
  line-height: 1.35;
  text-align: right;
}

.counter {
  color: var(--dim);
  font-size: 12px;
  white-space: nowrap;
}

/* ---- controls ---- */

.phases { display: flex; gap: 3px; flex-wrap: wrap; }

button, select {
  font: inherit;
  font-size: 12px;
  color: var(--ink);
  background: var(--panel);
  border: 1px solid var(--rule);
  border-radius: 4px;
  padding: 3px 9px;
  cursor: pointer;
}

button:hover:not(:disabled), select:hover { border-color: var(--accent); }

button:disabled { opacity: 0.35; cursor: default; }

.pill { display: inline-flex; align-items: center; gap: 6px; }

.pill .num {
  display: inline-block;
  min-width: 15px;
  text-align: center;
  border-radius: 3px;
  background: var(--rule);
  color: var(--ink);
  font-size: 10.5px;
  line-height: 15px;
}

.pill.on, .toggle.on {
  background: var(--accent);
  border-color: var(--accent);
  color: var(--onInk);
}

.pill.on .num { background: rgba(255,255,255,0.28); color: var(--onInk); }

/* ---- panes ---- */

.split { display: flex; flex: 1 1 auto; min-height: 0; }

.pane {
  flex: 1 1 50%;
  min-width: 0;
  min-height: 0;
  display: flex;
  flex-direction: column;
  border-right: 1px solid var(--rule);
}

.pane:last-child { border-right: none; }

.pane-head {
  display: flex;
  align-items: center;
  gap: 8px;
  padding: 5px 12px;
  background: var(--panel);
  border-bottom: 1px solid var(--rule);
  flex: none;
  font-size: 12px;
  min-height: 32px;
}

.pane-title { font-weight: 700; }

.tab { border-color: transparent; background: transparent; }

.tab.on {
  background: var(--accent);
  border-color: var(--accent);
  color: var(--onInk);
  font-weight: 700;
}

.dot { color: var(--hlEdge); font-size: 9px; margin-left: 5px; vertical-align: middle; }

.tab.on .dot { color: var(--onInk); }

.src {
  flex: 1 1 auto;
  min-height: 0;
  overflow: auto;
  padding: 10px 0 24px 0;
}

.line {
  white-space: pre;
  padding: 0 14px;
  border-left: 3px solid transparent;
}

.line.sel { background: var(--hl); border-left-color: var(--hlEdge); }

.line.new:not(.sel) { background: var(--new); border-left-color: var(--newEdge); }

.frag { white-space: pre; border-radius: 2px; }

.frag.sel { background: var(--hl); box-shadow: 0 0 0 1px var(--hlEdge); }

.frag.new:not(.sel) { background: var(--new); box-shadow: 0 0 0 1px var(--newEdge); }

.method-head {
  margin: 14px 0 2px 0;
  padding: 2px 14px;
  font-size: 11px;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  color: var(--dim);
  border-top: 1px solid var(--rule);
  padding-top: 8px;
}

.src > .method-head:first-child { margin-top: 0; border-top: none; padding-top: 2px; }

.hint { padding: 2px 14px 6px 14px; font-size: 11.5px; color: var(--dim); }

/* ---- explanation strip ---- */

.note {
  flex: none;
  padding: 9px 16px 12px 16px;
  background: var(--panel);
  border-top: 2px solid var(--rule);
  max-height: 30vh;
  overflow: auto;
}

.note-head { display: flex; align-items: center; gap: 9px; flex-wrap: wrap; }

.note-title { font-weight: 700; font-size: 13px; }

.chip {
  font-size: 10.5px;
  letter-spacing: 0.05em;
  text-transform: uppercase;
  padding: 1px 7px;
  border-radius: 999px;
  background: var(--accent);
  color: var(--onInk);
  white-space: nowrap;
}

.chip.soft { background: var(--rule); color: var(--dim); }

.note-body {
  margin: 5px 0 0 0;
  font-size: 12.5px;
  line-height: 1.55;
  max-width: 115ch;
}

/* ---- narrow screens ---- */

@media (max-width: 900px) {
  .app { height: auto; min-height: 100vh; overflow: visible; }
  .split { flex-direction: column; }
  .pane { flex: 0 0 auto; border-right: none; border-bottom: 1px solid var(--rule); }
  .src { max-height: 46vh; padding: 8px 0 10px 0; }
  .blurb { display: none; }
  .note { max-height: none; }
}
"""


-- PARSER.JAVA --------------------------------------------------------------
--
-- One entry per production, holding the method Coco/R generated for it.
-- Line indices are relative to each method's own list, which is what the
-- segments' spans refer to.
--
-- Phase notes for this file:
--   * ExpectWeak / WeakSeparator / SYNC loops and every Warning replace or
--     join their phase-1 forms at ErrorHandling.
--   * Entry, Table.*, names and stack frame offsets arrive at ScopeRules.
--   * Types.*, the out-parameters that carry types, and every assignable /
--     isArith / compatible test arrive at TypeRules.
--   * Label and every CodeGen call arrive at CodeGeneration.


parserSrc : List Method
parserSrc =
    [ Method "Parva" parvaMethod
    , Method "Statement" statementMethod
    , Method "Block" blockMethod
    , Method "IfStatement" ifStatementMethod
    , Method "WhileStatement" whileStatementMethod
    , Method "VarList" varListMethod
    , Method "OneVar" oneVarMethod
    , Method "Designator" designatorMethod
    , Method "Primary" primaryMethod
    , Method "AddOp" addOpMethod
    , Method "Ident" identMethod
    ]


parvaMethod : List CodeLine
parvaMethod =
    [ code 0 "static void Parva() {"
    , code 4 "Expect(void_Sym);"
    , codeP 4 [ since ScopeRules "Entry program = new Entry();" ]
    , codeP 4
        [ before ScopeRules "Ident();"
        , since ScopeRules "program.name = Ident();"
        ]
    , code 4 "Expect(lparen_Sym);"
    , code 4 "Expect(rparen_Sym);"
    , codeP 4 [ since ScopeRules "program.kind = Kinds.Fun;" ]
    , codeP 4 [ since TypeRules "program.type = Types.voidType;" ]
    , codeP 4 [ since ScopeRules "Table.insert(program);" ]
    , codeP 4 [ since ScopeRules "StackFrame frame = new StackFrame();" ]
    , codeP 4 [ since ScopeRules "Table.openScope();" ]
    , codeP 4 [ since CodeGeneration "Label DSPLabel = new Label(known);" ]
    , codeP 4 [ since CodeGeneration "CodeGen.openStackFrame(0);" ]
    , codeP 4
        [ before ErrorHandling "Expect(lbrace_Sym);"
        , since ErrorHandling "ExpectWeak(lbrace_Sym, 1);"
        ]
    , code 4 "while (StartOf(2)) {"
    , codeP 8
        [ before ScopeRules "Statement();"
        , since ScopeRules "Statement(frame);"
        ]
    , code 4 "}"
    , codeP 4
        [ before ErrorHandling "Expect(rbrace_Sym);"
        , since ErrorHandling "ExpectWeak(rbrace_Sym, 3);"
        ]
    , codeP 4 [ since CodeGeneration "CodeGen.fixDSP(DSPLabel.address(), frame.size);" ]
    , codeP 4 [ since CodeGeneration "CodeGen.leaveProgram();" ]
    , codeP 4 [ since ScopeRules "Table.closeScope();" ]
    , code 0 "}"
    ]


statementMethod : List CodeLine
statementMethod =
    [ codeP 0
        [ core "static void Statement("
        , since ScopeRules "StackFrame frame"
        , core ") {"
        ]
    , codeP 4 [ since ErrorHandling "while (!(StartOf(3))) {SynErr(54); Get();}" ]
    , code 4 "switch (la.kind) {"
    , code 4 "case lbrace_Sym: {"
    , codeP 8 [ before ScopeRules "Block();", since ScopeRules "Block(frame);" ]
    , code 8 "break;"
    , code 4 "}"
    , code 4 "case const_Sym: {"
    , code 8 "ConstDeclarations();"
    , code 8 "break;"
    , code 4 "}"
    , code 4 "case int_Sym: case bool_Sym: case char_Sym: {"
    , codeP 8
        [ before ScopeRules "VarDeclarations();"
        , since ScopeRules "VarDeclarations(frame);"
        ]
    , code 8 "break;"
    , code 4 "}"
    , code 4 "case identifier_Sym: case plusplus_Sym: case minusminus_Sym: {"
    , code 8 "AssignmentStatement();"
    , code 8 "break;"
    , code 4 "}"
    , code 4 "case if_Sym: {"
    , codeP 8 [ before ScopeRules "IfStatement();", since ScopeRules "IfStatement(frame);" ]
    , code 8 "break;"
    , code 4 "}"
    , code 4 "case while_Sym: {"
    , codeP 8
        [ before ScopeRules "WhileStatement();"
        , since ScopeRules "WhileStatement(frame);"
        ]
    , code 8 "break;"
    , code 4 "}"
    , code 4 "case do_Sym: {"
    , codeP 8
        [ before ScopeRules "DoWhileStatement();"
        , since ScopeRules "DoWhileStatement(frame);"
        ]
    , code 8 "break;"
    , code 4 "}"
    , code 4 "case break_Sym: {"
    , code 8 "BreakStatement();"
    , code 8 "break;"
    , code 4 "}"
    , code 4 "case halt_Sym: {"
    , code 8 "HaltStatement();"
    , code 8 "break;"
    , code 4 "}"
    , code 4 "case read_Sym: case readLine_Sym: {"
    , code 8 "ReadStatement();"
    , code 8 "break;"
    , code 4 "}"
    , code 4 "case write_Sym: case writeLine_Sym: {"
    , code 8 "WriteStatement();"
    , code 8 "break;"
    , code 4 "}"
    , code 4 "case semicolon_Sym: {"
    , code 8 "Get();"
    , codeP 8 [ since ErrorHandling "if (warnings) Warning(\"empty statement\");" ]
    , code 8 "break;"
    , code 4 "}"
    , code 4 "default: SynErr(55); break;"
    , code 4 "}"
    , code 0 "}"
    ]


blockMethod : List CodeLine
blockMethod =
    [ codeP 0
        [ core "static void Block("
        , since ScopeRules "StackFrame frame"
        , core ") {"
        ]
    , codeP 4 [ since ScopeRules "Table.openScope();" ]
    , codeP 4 [ since ErrorHandling "boolean empty = true;" ]
    , code 4 "Expect(lbrace_Sym);"
    , code 4 "while (StartOf(2)) {"
    , codeP 8 [ before ScopeRules "Statement();", since ScopeRules "Statement(frame);" ]
    , codeP 8 [ since ErrorHandling "empty = false;" ]
    , code 4 "}"
    , codeP 4 [ since ErrorHandling "if (empty && warnings) Warning(\"empty {} block\");" ]
    , codeP 4
        [ before ErrorHandling "Expect(rbrace_Sym);"
        , since ErrorHandling "ExpectWeak(rbrace_Sym, 4);"
        ]
    , codeP 4 [ since ScopeRules "Table.closeScope();" ]
    , code 0 "}"
    ]


ifStatementMethod : List CodeLine
ifStatementMethod =
    [ codeP 0
        [ core "static void IfStatement("
        , since ScopeRules "StackFrame frame"
        , core ") {"
        ]
    , codeP 4 [ since CodeGeneration "Label falseLabel = new Label(!known);" ]
    , codeP 4 [ since CodeGeneration "Label exitLabel = null;" ]
    , codeP 4 [ since CodeGeneration "boolean hasElse = false;" ]
    , code 4 "Expect(if_Sym);"
    , code 4 "Expect(lparen_Sym);"
    , code 4 "Condition();"
    , code 4 "Expect(rparen_Sym);"
    , codeP 4 [ since CodeGeneration "CodeGen.branchFalse(falseLabel);" ]
    , codeP 4 [ before ScopeRules "Statement();", since ScopeRules "Statement(frame);" ]
    , code 4 "if (la.kind == else_Sym) {"
    , code 8 "Get();"
    , codeP 8 [ since CodeGeneration "hasElse = true;" ]
    , codeP 8 [ since CodeGeneration "exitLabel = new Label(!known);" ]
    , codeP 8 [ since CodeGeneration "CodeGen.branch(exitLabel);" ]
    , codeP 8 [ since CodeGeneration "falseLabel.here();" ]
    , codeP 8 [ before ScopeRules "Statement();", since ScopeRules "Statement(frame);" ]
    , code 4 "}"
    , codeP 4 [ since CodeGeneration "if (hasElse) exitLabel.here();" ]
    , codeP 4 [ since CodeGeneration "else falseLabel.here();" ]
    , code 0 "}"
    ]


whileStatementMethod : List CodeLine
whileStatementMethod =
    [ codeP 0
        [ core "static void WhileStatement("
        , since ScopeRules "StackFrame frame"
        , core ") {"
        ]
    , codeP 4 [ since CodeGeneration "Label loopExit  = new Label(!known);" ]
    , codeP 4 [ since CodeGeneration "Label loopStart = new Label(known);" ]
    , codeP 4 [ since CodeGeneration "CodeGen.brkLabels[CodeGen.bl++] = loopExit;" ]
    , code 4 "Expect(while_Sym);"
    , code 4 "Expect(lparen_Sym);"
    , code 4 "Condition();"
    , code 4 "Expect(rparen_Sym);"
    , codeP 4 [ since CodeGeneration "CodeGen.branchFalse(loopExit);" ]
    , codeP 4 [ before ScopeRules "Statement();", since ScopeRules "Statement(frame);" ]
    , codeP 4 [ since CodeGeneration "CodeGen.branch(loopStart);" ]
    , codeP 4 [ since CodeGeneration "loopExit.here();" ]
    , codeP 4 [ since CodeGeneration "CodeGen.bl--;" ]
    , code 0 "}"
    ]


varListMethod : List CodeLine
varListMethod =
    [ codeP 0
        [ core "static void VarList("
        , since ScopeRules "StackFrame frame"
        , since TypeRules ", int type"
        , core ") {"
        ]
    , codeP 4
        [ before ScopeRules "OneVar();"
        , onlyIn ScopeRules TypeRules "OneVar(frame);"
        , since TypeRules "OneVar(frame, type);"
        ]
    , codeP 4
        [ before ErrorHandling "while (la.kind == comma_Sym) {"
        , since ErrorHandling "while (WeakSeparator(comma_Sym, 5, 6)) {"
        ]
    , codeP 8 [ before ErrorHandling "Get();" ]
    , codeP 8
        [ before ScopeRules "OneVar();"
        , onlyIn ScopeRules TypeRules "OneVar(frame);"
        , since TypeRules "OneVar(frame, type);"
        ]
    , code 4 "}"
    , code 0 "}"
    ]


oneVarMethod : List CodeLine
oneVarMethod =
    [ codeP 0
        [ core "static void OneVar("
        , since ScopeRules "StackFrame frame"
        , since TypeRules ", int type"
        , core ") {"
        ]
    , codeP 4 [ since TypeRules "int expType;" ]
    , codeP 4 [ since ScopeRules "Entry var = new Entry();" ]
    , codeP 4 [ before ScopeRules "Ident();", since ScopeRules "var.name = Ident();" ]
    , codeP 4 [ since ScopeRules "var.kind = Kinds.Var;" ]
    , codeP 4 [ since TypeRules "var.type = type;" ]
    , codeP 4 [ since ScopeRules "var.offset = frame.size;" ]
    , codeP 4 [ since ScopeRules "frame.size++;" ]
    , code 4 "if (la.kind == equal_Sym) {"
    , code 8 "AssignOp();"
    , codeP 8 [ since CodeGeneration "CodeGen.loadAddress(var);" ]
    , codeP 8
        [ before TypeRules "Expression();"
        , since TypeRules "expType = Expression();"
        ]
    , codeP 8 [ since TypeRules "if (!assignable(var.type, expType))" ]
    , codeP 10 [ since TypeRules "SemError(\"incompatible types in assignment\");" ]
    , codeP 8 [ since CodeGeneration "CodeGen.assign(var.type);" ]
    , code 4 "} else if (la.kind == semicolon_Sym || la.kind == comma_Sym) {"
    , code 4 "} else SynErr(62);"
    , codeP 4 [ since ScopeRules "Table.insert(var);" ]
    , code 0 "}"
    ]


designatorMethod : List CodeLine
designatorMethod =
    [ codeP 0
        [ core "static "
        , before ScopeRules "void"
        , since ScopeRules "DesType"
        , core " Designator() {"
        ]
    , codeP 4 [ since ScopeRules "DesType des;" ]
    , codeP 4 [ since ScopeRules "String name;" ]
    , codeP 4 [ since TypeRules "int indexType;" ]
    , codeP 4 [ before ScopeRules "Ident();", since ScopeRules "name = Ident();" ]
    , codeP 4 [ since ScopeRules "Entry entry = Table.find(name);" ]
    , codeP 4 [ since ScopeRules "boolean notDeclared = !entry.declared;" ]
    , codeP 4 [ since ScopeRules "if (notDeclared) {" ]
    , codeP 6 [ since ScopeRules "SemError(\"undeclared identifier\");" ]
    , codeP 6 [ since ScopeRules "entry = new Entry(); // new is critical" ]
    , codeP 6 [ since ScopeRules "entry.name = name;   // make an entry anyway" ]
    , codeP 6 [ since ScopeRules "entry.kind = Kinds.Var;" ]
    , codeP 6 [ since TypeRules "entry.type = Types.noType;" ]
    , codeP 6 [ since ScopeRules "entry.offset = 0;    // unimportant" ]
    , codeP 6 [ since ScopeRules "Table.insert(entry);" ]
    , codeP 4 [ since ScopeRules "}" ]
    , codeP 4 [ since ScopeRules "des = new DesType(entry);" ]
    , codeP 4 [ since CodeGeneration "if (entry.kind == Kinds.Var)" ]
    , codeP 6 [ since CodeGeneration "CodeGen.loadAddress(entry);" ]
    , code 4 "if (la.kind == lbrack_Sym) {"
    , code 8 "Get();"
    , codeP 8 [ since TypeRules "if (notDeclared) entry.type++;" ]
    , codeP 8 [ since TypeRules "else if (isArray(des.type)) des.type--;" ]
    , codeP 8 [ since TypeRules "else SemError(\"unexpected subscript\");" ]
    , codeP 8 [ since TypeRules "if (des.entry.kind != Kinds.Var)" ]
    , codeP 10 [ since TypeRules "SemError(\"unexpected subscript\");" ]
    , codeP 8 [ since CodeGeneration "CodeGen.dereference();" ]
    , codeP 8
        [ before TypeRules "Expression();"
        , since TypeRules "indexType = Expression();"
        ]
    , codeP 8 [ since TypeRules "if (!isArith(indexType))" ]
    , codeP 10 [ since TypeRules "SemError(\"invalid subscript type\");" ]
    , codeP 8 [ since CodeGeneration "CodeGen.index();" ]
    , code 8 "Expect(rbrack_Sym);"
    , code 4 "}"
    , codeP 4 [ since ScopeRules "return des;" ]
    , code 0 "}"
    ]


primaryMethod : List CodeLine
primaryMethod =
    [ codeP 0
        [ core "static "
        , before TypeRules "void"
        , since TypeRules "int"
        , core " Primary() {"
        ]
    , codeP 4 [ since TypeRules "int type;" ]
    , codeP 4 [ since TypeRules "type = Types.noType;" ]
    , codeP 4
        [ onlyIn TypeRules CodeGeneration "int size;"
        , since CodeGeneration "int size; boolean upper = true;"
        ]
    , codeP 4 [ since ScopeRules "DesType des;" ]
    , codeP 4 [ since TypeRules "ConstRec con;" ]
    , code 4 "switch (la.kind) {"
    , code 4 "case identifier_Sym: {"
    , codeP 8 [ before ScopeRules "Designator();", since ScopeRules "des = Designator();" ]
    , codeP 8 [ since TypeRules "type = des.type;" ]
    , codeP 8 [ since CodeGeneration "switch (des.entry.kind) {" ]
    , codeP 8 [ since CodeGeneration "  case Kinds.Var:" ]
    , codeP 10 [ since CodeGeneration "  CodeGen.dereference();" ]
    , codeP 10 [ since CodeGeneration "  break;" ]
    , codeP 8 [ since CodeGeneration "  case Kinds.Con:" ]
    , codeP 10 [ since CodeGeneration "  CodeGen.loadConstant(des.entry.value);" ]
    , codeP 10 [ since CodeGeneration "  break;" ]
    , codeP 8 [ since CodeGeneration "  default:" ]
    , codeP 10 [ since CodeGeneration "  SemError(\"wrong kind of identifier\");" ]
    , codeP 10 [ since CodeGeneration "  break;" ]
    , codeP 8 [ since CodeGeneration "}" ]
    , code 8 "break;"
    , code 4 "}"
    , code 4 "case number_Sym: case charLit_Sym: case true_Sym: case false_Sym: case null_Sym: {"
    , codeP 8 [ before TypeRules "Constant();", since TypeRules "con = Constant();" ]
    , codeP 8 [ since TypeRules "type = con.type;" ]
    , codeP 8 [ since CodeGeneration "CodeGen.loadConstant(con.value);" ]
    , code 8 "break;"
    , code 4 "}"
    , code 4 "case new_Sym: {"
    , code 8 "Get();"
    , codeP 8 [ before TypeRules "BasicType();", since TypeRules "type = BasicType();" ]
    , code 8 "Expect(lbrack_Sym);"
    , codeP 8 [ before TypeRules "Expression();", since TypeRules "size = Expression();" ]
    , codeP 8 [ since TypeRules "if (!isArith(size))" ]
    , codeP 10 [ since TypeRules "SemError(\"array size must be integer\");" ]
    , codeP 8 [ since TypeRules "type++;" ]
    , code 8 "Expect(rbrack_Sym);"
    , codeP 8 [ since CodeGeneration "CodeGen.allocate();" ]
    , code 8 "break;"
    , code 4 "}"
    , code 4 "case lparen_Sym: {"
    , code 8 "Get();"
    , code 8 "if (la.kind == char_Sym) {"
    , code 12 "Get();"
    , code 12 "Expect(rparen_Sym);"
    , codeP 12 [ before TypeRules "Factor();", since TypeRules "type = Factor();" ]
    , codeP 12 [ since TypeRules "if (!isArith(type))" ]
    , codeP 14 [ since TypeRules "SemError(\"invalid cast\");" ]
    , codeP 12
        [ onlyIn TypeRules CodeGeneration "else type = Types.charType;"
        , since CodeGeneration "else { type = Types.charType; CodeGen.castToChar(); }"
        ]
    , code 8 "} else if (la.kind == int_Sym) {"
    , code 12 "Get();"
    , code 12 "Expect(rparen_Sym);"
    , codeP 12 [ before TypeRules "Factor();", since TypeRules "type = Factor();" ]
    , codeP 12 [ since TypeRules "if (!isArith(type))" ]
    , codeP 14 [ since TypeRules "SemError(\"invalid cast\");" ]
    , codeP 12 [ since TypeRules "else type = Types.intType;" ]
    , code 8 "} else if (StartOf(11)) {"
    , codeP 12 [ before TypeRules "Expression();", since TypeRules "type = Expression();" ]
    , code 12 "Expect(rparen_Sym);"
    , code 8 "} else SynErr(70);"
    , code 8 "break;"
    , code 4 "}"
    , code 4 "case cap_Sym: case low_Sym: {"
    , code 8 "if (la.kind == cap_Sym) {"
    , code 12 "Get();"
    , codeP 12 [ since CodeGeneration "upper = true;" ]
    , code 8 "} else {"
    , code 12 "Get();"
    , codeP 12 [ since CodeGeneration "upper = false;" ]
    , code 8 "}"
    , code 8 "Expect(lparen_Sym);"
    , codeP 8 [ before TypeRules "Expression();", since TypeRules "type = Expression();" ]
    , codeP 8 [ since TypeRules "if (type != Types.charType)" ]
    , codeP 10 [ since TypeRules "SemError(\"character argument needed\");" ]
    , codeP 8 [ since TypeRules "type = Types.charType;" ]
    , codeP 8 [ since CodeGeneration "if (upper) CodeGen.capChar();" ]
    , codeP 8 [ since CodeGeneration "else CodeGen.lowChar();" ]
    , code 8 "Expect(rparen_Sym);"
    , code 8 "break;"
    , code 4 "}"
    , code 4 "case length_Sym: {"
    , code 8 "Get();"
    , code 8 "Expect(lparen_Sym);"
    , codeP 8 [ before ScopeRules "Designator();", since ScopeRules "des = Designator();" ]
    , codeP 8 [ since TypeRules "if (!isArray(des.type))" ]
    , codeP 10 [ since TypeRules "SemError(\"array type needed for length\");" ]
    , codeP 8 [ since CodeGeneration "CodeGen.dereference();" ]
    , codeP 8 [ since CodeGeneration "CodeGen.dereference();" ]
    , codeP 8 [ since TypeRules "type = Types.intType;" ]
    , code 8 "Expect(rparen_Sym);"
    , code 8 "break;"
    , code 4 "}"
    , code 4 "default: SynErr(71); break;"
    , code 4 "}"
    , codeP 4 [ since TypeRules "return type;" ]
    , code 0 "}"
    ]


addOpMethod : List CodeLine
addOpMethod =
    [ codeP 0
        [ core "static "
        , before CodeGeneration "void"
        , since CodeGeneration "int"
        , core " AddOp() {"
        ]
    , codeP 4 [ since CodeGeneration "int op;" ]
    , codeP 4 [ since CodeGeneration "op = CodeGen.nop;" ]
    , code 4 "if (la.kind == plus_Sym) {"
    , code 8 "Get();"
    , codeP 8 [ since CodeGeneration "op = CodeGen.add;" ]
    , code 4 "} else if (la.kind == minus_Sym) {"
    , code 8 "Get();"
    , codeP 8 [ since CodeGeneration "op = CodeGen.sub;" ]
    , code 4 "} else SynErr(67);"
    , codeP 4 [ since CodeGeneration "return op;" ]
    , code 0 "}"
    ]


identMethod : List CodeLine
identMethod =
    [ codeP 0
        [ core "static "
        , before ScopeRules "void"
        , since ScopeRules "String"
        , core " Ident() {"
        ]
    , codeP 4 [ since ScopeRules "String name;" ]
    , code 4 "Expect(identifier_Sym);"
    , codeP 4 [ since ScopeRules "name = token.val;" ]
    , codeP 4 [ since ScopeRules "return name;" ]
    , code 0 "}"
    ]


-- TABLE.JAVA ---------------------------------------------------------------
--
-- Every line here is phase-invariant: the whole file only enters the story at
-- ScopeRules, and the tab that shows it does not exist before then.


tableSrc : List Method
tableSrc =
    [ Method "class Entry"
        [ code 0 "class Entry {"
        , code 2 "// All fields initialised, but modified after construction"
        , code 2 "// by the semantic analyser"
        , code 2 "public int     kind     = Kinds.Var;"
        , code 2 "public String  name     = \"\";"
        , code 2 "public int     type     = Types.noType;"
        , code 2 "public int     value    = 0;       // constants"
        , code 2 "public int     offset   = 0;       // variables"
        , code 2 "public boolean declared = true;    // false for the sentinel"
        , code 2 "public Entry   nextInScope = null; // next entry in this scope"
        , code 0 "}"
        ]
    , Method "class Scope"
        [ code 0 "class Scope {"
        , code 2 "public Scope outer;        // enclosing scope"
        , code 2 "public Entry firstInScope; // first entry in this scope"
        , code 0 "}"
        ]
    , Method "class StackFrame"
        [ code 0 "class StackFrame {"
        , code 2 "// Tracks the space needed by variables as they are declared"
        , code 2 "// and come in and out of scope"
        , code 2 "public int size = 0;"
        , code 0 "}"
        ]
    , Method "class DesType"
        [ code 0 "class DesType {"
        , code 2 "// Associated with l-value and r-value designators"
        , code 2 "public Entry entry;  // the identifier properties"
        , code 2 "public int type;     // designator type (not always entry type)"
        , code 0 ""
        , code 2 "public DesType(Entry entry) {"
        , code 4 "this.entry = entry;"
        , code 4 "this.type  = entry.type;  // may need modification later"
        , code 2 "}"
        , code 0 "}"
        ]
    , Method "class ConstRec"
        [ code 0 "class ConstRec {"
        , code 2 "// Associated with literal constants"
        , code 2 "public int value;  // value of a constant literal"
        , code 2 "public int type;   // type determined from the literal"
        , code 0 "}"
        ]
    , Method "class Kinds"
        [ code 0 "class Kinds {"
        , code 2 "public static final int"
        , code 4 "Con = 0,"
        , code 4 "Var = 1,"
        , code 4 "Fun = 2;"
        , code 0 ""
        , code 2 "public static String[] kindNames = { \"const\", \"var  \", \"func \" };"
        , code 0 "}"
        ]
    , Method "class Types"
        [ code 0 "class Types {"
        , code 2 "// The numbering is significant: an array type is the"
        , code 2 "// matching simple type + 1"
        , code 2 "public static final int"
        , code 4 "noType   =  0,"
        , code 4 "nullType =  2,"
        , code 4 "intType  =  4,"
        , code 4 "boolType =  6,"
        , code 4 "charType =  8,"
        , code 4 "voidType = 10;"
        , code 0 ""
        , code 2 "public static int addType(String name) {"
        , code 4 "int thisType = nextType;"
        , code 4 "nextType += 2;"
        , code 4 "typeNames.add(name);        // simple"
        , code 4 "typeNames.add(name + \"[]\"); // matching array type"
        , code 4 "return thisType;"
        , code 2 "}"
        , code 0 "}"
        ]
    , Method "Table.insert"
        [ code 0 "public static void insert(Entry entry) {"
        , code 2 "// Adds entry to symbol table"
        , code 2 "Entry earlierEntry = find(entry.name);"
        , code 2 "if (earlierEntry.declared)"
        , code 4 "Parser.SemError(\"earlier declaration of \" + entry.name"
        , code 4 "                + \" is still in scope\");"
        , code 2 "sentinelEntry.name = entry.name;"
        , code 2 "Entry look = topScope.firstInScope, previous = null;"
        , code 2 "while (!look.name.equals(entry.name)) {"
        , code 4 "previous = look; look = look.nextInScope;"
        , code 2 "}"
        , code 2 "entry.nextInScope = look;"
        , code 2 "if (previous == null) topScope.firstInScope = entry;"
        , code 2 "else previous.nextInScope = entry;"
        , code 2 "entry.declared = true;"
        , code 0 "}"
        ]
    , Method "Table.find"
        [ code 0 "public static Entry find(String name) {"
        , code 2 "// Searches the table for an entry matching name. If found,"
        , code 2 "// returns it; otherwise returns the sentinel entry, which is"
        , code 2 "// marked as not declared."
        , code 2 "sentinelEntry.name = name;"
        , code 2 "Scope scope = topScope;"
        , code 2 "while (scope != null) {"
        , code 4 "Entry entry = scope.firstInScope;"
        , code 4 "while (!entry.name.equals(name)) entry = entry.nextInScope;"
        , code 4 "if (entry != sentinelEntry) return entry;"
        , code 4 "scope = scope.outer;"
        , code 2 "}"
        , code 2 "return sentinelEntry;"
        , code 0 "}"
        ]
    , Method "Table.openScope"
        [ code 0 "public static void openScope() {"
        , code 2 "// Opens a scope record at the start of a statement block"
        , code 2 "Scope newScope = new Scope();"
        , code 2 "newScope.outer = topScope;"
        , code 2 "newScope.firstInScope = sentinelEntry;"
        , code 2 "topScope = newScope;"
        , code 0 "}"
        ]
    , Method "Table.closeScope"
        [ code 0 "public static void closeScope() {"
        , code 2 "// Closes a scope record at the end of a statement block"
        , code 2 "topScope = topScope.outer;"
        , code 0 "}"
        ]
    ]



-- CODEGEN.JAVA -------------------------------------------------------------
--
-- As with Table.java, every line is phase-invariant: the file, and its tab,
-- only exist from CodeGeneration onwards.


codeGenSrc : List Method
codeGenSrc =
    [ Method "class Label"
        [ code 0 "class Label {"
        , code 2 "private int memAdr;      // address if defined, else the last"
        , code 2 "                         // forward reference"
        , code 2 "private boolean defined; // true once memAdr is known"
        , code 0 ""
        , code 2 "public Label(boolean known) {"
        , code 4 "// Constructor for a label, possibly at an already known location"
        , code 4 "if (known) this.memAdr = CodeGen.getCodeLength();"
        , code 4 "else this.memAdr = CodeGen.undefined; // end of the chain"
        , code 4 "this.defined = known;"
        , code 2 "}"
        , code 0 ""
        , code 2 "public int address() {"
        , code 4 "// Returns memAdr if known; otherwise adds to a forward"
        , code 4 "// reference chain that here() will resolve later"
        , code 4 "int adr = memAdr;"
        , code 4 "if (!defined) memAdr = CodeGen.getCodeLength();"
        , code 4 "return adr;"
        , code 2 "}"
        , code 0 ""
        , code 2 "public void here() {"
        , code 4 "// Defines memAdr to be the current location counter, after"
        , code 4 "// fixing any outstanding forward references"
        , code 4 "if (defined) Parser.SemError(\"Compiler error - bad label\");"
        , code 4 "else CodeGen.backPatch(memAdr);"
        , code 4 "memAdr = CodeGen.getCodeLength();"
        , code 4 "defined = true;"
        , code 2 "}"
        , code 0 "}"
        ]
    , Method "CodeGen.emit"
        [ code 0 "private static void emit(int word) {"
        , code 2 "// Code generator for a single word"
        , code 2 "if (!generatingCode) return;"
        , code 2 "if (codeTop >= stkTop) {"
        , code 4 "Parser.SemError(\"program too long\"); generatingCode = false;"
        , code 2 "}"
        , code 2 "else {"
        , code 4 "PVM.mem[codeTop] = word; codeTop++;"
        , code 2 "}"
        , code 0 "}"
        ]
    , Method "CodeGen opcodes"
        [ code 0 "public static final int"
        , code 2 "undefined = -1,"
        , code 2 "nop =  1,  add =  2,  sub =  3,  mul =  4,"
        , code 2 "div =  5,  rem =  6,  and =  7,  or  =  8,"
        , code 2 "ceq =  9,  cne = 10,  clt = 11,  cge = 12,"
        , code 2 "cgt = 13,  cle = 14;"
        ]
    , Method "CodeGen.openStackFrame"
        [ code 0 "public static void openStackFrame(int size) {"
        , code 2 "// Generates (possibly incomplete) code to reserve space for"
        , code 2 "// local variables"
        , code 2 "emit(PVM.dsp); emit(size);"
        , code 0 "}"
        ]
    , Method "CodeGen.fixDSP"
        [ code 0 "public static void fixDSP(int location, int size) {"
        , code 2 "// Fixes up the DSP instruction at location to reserve size"
        , code 2 "// space for variables"
        , code 2 "PVM.mem[location+1] = size;"
        , code 0 "}"
        ]
    , Method "CodeGen.leaveProgram"
        [ code 0 "public static void leaveProgram() {"
        , code 2 "// Generates the code needed to leave a program (halt)"
        , code 2 "emit(PVM.halt);"
        , code 0 "}"
        ]
    , Method "CodeGen.branch"
        [ code 0 "public static void branch(Label destination) {"
        , code 2 "// Generates an unconditional branch to destination"
        , code 2 "emit(PVM.brn); emit(destination.address());"
        , code 0 "}"
        ]
    , Method "CodeGen.branchFalse"
        [ code 0 "public static void branchFalse(Label destination) {"
        , code 2 "// Generates a branch to destination, conditional on the Boolean"
        , code 2 "// currently on top of the evaluation stack, popping that value"
        , code 2 "emit(PVM.bze); emit(destination.address());"
        , code 0 "}"
        ]
    , Method "CodeGen.branchTrue"
        [ code 0 "public static void branchTrue(Label destination) {"
        , code 2 "// Generates a branch to destination if TOS is true (non-zero),"
        , code 2 "// popping that value"
        , code 2 "emit(PVM.not); emit(PVM.bze); emit(destination.address());"
        , code 0 "}"
        ]
    , Method "CodeGen.loadAddress"
        [ code 0 "public static void loadAddress(Entry var) {"
        , code 2 "// Generates code to push the address of variable var onto the"
        , code 2 "// evaluation stack"
        , code 2 "emit(PVM.lda); emit(var.offset);"
        , code 0 "}"
        ]
    , Method "CodeGen.loadConstant"
        [ code 0 "public static void loadConstant(int number) {"
        , code 2 "// Generates code to push number onto the evaluation stack"
        , code 2 "emit(PVM.ldc); emit(number);"
        , code 0 "}"
        ]
    , Method "CodeGen.dereference"
        [ code 0 "public static void dereference() {"
        , code 2 "// Replaces the top of the evaluation stack by the value found"
        , code 2 "// at the address currently stored there"
        , code 2 "emit(PVM.ldv);"
        , code 0 "}"
        ]
    , Method "CodeGen.assign"
        [ code 0 "public static void assign(int type) {"
        , code 2 "// Stores the value on top of stack at the address given by"
        , code 2 "// next-to-top, popping both"
        , code 2 "if (type == Types.charType) emit(PVM.stoc);"
        , code 2 "else emit(PVM.sto);"
        , code 0 "}"
        ]
    , Method "CodeGen.index"
        [ code 0 "public static void index() {"
        , code 2 "// Generates code to index an array on the heap"
        , code 2 "emit(PVM.ldxa);"
        , code 0 "}"
        ]
    , Method "CodeGen.allocate"
        [ code 0 "public static void allocate() {"
        , code 2 "// Generates code to allocate an array on the heap"
        , code 2 "emit(PVM.anew);"
        , code 0 "}"
        ]
    , Method "CodeGen.castToChar"
        [ code 0 "public static void castToChar() {"
        , code 2 "// Generates a range-checked integer-to-char conversion"
        , code 2 "emit(PVM.i2c);"
        , code 0 "}"
        ]
    , Method "CodeGen.capChar"
        [ code 0 "public static void capChar() {"
        , code 2 "// Generates code to convert the char on TOS to uppercase"
        , code 2 "emit(PVM.capc);"
        , code 0 "}"
        ]
    , Method "CodeGen.lowChar"
        [ code 0 "public static void lowChar() {"
        , code 2 "// Generates code to convert the char on TOS to lowercase"
        , code 2 "emit(PVM.lowc);"
        , code 0 "}"
        ]
    , Method "CodeGen.backPatch"
        [ code 0 "public static void backPatch(int adr) {"
        , code 2 "// Stores the current location counter as the address field of"
        , code 2 "// the branch instruction holding a forward reference to adr,"
        , code 2 "// then works back through the linked list of such instructions"
        , code 2 "while (adr != undefined) {"
        , code 4 "int nextAdr = PVM.mem[adr];"
        , code 4 "PVM.mem[adr] = codeTop;"
        , code 4 "adr = nextAdr;"
        , code 2 "}"
        , code 0 "}"
        ]
    ]


-- SHARED TEACHING NOTES ----------------------------------------------------
--
-- Most segments of a given kind are explained by the same rule, so the note
-- bodies live here and the per-segment data supplies only a title and any
-- bespoke wording.


headNote : String
headNote =
    "Coco/R turns every production into exactly one static method. Formal attributes in angle brackets become the method's parameters -- except a lone <out T x>, which becomes the RETURN TYPE, because Java has no out parameters. Coco/R also declares x as a local and adds a return at the end."


nonTerminalNote : String
nonTerminalNote =
    "A non-terminal becomes a call to the method of the same name. Input attributes become arguments; an <out x> attribute becomes an assignment from the returned value, so the attribute is carried back up the parse."


terminalExpectNote : String
terminalExpectNote =
    "A terminal in a sequence becomes Expect(sym). Expect tests the lookahead, consumes it with Get() if it matches and calls SynErr if it does not. The _Sym constants are numbered by Coco/R at the head of Parser.java."


terminalCaseNote : String
terminalCaseNote =
    "Inside an alternation a terminal becomes a bare Get() rather than Expect(). The case label has already established what the lookahead is, so there is nothing left to test -- only to consume. This is why identical terminals generate different code in different positions."


actionNote : String
actionNote =
    "A semantic action is copied verbatim into the method at exactly the position it occupies in the production. Coco/R never reads it, checks it or reorders it. That is why moving an action before or after a symbol changes when its code runs."


iterateNote : String
iterateNote =
    "{ X } becomes a while loop. The guard tests the lookahead against FIRST(X): StartOf(n) indexes row n of the set table at the foot of Parser.java, or, where FIRST(X) is a single terminal, the guard collapses to a direct la.kind comparison."


optionNote : String
optionNote =
    "[ X ] becomes an if with no else. The condition is the same FIRST(X) test an iteration would use; the only difference is that the body can run at most once."


alternationNote : String
alternationNote =
    "An alternation becomes a switch on la.kind when there are several alternatives, or an if / else-if chain for two or three. Each case label is the FIRST set of that alternative, and the default branch reports that no alternative applies."


syncNote : String
syncNote =
    "SYNC generates a skip loop: while the lookahead is not in the synchronisation set, report an error and throw the token away. It marks a point where particularly safe tokens are expected, so the parser can find its feet again after an error instead of cascading."


weakExpectNote : String
weakExpectNote =
    "WEAK t generates ExpectWeak(t, follow) in place of Expect(t). If t is missing the parser reports it and then skips input until it reaches a token that may legally follow, rather than abandoning the whole construct."


weakSeparatorNote : String
weakSeparatorNote =
    "A WEAK terminal at the head of an iteration becomes the loop guard itself: WeakSeparator(n, syFol, repFol). It returns true when the separator is present; on a mistyped separator it skips either to the start of the next repetition or out of the loop, so a list is not silently cut short."



-- LAYOUT HELPERS -----------------------------------------------------------


{-| Fixed run of spaces, used to line alternation bars up under their opening
bracket.
-}
gap : Int -> Frag
gap width =
    Frag Nothing False [ core (String.repeat width " ") ]


{-| Leading space that only exists from a given phase onwards. SYNC pushes the
first line of Statement five columns to the right, and the bars below it have
to move with it.
-}
pad : Phase -> Int -> Frag
pad phase width =
    Frag Nothing False [ since phase (String.repeat width " ") ]



-- THE PRODUCTIONS ----------------------------------------------------------


productions : Array Production
productions =
    Array.fromList
        [ parvaProduction
        , statementProduction
        , blockProduction
        , ifStatementProduction
        , whileStatementProduction
        , varListProduction
        , oneVarProduction
        , designatorProduction
        , primaryProduction
        , addOpProduction
        , identProduction
        ]



-- Parva --------------------------------------------------------------------


parvaProduction : Production
parvaProduction =
    { name = "Parva"
    , method = "Parva"
    , lines =
        [ gline 2 [ txt "Parva" |> own 0 ]
        , gline 2 [ txt "=", txt "\"void\"" |> own 1 ]
        , gline 6 [ varying [ since ScopeRules "(. Entry program = new Entry(); .)" ] |> own 2 ]
        , gline 4
            [ varying [ core "Ident", since ScopeRules "<out program.name>" ] |> own 3
            , txt "\"(\"" |> own 4
            , txt "\")\"" |> own 5
            ]
        , gline 6 [ varying [ since ScopeRules "(." ] |> own 6 ]
        , gline 9 [ varying [ since ScopeRules "program.kind = Kinds.Fun;" ] |> own 6 ]
        , gline 9 [ varying [ since TypeRules "program.type = Types.voidType;" ] |> own 6 ]
        , gline 9 [ varying [ since ScopeRules "Table.insert(program);" ] |> own 6 ]
        , gline 9 [ varying [ since ScopeRules "StackFrame frame = new StackFrame();" ] |> own 6 ]
        , gline 9 [ varying [ since ScopeRules "Table.openScope();" ] |> own 6 ]
        , gline 9 [ varying [ since CodeGeneration "Label DSPLabel = new Label(known);" ] |> own 6 ]
        , gline 9 [ varying [ since CodeGeneration "CodeGen.openStackFrame(0);" ] |> own 6 ]
        , gline 6 [ varying [ since ScopeRules ".)" ] |> own 6 ]
        , gline 4
            [ varying [ since ErrorHandling "WEAK" ] |> own 7
            , txt "\"{\"" |> own 8
            , txt "{" |> own 9
            , varying [ core "Statement", since ScopeRules "<frame>" ] |> own 10
            , txt "}" |> own 9
            ]
        , gline 4
            [ varying [ since ErrorHandling "WEAK" ] |> own 11
            , txt "\"}\"" |> own 12
            ]
        , gline 6 [ varying [ since ScopeRules "(." ] |> own 13 ]
        , gline 9 [ varying [ since CodeGeneration "CodeGen.fixDSP(DSPLabel.address(), frame.size);" ] |> own 13 ]
        , gline 9 [ varying [ since CodeGeneration "CodeGen.leaveProgram();" ] |> own 13 ]
        , gline 9 [ varying [ since ScopeRules "Table.closeScope();" ] |> own 13 ]
        , gline 6 [ varying [ since ScopeRules ".)" ] |> own 13 ]
        , gline 2 [ txt "." ]
        ]
    , segments =
        Array.fromList
            [ seg Head 0 0 "The goal symbol becomes Parva()" headNote
            , seg Terminal 1 1 "\"void\" becomes Expect(void_Sym)" terminalExpectNote
            , seg Action 2 2 "Declaring the program's symbol table entry" actionNote
                |> callsTable [ "class Entry" ]
            , seg NonTerminal 3 3 "Ident() returns the name it read" nonTerminalNote
            , seg Terminal 4 4 "\"(\" becomes Expect(lparen_Sym)" terminalExpectNote
            , seg Terminal 5 5 "\")\" becomes Expect(rparen_Sym)" terminalExpectNote
            , seg Action 6 12 "Opening the program: table entry, scope and stack frame" ("Seven statements in one action, copied out in order. Watch them appear one phase at a time: the table work at phase 3, the type at phase 4, the Label and the DSP instruction only at phase 5. " ++ actionNote)
                |> callsTable [ "class Kinds", "class Types", "Table.insert", "class StackFrame", "Table.openScope" ]
                |> callsCodeGen [ "class Label", "CodeGen.openStackFrame" ]
            , seg Weak 13 13 "WEAK \"{\" becomes ExpectWeak" weakExpectNote
            , seg Terminal 13 13 "\"{\" becomes Expect(lbrace_Sym)" ("At phase 1 this is a plain Expect(lbrace_Sym). " ++ terminalExpectNote)
            , seg Iterate 14 16 "{ Statement } becomes while (StartOf(2))" iterateNote
            , seg NonTerminal 15 15 "Statement(frame) passes the stack frame down" nonTerminalNote
            , seg Weak 17 17 "WEAK \"}\" becomes ExpectWeak" weakExpectNote
            , seg Terminal 17 17 "\"}\" becomes Expect(rbrace_Sym)" terminalExpectNote
            , seg Action 18 20 "Closing the program: fix the frame size, halt, close the scope" ("CodeGen.fixDSP goes back and patches the operand of the DSP instruction emitted at the top of the method, now that frame.size is finally known. " ++ actionNote)
                |> callsTable [ "Table.closeScope" ]
                |> callsCodeGen [ "CodeGen.fixDSP", "CodeGen.leaveProgram" ]
            ]
    }



-- Statement ----------------------------------------------------------------


statementProduction : Production
statementProduction =
    { name = "Statement"
    , method = "Statement"
    , lines =
        [ gline 2 [ varying [ core "Statement", since ScopeRules "<StackFrame frame>" ] |> own 0 ]
        , gline 2
            [ txt "="
            , varying [ since ErrorHandling "SYNC" ] |> own 1
            , txt "(" |> own 2
            , varying [ core "Block", since ScopeRules "<frame>" ] |> own 3
            ]
        , gline 4 [ pad ErrorHandling 5, txt "|" |> own 2 |> tight, txt "ConstDeclarations" |> own 4 ]
        , gline 4 [ pad ErrorHandling 5, txt "|" |> own 2 |> tight, varying [ core "VarDeclarations", since ScopeRules "<frame>" ] |> own 5 ]
        , gline 4 [ pad ErrorHandling 5, txt "|" |> own 2 |> tight, txt "AssignmentStatement" |> own 6 ]
        , gline 4 [ pad ErrorHandling 5, txt "|" |> own 2 |> tight, varying [ core "IfStatement", since ScopeRules "<frame>" ] |> own 7 ]
        , gline 4 [ pad ErrorHandling 5, txt "|" |> own 2 |> tight, varying [ core "WhileStatement", since ScopeRules "<frame>" ] |> own 8 ]
        , gline 4 [ pad ErrorHandling 5, txt "|" |> own 2 |> tight, varying [ core "DoWhileStatement", since ScopeRules "<frame>" ] |> own 9 ]
        , gline 4 [ pad ErrorHandling 5, txt "|" |> own 2 |> tight, txt "BreakStatement" |> own 10 ]
        , gline 4 [ pad ErrorHandling 5, txt "|" |> own 2 |> tight, txt "HaltStatement" |> own 11 ]
        , gline 4 [ pad ErrorHandling 5, txt "|" |> own 2 |> tight, txt "ReadStatement" |> own 12 ]
        , gline 4 [ pad ErrorHandling 5, txt "|" |> own 2 |> tight, txt "WriteStatement" |> own 13 ]
        , gline 4 [ pad ErrorHandling 5, txt "|" |> own 2 |> tight, txt "\";\"" |> own 14 ]
        , gline 8 [ pad ErrorHandling 5, varying [ since ErrorHandling "(. if (warnings) Warning(\"empty statement\"); .)" ] |> own 15 |> tight ]
        , gline 4 [ pad ErrorHandling 5, txt ")" |> own 2 |> tight ]
        , gline 2 [ txt "." ]
        ]
    , segments =
        Array.fromList
            [ seg Head 0 0 "Statement becomes one static method" headNote
            , seg Sync 1 1 "SYNC becomes a skip loop" syncNote
            , seg Alternation 2 53 "Twelve alternatives become one switch on la.kind" ("With this many alternatives Coco/R emits a switch rather than an if chain. Every case label is the FIRST set of its alternative, which is why a single keyword such as const selects ConstDeclarations, while three different symbols all select AssignmentStatement. " ++ alternationNote)
            , seg NonTerminal 3 6 "Block becomes case lbrace_Sym" nonTerminalNote
            , seg NonTerminal 7 10 "ConstDeclarations becomes case const_Sym" nonTerminalNote
            , seg NonTerminal 11 14 "VarDeclarations is selected by three type keywords" ("FIRST(VarDeclarations) is { int, bool, char }, so the alternative needs three case labels stacked on one arm. " ++ nonTerminalNote)
            , seg NonTerminal 15 18 "AssignmentStatement is selected by identifier, ++ or --" nonTerminalNote
            , seg NonTerminal 19 22 "IfStatement becomes case if_Sym" nonTerminalNote
            , seg NonTerminal 23 26 "WhileStatement becomes case while_Sym" nonTerminalNote
            , seg NonTerminal 27 30 "DoWhileStatement becomes case do_Sym" nonTerminalNote
            , seg NonTerminal 31 34 "BreakStatement becomes case break_Sym" nonTerminalNote
            , seg NonTerminal 35 38 "HaltStatement becomes case halt_Sym" nonTerminalNote
            , seg NonTerminal 39 42 "ReadStatement is selected by read or readLine" nonTerminalNote
            , seg NonTerminal 43 46 "WriteStatement is selected by write or writeLine" nonTerminalNote
            , seg Terminal 47 51 "\";\" becomes case semicolon_Sym with a bare Get()" terminalCaseNote
            , seg Action 49 49 "The empty statement warning" ("The action sits between the terminal and the break, so the warning is issued after the semicolon has been consumed. " ++ actionNote)
            ]
    }



-- Block --------------------------------------------------------------------


blockProduction : Production
blockProduction =
    { name = "Block"
    , method = "Block"
    , lines =
        [ gline 2 [ varying [ core "Block", since ScopeRules "<StackFrame frame>" ] |> own 0 ]
        , gline 2 [ txt "=" ]
        , gline 6 [ varying [ since ErrorHandling "(." ] |> own 1 ]
        , gline 9 [ varying [ since ScopeRules "Table.openScope();" ] |> own 1 ]
        , gline 9 [ varying [ since ErrorHandling "boolean empty = true;" ] |> own 1 ]
        , gline 6 [ varying [ since ErrorHandling ".)" ] |> own 1 ]
        , gline 4
            [ txt "\"{\"" |> own 2
            , txt "{" |> own 3
            , varying [ core "Statement", since ScopeRules "<frame>" ] |> own 4
            ]
        , gline 11 [ varying [ since ErrorHandling "(. empty = false; .)" ] |> own 5 ]
        , gline 10 [ txt "}" |> own 3 ]
        , gline 6 [ varying [ since ErrorHandling "(. if (empty && warnings) Warning(\"empty {} block\"); .)" ] |> own 6 ]
        , gline 4
            [ varying [ since ErrorHandling "WEAK" ] |> own 7
            , txt "\"}\"" |> own 8
            ]
        , gline 6 [ varying [ since ScopeRules "(. Table.closeScope(); .)" ] |> own 9 ]
        , gline 2 [ txt "." ]
        ]
    , segments =
        Array.fromList
            [ seg Head 0 0 "Block becomes one static method" headNote
            , seg Action 1 2 "An action placed before the first symbol" ("This action sits immediately after the equals sign, before any symbol has been matched, so its code runs on entry to the method -- before Expect(lbrace_Sym). That is how a scope is opened at the right moment. " ++ actionNote)
                |> callsTable [ "Table.openScope" ]
            , seg Terminal 3 3 "\"{\" becomes Expect(lbrace_Sym)" terminalExpectNote
            , seg Iterate 4 7 "{ Statement } becomes while (StartOf(2))" iterateNote
            , seg NonTerminal 5 5 "Statement(frame) inside the loop body" nonTerminalNote
            , seg Action 6 6 "An action inside the loop body runs once per repetition" ("Because the action is inside the braces, it lands inside the while loop, so empty is cleared on every statement rather than once. " ++ actionNote)
            , seg Action 8 8 "An action after the iteration runs when the loop ends" actionNote
            , seg Weak 9 9 "WEAK \"}\" becomes ExpectWeak" weakExpectNote
            , seg Terminal 9 9 "\"}\" becomes Expect(rbrace_Sym)" terminalExpectNote
            , seg Action 10 10 "Closing the scope" ("Every Entry inserted since openScope becomes invisible again, which is the whole of Parva's block scoping. " ++ actionNote)
                |> callsTable [ "Table.closeScope" ]
            ]
    }



-- IfStatement --------------------------------------------------------------


ifStatementProduction : Production
ifStatementProduction =
    { name = "IfStatement"
    , method = "IfStatement"
    , lines =
        [ gline 2 [ varying [ core "IfStatement", since ScopeRules "<StackFrame frame>" ] |> own 0 ]
        , gline 6 [ varying [ since CodeGeneration "(." ] |> own 1 ]
        , gline 9 [ varying [ since CodeGeneration "Label falseLabel = new Label(!known);" ] |> own 1 ]
        , gline 9 [ varying [ since CodeGeneration "Label exitLabel = null;" ] |> own 1 ]
        , gline 9 [ varying [ since CodeGeneration "boolean hasElse = false;" ] |> own 1 ]
        , gline 6 [ varying [ since CodeGeneration ".)" ] |> own 1 ]
        , gline 2
            [ txt "="
            , txt "\"if\"" |> own 2
            , txt "\"(\"" |> own 3
            , txt "Condition" |> own 4
            , txt "\")\"" |> own 5
            ]
        , gline 6 [ varying [ since CodeGeneration "(. CodeGen.branchFalse(falseLabel); .)" ] |> own 6 ]
        , gline 4 [ varying [ core "Statement", since ScopeRules "<frame>" ] |> own 7 ]
        , gline 4 [ txt "[" |> own 8, txt "\"else\"" |> own 9 ]
        , gline 6 [ varying [ since CodeGeneration "(." ] |> own 10 ]
        , gline 9 [ varying [ since CodeGeneration "hasElse = true;" ] |> own 10 ]
        , gline 9 [ varying [ since CodeGeneration "exitLabel = new Label(!known);" ] |> own 10 ]
        , gline 9 [ varying [ since CodeGeneration "CodeGen.branch(exitLabel);" ] |> own 10 ]
        , gline 9 [ varying [ since CodeGeneration "falseLabel.here();" ] |> own 10 ]
        , gline 6 [ varying [ since CodeGeneration ".)" ] |> own 10 ]
        , gline 6 [ varying [ core "Statement", since ScopeRules "<frame>" ] |> own 11 ]
        , gline 4 [ txt "]" |> own 8 ]
        , gline 6 [ varying [ since CodeGeneration "(." ] |> own 12 ]
        , gline 9 [ varying [ since CodeGeneration "if (hasElse) exitLabel.here();" ] |> own 12 ]
        , gline 9 [ varying [ since CodeGeneration "else falseLabel.here();" ] |> own 12 ]
        , gline 6 [ varying [ since CodeGeneration ".)" ] |> own 12 ]
        , gline 2 [ txt "." ]
        ]
    , segments =
        Array.fromList
            [ seg Head 0 0 "IfStatement becomes one method" headNote
            , seg Action 1 3 "An action before the equals sign declares the method's locals" ("An action written against the production head, before the equals sign, is emitted at the very top of the method. That is where Coco/R users put local declarations -- here, the two forward-referenced branch targets. " ++ actionNote)
                |> callsCodeGen [ "class Label" ]
            , seg Terminal 4 4 "\"if\" becomes Expect(if_Sym)" terminalExpectNote
            , seg Terminal 5 5 "\"(\" becomes Expect(lparen_Sym)" terminalExpectNote
            , seg NonTerminal 6 6 "Condition() leaves a Boolean on the evaluation stack" nonTerminalNote
            , seg Terminal 7 7 "\")\" becomes Expect(rparen_Sym)" terminalExpectNote
            , seg Action 8 8 "Branch over the then-part if the condition is false" ("falseLabel is still undefined here, so branchFalse emits a placeholder address and links it into a forward reference chain that here() will later patch. " ++ actionNote)
                |> callsCodeGen [ "CodeGen.branchFalse", "class Label" ]
            , seg NonTerminal 9 9 "The then-part" nonTerminalNote
            , seg Optional 10 17 "[ \"else\" ... ] becomes if (la.kind == else_Sym)" ("FIRST of the optional part is the single terminal else, so the guard is a direct comparison rather than a StartOf test. This is also where the dangling-else is silently resolved: the else binds to the nearest if, because that is the invocation whose lookahead is tested first. " ++ optionNote)
            , seg Terminal 11 11 "\"else\" becomes a bare Get()" terminalCaseNote
            , seg Action 12 15 "Jump over the else-part, then fix up the false branch" actionNote
                |> callsCodeGen [ "class Label", "CodeGen.branch" ]
            , seg NonTerminal 16 16 "The else-part" nonTerminalNote
            , seg Action 18 19 "An action after the option runs on both paths" ("Placed after the closing bracket, this code runs whether or not the else was present, which is exactly what is needed to define whichever label is still outstanding. " ++ actionNote)
                |> callsCodeGen [ "class Label" ]
            ]
    }



-- WhileStatement -----------------------------------------------------------


whileStatementProduction : Production
whileStatementProduction =
    { name = "WhileStatement"
    , method = "WhileStatement"
    , lines =
        [ gline 2 [ varying [ core "WhileStatement", since ScopeRules "<StackFrame frame>" ] |> own 0 ]
        , gline 6 [ varying [ since CodeGeneration "(." ] |> own 1 ]
        , gline 9 [ varying [ since CodeGeneration "Label loopExit  = new Label(!known);" ] |> own 1 ]
        , gline 9 [ varying [ since CodeGeneration "Label loopStart = new Label(known);" ] |> own 1 ]
        , gline 9 [ varying [ since CodeGeneration "CodeGen.brkLabels[CodeGen.bl++] = loopExit;" ] |> own 1 ]
        , gline 6 [ varying [ since CodeGeneration ".)" ] |> own 1 ]
        , gline 2
            [ txt "="
            , txt "\"while\"" |> own 2
            , txt "\"(\"" |> own 3
            , txt "Condition" |> own 4
            , txt "\")\"" |> own 5
            ]
        , gline 6 [ varying [ since CodeGeneration "(. CodeGen.branchFalse(loopExit); .)" ] |> own 6 ]
        , gline 4 [ varying [ core "Statement", since ScopeRules "<frame>" ] |> own 7 ]
        , gline 6 [ varying [ since CodeGeneration "(." ] |> own 8 ]
        , gline 9 [ varying [ since CodeGeneration "CodeGen.branch(loopStart);" ] |> own 8 ]
        , gline 9 [ varying [ since CodeGeneration "loopExit.here();" ] |> own 8 ]
        , gline 9 [ varying [ since CodeGeneration "CodeGen.bl--;" ] |> own 8 ]
        , gline 6 [ varying [ since CodeGeneration ".)" ] |> own 8 ]
        , gline 2 [ txt "." ]
        ]
    , segments =
        Array.fromList
            [ seg Head 0 0 "WhileStatement becomes one method" headNote
            , seg Action 1 3 "Two labels, one known and one not" ("loopStart is created with known = true, so it records the current code address immediately -- the loop body has not been generated yet, so this IS the top of the loop. loopExit is created with !known, because where the loop ends is not yet decided. The label is also pushed on brkLabels so a break inside the body knows where to jump. " ++ actionNote)
                |> callsCodeGen [ "class Label" ]
            , seg Terminal 4 4 "\"while\" becomes Expect(while_Sym)" terminalExpectNote
            , seg Terminal 5 5 "\"(\" becomes Expect(lparen_Sym)" terminalExpectNote
            , seg NonTerminal 6 6 "Condition() is re-evaluated on every pass" nonTerminalNote
            , seg Terminal 7 7 "\")\" becomes Expect(rparen_Sym)" terminalExpectNote
            , seg Action 8 8 "Leave the loop when the condition is false" actionNote
                |> callsCodeGen [ "CodeGen.branchFalse" ]
            , seg NonTerminal 9 9 "The loop body" nonTerminalNote
            , seg Action 10 12 "Jump back to the top, then define the exit" ("A recursive-descent parser generates a loop out of straight-line code: an unconditional branch back to loopStart, followed by here() fixing loopExit to the instruction after it. " ++ actionNote)
                |> callsCodeGen [ "CodeGen.branch", "class Label" ]
            ]
    }



-- VarList ------------------------------------------------------------------


varListProduction : Production
varListProduction =
    { name = "VarList"
    , method = "VarList"
    , lines =
        [ gline 2
            [ varying
                [ core "VarList"
                , since ScopeRules "<StackFrame frame"
                , since TypeRules ", int type"
                , since ScopeRules ">"
                ]
                |> own 0
            ]
        , gline 2
            [ txt "="
            , varying
                [ core "OneVar"
                , since ScopeRules "<frame"
                , since TypeRules ", type"
                , since ScopeRules ">"
                ]
                |> own 1
            ]
        , gline 4
            [ txt "{" |> own 2
            , varying [ since ErrorHandling "WEAK" ] |> own 3
            , txt "\",\"" |> own 4
            , varying
                [ core "OneVar"
                , since ScopeRules "<frame"
                , since TypeRules ", type"
                , since ScopeRules ">"
                ]
                |> own 5
            , txt "}" |> own 2
            ]
        , gline 2 [ txt "." ]
        ]
    , segments =
        Array.fromList
            [ seg Head 0 0 "Two formal attributes become two parameters" ("Both attributes are inputs, so both become parameters and the method still returns void. Note that frame arrives at phase 3 and type at phase 4, so the signature grows in two steps. " ++ headNote)
            , seg NonTerminal 1 1 "The first element, matched unconditionally" nonTerminalNote
            , seg Iterate 2 5 "{ ... } becomes the while loop around the repetitions" ("Compare this guard with the one in Block. There the guard is StartOf(2); here the WEAK separator has taken the guard over entirely. " ++ iterateNote)
            , seg Weak 2 2 "WEAK at the head of an iteration becomes WeakSeparator" weakSeparatorNote
            , seg Terminal 2 3 "\",\" is consumed by the loop guard itself" ("At phase 1 the comma generates an explicit Get() inside the loop. From phase 2 the WEAK separator swallows it: WeakSeparator calls Get() itself when the comma is present, so the separate Get() disappears. " ++ terminalCaseNote)
            , seg NonTerminal 4 4 "Each further element" nonTerminalNote
            ]
    }



-- OneVar -------------------------------------------------------------------


oneVarProduction : Production
oneVarProduction =
    { name = "OneVar"
    , method = "OneVar"
    , lines =
        [ gline 2
            [ varying
                [ core "OneVar"
                , since ScopeRules "<StackFrame frame"
                , since TypeRules ", int type"
                , since ScopeRules ">"
                ]
                |> own 0
            ]
        , gline 6 [ varying [ since ScopeRules "(." ] |> own 1 ]
        , gline 9 [ varying [ since TypeRules "int expType;" ] |> own 1 ]
        , gline 9 [ varying [ since ScopeRules "Entry var = new Entry();" ] |> own 1 ]
        , gline 6 [ varying [ since ScopeRules ".)" ] |> own 1 ]
        , gline 2
            [ txt "="
            , varying [ core "Ident", since ScopeRules "<out var.name>" ] |> own 2
            ]
        , gline 6 [ varying [ since ScopeRules "(." ] |> own 3 ]
        , gline 9 [ varying [ since ScopeRules "var.kind = Kinds.Var;" ] |> own 3 ]
        , gline 9 [ varying [ since TypeRules "var.type = type;" ] |> own 3 ]
        , gline 9 [ varying [ since ScopeRules "var.offset = frame.size;" ] |> own 3 ]
        , gline 9 [ varying [ since ScopeRules "frame.size++;" ] |> own 3 ]
        , gline 6 [ varying [ since ScopeRules ".)" ] |> own 3 ]
        , gline 4 [ txt "(" |> own 4, txt "AssignOp" |> own 5 ]
        , gline 8 [ varying [ since CodeGeneration "(. CodeGen.loadAddress(var); .)" ] |> own 6 ]
        , gline 6 [ varying [ core "Expression", since TypeRules "<out expType>" ] |> own 7 ]
        , gline 10 [ varying [ since TypeRules "(." ] |> own 8 ]
        , gline 13 [ varying [ since TypeRules "if (!assignable(var.type, expType))" ] |> own 8 ]
        , gline 15 [ varying [ since TypeRules "SemError(\"incompatible types in assignment\");" ] |> own 8 ]
        , gline 13 [ varying [ since CodeGeneration "CodeGen.assign(var.type);" ] |> own 8 ]
        , gline 10 [ varying [ since TypeRules ".)" ] |> own 8 ]
        , gline 4 [ txt "|" |> own 4 |> tight ]
        , gline 4 [ txt ")" |> own 4 |> tight ]
        , gline 6 [ varying [ since ScopeRules "(. Table.insert(var); .)" ] |> own 9 ]
        , gline 2 [ txt "." ]
        ]
    , segments =
        Array.fromList
            [ seg Head 0 0 "OneVar takes the frame and the declared type" headNote
            , seg Action 1 2 "Local declarations for the method" actionNote
                |> callsTable [ "class Entry" ]
            , seg NonTerminal 3 3 "Ident() returns the name, stored straight into the entry" ("The out attribute is written as <out var.name>, so Coco/R assigns the returned value directly into the field: var.name = Ident(). " ++ nonTerminalNote)
            , seg Action 4 7 "Filling in the entry and allocating its slot" ("var.offset = frame.size followed by frame.size++ is the whole of Parva's storage allocation: each variable takes the next slot in the frame, and the running total becomes the DSP operand that Parva() patches at the end. " ++ actionNote)
                |> callsTable [ "class Kinds", "class StackFrame" ]
            , seg Alternation 8 16 "An alternation with an EMPTY second branch" ("The second alternative is empty -- there is nothing between the bar and the closing bracket. Coco/R still has to generate an arm for it, so it emits an else-if whose condition is the FOLLOW set (semicolon or comma) and whose body is empty, then a final else SynErr for anything else. An empty alternative is how a grammar says 'or nothing', and it is why an uninitialised variable is legal here. " ++ alternationNote)
            , seg NonTerminal 9 9 "AssignOp matches the equals sign" nonTerminalNote
            , seg Action 10 10 "Push the variable's address before evaluating the expression" ("The address has to be underneath the value when sto executes, so this action must come BEFORE Expression. Move it after, and the operands are the wrong way round on the stack. " ++ actionNote)
                |> callsCodeGen [ "CodeGen.loadAddress" ]
            , seg NonTerminal 11 11 "Expression() returns the type of what it evaluated" nonTerminalNote
            , seg Action 12 14 "Check the assignment, then emit the store" actionNote
                |> callsCodeGen [ "CodeGen.assign" ]
            , seg Action 17 17 "Insert the entry last, so a variable cannot use itself" ("Because Table.insert comes after the whole alternation, the name is not yet in scope while its own initialiser is parsed. " ++ actionNote)
                |> callsTable [ "Table.insert" ]
            ]
    }



-- Designator ---------------------------------------------------------------


designatorProduction : Production
designatorProduction =
    { name = "Designator"
    , method = "Designator"
    , lines =
        [ gline 2 [ varying [ core "Designator", since ScopeRules "<out DesType des>" ] |> own 0 ]
        , gline 6 [ varying [ since ScopeRules "(." ] |> own 1 ]
        , gline 9 [ varying [ since ScopeRules "String name;" ] |> own 1 ]
        , gline 9 [ varying [ since TypeRules "int indexType;" ] |> own 1 ]
        , gline 6 [ varying [ since ScopeRules ".)" ] |> own 1 ]
        , gline 2 [ txt "=", varying [ core "Ident", since ScopeRules "<out name>" ] |> own 2 ]
        , gline 6 [ varying [ since ScopeRules "(." ] |> own 3 ]
        , gline 9 [ varying [ since ScopeRules "Entry entry = Table.find(name);" ] |> own 3 ]
        , gline 9 [ varying [ since ScopeRules "boolean notDeclared = !entry.declared;" ] |> own 3 ]
        , gline 9 [ varying [ since ScopeRules "if (notDeclared) {" ] |> own 3 ]
        , gline 12 [ varying [ since ScopeRules "SemError(\"undeclared identifier\");" ] |> own 3 ]
        , gline 12 [ varying [ since ScopeRules "entry = new Entry();  // new is critical" ] |> own 3 ]
        , gline 12 [ varying [ since ScopeRules "entry.name = name;    // make an entry anyway" ] |> own 3 ]
        , gline 12 [ varying [ since ScopeRules "entry.kind = Kinds.Var;" ] |> own 3 ]
        , gline 12 [ varying [ since TypeRules "entry.type = Types.noType;" ] |> own 3 ]
        , gline 12 [ varying [ since ScopeRules "entry.offset = 0;     // unimportant" ] |> own 3 ]
        , gline 12 [ varying [ since ScopeRules "Table.insert(entry);" ] |> own 3 ]
        , gline 9 [ varying [ since ScopeRules "}" ] |> own 3 ]
        , gline 9 [ varying [ since ScopeRules "des = new DesType(entry);" ] |> own 3 ]
        , gline 9 [ varying [ since CodeGeneration "if (entry.kind == Kinds.Var)" ] |> own 3 ]
        , gline 11 [ varying [ since CodeGeneration "CodeGen.loadAddress(entry);" ] |> own 3 ]
        , gline 6 [ varying [ since ScopeRules ".)" ] |> own 3 ]
        , gline 4 [ txt "[" |> own 4, txt "\"[\"" |> own 5 ]
        , gline 8 [ varying [ since TypeRules "(." ] |> own 6 ]
        , gline 11 [ varying [ since TypeRules "if (notDeclared) entry.type++;" ] |> own 6 ]
        , gline 11 [ varying [ since TypeRules "else if (isArray(des.type)) des.type--;" ] |> own 6 ]
        , gline 11 [ varying [ since TypeRules "else SemError(\"unexpected subscript\");" ] |> own 6 ]
        , gline 11 [ varying [ since TypeRules "if (des.entry.kind != Kinds.Var)" ] |> own 6 ]
        , gline 13 [ varying [ since TypeRules "SemError(\"unexpected subscript\");" ] |> own 6 ]
        , gline 11 [ varying [ since CodeGeneration "CodeGen.dereference();" ] |> own 6 ]
        , gline 8 [ varying [ since TypeRules ".)" ] |> own 6 ]
        , gline 6 [ varying [ core "Expression", since TypeRules "<out indexType>" ] |> own 7 ]
        , gline 8 [ varying [ since TypeRules "(." ] |> own 8 ]
        , gline 11 [ varying [ since TypeRules "if (!isArith(indexType))" ] |> own 8 ]
        , gline 13 [ varying [ since TypeRules "SemError(\"invalid subscript type\");" ] |> own 8 ]
        , gline 11 [ varying [ since CodeGeneration "CodeGen.index();" ] |> own 8 ]
        , gline 8 [ varying [ since TypeRules ".)" ] |> own 8 ]
        , gline 6 [ txt "\"]\"" |> own 9 ]
        , gline 4 [ txt "]" |> own 4 ]
        , gline 2 [ txt "." ]
        ]
    , segments =
        Array.fromList
            [ seg Head 0 1 "A single <out> attribute becomes the return type" ("There is one out attribute and no input attributes, so Designator() takes nothing and returns a DesType. Coco/R declares the local des for you at the top and adds return des at the bottom. " ++ headNote)
            , seg Action 2 3 "Local declarations" actionNote
            , seg NonTerminal 4 4 "Ident() supplies the name to look up" nonTerminalNote
            , seg Action 5 18 "Look the name up, invent an entry if it is missing, load its address" ("Table.find never returns null: it returns a sentinel whose declared flag is false. Inventing a real entry for an undeclared name is deliberate error recovery -- it stops the same identifier being reported again on every subsequent use. " ++ actionNote)
                |> callsTable [ "Table.find", "class Entry", "Table.insert", "class DesType" ]
                |> callsCodeGen [ "CodeGen.loadAddress" ]
            , seg Optional 19 32 "[ \"[\" ... \"]\" ] becomes if (la.kind == lbrack_Sym)" optionNote
            , seg Terminal 20 20 "\"[\" becomes a bare Get()" terminalCaseNote
            , seg Action 21 26 "Strip one level of array-ness, then dereference" ("Parva encodes an array type as the element type plus one, so des.type-- turns int[] into int. " ++ actionNote)
                |> callsCodeGen [ "CodeGen.dereference" ]
            , seg NonTerminal 27 27 "Expression() computes the subscript" nonTerminalNote
            , seg Action 28 30 "Check the subscript type and emit the index instruction" actionNote
                |> callsCodeGen [ "CodeGen.index" ]
            , seg Terminal 31 31 "\"]\" becomes Expect(rbrack_Sym)" ("This terminal is back in a sequence rather than at the head of an alternative, so it needs a full Expect rather than a bare Get. " ++ terminalExpectNote)
            ]
    }


-- Primary ------------------------------------------------------------------


primaryProduction : Production
primaryProduction =
    { name = "Primary"
    , method = "Primary"
    , lines =
        [ gline 2 [ varying [ core "Primary", since TypeRules "<out int type>" ] |> own 0 ]
        , gline 6 [ varying [ since ScopeRules "(." ] |> own 1 ]
        , gline 9 [ varying [ since TypeRules "type = Types.noType;" ] |> own 1 ]
        , gline 9
            [ varying
                [ onlyIn TypeRules CodeGeneration "int size;"
                , since CodeGeneration "int size; boolean upper = true;"
                ]
                |> own 1
            ]
        , gline 9 [ varying [ since ScopeRules "DesType des;" ] |> own 1 ]
        , gline 9 [ varying [ since TypeRules "ConstRec con;" ] |> own 1 ]
        , gline 6 [ varying [ since ScopeRules ".)" ] |> own 1 ]
        , gline 2
            [ txt "="
            , gap 2
            , varying [ core "Designator", since ScopeRules "<out des>" ] |> own 3
            ]
        , gline 8 [ varying [ since TypeRules "(." ] |> own 4 ]
        , gline 11 [ varying [ since TypeRules "type = des.type;" ] |> own 4 ]
        , gline 11 [ varying [ since CodeGeneration "switch (des.entry.kind) {" ] |> own 4 ]
        , gline 13 [ varying [ since CodeGeneration "case Kinds.Var:" ] |> own 4 ]
        , gline 15 [ varying [ since CodeGeneration "CodeGen.dereference();" ] |> own 4 ]
        , gline 15 [ varying [ since CodeGeneration "break;" ] |> own 4 ]
        , gline 13 [ varying [ since CodeGeneration "case Kinds.Con:" ] |> own 4 ]
        , gline 15 [ varying [ since CodeGeneration "CodeGen.loadConstant(des.entry.value);" ] |> own 4 ]
        , gline 15 [ varying [ since CodeGeneration "break;" ] |> own 4 ]
        , gline 13 [ varying [ since CodeGeneration "default:" ] |> own 4 ]
        , gline 15 [ varying [ since CodeGeneration "SemError(\"wrong kind of identifier\");" ] |> own 4 ]
        , gline 15 [ varying [ since CodeGeneration "break;" ] |> own 4 ]
        , gline 11 [ varying [ since CodeGeneration "}" ] |> own 4 ]
        , gline 8 [ varying [ since TypeRules ".)" ] |> own 4 ]
        , gline 4
            [ txt "|" |> own 2 |> tight
            , varying [ core "Constant", since TypeRules "<out con>" ] |> own 5
            ]
        , gline 8 [ varying [ since TypeRules "(." ] |> own 6 ]
        , gline 11 [ varying [ since TypeRules "type = con.type;" ] |> own 6 ]
        , gline 11 [ varying [ since CodeGeneration "CodeGen.loadConstant(con.value);" ] |> own 6 ]
        , gline 8 [ varying [ since TypeRules ".)" ] |> own 6 ]
        , gline 4 [ txt "|" |> own 2 |> tight, txt "\"new\"" |> own 7 ]
        , gline 6
            [ txt "(" |> own 8
            , varying [ core "BasicType", since TypeRules "<out type>" ] |> own 9
            ]
        , gline 8
            [ txt "\"[\"" |> own 10
            , varying [ core "Expression", since TypeRules "<out size>" ] |> own 11
            ]
        , gline 11 [ varying [ since TypeRules "(." ] |> own 12 ]
        , gline 14 [ varying [ since TypeRules "if (!isArith(size))" ] |> own 12 ]
        , gline 16 [ varying [ since TypeRules "SemError(\"array size must be integer\");" ] |> own 12 ]
        , gline 14 [ varying [ since TypeRules "type++;" ] |> own 12 ]
        , gline 11 [ varying [ since TypeRules ".)" ] |> own 12 ]
        , gline 8 [ txt "\"]\"" |> own 13 ]
        , gline 6 [ txt ")" |> own 8 ]
        , gline 8 [ varying [ since CodeGeneration "(. CodeGen.allocate(); .)" ] |> own 14 ]
        , gline 4 [ txt "|" |> own 2 |> tight, txt "\"(\"" |> own 15 ]
        , gline 6
            [ txt "(" |> own 16
            , txt "\"char\"" |> own 17
            , txt "\")\"" |> own 18
            ]
        , gline 8 [ varying [ core "Factor", since TypeRules "<out type>" ] |> own 19 ]
        , gline 11 [ varying [ since TypeRules "(." ] |> own 20 ]
        , gline 14 [ varying [ since TypeRules "if (!isArith(type))" ] |> own 20 ]
        , gline 16 [ varying [ since TypeRules "SemError(\"invalid cast\");" ] |> own 20 ]
        , gline 14
            [ varying
                [ onlyIn TypeRules CodeGeneration "else type = Types.charType;"
                , since CodeGeneration "else { type = Types.charType; CodeGen.castToChar(); }"
                ]
                |> own 20
            ]
        , gline 11 [ varying [ since TypeRules ".)" ] |> own 20 ]
        , gline 6
            [ txt "|" |> own 16 |> tight
            , txt "\"int\"" |> own 21
            , txt "\")\"" |> own 22
            ]
        , gline 8 [ varying [ core "Factor", since TypeRules "<out type>" ] |> own 23 ]
        , gline 11 [ varying [ since TypeRules "(." ] |> own 24 ]
        , gline 14 [ varying [ since TypeRules "if (!isArith(type))" ] |> own 24 ]
        , gline 16 [ varying [ since TypeRules "SemError(\"invalid cast\");" ] |> own 24 ]
        , gline 14 [ varying [ since TypeRules "else type = Types.intType;" ] |> own 24 ]
        , gline 11 [ varying [ since TypeRules ".)" ] |> own 24 ]
        , gline 6
            [ txt "|" |> own 16 |> tight
            , varying [ core "Expression", since TypeRules "<out type>" ] |> own 25
            , txt "\")\"" |> own 26
            ]
        , gline 6 [ txt ")" |> own 16 |> tight ]
        , gline 4
            [ txt "|" |> own 2 |> tight
            , txt "(" |> own 27
            , txt "\"cap\"" |> own 28
            ]
        , gline 11 [ varying [ since CodeGeneration "(. upper = true; .)" ] |> own 29 ]
        , gline 6 [ txt "|" |> own 27 |> tight, txt "\"low\"" |> own 30 ]
        , gline 11 [ varying [ since CodeGeneration "(. upper = false; .)" ] |> own 31 ]
        , gline 6 [ txt ")" |> own 27 |> tight ]
        , gline 6
            [ txt "\"(\"" |> own 32
            , varying [ core "Expression", since TypeRules "<out type>" ] |> own 33
            ]
        , gline 8 [ varying [ since TypeRules "(." ] |> own 34 ]
        , gline 11 [ varying [ since TypeRules "if (type != Types.charType)" ] |> own 34 ]
        , gline 13 [ varying [ since TypeRules "SemError(\"character argument needed\");" ] |> own 34 ]
        , gline 11 [ varying [ since TypeRules "type = Types.charType;" ] |> own 34 ]
        , gline 11 [ varying [ since CodeGeneration "if (upper) CodeGen.capChar();" ] |> own 34 ]
        , gline 11 [ varying [ since CodeGeneration "else CodeGen.lowChar();" ] |> own 34 ]
        , gline 8 [ varying [ since TypeRules ".)" ] |> own 34 ]
        , gline 6 [ txt "\")\"" |> own 35 ]
        , gline 4
            [ txt "|" |> own 2 |> tight
            , txt "\"length\"" |> own 36
            , txt "\"(\"" |> own 37
            , varying [ core "Designator", since ScopeRules "<out des>" ] |> own 38
            ]
        , gline 8 [ varying [ since TypeRules "(." ] |> own 39 ]
        , gline 11 [ varying [ since TypeRules "if (!isArray(des.type))" ] |> own 39 ]
        , gline 13 [ varying [ since TypeRules "SemError(\"array type needed for length\");" ] |> own 39 ]
        , gline 11 [ varying [ since CodeGeneration "CodeGen.dereference();" ] |> own 39 ]
        , gline 11 [ varying [ since CodeGeneration "CodeGen.dereference();" ] |> own 39 ]
        , gline 11 [ varying [ since TypeRules "type = Types.intType;" ] |> own 39 ]
        , gline 8 [ varying [ since TypeRules ".)" ] |> own 39 ]
        , gline 6 [ txt "\")\"" |> own 40 ]
        , gline 2 [ txt "." ]
        ]
    , segments =
        Array.fromList
            [ seg Head 0 1 "Primary returns the type of whatever it recognised" headNote
            , seg Action 2 5 "Local declarations for every alternative" ("One declaration block serves all six alternatives, even though any given parse uses only one of them. " ++ actionNote)
                |> callsTable [ "class DesType", "class ConstRec", "class Types" ]
            , seg Alternation 6 94 "Six alternatives become one switch on la.kind" ("The FIRST sets of these six alternatives are disjoint, which is exactly the LL(1) condition. If two of them shared a symbol Coco/R would refuse to generate this switch and report an LL(1) conflict instead. " ++ alternationNote)
            , seg NonTerminal 7 22 "A named constant or variable" nonTerminalNote
            , seg Action 9 20 "Dereference a variable, or load a constant's value" ("A designator has already pushed an ADDRESS. For a variable that address must be dereferenced to get a value; for a named constant there is no address at all, so the value is loaded directly. The kind check is what tells the two apart. " ++ actionNote)
                |> callsCodeGen [ "CodeGen.dereference", "CodeGen.loadConstant" ]
            , seg NonTerminal 23 28 "A literal constant" nonTerminalNote
            , seg Action 25 26 "Push the literal's value" actionNote
                |> callsCodeGen [ "CodeGen.loadConstant" ]
            , seg Terminal 29 30 "\"new\" becomes case new_Sym with a bare Get()" terminalCaseNote
            , seg Alternation 31 37 "A group with only ONE alternative generates no branch at all" ("There is no bar inside these brackets, so there is nothing to choose between and Coco/R emits the sequence straight out. The group exists purely to delimit what the action after the closing bracket applies to. " ++ alternationNote)
            , seg NonTerminal 31 31 "BasicType() returns the element type" nonTerminalNote
            , seg Terminal 32 32 "\"[\" becomes Expect(lbrack_Sym)" terminalExpectNote
            , seg NonTerminal 33 33 "Expression() computes the array size" nonTerminalNote
            , seg Action 34 36 "Check the size, then make it an array type" ("Parva numbers an array type as its element type plus one, so type++ is literally how int becomes int[]. " ++ actionNote)
                |> callsTable [ "class Types" ]
            , seg Terminal 37 37 "\"]\" becomes Expect(rbrack_Sym)" terminalExpectNote
            , seg Action 38 38 "An action after the group runs once the whole group is matched" actionNote
                |> callsCodeGen [ "CodeGen.allocate" ]
            , seg Terminal 41 42 "\"(\" becomes case lparen_Sym with a bare Get()" terminalCaseNote
            , seg Alternation 43 60 "Three alternatives become an if / else-if chain" ("With only three alternatives Coco/R prefers a chain to a switch. The third arm is guarded by StartOf(11) rather than a single comparison, because FIRST(Expression) is a large set. " ++ alternationNote)
            , seg Terminal 44 44 "\"char\" becomes a bare Get()" terminalCaseNote
            , seg Terminal 45 45 "\")\" becomes Expect(rparen_Sym)" terminalExpectNote
            , seg NonTerminal 46 46 "Factor() parses the operand being cast" nonTerminalNote
            , seg Action 47 49 "A cast to char is a range check at run time" ("i2c is emitted because a char in Parva is a restricted int; the check cannot be done at compile time. " ++ actionNote)
                |> callsCodeGen [ "CodeGen.castToChar" ]
            , seg Terminal 51 51 "\"int\" becomes a bare Get()" terminalCaseNote
            , seg Terminal 52 52 "\")\" becomes Expect(rparen_Sym)" terminalExpectNote
            , seg NonTerminal 53 53 "Factor() parses the operand being cast" nonTerminalNote
            , seg Action 54 56 "A cast to int emits nothing at all" ("Widening char to int needs no instruction, so this action changes the type and leaves the generated code alone. A type rule with no code generation attached is perfectly normal. " ++ actionNote)
            , seg NonTerminal 58 58 "A plain parenthesised expression" nonTerminalNote
            , seg Terminal 59 59 "\")\" becomes Expect(rparen_Sym)" terminalExpectNote
            , seg Alternation 63 70 "( \"cap\" | \"low\" ) becomes an inner if / else" ("Both terminals reach the same case arm, because the outer switch cannot tell them apart from a single case label. The inner if then decides which of the two it actually was. " ++ alternationNote)
            , seg Terminal 64 65 "\"cap\" is tested by the inner if" terminalCaseNote
            , seg Action 66 66 "Remember which of the two was matched" actionNote
            , seg Terminal 67 68 "\"low\" is the else arm" ("There is nothing left to test in the else arm, so Coco/R emits only Get(). " ++ terminalCaseNote)
            , seg Action 69 69 "Remember which of the two was matched" actionNote
            , seg Terminal 71 71 "\"(\" becomes Expect(lparen_Sym)" terminalExpectNote
            , seg NonTerminal 72 72 "Expression() supplies the character" nonTerminalNote
            , seg Action 73 77 "Check the argument type, then emit capc or lowc" ("The single flag set two segments ago now selects between two different instructions. " ++ actionNote)
                |> callsCodeGen [ "CodeGen.capChar", "CodeGen.lowChar" ]
            , seg Terminal 78 78 "\")\" becomes Expect(rparen_Sym)" terminalExpectNote
            , seg Terminal 81 82 "\"length\" becomes case length_Sym with a bare Get()" terminalCaseNote
            , seg Terminal 83 83 "\"(\" becomes Expect(lparen_Sym)" terminalExpectNote
            , seg NonTerminal 84 84 "Designator() must name an array" nonTerminalNote
            , seg Action 85 89 "Two dereferences: one for the reference, one for the length" ("The designator pushed the address of the variable. The first ldv fetches the heap pointer stored there; the second fetches the length word that anew wrote at the front of the block. " ++ actionNote)
                |> callsCodeGen [ "CodeGen.dereference" ]
            , seg Terminal 90 90 "\")\" becomes Expect(rparen_Sym)" terminalExpectNote
            ]
    }



-- AddOp --------------------------------------------------------------------


addOpProduction : Production
addOpProduction =
    { name = "AddOp"
    , method = "AddOp"
    , lines =
        [ gline 2 [ varying [ core "AddOp", since CodeGeneration "<out int op>" ] |> own 0 ]
        , gline 6 [ varying [ since CodeGeneration "(. op = CodeGen.nop; .)" ] |> own 1 ]
        , gline 2 [ txt "=", gap 2, txt "\"+\"" |> own 3 ]
        , gline 6 [ varying [ since CodeGeneration "(. op = CodeGen.add; .)" ] |> own 4 ]
        , gline 4 [ txt "|" |> own 2 |> tight, txt "\"-\"" |> own 5 ]
        , gline 6 [ varying [ since CodeGeneration "(. op = CodeGen.sub; .)" ] |> own 6 ]
        , gline 2 [ txt "." ]
        ]
    , segments =
        Array.fromList
            [ seg Head 0 1 "The smallest useful shape: recognise a token, return a number" ("AddOp exists only to turn a symbol into an opcode. It contributes nothing to the parse that inlining the two terminals would not, but it keeps the arithmetic productions readable. " ++ headNote)
            , seg Action 2 2 "A default, in case neither alternative matches" ("If the input is broken and SynErr fires, op must still hold something, so it is initialised before the choice is made. " ++ actionNote)
                |> callsCodeGen [ "CodeGen opcodes" ]
            , seg Alternation 3 9 "Two alternatives become a plain if / else" ("With two single-terminal alternatives there is nothing to gain from a switch. " ++ alternationNote)
            , seg Terminal 3 4 "\"+\" is tested by the if" terminalCaseNote
            , seg Action 5 5 "Select the add opcode" actionNote
                |> callsCodeGen [ "CodeGen opcodes" ]
            , seg Terminal 6 7 "\"-\" is tested by the else-if" terminalCaseNote
            , seg Action 8 8 "Select the sub opcode" actionNote
                |> callsCodeGen [ "CodeGen opcodes" ]
            ]
    }



-- Ident --------------------------------------------------------------------


identProduction : Production
identProduction =
    { name = "Ident"
    , method = "Ident"
    , lines =
        [ gline 2 [ varying [ core "Ident", since ScopeRules "<out String name>" ] |> own 0 ]
        , gline 2 [ txt "=", txt "identifier" |> own 1 ]
        , gline 6 [ varying [ since ScopeRules "(. name = token.val; .)" ] |> own 2 ]
        , gline 2 [ txt "." ]
        ]
    , segments =
        Array.fromList
            [ seg Head 0 1 "One out attribute, so Ident returns a String" ("This is the whole rule in miniature: <out String name> becomes the return type, Coco/R declares the local, and the action fills it in. " ++ headNote)
            , seg Terminal 2 2 "A NAMED token, not a literal" ("identifier is declared in the TOKENS section rather than written as a quoted literal, so the scanner recognises it with a finite automaton. In the parser it looks exactly like any other terminal: Expect(identifier_Sym). " ++ terminalExpectNote)
            , seg Action 3 3 "token.val holds the text the scanner just matched" ("token is the LAST token accepted and la is the lookahead, so this action must run after Expect. Reading la.val here would fetch the wrong word -- a mistake worth making once. " ++ actionNote)
            ]
    }
