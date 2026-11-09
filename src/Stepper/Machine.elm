module Stepper.Machine exposing
    ( Built
    , CheckRec
    , DiagRec
    , Kid(..)
    , Node
    , Problem
    , ScanRec
    , Scope
    , State
    , Status(..)
    , Zipper
    , build
    , memAt
    , opName
    , step
    )

{-| Replays a trace one event at a time and enforces every invariant in
docs/TRACE.md section 4.3. `step` is the only way to advance a State, and the
first violated invariant stops the whole build: a rejected trace never
reaches the view.
-}

import Array exposing (Array)
import Dict exposing (Dict)
import Stepper.Trace as T exposing (Cause(..), EndInfo, Event(..), Pos, Severity(..), Stamped, Sym, Tok, Trace)


type alias State =
    { applied : Int
    , scanned : Array ScanRec
    , pending : Maybe Int
    , tree : Zipper
    , scopes : List Scope
    , checks : List CheckRec
    , mem : Dict Int Int
    , codeTop : Int
    , strBottom : Int
    , lastMem : Maybe Int
    , diags : List DiagRec
    , status : Status
    }


type alias ScanRec =
    { tok : Tok, pragma : Bool, consumed : Bool, atEvent : Int }


type alias Scope =
    { depth : Int, syms : List Sym }


type alias CheckRec =
    { rule : String, args : List String, ok : Bool, atEvent : Int, at : Maybe Pos }


type alias DiagRec =
    { severity : Severity, pos : Pos, msg : String, atEvent : Int }


type Status
    = Running
    | Finished EndInfo
    | Aborted String


{-| The parse tree as a zipper with a synthetic root, so it is never empty.
Kids are newest first; crumbs are the open ancestors, innermost first.
-}
type alias Node =
    { prod : String, kids : List Kid, enteredAt : Int }


type Kid
    = KNode Node
    | KTok Int


type alias Zipper =
    { focus : Node, crumbs : List Node }


type alias Problem =
    { event : Int, reason : String }


{-| states[k] is the state after events 0 .. k-1, so there are n + 1 of them.
-}
type alias Built =
    { states : Array State
    , matchExit : Dict Int Int
    , matchEnter : Dict Int Int
    }



-- BUILD


build : Trace -> Result Problem Built
build trace =
    let
        n =
            Array.length trace.events

        start =
            initial trace

        loop k state acc =
            case Array.get k trace.events of
                Nothing ->
                    Ok ( state, acc )

                Just stamped ->
                    case step trace stamped state of
                        Ok next ->
                            loop (k + 1) next (next :: acc)

                        Err problem ->
                            Err problem
    in
    if n > T.maxEvents then
        Err { event = T.maxEvents, reason = "the trace has " ++ String.fromInt n ++ " events; the limit is 200000" }

    else
        loop 0 start [ start ]
            |> Result.andThen
                (\( final, acc ) ->
                    case final.status of
                        Running ->
                            Err { event = n, reason = "I2: the trace has no end or abort event" }

                        _ ->
                            let
                                ( exits, enters ) =
                                    matches trace.events
                            in
                            Ok { states = Array.fromList (List.reverse acc), matchExit = exits, matchEnter = enters }
                )


initial : Trace -> State
initial trace =
    { applied = 0
    , scanned = Array.empty
    , pending = Nothing
    , tree = { focus = { prod = "", kids = [], enteredAt = -1 }, crumbs = [] }
    , scopes = []
    , checks = []
    , mem = Dict.empty
    , codeTop = 0
    , strBottom = trace.memSize
    , lastMem = Nothing
    , diags = []
    , status = Running
    }


{-| enter index to exit index, and back. Only called once the build has
already proved that enters and exits nest properly.
-}
matches : Array Stamped -> ( Dict Int Int, Dict Int Int )
matches =
    Array.foldl
        (\stamped ( stack, exits, enters ) ->
            case ( stamped.event, stack ) of
                ( Enter _, _ ) ->
                    ( stamped.i :: stack, exits, enters )

                ( Exit _, open :: rest ) ->
                    ( rest, Dict.insert open stamped.i exits, Dict.insert stamped.i open enters )

                _ ->
                    ( stack, exits, enters )
        )
        ( [], Dict.empty, Dict.empty )
        >> (\( _, exits, enters ) -> ( exits, enters ))



-- STEP


step : Trace -> Stamped -> State -> Result Problem State
step trace stamped state =
    let
        next =
            { state | applied = state.applied + 1, lastMem = Nothing }
    in
    sequenceIndex stamped state
        |> Result.andThen (\_ -> notFinished state)
        |> Result.andThen (\_ -> apply trace stamped next)
        |> Result.mapError (\reason -> { event = state.applied, reason = reason })


{-| I1 -}
sequenceIndex : Stamped -> State -> Result String ()
sequenceIndex stamped state =
    if stamped.i == state.applied then
        Ok ()

    else
        Err ("I1: event at position " ++ String.fromInt state.applied ++ " has i = " ++ String.fromInt stamped.i ++ " (truncated or merged file?)")


{-| I2 -}
notFinished : State -> Result String ()
notFinished state =
    case state.status of
        Running ->
            Ok ()

        Finished _ ->
            Err "I2: an event follows the end event"

        Aborted _ ->
            Err "I2: an event follows the abort event"


apply : Trace -> Stamped -> State -> Result String State
apply trace { i, at, event } s =
    case event of
        Scan tok pragma ->
            scan trace i tok pragma s

        Consume tok ->
            consume tok s

        Enter prod ->
            Ok { s | tree = { focus = { prod = prod, kids = [], enteredAt = i }, crumbs = s.tree.focus :: s.tree.crumbs } }

        Exit prod ->
            exit prod s

        ScopeOpen depth ->
            scopeOpen depth s

        ScopeClose depth ->
            scopeClose depth s

        SymInsert sym depth _ ->
            insert sym depth s

        SymFind name found ->
            Ok (addCheck "find" [ name ] (found /= Nothing) i at s)

        Check rule args ok ->
            Ok (addCheck rule args ok i at s)

        Mem write ->
            memory trace write s

        Diag severity pos msg ->
            Ok { s | diags = { severity = severity, pos = pos, msg = msg, atEvent = i } :: s.diags }

        End info ->
            finish trace info s

        Abort reason ->
            Ok { s | status = Aborted reason }



-- I3, I4: tokens


{-| Coco/R always numbers EOF 0, and Scanner.Init gives it the sentinel value
"EOF", which is not source text. So for EOF, I4 checks its position instead.
-}
eofKind : Int
eofKind =
    0


scan : Trace -> Int -> Tok -> Bool -> State -> Result String State
scan trace i tok pragma s =
    let
        previousPos =
            Array.get (Array.length s.scanned - 1) s.scanned |> Maybe.map (.tok >> .pos)

        record =
            { tok = tok, pragma = pragma, consumed = False, atEvent = i }
    in
    if not pragma && s.pending /= Nothing then
        Err "I3: a token is scanned while the previous lookahead is still pending"

    else if tok.kind == eofKind && tok.pos /= String.length trace.source then
        Err ("I4: EOF token at pos " ++ String.fromInt tok.pos ++ " but the source has " ++ String.fromInt (String.length trace.source) ++ " characters")

    else if tok.kind /= eofKind && tok.val /= "" && String.slice tok.pos (tok.pos + String.length tok.val) trace.source /= tok.val then
        Err ("I4: token \"" ++ tok.val ++ "\" is not in the source at pos " ++ String.fromInt tok.pos)

    else if Maybe.map (\p -> tok.pos < p) previousPos == Just True then
        Err ("I4: token pos " ++ String.fromInt tok.pos ++ " is before the previous token's")

    else
        Ok
            { s
                | scanned = Array.push record s.scanned
                , pending =
                    if pragma then
                        s.pending

                    else
                        Just (Array.length s.scanned)
            }


consume : Tok -> State -> Result String State
consume tok s =
    case s.pending |> Maybe.andThen (\k -> Array.get k s.scanned |> Maybe.map (Tuple.pair k)) of
        Nothing ->
            Err "I3: a token is consumed but no lookahead is pending"

        Just ( k, record ) ->
            if record.tok.pos == tok.pos && record.tok.kind == tok.kind then
                Ok
                    { s
                        | scanned = Array.set k { record | consumed = True } s.scanned
                        , pending = Nothing
                        , tree = addKid (KTok k) s.tree
                    }

            else
                Err ("I3: consumed token at pos " ++ String.fromInt tok.pos ++ " is not the pending lookahead (pos " ++ String.fromInt record.tok.pos ++ ")")



-- I5: productions


exit : String -> State -> Result String State
exit prod s =
    case s.tree.crumbs of
        parent :: rest ->
            if s.tree.focus.prod == prod then
                Ok { s | tree = { focus = { parent | kids = KNode s.tree.focus :: parent.kids }, crumbs = rest } }

            else
                Err ("I5: exit " ++ prod ++ " but the open production is " ++ s.tree.focus.prod)

        [] ->
            Err ("I5: exit " ++ prod ++ " but no production is open")


addKid : Kid -> Zipper -> Zipper
addKid kid zipper =
    let
        focus =
            zipper.focus
    in
    { zipper | focus = { focus | kids = kid :: focus.kids } }



-- I6: scopes


scopeOpen : Int -> State -> Result String State
scopeOpen depth s =
    if depth == List.length s.scopes + 1 then
        Ok { s | scopes = { depth = depth, syms = [] } :: s.scopes }

    else
        Err ("I6: scope.open to depth " ++ String.fromInt depth ++ " from depth " ++ String.fromInt (List.length s.scopes))


scopeClose : Int -> State -> Result String State
scopeClose depth s =
    if depth == List.length s.scopes - 1 && depth >= 0 then
        Ok { s | scopes = List.drop 1 s.scopes }

    else
        Err ("I6: scope.close to depth " ++ String.fromInt depth ++ " from depth " ++ String.fromInt (List.length s.scopes))


insert : Sym -> Int -> State -> Result String State
insert sym depth s =
    case s.scopes of
        top :: rest ->
            if depth == List.length s.scopes then
                Ok { s | scopes = { top | syms = sym :: top.syms } :: rest }

            else
                Err ("I6: sym.insert at depth " ++ String.fromInt depth ++ " but the open depth is " ++ String.fromInt (List.length s.scopes))

        [] ->
            Err "I6: sym.insert with no scope open"


addCheck : String -> List String -> Bool -> Int -> Maybe Pos -> State -> State
addCheck rule args ok i at s =
    { s | checks = { rule = rule, args = args, ok = ok, atEvent = i, at = at } :: s.checks }



-- I7, I8: memory


memAt : Int -> State -> Int
memAt addr state =
    Dict.get addr state.mem |> Maybe.withDefault 0


opName : Trace -> Int -> String
opName trace word =
    if word < 0 then
        ""

    else
        Array.get (remainderBy 256 word) trace.ops |> Maybe.map .name |> Maybe.withDefault ""


memory : Trace -> { addr : Int, old : Int, new : Int, cause : Cause } -> State -> Result String State
memory trace m s =
    let
        current =
            memAt m.addr s

        n =
            String.fromInt

        write state =
            { state
                | mem =
                    if m.new == 0 then
                        Dict.remove m.addr state.mem

                    else
                        Dict.insert m.addr m.new state.mem
                , lastMem = Just m.addr
            }
    in
    if m.addr < 0 || m.addr > trace.memSize then
        Err ("I8: write outside memory, at " ++ n m.addr)

    else if m.old /= current then
        Err ("I7: mem[" ++ n m.addr ++ "] is " ++ n current ++ " in the replay but the event says it was " ++ n m.old ++ " (a memory write was not hooked)")

    else
        case m.cause of
            Emit ->
                if m.addr == s.codeTop then
                    Ok (write { s | codeTop = s.codeTop + 1 })

                else
                    Err ("I8: emit at " ++ n m.addr ++ " but codeTop is " ++ n s.codeTop)

            Patch ->
                if m.addr >= s.codeTop then
                    Err ("I8: patch at " ++ n m.addr ++ ", not below codeTop " ++ n s.codeTop)

                else if m.new /= s.codeTop then
                    Err ("I8: patch writes " ++ n m.new ++ " but must write codeTop " ++ n s.codeTop)

                else if not (m.old == -1 || (m.old >= 0 && m.old < s.codeTop)) then
                    Err ("I8: patched word " ++ n m.old ++ " is not a forward-reference link")

                else
                    Ok (write s)

            FixDsp ->
                if m.addr >= s.codeTop then
                    Err ("I8: fixdsp at " ++ n m.addr ++ ", not below codeTop " ++ n s.codeTop)

                else if opName trace (memAt (m.addr - 1) s) /= "DSP" then
                    Err ("I8: fixdsp at " ++ n m.addr ++ " does not follow a DSP opcode")

                else
                    Ok (write s)

            StringData ->
                if m.addr /= s.strBottom - 1 then
                    Err ("I8: string word at " ++ n m.addr ++ " but the next string slot is " ++ n (s.strBottom - 1))

                else if m.addr < s.codeTop then
                    Err ("I8: string word at " ++ n m.addr ++ " overlaps code (codeTop " ++ n s.codeTop ++ ")")

                else
                    Ok (write { s | strBottom = m.addr })



-- I9: the end must reproduce the compiler's real output


finish : Trace -> EndInfo -> State -> Result String State
finish trace info s =
    let
        n =
            String.fromInt

        firstMismatch base words =
            Array.toIndexedList words
                |> List.filter (\( k, w ) -> memAt (base + k) s /= w)
                |> List.head
                |> Maybe.map (\( k, w ) -> "mem[" ++ n (base + k) ++ "] is " ++ n (memAt (base + k) s) ++ " in the replay but " ++ n w ++ " in the compiler")

        count predicate =
            List.length (List.filter (.severity >> predicate) s.diags)

        errorDiags =
            count (\severity -> severity /= Warning)

        warningDiags =
            count ((==) Warning)
    in
    if info.selfCheck /= "ok" then
        Err ("the compiler's own self-check failed: " ++ info.selfCheck)

    else if info.codeLength /= s.codeTop then
        Err ("I9: codeLength is " ++ n info.codeLength ++ " but the replay emitted " ++ n s.codeTop ++ " words")

    else if Array.length info.code /= info.codeLength then
        Err ("I9: end.code has " ++ n (Array.length info.code) ++ " words, not codeLength")

    else if info.initSP /= s.strBottom then
        Err ("I9: initSP is " ++ n info.initSP ++ " but the replayed strings start at " ++ n s.strBottom)

    else if Array.length info.strings /= trace.memSize - info.initSP then
        Err "I9: end.strings does not cover initSP .. memSize-1"

    else
        case ( firstMismatch 0 info.code, firstMismatch info.initSP info.strings ) of
            ( Just why, _ ) ->
                Err ("I9: code differs: " ++ why)

            ( _, Just why ) ->
                Err ("I9: strings differ: " ++ why)

            _ ->
                if not (List.isEmpty s.tree.crumbs) then
                    Err ("I9: production " ++ s.tree.focus.prod ++ " is still open at the end")

                else if errorDiags /= info.errors then
                    Err ("I9: " ++ n errorDiags ++ " error diags but the compiler counted " ++ n info.errors)

                else if warningDiags /= info.warnings then
                    Err ("I9: " ++ n warningDiags ++ " warning diags but the compiler counted " ++ n info.warnings)

                else
                    Ok { s | status = Finished info }
