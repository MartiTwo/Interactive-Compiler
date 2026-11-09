module Stepper.Main exposing (main)

{-| Parva stepper: replays a recorded compilation one event at a time.
Model, update and subscriptions; all HTML is in Stepper.View.
-}

import Array
import Browser
import Browser.Dom as Dom
import Browser.Events
import File exposing (File)
import File.Select
import Html exposing (Html)
import Json.Decode as D
import Stepper.Examples
import Stepper.Machine as Machine
import Stepper.Trace as Trace
import Stepper.View as View exposing (Example, Viewer)
import Task


type alias Model =
    { examples : List Example
    , loaded : Maybe Example
    , viewing : Maybe Viewer
    }


type Msg
    = Open String
    | Back
    | Goto Int
    | ToggleRaw
    | PickFile
    | GotFile File
    | GotText String String
    | NoOp


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( { examples = List.map (\( name, json ) -> load name json) Stepper.Examples.examples, loaded = Nothing, viewing = Nothing }, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


{-| Decode, then replay and validate. Either step failing is a ✗ with a reason.
-}
load : String -> String -> Example
load name json =
    { name = name
    , result =
        Trace.decode json
            |> Result.mapError (\why -> "not a valid trace file: " ++ why)
            |> Result.andThen
                (\trace ->
                    Machine.build trace
                        |> Result.map (Tuple.pair trace)
                        |> Result.mapError (\p -> "rejected at event " ++ String.fromInt p.event ++ ": " ++ p.reason)
                )
    }


open : Example -> Maybe Viewer
open example =
    case example.result of
        Ok ( trace, built ) ->
            Just { name = example.name, trace = trace, built = built, k = 0, rawCod = False }

        Err _ ->
            Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Open name ->
            ( { model | viewing = List.filter (.name >> (==) name) model.examples |> List.head |> Maybe.andThen open, loaded = Nothing }
            , Cmd.none
            )

        Back ->
            ( { model | viewing = Nothing }, Cmd.none )

        Goto k ->
            case model.viewing of
                Just v ->
                    ( { model | viewing = Just { v | k = clamp 0 (Array.length v.trace.events) k } }
                    , Cmd.batch (List.map keepInView View.scrollTargets)
                    )

                Nothing ->
                    ( model, Cmd.none )

        ToggleRaw ->
            ( { model | viewing = Maybe.map (\v -> { v | rawCod = not v.rawCod }) model.viewing }, Cmd.none )

        PickFile ->
            ( model, File.Select.file [ "application/json", ".json" ] GotFile )

        GotFile file ->
            ( model, Task.perform (GotText (File.name file)) (File.toString file) )

        GotText name json ->
            let
                example =
                    load name json
            in
            ( { model | loaded = Just example, viewing = open example }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


{-| Scroll a pane only when its marked line has gone out of sight.
-}
keepInView : ( String, String ) -> Cmd Msg
keepInView ( container, target ) =
    Task.map3
        (\box mark viewport ->
            let
                top =
                    mark.element.y - box.element.y
            in
            if top < 0 || top + mark.element.height > box.element.height then
                Dom.setViewportOf container viewport.viewport.x (max 0 (viewport.viewport.y + top - box.element.height / 3))

            else
                Task.succeed ()
        )
        (Dom.getElement container)
        (Dom.getElement target)
        (Dom.getViewportOf container)
        |> Task.andThen identity
        |> Task.attempt (\_ -> NoOp)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.viewing of
        Just v ->
            Browser.Events.onKeyDown (keys v)

        Nothing ->
            Sub.none


{-| ← and → step; ignored while typing in an input, so the slider's own arrow
handling does not step twice.
-}
keys : Viewer -> D.Decoder Msg
keys v =
    D.map2 Tuple.pair (D.field "key" D.string) (D.oneOf [ D.at [ "target", "tagName" ] D.string, D.succeed "" ])
        |> D.andThen
            (\( key, tag ) ->
                if tag == "INPUT" || tag == "TEXTAREA" || tag == "SELECT" then
                    D.fail "typing"

                else
                    case key of
                        "ArrowLeft" ->
                            D.succeed (Goto (v.k - 1))

                        "ArrowRight" ->
                            D.succeed (Goto (v.k + 1))

                        "Home" ->
                            D.succeed (Goto 0)

                        "End" ->
                            D.succeed (Goto (Array.length v.trace.events))

                        _ ->
                            D.fail "not a stepping key"
            )


view : Model -> Html Msg
view model =
    let
        actions =
            { open = Open, pickFile = PickFile, back = Back, goto = Goto, toggleRaw = ToggleRaw }
    in
    case model.viewing of
        Just v ->
            View.viewer actions v

        Nothing ->
            View.startPage actions model.examples model.loaded
