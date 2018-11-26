module Main exposing
    ( Example
    , Flags
    , ImageData(..)
    , Model
    , Msg(..)
    , Output(..)
    , RenderState(..)
    , decodeExample
    , init
    , listExamples
    , loadExample
    , main
    , render
    , subscriptions
    , update
    , view
    )

import Browser
import Browser.Navigation as Navigation
import Html
import Html.Attributes as Attr
import Html.Events as Event
import Http
import Json.Decode as Json
import Url exposing (Url)


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = always Ignore
        , onUrlChange = always Ignore
        }


type alias Flags =
    ()


type Output
    = OutputPuzzle
    | OutputSolution
    | OutputBoth


type ImageData
    = SVG String
    | Error String
    | NoImage


type RenderState
    = Rendering
    | Queued
    | Ready


type alias Model =
    { puzzle : String
    , output : Output
    , downloadFormat : String
    , image : ImageData
    , renderState : RenderState
    , examples : List Example
    , url : Url
    }


type alias Example =
    { name : String
    , path : String
    }


decodeExample : Json.Decoder Example
decodeExample =
    Json.map2 Example
        (Json.field "name" Json.string)
        (Json.field "path" Json.string)


listExamples : Http.Request (List Example)
listExamples =
    Http.get "/puzzles/draw/examples" (Json.list decodeExample)


loadExample : String -> Http.Request String
loadExample path =
    Http.getString path


render : Output -> String -> Http.Request String
render output body =
    let
        out =
            case output of
                OutputPuzzle ->
                    "puzzle"

                OutputSolution ->
                    "solution"

                OutputBoth ->
                    "both"
    in
    Http.request
        { method = "POST"
        , headers = []
        , url = "/puzzles/draw/puzzle?output=" ++ out
        , body = Http.stringBody "application/x-yaml" body
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }


type Msg
    = PuzzleChange String
    | OutputChange String
    | FormatChange String
    | ExamplesChange String
    | RenderResult (Result Http.Error String)
    | ExamplesResult (Result Http.Error (List Example))
    | ExampleResult (Result Http.Error String)
    | Ignore


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url _ =
    ( { puzzle = ""
      , output = OutputPuzzle
      , downloadFormat = "png"
      , image = NoImage
      , renderState = Ready
      , examples = []
      , url = url
      }
    , Http.send ExamplesResult listExamples
    )


view : Model -> Browser.Document Msg
view model =
    { title = "puzzle-draw web"
    , body =
        [ Html.h3 [] [ Html.text "puzzle-draw web" ]
        , Html.div []
            [ Html.label [] [ Html.text "Load an example: " ]
            , Html.select [ Event.onInput ExamplesChange ] <|
                Html.option [] []
                    :: List.map
                        (\e -> Html.option [ Attr.value e.path ] [ Html.text e.name ])
                        model.examples
            ]
        , Html.div []
            [ Html.textarea
                [ Attr.cols 80
                , Attr.rows 20
                , Attr.id "puzzle"
                , Attr.value model.puzzle
                , Event.onInput PuzzleChange
                ]
                []
            ]
        , Html.div [] <|
            let
                radio output value =
                    [ Html.input
                        [ Attr.id <| "o" ++ value
                        , Attr.type_ "radio"
                        , Attr.name "output"
                        , Attr.value value
                        , Attr.checked (model.output == output)

                        --, Event.onInput OutputChange
                        , Event.on "change" (Json.map OutputChange <| Json.at [ "target", "value" ] Json.string)
                        ]
                        []
                    , Html.label
                        [ Attr.for <| "o" ++ value
                        ]
                        [ Html.text value ]
                    ]
            in
            List.concat
                [ [ Html.label [] [ Html.text "Output choice: " ] ]
                , radio OutputPuzzle "puzzle"
                , radio OutputSolution "solution"
                , radio OutputBoth "both"
                ]
        , Html.div [] <|
            [ Html.form
                [ Attr.method "post"
                , Attr.id "downloadForm"
                , Attr.action "/puzzles/draw/download"
                ]
              <|
                let
                    format f =
                        [ Html.input
                            [ Attr.id <| "o" ++ f
                            , Attr.type_ "radio"
                            , Attr.name "format"
                            , Attr.value f
                            , Attr.checked (model.downloadFormat == f)
                            , Event.on "change" (Json.map FormatChange <| Json.at [ "target", "value" ] Json.string)
                            ]
                            []
                        , Html.label
                            [ Attr.for <| "o" ++ f
                            ]
                            [ Html.text f ]
                        ]

                    out =
                        case model.output of
                            OutputPuzzle ->
                                "puzzle"

                            OutputSolution ->
                                "solution"

                            OutputBoth ->
                                "both"
                in
                List.concat
                    [ [ Html.input [ Attr.type_ "hidden", Attr.name "pzl", Attr.value model.puzzle ] []
                      , Html.input [ Attr.type_ "hidden", Attr.name "output", Attr.value out ] []
                      , Html.label [] [ Html.text "Download format: " ]
                      ]
                    , format "svg"
                    , format "png"
                    , format "pdf"
                    , [ Html.br [] []
                      , Html.label [] [ Html.text "Filename: " ]
                      , Html.input [ Attr.type_ "text", Attr.name "filename", Attr.placeholder "puzzle" ] []
                      , Html.br [] []
                      , Html.input [ Attr.type_ "submit", Attr.value "Download" ] []
                      ]
                    ]
            ]
        , Html.div [] <|
            let
                updating =
                    model.renderState == Rendering || model.renderState == Queued
            in
            [ Html.div []
                [ Html.text <|
                    "Preview"
                        ++ (if updating then
                                " (updating...)"

                            else
                                ""
                           )
                ]
            , case model.image of
                SVG svg ->
                    Html.img [ Attr.id "drawing", Attr.src <| "data:image/svg+xml," ++ svg ] []

                Error err ->
                    Html.span [ Attr.id "error" ] [ Html.text err ]

                NoImage ->
                    Html.text ""
            ]
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        rerender m =
            case m.renderState of
                Ready ->
                    ( { m | renderState = Rendering }, Http.send RenderResult (render m.output m.puzzle) )

                Rendering ->
                    ( { m | renderState = Queued }, Cmd.none )

                Queued ->
                    ( m, Cmd.none )
    in
    case msg of
        Ignore ->
            ( model, Cmd.none )

        PuzzleChange puzzle ->
            rerender { model | puzzle = puzzle }

        OutputChange outstr ->
            let
                output =
                    case outstr of
                        "solution" ->
                            OutputSolution

                        "both" ->
                            OutputBoth

                        _ ->
                            OutputPuzzle
            in
            rerender { model | output = output }

        FormatChange fmt ->
            ( { model | downloadFormat = fmt }, Cmd.none )

        ExamplesChange path ->
            ( model, Http.send ExampleResult (loadExample path) )

        RenderResult res ->
            let
                m2 =
                    case res of
                        Err error ->
                            let
                                err =
                                    case error of
                                        Http.BadStatus resp ->
                                            resp.body

                                        Http.Timeout ->
                                            "timeout"

                                        Http.NetworkError ->
                                            "network error"

                                        _ ->
                                            Debug.toString error
                            in
                            { model | image = Error err }

                        Ok svg ->
                            { model | image = SVG svg }
            in
            case model.renderState of
                Ready ->
                    ( m2, Cmd.none )

                Rendering ->
                    ( { m2 | renderState = Ready }, Cmd.none )

                Queued ->
                    rerender { m2 | renderState = Ready }

        ExamplesResult res ->
            case res of
                Err error ->
                    ( model, Cmd.none )

                Ok examples ->
                    ( { model | examples = examples }, Cmd.none )

        ExampleResult res ->
            case res of
                Err error ->
                    ( model, Cmd.none )

                Ok example ->
                    rerender { model | puzzle = example }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
