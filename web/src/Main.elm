module Main exposing
    ( Example
    , Flags
    , ImageData
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
        , onUrlRequest = ClickedLink
        , onUrlChange = always Ignore
        }


type alias Flags =
    ()


type Output
    = OutputPuzzle
    | OutputSolution
    | OutputBoth


type alias ImageData =
    { svg : Maybe String
    , error : Maybe String
    }


type RenderState
    = Rendering
    | Queued
    | Ready


type alias Model =
    { puzzle : String
    , output : Output
    , device : String
    , scale : Float
    , code : Bool
    , preview : Bool
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
    Http.get "./api/examples" (Json.list decodeExample)


loadExample : String -> Http.Request String
loadExample path =
    Http.getString path


render : Output -> String -> Float -> Bool -> String -> Http.Request String
render output device scale code body =
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
        , url =
            "./api/preview?output="
                ++ out
                ++ "&device=" ++ device
                ++ "&code="
                ++ (if code then
                        "yes"

                    else
                        "no"
                   )
                ++ "&scale="
                ++ String.fromFloat scale
        , body = Http.stringBody "application/x-yaml" body
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }


type Msg
    = PuzzleChange String
    | OutputChange String
    | DeviceChange String
    | ScaleChange Float
    | CodeChange Bool
    | PreviewChange Bool
    | FormatChange String
    | ExamplesChange String
    | RenderResult (Result Http.Error String)
    | ExamplesResult (Result Http.Error (List Example))
    | ExampleResult (Result Http.Error String)
    | ClickedLink Browser.UrlRequest
    | Ignore


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url _ =
    ( { puzzle = ""
      , output = OutputPuzzle
      , device = "auto"
      , scale = 1.0
      , code = False
      , preview = True
      , downloadFormat = "png"
      , image = { svg = Nothing, error = Nothing }
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
        , Html.p []
            [ Html.text "This is a web interface to "
            , Html.a [ Attr.href "https://github.com/robx/puzzle-draw" ] [ Html.text "puzzle-draw" ]
            , Html.text ", a tool for formatting puzzle graphics from text descriptions."
            ]
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
                radio name msg mod val vals =
                    [ Html.input
                        [ Attr.id <| "o" ++ vals
                        , Attr.type_ "radio"
                        , Attr.name name
                        , Attr.value vals
                        , Attr.checked (mod == val)

                        , Event.on "change" (Json.map msg <| Json.at [ "target", "value" ] Json.string)
                        ]
                        []
                    , Html.label
                        [ Attr.for <| "o" ++ vals
                        ]
                        [ Html.text vals ]
                    ]
                radioout = radio "output" OutputChange model.output
                radiodev = radio "device" DeviceChange model.device
            in
            List.concat
                [ [ Html.label [] [ Html.text "Output choice: " ] ]
                , radioout OutputPuzzle "puzzle"
                , radioout OutputSolution "solution"
                , radioout OutputBoth "both"
                , [ Html.br [] []
                  , Html.label [] [ Html.text "Scale: " ]
                  , Html.input
                        [ Attr.type_ "range"
                        , Attr.min "0"
                        , Attr.step "0.25"
                        , Attr.max "4"
                        , Attr.value <| String.fromFloat model.scale
                        , Event.on "change" (Json.map ScaleChange <| Json.at [ "target", "valueAsNumber" ] Json.float)
                        ]
                        []
                  , Html.label [] [ Html.text <| " " ++ String.fromFloat model.scale ]
                  , Html.br [] []
                  , Html.label [] [ Html.text "Code markers: " ]
                  , Html.input
                        [ Attr.type_ "checkbox"
                        , Attr.checked model.code
                        , Event.onCheck CodeChange
                        ]
                        []
                  , Html.br [] []
                  , Html.label [] [ Html.text "Device: " ]
                  ]
                , radiodev "auto" "auto"
                , radiodev "screen" "screen"
                , radiodev "print" "print"
                ]
        , Html.div [] <|
            [ Html.form
                [ Attr.method "post"
                , Attr.id "downloadForm"
                , Attr.action "./api/download"
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
                      , Html.input [ Attr.type_ "hidden", Attr.name "device", Attr.value model.device ] []
                      , Html.input
                            [ Attr.type_ "hidden"
                            , Attr.name "code"
                            , Attr.value
                                (if model.code then
                                    "yes"

                                 else
                                    "no"
                                )
                            ]
                            []
                      , Html.input [ Attr.type_ "hidden", Attr.name "scale", Attr.value <| String.fromFloat model.scale ] []
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
                errored = model.image.error /= Nothing
                classes = List.map Attr.class <| List.concat
                    [ if updating then ["updating"] else []
                    , if errored then ["errored"] else []
                    ]
            in
            List.concat
                [ [ Html.div []
                    [ Html.input
                        [ Attr.type_ "checkbox"
                        , Attr.checked model.preview
                        , Event.onCheck PreviewChange
                        ]
                        []
                    , Html.text <|
                            "Live preview"
                                ++ (if updating then
                                        " (updating...)"

                                    else
                                        ""
                                   )
                        ]
                  ]
                , case model.image.error of
                    Just err ->
                        [ Html.div [ Attr.id "error" ] [ Html.text err ] ]

                    Nothing ->
                        []
                , case model.image.svg of
                    Just svg ->
                        [ Html.img ([ Attr.id "drawing", Attr.src <| "data:image/svg+xml," ++ svg ] ++ classes) [] ]

                    Nothing ->
                        []
                ]
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        rerender m =
            if m.preview then
              case m.renderState of
                Ready ->
                    ( { m | renderState = Rendering }, Http.send RenderResult (render m.output m.device m.scale m.code m.puzzle) )

                Rendering ->
                    ( { m | renderState = Queued }, Cmd.none )

                Queued ->
                    ( m, Cmd.none )
           else
              ( m, Cmd.none )
    in
    case msg of
        Ignore ->
            ( model, Cmd.none )

        ClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Cmd.none )
                Browser.External url ->
                    ( model, Navigation.load url )

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

        DeviceChange device ->
            rerender { model | device = device }

        ScaleChange scale ->
            rerender { model | scale = scale }

        CodeChange code ->
            rerender { model | code = code }

        FormatChange fmt ->
            ( { model | downloadFormat = fmt }, Cmd.none )

        PreviewChange preview ->
            rerender { model | preview = preview }

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
                                oldImage = model.image
                                newImage = { oldImage | error = Just err }
                            in
                            { model | image = newImage }

                        Ok svg ->
                            { model | image = { svg = Just svg, error = Nothing } }
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
