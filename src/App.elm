module App exposing (main)

import Docs.Module as M
    exposing
        ( Alias
        , Block(..)
        , Module
        , Section(..)
        , Tipe
        , Value
        )
import Html exposing (Html, a, div, h1, pre, text)
import Html.Attributes exposing (class, href)
import Http
import Json.Decode exposing (Decoder)
import Markdown
import Navigation exposing (Location)
import UrlParser as P exposing (Parser)


docsUrl : String
docsUrl =
    "/docs.json"


type Route
    = Home


locationParser : Parser (Route -> a) a
locationParser =
    P.map Home P.top


router : Location -> Route
router location =
    P.parseHash locationParser location
        |> Maybe.withDefault Home


type Docs
    = Loading
    | Invalid String
    | Loaded (List Module)


type alias Model =
    { route : Route
    , docs : Docs
    }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        load : Cmd Msg
        load =
            Http.get docsUrl (Json.Decode.list M.decode)
                |> Http.send LoadedModules
    in
    { route = router location
    , docs = Loading
    }
        ! [ load ]


type Msg
    = NavigateTo Route
    | LoadedModules (Result Http.Error (List Module))


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        NavigateTo route ->
            { model | route = route } ! []

        LoadedModules (Err err) ->
            { model | docs = Invalid (toString err) } ! []

        LoadedModules (Ok m) ->
            { model | docs = Loaded m } ! []


view : Model -> Html msg
view model =
    case model.docs of
        Loading ->
            Html.text "loading..."

        Invalid err ->
            Html.text <| "failure: " ++ err

        Loaded modules ->
            Html.div [] (List.map renderModule modules)


renderModule : Module -> Html msg
renderModule { name, content } =
    div []
        (h1 [ class "module-name" ] [ text name ] :: List.map (renderSection name) content)


renderSection : String -> Section -> Html msg
renderSection m block =
    case block of
        Text text ->
            Markdown.toHtml [] text

        Docs docs ->
            div [] (List.map (renderBlock m) docs)


renderBlock : String -> Block -> Html msg
renderBlock m block =
    case block of
        AnAlias a ->
            renderAlias m a

        AType t ->
            renderType m t

        AValue v ->
            renderValue m v


renderAlias : String -> Alias -> Html msg
renderAlias m { name, args, comment, tipe } =
    [ text "type alias "
    , text name
    , text " "
    , text (String.join " " args)
    , text " =\n"
    , text <| "    " ++ tipe
    ]
        |> withComment comment


renderType : String -> Tipe -> Html msg
renderType m { name, args, comment, cases } =
    let
        tipe =
            case cases of
                [] ->
                    ""

                _ ->
                    " = " ++ String.join " | " cases

        arg =
            case args of
                [] ->
                    ""

                _ ->
                    " " ++ String.join " " args
    in
    [ text ("type " ++ name ++ arg ++ tipe) ]
        |> withComment comment


renderValue : String -> Value -> Html msg
renderValue m { name, comment, tipe } =
    [ text (name ++ " : " ++ tipe) ]
        |> withComment comment


withComment : String -> List (Html msg) -> Html msg
withComment comment html =
    div [ class "docs-entry" ]
        [ div [ class "docs-annotation" ] html
        , Markdown.toHtml [] comment
        ]


main : Program Never Model Msg
main =
    Navigation.program (router >> NavigateTo)
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
