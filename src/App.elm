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
import Html exposing (Html, a, div, h1, pre, span, text)
import Html.Attributes exposing (class, href, id)
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
        (h1 [ id ("/" ++ String.toLower name), class "module-name" ] [ text name ] :: List.map (renderSection name) content)


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
            let
                id =
                    "/" ++ m ++ "/" ++ a.name |> String.toLower
            in
            renderAlias id a |> wrapblock id a.comment

        AType t ->
            let
                id =
                    "/" ++ m ++ "/" ++ t.name |> String.toLower
            in
            renderType id t |> wrapblock id t.comment

        AValue v ->
            let
                id =
                    "/" ++ m ++ "/" ++ v.name |> String.toLower
            in
            renderValue id v |> wrapblock id v.comment


wrapblock : String -> String -> List (Html msg) -> Html msg
wrapblock id_ comment content =
    div [ class "docs-entry", id id_ ]
        [ div [ class "docs-annotation" ] content
        , Markdown.toHtml [] comment
        ]


renderAlias : String -> Alias -> List (Html msg)
renderAlias id_ { name, args, comment, tipe } =
    [ text "type alias "
    , a [ href ("#" ++ id_), class "export" ] [ text name ]
    , text " "
    , text (String.join " " args)
    , text " =\n"
    , text <| "    " ++ tipe
    ]


renderType : String -> Tipe -> List (Html msg)
renderType id_ { name, args, comment, cases } =
    let
        tipe =
            case cases of
                [] ->
                    ""

                _ ->
                    " = " ++ String.join " | " (List.map (\( x, xs ) -> x ++ " " ++ String.join " " xs) cases)

        arg =
            case args of
                [] ->
                    ""

                _ ->
                    " " ++ String.join " " args
    in
    [ text "type "
    , a [ href ("#" ++ id_), class "export" ] [ text name ]
    , text <| arg ++ tipe
    ]


renderValue : String -> Value -> List (Html msg)
renderValue id_ { name, comment, tipe } =
    [ a [ href ("#" ++ id_), class "export" ] [ text name ]
    , text <| " : " ++ tipe
    ]


main : Program Never Model Msg
main =
    Navigation.program (router >> NavigateTo)
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
