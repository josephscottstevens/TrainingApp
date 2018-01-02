module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { courses : List String
    , currentCourse : String
    }


init : ( Model, Cmd Msg )
init =
    ( emptyModel, Cmd.none )



-- UPDATE


type Msg
    = UpdateText String
    | AddCourse


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateText str ->
            ( { model | currentCourse = str }, Cmd.none )

        AddCourse ->
            ( { model | courses = model.currentCourse :: model.courses }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "text", onInput UpdateText ] []
        , button [ onClick AddCourse ] [ text "+" ]
        , div [] (List.map (\t -> div [] [ text t ]) model.courses)
        ]


emptyModel : Model
emptyModel =
    { courses = [ "A", "B", "C" ]
    , currentCourse = ""
    }
