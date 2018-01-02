module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Json.Decode as Decode
import Mouse exposing (Position)


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
    { position : Position
    , dragOffset : Maybe Position
    }


init : ( Model, Cmd Msg )
init =
    ( emptyModel, Cmd.none )



-- UPDATE


type Msg
    = DragStart Position
    | DragAt Position
    | DragEnd Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DragStart position ->
            { model
                | position = position
                , dragOffset =
                    Just
                        { x = position.x - model.position.x
                        , y = position.y - model.position.y
                        }
            }
                ! []

        DragAt position ->
            { model | position = position } ! []

        DragEnd position ->
            { model
                | position =
                    case model.dragOffset of
                        Just t ->
                            { x = model.position.x - t.x
                            , y = model.position.y - t.y
                            }

                        Nothing ->
                            position
                , dragOffset = Nothing
            }
                ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.dragOffset of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ onMouseDown, getStyle model ]
        [ text "Drag Me!"
        ]


px : Int -> String
px number =
    toString number ++ "px"


getStyle model =
    let
        ( x, y ) =
            getPos model
    in
        style
            [ ( "background-color", "#3C8D2F" )
            , ( "cursor", "move" )
            , ( "width", "100px" )
            , ( "height", "100px" )
            , ( "border-radius", "4px" )
            , ( "position", "absolute" )
            , ( "left", px x )
            , ( "top", px y )
            , ( "color", "white" )
            , ( "display", "flex" )
            , ( "align-items", "center" )
            , ( "justify-content", "center" )
            ]


getPos : Model -> ( Int, Int )
getPos model =
    case model.dragOffset of
        Nothing ->
            ( model.position.x, model.position.y )

        Just t ->
            ( model.position.x - t.x, model.position.y - t.y )


onMouseDown : Attribute Msg
onMouseDown =
    on "mousedown" (Decode.map DragStart Mouse.position)


emptyModel : Model
emptyModel =
    { position = Position 200 200
    , dragOffset = Nothing
    }
