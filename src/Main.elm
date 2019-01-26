import Browser
import Html exposing (Html)
import Html.Events
import Html.Attributes
import Random
import Random.List

import Stats exposing (StatArray)
import Spreads exposing (PointSpread)


-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL


type Model
  = DisplayStats StatArray
  | PickSpread (List PointSpread)


init : () -> (Model, Cmd Msg)
init _ =
  ( PickSpread []
  , Random.generate ChosenOptions <| Spreads.randomSpreads 3
  )



-- UPDATE


type Msg
  = ChosenOptions (List PointSpread)
  | Scrambled PointSpread
  | Choose PointSpread
  | Restart


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Restart ->
      ( model
      , Random.generate ChosenOptions <| Spreads.randomSpreads 3
      )
    ChosenOptions spreads ->
      ( PickSpread spreads
      , Cmd.none
      )
    Choose spread ->
      ( model
      , Random.generate Scrambled <| Random.List.shuffle spread
      )
    Scrambled (str::dex::con::int::wis::cha::[]) ->
      ( DisplayStats <| StatArray str dex con int wis cha
      , Cmd.none
      )
    _ -> (model, Cmd.none) -- should not happen



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  case model of
    PickSpread spreads ->
      Html.div []
        [ Html.h1 [] [ Html.text "Choose one:" ]
        , Html.div [] <| List.map spreadOption spreads
        ]
    DisplayStats arr ->
      Html.div []
        [ Html.h1 [] [ Html.text "Final ability scores:" ]
        , viewStatArray arr
        , Html.button [ Html.Events.onClick Restart ] [ Html.text "Reselect Scores" ]
        ]

spreadOption : PointSpread -> Html Msg
spreadOption spread =
  Html.div
    [ Html.Attributes.class "point-spread"
    , Html.Events.onClick <| Choose spread
    ]
    <| List.map
      (\n -> Html.div [ Html.Attributes.class "score" ] [ Html.text <| String.fromInt n])
      spread

viewStatArray : StatArray -> Html Msg
viewStatArray arr =
  Html.div [ Html.Attributes.class "stat-report" ]
    <| List.map
      (\(stat, score) ->
        Html.div [ Html.Attributes.class "labeled-score" ]
          [ Html.div [ Html.Attributes.class "stat-label" ] [ Html.text stat ]
          , Html.div [ Html.Attributes.class "score" ] [ Html.text <| String.fromInt score ]
          ])
      <| Stats.report arr
