import Browser
import Html exposing (Html)
import Html.Events
import Html.Attributes as Attr
import Random
import Random.List

import Stats exposing (StatArray, ModifierArray, Ability)
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
  = DisplayStats StatArray Ability Ability ModifierArray
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
  | ChangeFirstSwapSelectionTo Ability
  | ChangeSecondSwapSelectionTo Ability
  | ChangeModifier Ability Int


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
      ( DisplayStats
          (StatArray str dex con int wis cha)
          Stats.Str Stats.Str Stats.rawModifiers
      , Cmd.none
      )
    _ ->
      ( case model of
          DisplayStats arr firstSwappedAbility secondSwappedAbility modifiers ->
            case msg of
              ChangeFirstSwapSelectionTo newFirstSwapSelection ->
                DisplayStats arr newFirstSwapSelection secondSwappedAbility modifiers
              ChangeSecondSwapSelectionTo newSecondSwapSelection ->
                DisplayStats arr firstSwappedAbility newSecondSwapSelection modifiers
              ChangeModifier ability newModifier ->
                DisplayStats arr firstSwappedAbility secondSwappedAbility
                  <| Stats.setterFor ability newModifier modifiers
              _ -> model -- Should never happen
          _ -> model -- Should never happen
      , Cmd.none
      )



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
    DisplayStats arr firstSwappedAbility secondSwappedAbility modifiers ->
      Html.div []
        [ Html.h1 [] [ Html.text "Ability scores:" ]
        , viewStatTable arr firstSwappedAbility secondSwappedAbility modifiers
        , Html.button [ Html.Events.onClick Restart ] [ Html.text "Reselect Scores" ]
        ]


spreadOption : PointSpread -> Html Msg
spreadOption spread =
  Html.div
    [ Attr.class "point-spread"
    , Html.Events.onClick <| Choose spread
    ]
    <| List.map
      (\n -> Html.div [ Attr.class "score" ] [ Html.text <| String.fromInt n])
      spread


viewStatTable : StatArray -> Ability -> Ability -> ModifierArray -> Html Msg
viewStatTable arr firstSwappedAbility secondSwappedAbility modifiers =
  Html.table [ Attr.class "stat-report" ]
    [ Html.thead []
        [ Html.tr []
            [ Html.th [ Attr.colspan 2 ] [ columnLabel "Raw Scores" ]
            , Html.th [ Attr.class "swap-col" ] [ columnLabel "Swap Two" ]
            , Html.th [ Attr.class "manual-mod-col" ] [ columnLabel "Racial Modifiers" ]
            , Html.th [ Attr.colspan 3 ] [ columnLabel "Final Scores" ]
            ]
        ]
    , Html.tbody []
        <| List.map5 (statTableRow firstSwappedAbility secondSwappedAbility)
          Stats.abilities
          Stats.labels
          (Stats.toList arr)
          (Stats.toList modifiers)
          (Stats.toList
            <| Stats.applyModifiers
              (Stats.swapStatsIn firstSwappedAbility secondSwappedAbility arr)
              modifiers)
    ]


statTableRow : Ability -> Ability -> Ability -> String -> Int -> Int -> Int -> Html Msg
statTableRow firstSwappedAbility secondSwappedAbility ability label rawScore racialModifier finalScore =
  Html.tr []
    <| List.map2 (\class -> Html.td [class])
      columnClasses
      [ [ Html.text label ]
      , [ Html.text <| String.fromInt rawScore ]
      , [ Html.input
            [ Attr.type_ "radio"
            , Attr.name "swap-first"
            , Html.Events.onClick <| ChangeFirstSwapSelectionTo ability
            , Attr.checked <| Stats.abilitiesEqual ability firstSwappedAbility
            ]
            []
        , Html.input
            [ Attr.type_ "radio"
            , Attr.name "swap-second"
            , Html.Events.onClick <| ChangeSecondSwapSelectionTo ability
            , Attr.checked <| Stats.abilitiesEqual ability secondSwappedAbility
            ]
            []
        ]
      , [ Html.text "+"
        , Html.input
            ((if racialModifier > 0
              then (::) <| Attr.value <| String.fromInt racialModifier
              else identity)
              [ Attr.type_ "number"
              , Attr.min "0"
              , Attr.max "2"
              , Html.Events.onInput
                <| String.toInt >> Maybe.withDefault 0 >> ChangeModifier ability
              , Attr.placeholder "0"
              ])
            []
        ]
      , [ Html.text label ]
      , [ Html.text <| String.fromInt <| finalScore ]
      , [ Html.text <| "(" ++ (Stats.modifierString <| Stats.modifier finalScore) ++ ")" ]
      ]


columnLabel : String -> Html msg
columnLabel label =
  Html.div [ Attr.class "column-label" ] [ Html.text label ]


columnClasses : List (Html.Attribute msg)
columnClasses =
  List.map Attr.class
    [ "ability-col"
    , "score-col"
    , "swap-col"
    , "manual-mod-col"
    , "ability-col"
    , "score-col"
    , "score-col"
    ]
