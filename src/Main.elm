module Main exposing (..)

import Browser
import Dict
import Html exposing (Html)
import Html.Events
import Html.Attributes as Attr
import Random

import Stats exposing (StatArray, ModifierArray, Ability)
import Arrays exposing (ScoreArray)


-- MAIN


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = always Sub.none
    , view = view
    }



-- MODEL


type alias Model =
  { arraysModel : Arrays.Model
  , stats : StatArray
  , configuringStat : Maybe ( Ability, String )
  , firstSwapSelection : Ability
  , secondSwapSelection : Ability
  , modifiers : ModifierArray
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( { arraysModel = Arrays.init
    , stats = StatArray 8 8 8 8 8 8
    , configuringStat = Nothing
    , firstSwapSelection = Stats.Str
    , secondSwapSelection = Stats.Str
    , modifiers = ModifierArray 0 0 0 0 0 0
    }
  , Cmd.none
  )



-- UPDATE


type Msg
  = ArraysMsg Arrays.Msg
  | SetScore Ability Int
  | SetModifier Ability Int
  | SetFirstSwapSelection Ability
  | SetSecondSwapSelection Ability
  | RandomizeStats
  | RandomizedScores ScoreArray
  | Configuring Ability String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ArraysMsg arraysMsg ->
      let
        newArraysModel = Arrays.update model.arraysModel arraysMsg
      in
        ( { model
          | arraysModel = newArraysModel
          , stats =
              Stats.map
                ( clamp
                  newArraysModel.scoreCosts.minimumScore
                  newArraysModel.scoreCosts.maximumScore
                )
                model.stats
          , configuringStat = Nothing
          }
        , Cmd.none
        )
    SetScore ability newScore ->
      ( { model
        | stats =
            Stats.setAbility
              ability
              ( clamp
                model.arraysModel.scoreCosts.minimumScore
                model.arraysModel.scoreCosts.maximumScore 
                newScore
              )
              model.stats
        , configuringStat = Nothing
        }
      , Cmd.none
      )
    SetModifier ability newModifier ->
      ( { model
        | modifiers =
            Stats.setAbility
              ability
              newModifier
              model.modifiers
        , configuringStat = Nothing
        }
      , Cmd.none
      )
    SetFirstSwapSelection newFirstSwapSelection ->
      ( { model
        | firstSwapSelection = newFirstSwapSelection
        , configuringStat = Nothing
        }
      , Cmd.none
      )
    SetSecondSwapSelection newSecondSwapSelection ->
      ( { model
        | secondSwapSelection = newSecondSwapSelection
        , configuringStat = Nothing
        }
      , Cmd.none
      )
    RandomizeStats ->
      ( { model
        | configuringStat = Nothing
        }
      , Random.generate RandomizedScores
          <| Arrays.randomArray model.arraysModel
      )
    RandomizedScores scoreArray ->
      ( { model
        | stats = Stats.fromList scoreArray |> Maybe.withDefault model.stats
        , configuringStat = Nothing
        }
      , Cmd.none
      )
    Configuring ability text ->
      ( { model
        | configuringStat = Just ( ability, text )
        }
      , Cmd.none )



-- VIEW

view : Model -> Html Msg
view model =
  Html.div [ Attr.class "content-container" ]
    [ Html.div []
        [ viewStatus model
        , viewStatTable model
        ]
    , Arrays.view model.arraysModel |> Html.map ArraysMsg
    ]


viewStatus : Model -> Html Msg
viewStatus model =
  Html.div
    [ Attr.class "status-bar" ]
    [ Html.span []
        [ model.stats
            |> Stats.toList
            |> List.map
                (\score ->
                  Dict.get score model.arraysModel.scoreCosts.costsByScore
                    |> Maybe.withDefault 0
                )
            |> List.sum
            |> (-) model.arraysModel.scoreCosts.totalPoints
            |> String.fromInt
            |> (++) "Points Left: "
            |> Html.text
        ]
    , Html.button
        ( if List.isEmpty model.arraysModel.allScoreArrays
          then
            [ Attr.disabled True
            , Attr.title "Impossible to spend all points; change costs or total to use randomizer"
            ]
          else [ Html.Events.onClick RandomizeStats ]
        )
        [ Html.text "Randomize All" ]
    ]


viewStatTable : Model -> Html Msg
viewStatTable { arraysModel, stats, configuringStat, firstSwapSelection, secondSwapSelection, modifiers } =
  Html.table [ Attr.class "stat-report" ]
    [ Html.thead []
        [ Html.tr []
            [ Html.th [ Attr.colspan 2, Attr.class "colgroup-end" ] [ Html.text "Raw Scores" ]
            , Html.th [ Attr.colspan 2, Attr.class "colgroup-start colgroup-end" ]
                [ Html.text "Swap"
                , Html.br [] []
                , Html.text "Two"
                ]
            , Html.th [ Attr.class "colgroup-start colgroup-end" ] [ Html.text "Bonuses" ]
            , Html.th [ Attr.colspan 3, Attr.class "colgroup-start" ] [ Html.text "Final Scores" ]
            ]
        ]
    , Html.tbody []
        <| List.map5
            ( statTableRow
              ( scoreSelector
                arraysModel.scoreCosts.minimumScore
                arraysModel.scoreCosts.maximumScore
                configuringStat
              )
              firstSwapSelection
              secondSwapSelection
            )
            Stats.abilities
            Stats.labels
            (Stats.toList stats)
            (Stats.toList modifiers)
            (Stats.toList
              <| Stats.applyModifiers
                (Stats.swapStatsIn firstSwapSelection secondSwapSelection stats)
                modifiers)
    ]


statTableRow :
  ( String -> Ability -> Int -> Html Msg )
  -> Ability -> Ability -> Ability -> String -> Int -> Int -> Int -> Html Msg
statTableRow
  scoreSelectorBuilder
  firstSwappedAbility
  secondSwappedAbility
  ability
  label
  rawScore
  modifier
  finalScore =
  Html.tr []
    <| List.map2 (\class -> Html.td [ class ])
      columnClasses
      [ [ Html.label [ Attr.for label ] [ Html.text label ] ]
      , [ scoreSelectorBuilder label ability rawScore ]
      , [ Html.input
            [ Attr.type_ "radio"
            , Attr.name "swap-first"
            , Html.Events.onClick <| SetFirstSwapSelection ability
            , Attr.checked <| Stats.abilitiesEqual ability firstSwappedAbility
            ]
            []
        ]
      , [ Html.input
            [ Attr.type_ "radio"
            , Attr.name "swap-second"
            , Html.Events.onClick <| SetSecondSwapSelection ability
            , Attr.checked <| Stats.abilitiesEqual ability secondSwappedAbility
            ]
            []
        ]
      , [ Html.text "+"
        , Html.input
            ( ( if modifier > 0
                then String.fromInt modifier |> Attr.value |> (::)
                else identity
              )
              [ Attr.type_ "number"
              , Attr.min "0"
              , Attr.max "2"
              , Html.Events.onInput
                <| String.toInt >> Maybe.withDefault 0 >> SetModifier ability
              , Attr.placeholder "0"
              ]
            )
            []
        ]
      , [ Html.text label ]
      , [ Html.text <| String.fromInt <| finalScore ]
      , [ Html.text <| "(" ++ (Stats.modifierString <| Stats.modifier finalScore) ++ ")" ]
      ]

scoreSelector : Int -> Int -> Maybe ( Ability, String ) -> String -> Ability -> Int -> Html Msg
scoreSelector minimumScore maximumScore configuringStat id ability score =
  Html.input
    ( [ Attr.value
          <|
            if configuringStat == Just ( ability, "" )
            then ""
            else if configuringStat == Just ( ability, "1" )
            then "1"
            else String.fromInt score
      , Attr.type_ "number"
      , Attr.min <| String.fromInt minimumScore
      , Attr.max <| String.fromInt maximumScore
      , Html.Events.onInput
          (\value ->
            if value == "1" || value == ""
            then Configuring ability value -- assume user is starting to type a longer number
            else
              value
                |> String.toInt
                |> Maybe.withDefault minimumScore
                |> SetScore ability
          )
      , Attr.id id
      ]
    )
    []

columnClasses : List (Html.Attribute msg)
columnClasses =
  List.map Attr.class
    [ ""
    , "colgroup-end"
    , "colgroup-start swap-col"
    , "colgroup-end swap-col"
    , "colgroup-start colgroup-end"
    , "colgroup-start"
    , ""
    , ""
    ]
