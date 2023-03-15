module Arrays exposing (..)

import Random
import Random.Extra
import Random.List
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Events
import Html.Attributes as Attr

type alias CostArray = List Int
type alias ScoreArray = List Int


-- MODEL

type alias Model =
  { scoreCosts : ScoreCosts
  , allScoreArrays : List ScoreArray
  , settingsShown : Bool
  }

init : Model
init =
  { scoreCosts = defaultScoreCosts
  , allScoreArrays = optimalScoreArrays defaultScoreCosts
  , settingsShown = False
  }


type alias ScoreCosts =
  { minimumScore : Int
  , maximumScore : Int
  , totalPoints : Int
  , costsByScore : Dict Int Int
  }

defaultScoreCosts : ScoreCosts
defaultScoreCosts =
  { minimumScore = 8
  , maximumScore = 15
  , totalPoints = 27
  , costsByScore = defaultCostsByScore
  }

defaultCostsByScore : Dict Int Int
defaultCostsByScore =
  Dict.fromList
    [ ( 6, -2 )
    , ( 7, -1 )
    , ( 8, 0 )
    , ( 9, 1 )
    , ( 10, 2 )
    , ( 11, 3 )
    , ( 12, 4 )
    , ( 13, 5 )
    , ( 14, 7 )
    , ( 15, 9 )
    , ( 16, 12 )
    , ( 17, 15 )
    , ( 18, 19 )
    ]

leastSupportedScore : number
leastSupportedScore = 6
greatestSupportedScore : number
greatestSupportedScore = 18


defaultTotalPoints : number
defaultTotalPoints = 27


optimalScoreArrays : ScoreCosts -> List ScoreArray
optimalScoreArrays { minimumScore, maximumScore, totalPoints, costsByScore } =
  let
    bestScoresAndCosts : List ( Int, Int )
    bestScoresAndCosts =
      Dict.toList costsByScore
        |> List.filter
            (\( score, _ ) ->
              score >= minimumScore && score <= maximumScore
            )
        |> List.sort
        -- Drop costs that are more expensive than the following cost
        |> List.foldr
              (\( score, cost ) greaterScores ->
                case greaterScores of
                  [] -> [ ( score, cost ) ]
                  ( _, nextCost ) :: _ ->
                    if cost >= nextCost
                    then greaterScores
                    else ( score, cost ) :: greaterScores
              )
              []

    bestScoresByCost : Dict Int Int
    bestScoresByCost =
      bestScoresAndCosts
        |> List.map (\( score, cost ) -> ( cost, score ))
        |> Dict.fromList

    lowestCost : Int
    lowestCost =
      List.head bestScoresAndCosts |> Maybe.withDefault ( 0, 0 ) |> Tuple.second

    adjustedCosts : List Int
    adjustedCosts =
      List.map (\( _, cost ) -> cost - lowestCost) bestScoresAndCosts

    arraysOfLengthCosting : Int -> Int -> Int -> List CostArray
    arraysOfLengthCosting length pointsRemaining maxCost =
      if pointsRemaining == 0
      then [ List.repeat length 0 ]
      else if length == 0 || pointsRemaining < 0
      then []
      else
        List.concatMap
          (\cost ->
            if
              cost > pointsRemaining
              || cost > maxCost
              || pointsRemaining > length * maxCost -- Drop suboptimal arrays
            then []
            else
              List.map ((::) cost)
                <| arraysOfLengthCosting (length - 1) (pointsRemaining - cost) cost)
          adjustedCosts
  in
    List.map
      (List.filterMap (\cost -> Dict.get (cost + lowestCost) bestScoresByCost))
      <| arraysOfLengthCosting 6 (totalPoints - 6 * lowestCost) (List.foldr max 0 adjustedCosts)


randomArray : Model -> Random.Generator ScoreArray
randomArray { scoreCosts, allScoreArrays } =
  Random.Extra.sample allScoreArrays
    |> Random.map (Maybe.withDefault <| List.repeat 6 scoreCosts.minimumScore)
    |> Random.andThen Random.List.shuffle

randomArrays : List ScoreArray -> Int -> Random.Generator (List ScoreArray)
randomArrays optimalArrays n =
  Random.List.choices n optimalArrays |> Random.map Tuple.first



-- UPDATE

type Msg
  = SetTotalPoints Int
  | SetMinimumScore Int
  | SetMaximumScore Int
  | SetScoreCost Int Int
  | ToggleSettingsShown


update : Model -> Msg -> Model
update ({ scoreCosts, settingsShown } as model) msg =
  case msg of
    ToggleSettingsShown ->
      { model | settingsShown = not settingsShown }
    _ ->
      let
        newScoreCosts =
          case msg of
            SetTotalPoints newTotalPoints ->
              { scoreCosts | totalPoints = max newTotalPoints 0 }
            SetMinimumScore newMinimumScore ->
              { scoreCosts
              | minimumScore =
                  clamp leastSupportedScore greatestSupportedScore newMinimumScore
              }
            SetMaximumScore newMaximumScore ->
              { scoreCosts
              | maximumScore =
                  clamp leastSupportedScore greatestSupportedScore newMaximumScore
              }
            SetScoreCost score newCost ->
              { scoreCosts
              | costsByScore = Dict.insert score newCost scoreCosts.costsByScore
              }
            _ -> scoreCosts
      in
        { scoreCosts = newScoreCosts
        , allScoreArrays = optimalScoreArrays newScoreCosts
        , settingsShown = True
        }



-- VIEW

view : Model -> Html Msg
view { scoreCosts, settingsShown } =
  Html.div [ Attr.class "settings-container" ]
    <|
      ( Html.button
          [ Attr.class "settings-toggle"
          , Html.Events.onClick ToggleSettingsShown
          ]
          [ Html.text "[Settings]" ]
      )
      ::
      if settingsShown
      then
        [ settings scoreCosts ]
      else
        []


settings : ScoreCosts -> Html Msg
settings scoreCosts =
  Html.div [ Attr.class "settings" ]
    [ Html.div [ Attr.class "points-settings" ]
        [ totalPointsSelector scoreCosts.totalPoints
        , limitSelector "Minimum Score:" SetMinimumScore scoreCosts.minimumScore
        , limitSelector "Maximum Score:" SetMaximumScore scoreCosts.maximumScore
        ]
    , scoreCostsSelector scoreCosts
    ]

totalPointsSelector : Int -> Html Msg
totalPointsSelector totalPoints =
  Html.div []
    [ Html.text "Total Points:"
    , Html.input
        [ Attr.value <| String.fromInt totalPoints
        , Attr.type_ "number"
        , Attr.min "0"
        , Html.Events.onInput
            <| String.toInt >> Maybe.withDefault 0 >> SetTotalPoints
        ]
        []
    ]

limitSelector : String -> ( Int -> Msg ) -> Int -> Html Msg
limitSelector label msg limit =
  Html.div []
    [ Html.text label
    , Html.input
        [ Attr.value <| String.fromInt limit
        , Attr.type_ "number"
        , Attr.min <| String.fromInt leastSupportedScore
        , Attr.max <| String.fromInt greatestSupportedScore
        , Html.Events.onInput
            <| String.toInt >> Maybe.withDefault 0 >> msg
        ]
        []
    ]

scoreCostsSelector : ScoreCosts -> Html Msg
scoreCostsSelector { minimumScore, maximumScore, costsByScore } =
  Html.table [ Attr.class "score-cost-selector" ]
    [ Html.thead []
        [ Html.tr []
            [ Html.th [] [ Html.text "Score"]
            , Html.th [] [ Html.text "Cost"]
            ]
        ]
    , let
        scores = List.range leastSupportedScore greatestSupportedScore
        costs = List.filterMap (\score -> Dict.get score costsByScore) scores

        rows =
          List.map2
            (\score cost ->
              Html.tr
                ( if minimumScore <= score && score <= maximumScore
                  then []
                  else [ Attr.class "inactive" ]
                )
                [ Html.td []
                    [ Html.text <| String.fromInt score ]
                , Html.td []
                    [ costSelector score cost ]
                ]
            )
            scores
            costs
      in
        Html.tbody [] rows
    ]


costSelector : Int -> Int -> Html Msg
costSelector score cost =
  Html.input
    [ Attr.value <| String.fromInt cost
    , Attr.type_ "number"
    , Html.Events.onInput
        <| String.toInt >> Maybe.withDefault 0 >> SetScoreCost score
    ]
    []
