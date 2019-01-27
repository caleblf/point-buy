module Spreads exposing (..)

import Random
import Random.List


type alias CostSpread = List Int
type alias PointSpread = List Int


defaultTotalPoints = 27


optimalPointSpreads : List PointSpread
optimalPointSpreads =
  List.map
    (List.map spend)
    <| spreadsOfLengthCosting 6 defaultTotalPoints (List.foldr max 0 validCosts)


validCosts : List Int
validCosts =
  [ 0
  , 1
  , 2
  , 3
  , 4
  , 5
  , 7
  , 9
  ]


spend : Int -> Int
spend points =
  case points of
    0 -> 8
    1 -> 9
    2 -> 10
    3 -> 11
    4 -> 12
    5 -> 13
    7 -> 14
    9 -> 15
    _ -> -1 -- invalid cost


spreadsOfLengthCosting : Int -> Int -> Int -> List CostSpread
spreadsOfLengthCosting length totalPoints maxCost =
  case (length, totalPoints) of
    (_, 0) -> [List.repeat length 0]
    (0, _) -> []
    _ ->
      List.concatMap
        (\cost ->
          if cost > totalPoints || cost > maxCost || totalPoints > length * maxCost then []
          else
            List.map ((::) cost)
              <| spreadsOfLengthCosting (length - 1) (totalPoints - cost) cost)
        validCosts


randomSpreads : Int -> Random.Generator (List PointSpread)
randomSpreads n =
  Random.map
    (List.take n >> List.sortBy spreadSortKey)
    <| Random.List.shuffle optimalPointSpreads


spreadSortKey : PointSpread -> String
spreadSortKey =
  List.map ((-) 20 >> String.fromInt >> String.padLeft 2 '0') >> String.concat

