module Stats exposing (..)


type alias StatArray =
  { str : Int
  , dex : Int
  , con : Int
  , int : Int
  , wis : Int
  , cha : Int
  }

type alias ModifierArray =
  { str : Int
  , dex : Int
  , con : Int
  , int : Int
  , wis : Int
  , cha : Int
  }

type Ability
  = Str
  | Dex
  | Con
  | Int_
  | Wis
  | Cha


abilities : List Ability
abilities =
  [ Str
  , Dex
  , Con
  , Int_
  , Wis
  , Cha
  ]


abilitiesEqual : Ability -> Ability -> Bool
abilitiesEqual ability1 ability2 =
  case (ability1, ability2) of
    (Str, Str) -> True
    (Dex, Dex) -> True
    (Con, Con) -> True
    (Int_, Int_) -> True
    (Wis, Wis) -> True
    (Cha, Cha) -> True
    _ -> False


setterFor : Ability -> Int -> StatArray -> StatArray
setterFor ability =
  case ability of
    Str -> changeStrTo
    Dex -> changeDexTo
    Con -> changeConTo
    Int_ -> changeIntTo
    Wis -> changeWisTo
    Cha -> changeChaTo

getterFor : Ability -> StatArray -> Int
getterFor ability =
  case ability of
    Str -> .str
    Dex -> .dex
    Con -> .con
    Int_ -> .int
    Wis -> .wis
    Cha -> .cha


labels : List String
labels =
  [ "STR"
  , "DEX"
  , "CON"
  , "INT"
  , "WIS"
  , "CHA"
  ]


toList : StatArray -> List Int
toList { str, dex, con, int, wis, cha } =
  [ str
  , dex
  , con
  , int
  , wis
  , cha
  ]


zip : StatArray -> StatArray -> List (Int, Int)
zip arr1 arr2 =
  [ (arr1.str, arr2.str)
  , (arr1.dex, arr2.dex)
  , (arr1.con, arr2.con)
  , (arr1.int, arr2.int)
  , (arr1.wis, arr2.wis)
  , (arr1.cha, arr2.cha)
  ]


swapStatsIn : Ability -> Ability -> StatArray -> StatArray
swapStatsIn ability1 ability2 arr =
  setterFor ability1 (getterFor ability2 arr)
    <| setterFor ability2 (getterFor ability1 arr)
    <| arr


rawModifiers : ModifierArray
rawModifiers =
  ModifierArray 0 0 0 0 0 0


modifier : Int -> Int
modifier score =
  score // 2 - 5

modifierString : Int -> String
modifierString mod =
  String.fromInt mod
  |> if mod > 0 then String.cons '+'
     else identity


applyModifiers : StatArray -> ModifierArray -> StatArray
applyModifiers arr1 arr2 =
  StatArray
    (arr1.str + arr2.str)
    (arr1.dex + arr2.dex)
    (arr1.con + arr2.con)
    (arr1.int + arr2.int)
    (arr1.wis + arr2.wis)
    (arr1.cha + arr2.cha)


changeStrTo : Int -> StatArray -> StatArray
changeStrTo score arr =
  { arr | str = score }

changeDexTo : Int -> StatArray -> StatArray
changeDexTo score arr =
  { arr | dex = score }

changeConTo : Int -> StatArray -> StatArray
changeConTo score arr =
  { arr | con = score }

changeIntTo : Int -> StatArray -> StatArray
changeIntTo score arr =
  { arr | int = score }

changeWisTo : Int -> StatArray -> StatArray
changeWisTo score arr =
  { arr | wis = score }

changeChaTo : Int -> StatArray -> StatArray
changeChaTo score arr =
  { arr | cha = score }

