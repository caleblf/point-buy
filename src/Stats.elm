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

fromList : List Int -> Maybe StatArray
fromList values =
  case values of
    str::dex::con::int::wis::cha::[] ->
      StatArray str dex con int wis cha |> Just
    _ ->
      Nothing

toList : StatArray -> List Int
toList { str, dex, con, int, wis, cha } =
  [ str, dex, con, int, wis, cha ]

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


setAbility : Ability -> Int -> StatArray -> StatArray
setAbility ability =
  case ability of
    Str -> setStrTo
    Dex -> setDexTo
    Con -> setConTo
    Int_ -> setIntTo
    Wis -> setWisTo
    Cha -> setChaTo

getAbility : Ability -> StatArray -> Int
getAbility ability =
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
  arr
    |> setAbility ability1 (getAbility ability2 arr)
    |> setAbility ability2 (getAbility ability1 arr)


rawModifiers : ModifierArray
rawModifiers =
  ModifierArray 0 0 0 0 0 0


modifier : Int -> Int
modifier score =
  score // 2 - 5

modifierString : Int -> String
modifierString mod =
  String.fromInt mod
    |> if mod > 0 then String.cons '+' else identity


applyModifiers : StatArray -> ModifierArray -> StatArray
applyModifiers arr1 arr2 =
  StatArray
    (arr1.str + arr2.str)
    (arr1.dex + arr2.dex)
    (arr1.con + arr2.con)
    (arr1.int + arr2.int)
    (arr1.wis + arr2.wis)
    (arr1.cha + arr2.cha)


setStrTo : Int -> StatArray -> StatArray
setStrTo score arr =
  { arr | str = score }

setDexTo : Int -> StatArray -> StatArray
setDexTo score arr =
  { arr | dex = score }

setConTo : Int -> StatArray -> StatArray
setConTo score arr =
  { arr | con = score }

setIntTo : Int -> StatArray -> StatArray
setIntTo score arr =
  { arr | int = score }

setWisTo : Int -> StatArray -> StatArray
setWisTo score arr =
  { arr | wis = score }

setChaTo : Int -> StatArray -> StatArray
setChaTo score arr =
  { arr | cha = score }

map : ( Int -> Int ) -> StatArray -> StatArray
map mapper arr =
  StatArray
    (mapper arr.str)
    (mapper arr.dex)
    (mapper arr.con)
    (mapper arr.int)
    (mapper arr.wis)
    (mapper arr.cha)
