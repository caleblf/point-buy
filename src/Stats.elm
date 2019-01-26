module Stats exposing (..)


type alias StatArray =
  { str : Int
  , dex : Int
  , con : Int
  , int : Int
  , wis : Int
  , cha : Int
  }

report : StatArray -> List (String, Int)
report {str, dex, con, int, wis, cha} =
  [ ("STR", str)
  , ("DEX", dex)
  , ("CON", con)
  , ("INT", int)
  , ("WIS", wis)
  , ("CHA", cha)
  ]
