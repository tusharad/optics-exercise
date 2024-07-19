{-# LANGUAGE TemplateHaskell #-}
module Optics.Chap3 where

import Control.Lens

data Ship = Ship {
    _numCrew :: Int
  , _name    :: String
} deriving (Show,Eq)

data Inventory = Inventory {
    _book :: String
  , _price :: Int
} deriving (Show,Eq)

{-
Below code throws error because Lens also removes type name along with _.
data Pet = Pet { 
  _petName :: String
 , _someType :: String
}

makeLenses ''Pet

getPetName :: Pet -> String
getPetName pet = view petName pet
-}

getShipName :: Ship -> String
getShipName = _name

setShipName :: Ship -> String -> Ship
setShipName s n = s { _name = n }

name :: Lens' Ship String
name = lens getShipName setShipName

makeLenses ''Inventory

main0 :: IO ()
main0 = do
    let x = Ship 3 "Titanic"
    print $ view name x
    print $ law0 x
    print $ law1 x
    print $ law2 x

-- Laws of lens
law0 :: Ship -> Bool
law0 s = view name (set name "titanic" s) == "titanic"

law1 :: Ship -> Bool
law1 s = set name (view name s) s == s

law2 :: Ship -> Bool
law2 s = set name "titanic" (set name "old titanic" s) == set name "titanic" s
