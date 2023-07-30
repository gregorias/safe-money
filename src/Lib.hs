module Lib (
  someFunc,
) where

import Data.Currency
import Data.Money
import Relude
import Witch.Utility (unsafeFrom)

chf50 :: Money CHF Int
chf50 = Money CHFWitness 50

chf100 :: Money CHF Int
chf100 = Money CHFWitness 100

pln50 :: Money PLN Int
pln50 = Money PLNWitness 50

pln100 :: Money PLN Int
pln100 = Money PLNWitness 100

someFunc :: IO ()
someFunc = do
  print chf50
  print $ add chf50 chf100
  print $ add pln50 pln100
  print $ multiply chf50 (2137 :: Int)
  print $ divide (unsafeFrom chf50) (2137 :: Float)
  putStrLn "someFunc"
