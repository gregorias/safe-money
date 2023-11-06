-- | A prototype of a currency type.
{-# LANGUAGE RankNTypes #-}
module Data.Currency (
  -- * Types with their singletons.
  Currency (..),
  CurrencyWitness (..),
  SomeCurrencyWitness (..),

  -- * Conversions
  currencyToWitness,
  witnessToCurrency,
  withTypeableCurrency,
  currencyP,
) where

import Relude
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char (letterChar)

data Currency = CHF | EUR | PLN | USD
  deriving stock (Bounded, Enum, Eq, Read, Show, Typeable)

deriving stock instance Typeable CHF
deriving stock instance Typeable EUR
deriving stock instance Typeable PLN
deriving stock instance Typeable USD

data CurrencyWitness (c :: Currency) where
  CHFWitness :: CurrencyWitness CHF
  EURWitness :: CurrencyWitness EUR
  PLNWitness :: CurrencyWitness PLN
  USDWitness :: CurrencyWitness USD

deriving stock instance Eq (CurrencyWitness c)

deriving stock instance Show (CurrencyWitness c)

data SomeCurrencyWitness where
  SomeCurrencyWitness :: CurrencyWitness c -> SomeCurrencyWitness

-- Unfortunately, can't do
-- currencyToWitness :: (c :: Currency) -> CurrencyWitness c
-- because all parameters must be of kind '*' in ghc.
currencyToWitness :: Currency -> SomeCurrencyWitness
currencyToWitness CHF = SomeCurrencyWitness CHFWitness
currencyToWitness EUR = SomeCurrencyWitness EURWitness
currencyToWitness PLN = SomeCurrencyWitness PLNWitness
currencyToWitness USD = SomeCurrencyWitness USDWitness

witnessToCurrency :: CurrencyWitness c -> Currency
witnessToCurrency CHFWitness = CHF
witnessToCurrency EURWitness = EUR
witnessToCurrency PLNWitness = PLN
witnessToCurrency USDWitness = USD

-- Given a CurrencyWitness, we can get a Typeable constraint on currency with
-- this function.
--
-- This function is necessary, because there's no other way to declare all
-- Currency values to be Typeable.
withTypeableCurrency :: CurrencyWitness c -> ((Typeable c) => a) -> a
withTypeableCurrency CHFWitness x = x
withTypeableCurrency EURWitness x = x
withTypeableCurrency PLNWitness x = x
withTypeableCurrency USDWitness x = x

parseCurrency :: Text -> Maybe Currency
parseCurrency = inverseMap show

currencyP ::
  ( MonadFail m
  , MP.MonadParsec e s m
  , MP.Token s ~ Char
  ) =>
  m Currency
currencyP = do
  Just cur <- parseCurrency . toText <$> replicateM 3 letterChar
  return cur
