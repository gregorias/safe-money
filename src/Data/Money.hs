-- Required for instance (Num r) => Additive r. I don't think wrapping the head
-- in a newtype makes sense.
{-# LANGUAGE UndecidableInstances #-}

-- | The safe Money type.
module Data.Money (
  -- * Types
  Money (..),
  SomeMoney (..),
  makeMoney,
  makeSomeMoney,

  -- * Operations
  add,
  multiply,
  negate,
  divide,
  abs,
) where

import Data.Currency (
  Currency,
  CurrencyWitness,
  SomeCurrencyWitness (..),
  currencyToWitness,
 )
import Relude hiding (
  abs,
  negate,
 )
import Relude qualified
import Witch (From (..), TryFrom (..), TryFromException (..))

-- | Money consists of a currency and an amount.
--
-- The currency is a type-level value together with its type witness, and the
-- amount is a runtime value.
--
-- The representation can be any type, but in practice it's usually a fixed
-- point decimal.
data Money (currency :: Currency) representation = Money
  { moneyCurrency :: !(CurrencyWitness currency)
  , moneyAmount :: !representation
  }
  deriving stock (Eq, Show)

instance (From l r, c0 ~ c1) => From (Money c0 l) (Money c1 r) where
  from (Money witness amount) = Money witness (from amount)

instance (TryFrom l r, c0 ~ c1, Show l, Typeable l, Typeable r) => TryFrom (Money c0 l) (Money c1 r) where
  tryFrom source@(Money witness amount) =
    case tryFrom amount of
      Right newAmount -> Right $ Money witness newAmount
      Left exception -> Left (TryFromException source (Just $ SomeException exception))

-- | The existential type for Money.
--
-- We hide only the currency, because we expect that return values will often
-- not know it at compile-time unlike the representation.
data SomeMoney r where
  SomeMoney :: Money c r -> SomeMoney r

-- | Makes a 'Money' value from a currency and an amount.
makeSomeMoney :: Currency -> r -> SomeMoney r
makeSomeMoney currency amount =
  case currencyToWitness currency of
    SomeCurrencyWitness witness -> SomeMoney $ Money witness amount

-- | Makes a 'Money' value from a currency and an amount.
makeMoney :: CurrencyWitness c -> r -> Money c r
makeMoney = Money

-- We can't use the Num class for Money, because some operations don't make sense, e.g.,
-- multiplication of two money values. We have to make our own classes for numeric-like operations.

add :: (Num r) => Money c r -> Money c r -> Money c r
add (Money witness1 amount1) (Money _ amount2) =
  Money witness1 (amount1 + amount2)

multiply :: (Num r) => Money c r -> r -> Money c r
multiply (Money witness1 amount1) amount2 =
  Money witness1 (amount1 * amount2)

negate :: (Num r) => Money c r -> Money c r
negate (Money witness amount) = Money witness (Relude.negate amount)

divide :: (Fractional r) => Money c r -> r -> Money c r
divide (Money witness1 amount1) amount2 =
  Money witness1 (amount1 / amount2)

abs :: (Num r) => Money c r -> Money c r
abs (Money witness amount) = Money witness (Relude.abs amount)
