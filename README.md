# safe-money

This is an experimental Haskell library that explores creating a safe type for
representing monetary amounts.

I set up the following requirements:

1. The type should allow declaring the number of fractional digits.
2. The type should allow declaring the amount's currency and prevent operations
   on monetary amounts of differring currencies.
3. Currency should only allow valid currencies.

## Unsolved problems

```haskell
-- How do I write this function efficiently?
addSomeMoney :: SomeMoney r -> SomeMoney r -> Maybe (SomeMoney r)
-- This kind of pattern matching doesn't work (making sure currency is the same):
addSomeMoney (SomeMoney m@(Money c a)) m'@(SomeMoney (Money c b)) =
  Just . SomeMoney $ add m m'
addSomeMoney _ _ = Nothing
-- This gives "Conlicting definitions of 'c'".

-- Best I can do is to list out all posibilities:
addSomeMoney (SomeMoney m@(Money c _)) (SomeMoney m'@(Money c' _)) = case (c, c') of
  (CHFWitness, CHFWitness) -> Just . SomeMoney $ add m m'
  (PLNWitness, PLNWitness) -> Just . SomeMoney $ add m m'
  -- ...
  (_, _) -> Nothing

-- Can I do not forgetful currency to witness cast?
currencyToWitness :: (c :: Currency) -> CurrencyWitness c
-- The above doesn't work, because (c :: Currency) says that currencyToWitness
-- accepts a paramater of type c, which is a value (of type Currency), so it
-- doesn't work.
```
