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
-- Can I do not forgetful currency to witness cast?
currencyToWitness :: (c :: Currency) -> CurrencyWitness c
-- The above doesn't work, because (c :: Currency) says that currencyToWitness
-- accepts a paramater of type c, which is a value (of type Currency), so it
-- doesn't work.
```

## See also

- [How do I efficiently add existentially typed safe money values?](https://stackoverflow.com/questions/77429592/how-do-i-efficiently-add-existentially-typed-safe-money-values)
