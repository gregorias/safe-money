# safe-money

This is an experimental Haskell library that explores creating a safe type for
representing monetary amounts.

I set up the following requirements:

1. The type should allow declaring the number of fractional digits.
2. The type should allow declaring the amount's currency and prevent operations
   on monetary amounts of differring currencies.
3. Currency should only allow valid currencies.
