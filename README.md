# safe-money

This is an experimental Haskell library that explores creating a safe type for
representing monetary amounts.

## Development

This section is intended for developers. It describes development related
matters.

### Dev environment setup

1. Install hlint and fourmolu, which are used in pre-commits (I do it from my
   home directory):

   ```shell
   stack install hlint fourmolu
   ```

2. To setup Git hooks, install lefthook:

   ```shell
   lefthook install
   ```
