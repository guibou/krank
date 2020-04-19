Krank is written in Haskell.

# Build with nix/cabal

```
nix-build -A krank
```

Should build the package.

```
nix-shell
```

Will drop you in a shell where you can `cabal v2-build`, `cabal v2-test`, ...

# Build with stack

```
stack build
```

Should work.

# Hacking

Edit code, ensure that test are passing and open a pull request on Github. Thank you ;)

- `nix-build -A krank` will build it
- `nix-build -A ormolu` will check formatting using ormolu. You can use `nix-shell -A ormolu-fix` to fix your code.
- `nix-build -A hlint` will lint with hlint. You can use `nix-shell -A hlint-fix` to fix your code.
