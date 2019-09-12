Krank is written in Haskell.

# Build with nix/cabal

```
nix-build -A krank
```

Should build the package.

```
nix-shell
```

Will drop you in a shell where you can `cabal new-build`, `cabal new-test`, ...

# Build with stack

```
stack build
```

Should work.

# Hacking

Edit code, ensure that test are passing and open a pull request on github. Thank you ;)
