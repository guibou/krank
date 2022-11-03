# Contributions

## Design guidelines

When designing a rule, we want to follow some guidelines:

- Be as language agnostic as possible. We look for plain text in comment and if possible we will
  parse the full text without any logic to separate comment from code.
- Be as simple as possible. The less custom markers, the better.
- We can be opiniated if needed. That's better to have a simple rule with an opiniated choice than a
  complex rule aiming at matching all the use case of the universe.

## Build

Krank can be build with stack, or cabal. The preferred developer workflow is based on nix.

```shell
$ nix build
```

Will build the krank package.

```
$ nix develop
```

Will open a nix shell where `cabal` and haskell language server are available
for work. You may want to use `nix develop .#shell` if you don't want to
download HLS.

```
$ nix run .#ormolu
```

Will run `ormolu` on the codebase to fix formatting.
