# Contributions

## Design guidelines

When designing a rule, we want to follow some guidelines:

- Be as language agnostic as possible. We look for plain text in comment and if possible we will
  parse the full text without any logic to separate comment from code.
- Be as simple as possible. The less custom markers, the better.
- We can be opiniated if needed. That's better to have a simple rule with an opiniated choice than a
  complex rule aiming at matching all the use case of the universe.

## Build

Krank can be build with stack, or cabal. The preferred developer workflow is based on nix and optionally direnv.

```shell
$ nix-build . -A something
```

Will build `something`. See auto completion for the different options.

```
$ nix-shell
```

Will open a nix shell where `cabal` is available to work on the project using the latest GHC.

```
$ nix-build . -A hlint
```

Will run `hlint` (this is part of CI).

```
$ nix-build . -A ormolu
```

Will run `ormolu` to check your codebase formatting (this is part of CI).

```
$ nix-shell . -A ormolu-fix
```

Will run `ormolu` on the codebase to fix formatting.
