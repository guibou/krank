# Krank

Krank checks your code source comments for important markers.

Comments are part of our code and are not usually tested correctly. Hence their content can become incoherent or obsolete. Krank tries to avoid that by running checkers on the comment themselves.

# Usage

Just launch the `krank` command with a list of files as arguments. It
works on any kind of source code file and print a reports of
informations found in the comments:

```bash
$ krank $(git ls-files)

default.nix:20:20: info:
  still Open: https://github.com/NixOS/nix/issues/2733

foo.hs:67:11: error:
  is now Closed: https://github.com/bazelbuild/bazel/issues/6313
```

Here `krank` is telling us that our source code links to github
issues which are now closed. Time to remove some workarounds now that upstream issues are fixed!

You can check `krank --help` for a list of options.

# Available checkers

- [IssueTracker](docs/Checkers/IssueTracker.md) is listing Github
  issue linked in comment. Issues which are still Open will be listed
  as info and Closed *issues* are listed as *error*. Convenient to know
  when to remove workarounds.

# Misc

- Krank is available on Hackage, but you can also [build it manually](HACKING.md).
- If you want to contribute or add a new checker, you can read the [Contributions guidelines](CONTRIBUTING.md) or [open an issue](https://github.com/guibou/krank/issues).
