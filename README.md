# Krank

![Hackage](https://img.shields.io/hackage/v/krank)

Krank checks your code source comments for link to issue trackers, such as
gitlab or github.

Krank will then outputs a report of the status of theses issues, if they are
closed or still open.

# Why

You are blocked on upstream ticket, there you write a nice comment in your code such as:

```python
# blocked on upstream ticket https//github.com/Foo/Bar/issues/1234
# This workaround should be removed once the upstream ticket is closed
if workaround:
  pass
```

And, this stays in your codebase for the next ten years, even if the upstream issue is closed.

Now you can run `krank` on your codebase, and it will tells you that issue
`#1234` in project `Foo/Bar` has been closed. Time to remove your workaround.

# Usage

Just launch the `krank` command, optionally with a list of files as arguments. It
works on any kind of source code file and prints a report of
informations found in the comments:

```bash
$ krank

default.nix:20:20: info:
  still Open: https://github.com/NixOS/nix/issues/2733

foo.hs:67:11: error:
  now Closed: https://github.com/bazelbuild/bazel/issues/6313
```

`krank` will fail (i.e. non-zero exit code) in case of any error.

Here `krank` is telling us that our source code links to github
issues which are now closed. Time to remove some workarounds now that
upstream issues are fixed!

You can check `krank --help` for a list of options, such as
configuring your API token for external services, such as github and
gitlab.

# Specific documentation

[IssueTracker](docs/Checkers/IssueTracker.md) is listing Github and Gitlab
issue linked in comment. Issues which are still Open will be listed as info and
Closed *issues* are listed as *error*. Convenient to know when to remove
workarounds.

# Red herring

If you want to ignore all krank checkers on a given line of code, add `krank:ignore-line` on this
same line. Krank will then skip this line and not report anything on it

# Misc

- Krank is available on
  [Hackage](http://hackage.haskell.org/package/krank), but you can
  also [build it manually](HACKING.md).
- If you want to contribute or add a new checker, you can read the
  [Contributions guidelines](CONTRIBUTING.md) or [open an
  issue](https://github.com/guibou/krank/issues).
