# Revision history for krank

* #88 krank tries to test files listed by `git ls-files` or `find` by default.
* #89 support `NO_COLOR`. https://no-color.org/.

## 0.2.2 -- 2020-06-30

* #84 fix build with `unordered-containers` 0.2.11.0
* #81, new `--version` command line argument

## 0.2.1 -- 2020-05-02

* #76 fix. ignore space in URL
* Output now includes the issue title.

## 0.2.0 -- 2020-04-19

* GHC 8.10 support
* Output is more compact and more detailed.
* Output is now colored (can be disabled with `--no-colors`).
* Gitlab support. See `--issuetracker-gitlabhost` for host / api key pairs.
* Huge performance improvement. Parsing a huge codebase should only be a matter
  of a few seconds.
* Parallel request execution. Requests to the issue tracker host are not
  sequential anymore. This can dramatically reduce the runtime.
* Ignore line. Add `krank:ignore-line` in your comments and the associated
  closed issue won't be seen as an error anymore.
* Failure mode: `krank` will return non 0 exit code if an issue is closed (an
  not ignored).

## 0.1.0.0 -- 2019-10-21

* First version. Released on an unsuspecting world.
* Support for Github issues tracking
