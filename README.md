# Krank

Krank checks your code source comments for important markers.  
For a list of available comment checkers, see [Checkers](#Checkers)

For now it is vaporware, it is still in the design phase.

# Purpose

Krank will parse the comments in the code and check fact, so the code won't bitrot. For example, it
will:

- look for link to github issue / pull request and warn you if there are now closed: #1
- check for cross reference in comments: #2
- check for outdated / bitroting TODOs: #3, #4
- check for association between code and comment: #5

Check the github issues section to see the list of possible checkers.

# Usage

Just launch the `krank` command and pass a file or a list of files.  
You can check `krank --help` for a list of options.  
Additionally, the checker documentation pages might contain more information about checker specific
option (See below)

## Outputs

Each checker will return a list of *Violation* each specifying a level:

* **Info** violations are there just to give some information
* **Warning** violations are meant to warn the user about an issue in the comments, but one that is
  not considered important enough to fail a build.
* **Error** violations are meant to fail the build.

Each checker provides a default level for each type of violation it might return, but this level can
be overridden by configuration flags (TBD)

## Return Code (TBD)

`krank` will exit with a code that is equal to the number of **Error** level violations it reported.

This implies that if `krank` returns no **Error** but only **Info** or **Warning**, it will return
with an exit code of `0`

# Checkers

[IssueTracker](docs/Checkers/IssueTracker.md)
