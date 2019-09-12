# Krank

Krank checks your code source comments for important markers

For now it is vaporware, it is still in the design phase.

# Purpose

Krank will parse you comment and check fact, so the code won't bitrot. For example, it will:

- look for link to github issue / pull request and warn you if there are now closed: #1
- check for cross reference in comments: #2
- check for outdated / bitroting TODOs: #3, #4
- check for association between code and comment: #5

Check the github issues section to see the list of possible checkers.


# Design guidelines

When designing a rule, we want to follow some guidelines:

- Be as language agnostic as possible. We look for plain text in comment and if possible we will parse the full text without any logic to separate comment from code.
- Be as simple as possible. The less custom markers, the better.
- We can be opiniated if needed. That's better to have a simple rule with an opiniated choice than a complex rule aiming at matching all the use case of the universe.

# UI Design

## Level of error

Krank will raise two type of notifications:

- Warnings
- Fatal errors

A *Fatal error* is an assumption on the codebase which should never be violated and only depends on the local source code. This should fail in a continuous integration system.

A *Warning* is an assumption on the codebase which may be violated by external changes. For example, a github issue may be closed at any point in time and raise the warning. We don't want that to fail in a continuous integration context to ensure reproducibility of build. However the notification is important.

The UI will provide flags to ignore some notifications or change the level of them (e.g. consider all warnings as fatal or the converse).

## Performance

Parsing of text file and evaluating of rules (especially the ones which need Internet access) should be done in a parallel way to reduce the time used by the executable.

# Hacking

Krank is written in Haskell, because why not ;) It aims at being available in Hackage / Stackage with minimal (if zero) system dependencies.
