# Contributions

## Design guidelines

When designing a rule, we want to follow some guidelines:

- Be as language agnostic as possible. We look for plain text in comment and if possible we will
  parse the full text without any logic to separate comment from code.
- Be as simple as possible. The less custom markers, the better.
- We can be opiniated if needed. That's better to have a simple rule with an opiniated choice than a
  complex rule aiming at matching all the use case of the universe.

## UI Design

### Level of error

Krank will raise 3 types of notifications:

- Info
- Warning
- Error

An *Error* is an assumption on the codebase which should never be violated. In the default usage,
those are meant to fail a build (in a CI system).
A *Warning* is an assumption on the codebase that is important to report but which is not deemed
critical enough to block the build.
An *Info* is simply a notification for the user that can give some context about a comment check.

The UI will provide flags to ignore some notifications or change their level (e.g. consider all
warnings as fatal or the converse).

### Performance

Parsing of text file and evaluating of rules (especially the ones which need Internet access) should
be done in a parallel way to reduce the time used by the executable.

## Hacking

Krank is written in Haskell, because why not ;) It aims at being available in Hackage / Stackage
with minimal (if zero) system dependencies.
