# IssueTracker

## Purpose

Looks for issue tracker references in the codebase. When those are put in comments, it's generally
to signal that some code is there to workaround a library bug/limitation.
This checker will inspect the issue status and warn if the issue mentioned is closed (meaning that
the workaround might be now unnecessary)

## Support

**IssueTracker** currently supports the following list of issue trackers:
* Github

## Configuration

### Github

#### API rate limitation

If you are inspecting a codebase that has many issue references, or if you are running `krank` often
from the same IP, you might hit a Github rate limitation (60 api calls per hour for anonymous
requests).
To circumvent this, you can create a **Personal Access Token** and pass it to `krank` with the
`--issuetracker-githubkey` option. Github API calls will now be authenticated and the rate limit
will be 5000 API calls per hour.
Note that for IssueTracker to be able to report on issues from public repositories, you don't need
to give any scope at all to your **Personal Access Token**.

To generate a **Personal Access Token**, consult [the following documentation](https://help.github.com/en/articles/creating-a-personal-access-token-for-the-command-line)

#### Private repositories

To get information about private repositories, you need to provide Github authentication to the
**IssueTracker** checker.
To do so, you need to generate a **Personal Access Token**
([See
documentation](https://help.github.com/en/articles/creating-a-personal-access-token-for-the-command-line))
with the `repo` scope and pass it to `krank` with the `--issuetracker-githubkey` option
