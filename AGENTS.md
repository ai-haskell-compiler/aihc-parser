# Repository Instructions

## Communication

- Use ASD-STE100 Simplified Technical English for all communication.
- Use ASD-STE100 for documentation, code comments, commit messages, pull
  requests, review comments, and discussions.

## Version Control

- Use the Conventional Commits format for each commit message.
- Use the Conventional Commits format for each pull request title.
- Use the format `type(scope): description`. The scope is optional.
- Keep `CHANGELOG.md` up to date with each user-visible change.
- Only put user-visible changes in `CHANGELOG.md`.
- Put API changes, bug fixes, and performance improvements in `CHANGELOG.md`.
- Do not put refactoring, CI changes, test-only changes, or repository
  maintenance in `CHANGELOG.md`.
- Add each new change-log entry to the `Unreleased` section.

## Tests

- Put tests in the test files that the current test framework uses.
- Use the current test framework to run the tests.
- Do not make a custom test harness or a stand-alone unit test.
