# Contributing

Thanks for your interest! This repo uses **Conventional Commits** and **semantic versioning**.

## Development Workflow
1. Create a feature branch from `main`: `git checkout -b feat/your-feature`
2. Make changes in `src/` and add/update examples or tests.
3. Commit using Conventional Commits:
   - `feat: add support for multi-line sheet titles`
   - `fix: handle missing attribute gracefully`
   - `docs: update README with usage steps`
4. Open a Pull Request with:
   - A summary of the change
   - Screenshots or a short GIF (optional but helpful)
   - Notes on testing performed

## Coding Guidelines
- Prefer small, composable functions in AutoLISP.
- Document public functions with a short header comment (purpose, params, return).
- Keep line length reasonable and avoid trailing whitespace.

## Releasing
- Bump version in tags (e.g., `v0.2.0`) following semver.
- Update `CHANGELOG.md` with user-facing changes.
