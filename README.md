# AutoCAD Rename Layouts (AutoLISP)

A small but growing AutoLISP utility for **renaming layout tabs based on title block attributes**.

## Features
- Read block attributes from layouts (e.g., sheet number/title) and rename layouts accordingly
- Modular `src/` structure with versioned releases
- CI checks for Markdown and EditorConfig compliance
- Conventional Commits + semantic versioning
- Example files and tests scaffolding

## Getting Started

### Installation
1. Clone or download this repository.
2. In AutoCAD, run `APPLOAD`, browse to `src/RenameLayouts.lsp`, and load it.  
   Alternatively, add the folder to your AutoCAD support file search path and use:
   ```lisp
   (load "RenameLayouts.lsp")
   ```

### Usage
This project exposes one or more commands/functions to rename layouts based on your title block attributes.  
Open `src/RenameLayouts.lsp` to see available commands (look for any `C:<CommandName>` definitions). Common patterns:
```lisp
; Example invocation once loaded:
; (c:RenameLayouts)       ; if defined
; (c:MatchLayoutNameToSheet) ; if defined in your script
```

> Tip: If your title block uses specific attribute tags, adjust the constants in the script or extend the helper functions accordingly.

### Repository Structure
```
.github/
  ISSUE_TEMPLATE/
    bug_report.md
    feature_request.md
  workflows/
    ci.yml
docs/
examples/
src/
  RenameLayouts.lsp
tests/
.editorconfig
.gitignore
CHANGELOG.md
CONTRIBUTING.md
LICENSE
README.md
pre-commit-config.yaml
```

### Development
- Edit AutoLISP in **Visual Studio Code** with the **AutoLISP extension** (Autodesk) for syntax highlighting and lint-like feedback.
- Use **Conventional Commits** for messages (e.g., `feat: add multi-line title support`).  
- Open issues with clear reproduction steps; submit PRs with a short demo GIF if possible.

### Releasing
- Use semantic versioning: `v0.1.0`, `v0.2.0`, …
- Create a GitHub Release with notes pulled from `CHANGELOG.md`

## Roadmap
- [ ] Robust attribute reader with error handling
- [ ] Support multiple title lines (e.g., `TITLE1`, `TITLE2`)
- [ ] Config file for block name + attribute tags
- [ ] Dry-run and verbose logging
- [ ] Batch processing for multiple drawings

## License
MIT © 2025 Tommy Lee
