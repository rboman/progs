# AI Navigation Guide

This guide is for AI agents working in this monorepo.

## First Five Minutes

1. Run `git status --short` and note existing user changes.
2. Identify the target family: `apps`, `classes`, `student`, `sandbox`, `metafor`, `bin`, or support files.
3. Read the nearest `README.md`.
4. Check for a local `AGENTS.md`; currently `apps/splines/AGENTS.md` is one such file.
5. Inspect manifests/build files before deciding how to test.

## Search Patterns

Use repository-wide search when the user asks about a concept, then narrow:

```powershell
rg "concept"
rg --files apps classes student sandbox metafor
rg --files -g README.md -g CMakeLists.txt -g requirements.txt -g pyproject.toml
```

Useful path heuristics:

- Need a usable utility: search `apps/` and `bin/` first.
- Need a teaching/scientific explanation: search `classes/`.
- Need old numerical code or recovered student work: search `student/`.
- Need examples of a language/library/build problem: search `sandbox/` and `skel/`.
- Need Metafor-specific context: search `metafor/`.

## Editing Guidance

This repo deliberately preserves many coding eras. Do not normalize style globally.

When editing:

- Keep changes local to the project requested.
- Preserve old code style unless the user asks for modernization.
- Add tests close to the project when there is an existing pattern.
- Prefer CMake for compiled projects already using CMake.
- Prefer simple script-level changes for `bin/`.
- Do not move projects as part of ordinary bug fixes.

When documenting:

- Add local README notes before attempting a structural reorganization.
- Mention entry points, build/test commands, dependencies, and project status.
- Use relative links so docs work on GitHub and locally.

## Build And Test Expectations

There is no single trustworthy root-level build for all projects. Each project is independent.

Common commands:

```powershell
pytest -q
cmake -S . -B build
cmake --build build --config Release
ctest --test-dir build -C Release --output-on-failure
```

Run commands from the project directory unless the local README says otherwise.

## Risk Areas

- `bin/` scripts may encode personal workflows and paths.
- `classes/` may contain teaching statements where behavior and naming matter.
- `student/` may be historical; modernization can erase useful context.
- `sandbox/` may be intentionally incomplete.
- `externals/` may download third-party code; avoid network-dependent edits unless requested.
- Files with mojibake or old encodings may be historical; avoid bulk encoding rewrites unless that is the task.

## Suggested Project Metadata

For future local READMEs, use this compact template:

```markdown
# Project Name

Status: active/reference/archival/sandbox
Family: apps/classes/student/sandbox/metafor/bin/skel/snippets
Languages: Python, C++, Fortran, ...

## Purpose

One paragraph describing why this exists.

## Entry Points

- `main.py` or executable target
- Important modules

## Build And Test

Commands that work from this directory.

## Notes For Future Agents

Anything surprising, fragile, historical, or intentionally unfinished.
```

