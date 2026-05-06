# Repository Guidelines

This repository is a personal monorepo containing small applications, teaching material, numerical experiments, old student projects, daily helper scripts, skeleton projects, and technology sandboxes.

Start by reading:

- `README.md` for the short public overview.
- `docs/REPOSITORY_MAP.md` for the human-scale map.
- `docs/PROJECT_CATALOG.md` for the inventory of project families.
- `docs/AI_NAVIGATION.md` for agent-specific working guidance.
- A local `README.md` or `AGENTS.md` inside the project you are touching, if one exists.

## Repository Shape

The main project families are:

- `apps`: small applications and tools, often closer to usable programs.
- `bin`: daily scripts and command-line utilities.
- `classes`: teaching and research/course material, mostly numerical methods and scientific computing.
- `student`: older student-era projects and recovered historical code.
- `sandbox`: experiments used to learn languages, libraries, build systems, or APIs.
- `skel`: starter templates for C++, Fortran, CMake, SWIG, and static library layouts.
- `metafor`: utilities related to the Metafor finite element ecosystem.
- `pytools`, `snippets`, `envs`, `externals`, `docs`, `ico`: shared support material.

## Working Rules

- Treat old projects as historical unless the task explicitly modernizes them.
- Prefer local conventions over introducing new frameworks.
- Look for a nearby `README.md`, `CMakeLists.txt`, `requirements.txt`, `pyproject.toml`, or local `AGENTS.md` before editing.
- Avoid sweeping repository-wide refactors. This monorepo intentionally mixes eras, languages, and coding styles.
- Keep generated build outputs, downloaded externals, and temporary files out of version control.
- When changing numerical code, preserve reproducibility and add small deterministic tests when feasible.
- When touching teaching material, avoid changing statement semantics unless requested.

## Common Commands

Useful discovery commands:

```powershell
git status --short
git ls-files
rg --files
rg "pattern" apps classes student sandbox metafor
```

Build and test commands are project-local. Common patterns include:

```powershell
cmake -S <project> -B <project>/build
cmake --build <project>/build --config Release
pytest -q
```

Do not assume one root-level build command covers the whole monorepo.

