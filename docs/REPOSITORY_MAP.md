# Repository Map

This document maps the `progs` monorepo at a high level. It is intended for both humans and AI agents that need to orient themselves before editing code.

Last mapped: 2026-05-06.

## Overview

`progs` is a long-lived personal monorepo. It mixes usable utilities, scientific computing experiments, university teaching material, historical student projects, skeleton projects, and sandboxes used to learn specific technologies.

The repository currently contains about 2,000 tracked files. The dominant languages are Python, C++, Fortran, MATLAB/Octave, shell scripts, and Markdown. CMake is the most common build system for compiled projects.

## Top-Level Areas

| Path | Role | Typical contents | Edit posture |
| --- | --- | --- | --- |
| `apps/` | Small applications and near-finished tools | Python utilities, C++/Qt tools, scientific apps, media/PDF helpers | Good place for feature work when a project has a README or clear entry point |
| `bin/` | Daily helper scripts | File conversion, backup, media, markdown/PDF, repo utilities | Treat as operational scripts; keep changes small and backwards-compatible |
| `classes/` | Teaching and research/course code | DG, SPH, FEM, GPU, Gmsh, numerical methods | Preserve pedagogical clarity and reproducibility |
| `student/` | Historical student-era projects | Mechanisms, linear solvers, MATLAB/Fortran/C++ projects | Treat as archival unless explicitly modernizing |
| `sandbox/` | Learning experiments | Language/library/build/API examples | Expect incomplete or one-off code; useful for examples and prior art |
| `skel/` | Project skeletons | CMake, C++, Fortran, static library, SWIG templates | Use as starting patterns for new small projects |
| `metafor/` | Metafor ecosystem utilities | Mail, launch parameters, integrity checks, biomec helpers | Domain-specific; read local README first |
| `pytools/` | Shared Python helpers | Build and versioning helpers | Potentially shared by several projects |
| `snippets/` | Small reusable fragments | Bash, CBLAS, LaTeX, Pandoc, hello worlds | Copy/reference material rather than full projects |
| `envs/` | Environment setup | Platform build scripts | Operational setup scripts |
| `externals/` | External dependency download/build helpers | Eigen, Gmsh, Lua, olcPixelGameEngine scripts | Avoid vendoring generated downloads unless intended |
| `docs/` | Miscellaneous notes | Markdown, editor notes, Fortran notes, examples | Repository-level notes, not complete project docs |
| `ico/` | Shared icons/images | Small PNG assets | Shared visual resources |
| `attic/` | Archived repository-era metadata | Old CI file | Historical |
| `unsorted/` | Projects not yet classified | Flask, donut, STL perf, miscellaneous files | Candidate area for future triage |

## Size By Family

Approximate tracked-file distribution from `git ls-files`:

| Area | Files | Signal |
| --- | ---: | --- |
| `sandbox/` | 700 | Largest area; mostly learning experiments |
| `classes/` | 661 | Large body of teaching/scientific computing material |
| `student/` | 315 | Historical projects and old numerical code |
| `apps/` | 252 | Usable or semi-usable applications |
| `metafor/` | 54 | Focused domain utilities |
| `bin/` | 52 | Daily scripts |
| `skel/` | 38 | Templates |
| `snippets/` | 26 | Fragments |
| Other top-level support | smaller | Docs, envs, externals, icons, config |

## Language Profile

Most common tracked source/documentation extensions:

| Extension | Approx. files | Meaning |
| --- | ---: | --- |
| `.py` | 401 | Python scripts, tests, teaching utilities, app glue |
| `.cpp` | 326 | C++ apps, numerical kernels, examples |
| `.txt` | 241 | Notes, data, outputs, statements |
| `.md` | 199 | README and documentation |
| `.h` | 169 | C/C++ headers |
| `.m` | 96 | MATLAB/Octave scripts |
| `.sh` | 84 | Unix shell helpers |
| `.for`, `.f90`, `.f` | 159 combined | Fortran legacy and numerical code |
| `.cmd`, `.bat` | 51 combined | Windows build/run helpers |

## Recently Active Areas

Recent commit history and timestamps suggest active or recently revisited work in:

- `bin/`: Markdown/PDF conversion scripts.
- `apps/splines/`: Python spline code with pytest coverage and a local `AGENTS.md`.
- `student/dcm1/`: restored four-bar mechanism Qt application with tests.
- `classes/dg_lts_2025/`: current course material for MATH0471.
- `classes/louis/`, `sandbox/julia/`, `sandbox/python_packaging/`, `sandbox/vtk/`, `sandbox/pyvista/`: recent filesystem timestamps indicate possible ongoing experiments.

## Navigation Strategy

For a quick answer:

1. Search by concept with `rg`.
2. Narrow by family: `apps` for tools, `classes` for course/scientific material, `sandbox` for examples, `student` for historical code.
3. Read the nearest README.
4. Inspect manifest/build files before editing.

For repository cleanup:

1. Do not begin with code moves.
2. Add or improve local README files first.
3. Mark each project as `active`, `reference`, `archival`, or `candidate for extraction`.
4. Only then split, archive, or promote projects.

