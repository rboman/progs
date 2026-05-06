# Reorganization Notes

These notes propose a gradual way to make the monorepo easier to navigate without losing its value as a searchable personal archive.

## Guiding Idea

The monorepo is useful because it keeps personal context, teaching material, experiments, and utilities searchable in one place. The goal should not be to make it look like a clean single-product repository. The goal is to make intent visible.

## Recommended Metadata

Classify each project with four small labels:

- `status`: active, reference, archival, sandbox, unknown.
- `family`: app, script, teaching, research, student, skeleton, snippet, external, docs.
- `language`: primary languages.
- `portability`: Windows, Linux, cross-platform, unknown.

This can live in local README files before introducing any machine-readable index.

## Suggested Future Layout

The current top-level layout is mostly sound. The main cleanup opportunity is inside `unsorted/` and a few large mixed areas.

Possible future moves:

| Current | Possible destination | Reason |
| --- | --- | --- |
| `unsorted/flask` | `sandbox/webapi/flask` | It is a web framework experiment |
| `unsorted/donut` | `sandbox/graphics/donut` | It is a rendering/demo experiment |
| `unsorted/stl_perf` | `sandbox/cpp/perf` or `sandbox/cpp11/stl_perf` | It is a C++ performance experiment |
| standalone `unsorted/*.cpp` | `snippets/cpp` or relevant sandbox | Small focused fragments |
| standalone `unsorted/*.f90` | `snippets/fortran` or `sandbox/fortran` | Small focused fragments |

Do these moves only after adding a tiny README to each item so search context survives the move.

## Extraction Candidates

Projects that may deserve their own repository later:

- `apps/splines`: small, active Python project with tests and clear scope.
- `student/dcm1`: historically interesting Qt mechanism simulator with modernized structure and tests.
- `classes/dg_lts_2025`: active teaching material that may benefit from course-specific issue tracking and CI.
- `bin` Markdown/PDF workflow scripts: daily utilities could become a small personal tooling repo if they grow.

## Documentation Roadmap

1. Keep `docs/REPOSITORY_MAP.md` and `docs/PROJECT_CATALOG.md` updated after major moves.
2. Add short README files to undocumented active projects, starting with `apps/contacts`, `apps/winlibs`, `classes/louis`, and recent `sandbox/*` directories.
3. Add local `AGENTS.md` only where agent behavior differs from the global guidance.
4. Add `STATUS.md` or project metadata only if manual README updates become too hard to maintain.

## What Not To Do Yet

- Do not split the repository solely because it is large.
- Do not rewrite old code to modern style without a concrete goal.
- Do not delete sandbox or historical material just because it is incomplete.
- Do not introduce a monorepo-wide package manager unless several active projects share a build lifecycle.

