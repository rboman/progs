# Repository Guidelines

## Project Structure & Module Organization
This repository is a small root-level Python project. Core geometry lives in `point.py` (`Pt`) and spline logic lives in `splines.py` (`Seg`, `Spline`). Demo entry points are `plot_matplot.py` for a quick Matplotlib plot and `plot_qt.py` for the interactive PyQt viewer. Tests currently live beside the code in `test_point.py`; follow that pattern for new modules with files such as `test_splines.py`.

## Build, Test, and Development Commands
Use the system Python directly; there is no build system or package wrapper.

- `pytest -q`: run the test suite. This is the primary verification command.
- `python3 plot_matplot.py`: open the Matplotlib demo for visual spline checks.
- `python3 plot_qt.py`: launch the interactive Qt editor for manual exploration.

If you add external dependencies, document installation steps in the README or in the PR description.

## Coding Style & Naming Conventions
Follow the existing Python style: 4-space indentation, small modules, and straightforward class-based code. Keep public class names in `CapWords` (`Pt`, `Spline`) and functions/tests in `snake_case`. Preserve the current naming tone in numerical code, even where short math-oriented names such as `u`, `l1`, or `ksiOnSeg` are already established. Prefer explicit imports like `from point import Pt` over adding package indirection unless the project is reorganized.

## Testing Guidelines
Tests use `pytest` and currently emphasize behavior-level checks for point arithmetic and error handling. Name tests `test_<behavior>()` and keep them deterministic. Add tests for every bug fix and for spline changes that affect tangent computation, segment length, or evaluation. Run `pytest -q` before opening a PR.

## Commit & Pull Request Guidelines
Recent history mixes short imperative subjects (`Add pytest coverage for Pt`, `Harden point operations`) with scoped forms like `docs(context): ...` and `fix(review): ...`. Keep commit titles concise, imperative, and specific to one change. PRs should explain the behavior change, list verification performed, and include screenshots or brief notes when plotting behavior or GUI interaction changes.

## Dependencies & Environment
`pytest` is required for tests. The demos also require `numpy`, plus `matplotlib` for `plot_matplot.py` and `PyQt5` for `plot_qt.py`. Keep optional demo dependencies separate from core logic when possible.
