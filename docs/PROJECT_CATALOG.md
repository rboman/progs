# Project Catalog

This catalog groups the known projects and subareas of the monorepo. It is descriptive, not normative: many directories are historical, experimental, or intentionally rough.

Last mapped: 2026-05-06.

## Status Labels

- `active`: likely useful now or recently touched.
- `reference`: useful as examples, teaching material, or snippets.
- `archival`: old code kept for memory, history, or occasional recovery.
- `sandbox`: experiment or learning material.
- `unknown`: needs a local read before classification.

## Apps

`apps/` contains small applications and semi-finished tools.

| Path | Description | Likely status |
| --- | --- | --- |
| `apps/contacts` | Python contact-management utility; README currently empty. | active/unknown |
| `apps/EHD` | Scientific/numerical application with several CMake subprojects (`ehd`, `gauss`, `sky`) and tests. | reference |
| `apps/ffmpeg` | Media conversion and FFmpeg experiments/tools. | reference |
| `apps/fractal` | Fractal experiments in C++/Qt, Java, and olcPixelGameEngine. | sandbox/reference |
| `apps/GenMAI` | Quad mesher for asperity crushing simulations in cold rolling. README warns the C++ style is historical. | archival/reference |
| `apps/mails` | Mail-related helper. | unknown |
| `apps/octree` | Octree-related app/experiment. | reference |
| `apps/pdf2ppt` | PDF to PowerPoint helper. | reference |
| `apps/plot_applet`, `apps/plotter` | Plotting helpers/applets. | reference |
| `apps/PPTcalc` | PowerPoint calculation tool with Qt Designer material. | archival/reference |
| `apps/splines` | Python spline playground with Matplotlib/PyQt demos and pytest tests for `Pt`. Has local `AGENTS.md`. | active |
| `apps/textfilters` | Text processing filters. | reference |
| `apps/tra2py` | Translation/conversion helper. | reference |
| `apps/unixadm` | Unix administration helper. | unknown |
| `apps/vtktools` | VTK-related tools. | reference |
| `apps/winlibs` | Windows library helper notes. | active/reference |

## Bin

`bin/` contains standalone scripts. They are operational by nature and may be used directly from the shell.

Representative themes:

- Media conversion: `avi2mp4.py`, `cam2mp4.py`, `flac2mp3.py`, `ffmpeg_cpu_vs_gpu.py`.
- PDF/Markdown workflows: `convert_code_to_pdf.py`, `mkhandout.py`, `pdfmerge.py`, `pdf4x4split.py`, `several_markdown_2_one_pdf.sh`.
- File/repo maintenance: `batch_rename_files.py`, `bookmarks_cleaner.py`, `chkencoding.py`, `clean_fortran_old.py`, `rsync_*.sh`, `upgrade_fortran*.py`.
- Small daily utilities: `calc.py`, `check_env.py`, `which_python.py`, `starttime.py`.

Likely status: active/reference. Changes here should be conservative because scripts may encode personal workflows.

## Classes

`classes/` is mostly teaching material and scientific computing experiments.

| Path | Description | Likely status |
| --- | --- | --- |
| `classes/dg_2023` | Discontinuous Galerkin course/project material, 2023 generation. | reference |
| `classes/dg_lts_2025` | Current MATH0471 material with DG/LTS examples, tests, envs, verification notes. | active |
| `classes/dg1d_python` | 1D DG Python material. | reference |
| `classes/fdtd` | FDTD material. | reference |
| `classes/femcode` | FEM-related examples, including MUMPS notes and attic variants. | reference |
| `classes/gmsh_examples`, `classes/gmshio` | Gmsh examples and mesh I/O material. | reference |
| `classes/gpu` | GPU/OpenACC/CMake material and tests. | reference |
| `classes/hw1_interp_error` | Interpolation error homework/material. | active/reference |
| `classes/lbm` | Lattice Boltzmann method material. | reference |
| `classes/louis` | SPH/numerical work with tests and recent activity. | active/unknown |
| `classes/minibarreTE` | Mini-bar/coupling teaching or numerical project. | reference |
| `classes/sph`, `classes/sph0`, `classes/sph_2024` | SPH teaching/research material across generations. | reference/active |
| `classes/wiki` | Collected teaching/wiki material. | reference |

## Student

`student/` stores old student-era projects and later recovered code.

| Path | Description | Likely status |
| --- | --- | --- |
| `student/dcm1` | Four-bar mechanism simulator from 1994, modernized to Qt 5 with solver tests. | active/archival |
| `student/dcm2` | Follow-up DCM project with CMake, tests, and Python helpers. | archival/reference |
| `student/lejeune`, `student/litt`, `student/mico`, `student/ndh`, `student/nihoul` | Historical student projects, mostly CMake/C++/Python/Fortran. | archival |
| `student/opti`, `student/tcm` | Optimization and mechanics/course projects. | archival/reference |
| `student/tfe` | Older thesis/final-project numerical solvers: CG, GMRES, BiCG, Symmlq, SPARSKIT, EISPACK, tests. | archival/reference |

## Sandbox

`sandbox/` is the largest area. It is best understood as a learning lab.

Major themes:

- Languages: `asm`, `bash`, `csharp`, `fortran`, `golang`, `javascript`, `julia`, `lua`, `matlab`, `php`, `python`.
- C++ and build experiments: `cpp11`, `exporttpl`, `singleton`, `versions`, `understand_findpython`, `clang`, `clangformat`.
- Scientific/HPC libraries: `fortran-blas`, `mkl`, `mpi`, `openmp`, `scalapack`, `tbb`, `vtk`, `pyvista`, `gmsh`.
- Python/native bindings: `fortranpython`, `fortranc`, `swig-distutils`, `swig-leak`, `swig-stdvector`, `swig-unicode`, `python_packaging`.
- GUI/graphics/games: `qt`, `qt-retina`, `pygame`, `pyopengl`, `raytracing`, `onelonecoder`, `processing`, `matplotlib`, `tkinter`.
- Miscellaneous APIs and experiments: `chatgpt`, `copilot`, `filemgr`, `json`, `md5`, `regexp`, `remark`, `sockets`, `unicode`, `webapi`, `wxwidgets`.
- Historical examples: `oldies`.

Likely status: sandbox/reference. Prefer extracting ideas from here rather than assuming production quality.

## Metafor

`metafor/` contains utilities around the Metafor finite element ecosystem.

| Path | Description | Likely status |
| --- | --- | --- |
| `metafor/arbre` | Tree/structure utility with CMake. | reference |
| `metafor/biomec` | Biomechanics-related helper. | reference |
| `metafor/drmeta` | C/C++/Fortran-related Metafor helper with tests. | reference |
| `metafor/integrity` | Integrity checking helper. | reference |
| `metafor/launch_param` | Launch parameter helper. | reference |
| `metafor/mailsph` | Mail/SPH-related utility with CMake. | reference |

## Skeletons And Shared Material

| Path | Description | Likely status |
| --- | --- | --- |
| `skel/cmake` | CMake examples and find-module experiments. | reference |
| `skel/cpp` | Minimal C++ skeleton. | reference |
| `skel/cppswig` | C++/SWIG skeleton. | reference |
| `skel/fortran` | Fortran skeleton. | reference |
| `skel/staticlib` | Static library skeleton with executable and library layout. | reference |
| `snippets/*` | Small reusable fragments for Bash, CBLAS, LaTeX, Pandoc, hello worlds. | reference |
| `pytools` | Shared Python build/version utilities. | reference |
| `externals` | Scripts to obtain external dependencies. | operational |
| `envs` | Environment setup scripts. | operational |

## Unsorted

`unsorted/` contains material that has not yet found a permanent home.

| Path | Description | Suggested destination |
| --- | --- | --- |
| `unsorted/donut` | Donut rendering demo. | `sandbox/graphics` or `sandbox/ascii` |
| `unsorted/flask` | Flask experiment. | `sandbox/webapi` or `sandbox/python` |
| `unsorted/stl_perf` | C++ STL performance experiment with CMake. | `sandbox/cpp11` or `sandbox/perf` |
| `unsorted/*.cpp`, `unsorted/*.f90` | Small standalone experiments. | `snippets/` or matching sandbox |

