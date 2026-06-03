from __future__ import annotations

import argparse
import platform
import shutil
import subprocess
from pathlib import Path


def run(cmd: list[str], cwd: Path | None = None) -> None:
    print("+", " ".join(cmd))
    completed = subprocess.run(cmd, cwd=cwd)
    if completed.returncode != 0:
        raise SystemExit(completed.returncode)


def find_project_root(start: Path) -> Path:
    current = start.resolve()

    while True:
        if (current / "CMakeLists.txt").is_file():
            return current

        if current.parent == current:
            raise SystemExit(
                "Erreur : aucun CMakeLists.txt trouvé dans le répertoire courant "
                "ou ses parents."
            )

        current = current.parent


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Configure and build a CMake project."
    )

    parser.add_argument(
        "-S",
        "--source",
        type=Path,
        default=None,
        help="Source directory. Default: nearest parent containing CMakeLists.txt.",
    )

    parser.add_argument(
        "-B",
        "--build-dir",
        type=Path,
        default=None,
        help="Build directory. Default: <source>/build.",
    )

    parser.add_argument(
        "-c",
        "--config",
        default="Release",
        help="Build configuration. Default: Release.",
    )

    parser.add_argument(
        "-j",
        "--jobs",
        type=int,
        default=None,
        help="Number of parallel build jobs.",
    )

    parser.add_argument(
        "--clean",
        action="store_true",
        help="Delete the build directory before configuring.",
    )

    parser.add_argument(
        "--test",
        action="store_true",
        help="Run ctest after build.",
    )

    parser.add_argument(
        "--generator",
        default=None,
        help="CMake generator, for example Ninja or Unix Makefiles.",
    )

    args = parser.parse_args()

    source_dir = args.source.resolve() if args.source else find_project_root(Path.cwd())
    build_dir = args.build_dir.resolve() if args.build_dir else source_dir / "build"

    if args.clean and build_dir.exists():
        print(f"Removing {build_dir}")
        shutil.rmtree(build_dir)

    build_dir.mkdir(parents=True, exist_ok=True)

    configure_cmd = [
        "cmake",
        "-S",
        str(source_dir),
        "-B",
        str(build_dir),
    ]

    if args.generator:
        configure_cmd += ["-G", args.generator]

    if platform.system() == "Windows" and args.generator is None:
        configure_cmd += ["-A", "x64"]

    run(configure_cmd)

    build_cmd = [
        "cmake",
        "--build",
        str(build_dir),
        "--config",
        args.config,
    ]

    if args.jobs is not None:
        build_cmd += ["--parallel", str(args.jobs)]

    run(build_cmd)

    if args.test:
        test_cmd = [
            "ctest",
            "--test-dir",
            str(build_dir),
            "--output-on-failure",
            "-C",
            args.config,
        ]
        run(test_cmd)