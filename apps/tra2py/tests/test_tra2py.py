from pathlib import Path
import sys

import pytest

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from tra2py import resolve_tra_path, tra2py


def test_resolve_tra_path_adds_extension_when_missing():
    resolved = resolve_tra_path("PLAFOS4")
    assert resolved.name == "PLAFOS4.TRA"


def test_resolve_tra_path_keeps_extension_when_present():
    resolved = resolve_tra_path("PLAFOS4.TRA")
    assert resolved.name == "PLAFOS4.TRA"


def test_tra2py_extracts_columns_without_plot(tmp_path):
    source = Path(__file__).resolve().parents[1] / "PLAFOS4.TRA"
    output = tmp_path / "extracted.txt"

    returned_output = tra2py(str(source), "ecrou", 1, 2, str(output), do_plot=False)

    assert returned_output == str(output)
    assert output.exists()

    lines = output.read_text(encoding="utf-8").splitlines()
    assert len(lines) > 1000
    assert lines[0] == "0.0 94.9846"


def test_tra2py_raises_on_missing_file(tmp_path):
    missing = tmp_path / "does_not_exist"
    with pytest.raises(FileNotFoundError):
        tra2py(str(missing), "curve", 1, 2, do_plot=False)
