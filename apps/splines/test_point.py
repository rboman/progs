import pytest

from point import Pt


def test_constructor_converts_values_to_float():
    p = Pt(1, 2, 3)

    assert p.x == 1.0
    assert p.y == 2.0
    assert p.z == 3.0


def test_string_representation():
    assert str(Pt(1, 2, 3)) == "(1.0,2.0,3.0)"


def test_addition():
    p = Pt(1, 2, 3) + Pt(4, 5, 6)

    assert p.x == 5.0
    assert p.y == 7.0
    assert p.z == 9.0


def test_subtraction():
    p = Pt(4, 5, 6) - Pt(1, 2, 3)

    assert p.x == 3.0
    assert p.y == 3.0
    assert p.z == 3.0


def test_scalar_multiplication():
    p = Pt(1, 2, 3) * 2

    assert p.x == 2.0
    assert p.y == 4.0
    assert p.z == 6.0


def test_right_scalar_multiplication():
    p = 2 * Pt(1, 2, 3)

    assert p.x == 2.0
    assert p.y == 4.0
    assert p.z == 6.0


def test_dot_product():
    value = Pt(1, 2, 3) * Pt(4, 5, 6)

    assert value == 32.0


def test_true_division():
    p = Pt(3, 6, 9) / 3

    assert p.x == 1.0
    assert p.y == 2.0
    assert p.z == 3.0


def test_abs_returns_euclidean_norm():
    assert abs(Pt(3, 4, 0)) == pytest.approx(5.0)


def test_normalized_returns_unit_vector():
    p = Pt(3, 4, 0).normalized()

    assert p.x == pytest.approx(0.6)
    assert p.y == pytest.approx(0.8)
    assert p.z == pytest.approx(0.0)
    assert abs(p) == pytest.approx(1.0)


def test_normalized_zero_vector_raises_value_error():
    with pytest.raises(ValueError, match="zero-length vector"):
        Pt(0, 0, 0).normalized()


def test_copy_returns_distinct_object():
    p1 = Pt(1, 2, 3)
    p2 = p1.copy()

    assert p2 is not p1
    assert p2.x == p1.x
    assert p2.y == p1.y
    assert p2.z == p1.z


def test_update_min_mutates_components():
    p = Pt(5, 5, 5)
    p.update_min(Pt(6, 4, 7))

    assert p.x == 5.0
    assert p.y == 4.0
    assert p.z == 5.0


def test_update_max_mutates_components():
    p = Pt(5, 5, 5)
    p.update_max(Pt(6, 4, 7))

    assert p.x == 6.0
    assert p.y == 5.0
    assert p.z == 7.0


def test_componentwise_comparisons():
    p1 = Pt(1, 2, 3)
    p2 = Pt(2, 3, 4)

    assert p1 < p2
    assert p2 > p1
    assert p1 <= p2
    assert p2 >= p1


def test_invalid_addition_raises_type_error():
    with pytest.raises(TypeError):
        Pt(1, 2, 3) + 1


def test_invalid_multiplication_raises_type_error():
    with pytest.raises(TypeError):
        Pt(1, 2, 3) * "bad"
