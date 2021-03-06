# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2020, Science and Technology Facilities Council.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# -----------------------------------------------------------------------------
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' Performs py.test tests on the Reference PSyIR node. '''

from __future__ import absolute_import
import pytest
from psyclone.psyir.nodes import Reference, Array, Assignment, Literal, \
    BinaryOperation, Range, KernelSchedule
from psyclone.psyir.symbols import DataSymbol, ArrayType, \
    REAL_SINGLE_TYPE, INTEGER_SINGLE_TYPE, REAL_TYPE, INTEGER_TYPE
from psyclone.psyGen import GenerationError
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.tests.utilities import check_links
from psyclone.core.access_info import VariablesAccessInfo


def test_reference_bad_init():
    '''Check that the __init__ method of the Reference class raises the
    expected exception if the symbol argument is not of the right
    type.

    '''
    with pytest.raises(TypeError) as excinfo:
        _ = Reference("hello")
    assert ("In Reference initialisation expecting a symbol but found 'str'."
            in str(excinfo.value))


def test_reference_node_str():
    ''' Check the node_str method of the Reference class.'''
    from psyclone.psyir.nodes.node import colored, SCHEDULE_COLOUR_MAP
    kschedule = KernelSchedule("kname")
    symbol = DataSymbol("rname", INTEGER_SINGLE_TYPE)
    kschedule.symbol_table.add(symbol)
    assignment = Assignment(parent=kschedule)
    ref = Reference(symbol, assignment)
    coloredtext = colored("Reference", SCHEDULE_COLOUR_MAP["Reference"])
    assert coloredtext+"[name:'rname']" in ref.node_str()


def test_reference_can_be_printed():
    '''Test that a Reference instance can always be printed (i.e. is
    initialised fully)'''
    kschedule = KernelSchedule("kname")
    symbol = DataSymbol("rname", INTEGER_SINGLE_TYPE)
    kschedule.symbol_table.add(symbol)
    assignment = Assignment(parent=kschedule)
    ref = Reference(symbol, assignment)
    assert "Reference[name:'rname']" in str(ref)


def test_reference_optional_parent():
    '''Test that the parent attribute is None if the optional parent
    argument is not supplied.

    '''
    ref = Reference(DataSymbol("rname", REAL_SINGLE_TYPE))
    assert ref.parent is None


def test_reference_children_validation():
    '''Test that children added to Reference are validated. A Reference node
    does not accept any children.

    '''
    ref = Reference(DataSymbol("rname", REAL_SINGLE_TYPE))
    with pytest.raises(GenerationError) as excinfo:
        ref.addchild(Literal("2", INTEGER_SINGLE_TYPE))
    assert ("Item 'Literal' can't be child 0 of 'Reference'. Reference is a"
            " LeafNode and doesn't accept children.") in str(excinfo.value)

# Test Array class


def test_array_node_str():
    ''' Check the node_str method of the Array class.'''
    from psyclone.psyir.nodes.node import colored, SCHEDULE_COLOUR_MAP
    kschedule = KernelSchedule("kname")
    array_type = ArrayType(INTEGER_SINGLE_TYPE, [ArrayType.Extent.ATTRIBUTE])
    symbol = DataSymbol("aname", array_type)
    kschedule.symbol_table.add(symbol)
    assignment = Assignment(parent=kschedule)
    array = Array(symbol, parent=assignment)
    coloredtext = colored("ArrayReference", SCHEDULE_COLOUR_MAP["Reference"])
    assert coloredtext+"[name:'aname']" in array.node_str()


def test_array_can_be_printed():
    '''Test that an Array instance can always be printed (i.e. is
    initialised fully)'''
    kschedule = KernelSchedule("kname")
    symbol = DataSymbol("aname", INTEGER_SINGLE_TYPE)
    kschedule.symbol_table.add(symbol)
    assignment = Assignment(parent=kschedule)
    array = Array(symbol, assignment)
    assert "ArrayReference[name:'aname']\n" in str(array)


def test_array_create():
    '''Test that the create method in the Array class correctly
    creates an Array instance.

    '''
    array_type = ArrayType(REAL_SINGLE_TYPE, [10, 10, 10])
    symbol_temp = DataSymbol("temp", array_type)
    symbol_i = DataSymbol("i", INTEGER_SINGLE_TYPE)
    symbol_j = DataSymbol("j", INTEGER_SINGLE_TYPE)
    children = [Reference(symbol_i), Reference(symbol_j),
                Literal("1", INTEGER_SINGLE_TYPE)]
    array = Array.create(symbol_temp, children)
    check_links(array, children)
    result = FortranWriter().array_node(array)
    assert result == "temp(i,j,1)"


def test_array_create_invalid1():
    '''Test that the create method in the Array class raises an exception
    if the provided symbol is not an array.

    '''
    symbol_i = DataSymbol("i", INTEGER_SINGLE_TYPE)
    symbol_j = DataSymbol("j", INTEGER_SINGLE_TYPE)
    symbol_temp = DataSymbol("temp", REAL_SINGLE_TYPE)
    children = [Reference(symbol_i), Reference(symbol_j),
                Literal("1", INTEGER_SINGLE_TYPE)]
    with pytest.raises(GenerationError) as excinfo:
        _ = Array.create(symbol_temp, children)
    assert ("expecting the symbol to be an array, not a scalar."
            in str(excinfo.value))


def test_array_create_invalid2():
    '''Test that the create method in the Array class raises an exception
    if the number of dimension in the provided symbol is different to
    the number of indices provided to the create method.

    '''
    array_type = ArrayType(REAL_SINGLE_TYPE, [10])
    symbol_temp = DataSymbol("temp", array_type)
    symbol_i = DataSymbol("i", INTEGER_SINGLE_TYPE)
    symbol_j = DataSymbol("j", INTEGER_SINGLE_TYPE)
    children = [Reference(symbol_i), Reference(symbol_j),
                Literal("1", INTEGER_SINGLE_TYPE)]
    with pytest.raises(GenerationError) as excinfo:
        _ = Array.create(symbol_temp, children)
    assert ("the symbol should have the same number of dimensions as indices "
            "(provided in the 'children' argument). Expecting '3' but found "
            "'1'." in str(excinfo.value))


def test_array_create_invalid3():
    '''Test that the create method in an Array class raises the expected
    exception if the provided input is invalid.

    '''
    # symbol argument is not a DataSymbol
    with pytest.raises(GenerationError) as excinfo:
        _ = Array.create([], [])
    assert ("symbol argument in create method of Array class should "
            "be a DataSymbol but found 'list'."
            in str(excinfo.value))

    # children not a list
    with pytest.raises(GenerationError) as excinfo:
        _ = Array.create(DataSymbol("temp", REAL_SINGLE_TYPE), "invalid")
    assert ("children argument in create method of Array class should "
            "be a list but found 'str'." in str(excinfo.value))


def test_array_children_validation():
    '''Test that children added to Array are validated. Array accepts
    DataNodes and Range children.'''
    array_type = ArrayType(REAL_SINGLE_TYPE, shape=[5, 5])
    array = Array(DataSymbol("rname", array_type))
    datanode1 = Literal("1", INTEGER_SINGLE_TYPE)
    erange = Range()
    assignment = Assignment()

    # Invalid child
    with pytest.raises(GenerationError) as excinfo:
        array.addchild(assignment)
    assert ("Item 'Assignment' can't be child 0 of 'ArrayReference'. The valid"
            " format is: '[DataNode | Range]*'." in str(excinfo.value))

    # Valid children
    array.addchild(datanode1)
    array.addchild(erange)


def test_array_validate_index():
    '''Test that the validate_index utility function behaves as expected.'''
    array = Array.create(DataSymbol("test", ArrayType(REAL_TYPE, [10])),
                         [Literal("1", INTEGER_TYPE)])
    with pytest.raises(TypeError) as info:
        array._validate_index("hello")
    assert ("The index argument should be an integer but found 'str'."
            in str(info.value))

    with pytest.raises(ValueError) as info:
        array._validate_index(1)
    assert ("In Array 'test' the specified index '1' must be less than the "
            "number of dimensions '1'." in str(info.value))

    array._validate_index(0)
    array._validate_index(-1)


def test_array_is_lower_bound():
    '''Test that the is_lower_bound method in the Array Node works as
    expected.

    '''
    one = Literal("1", INTEGER_TYPE)
    array = Array.create(DataSymbol("test", ArrayType(REAL_TYPE, [10])),
                         [one])
    with pytest.raises(TypeError) as info:
        array.is_lower_bound("hello")
    assert ("The index argument should be an integer but found 'str'."
            in str(info.value))

    # not a range node at index 0
    assert not array.is_lower_bound(0)

    # range node does not have a binary operator for its start value
    array.children[0] = Range.create(one, one, one)
    assert not array.is_lower_bound(0)

    # range node lbound references a different array
    array2 = Array.create(DataSymbol("test2", ArrayType(REAL_TYPE, [10])),
                          [one])
    operator = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND, array2,
        Literal("1", INTEGER_TYPE))
    array.children[0] = Range.create(operator, one, one)
    assert not array.is_lower_bound(0)

    # range node lbound references a different index
    operator = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND, array,
        Literal("2", INTEGER_TYPE))
    array.children[0] = Range.create(operator, one, one)
    assert not array.is_lower_bound(0)

    # all is well
    operator = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND, array, one)
    array.children[0] = Range.create(operator, one, one)
    assert array.is_lower_bound(0)


def test_array_is_upper_bound():
    '''Test that the is_upper_bound method in the Array Node works as
    expected.

    '''
    one = Literal("1", INTEGER_TYPE)
    array = Array.create(DataSymbol("test", ArrayType(REAL_TYPE, [10])),
                         [one])
    with pytest.raises(TypeError) as info:
        array.is_upper_bound("hello")
    assert ("The index argument should be an integer but found 'str'."
            in str(info.value))

    # not a range node at index 0
    assert not array.is_upper_bound(0)

    # range node does not have a binary operator for its stop value
    array.children[0] = Range.create(one, one, one)
    assert not array.is_upper_bound(0)

    # range node ubound references a different array
    array2 = Array.create(DataSymbol("test2", ArrayType(REAL_TYPE, [10])),
                          [one])
    operator = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND, array2, one)
    array.children[0] = Range.create(one, operator, one)
    assert not array.is_upper_bound(0)

    # range node ubound references a different index
    operator = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND, array,
        Literal("2", INTEGER_TYPE))
    array.children[0] = Range.create(one, operator, one)
    assert not array.is_upper_bound(0)

    # all is well
    operator = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND, array, one)
    array.children[0] = Range.create(one, operator, one)
    assert array.is_upper_bound(0)


def test_array_is_full_range():
    '''Test that the is_full_range method in the Array Node works as
    expected. '''
    # pylint: disable=too-many-statements
    zero = Literal("0", INTEGER_SINGLE_TYPE)
    one = Literal("1", INTEGER_SINGLE_TYPE)
    array_type = ArrayType(REAL_SINGLE_TYPE, [10])
    symbol = DataSymbol("my_array", array_type)
    reference = Reference(symbol)
    lbound = BinaryOperation.create(BinaryOperation.Operator.LBOUND,
                                    reference, one)
    ubound = BinaryOperation.create(BinaryOperation.Operator.UBOUND,
                                    reference, one)
    symbol_error = DataSymbol("another_array", array_type)
    reference_error = Reference(symbol_error)

    # Index out of bounds
    array_reference = Array.create(symbol, [one])
    with pytest.raises(ValueError) as excinfo:
        array_reference.is_full_range(1)
    assert ("In Array 'my_array' the specified index '1' must be less than "
            "the number of dimensions '1'." in str(excinfo.value))

    # Array dimension is not a Range
    assert not array_reference.is_full_range(0)

    # Check LBOUND
    # Array dimension range lower bound is not a binary operation
    my_range = Range.create(one, one, one)
    array_reference = Array.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range lower bound is not an LBOUND binary operation
    my_range = Range.create(ubound, one, one)
    array_reference = Array.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range lower bound is an LBOUND binary operation
    # with the first value not being a reference
    lbound_error = BinaryOperation.create(BinaryOperation.Operator.LBOUND,
                                          zero, zero)
    my_range = Range.create(lbound_error, one, one)
    array_reference = Array.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range lower bound is an LBOUND binary operation
    # with the first value being a reference to a different symbol
    lbound_error = BinaryOperation.create(BinaryOperation.Operator.LBOUND,
                                          reference_error, zero)
    my_range = Range.create(lbound_error, one, one)
    array_reference = Array.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range lower bound is an LBOUND binary operation
    # with the second value not being a literal.
    lbound_error = BinaryOperation.create(BinaryOperation.Operator.LBOUND,
                                          reference, reference)
    my_range = Range.create(lbound_error, one, one)
    array_reference = Array.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range lower bound is an LBOUND binary operation
    # with the second value not being an integer literal.
    lbound_error = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND, reference,
        Literal("1.0", REAL_SINGLE_TYPE))
    my_range = Range.create(lbound_error, one, one)
    array_reference = Array.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range lower bound is an LBOUND binary operation
    # with the second value being an integer literal with the wrong
    # value (should be 0 as this dimension index is 0).
    lbound_error = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND, reference, one)
    my_range = Range.create(lbound_error, one, one)
    array_reference = Array.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Check UBOUND
    # Array dimension range upper bound is not a binary operation
    my_range = Range.create(lbound, one, one)
    array_reference = Array.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range upper bound is not a UBOUND binary operation
    my_range = Range.create(lbound, lbound, one)
    array_reference = Array.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range upper bound is a UBOUND binary operation
    # with the first value not being a reference
    ubound_error = BinaryOperation.create(BinaryOperation.Operator.UBOUND,
                                          zero, zero)
    my_range = Range.create(lbound, ubound_error, one)
    array_reference = Array.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range upper bound is a UBOUND binary operation
    # with the first value being a reference to a different symbol
    ubound_error = BinaryOperation.create(BinaryOperation.Operator.UBOUND,
                                          reference_error, zero)
    my_range = Range.create(lbound, ubound_error, one)
    array_reference = Array.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range upper bound is a UBOUND binary operation
    # with the second value not being a literal.
    ubound_error = BinaryOperation.create(BinaryOperation.Operator.UBOUND,
                                          reference, reference)
    my_range = Range.create(lbound, ubound_error, one)
    array_reference = Array.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range upper bound is a UBOUND binary operation
    # with the second value not being an integer literal.
    ubound_error = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND, reference,
        Literal("1.0", REAL_SINGLE_TYPE))
    my_range = Range.create(lbound, ubound_error, one)
    array_reference = Array.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range upper bound is a UBOUND binary operation
    # with the second value being an integer literal with the wrong
    # value (should be 1 as this dimension is 1).
    ubound_error = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND, reference, zero)
    my_range = Range.create(lbound, ubound_error, one)
    array_reference = Array.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Check Step
    # Array dimension range step is not a literal.
    my_range = Range.create(lbound, ubound, lbound)
    array_reference = Array.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range step is not an integer literal.
    my_range = Range.create(lbound, ubound, one)
    # We have to change this to a non-integer manually as the create
    # function only accepts integer literals for the step argument.
    my_range.children[2] = Literal("1.0", REAL_SINGLE_TYPE)
    array_reference = Array.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range step is is an integer literal with the
    # wrong value (not 1).
    my_range = Range.create(lbound, ubound, zero)
    array_reference = Array.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # All is as it should be.
    # The full range is covered so return true.
    my_range = Range.create(lbound, ubound, one)
    array_reference = Array.create(symbol, [my_range])
    assert array_reference.is_full_range(0)


def test_reference_accesses():
    '''Test that the reference_accesses method behaves as expected in the
    usual case (see the next test for the unusual case).

    '''
    reference = Reference(DataSymbol("test", REAL_TYPE))
    var_access_info = VariablesAccessInfo()
    reference.reference_accesses(var_access_info)
    assert (str(var_access_info)) == "test: READ"


@pytest.mark.parametrize("operator_type", [BinaryOperation.Operator.LBOUND,
                                           BinaryOperation.Operator.UBOUND])
def test_reference_accesses_bounds(operator_type):
    '''Test that the reference_accesses method behaves as expected when
    the reference is the first argument to either the lbound or ubound
    intrinsic as that is simply looking up the array bounds (therefore
    var_access_info should be empty) and when the reference is the
    second argument of either the lbound or ubound intrinsic (in which
    case the access should be a read).

    '''
    # Note, one would usually expect UBOUND to provide the upper bound
    # of a range but to simplify the test both LBOUND and UBOUND are
    # used for the lower bound. This does not affect the test.
    one = Literal("1", INTEGER_TYPE)
    array_symbol = DataSymbol("test", ArrayType(REAL_TYPE, [10]))
    array_ref1 = Reference(array_symbol)
    array_ref2 = Reference(array_symbol)
    array_access = Array.create(array_symbol, [one])

    # test when first or second argument to LBOUND or UBOUND is an
    # array reference
    operator = BinaryOperation.create(operator_type, array_ref1, array_ref2)
    array_access.children[0] = Range.create(operator, one, one)
    var_access_info = VariablesAccessInfo()
    array_ref1.reference_accesses(var_access_info)
    assert str(var_access_info) == ""
    var_access_info = VariablesAccessInfo()
    array_ref2.reference_accesses(var_access_info)
    assert str(var_access_info) == "test: READ"
