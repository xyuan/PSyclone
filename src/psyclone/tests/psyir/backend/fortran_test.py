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
# Author R. W. Ford, STFC Daresbury Lab
# Modified by A. R. Porter and S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

'''Performs pytest tests on the psyclone.psyir.backend.fortran module'''

from __future__ import absolute_import

from collections import OrderedDict
import pytest
from fparser.common.readfortran import FortranStringReader
from psyclone.psyir.backend.visitor import VisitorError
from psyclone.psyir.backend.fortran import gen_intent, gen_dims, \
    FortranWriter, gen_datatype, get_fortran_operator, _reverse_map, \
    is_fortran_intrinsic, precedence
from psyclone.psyir.nodes import Node, CodeBlock, Container, Literal, \
    UnaryOperation, BinaryOperation, NaryOperation, Reference, Call, \
    KernelSchedule
from psyclone.psyir.symbols import DataSymbol, SymbolTable, ContainerSymbol, \
    GlobalInterface, ArgumentInterface, UnresolvedInterface, ScalarType, \
    ArrayType, INTEGER_TYPE, REAL_TYPE, CHARACTER_TYPE, BOOLEAN_TYPE, \
    DeferredType, RoutineSymbol, Symbol
from psyclone.tests.utilities import create_schedule
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.errors import InternalError
from psyclone.tests.utilities import Compile


@pytest.fixture(scope="function", name="fort_writer")
def fixture_fort_writer():
    '''Create and return a FortranWriter object with default settings.'''
    return FortranWriter()


def test_gen_intent():
    '''Check the gen_intent function produces the expected intent
    strings.

    '''
    symbol = DataSymbol("dummy", INTEGER_TYPE,
                        interface=ArgumentInterface(
                            ArgumentInterface.Access.UNKNOWN))
    assert gen_intent(symbol) is None
    symbol = DataSymbol("dummy", INTEGER_TYPE,
                        interface=ArgumentInterface(
                            ArgumentInterface.Access.READ))
    assert gen_intent(symbol) == "in"
    symbol = DataSymbol("dummy", INTEGER_TYPE,
                        interface=ArgumentInterface(
                            ArgumentInterface.Access.WRITE))
    assert gen_intent(symbol) == "out"
    symbol = DataSymbol("dummy", INTEGER_TYPE,
                        interface=ArgumentInterface(
                            ArgumentInterface.Access.READWRITE))
    assert gen_intent(symbol) == "inout"


def test_gen_intent_error(monkeypatch):
    '''Check the gen_intent function raises an exception if an unsupported
    access type is found.

    '''
    symbol = DataSymbol("dummy", INTEGER_TYPE,
                        interface=ArgumentInterface(
                            ArgumentInterface.Access.UNKNOWN))
    monkeypatch.setattr(symbol.interface, "_access", "UNSUPPORTED")
    with pytest.raises(VisitorError) as excinfo:
        _ = gen_intent(symbol)
    assert "Unsupported access ''UNSUPPORTED'' found." in str(excinfo.value)


def test_gen_dims():
    '''Check the gen_dims function produces the expected dimension
    strings.

    '''
    arg = DataSymbol("arg", INTEGER_TYPE,
                     interface=ArgumentInterface(
                         ArgumentInterface.Access.UNKNOWN))
    array_type = ArrayType(INTEGER_TYPE, [arg, 2, ArrayType.Extent.ATTRIBUTE])
    symbol = DataSymbol("dummy", array_type,
                        interface=ArgumentInterface(
                            ArgumentInterface.Access.UNKNOWN))
    assert gen_dims(symbol) == ["arg", "2", ":"]


def test_gen_dims_error(monkeypatch):
    '''Check the gen_dims function raises an exception if a symbol shape
    entry is not supported.

    '''
    array_type = ArrayType(INTEGER_TYPE, [10])
    symbol = DataSymbol("dummy", array_type,
                        interface=ArgumentInterface(
                            ArgumentInterface.Access.UNKNOWN))
    monkeypatch.setattr(array_type, "_shape", ["invalid"])
    with pytest.raises(NotImplementedError) as excinfo:
        _ = gen_dims(symbol)
    assert "unsupported gen_dims index 'invalid'" in str(excinfo.value)


@pytest.mark.parametrize(
    "type_name,result",
    [(ScalarType.Intrinsic.REAL, "real"),
     (ScalarType.Intrinsic.INTEGER, "integer"),
     (ScalarType.Intrinsic.CHARACTER, "character"),
     (ScalarType.Intrinsic.BOOLEAN, "logical")])
def test_gen_datatype_default_precision(type_name, result):
    '''Check for all supported datatype names that the gen_datatype
    function produces the expected Fortran types for scalar and arrays
    when no explicit precision is provided.

    Note, in the future PSyclone should be extended to set default
    precision in a config file.

    '''
    scalar_type = ScalarType(type_name, ScalarType.Precision.UNDEFINED)
    array_type = ArrayType(scalar_type, [10, 10])
    for my_type in [scalar_type, array_type]:
        symbol = DataSymbol("dummy", my_type)
        assert gen_datatype(symbol) == result


@pytest.mark.parametrize(
    "type_name,precision,result",
    [(ScalarType.Intrinsic.REAL, ScalarType.Precision.SINGLE, "real"),
     (ScalarType.Intrinsic.REAL, ScalarType.Precision.DOUBLE,
      "double precision"),
     (ScalarType.Intrinsic.INTEGER, ScalarType.Precision.SINGLE, "integer"),
     (ScalarType.Intrinsic.INTEGER, ScalarType.Precision.DOUBLE, "integer"),
     (ScalarType.Intrinsic.CHARACTER, ScalarType.Precision.SINGLE,
      "character"),
     (ScalarType.Intrinsic.CHARACTER, ScalarType.Precision.DOUBLE,
      "character"),
     (ScalarType.Intrinsic.BOOLEAN, ScalarType.Precision.SINGLE, "logical"),
     (ScalarType.Intrinsic.BOOLEAN, ScalarType.Precision.DOUBLE, "logical")])
def test_gen_datatype_relative_precision(type_name, precision, result):
    '''Check for all supported datatype names that the gen_datatype
    function produces the expected Fortran types for scalar and arrays
    when relative precision is provided.

    '''
    scalar_type = ScalarType(type_name, precision=precision)
    array_type = ArrayType(scalar_type, [10, 10])
    for my_type in [scalar_type, array_type]:
        symbol = DataSymbol("dummy", my_type)
        assert gen_datatype(symbol) == result


@pytest.mark.parametrize("precision", [1, 2, 4, 8, 16, 32])
@pytest.mark.parametrize("type_name,fort_name",
                         [(ScalarType.Intrinsic.INTEGER, "integer"),
                          (ScalarType.Intrinsic.BOOLEAN, "logical")])
def test_gen_datatype_absolute_precision(type_name, precision, fort_name):
    '''Check for the integer and logical datatype names that the
    gen_datatype function produces the expected Fortran types for
    scalar and arrays when explicit precision is provided.

    All should pass except 32. Other types are tested separately.

    '''
    symbol_name = "dummy"
    scalar_type = ScalarType(type_name, precision=precision)
    array_type = ArrayType(scalar_type, [10, 10])
    for my_type in [scalar_type, array_type]:
        symbol = DataSymbol(symbol_name, my_type)
        if precision in [32]:
            with pytest.raises(VisitorError) as excinfo:
                gen_datatype(symbol)
            assert ("Datatype '{0}' in symbol '{1}' supports fixed precision "
                    "of [1, 2, 4, 8, 16] but found '{2}'."
                    "".format(fort_name, symbol_name, precision)
                    in str(excinfo.value))
        else:
            assert (gen_datatype(symbol) ==
                    "{0}*{1}".format(fort_name, precision))


@pytest.mark.parametrize(
    "precision", [1, 2, 4, 8, 16, 32])
def test_gen_datatype_absolute_precision_real(precision):
    '''Check for the real datatype name that the gen_datatype function
    produces the expected Fortran types for scalars and arrays when
    explicit precision is provided.

    All should pass except 1, 2 and 32.

    '''
    symbol_name = "dummy"
    scalar_type = ScalarType(ScalarType.Intrinsic.REAL, precision=precision)
    array_type = ArrayType(scalar_type, [10, 10])
    for my_type in [scalar_type, array_type]:
        symbol = DataSymbol(symbol_name, my_type)
        if precision in [1, 2, 32]:
            with pytest.raises(VisitorError) as excinfo:
                gen_datatype(symbol)
            assert ("Datatype 'real' in symbol '{0}' supports fixed precision "
                    "of [4, 8, 16] but found '{1}'."
                    "".format(symbol_name, precision) in str(excinfo.value))
        else:
            assert gen_datatype(symbol) == "real*{0}".format(precision)


def test_gen_datatype_absolute_precision_character():
    '''Check for the character datatype name that the
    gen_datatype function produces the expected Fortran types for
    scalars and arrays when explicit precision is provided.

    '''
    symbol_name = "dummy"
    scalar_type = ScalarType(ScalarType.Intrinsic.CHARACTER, precision=4)
    array_type = ArrayType(scalar_type, [10, 10])
    for my_type in [scalar_type, array_type]:
        symbol = DataSymbol(symbol_name, my_type)
        with pytest.raises(VisitorError) as excinfo:
            gen_datatype(symbol)
        assert ("Explicit precision not supported for datatype '{0}' in "
                "symbol '{1}' in Fortran backend."
                "".format("character", symbol_name) in str(excinfo.value))


@pytest.mark.parametrize(
    "type_name,result",
    [(ScalarType.Intrinsic.REAL, "real"),
     (ScalarType.Intrinsic.INTEGER, "integer"),
     (ScalarType.Intrinsic.CHARACTER, "character"),
     (ScalarType.Intrinsic.BOOLEAN, "logical")])
def test_gen_datatype_kind_precision(type_name, result):
    '''Check for all supported datatype names that the gen_datatype
    function produces the expected Fortran types for scalars and
    arrays when precision is provided via another symbol.

    '''
    precision_name = "prec_def"
    symbol_name = "dummy"
    precision = DataSymbol(precision_name, INTEGER_TYPE)
    scalar_type = ScalarType(type_name, precision=precision)
    array_type = ArrayType(scalar_type, [10, 10])
    for my_type in [scalar_type, array_type]:
        symbol = DataSymbol(symbol_name, my_type)
        if type_name == ScalarType.Intrinsic.CHARACTER:
            with pytest.raises(VisitorError) as excinfo:
                gen_datatype(symbol)
            assert ("kind not supported for datatype '{0}' in symbol '{1}' "
                    "in Fortran backend.".format("character", symbol_name)
                    in str(excinfo.value))
        else:
            assert (gen_datatype(symbol) ==
                    "{0}(kind={1})".format(result, precision_name))


def test_gen_datatype_exception_1():
    '''Check that an exception is raised if gen_datatype is called with a
    symbol containing an unsupported datatype.

    '''
    data_type = ScalarType(ScalarType.Intrinsic.REAL, 4)
    symbol = DataSymbol("fred", data_type)
    symbol.datatype._intrinsic = None
    with pytest.raises(NotImplementedError) as excinfo:
        _ = gen_datatype(symbol)
    assert ("Unsupported datatype 'None' for symbol 'fred' found in "
            "gen_datatype()." in str(excinfo.value))


def test_gen_datatype_exception_2():
    '''Check that an exception is raised if gen_datatype is called with a
    symbol containing an unsupported precision.

    '''
    data_type = ScalarType(ScalarType.Intrinsic.REAL, 4)
    symbol = DataSymbol("fred", data_type)
    symbol.datatype._precision = None
    with pytest.raises(VisitorError) as excinfo:
        _ = gen_datatype(symbol)
    assert ("Unsupported precision type 'NoneType' found for symbol 'fred' "
            "in Fortran backend." in str(excinfo.value))


# Commented this test out until #11 is addressed.
# @pytest.mark.xfail(reason="issue #11 backend logging output is affected by "
#                    "some other part PSyclone.")
# def test_gen_datatype_precision_log(caplog):
#     '''Check the gen_datatype function produces a logging warning if
#     relative precision is specified for a Fortran datatype that does
#     not support relative precision (only real/double precision
#     supports it)
#
#     '''
#     import logging
#     with caplog.at_level(logging.WARNING):
#         symbol = Symbol("dummy", INTEGER_TYPE,
#                         precision=Symbol.Precision.DOUBLE)
#         _ = gen_datatype(symbol)
#         assert (
#             "WARNING  Fortran does not support relative precision for the "
#             "'integer' datatype but 'Precision.DOUBLE' was specified for "
#             "variable 'dummy'." in caplog.text)


def test_reverse_map():
    '''Check that the internal _reverse_map function returns a map with
    the expected behaviour

    '''
    result = _reverse_map(OrderedDict([('+', 'PLUS')]))
    assert isinstance(result, dict)
    assert result['PLUS'] == '+'


def test_reverse_map_duplicates():
    '''Check that the internal _reverse_map function returns a map with
    the expected behaviour when there are duplicates in the items of
    the input ordered dictionary. It should use the first one found.

    '''
    result = _reverse_map(OrderedDict([('==', 'EQUAL'), ('.eq.', 'EQUAL')]))
    assert isinstance(result, dict)
    assert result['EQUAL'] == '=='
    assert len(result) == 1

    result = _reverse_map(OrderedDict([('.eq.', 'EQUAL'), ('==', 'EQUAL')]))
    assert isinstance(result, dict)
    assert result['EQUAL'] == '.eq.'
    assert len(result) == 1


@pytest.mark.parametrize("operator,result",
                         [(UnaryOperation.Operator.SIN, "SIN"),
                          (BinaryOperation.Operator.MIN, "MIN"),
                          (NaryOperation.Operator.SUM, "SUM")])
def test_get_fortran_operator(operator, result):
    '''Check that the get_fortran_operator function returns the expected
    values when provided with valid unary, binary and nary operators.

    '''
    assert result == get_fortran_operator(operator)


def test_get_fortran_operator_error():
    '''Check that the get_fortran_operator function raises the expected
    exception when an unknown operator is provided.

    '''
    with pytest.raises(KeyError):
        _ = get_fortran_operator(None)


def test_is_fortran_intrinsic():
    '''Check that the is_fortran_intrinsic function returns true if the
    supplied operator is a fortran intrinsic and false otherwise.

    '''
    assert is_fortran_intrinsic("SIN")
    assert not is_fortran_intrinsic("+")
    assert not is_fortran_intrinsic(None)


def test_precedence():
    '''Check that the precedence function returns the expected relative
    precedence values.

    '''
    assert precedence('.OR.') < precedence('.AND.')
    assert precedence('*') < precedence('**')
    assert precedence('.EQ.') == precedence('==')
    assert precedence('*') == precedence('/')


def test_precedence_error():
    '''Check that the precedence function returns the expected exception
    if an unknown operator is provided.

    '''
    with pytest.raises(KeyError):
        _ = precedence('invalid')


def test_fw_gen_use(fort_writer):
    '''Check the FortranWriter class gen_use method produces the expected
    declaration. Also check that an exception is raised if the symbol
    does not describe a Container.

    '''
    symbol_table = SymbolTable()
    container_symbol = ContainerSymbol("my_module")
    symbol_table.add(container_symbol)
    symbol = DataSymbol("dummy1", DeferredType(),
                        interface=GlobalInterface(container_symbol))
    symbol_table.add(symbol)
    symbol = RoutineSymbol(
        "my_sub", interface=GlobalInterface(container_symbol))
    symbol_table.add(symbol)
    result = fort_writer.gen_use(container_symbol, symbol_table)
    assert result == "use my_module, only : dummy1, my_sub\n"

    container_symbol.wildcard_import = True
    result = fort_writer.gen_use(container_symbol, symbol_table)
    assert result == ("use my_module, only : dummy1, my_sub\n"
                      "use my_module\n")

    symbol2 = DataSymbol("dummy2", DeferredType(),
                         interface=GlobalInterface(container_symbol))
    symbol_table.add(symbol2)
    result = fort_writer.gen_use(container_symbol, symbol_table)
    assert result == ("use my_module, only : dummy1, dummy2, my_sub\n"
                      "use my_module\n")

    # container2 has no symbols associated with it and has not been marked
    # as having a wildcard import. It should therefore result in a USE
    # with an empty 'ONLY' list (which serves to keep the module in scope
    # while not accessing any data from it).
    container2 = ContainerSymbol("my_mod2")
    symbol_table.add(container2)
    result = fort_writer.gen_use(container2, symbol_table)
    assert result == "use my_mod2, only :\n"
    # If we now add a wildcard import of this module then that's all we
    # should get from the backend (as it makes the "only:" redundant)
    container2.wildcard_import = True
    result = fort_writer.gen_use(container2, symbol_table)
    assert result == "use my_mod2\n"
    # Wrong type for first argument
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer.gen_use(symbol2, symbol_table)
    assert ("expects a ContainerSymbol as its first argument but got "
            "'DataSymbol'" in str(excinfo.value))
    # Wrong type for second argument
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer.gen_use(container2, symbol2)
    assert ("expects a SymbolTable as its second argument but got 'DataSymbol'"
            in str(excinfo.value))
    # Symbol not in SymbolTable
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer.gen_use(ContainerSymbol("my_mod3"), symbol_table)
    assert ("the supplied symbol ('my_mod3') is not in the supplied "
            "SymbolTable" in str(excinfo.value))
    # A different ContainerSymbol with the same name as an entry in the
    # SymbolTable should be picked up
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer.gen_use(ContainerSymbol("my_mod2"), symbol_table)
    assert ("the supplied symbol ('my_mod2') is not the same object as the "
            "entry" in str(excinfo.value))


def test_fw_gen_vardecl(fort_writer):
    '''Check the FortranWriter class gen_vardecl method produces the
    expected declarations. Also check that an exception is raised if
    the symbol does not describe a valid variable declaration statement.

    '''
    # Basic entry
    symbol = DataSymbol("dummy1", INTEGER_TYPE)
    result = fort_writer.gen_vardecl(symbol)
    assert result == "integer :: dummy1\n"

    # Assumed-size array with intent
    array_type = ArrayType(INTEGER_TYPE, [2, 2, ArrayType.Extent.ATTRIBUTE])
    symbol = DataSymbol("dummy2", array_type,
                        interface=ArgumentInterface(
                            ArgumentInterface.Access.READ))
    result = fort_writer.gen_vardecl(symbol)
    assert result == "integer, dimension(2,2,:), intent(in) :: dummy2\n"

    # Assumed-size array with unknown intent
    array_type = ArrayType(INTEGER_TYPE, [2, 2, ArrayType.Extent.ATTRIBUTE])
    symbol = DataSymbol("dummy2", array_type,
                        interface=ArgumentInterface(
                            ArgumentInterface.Access.UNKNOWN))
    result = fort_writer.gen_vardecl(symbol)
    assert result == "integer, dimension(2,2,:) :: dummy2\n"

    # Allocatable array
    array_type = ArrayType(REAL_TYPE, [ArrayType.Extent.DEFERRED,
                                       ArrayType.Extent.DEFERRED])
    symbol = DataSymbol("dummy2", array_type,
                        interface=ArgumentInterface(
                            ArgumentInterface.Access.READWRITE))
    result = fort_writer.gen_vardecl(symbol)
    assert result == \
        "real, allocatable, dimension(:,:), intent(inout) :: dummy2\n"

    # Constant
    symbol = DataSymbol("dummy3", INTEGER_TYPE, constant_value=10)
    result = fort_writer.gen_vardecl(symbol)
    assert result == "integer, parameter :: dummy3 = 10\n"

    # Use statement
    symbol = DataSymbol("dummy1", DeferredType(),
                        interface=GlobalInterface(
                            ContainerSymbol("my_module")))
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer.gen_vardecl(symbol)
    assert ("gen_vardecl requires the symbol 'dummy1' to have a Local or "
            "an Argument interface but found a 'GlobalInterface' interface."
            in str(excinfo.value))

    # An unresolved symbol
    symbol = DataSymbol("dummy1", DeferredType(),
                        interface=UnresolvedInterface())
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer.gen_vardecl(symbol)
    assert ("gen_vardecl requires the symbol 'dummy1' to have a Local or "
            "an Argument interface but found a 'UnresolvedInterface' "
            "interface." in str(excinfo.value))

    # An array with a mixture of deferred and explicit extents
    array_type = ArrayType(INTEGER_TYPE, [2, ArrayType.Extent.DEFERRED])
    symbol = DataSymbol("dummy1", array_type)
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer.gen_vardecl(symbol)
    assert ("Fortran declaration of an allocatable array must have the "
            "extent of every dimension as 'DEFERRED' but symbol 'dummy1' "
            "has shape: [2, " in str(excinfo.value))

    # An assumed-size array must have only the extent of its outermost
    # rank undefined
    array_type = ArrayType(INTEGER_TYPE, [2, ArrayType.Extent.ATTRIBUTE, 2])
    symbol = DataSymbol("dummy1", array_type)
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer.gen_vardecl(symbol)
    assert ("assumed-size Fortran array must only have its last dimension "
            "unspecified (as 'ATTRIBUTE') but symbol 'dummy1' has shape: [2, "
            in str(excinfo.value))
    # With two dimensions unspecified, even though one is outermost
    array_type = ArrayType(INTEGER_TYPE, [2, ArrayType.Extent.ATTRIBUTE,
                                          ArrayType.Extent.ATTRIBUTE])
    symbol = DataSymbol("dummy1", array_type)
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer.gen_vardecl(symbol)
    assert ("assumed-size Fortran array must only have its last dimension "
            "unspecified (as 'ATTRIBUTE') but symbol 'dummy1' has shape: [2, "
            in str(excinfo.value))


def test_gen_decls(fort_writer):
    '''Check the FortranWriter class gen_decls method produces the
    expected declarations. Also check that an exception is raised if
    an 'argument' symbol exists in the supplied symbol table and the
    optional argument 'args_allowed' is set to False.

    '''
    symbol_table = SymbolTable()
    symbol_table.add(ContainerSymbol("my_module"))
    use_statement = DataSymbol("my_use", DeferredType(),
                               interface=GlobalInterface(
                                   symbol_table.lookup("my_module")))
    symbol_table.add(use_statement)
    argument_variable = DataSymbol("arg", INTEGER_TYPE,
                                   interface=ArgumentInterface())
    symbol_table.add(argument_variable)
    local_variable = DataSymbol("local", INTEGER_TYPE)
    symbol_table.add(local_variable)
    result = fort_writer.gen_decls(symbol_table)
    assert (result ==
            "use my_module, only : my_use\n"
            "integer :: arg\n"
            "integer :: local\n")
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer.gen_decls(symbol_table, args_allowed=False)
    assert ("Arguments are not allowed in this context but this symbol table "
            "contains argument(s): '['arg']'." in str(excinfo.value))

    # Add a symbol with a deferred (unknown) interface
    symbol_table.add(DataSymbol("unknown", INTEGER_TYPE,
                                interface=UnresolvedInterface()))
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer.gen_decls(symbol_table)
    assert ("The following symbols are not explicitly declared or imported "
            "from a module (in the local scope) and are not KIND parameters: "
            "'unknown'" in str(excinfo.value))


def test_gen_decls_routine(fort_writer):
    '''Test that the gen_decls method raises an exception if the interface
    of a routine symbol is not a GlobalInterface.

    '''
    symbol_table = SymbolTable()
    symbol_table.add(RoutineSymbol("arg_sub", interface=ArgumentInterface()))
    with pytest.raises(VisitorError) as info:
        _ = fort_writer.gen_decls(symbol_table)
    assert (
        "Routine symbols without a global or local interface are not supported"
        " by the Fortran back-end." in str(info.value))


def test_gen_routine_access_stmts(fort_writer):
    '''
    Tests for the gen_routine_access_stmts method of FortranWriter.
    '''
    symbol_table = SymbolTable()
    symbol_table.add(RoutineSymbol("my_sub1",
                                   visibility=Symbol.Visibility.PUBLIC))
    code = fort_writer.gen_routine_access_stmts(symbol_table)
    assert "public :: my_sub1" in code
    sub2 = RoutineSymbol("my_sub2", visibility=Symbol.Visibility.PRIVATE)
    symbol_table.add(sub2)
    code = fort_writer.gen_routine_access_stmts(symbol_table)
    assert "public :: my_sub1\nprivate :: my_sub2\n" in code
    # Check that the interface of the symbol does not matter
    symbol_table.add(
        RoutineSymbol("used_sub", visibility=Symbol.Visibility.PRIVATE,
                      interface=GlobalInterface(ContainerSymbol("some_mod"))))
    code = fort_writer.gen_routine_access_stmts(symbol_table)
    assert "public :: my_sub1\nprivate :: my_sub2, used_sub\n" in code
    # Break the visibility of the second symbol
    sub2._visibility = "broken"
    with pytest.raises(InternalError) as err:
        fort_writer.gen_routine_access_stmts(symbol_table)
    assert ("Unrecognised visibility ('broken') found for symbol 'my_sub2'"
            in str(err.value))


def test_fw_exception(fort_writer):
    '''Check the FortranWriter class instance raises an exception if an
    unsupported PSyIR node is found.

    '''
    # 'unsupported' should be a node that neither its generic classes nor
    # itself have a visitor implemented.
    unsupported = Node()

    # Generate Fortran from the given PSyIR
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer(unsupported)
    assert "Unsupported node 'Node' found" in str(excinfo.value)


def test_fw_container_1(fort_writer, monkeypatch):
    '''Check the FortranWriter class outputs correct code when a Container
    node with no content is found. Also tests that an exception is
    raised if Container.name does not have a value.

    '''
    container = Container("test")
    result = fort_writer(container)
    assert (
        "module test\n\n"
        "  contains\n\n"
        "end module test\n" in result)

    monkeypatch.setattr(container, "_name", None)
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer(container)
    assert ("Expected Container node name to have a value."
            in str(excinfo.value))


def test_fw_container_2(fort_writer):
    '''Check the FortranWriter class outputs correct code when a Container
    node is found with a subroutine, use statements and
    declarations. Also raise an exception if the Container contains a
    Container.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "use test2_mod, only : a,b\n"
        "real :: c,d\n"
        "contains\n"
        "subroutine tmp()\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")
    container = schedule.root

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(container)

    assert (
        "module test\n"
        "  use test2_mod, only : a, b\n"
        "  real :: c\n"
        "  real :: d\n\n"
        "  public :: tmp\n\n"
        "  contains\n"
        "  subroutine tmp()\n\n\n"
        "  end subroutine tmp\n\n"
        "end module test\n" in result)

    container.children.append(Container("child", parent=container))
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer(container)
    assert ("The Fortran back-end requires all children of a Container "
            "to be KernelSchedules." in str(excinfo.value))


def test_fw_container_3(fort_writer, monkeypatch):
    '''Check the FortranWriter class raises an exception when a Container
    node contains a symbol table with an argument declaration (as this
    does not make sense).

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "real :: a\n"
        "contains\n"
        "subroutine tmp()\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")
    container = schedule.root
    symbol = container.symbol_table.lookup("a")
    monkeypatch.setattr(symbol, "_interface", ArgumentInterface())

    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer(container)
    assert ("Arguments are not allowed in this context but this symbol table "
            "contains argument(s): '['a']'." in str(excinfo.value))


def test_fw_kernelschedule(fort_writer, monkeypatch):
    '''Check the FortranWriter class outputs correct code when a
    KernelSchedule node is found. Also tests that an exception is
    raised if KernelSchedule.name does not have a value.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,b,c)\n"
        "  use my_mod, only : d\n"
        "  real, intent(out) :: a(:)\n"
        "  real, intent(in) :: b(:)\n"
        "  integer, intent(in) :: c\n"
        "  a = b/c\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)

    assert(
        "subroutine tmp(a,b,c)\n"
        "  use my_mod, only : d\n"
        "  real, dimension(:), intent(out) :: a\n"
        "  real, dimension(:), intent(in) :: b\n"
        "  integer, intent(in) :: c\n"
        "\n"
        "  a=b / c\n"
        "\n"
        "end subroutine tmp\n") in result

    monkeypatch.setattr(schedule, "_name", None)
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer(schedule)
    assert "Expected node name to have a value." in str(excinfo.value)

# assignment and binaryoperation (not intrinsics) are already checked
# within previous tests


@pytest.mark.parametrize("binary_intrinsic", ["mod", "max", "min",
                                              "sign"])
def test_fw_binaryoperator(fort_writer, binary_intrinsic, tmpdir):
    '''Check the FortranWriter class binary_operation method correctly
    prints out the Fortran representation of an intrinsic. Tests all
    of the binary operators, apart from sum (as it requires different
    data types so is tested separately) and matmul ( as it requires
    its arguments to be arrays).

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a(n)\n"
        "    a = {0}(1.0,1.0)\n"
        "end subroutine tmp\n"
        "end module test").format(binary_intrinsic)
    schedule = create_schedule(code, "tmp")

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)
    assert "a={0}(1.0, 1.0)".format(binary_intrinsic.upper()) in result
    assert Compile(tmpdir).string_compiles(result)


def test_fw_binaryoperator_sum(fort_writer, tmpdir):
    '''Check the FortranWriter class binary_operation method with the sum
    operator correctly prints out the Fortran representation of an
    intrinsic.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(array,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: array(n)\n"
        "  integer :: a\n"
        "    a = sum(array,dim=1)\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)
    assert "a=SUM(array, dim = 1)" in result
    assert Compile(tmpdir).string_compiles(result)


def test_fw_binaryoperator_matmul(fort_writer, tmpdir):
    '''Check the FortranWriter class binary_operation method with the matmul
    operator correctly prints out the Fortran representation of an
    intrinsic.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,b,c,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(in) :: a(n,n), b(n)\n"
        "  real, intent(out) :: c(n)\n"
        "    c = MATMUL(a,b)\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)
    assert "c=MATMUL(a, b)" in result
    assert Compile(tmpdir).string_compiles(result)


def test_fw_binaryoperator_unknown(fort_writer, monkeypatch):
    '''Check the FortranWriter class binary_operation method raises an
    exception if an unknown binary operator is found.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a(n)\n"
        "    a = sign(1.0,1.0)\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")
    # Remove sign() from the list of supported binary operators
    monkeypatch.delitem(Fparser2Reader.binary_operators, "sign")
    # Generate Fortran from the PSyIR schedule
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer(schedule)
    assert "Unexpected binary op" in str(excinfo.value)


def test_fw_binaryoperator_precedence(fort_writer):
    '''Check the FortranWriter class binary_operation method complies with
    the operator precedence rules. This is achieved by placing the
    operation in brackets.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp()\n"
        "  real :: a, b, c, d\n"
        "  logical :: e, f, g\n"
        "    a = b * (c + d)\n"
        "    a = b * c + d\n"
        "    a = (b * c) + d\n"
        "    a = b * c * d * a\n"
        "    a = (((b * c) * d) * a)\n"
        "    a = (b * (c * (d * a)))\n"
        "    a = -(a + b)\n"
        "    e = .not.(e .and. (f .or. g))\n"
        "    e = (((.not.e) .and. f) .or. g)\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")
    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)
    expected = (
        "  a=b * (c + d)\n"
        "  a=b * c + d\n"
        "  a=b * c + d\n"
        "  a=b * c * d * a\n"
        "  a=b * c * d * a\n"
        "  a=b * (c * (d * a))\n"
        "  a=-(a + b)\n"
        "  e=.NOT.(e .AND. (f .OR. g))\n"
        "  e=.NOT.e .AND. f .OR. g\n")
    assert expected in result


def test_fw_mixed_operator_precedence(fort_writer):
    '''Check the FortranWriter class unary_operation and binary_operation
    methods complies with the operator precedence rules. This is
    achieved by placing the binary operation in brackets.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp()\n"
        "  real :: a, b, c, d\n"
        "  logical :: e, f, g\n"
        "    a = -a * (-b + c)\n"
        "    a = (-a) * (-b + c)\n"
        "    a = -a + (-b + (-c))\n"
        "    e = .not. f .or. .not. g\n"
        "    a = log(b*c)\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")
    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)
    expected = (
        "  a=-a * (-b + c)\n"
        "  a=-a * (-b + c)\n"
        "  a=-a + (-b + -c)\n"
        "  e=.NOT.f .OR. .NOT.g\n"
        "  a=LOG(b * c)\n")
    assert expected in result


def test_fw_naryoperator(fort_writer, tmpdir):
    ''' Check that the FortranWriter class nary_operation method correctly
    prints out the Fortran representation of an intrinsic.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a\n"
        "    a = max(1.0,1.0,2.0)\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)
    assert "a=MAX(1.0, 1.0, 2.0)" in result
    assert Compile(tmpdir).string_compiles(result)


def test_fw_naryoperator_unknown(fort_writer, monkeypatch):
    ''' Check that the FortranWriter class nary_operation method raises
    the expected error if it encounters an unknown operator.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a\n"
        "    a = max(1.0,1.0,2.0)\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")
    # Remove max() from the list of supported nary operators
    monkeypatch.delitem(Fparser2Reader.nary_operators, "max")
    # Generate Fortran from the PSyIR schedule
    with pytest.raises(VisitorError) as err:
        _ = fort_writer(schedule)
    assert "Unexpected N-ary op" in str(err.value)


def test_fw_reference(fort_writer):
    '''Check the FortranWriter class reference method prints the
    appropriate information (the name of the reference it points to).

    '''
    # Generate fparser2 parse tree from Fortran code. The line of
    # interest is a(n) = 0.0. The additional a=1 line is added to get
    # round a bug in the parser.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a(n)\n"
        "    a = 1\n"
        "    a(n) = 0.0\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)

    # The asserts need to be split as the declaration order can change
    # between different versions of Python.
    assert (
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, dimension(n), intent(out) :: a\n"
        "\n"
        "  a=1\n"
        "  a(n)=0.0\n"
        "\n"
        "end subroutine tmp\n") in result


def test_fw_array(fort_writer):
    '''Check the FortranWriter class array method correctly prints
    out the Fortran representation of an array

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a(n,n,n)\n"
        "    a(2,n,3) = 0.0\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)
    assert "a(2,n,3)=0.0" in result


def test_fw_range(fort_writer):
    '''Check the FortranWriter class range_node and array_node methods
    produce the expected code when an array section is specified. We
    can't test the Range node in isolation as one of the checks in the
    Range code requires access to the (Array) parent (to determine the
    array index of a Range node).

    '''
    from psyclone.psyir.nodes import Array, Range
    array_type = ArrayType(REAL_TYPE, [10, 10])
    symbol = DataSymbol("a", array_type)
    dim1_bound_start = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND,
        Reference(symbol),
        Literal("1", INTEGER_TYPE))
    dim1_bound_stop = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND,
        Reference(symbol),
        Literal("1", INTEGER_TYPE))
    dim2_bound_start = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND,
        Reference(symbol),
        Literal("2", INTEGER_TYPE))
    dim3_bound_start = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND,
        Reference(symbol),
        Literal("3", INTEGER_TYPE))
    dim3_bound_stop = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND,
        Reference(symbol),
        Literal("3", INTEGER_TYPE))
    one = Literal("1", INTEGER_TYPE)
    two = Literal("2", INTEGER_TYPE)
    three = Literal("3", INTEGER_TYPE)
    plus = BinaryOperation.create(
        BinaryOperation.Operator.ADD,
        Reference(DataSymbol("b", REAL_TYPE)),
        Reference(DataSymbol("c", REAL_TYPE)))
    array = Array.create(symbol, [Range.create(one, dim1_bound_stop),
                                  Range.create(dim2_bound_start, plus,
                                               step=three)])
    result = fort_writer.array_node(array)
    assert result == "a(1:,:b + c:3)"

    array_type = ArrayType(REAL_TYPE, [10, 10, 10])
    symbol = DataSymbol("a", array_type)
    array = Array.create(
        symbol,
        [Range.create(dim1_bound_start, dim1_bound_stop),
         Range.create(one, two, step=three),
         Range.create(dim3_bound_start, dim3_bound_stop, step=three)])
    result = fort_writer.array_node(array)
    assert result == "a(:,1:2:3,::3)"

    # Make a) lbound and ubound come from a different array and b)
    # switch lbound and ubound round. These bounds should then be
    # output.
    array_type = ArrayType(REAL_TYPE, [10])
    symbol_b = DataSymbol("b", array_type)
    b_dim1_bound_start = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND,
        Reference(symbol_b),
        Literal("1", INTEGER_TYPE))
    b_dim1_bound_stop = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND,
        Reference(symbol_b),
        Literal("1", INTEGER_TYPE))
    array = Array.create(
        symbol,
        [Range.create(b_dim1_bound_start, b_dim1_bound_stop),
         Range.create(one, two, step=three),
         Range.create(dim3_bound_stop, dim3_bound_start, step=three)])
    result = fort_writer.array_node(array)
    assert result == ("a(LBOUND(b, 1):UBOUND(b, 1),1:2:3,"
                      "UBOUND(a, 3):LBOUND(a, 3):3)")

# literal is already checked within previous tests


def test_fw_ifblock(fort_writer):
    '''Check the FortranWriter class ifblock method
    correctly prints out the Fortran representation.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(inout) :: n\n"
        "  real, intent(out) :: a(n)\n"
        "    if (n.gt.2) then\n"
        "      n=n+1\n"
        "    end if\n"
        "    if (n.gt.4) then\n"
        "      a = -1\n"
        "    else\n"
        "      a = 1\n"
        "    end if\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)
    assert (
        "  if (n > 2) then\n"
        "    n=n + 1\n"
        "  end if\n"
        "  if (n > 4) then\n"
        "    a=-1\n"
        "  else\n"
        "    a=1\n"
        "  end if\n") in result


def test_fw_loop(fort_writer):
    '''Check the FortranWriter class loop method
    correctly prints out the Fortran representation.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp()\n"
        "  integer :: i, sum\n"
        "  sum = 0\n"
        "  do i = 1, 20, 2\n"
        "    sum = sum + i\n"
        "  end do\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)
    assert "do i = 1, 20, 2\n" in result


def test_fw_unaryoperator(fort_writer):
    '''Check the FortranWriter class unary_operation method
    correctly prints out the Fortran representation. Uses -1 as the
    example.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a(n)\n"
        "    a = -1\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)
    assert "a=-1" in result


def test_fw_unaryoperator2(fort_writer):
    '''Check the FortranWriter class unary_operation method correctly
    prints out the Fortran representation of an intrinsic. Uses sin as
    the example.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a(n)\n"
        "    a = sin(1.0)\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)
    assert "a=SIN(1.0)" in result


def test_fw_unaryoperator_unknown(fort_writer, monkeypatch):
    '''Check the FortranWriter class unary_operation method raises an
    exception if an unknown unary operator is found.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a(n)\n"
        "    a = sin(1.0)\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")
    # Remove sin() from the dict of unary operators
    monkeypatch.delitem(Fparser2Reader.unary_operators, "sin")
    # Generate Fortran from the PSyIR schedule
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer(schedule)
    assert "Unexpected unary op" in str(excinfo.value)


def test_fw_return(fort_writer):
    '''Check the FortranWriter class return method
    correctly prints out the Fortran representation.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp()\n"
        "  return\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)
    assert "  return\n" in result


def test_fw_codeblock_1(fort_writer):
    '''Check the FortranWriter class codeblock method correctly
    prints out the Fortran code contained within it.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp()\n"
        "  integer :: a\n"
        "  a=1\n"
        "  print *,\"I am a code block\"\n"
        "  print *,\"with more than one line\"\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")
    # Check a code block exists in the schedule
    assert schedule.walk(CodeBlock)
    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)
    assert (
        "  a=1\n"
        "  PRINT *, \"I am a code block\"\n"
        "  PRINT *, \"with more than one line\"\n" in result)


def test_fw_codeblock_2(fort_writer):
    '''Check the FortranWriter class codeblock method correctly prints out
    the Fortran representation when there is a code block that is part
    of a line (not a whole line). In this case the data initialisation
    of the array 'a' "(/ 0.0 /)" is a code block.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp()\n"
        "  real a(1)\n"
        "  a = (/ 0.0 /)\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")

    # Check a code block exists in the schedule
    assert schedule.walk(CodeBlock)

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)
    assert "a=(/0.0/)" in result


def test_fw_codeblock_3(fort_writer):
    '''Check the FortranWriter class codeblock method raises the expected
    exception if an unsupported CodeBlock structure value is found.

    '''
    code_block = CodeBlock([], "unsupported")
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer.codeblock_node(code_block)
    assert ("Unsupported CodeBlock Structure 'unsupported' found."
            in str(excinfo.value))


def get_nemo_schedule(parser, code):
    '''Utility function that returns the first schedule for a code with
    the nemo api.

    :param parser: the parser class.
    :type parser: :py:class:`fparser.two.Fortran2003.Program`
    :param str code: the code as a string.

    :returns: the first schedule in the supplied code.
    :rtype: :py:class:`psyclone.nemo.NemoInvokeSchedule`

    '''
    from psyclone.psyGen import PSyFactory
    reader = FortranStringReader(code)
    prog = parser(reader)
    psy = PSyFactory(api="nemo").create(prog)
    return psy.invokes.invoke_list[0].schedule


def test_fw_nemoinvokeschedule(fort_writer, parser):
    '''Check that the FortranWriter class nemoinvokeschedule accepts the
    NemoInvokeSchedule node and prints the expected code (from any
    children of the node as the node itself simply calls its
    children).

    '''
    from psyclone.nemo import NemoInvokeSchedule
    code = (
        "program test\n"
        "  integer :: a\n"
        "  a=1\n"
        "end program test\n")
    schedule = get_nemo_schedule(parser, code)
    assert isinstance(schedule, NemoInvokeSchedule)
    result = fort_writer(schedule)
    assert "a=1\n" in result


def test_fw_nemokern(fort_writer, parser):
    '''Check the FortranWriter class nemokern method prints the
    class information and calls any children. This method is used to
    output nothing for a NemoKern object and simply call its children
    as NemoKern is a collection of PSyIR nodes so needs no
    output itself.

    '''
    from psyclone.nemo import NemoKern
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "program test\n"
        "  integer :: i, j, k, n\n"
        "  real :: a(n,n,n)\n"
        "  do k=1,n\n"
        "    do j=1,n\n"
        "      do i=1,n\n"
        "        a(i,j,k) = 0.0\n"
        "      end do\n"
        "    end do\n"
        "  end do\n"
        "end program test")
    schedule = get_nemo_schedule(parser, code)

    kernel = schedule[0].loop_body[0].loop_body[0].loop_body[0]
    assert isinstance(kernel, NemoKern)

    result = fort_writer(schedule)
    assert (
        "    do i = 1, n, 1\n"
        "      a(i,j,k)=0.0\n"
        "    enddo\n" in result)


def test_fw_query_intrinsics(fort_writer):
    ''' Check that the FortranWriter outputs SIZE/LBOUND/UBOUND
    intrinsic calls. '''
    code = ("module test_mod\n"
            "contains\n"
            "subroutine test_kern(a)\n"
            "  real, intent(in) :: a(:,:)\n"
            "  integer :: mysize, lb, ub\n"
            "  mysize = size(a, 2)\n"
            "  lb = lbound(a, 2)\n"
            "  ub = ubound(a, 2)\n"
            "end subroutine test_kern\n"
            "end module test_mod\n")
    schedule = create_schedule(code, "test_kern")

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule).lower()
    assert "mysize=size(a, 2)" in result
    assert "lb=lbound(a, 2)" in result
    assert "ub=ubound(a, 2)" in result


def test_fw_literal_node(fort_writer):
    ''' Test the PSyIR literals are converted to the proper Fortran format
    when necessary. '''

    # By default literals are not modified
    lit1 = Literal('a', CHARACTER_TYPE)
    result = fort_writer(lit1)
    assert result == "'a'"

    lit1 = Literal('3.14', REAL_TYPE)
    result = fort_writer(lit1)
    assert result == '3.14'

    # Check that BOOLEANS use the FORTRAN formatting
    lit1 = Literal('true', BOOLEAN_TYPE)
    result = fort_writer(lit1)
    assert result == '.true.'
    lit1 = Literal('false', BOOLEAN_TYPE)
    result = fort_writer(lit1)
    assert result == '.false.'

    # Check precision symbols are output as expected
    precision_symbol = DataSymbol("rdef", INTEGER_TYPE)
    my_type = ScalarType(ScalarType.Intrinsic.REAL, precision_symbol)
    lit1 = Literal("3.14", my_type)
    result = fort_writer(lit1)
    assert result == "3.14_rdef"

    # Check character precision symbols are output as expected
    my_type = ScalarType(ScalarType.Intrinsic.CHARACTER, precision_symbol)
    lit1 = Literal("hello", my_type)
    result = fort_writer(lit1)
    assert result == "rdef_'hello'"

    # Check explicit precision is output as expected
    my_type = ScalarType(ScalarType.Intrinsic.REAL, 4)
    lit1 = Literal("3.14", my_type)
    result = fort_writer(lit1)
    assert result == "3.14_4"

    # Check explicit character precision is output as expected
    my_type = ScalarType(ScalarType.Intrinsic.CHARACTER, 1)
    lit1 = Literal("hello", my_type)
    result = fort_writer(lit1)
    assert result == "1_'hello'"


def test_fw_call_node(fort_writer):
    '''Test the PSyIR call node is translated to the required Fortran
    code.

    '''
    # no args
    routine_symbol = RoutineSymbol("mysub")
    call = Call(routine_symbol, [])
    result = fort_writer(call)
    assert result == "call mysub()\n"

    # simple args
    args = [Reference(DataSymbol("arg1", REAL_TYPE)),
            Reference(DataSymbol("arg2", REAL_TYPE))]
    call = Call.create(routine_symbol, args)
    result = fort_writer(call)
    assert result == "call mysub(arg1, arg2)\n"

    symbol_table = SymbolTable()
    symbol_a = DataSymbol("a", REAL_TYPE)
    symbol_table.add(symbol_a)
    ref_a = Reference(symbol_a)
    symbol_b = DataSymbol("b", REAL_TYPE)
    symbol_table.add(symbol_b)
    ref_b = Reference(symbol_b)
    symbol_use = ContainerSymbol("my_mod")
    symbol_table.add(symbol_use)
    symbol_call = RoutineSymbol(
        "my_sub", interface=GlobalInterface(symbol_use))
    symbol_table.add(symbol_call)
    mult_ab = BinaryOperation.create(
        BinaryOperation.Operator.MUL, ref_a, ref_b)
    max_ab = NaryOperation.create(NaryOperation.Operator.MAX, [ref_a, ref_b])
    call = Call.create(symbol_call, [mult_ab, max_ab])
    schedule = KernelSchedule.create("work", symbol_table, [call])
    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)
    expected = "  call my_sub(a * b, MAX(a, b))\n"
    assert expected in result
