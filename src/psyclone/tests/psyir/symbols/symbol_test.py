# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council.
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
# Author S. Siso, STFC Daresbury Lab
# Modified by R. W. Ford, STFC Daresbury Lab
#             A. R. Porter, STFC Daresbury Lab
# -----------------------------------------------------------------------------

'''Perform py.test tests on the psygen.psyir.symbols.symbol module.

Note that SymbolError is declared but not used in symbol.py. There are
many other files that use SymbolError and have associated tests so it
is not tested here.

'''


from __future__ import absolute_import
import pytest
from psyclone.psyir.symbols import Symbol, LocalInterface, GlobalInterface, \
                                   ArgumentInterface, UnresolvedInterface, \
                                   ContainerSymbol, DataSymbol, SymbolError, \
                                   SymbolTable, INTEGER_SINGLE_TYPE
from psyclone.psyir.symbols.symbol import SymbolInterface
from psyclone.psyir.nodes import Container, Literal, KernelSchedule


def test_symbol_initialisation():
    '''Test that a Symbol instance can be created when valid arguments are
    given, otherwise raise relevant exceptions. Also tests the
    internal Visibility class, the name, visibility and interface properties.

    '''
    sym = Symbol("sym1")
    assert isinstance(sym, Symbol)
    assert sym.name == "sym1"
    assert sym.visibility == Symbol.DEFAULT_VISIBILITY
    assert isinstance(sym.interface, LocalInterface)
    # Check that the default visibility is public
    assert Symbol.DEFAULT_VISIBILITY == Symbol.Visibility.PUBLIC

    sym = Symbol("sym2", Symbol.Visibility.PRIVATE)
    assert sym.visibility == Symbol.Visibility.PRIVATE

    sym = Symbol("sym3", interface=UnresolvedInterface())
    assert isinstance(sym.interface, UnresolvedInterface)

    with pytest.raises(TypeError) as error:
        sym = Symbol(None)
    assert ("Symbol 'name' attribute should be of type 'str'"
            in str(error.value))

    with pytest.raises(TypeError) as error:
        Symbol('sym1', visibility="hello")
    assert ("Symbol 'visibility' attribute should be of type "
            "psyir.symbols.Symbol.Visibility but" in str(error.value))

    with pytest.raises(TypeError) as error:
        Symbol('sym1', interface="hello")
    assert ("The interface to a Symbol must be a SymbolInterface but got "
            "'str'" in str(error.value))


def test_symbol_interface_setter():
    '''Test that the Symbol interface setter behaves as expected,
    including raising an exception if the input is of the wrong
    type. Also use this to test the is_local, is_global and
    is_argument and is_unresolved properties.

    '''
    symbol = Symbol('sym1')
    assert symbol.is_local
    assert not symbol.is_global
    assert not symbol.is_argument
    assert not symbol.is_unresolved

    symbol.interface = GlobalInterface(ContainerSymbol("my_mod"))
    assert not symbol.is_local
    assert symbol.is_global
    assert not symbol.is_argument
    assert not symbol.is_unresolved

    symbol.interface = ArgumentInterface()
    assert not symbol.is_local
    assert not symbol.is_global
    assert symbol.is_argument
    assert not symbol.is_unresolved

    symbol.interface = UnresolvedInterface()
    assert not symbol.is_local
    assert not symbol.is_global
    assert not symbol.is_argument
    assert symbol.is_unresolved

    with pytest.raises(TypeError) as info:
        symbol.interface = "hello"
    assert ("The interface to a Symbol must be a SymbolInterface but got "
            "'str'" in str(info.value))


def test_symbol_str():
    '''Test that a Symbol instance can be stringified'''

    sym = Symbol("my_symbol")
    assert str(sym) == "my_symbol"


def test_symbolinterface():
    '''Test we can create a SymbolInterface instance. This does nothing so
    needs no further testing.

    '''
    _ = SymbolInterface()


def test_localinterface():
    '''Test we can create a LocalInterface instance and check its __str__
    value

    '''
    interface = LocalInterface()
    assert str(interface) == "Local"


def test_unresolvedinterface():
    '''Test we can create an UnresolvedInterface instance and check its
    __str__ value

    '''
    interface = UnresolvedInterface()
    assert str(interface) == "Unresolved"


def test_globalinterface():
    '''Test that we can create a global interface successfully, that is
    raises the expected exception if the container_symbol attribute is
    of the wrong type, that the container symbol property and str
    method work as expected.

    '''
    container_symbol = ContainerSymbol("my_mod")
    global_interface = GlobalInterface(container_symbol)
    assert global_interface.container_symbol is container_symbol
    assert str(global_interface) == "Global(container='my_mod')"

    with pytest.raises(TypeError) as info:
        _ = GlobalInterface("hello")
    assert ("Global container_symbol parameter must be of type "
            "ContainerSymbol, but found 'str'." in str(info.value))


def test_argumentinterface_init():
    '''Check that the ArgumentInterface can be created successfully and
    has the expected values. Also checks the access property and that
    an exception is raised if the supplied access value is the wrong
    type.

    '''
    argument_interface = ArgumentInterface()
    assert argument_interface._access == ArgumentInterface.Access.UNKNOWN
    assert argument_interface.access == argument_interface._access
    assert argument_interface._pass_by_value is False

    argument_interface = ArgumentInterface(ArgumentInterface.Access.READ)
    assert argument_interface._access == ArgumentInterface.Access.READ
    assert argument_interface.access == argument_interface._access

    with pytest.raises(TypeError) as info:
        _ = ArgumentInterface("hello")
    assert ("SymbolInterface.access must be an 'ArgumentInterface.Access' but "
            "got 'str'." in str(info.value))
    with pytest.raises(TypeError) as info:
        argument_interface.access = "hello"
    assert ("SymbolInterface.access must be an 'ArgumentInterface.Access' but "
            "got 'str'." in str(info.value))


@pytest.mark.parametrize("access", [ArgumentInterface.Access.READ,
                                    ArgumentInterface.Access.WRITE,
                                    ArgumentInterface.Access.READWRITE,
                                    ArgumentInterface.Access.UNKNOWN])
def test_argumentinterface_access_values(access):
    '''Check that all the ArgumentInterface access values are supported.

    '''
    argument_interface = ArgumentInterface()
    argument_interface.access = access
    assert argument_interface.access == access


def test_argumentinterface_str():
    '''Test that an ArgumentInterface instance can be stringified'''

    argument_interface = ArgumentInterface()
    assert str(argument_interface) == "Argument(pass-by-value=False)"


def test_find_symbol_table():
    ''' Test the find_symbol_table() method. '''
    sym = Symbol("a_var")
    with pytest.raises(TypeError) as err:
        sym.find_symbol_table("3")
    assert ("expected to be passed an instance of psyir.nodes.Node but got "
            "'str'" in str(err.value))
    # Search for a SymbolTable with only one level of hierarchy
    sched = KernelSchedule("dummy")
    table = sched.symbol_table
    table.add(sym)
    assert sym.find_symbol_table(sched) is table
    # Create a Container so that we have two levels of hierarchy
    ctable = SymbolTable()
    sym2 = Symbol("b_var")
    ctable.add(sym2)
    _ = Container.create("test", ctable, [sched])
    assert sym2.find_symbol_table(sched) is ctable
    # A Symbol that isn't in any table
    sym3 = Symbol("missing")
    assert sym3.find_symbol_table(sched) is None
    # When there is no SymbolTable associated with the PSyIR node
    orphan = Literal("1", INTEGER_SINGLE_TYPE)
    assert sym3.find_symbol_table(orphan) is None


def test_symbol_copy():
    '''
    Test the Symbol.copy() method.
    '''
    csym = ContainerSymbol("some_mod")
    asym = Symbol("a", visibility=Symbol.Visibility.PRIVATE,
                  interface=GlobalInterface(csym))
    new_sym = asym.copy()
    assert new_sym is not asym
    assert new_sym.name == asym.name
    assert new_sym.interface == asym.interface
    assert new_sym.visibility == asym.visibility


def test_get_external_symbol(monkeypatch):
    ''' Test the get_external_symbol() method. '''
    asym = Symbol("a")
    with pytest.raises(NotImplementedError) as err:
        asym.get_external_symbol()
    assert ("trying to resolve symbol 'a' properties, the lazy evaluation "
            "of 'Local' interfaces is not supported" in str(err.value))
    other_container = ContainerSymbol("some_mod")
    ctable = SymbolTable()
    ctable.add(other_container)
    # Create a Symbol that is imported from the "some_mod" Container
    bsym = Symbol("b", interface=GlobalInterface(other_container))
    ctable.add(bsym)
    _ = Container.create("test", ctable, [KernelSchedule("dummy")])
    # Monkeypatch the container's FortranModuleInterface so that it always
    # appears to be unable to find the "some_mod" module

    def fake_import(name):
        raise SymbolError("Oh dear")
    monkeypatch.setattr(other_container._interface, "import_container",
                        fake_import)
    with pytest.raises(SymbolError) as err:
        bsym.get_external_symbol()
    assert ("trying to resolve the properties of symbol 'b' in module "
            "'some_mod': PSyclone SymbolTable error: Oh dear" in
            str(err.value))
    # Now create a Container for the 'some_mod' module and attach this to
    # the ContainerSymbol
    ctable2 = SymbolTable()
    some_mod = Container.create("some_mod", ctable2,
                                [KernelSchedule("dummy2")])
    other_container._reference = some_mod
    # Currently the Container does not contain an entry for 'b'
    with pytest.raises(SymbolError) as err:
        bsym.get_external_symbol()
    assert ("trying to resolve the properties of symbol 'b'. The interface "
            "points to module 'some_mod' but could not find the definition" in
            str(err.value))
    # Add an entry for 'b' to the Container's symbol table
    ctable2.add(DataSymbol("b", INTEGER_SINGLE_TYPE))
    new_sym = bsym.resolve_deferred()
    assert isinstance(new_sym, DataSymbol)
    assert new_sym.datatype == INTEGER_SINGLE_TYPE


def test_symbol_resolve_deferred(monkeypatch):
    ''' Test the resolve_deferred method. '''
    # resolve_deferred() for a local symbol has nothing to do so should
    # just return itself.
    asym = Symbol("a")
    assert asym.resolve_deferred() is asym
    # Now test for a symbol that is imported from another Container
    other_container = ContainerSymbol("some_mod")
    bsym = Symbol("b", visibility=Symbol.Visibility.PRIVATE,
                  interface=GlobalInterface(other_container))
    # Monkeypatch the get_external_symbol() method so that it just returns
    # a new DataSymbol
    monkeypatch.setattr(bsym, "get_external_symbol",
                        lambda: DataSymbol("b", INTEGER_SINGLE_TYPE))
    new_sym = bsym.resolve_deferred()
    # We should have a brand new symbol but with some of the properties
    # of the original 'bsym' symbol.
    assert new_sym is not bsym
    assert new_sym.datatype == INTEGER_SINGLE_TYPE
    assert new_sym.visibility == Symbol.Visibility.PRIVATE
    assert new_sym.is_global
