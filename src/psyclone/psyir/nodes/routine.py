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
# Author: A. R. Porter, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains the Routine node implementation.'''

from psyclone.psyir.nodes.schedule import Schedule
from psyclone.psyir.symbols import DataType
from psyclone.psyir.nodes.node import Node
from psyclone.psyir.symbols.symboltable import SymbolTable


class Routine(Schedule):
    '''
    A sub-class of a Schedule that represents a subroutine, function or
    program unit.

    :param str name: the name of this routine.
    :param bool is_program: whether this Routine represents the entry point \
                            into a program (e.g. Fortran Program or C main()).
    :param return_type: the return-type of this routine.
    :type return_type: :py:class:`psyclone.psyir.symbols.DataType` or NoneType
    :param parent: the parent node of this Routine node in the PSyIR.
    :type parent: :py:class:`psyclone.psyir.nodes.Node` or NoneType

    :raises TypeError: if any of the supplied arguments are of the wrong type.

    '''
    # Textual description of the node.
    _children_valid_format = "[Statement]*"
    _text_name = "Routine"
    _colour_key = "Schedule"

    def __init__(self, name, is_program=False, return_type=None, parent=None):
        super(Routine, self).__init__(parent=parent)
        self.name = name

        if not isinstance(is_program, bool):
            raise TypeError("Routine argument 'is_program' must be a bool but "
                            "got '{0}'".format(type(is_program).__name__))
        self._is_program = is_program

        if return_type and not isinstance(return_type, DataType):
            raise TypeError("Routine argument 'return_type' must be of type "
                            "DataType but got '{0}'".format(
                                type(return_type).__name__))
        self._return_type = return_type

    @classmethod
    def create(cls, name, symbol_table, children, is_program=False,
               return_type=None):
        '''Create an instance of the supplied class given a name, a symbol
        table and a list of child nodes. This is implemented as a classmethod
        so that it is able to act as a Factory for subclasses - e.g. it
        will create a KernelSchedule if called from KernelSchedule.create().

        :param str name: the name of the Routine (or subclass).
        :param symbol_table: the symbol table associated with this Routine.
        :type symbol_table: :py:class:`psyclone.psyGen.SymbolTable`
        :param children: a list of PSyIR nodes contained in the Routine.
        :type children: list of :py:class:`psyclone.psyir.nodes.Node`
        :param bool is_program: whether this Routine represents the entry \
            point into a program (i.e. Fortran Program or C main()).
        :param return_type: the return-type of this routine.
        :type return_type: :py:class:`psyclone.psyir.symbols.DataType` or \
            NoneType

        :returns: an instance of `cls`.
        :rtype: :py:class:`psyclone.psyGen.Routine` or subclass

        :raises TypeError: if the arguments to the create method \
            are not of the expected type.

        '''
        if not isinstance(name, str):
            raise TypeError(
                "name argument in create method of Routine class "
                "should be a string but found '{0}'."
                "".format(type(name).__name__))
        if not isinstance(symbol_table, SymbolTable):
            raise TypeError(
                "symbol_table argument in create method of Routine "
                "class should be a SymbolTable but found '{0}'."
                "".format(type(symbol_table).__name__))
        if not isinstance(children, list):
            raise TypeError(
                "children argument in create method of Routine class "
                "should be a list but found '{0}'."
                "".format(type(children).__name__))
        for child in children:
            if not isinstance(child, Node):
                raise TypeError(
                    "child of children argument in create method of "
                    "Routine class should be a PSyIR Node but "
                    "found '{0}'.".format(type(child).__name__))

        kern = cls(name)
        # pylint: disable=protected-access
        kern._is_program = is_program
        kern._return_type = return_type
        kern._symbol_table = symbol_table
        symbol_table._node = kern
        for child in children:
            child.parent = kern
        kern.children = children
        return kern

    def node_str(self, colour=True):
        ''' Returns the name of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        return self.coloured_name(colour) + "[name:'" + self.name + "']"

    @property
    def dag_name(self):
        '''
        :returns: the name of this node in the dag.
        :rtype: str
        '''
        return "_".join(["routine", self.name, str(self.abs_position)])

    @property
    def name(self):
        '''
        :returns: the name of this Routine.
        :rtype: str
        '''
        return self._name

    @name.setter
    def name(self, new_name):
        '''
        Sets a new name for the Routine.

        :param str new_name: new name for the Routine.
        '''
        if not isinstance(new_name, str):
            raise TypeError("Routine name must be a str but got "
                            "'{0}'".format(type(new_name).__name__))
        self._name = new_name

    def __str__(self):
        result = self.node_str(False) + ":\n"
        for entity in self._children:
            result += str(entity) + "\n"
        result += "End " + self.coloured_name(False)
        return result

    @property
    def is_program(self):
        '''
        :returns: whether this Routine represents the entry point into a \
                  program (e.g. is a Fortran Program or a C main()).
        :rtype: bool
        '''
        return self._is_program

    @property
    def return_type(self):
        '''
        :returns: the return type of this Routine.
        :rtype: :py:class:`psyclone.psyir.symbols.DataType` or NoneType
        '''
        return self._return_type


# For automatic documentation generation
__all__ = ["Routine"]
