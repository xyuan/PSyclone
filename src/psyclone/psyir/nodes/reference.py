# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2020, Science and Technology Facilities Council.
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

''' This module contains the implementation of the Reference and Array
nodes.'''

from __future__ import absolute_import
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.nodes import Operation, BinaryOperation, Literal
from psyclone.psyir.nodes.ranges import Range
from psyclone.core.access_info import AccessType
from psyclone.psyir.symbols import Symbol, DataSymbol, ScalarType
from psyclone.errors import GenerationError


class Reference(DataNode):
    '''
    Node representing a Reference Expression.

    :param symbol: the symbol being referenced.
    :type symbol: :py:class:`psyclone.psyir.symbols.Symbol`
    :param parent: the parent node of this Reference in the PSyIR.
    :type parent: :py:class:`psyclone.psyir.nodes.Node` or NoneType

    :raises TypeError: if the symbol argument is not of type Symbol.

    '''
    # Textual description of the node.
    _children_valid_format = "<LeafNode>"
    _text_name = "Reference"
    _colour_key = "Reference"

    def __init__(self, symbol, parent=None):
        if not isinstance(symbol, Symbol):
            raise TypeError("In Reference initialisation expecting a symbol "
                            "but found '{0}'.".format(type(symbol).__name__))
        super(Reference, self).__init__(parent=parent)
        self._symbol = symbol

    @property
    def symbol(self):
        ''' Return the referenced symbol.

        :returns: the referenced symbol.
        :rtype: :py:class:`psyclone.psyir.symbols.Symbol`

        '''
        return self._symbol

    @property
    def name(self):
        ''' Return the name of the referenced symbol.

        :returns: Name of the referenced symbol.
        :rtype: str

        '''
        return self._symbol.name

    def node_str(self, colour=True):
        ''' Create a text description of this node in the schedule, optionally
        including control codes for colour.

        :param bool colour: whether or not to include colour control codes.

        :return: text description of this node.
        :rtype: str
        '''
        return self.coloured_name(colour) + "[name:'" + self.name + "']"

    def __str__(self):
        return self.node_str(False)

    def math_equal(self, other):
        ''':param other: the node to compare self with.
        :type other: py:class:`psyclone.psyir.nodes.Node`

        :returns: True if the self has the same results as other.
        :rtype: bool
        '''
        if not super(Reference, self).math_equal(other):
            return False
        return self.name == other.name

    def reference_accesses(self, var_accesses):
        '''Get all variable access information from this node, i.e.
        it sets this variable to be read.

        :param var_accesses: VariablesAccessInfo instance that stores the \
            information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        if (self.parent and isinstance(self.parent, Operation) and
                self.parent.operator in [BinaryOperation.Operator.LBOUND,
                                         BinaryOperation.Operator.UBOUND] and
                self.parent.children.index(self) == 0):
            # This reference is the first argument to a lbound or
            # ubound intrinsic. These intrinsics do not access the
            # array elements, they determine the array
            # bounds. Therefore there is no data dependence.
            return
        var_accesses.add_access(self.name, AccessType.READ, self)


class Array(Reference):
    '''
    Node representing an Array reference. As such it has a reference and a
    subscript list as children 0 and 1, respectively.

    '''
    # Textual description of the node.
    _children_valid_format = "[DataNode | Range]*"
    _text_name = "ArrayReference"

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        # pylint: disable=unused-argument
        return isinstance(child, (DataNode, Range))

    @staticmethod
    def create(symbol, children):
        '''Create an Array instance given a symbol and a list of Node
        array indices.

        :param symbol: the symbol that this array is associated with.
        :type symbol: :py:class:`psyclone.psyir.symbols.DataSymbol`
        :param children: a list of Nodes describing the array indices.
        :type children: list of :py:class:`psyclone.psyir.nodes.Node`

        :returns: an Array instance.
        :rtype: :py:class:`psyclone.psyir.nodes.Array`

        :raises GenerationError: if the arguments to the create method \
            are not of the expected type.

        '''
        if not isinstance(symbol, DataSymbol):
            raise GenerationError(
                "symbol argument in create method of Array class should be a "
                "DataSymbol but found '{0}'.".format(type(symbol).__name__))
        if not isinstance(children, list):
            raise GenerationError(
                "children argument in create method of Array class "
                "should be a list but found '{0}'."
                "".format(type(children).__name__))
        if not symbol.is_array:
            raise GenerationError(
                "expecting the symbol to be an array, not a scalar.")
        if len(symbol.shape) != len(children):
            raise GenerationError(
                "the symbol should have the same number of dimensions as "
                "indices (provided in the 'children' argument). "
                "Expecting '{0}' but found '{1}'.".format(
                    len(children), len(symbol.shape)))

        array = Array(symbol)
        array.children = children
        for child in children:
            child.parent = array
        return array

    def __str__(self):
        result = "Array" + super(Array, self).__str__() + "\n"
        for entity in self._children:
            result += str(entity) + "\n"
        return result

    def reference_accesses(self, var_accesses):
        '''Get all variable access information. All variables used as indices
        in the access of the array will be added as READ.

        :param var_accesses: variable access information.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`
        '''

        # This will set the array-name as READ
        super(Array, self).reference_accesses(var_accesses)

        # Now add all children: Note that the class Reference
        # does not recurse to the children (which store the indices), so at
        # this stage no index information has been stored:
        list_indices = []
        for child in self._children:
            child.reference_accesses(var_accesses)
            list_indices.append(child)

        if list_indices:
            var_info = var_accesses[self.name]
            # The last entry in all_accesses is the one added above
            # in super(Array...). Add the indices to that entry.
            var_info.all_accesses[-1].indices = list_indices

    def _validate_index(self, index):
        '''Utility function that checks that the supplied index is an integer
        and is less than the number of array dimensions.

        :param int index: the array index to check.

        :raises TypeError: if the index argument is not an integer.
        :raises ValueError: if the index value is greater than the \
            number of dimensions in the array (-1).

        '''
        if not isinstance(index, int):
            raise TypeError(
                "The index argument should be an integer but found '{0}'."
                "".format(type(index).__name__))
        if index > len(self.children)-1:
            raise ValueError(
                "In Array '{0}' the specified index '{1}' must be less than "
                "the number of dimensions '{2}'."
                "".format(self.name, index, len(self.children)))

    def is_lower_bound(self, index):
        '''Returns True if the specified array index contains a Range node
        which has a starting value given by the 'LBOUND(name,index)'
        intrinsic where 'name' is the name of the current Array and
        'index' matches the specified array index. Otherwise False is
        returned.

        For example, if a Fortran array A was declared as
        A(10) then the starting value is 1 and LBOUND(A,1) would
        return that value.

        :param int index: the array index to check.

        :returns: True if the array index is a range with its start \
            value being LBOUND(array,index) and False otherwise.
        :rtype: bool

        '''
        self._validate_index(index)

        array_dimension = self.children[index]
        if not isinstance(array_dimension, Range):
            return False

        lower = array_dimension.children[0]
        if not (isinstance(lower, BinaryOperation) and
                lower.operator == BinaryOperation.Operator.LBOUND):
            return False
        if not (isinstance(lower.children[0], Reference) and
                lower.children[0].name == self.name):
            return False
        if not (isinstance(lower.children[1], Literal) and
                lower.children[1].datatype.intrinsic ==
                ScalarType.Intrinsic.INTEGER
                and lower.children[1].value == str(index+1)):
            return False
        return True

    def is_upper_bound(self, index):
        '''Returns True if the specified array index contains a Range node
        which has a stopping value given by the 'UBOUND(name,index)'
        intrinsic where 'name' is the name of the current Array and
        'index' matches the specified array index. Otherwise False is
        returned.

        For example, if a Fortran array A was declared as
        A(10) then the stopping value is 10 and UBOUND(A,1) would
        return that value.

        :param int index: the array index to check.

        :returns: True if the array index is a range with its stop \
            value being UBOUND(array,index) and False otherwise.
        :rtype: bool

        '''
        self._validate_index(index)

        array_dimension = self.children[index]
        if not isinstance(array_dimension, Range):
            return False

        upper = array_dimension.children[1]
        if not (isinstance(upper, BinaryOperation) and
                upper.operator == BinaryOperation.Operator.UBOUND):
            return False
        if not (isinstance(upper.children[0], Reference) and
                upper.children[0].name == self.name):
            return False
        if not (isinstance(upper.children[1], Literal) and
                upper.children[1].datatype.intrinsic ==
                ScalarType.Intrinsic.INTEGER
                and upper.children[1].value == str(index+1)):
            return False
        return True

    def is_full_range(self, index):
        '''Returns True if the specified array index is a Range Node that
        specifies all elements in this index. In the PSyIR this is
        specified by using LBOUND(name,index) for the lower bound of
        the range, UBOUND(name,index) for the upper bound of the range
        and "1" for the range step.

        :param int index: the array index to check.

        :returns: True if the access to this array index is a range \
            that specifies all index elements. Otherwise returns \
            False.
        :rtype: bool

        '''
        self._validate_index(index)

        array_dimension = self.children[index]
        if isinstance(array_dimension, Range):
            if self.is_lower_bound(index) and self.is_upper_bound(index):
                step = array_dimension.children[2]
                if (isinstance(step, Literal) and
                        step.datatype.intrinsic == ScalarType.Intrinsic.INTEGER
                        and step.value == "1"):
                    return True
        return False
