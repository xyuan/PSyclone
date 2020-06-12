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
# Modified I. Kavcic, Met Office

'''
This module contains the LFRicArgDescriptor object and related class constants
and properties.
'''

# Imports
from __future__ import print_function, absolute_import
import os
from psyclone.parse.kernel import Descriptor
from psyclone.parse.utils import ParseError
import psyclone.expression as expr
from psyclone.configuration import Config
from psyclone.core.access_type import AccessType
from psyclone.domain.lfric import FunctionSpace
from psyclone.errors import InternalError


class LFRicArgDescriptor(Descriptor):
    '''
    This class captures the information specified in one of LFRic API argument
    descriptors (scalars, fields and operators).

    :param arg_type: Dynamo0.3 argument type (scalar, field or operator)
    :type arg_type: :py:class:`psyclone.expression.FunctionVar` or \
                    :py:class:`psyclone.expression.BinaryOperator`

    :raises ParseError: if a 'meta_arg' entry is not of 'arg_type' type.
    :raises ParseError: if a 'meta_arg' entry has fewer than 2 args.
    :raises ParseError: if an argument type is not one of LFRic (Dynamo0.3) \
                        API valid argument types (scalar, field, operator).
    :raises ParseError: if a field vector notation is not in the correct \
                        format '(field*n)' where 'n' is an integer.
    :raises ParseError: if the field vector notation is used for the vector \
                        size of less than 1.
    :raises ParseError: if the field vector notation uses a wrong separator \
                        operator.
    :raises ParseError: if the field vector notation is used for an \
                        argument that is not a field.
    :raises InternalError: if the argument type checks fail when an invalid \
                           argument type is passed in.
    :raises ParseError: if the second 'meta_arg' entry is not a valid \
                        access descriptor.
    :raises ParseError: if an argument that is not a real scalar has a \
                        reduction access.
    :raises InternalError: if none of the checks caught an invalid argument.

    '''

    # Class constants:
    # - Valid LFRic API datatypes (scalars, fields, operators)
    VALID_SCALAR_NAMES = ["gh_real", "gh_integer"]
    VALID_FIELD_NAMES = ["gh_field"]
    VALID_OPERATOR_NAMES = ["gh_operator", "gh_columnwise_operator"]
    VALID_ARG_TYPE_NAMES = VALID_FIELD_NAMES + VALID_OPERATOR_NAMES + \
        VALID_SCALAR_NAMES

    def __init__(self, arg_type):
        self._arg_type = arg_type
        # Initialise properties
        self._type = None
        self._function_space_to = None
        self._function_space_from = None
        self._function_space = None
        self._function_spaces = []
        # Set vector size to 1 (scalars set it to 0 in their validation)
        self._vector_size = 1
        # Initialise other internal arguments
        self._access_type = None
        self._function_space1 = None
        self._function_space2 = None
        self._stencil = None
        self._mesh = None

        # Check for correct type descriptor
        if arg_type.name != 'arg_type':
            raise ParseError(
                "In the Dynamo0.3 API each 'meta_arg' entry must be of type "
                "'arg_type', but found '{0}'".format(arg_type.name))

        # We require at least 2 args
        if len(arg_type.args) < 2:
            raise ParseError(
                "In the Dynamo0.3 API each 'meta_arg' entry must have at "
                "least 2 args, but found '{0}'".format(len(arg_type.args)))

        # Check the first argument descriptor. If it is a binary operator
        # then it has to be a field vector with an "*n" appended. If it is
        # a variable then it can be other allowed type of argument.
        if isinstance(arg_type.args[0], expr.BinaryOperator):
            # We expect 'field_type * n' to have been specified
            argtype = arg_type.args[0].toks[0].name
            operator = arg_type.args[0].toks[1]
            # First check for a valid argument type...
            if argtype not in LFRicArgDescriptor.VALID_ARG_TYPE_NAMES:
                raise ParseError(
                    "In the Dynamo0.3 API the 1st argument of a 'meta_arg' "
                    "entry should be a valid argument type (one of {0}), but "
                    "found '{1}' in '{2}'".
                    format(LFRicArgDescriptor.VALID_ARG_TYPE_NAMES,
                           self._type, arg_type))
            # ... and set it if the check passes
            self._type = argtype

            # Now try to find the vector size for a field vector and return
            # an error if it is not an integer number...
            try:
                vectsize = int(arg_type.args[0].toks[2])
            except TypeError:
                raise ParseError(
                    "In the Dynamo0.3 API field vector notation expects the "
                    "format '(field*n)' where 'n' is an integer, but the "
                    "following '{0}' was found in '{1}'.".
                    format(str(arg_type.args[0].toks[2]), arg_type))
            # ... or it is less than 2 (1 is the default for all fields)...
            if vectsize < 2:
                raise ParseError(
                    "In the Dynamo0.3 API the 1st argument of a 'meta_arg' "
                    "entry may be a vector but if so must contain a valid "
                    "integer vector size in the format '(field*n, "
                    "where n > 1)', but found n = {0} in '{1}'".
                    format(vectsize, arg_type))
            # ... and set the vector size if all checks pass
            self._vector_size = vectsize

            # Check that the separator operator is correct
            if operator != "*":
                raise ParseError(
                    "In the Dynamo0.3 API the 1st argument of a 'meta_arg' "
                    "entry may be a vector but if so must use '*' as the "
                    "separator in the format '(field*n)', but found '{0}' "
                    "in '{1}'".format(operator, arg_type))

            # Check than no other arguments than fields use vector notation
            if self._type not in \
               LFRicArgDescriptor.VALID_FIELD_NAMES and self._vector_size:
                raise ParseError(
                    "In the Dynamo0.3 API vector notation is only supported "
                    "for a {0} argument type but found '{1}'".
                    format(LFRicArgDescriptor.VALID_FIELD_NAMES,
                           arg_type.args[0]))

        elif isinstance(arg_type.args[0], expr.FunctionVar):
            # We expect 'arg_type' to have been specified
            if arg_type.args[0].name not in \
               LFRicArgDescriptor.VALID_ARG_TYPE_NAMES:
                raise ParseError(
                    "In the Dynamo0.3 API the 1st argument of a 'meta_arg' "
                    "entry should be a valid argument type (one of {0}), "
                    "but found '{1}' in '{2}'".
                    format(LFRicArgDescriptor.VALID_ARG_TYPE_NAMES,
                           arg_type.args[0].name, arg_type))
            self._type = arg_type.args[0].name
        else:
            raise InternalError(
                "LFRicArgDescriptor.__init__(): invalid argument type after "
                "checks, should not get to here")

        # The 2nd arg is an access descriptor (TODO: 3rd in case of scalar)
        # Convert from GH_* names to the generic access type:
        api_config = Config.get().api_conf("dynamo0.3")
        access_mapping = api_config.get_access_mapping()
        try:
            self._access_type = access_mapping[arg_type.args[1].name]
        except KeyError:
            valid_names = api_config.get_valid_accesses_api()
            raise ParseError(
                "In the Dynamo0.3 API the 2nd argument of a 'meta_arg' entry "
                "must be a valid access descriptor (one of {0}), but found "
                "'{1}' in '{2}'".format(valid_names,
                                        arg_type.args[1].name, arg_type))

        # Reduction access descriptors are only valid for real scalar arguments
        if self._type != "gh_real" and self._access_type in \
           AccessType.get_valid_reduction_modes():
            raise ParseError(
                "In the Dynamo0.3 API a reduction access '{0}' is only valid "
                "with a real scalar argument, but '{1}' was found".
                format(self._access_type.api_specific_name(),
                       self._type))

        # FIELD, OPERATOR and SCALAR argument types descriptors and checks
        # Fields
        if self._type in LFRicArgDescriptor.VALID_FIELD_NAMES:
            self._validate_field(arg_type)

        # Operators
        elif self._type in LFRicArgDescriptor.VALID_OPERATOR_NAMES:
            self._validate_operator(arg_type)

        # Scalars
        elif self._type in LFRicArgDescriptor.VALID_SCALAR_NAMES:
            self._validate_scalar(arg_type)

        # We should never get to here (#TODO: check if tested)
        else:
            raise InternalError(
                "LFRicArgDescriptor.__init__(): failed argument validation, "
                "should not get to here")

        # Initialise the parent class
        super(LFRicArgDescriptor,
              self).__init__(self._access_type, self._function_space1,
                             stencil=self._stencil, mesh=self._mesh)

    def _validate_field(self, arg_type):
        '''
        Validates argument descriptors for field arguments and
        populates argument properties accordingly.

        :param arg_type: Dynamo0.3 field (vector) argument type
        :type arg_type: :py:class:`psyclone.expression.FunctionVar` or \
                        :py:class:`psyclone.expression.BinaryOperator`

        :raises InternalError: if argument type other than a field is \
                               passed in.
        :raises ParseError: if there are fewer than 3 metadata arguments.
        :raises ParseError: if there are more than 4 metadata arguments.
        :raises ParseError: if the 3rd argument is not one of valid \
                            function spaces.
        :raises ParseError: if the optional 4th argument is not a stencil \
                            specification or a mesh identifier (for \
                            inter-grid kernels).
        :raises ParseError: if a field on a discontinuous function space \
                            has 'gh_inc' access.
        :raises ParseError: if a field on a continuous function space has \
                            'gh_readwrite' access.
        :raises ParseError: if a field on 'any_space' function space has \
                            'gh_readwrite' access.
        :raises ParseError: if a field stencil argument is not read-only.

        '''
        # Check whether something other than a field is passed in
        if self._type not in LFRicArgDescriptor.VALID_FIELD_NAMES:
            raise InternalError(
                "LFRicArgDescriptor._validate_field(): expecting a field "
                "argument but got an argument of type '{0}'. Should be "
                "impossible.".format(arg_type.args[0]))

        # There must be at least 3 arguments
        if len(arg_type.args) < 3:
            raise ParseError(
                "In the Dynamo0.3 API each 'meta_arg' entry must have at "
                "least 3 arguments if its first argument is of a {0} type, "
                "but found {1} in '{2}'".
                format(LFRicArgDescriptor.VALID_FIELD_NAMES,
                       len(arg_type.args), arg_type))
        # There must be at most 4 arguments
        if len(arg_type.args) > 4:
            raise ParseError(
                "In the Dynamo0.3 API each 'meta_arg' entry must have at "
                "most 4 arguments if its first argument is of a {0} type, "
                "but found {1} in '{2}'".
                format(LFRicArgDescriptor.VALID_FIELD_NAMES,
                       len(arg_type.args), arg_type))

        # The 3rd argument must be a valid function space name
        if arg_type.args[2].name not in \
           FunctionSpace.VALID_FUNCTION_SPACE_NAMES:
            raise ParseError(
                "In the Dynamo0.3 API the 3rd argument of a 'meta_arg' "
                "entry must be a valid function space name (one of {0}) if "
                "its first argument is of a {1} type, but found '{2}' in "
                "'{3}".format(FunctionSpace.VALID_FUNCTION_SPACE_NAMES,
                              LFRicArgDescriptor.VALID_FIELD_NAMES,
                              arg_type.args[2].name, arg_type))
        self._function_space1 = arg_type.args[2].name

        # The optional 4th argument is either a stencil specification
        # or a mesh identifier (for inter-grid kernels)
        if len(arg_type.args) == 4:
            try:
                from psyclone.parse.kernel import get_stencil, get_mesh
                if "stencil" in str(arg_type.args[3]):
                    self._stencil = get_stencil(arg_type.args[3],
                                                VALID_STENCIL_TYPES)
                elif "mesh" in str(arg_type.args[3]):
                    self._mesh = get_mesh(arg_type.args[3], VALID_MESH_TYPES)
                else:
                    raise ParseError("Unrecognised metadata entry")
            except ParseError as err:
                raise ParseError(
                    "In the Dynamo0.3 API the 4th argument of a 'meta_arg' "
                    "field entry must be either a valid stencil specification"
                    "or a mesh identifier (for inter-grid kernels). However, "
                    "entry '{0}' raised the following error: {1}".
                    format(arg_type, str(err)))

        # Test allowed accesses for fields
        if self._function_space1.lower() in \
           FunctionSpace.VALID_DISCONTINUOUS_NAMES \
           and self._access_type == AccessType.INC:
            raise ParseError(
                "It does not make sense for a field on a discontinuous "
                "space ('{0}') to have a 'gh_inc' access".
                format(self._function_space1.lower()))
        if self._function_space1.lower() in \
           FunctionSpace.CONTINUOUS_FUNCTION_SPACES \
           and self._access_type == AccessType.READWRITE:
            raise ParseError(
                "It does not make sense for a field on a continuous "
                "space ('{0}') to have a 'gh_readwrite' access".
                format(self._function_space1.lower()))
        # TODO: extend restriction to "gh_write" for kernels that loop
        # over cells (issue #138) and update access rules for kernels
        # (built-ins) that loop over DoFs to accesses for discontinuous
        # quantities (issue #471)
        if self._function_space1.lower() in \
           FunctionSpace.VALID_ANY_SPACE_NAMES \
           and self._access_type == AccessType.READWRITE:
            raise ParseError(
                "In the Dynamo0.3 API a field on 'any_space' cannot have "
                "'gh_readwrite' access because it is treated as continuous")
        if self._stencil and self._access_type != AccessType.READ:
            raise ParseError("A stencil must be read-only so its access "
                             "should be 'gh_read'")

    def _validate_operator(self, arg_type):
        '''
        Validates argument descriptors for operator arguments and
        populates argument properties accordingly.

        :param arg_type: Dynamo0.3 operator argument type
        :type arg_type: :py:class:`psyclone.expression.FunctionVar`

        :raises InternalError: if argument type other than an operator is \
                               passed in.
        :raises ParseError: if there are not exactly 4 metadata arguments.
        :raises ParseError: if the function space to- is not one of the \
                            valid function spaces.
        :raises ParseError: if the function space from- is not one of the \
                            valid function spaces.
        :raises ParseError: if the operator argument has an invalid access.

        '''
        # Check whether something other than an operator is passed in
        if self._type not in LFRicArgDescriptor.VALID_OPERATOR_NAMES:
            raise InternalError(
                "LFRicArgDescriptor._validate_operator(): expecting an "
                "operator argument but got an argument of type '{0}'. "
                "Should be impossible.".format(self._type))

        # We expect 4 arguments with the 3rd and 4th each being a
        # function space
        if len(arg_type.args) != 4:
            raise ParseError(
                "In the Dynamo0.3 API each 'meta_arg' entry must have 4 "
                "arguments if its first argument is one of {0}, but found "
                "{1} in '{2}'".
                format(LFRicArgDescriptor.VALID_OPERATOR_NAMES,
                       len(arg_type.args), arg_type))

        # Operator arguments need to have valid to- and from- function spaces
        if arg_type.args[2].name not in \
           FunctionSpace.VALID_FUNCTION_SPACE_NAMES:
            raise ParseError(
                "In the Dynamo0.3 API the 3rd argument of a 'meta_arg' "
                "operator entry must be a valid function space name (one of "
                "{0}), but found '{1}' in '{2}".
                format(FunctionSpace.VALID_FUNCTION_SPACE_NAMES,
                       arg_type.args[2].name, arg_type))
        self._function_space1 = arg_type.args[2].name
        if arg_type.args[3].name not in \
           FunctionSpace.VALID_FUNCTION_SPACE_NAMES:
            raise ParseError(
                "In the Dynamo0.3 API the 4th argument of a 'meta_arg' "
                "operator entry must be a valid function space name (one of "
                "{0}), but found '{1}' in '{2}".
                format(FunctionSpace.VALID_FUNCTION_SPACE_NAMES,
                       arg_type.args[3].name, arg_type))
        self._function_space2 = arg_type.args[3].name

        # Test allowed accesses for operators
        if self._access_type == AccessType.INC:
            raise ParseError(
                "In the Dynamo0.3 API operators cannot have a 'gh_inc' "
                "access because they behave as discontinuous quantities")

    def _validate_scalar(self, arg_type):
        '''
        Validates argument descriptors for scalar arguments and
        populates argument properties accordingly.

        :param arg_type: Dynamo0.3 scalar argument type
        :type arg_type: :py:class:`psyclone.expression.FunctionVar`

        :raises InternalError: if argument type other than a scalar is \
                               passed in.
        :raises ParseError: if there are not exactly 2 metadata arguments.
        :raises ParseError: if scalar arguments do not have a read-only or
                            a reduction access.

        '''
        # Check whether something other than a scalar is passed in
        if self._type not in LFRicArgDescriptor.VALID_SCALAR_NAMES:
            raise InternalError(
                "LFRicArgDescriptor._validate_scalar(): expecting a scalar "
                "argument but got an argument of type '{0}'. Should be "
                "impossible.".format(arg_type.args[0]))

        # There must be at least 2 arguments to describe a scalar
        if len(arg_type.args) != 2:
            raise ParseError(
                "In the Dynamo0.3 API each 'meta_arg' entry must have 2 "
                "arguments if its first argument is 'gh_{{r,i}}scalar', but "
                "found {0} in '{1}'".format(len(arg_type.args), arg_type))

        # Test allowed accesses for scalars (read_only or reduction)
        if self._access_type not in [AccessType.READ] + \
           AccessType.get_valid_reduction_modes():
            api_config = Config.get().api_conf("dynamo0.3")
            rev_access_mapping = api_config.get_reverse_access_mapping()
            api_specific_name = rev_access_mapping[self._access_type]
            valid_reductions = AccessType.get_valid_reduction_names()
            raise ParseError(
                "In the Dynamo0.3 API scalar arguments must be "
                "read-only ('gh_read') or a reduction ({0}) but found "
                "'{1}' in '{2}'".format(valid_reductions, api_specific_name,
                                        arg_type))

        # Scalars don't have vector size
        self._vector_size = 0

    @property
    def type(self):
        '''
        Returns the type of the argument (gh_field, gh_operator, ...).

        :returns: type of the argument.
        :rtype: str

        '''
        return self._type

    @property
    def function_space_to(self):
        '''
        Returns the "to" function space for an operator. This is
        the first function space specified in the metadata.

        :returns: "to" function space for an operator.
        :rtype: str

        :raises RuntimeError: if this is not an operator.

        '''
        if self._type in LFRicArgDescriptor.VALID_OPERATOR_NAMES:
            return self._function_space1
        raise RuntimeError(
            "In the Dynamo0.3 API 'function_space_to' only makes sense "
            "for one of {0}, but this is a '{1}'".
            format(LFRicArgDescriptor.VALID_OPERATOR_NAMES, self._type))

    @property
    def function_space_from(self):
        '''
        Returns the "from" function space for an operator. This is
        the second function space specified in the metadata.

        :returns: "from" function space for an operator.
        :rtype: str

        :raises RuntimeError: if this is not an operator.

        '''
        if self._type in LFRicArgDescriptor.VALID_OPERATOR_NAMES:
            return self._function_space2
        raise RuntimeError(
            "In the Dynamo0.3 API 'function_space_from' only makes sense "
            "for one of {0}, but this is a '{1}'".
            format(LFRicArgDescriptor.VALID_OPERATOR_NAMES, self._type))

    @property
    def function_space(self):
        '''
        Returns the function space name that this instance operates on
        depending on the argument type: a single function space for a field,
        function_space_from for an operator and nothing for a scalar.

        :returns: function space than an argument instance operates on.
        :rtype: str

        :raises InternalError: if an invalid argument type is passed in.

        '''
        if self._type in LFRicArgDescriptor.VALID_FIELD_NAMES:
            return self._function_space1
        if self._type in LFRicArgDescriptor.VALID_OPERATOR_NAMES:
            return self._function_space2
        if self._type in LFRicArgDescriptor.VALID_SCALAR_NAMES:
            return None
        raise InternalError("LFRicArgDescriptor.function_space(), should "
                            "not get to here.")

    @property
    def function_spaces(self):
        '''
        Returns the function space names that this instance operates on as
        a list depending on the argument type: one function space for a
        field, both function spaces for an operator and an empty list for
        a scalar.

        :returns: function spaces than an argument instance operates on.
        :rtype: list of str

        :raises InternalError: if an invalid argument type is passed in.

        '''
        if self._type in LFRicArgDescriptor.VALID_FIELD_NAMES:
            return [self.function_space]
        if self._type in LFRicArgDescriptor.VALID_OPERATOR_NAMES:
            # return to before from to maintain expected ordering
            return [self.function_space_to, self.function_space_from]
        if self._type in LFRicArgDescriptor.VALID_SCALAR_NAMES:
            return []
        raise InternalError("LFRicArgDescriptor.function_spaces(), should "
                            "not get to here.")

    @property
    def vector_size(self):
        '''
        Returns the vector size of the argument. This will be 1 if *n
        has not been specified for all arguments except scalars (their
        vector size is set to 0).

        :returns: vector size of the argument.
        :rtype: str

        '''
        return self._vector_size

    def __str__(self):
        '''
        Creates a string representation of the argument descriptor. This
        is type and access for scalars with the addition of function
        space(s) for fields and operators.

        :returns: string representation of the argument descriptor.
        :rtype: str

        :raises InternalError: if an invalid argument type is passed in.

        '''
        res = "LFRicArgDescriptor object" + os.linesep
        res += "  argument_type[0]='{0}'".format(self._type)
        if self._vector_size > 1:
            res += "*"+str(self._vector_size)
        res += os.linesep
        res += "  access_descriptor[1]='{0}'"\
               .format(self._access_type.api_specific_name())\
               + os.linesep
        if self._type in LFRicArgDescriptor.VALID_FIELD_NAMES:
            res += "  function_space[2]='{0}'".format(self._function_space1) \
                   + os.linesep
        elif self._type in LFRicArgDescriptor.VALID_OPERATOR_NAMES:
            res += "  function_space_to[2]='{0}'".\
                   format(self._function_space1) + os.linesep
            res += "  function_space_from[3]='{0}'".\
                   format(self._function_space2) + os.linesep
        elif self._type in LFRicArgDescriptor.VALID_SCALAR_NAMES:
            pass  # we have nothing to add if we're a scalar
        else:  # we should never get to here
            raise InternalError("LFRicArgDescriptor.__str__(), should not "
                                "get to here.")
        return res