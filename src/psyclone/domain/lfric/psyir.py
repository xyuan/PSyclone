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
# Author R. W. Ford STFC Daresbury Lab

'''This module generates LFRic-specific PSyIR classes from lists of
definitions.

'''
# pylint: disable=unused-import
# pylint: disable=exec-used
from collections import namedtuple
from psyclone.psyir.symbols import ContainerSymbol, DataSymbol, DeferredType, \
    GlobalInterface, ScalarType, ArrayType

# Define LFRic module symbols.

# The first Module named-tuple argument specifies the name of the
# module and the second argument declares the name(s) of any symbols
# declared by the module.

Module = namedtuple('Module', ["name", "vars"])
modules = [
    Module("constants_mod", ["i_def", "r_def", "l_def"])]

# Generate LFRic module symbols from definitions
for module in modules:
    MODULE_NAME = module.name
    # Create the module (ContainerSymbol)
    exec("{0} = ContainerSymbol('{1}')\n".format(
        MODULE_NAME.upper(), MODULE_NAME))
    # Create the variables specified by the module (DataSymbols)
    for module_var in module.vars:
        exec("{0} = DataSymbol('{1}', DeferredType(), interface="
             "GlobalInterface({2}))".format(
                 module_var.upper(), module_var, MODULE_NAME.upper()))

# Define generic LFRic scalar datatypes and symbols

# The first Scalar named-tuple argument determines the names of the
# resultant datatype and datasymbol classes, the second argument
# specifies the intrinsic PSyIR type and the third argument specifies
# the precision required by referencing symbols already declared
# above.

GenericScalar = namedtuple('GenericScalar', ["name", "intrinsic", "precision"])
generic_scalar_datatypes = [
    GenericScalar("lfric integer scalar", "integer", "i_def"),
    GenericScalar("lfric real scalar", "real", "r_def"),
    GenericScalar("lfric logical scalar", "boolean", "l_def")]

# Generate generic LFRic scalar datatypes and symbols from definitions
for info in generic_scalar_datatypes:
    NAME = "".join(info.name.title().split())
    INTRINSIC = info.intrinsic.upper()
    PRECISION = info.precision
    ARGS = ["self", "name", "interface=None"]
    # Create the specific datatype
    exec(
        "class {0}DataType(ScalarType):\n"
        "    def __init__(self):\n"
        "        super({0}DataType, self).__init__(\n"
        "            ScalarType.Intrinsic.{1}, {2})\n"
        "".format(NAME, INTRINSIC, PRECISION.upper()))
    # Create the specific symbol
    exec(
        "class {0}DataSymbol(DataSymbol):\n"
        "    def __init__(self, name, interface=None):\n"
        "        super({0}DataSymbol, self).__init__(\n"
        "            name, {0}DataType(), interface=interface)\n"
        "".format(NAME))

# Define specific LFRic scalar datatypes and symbols

# The first Scalar named-tuple argument determines the names of the
# resultant datatype and datasymbol classes, the second argument
# references the generic scalar type classes declared above and the
# third argument specifies any additional class variables that should
# be declared in the generated datasymbol class.

Scalar = namedtuple('Scalar', ["name", "generic_type", "variables"])
specific_scalar_datatypes = [
    Scalar("cell position", "lfric integer scalar", []),
    Scalar("mesh height", "lfric integer scalar", []),
    Scalar("number of cells", "lfric integer scalar", []),
    Scalar("number of dofs", "lfric integer scalar", ["fs"]),
    Scalar("number of unique dofs", "lfric integer scalar", ["fs"]),
    Scalar("number of faces", "lfric integer scalar", []),
    Scalar("number of edges", "lfric integer scalar", []),
    Scalar("number of qr points in horizontal", "lfric integer scalar", []),
    Scalar("number of qr points in vertical", "lfric integer scalar", []),
    Scalar("number of qr points", "lfric integer scalar", [])]

# Generate specific LFRic scalar datatypes and symbols from definitions
for info in specific_scalar_datatypes:
    NAME = "".join(info.name.title().split())
    TYPE = "".join(info.generic_type.title().split())
    ARGS = ["self", "name"] + info.variables + ["interface=None"]
    VARS = ["        self.{0} = {0}".format(var) for var in info.variables]
    # Create the specific datatype
    exec(
        "class {0}DataType({1}DataType):\n"
        "    pass\n"
        "".format(NAME, TYPE))
    # Create the specific symbol
    exec(
        "class {0}DataSymbol({1}DataSymbol):\n"
        "    def __init__({2}):\n"
        "{3}\n"
        "        super({0}DataSymbol, self).__init__(\n"
        "            name, interface=interface)\n"
        "".format(NAME, TYPE, ", ".join(ARGS), "\n".join(VARS)))

# Define LFRic field datatypes and symbols

# Note, field_datatypes are no different to array_datatypes and are
# treated in the same way. They are only separated into a different
# list because they are used to create vector field datatypes and
# symbols.

# The first Array named-tuple argument determines the names of the
# resultant datatype and datasymbol classes, the second argument
# references the generic scalar type classes declared above, the third
# argument specifies the dimensions of the array by specifying a list
# of scalar type classes declared above, and the fourth argument
# specifies any additional class variables that should be declared in
# the generated datasymbol class.

Array = namedtuple('Array', ["name", "scalar_type", "dims", "variables"])
field_datatypes = [
    Array("real field data", "lfric real scalar", ["number of unique dofs"],
          ["fs"]),
    Array("integer field data", "lfric integer scalar",
          ["number of unique dofs"], ["fs"]),
    Array("logical field data", "lfric logical scalar",
          ["number of unique dofs"], ["fs"])]

# Define all other LFRic array datatypes and symbols

# TBD: #918 the dimension datatypes and their ordering is captured in
# field_datatypes and array_datatypes but is not stored in the
# generated classes.

# TBD: #926 attributes will be constrained to certain datatypes and
# values. For example, a function space attribute should be a string
# containing the name of a supported function space. These are not
# currently checked.

# TBD: #927 in some cases the values of attributes can be inferred, or
# at least must be consistent. For example, a field datatype has an
# associated function space attribute, its dimension symbol (if there
# is one) must be a NumberOfUniqueDofsDataSymbol which also has a
# function space attribute and the two function spaces must be
# the same. This is not curently checked.

array_datatypes = [
    Array("operator", "lfric real scalar",
          ["number of dofs", "number of dofs", "number of cells"],
          ["fs_from", "fs_to"]),
    Array("dof map", "lfric integer scalar", ["number of dofs"], ["fs"]),
    Array("basis function qr xyoz", "lfric real scalar",
          ["lfric integer scalar", "number of dofs",
           "number of qr points in horizontal",
           "number of qr points in vertical"], ["fs"]),
    Array("basis function qr face", "lfric real scalar",
          ["lfric integer scalar", "number of dofs", "number of qr points",
           "number of faces"], ["fs"]),
    Array("basis function qr edge", "lfric real scalar",
          ["lfric integer scalar", "number of dofs", "number of qr points",
           "number of edges"], ["fs"]),
    Array("diff basis function qr xyoz", "lfric real scalar",
          ["lfric integer scalar", "number of dofs",
           "number of qr points in horizontal",
           "number of qr points in vertical"], ["fs"]),
    Array("diff basis function qr face", "lfric real scalar",
          ["lfric integer scalar", "number of dofs", "number of qr points",
           "number of faces"], ["fs"]),
    Array("diff basis function qr edge", "lfric real scalar",
          ["lfric integer scalar", "number of dofs", "number of qr points",
           "number of edges"], ["fs"]),
    Array("qr weights in horizontal", "lfric real scalar",
          ["number of qr points in horizontal"], []),
    Array("qr weights in vertical", "lfric real scalar",
          ["number of qr points in vertical"], []),
    Array("qr weights", "lfric real scalar", ["number of qr points"], [])]

# Generate LFRic array (including field) datatypes and symbols from definitions
for array_type in array_datatypes + field_datatypes:
    NAME = "".join(array_type.name.title().split())
    DIMS = array_type.dims
    SCALAR_TYPE = "".join(array_type.scalar_type.title().split())
    ARGS = ["self", "name", "dims"] + array_type.variables + ["interface=None"]
    VARS = ["        self.{0} = {0}".format(var) for var in
            array_type.variables]
    # Create the specific datatype
    exec(
        "class {0}DataType(ArrayType):\n"
        "    def __init__(self, dims):\n"
        "        if (len(dims) != {2}):\n"
        "            raise TypeError(\n"
        "                \"{0}DataType expected the number of supplied \"\n"
        "                \"dimensions to be {2} but found {{0}}.\"\n"
        "                \"\".format(len(dims)))\n"
        "        super({0}DataType, self).__init__(\n"
        "            {1}DataType(), dims)\n"
        "".format(NAME, SCALAR_TYPE, len(DIMS)))
    # Create the specific symbol
    exec(
        "class {0}DataSymbol(DataSymbol):\n"
        "    def __init__({1}):\n"
        "{2}\n"
        "        super({0}DataSymbol, self).__init__(\n"
        "            name, {0}DataType(dims), interface=interface)\n"
        "".format(NAME, ", ".join(ARGS), "\n".join(VARS)))

# Generate LFRic vector-field-data symbols as subclasses of field-data symbols
for array_type in field_datatypes:
    NAME = "".join(array_type.name.title().split())
    VECTOR_NAME = NAME.replace("Field", "VectorField")
    exec(
        "class {0}DataSymbol({1}DataSymbol):\n"
        "    pass\n"
        "".format(VECTOR_NAME, NAME))