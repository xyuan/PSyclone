# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2019, Science and Technology Facilities Council.
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
# Author A. R. Porter, STFC Daresbury Lab

''' This module tests the support for named reference-cell properties in
the Dynamo 0.3 API using pytest. '''

from __future__ import absolute_import, print_function
import os
import pytest
from fparser import api as fpapi
from psyclone.parse import ParseError, parse
from psyclone.dynamo0p3 import DynKernMetadata
from psyclone.psyGen import PSyFactory, GenerationError

# Constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3")

MDATA = '''
module testkern
  type, extends(kernel_type) :: testkern_type
     type(arg_type), meta_args(3) =                    &
          (/ arg_type(gh_field,gh_write, any_space_1), &
             arg_type(gh_real, gh_read),               &
             arg_type(gh_out_face_normal)              &
           /)
     integer, parameter :: iterates_over = cells
   contains
     procedure() :: code => testkern_code
  end type testkern_type
contains
  subroutine testkern_code(a,b,c,d)
  end subroutine testkern_code
end module testkern
'''


def test_mdata():
    ''' Check that we can parse meta-data entries containing
    reference-element properties. '''
    ast = fpapi.parse(MDATA, ignore_comments=False)
    name = "testkern_type"
    dkm = DynKernMetadata(ast, name=name)
    dkm_str = str(dkm.arg_descriptors[2])
    expected = (
        "DynArgDescriptor03 object\n"
        "  argument_type[0]='gh_out_face_normal'\n"
        "  access_descriptor[1]='gh_read'\n")
    print(dkm_str)
    assert expected in dkm_str


def test_wrong_mdata():
    ''' Check that we reject a property that we don't recognise. '''
    _mdata = MDATA.replace("gh_out_face_normal", "not_a_property")
    ast = fpapi.parse(_mdata, ignore_comments=False)
    name = "testkern_type"
    with pytest.raises(ParseError) as err:
        _ = DynKernMetadata(ast, name=name)
    assert ("but found 'not_a_property' in 'arg_type(not_a_property)'"
            in str(err))


def test_duplicate_property_error():
    ''' Check that we raise an error if the same ref-element property
    is specified more than once in kernel meta-data. '''
    _mdata = MDATA.replace(
        "             arg_type(gh_real, gh_read),               &",
        "             arg_type(gh_out_face_normal),             &\n" +
        "             arg_type(gh_real, gh_read),               &")
    _mdata = _mdata.replace("meta_args(3)", "meta_args(4)")
    ast = fpapi.parse(_mdata, ignore_comments=False)
    name = "testkern_type"
    with pytest.raises(ParseError) as err:
        _ = DynKernMetadata(ast, name=name)
    assert ("for kernel testkern_type contains duplicate entries for the "
            "'gh_out_face_normal' reference-element property" in str(err))
