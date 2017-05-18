# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017, Science and Technology Facilities Council
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
# Author R. Ford and A. R. Porter, STFC Daresbury Lab

''' This module contains tests for the infrastructure used to test
the compilation of generated Fortran code '''

import os
import pytest
import utils

def test_compiler_works(tmpdir):
    ''' Check that the specified compiler works for a hello-world
    example '''

    hello_code = '''
program hello
  write (*,*) "Hello"
end program hello
'''
    tmpdir.chdir()
    with open("hello_world.f90", "w") as ffile:
        ffile.write(hello_code)

    success = utils.compile_file("hello_world.f90")
    assert success

    
def test_build_invalid_fortran(tmpdir):
    ''' Check that we raise the expected error when attempting
    to compile some invalid Fortran '''
    if not utils.F90_COMPILER:
        return

    invalid_code = '''
program hello
  wite (*,*) "Hello"
end program hello
'''
    tmpdir.chdir()
    with open("hello_world.f90", "w") as ffile:
        ffile.write(invalid_code)
    with pytest.raises(utils.BuildError) as excinfo:
        _ = utils.compile_file("hello_world.f90")
    assert "Compile error" in str(excinfo)
