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


''' Performs pytest tests on the fparser2 PSyIR frontend handling of
    array declarations. '''

from __future__ import absolute_import
import pytest
from psyclone.psyir.nodes import Range, Literal, Reference
from psyclone.psyir.symbols import DataSymbol
from psyclone.tests.psyir.frontend.fparser2_test import process_declarations


@pytest.mark.usefixtures("parser")
def test_array_range():
    ''' Test that we can handle arrays dimensioned with explicit lower and
    upper bounds. '''
    sched, _ = process_declarations("real, dimension(0:n) :: a")
    print(sched.symbol_table)
    array = sched.symbol_table.lookup("a")
    assert isinstance(array, DataSymbol)
    assert isinstance(array.shape[0], Range)
    assert isinstance(array.shape[0].start, Literal)
    assert array.shape[0].start.value == "0"
    assert isinstance(array.shape[0].stop, Reference)
    assert array.shape[0].stop.symbol.name == "n"
    bound = sched.symbol_table.lookup("n")
    assert isinstance(bound, DataSymbol)


def test_array_extent_expression():
    ''' Test that we can handle an array dimensioned using expressions. '''
    sched, _ = process_declarations("real, dimension(n+1) :: a")
    print(sched.symbol_table)
    array = sched.symbol_table.lookup("a")
    assert isinstance(array, DataSymbol)
    assert isinstance(array.shape[0], Range)
    assert 0
