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
# Authors: R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
#          I. Kavcic, Met Office
# -----------------------------------------------------------------------------

''' pytest tests for the InvokeSchedule class. '''

from __future__ import absolute_import
import os
import pytest
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory


BASE_PATH = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(
    os.path.abspath(__file__)))), "test_files", "dynamo0p3")


def test_invokeschedule_node_str():
    ''' Check the node_str method of the InvokeSchedule class. We need an
    Invoke object for this which we get using the dynamo0.3 API. '''
    from psyclone.psyGen import InvokeSchedule
    from psyclone.psyir.nodes.node import colored, SCHEDULE_COLOUR_MAP
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.9.1_X_innerproduct_Y_builtin.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    # Create a plain InvokeSchedule
    sched = InvokeSchedule("test", None, None)
    # Manually supply it with an Invoke object created with the Dynamo API.
    sched._invoke = psy.invokes.invoke_list[0]
    output = sched.node_str()
    assert colored("InvokeSchedule", SCHEDULE_COLOUR_MAP["Schedule"]) in output


def test_sched_ocl_setter():
    ''' Check that the opencl setter raises the expected error if not passed
    a bool. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.9.1_X_innerproduct_Y_builtin.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    with pytest.raises(ValueError) as err:
        psy.invokes.invoke_list[0].schedule.opencl = "a string"
    assert "Schedule.opencl must be a bool but got " in str(err.value)


def test_invokeschedule_can_be_printed():
    ''' Check the InvokeSchedule class can always be printed'''
    from psyclone.psyGen import InvokeSchedule
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.9.1_X_innerproduct_Y_builtin.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)

    # For this test use the generic class
    psy.invokes.invoke_list[0].schedule.__class__ = InvokeSchedule
    output = str(psy.invokes.invoke_list[0].schedule)

    assert "InvokeSchedule:\n" in output
