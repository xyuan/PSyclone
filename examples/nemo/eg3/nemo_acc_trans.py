#!/usr/bin/env python
# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018, Science and Technology Facilities Council
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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''
A PSyclone transformation script for NEMO. Must be supplied to PSyclone
via the -s command-line flag.

'''

from __future__ import print_function
from psyclone.psyGen import TransInfo
from psyclone.nemo import NemoKern, NemoLoop


def trans(psy):
    '''A PSyclone-script compliant function. Demonstrates the application
    of the Kernels, Parallel and Loop OpenACC directives to the
    'tra_adv' code.

    '''
    print("Invokes found:")
    print(psy.invokes.names)

    trans_info = TransInfo()
    acc_trans = trans_info.get_trans_name('ACCKernelsTrans')
    acc_loop_trans = trans_info.get_trans_name('ACCLoopTrans')

    for invoke in psy.invokes.invoke_list:

        sched = invoke.schedule
        sched.view()

        # Enclose loops within a 'kernels' region. Ideally we'd do
        # this at as high a level as possible but that involves
        # identifying nodes that cannot be within this region
        # (i.e. subroutine calls).
        for loop in sched.loops():
            kernels = loop.walk(loop.children, NemoKern)
            if kernels:
                if loop.loop_type == "levels":
                    sched, _ = acc_trans.apply([loop])
                elif loop.loop_type == "lat":
                    if not isinstance(loop.parent, NemoLoop):
                        sched, _ = acc_trans.apply([loop])
                    sched, _ = acc_loop_trans.apply(loop, collapse=2)

        sched.view()

        invoke.schedule = sched
