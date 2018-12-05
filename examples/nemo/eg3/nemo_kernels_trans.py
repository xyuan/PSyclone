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

'''A simple test script showing the introduction of the OpenACC
kernels, loop and parallel directives with PSyclone.  In order to use
it you must first install PSyclone. See README.md in the top-level
psyclone directory.

Once you have psyclone installed, this script may be run by doing (you may
need to make it executable first with chmod u+x ./runme_openacc.py):

 >>> ./runme_openacc.py

This should generate a lot of output, ending with generated
Fortran.

'''

from __future__ import print_function
from psyclone.psyGen import TransInfo
from psyclone.nemo import NemoKern, NemoLoop, NemoCodeBlock


ACC_TRANS = TransInfo().get_trans_name('ACCKernelsTrans')

def valid_kernel(node):
    if isinstance(node, NemoCodeBlock):
        return False
    code_blocks = node.walk(node.children, NemoCodeBlock)
    if code_blocks:
        return False
    return True


def add_kernels(children):
    '''
    :param children: list of sibling Nodes in PSyIRe that are candidates for \
                     inclusion in an ACC KERNELS region.
    :type children: list of :py:class:`psyclone.psyGen.Node`
    '''
    if not children:
        return
    node_list = []
    for child in children[:]:
        # Can this node be included in a kernels region?
        if not valid_kernel(child):
            if node_list:
                _, _ = ACC_TRANS.apply(node_list)
                node_list = []
            # recurse
            add_kernels(child.children)
        else:
            node_list.append(child)
    if node_list:
        _, _ = ACC_TRANS.apply(node_list)


def trans(psy):
    '''A PSyclone-script compliant function. Demonstrates the application
    of the Kernels, Parallel and Loop OpenACC directives to the
    'tra_adv' code.

    '''
    print("Invokes found:")
    print(psy.invokes.names)

    for invoke in psy.invokes.invoke_list:

        sched = invoke.schedule
        sched.view()
        #import pdb; pdb.set_trace()
        add_kernels(sched.children)
        sched.view()
        # Enclose all children in the schedule within 'kernels'. We must ensure
        # that the allocate/deallocate's are done before/after the kernels
        # directive and so we search for the first and last loops
        if 0:
            first_idx = -1
            last_idx = -1
            for idx, child in enumerate(sched.children):
                if isinstance(child, NemoLoop):
                    if first_idx == -1:
                        first_idx = idx
                    last_idx = idx
            print("first, last = ", first_idx, last_idx)
            if first_idx == -1:
                # We didn't find any loops so we skip this invoke
                continue
            sched, _ = ACC_TRANS.apply(sched.children[first_idx:last_idx+1])

            sched.view()

        invoke.schedule = sched
