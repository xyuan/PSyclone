#!/usr/bin/env python
# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2018, Science and Technology Facilities Council
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
# Author A. R. Porter, STFC Daresbury Laboratory
from __future__ import print_function

if __name__ == "__main__":
    import os
    import sys
    from argparse import ArgumentParser
    from xml.dom.minidom import parse
    from psyclone.psyGen import PSyFactory
    from psyclone.nemo0p1 import NemoSchedule, NemoLoop
    from psyclone.transformations import OMPParallelLoopTrans

    parser = ArgumentParser(
        description="Convert XcodeML/Fortran into XcodeML/C")
    parser.add_argument("files", metavar="input_file", type=str, nargs="+",
                        help="Name of XCodeML/F file(s) to convert")
    result = parser.parse_args(sys.argv[1:])

    omptrans = OMPParallelLoopTrans()

    for infile in result.files:
        if not os.path.isfile(infile):
            print("Cannot find input file '{0}' - skipping".format(file))
            continue
        # Parse the supplied XCodeML/F file
        dom = parse(infile)

        # Use the resulting DOM to create our psy object
        psy = PSyFactory("nemo0.1").create(dom)

        sched = psy.invokes.get('invoke_0').schedule

        sched.view()
        dom2 = psy.gen

        # Apply a transformation to the Schedule
        for node in sched.children:
            if isinstance(node, NemoLoop) and node.loop_type == 'levels':
                omptrans.apply(node)

        sched.view()
        dom3 = psy.gen

        # Write the XML to file so we can process it with CLAW
        xml = dom3.toxml()
        with open("new.xml", "w") as xmlfile:
            xmlfile.write(xml)

        print("Transformed XML written to new.xml")
