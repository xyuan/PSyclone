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
import os


def _run_omni_front(filename):
    '''
    Run the OMNI front-end on the specified filename and return a DOM.
    :param str filename: name (and path) of Fortran source file
    :returns: DOM of XCodeML/F representation of the Fortran
    :rtype: :py:class:`xml.dom` TBD
    '''
    import subprocess
    from xml.dom.minidom import parse
    xmlfile = filename + ".xml"
    # TODO OMNI must be able to find .xmod files for any modules that
    # the supplied Fortran code USEs.
    arg_list = ['F_Front', filename, '-o', xmlfile]
    try:
        build = subprocess.Popen(arg_list,
                                 stdout=subprocess.PIPE,
                                 stderr=subprocess.STDOUT)
        (output, error) = build.communicate()
    except OSError as err:
        print("Failed to run: {0}: ".format(" ".join(arg_list)))
        print("Error was: ", str(err))
        raise Exception(str(err))

    # Parse the generated XCodeML/F file
    dom = parse(xmlfile)

    # Remove the XCodeML/F file
    os.remove(xmlfile)

    return dom


def _run_omni_back(xdom, outfile):
    '''
    :param xdom: DOM to create Fortran from
    '''
    import subprocess
    import tempfile

    # Write the XML to file so we can process it with CLAW
    xml = xdom.toxml()
    tfile = tempfile.NamedTemporaryFile(mode='w', suffix='xml',
                                        delete=False)
    tfile.write(xml)
    tfile.close()

    # Run OMNI backend on the generated XML file
    arg_list = ['F_Back', '-l', tfile.name, '-o', outfile]
    try:
        build = subprocess.Popen(arg_list,
                                 stdout=subprocess.PIPE,
                                 stderr=subprocess.STDOUT)
        (output, error) = build.communicate()
    except OSError as err:
        print("Failed to run: {0}: ".format(" ".join(arg_list)))
        print("Error was: ", str(err))
        raise Exception(str(err))
    print("DOM written to {0}".format(outfile))


if __name__ == "__main__":
    import sys
    import subprocess
    from xml.dom.minidom import parse
    from argparse import ArgumentParser
    from psyclone.psyGen import PSyFactory
    from psyclone.nemo0p1 import NemoSchedule, NemoLoop
    from psyclone.transformations import OMPParallelLoopTrans

    parser = ArgumentParser(
        description="Transform XcodeML/Fortran and convert back to Fortran")
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

        # Find all loops in the Schedule
        loops = sched.walk(sched.children, NemoLoop)

        # Apply a transformation to every loop over levels
        for loop in loops:
            if loop.loop_type == 'levels':
                omptrans.apply(loop)

        sched.view()

        # Generate the new XCodeML/F from the transformed AST and run the
        # OMNI back-end on it
        dom3 = psy.gen
        outfile = infile.replace(".xml", ".new.f90")
        _run_omni_back(dom3, outfile)
