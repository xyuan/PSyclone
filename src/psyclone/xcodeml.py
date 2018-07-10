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
# Author: A. R. Porter, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module provides support for creating/obtaining the XCodeML/F
    representation of Fortran code. It uses the Omni compiler which
    must be available on the user's PATH. '''

from __future__ import print_function
import os


class ExternalError(Exception):
    ''' Provides a PSyclone specific error class for errors that occur
    when running external processes. '''
    def __init__(self, value):
        Exception.__init__(self, value)
        self.value = "External Error: "+value

    def __str__(self):
        return repr(self.value)


def omni_frontend(filename, mod_search_paths=None):
    '''
    Run the OMNI front-end on the specified filename and return a DOM.
    :param str filename: name (and path) of Fortran source file.
    :param list mod_search_paths: list of locations (str) to search for \
                                  Fortran module files.
    :returns: DOM of XCodeML/F representation of the Fortran.
    :rtype: :py:class:`xml.dom` TBD
    '''
    import subprocess
    from xml.dom.minidom import parse
    xmlfile = filename + ".xml"
    # TODO OMNI must be able to find .xmod files for any modules that
    # the supplied Fortran code USEs.
    arg_list = ['F_Front']
    if mod_search_paths:
        inc_args = ["-I{0}".format(path) for path in mod_search_paths]
        mod_path = " ".join(inc_args)
        arg_list.append(mod_path)
    arg_list += [filename, '-o', xmlfile]
    try:
        build = subprocess.Popen(arg_list,
                                 stdout=subprocess.PIPE,
                                 stderr=subprocess.STDOUT)
        (output, error) = build.communicate()
    except OSError as err:
        print("Failed to run: {0}: ".format(" ".join(arg_list)))
        print("Error was: ", str(err))
        raise ExternalError(str(err))

    if build.returncode != 0:
        raise ExternalError(
            "Failed to run Omni frontend:\n stderr={0}, stdout={1}".
            format(str(error), str(output)))
    
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


def _validate_omni_setup():
    '''
    Perform some manual checks to catch any obvious errors in configuration/
    installation of Omni.

    :raises TransformationError: if a problem is found.
    '''
    # Check that the Omni frontend is on our path
    found = False
    path_env = os.environ['PATH']
    if path_env:
        for locn in path_env.split(':'):
            if os.path.isfile(os.path.join(locn, "F_Front")):
                found = True
                break
    if not found:
        raise TransformationError(
            "The frontend of the Omni compiler (F_Front) cannot be "
            "found. Please ensure that it is on your PATH ({0}).".
            format(path_env))

    # Check that any locations specified for Omni-compiled modules do
    # at least exist
    #for api in claw_config.OMNI_MODULES_PATH:
    #    if not os.path.exists(claw_config.OMNI_MODULES_PATH[api]):
    #        raise TransformationError(
    #            "The location ({0}) for Omni-compiled modules for the {1} "
    #            "API does not exist. Please correct OMNI_MODULES_PATH in "
    #            "the PSyclone configuration file.".
    #            format(claw_config.OMNI_MODULES_PATH[api], api))
