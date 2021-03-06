# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2020, Science and Technology Facilities Council.
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
# Author J. Henrichs, Bureau of Meteorology
# Modified by A. R. Porter, STFC Daresbury Lab
# Modified by R. W. Ford, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module provides support for adding profiling to code
    generated by PSyclone. '''

from __future__ import absolute_import, print_function
from psyclone.errors import GenerationError


class Profiler():
    ''' This class wraps all profiling related settings.'''

    # Command line option to use for the various profiling options
    # INVOKES: Automatically add a region for each invoke. i.e. at
    #          the start and end of each PSyclone created subroutine.
    # KERNELS: Automatically add a profile region around every
    #          kernel call including the loop structure created.
    INVOKES = "invokes"
    KERNELS = "kernels"
    SUPPORTED_OPTIONS = [INVOKES, KERNELS]
    _options = []

    # -------------------------------------------------------------------------
    @staticmethod
    def set_options(options):
        '''Sets the option the user required.
        :param options: List of options selected by the user, or None to
                        disable all automatic profiling.
        :type options: List of strings.
        :raises GenerationError: If any option is not KERNELS or INVOKES.
        '''
        # Test that all options are valid
        if options is None:
            options = []   # Makes it easier to test
        for index, option in enumerate(options):
            if option not in [Profiler.INVOKES, Profiler.KERNELS]:
                # Create a 'nice' representation of the allowed options.
                # [1:-1] cuts out the '[' and ']' that surrounding the
                # string of the list.
                allowed_options = str(Profiler.SUPPORTED_OPTIONS)[1:-1]
                raise GenerationError("Error in Profiler.setOptions: options "
                                      "must be one of {0} but found '{1}' "
                                      "at {2}"
                                      .format(allowed_options,
                                              str(option), index))

        # Store options so they can be queried later
        Profiler._options = options

    # -------------------------------------------------------------------------
    @staticmethod
    def profile_kernels():
        '''Returns true if kernel profiling is enabled.
        :return: True if kernels should be profiled.
        :rtype: bool'''
        return Profiler.KERNELS in Profiler._options

    # -------------------------------------------------------------------------
    @staticmethod
    def profile_invokes():
        '''Returns true if invoke profiling is enabled.
        :return: True if invokes should be profiled.
        :rtype: bool'''
        return Profiler.INVOKES in Profiler._options

    # -------------------------------------------------------------------------
    @staticmethod
    def add_profile_nodes(schedule, loop_class):
        '''This function inserts all required Profiling Nodes (for invokes
        and kernels, as specified on the command line) into a schedule.
        :param schedule: The schedule to instrument.
        :type schedule: :py:class:`psyclone.psyGen.InvokeSchedule` or \
                        derived class
        :param loop_class: The loop class (e.g. GOLoop, DynLoop) to instrument.
        :type loop_class: :py:class:`psyclone.psyir.nodes.Loop` or \
                          derived class.
        '''

        from psyclone.psyir.transformations import ProfileTrans
        profile_trans = ProfileTrans()
        if Profiler.profile_kernels():
            for i in schedule.children:
                if isinstance(i, loop_class):
                    profile_trans.apply(i)
        if Profiler.profile_invokes():
            profile_trans.apply(schedule.children)
