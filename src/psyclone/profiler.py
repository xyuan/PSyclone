# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017, Science and Technology Facilities Council
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2016.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
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
# -----------------------------------------------------------------------------

''' This module provides support for adding profiling to code
    generated by PSyclone. '''

from psyclone.f2pygen import CallGen, ModuleGen, SubroutineGen, TypeDeclGen
from psyclone.psyGen import GenerationError, Kern, NameSpaceFactory, Node


class Profiler(object):
    ''' This class wraps all profiling related settings.'''

    # Command line option to use for the various profiling options
    INVOKES = "invokes"
    KERNELS = "kernels"
    SUPPORTED_OPTIONS = [INVOKES, KERNELS]
    _options = []

    # -------------------------------------------------------------------------
    @staticmethod
    def set_options(options):
        '''Sets the option the user required.
        :param options: List of options selected by the user.
        :type options: List of strings.'''
        Profiler._options = options

    # -------------------------------------------------------------------------
    @staticmethod
    def profile_kernels():
        '''Returns true if kernel profiling is enabled.
        :return: True if kernels should be profiled.
        :rtype: bool'''
        return Profiler._options is not None and \
            Profiler.KERNELS in Profiler._options

    # -------------------------------------------------------------------------
    @staticmethod
    def profile_invokes():
        '''Returns true if kernel profiling is enabled.
        :return: True if kernels should be profiled.
        :rtype: bool'''
        return Profiler._options is not None and \
            Profiler.INVOKES in Profiler._options

    # -------------------------------------------------------------------------
    @staticmethod
    def add_profile_nodes(schedule, loop_class):
        '''This function inserts all required Profiling Nodes into a
        schedule.'''
        from psyclone.transformations import ProfileRegionTrans
        if Profiler.profile_kernels():
            profile_trans = ProfileRegionTrans()
            for i in schedule.children:
                if isinstance(i, loop_class):
                    profile_trans.apply(i)
        if Profiler.profile_invokes():
            profile_trans = ProfileRegionTrans()
            profile_trans.apply(schedule.children)


# =============================================================================
class ProfileNode(Node):
    '''This class can be inserted into a schedule to create profiling code.
    '''

    def __str__(self):
        return "Profiler"

    # -------------------------------------------------------------------------
    def view(self, indent=0):
        # pylint: disable=arguments-differ
        print self.indent(indent) + "[Profile]"
        for entity in self._children:
            entity.view(indent=indent + 1)

    # -------------------------------------------------------------------------
    def gen_code(self, parent):
        # pylint: disable=arguments-differ
        '''Creates the profile start and end calls, surrounding the children
        of this node.
        :param parent: The parent of this node.
        :type parent: :py:class:`psyclone.psyGen.Node`.'''

        # Find the first kernel and use its name. In plain PSyclone there
        # should be only one kernel, but if Profile is invoked after e.g.
        # a loop merge more kernels might be there
        kernel_name = "unknown-kernel"
        for k in self.walk(self.children, Kern):
            kernel_name = k.name
            break
        # Use the namespace manager to create unique names within one module
        # one subroutine might have many identical calls.
        kernel_name = NameSpaceFactory().create().create_name(kernel_name)

        # Find the enclosing subroutine statement for declaring variables
        subroutine = parent
        while subroutine and not isinstance(subroutine, SubroutineGen):
            subroutine = subroutine.parent

        if not subroutine:
            raise GenerationError("Can not find subroutine for '{0}'.".
                                  format(str(parent)))

        # Find the name of the module
        ancestor = parent
        while ancestor and not isinstance(ancestor, ModuleGen):
            ancestor = ancestor.parent
        if ancestor:
            module_name = ancestor.root.name
        else:
            module_name = "unknown module"

        profile_name = NameSpaceFactory().create().create_name("profile")
        prof_var_decl = TypeDeclGen(parent, datatype="ProfilerData",
                                    entity_decls=[profile_name],
                                    attrspec=["save"])
        parent.add(prof_var_decl)

        prof_start = CallGen(parent, "profile_start",
                             ["\"{0}\"".format(kernel_name),
                              "\"{0}\"".format(module_name),
                              profile_name])

        obj = subroutine.last_declaration()
        parent.add(prof_start, position=["auto", obj])

        for child in self.children:
            child.gen_code(parent)

        prof_end = CallGen(parent, "profile_end",
                           [profile_name])
        parent.add(prof_end)
