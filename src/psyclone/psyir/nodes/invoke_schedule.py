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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module contains the implementation of the InvokeSchedule node. '''

from psyclone.psyir.nodes.routine import Routine
from psyclone.parse.algorithm import BuiltInCall
from psyclone.psyir.symbols import DataSymbol, ArrayType, \
    Symbol, INTEGER_TYPE, BOOLEAN_TYPE
from psyclone.f2pygen import UseGen, DeclGen, AssignGen, CommentGen, \
    IfThenGen, CallGen


class InvokeSchedule(Routine):
    '''
    Stores schedule information for an invocation call. Schedules can be
    optimised using transformations.

    >>> from psyclone.parse.algorithm import parse
    >>> ast, info = parse("algorithm.f90")
    >>> from psyclone.psyGen import PSyFactory
    >>> api = "..."
    >>> psy = PSyFactory(api).create(info)
    >>> invokes = psy.invokes
    >>> invokes.names
    >>> invoke = invokes.get("name")
    >>> schedule = invoke.schedule
    >>> schedule.view()

    :param type KernFactory: class instance of the factory to use when \
     creating Kernels. e.g. :py:class:`psyclone.dynamo0p3.DynKernCallFactory`.
    :param type BuiltInFactory: class instance of the factory to use when \
     creating built-ins. e.g. \
     :py:class:`psyclone.dynamo0p3_builtins.DynBuiltInCallFactory`.
    :param alg_calls: list of Kernel calls in the schedule.
    :type alg_calls: list of :py:class:`psyclone.parse.algorithm.KernelCall`

    '''
    # Textual description of the node.
    _text_name = "InvokeSchedule"

    def __init__(self, name, KernFactory, BuiltInFactory, alg_calls=None,
                 reserved_names=None):
        super(InvokeSchedule, self).__init__(name)

        self._invoke = None
        self._opencl = False  # Whether or not to generate OpenCL

        # InvokeSchedule opencl_options default values
        self._opencl_options = {"end_barrier": True}

        # Populate the Schedule Symbol Table with the reserved names.
        if reserved_names:
            for name in reserved_names:
                self.symbol_table.add(Symbol(name))

        # We need to separate calls into loops (an iteration space really)
        # and calls so that we can perform optimisations separately on the
        # two entities.
        if alg_calls is None:
            alg_calls = []
        for call in alg_calls:
            if isinstance(call, BuiltInCall):
                self.addchild(BuiltInFactory.create(call, parent=self))
            else:
                self.addchild(KernFactory.create(call, parent=self))

    @property
    def symbol_table(self):
        '''
        :returns: Table containing symbol information for the schedule.
        :rtype: :py:class:`psyclone.psyir.symbols.SymbolTable`
        '''
        return self._symbol_table

    def set_opencl_options(self, options):
        '''
        Validate and store a set of options associated with the InvokeSchedule
        to tune the OpenCL code generation.

        :param options: a set of options to tune the OpenCL code.
        :type options: dictionary of <string>:<value>

        '''
        valid_opencl_options = ['end_barrier']

        # Validate that the options given are supported and store them
        for key, value in options.items():
            if key in valid_opencl_options:
                if key == "end_barrier":
                    if not isinstance(value, bool):
                        raise TypeError(
                            "InvokeSchedule opencl_option 'end_barrier' "
                            "should be a boolean.")
            else:
                raise AttributeError(
                    "InvokeSchedule does not support the opencl_option '{0}'. "
                    "The supported options are: {1}."
                    "".format(key, valid_opencl_options))

            self._opencl_options[key] = value

    @property
    def invoke(self):
        return self._invoke

    @invoke.setter
    def invoke(self, my_invoke):
        self._invoke = my_invoke

    def node_str(self, colour=True):
        '''
        Returns the name of this node with appropriate control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        return "{0}[invoke='{1}']".format(
            self.coloured_name(colour), self.invoke.name)

    def __str__(self):
        result = self.coloured_name(False) + ":\n"
        for entity in self._children:
            result += str(entity) + "\n"
        result += "End " + self.coloured_name(False) + "\n"
        return result

    def gen_code(self, parent):
        '''
        Generate the Nodes in the f2pygen AST for this schedule.

        :param parent: the parent Node (i.e. the enclosing subroutine) to \
                       which to add content.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        # Have to do this import here to avoid circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.psyGen import Kern

        # The gen_code methods may generate new Symbol names, however, we want
        # subsequent calls to invoke.gen_code() to produce the exact same code,
        # including symbol names, and therefore new symbols should not be kept
        # permanently outside the hierarchic gen_code call-chain.
        # To make this possible we create here a duplicate of the symbol table.
        # This duplicate will be used by all recursive gen_code() methods
        # called below this one and thus maintaining a consistent Symbol Table
        # during the whole gen_code() chain, but at the end of this method the
        # original symbol table is restored.
        symbol_table_before_gen = self.symbol_table
        self._symbol_table = self.symbol_table.shallow_copy()

        # Global symbols promoted from Kernel Globals are in the SymbolTable
        # First aggregate all globals variables from the same module in a map
        module_map = {}
        for globalvar in self.symbol_table.global_symbols:
            module_name = globalvar.interface.container_symbol.name
            if module_name in module_map:
                module_map[module_name].append(globalvar.name)
            else:
                module_map[module_name] = [globalvar.name]

        # Then we can produce the UseGen statements without repeating modules
        for module_name, var_list in module_map.items():
            parent.add(UseGen(parent, name=module_name, only=True,
                              funcnames=var_list))

        if self._opencl:
            parent.add(UseGen(parent, name="iso_c_binding"))
            parent.add(UseGen(parent, name="clfortran"))
            parent.add(UseGen(parent, name="fortcl", only=True,
                              funcnames=["get_num_cmd_queues",
                                         "get_cmd_queues",
                                         "get_kernel_by_name"]))

            # Declare variables needed on a OpenCL PSy-layer invoke
            nqueues = self.symbol_table.new_symbol_name("num_cmd_queues")
            self.symbol_table.add(DataSymbol(nqueues, INTEGER_TYPE),
                                  tag="opencl_num_cmd_queues")
            qlist = self.symbol_table.new_symbol_name("cmd_queues")
            self.symbol_table.add(
                DataSymbol(qlist,
                           ArrayType(INTEGER_TYPE,
                                     [ArrayType.Extent.ATTRIBUTE])),
                tag="opencl_cmd_queues")
            first = self.symbol_table.new_symbol_name("first_time")
            self.symbol_table.add(
                DataSymbol(first, BOOLEAN_TYPE), tag="first_time")
            flag = self.symbol_table.new_symbol_name("ierr")
            self.symbol_table.add(
                DataSymbol(flag, INTEGER_TYPE), tag="opencl_error")
            nbytes = self.root.symbol_table.new_symbol_name(
                "size_in_bytes")
            self.symbol_table.add(
                DataSymbol(nbytes, INTEGER_TYPE), tag="opencl_bytes")
            wevent = self.root.symbol_table.new_symbol_name("write_event")
            self.symbol_table.add(
                DataSymbol(wevent, INTEGER_TYPE), tag="opencl_wevent")

            parent.add(DeclGen(parent, datatype="integer", save=True,
                               entity_decls=[nqueues]))
            parent.add(DeclGen(parent, datatype="integer", save=True,
                               pointer=True, kind="c_intptr_t",
                               entity_decls=[qlist + "(:)"]))
            parent.add(DeclGen(parent, datatype="integer",
                               entity_decls=[flag]))
            parent.add(DeclGen(parent, datatype="logical", save=True,
                               entity_decls=[first],
                               initial_values=[".true."]))
            if_first = IfThenGen(parent, first)
            parent.add(if_first)
            if_first.add(AssignGen(if_first, lhs=first, rhs=".false."))
            if_first.add(CommentGen(if_first,
                                    " Ensure OpenCL run-time is initialised "
                                    "for this PSy-layer module"))
            if_first.add(CallGen(if_first, "psy_init"))
            if_first.add(AssignGen(if_first, lhs=nqueues,
                                   rhs="get_num_cmd_queues()"))
            if_first.add(AssignGen(if_first, lhs=qlist, pointer=True,
                                   rhs="get_cmd_queues()"))
            # Kernel pointers
            kernels = self.walk(Kern)
            for kern in kernels:
                base = "kernel_" + kern.name
                kernel = self.root.symbol_table.new_symbol_name(base)
                self.symbol_table.add(Symbol(kernel), tag=kernel)
                parent.add(
                    DeclGen(parent, datatype="integer", kind="c_intptr_t",
                            save=True, target=True, entity_decls=[kernel]))
                if_first.add(
                    AssignGen(
                        if_first, lhs=kernel,
                        rhs='get_kernel_by_name("{0}")'.format(kern.name)))

        for entity in self._children:
            entity.gen_code(parent)

        if self.opencl and self._opencl_options['end_barrier']:

            parent.add(CommentGen(parent,
                                  " Block until all kernels have finished"))

            # We need a clFinish for all the queues in the implementation
            opencl_num_queues = 1
            for kern in self.coded_kernels():
                opencl_num_queues = max(
                    opencl_num_queues,
                    kern.opencl_options['queue_number'])
            for queue_number in range(1, opencl_num_queues + 1):
                parent.add(
                    AssignGen(parent, lhs=flag,
                              rhs="clFinish({0}({1}))".format(qlist,
                                                              queue_number)))

        # Restore symbol table
        self._symbol_table = symbol_table_before_gen

    @property
    def opencl(self):
        '''
        :returns: Whether or not we are generating OpenCL for this \
            InvokeSchedule.
        :rtype: bool
        '''
        return self._opencl

    @opencl.setter
    def opencl(self, value):
        '''
        Setter for whether or not to generate the OpenCL version of this
        schedule.

        :param bool value: whether or not to generate OpenCL.
        '''
        if not isinstance(value, bool):
            raise ValueError(
                "InvokeSchedule.opencl must be a bool but got {0}".
                format(type(value)))
        self._opencl = value
