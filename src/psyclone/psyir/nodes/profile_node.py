# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2019, Science and Technology Facilities Council.
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
# -----------------------------------------------------------------------------

''' This module provides support for adding profiling to code
    generated by PSyclone. '''

from __future__ import absolute_import, print_function

from psyclone.psyir.nodes.psy_data_node import PSyDataNode


class ProfileNode(PSyDataNode):
    '''
    This class can be inserted into a schedule to create profiling code.

    :param ast: reference into the fparser2 parse tree corresponding to \
                this node.
    :type ast: sub-class of :py:class:`fparser.two.Fortran2003.Base`
    :param children: a list of child nodes for this node. These will be made \
                     children of the child Schedule of this Profile Node.
    :type children: list of :py::class::`psyclone.psyGen.Node` \
                    or derived classes
    :param parent: the parent of this node in the PSyIR.
    :type parent: :py::class::`psyclone.psyGen.Node`

    '''
    # Profiling interface Fortran module
    fortran_module = "psy_data_mod"
    # The symbols we import from the profiling Fortran module
    profiling_symbols = ["PSyDataType"]
    # The use statement that we will insert. Any use of a module of the
    # same name that doesn't match this will result in a NotImplementedError
    # at code-generation time.
    use_stmt = "use psy_data_mod, only: " + ", ".join(profiling_symbols)
    # Root of the name to use for variables associated with profiling regions
    profiling_var = "psy_profile"

    def __init__(self, ast=None, children=None, parent=None):
        super(ProfileNode, self).__init__(ast=ast, children=children,
                                          parent=parent)

        # Name and colour to use for this node
        self._text_name = "Profile"
        self._colour_key = "Profile"

    # -------------------------------------------------------------------------
    def __str__(self):
        ''' Returns a string representation of the subtree starting at
        this node. '''
        result = "ProfileStart[var={0}]\n".format(self._var_name)
        for child in self.profile_body.children:
            result += str(child)+"\n"
        return result+"ProfileEnd"

    @property
    def profile_body(self):
        '''
        :returns: the Schedule associated with this Profiling region.
        :rtype: :py:class:`psyclone.psyGen.Schedule`

        :raises InternalError: if this Profile node does not have a Schedule \
                               as its one and only child.
        '''
        from psyclone.psyGen import Schedule, InternalError
        if len(self.children) != 1 or not \
           isinstance(self.children[0], Schedule):
            raise InternalError(
                "ProfileNode malformed or incomplete. It should have a single "
                "Schedule as a child but found: {0}".format(
                    [type(child).__name__ for child in self.children]))
        return super(ProfileNode, self).psy_data_body

    def gen_code(self, parent):
        # pylint: disable=arguments-differ
        '''Creates the profile start and end calls, surrounding the children
        of this node.

        :param parent: the parent of this node.
        :type parent: :py:class:`psyclone.psyGen.Node`

        '''
        options = {'pre-var-list': [],
                   'post-var-list': []}

        super(ProfileNode, self).gen_code(parent, options)

    # -------------------------------------------------------------------------
    def gen_c_code(self, indent=0):
        '''
        Generates a string representation of this Node using C language
        (currently not supported).

        :param int indent: Depth of indent for the output string.
        :raises NotImplementedError: Not yet supported for profiling.
        '''
        raise NotImplementedError("Generation of C code is not supported "
                                  "for profiling")

    def update(self):
        # pylint: disable=too-many-branches, too-many-statements
        # pylint: disable=too-many-locals
        '''
        Update the underlying fparser2 parse tree to implement the profiling
        region represented by this Node. This involves adding the necessary
        module use statement as well as the calls to the profiling API.

        TODO #435 - remove this whole method once the NEMO API uses the
        Fortran backend of the PSyIR.

        :raises NotImplementedError: if the routine which is to have \
                             profiling added to it does not already have a \
                             Specification Part (i.e. some declarations).
        :raises NotImplementedError: if there would be a name clash with \
                             existing variable/module names in the code to \
                             be transformed.
        :raises InternalError: if we fail to find the node in the parse tree \
                             corresponding to the end of the profiling region.

        '''
        from fparser.common.sourceinfo import FortranFormat
        from fparser.common.readfortran import FortranStringReader
        from fparser.two.utils import walk_ast
        from fparser.two import Fortran2003
        from psyclone.psyGen import object_index, Schedule, InternalError

        # Ensure child nodes are up-to-date
        #super(ProfileNode, self).update()
        from psyclone.psyGen import Node
        Node.update(self)

        # Get the parse tree of the routine containing this region
        ptree = self.root.invoke._ast
        # Rather than repeatedly walk the tree, we do it once for all of
        # the node types we will be interested in...
        node_list = walk_ast([ptree], [Fortran2003.Main_Program,
                                       Fortran2003.Subroutine_Stmt,
                                       Fortran2003.Function_Stmt,
                                       Fortran2003.Specification_Part,
                                       Fortran2003.Use_Stmt,
                                       Fortran2003.Name])
        for node in node_list:
            if isinstance(node, (Fortran2003.Main_Program,
                                 Fortran2003.Subroutine_Stmt,
                                 Fortran2003.Function_Stmt)):
                names = walk_ast([node], [Fortran2003.Name])
                routine_name = str(names[0]).lower()
                break

        for node in node_list:
            if isinstance(node, Fortran2003.Specification_Part):
                spec_part = node
                break
        else:
            # This limitation will be removed when we use the Fortran
            # backend of the PSyIR (#435)
            raise NotImplementedError(
                "Addition of profiling regions to routines without any "
                "existing declarations is not supported and '{0}' has no "
                "Specification-Part".format(routine_name))

        # Get the existing use statements
        found = False
        for node in node_list[:]:
            if isinstance(node, Fortran2003.Use_Stmt) and \
               self.fortran_module == str(node.items[2]).lower():
                # Check that the use statement matches the one we would
                # insert (i.e. the code doesn't already contain a module
                # with the same name as that used by the profiling API)
                if str(node).lower() != self.use_stmt.lower():
                    raise NotImplementedError(
                        "Cannot add profiling to '{0}' because it already "
                        "'uses' a module named '{1}'".format(
                            routine_name, self.fortran_module))
                found = True
                # To make our check on name clashes below easier, remove
                # the Name nodes associated with this use from our
                # list of nodes.
                names = walk_ast([node], [Fortran2003.Name])
                for name in names:
                    node_list.remove(name)

        if not found:
            # We don't already have a use for the profiling module so
            # add one.
            reader = FortranStringReader(
                "use psy_data_mod, only: PSyDataType")
            # Tell the reader that the source is free format
            reader.set_format(FortranFormat(True, False))
            use = Fortran2003.Use_Stmt(reader)
            spec_part.content.insert(0, use)

        # Check that we won't have any name-clashes when we insert the
        # symbols required for profiling. This check uses the list of symbols
        # that we created before adding the `use profile_mod...` statement.
        if not self.root.profiling_name_clashes_checked:
            for node in node_list:
                if isinstance(node, Fortran2003.Name):
                    text = str(node).lower()
                    # Check for the symbols we import from the profiling module
                    for symbol in self.profiling_symbols:
                        if text == symbol.lower():
                            raise NotImplementedError(
                                "Cannot add profiling to '{0}' because it "
                                "already contains a symbol that clashes with "
                                "one of those ('{1}') that must be imported "
                                "from the PSyclone profiling module.".
                                format(routine_name, symbol))
                    # Check for the name of the profiling module itself
                    if text == self.fortran_module:
                        raise NotImplementedError(
                            "Cannot add profiling to '{0}' because it already "
                            "contains a symbol that clashes with the name of "
                            "the PSyclone profiling module ('profile_mod')".
                            format(routine_name))
                    # Check for the names of profiling variables
                    if text.startswith(self.profiling_var):
                        raise NotImplementedError(
                            "Cannot add profiling to '{0}' because it already"
                            " contains symbols that potentially clash with "
                            "the variables we will insert for each profiling "
                            "region ('{1}*').".format(routine_name,
                                                      self.profiling_var))
        # Flag that we have now checked for name clashes so that if there's
        # more than one profiling node we don't fall over on the symbols
        # we've previous inserted.
        self.root.profiling_name_clashes_checked = True

        # Create a name for this region by finding where this profiling
        # node is in the list of profiling nodes in this Invoke.
        sched = self.root
        pnodes = sched.walk(ProfileNode)
        region_idx = pnodes.index(self)
        region_name = "r{0}".format(region_idx)
        var_name = "psy_data{0}".format(region_idx)

        # Create a variable for this profiling region
        reader = FortranStringReader(
            "type(PSyDataType), save :: {0}".format(var_name))
        # Tell the reader that the source is free format
        reader.set_format(FortranFormat(True, False))
        decln = Fortran2003.Type_Declaration_Stmt(reader)
        spec_part.content.append(decln)

        # Find the parent in the parse tree - first get a pointer to the
        # AST for the content of this region.
        content_ast = self.profile_body.children[0].ast
        # Now store the parent of this region
        fp_parent = content_ast._parent
        # Find the location of the AST of our first child node in the
        # list of child nodes of our parent in the fparser parse tree.
        ast_start_index = object_index(fp_parent.content,
                                       content_ast)
        # Finding the location of the end is harder as it might be the
        # end of a clause within an If or Select block. We therefore
        # work back up the fparser2 parse tree until we find a node that is
        # a direct child of the parent node.
        ast_end_index = None
        if self.profile_body[-1].ast_end:
            ast_end = self.profile_body[-1].ast_end
        else:
            ast_end = self.profile_body[-1].ast
        # Keep a copy of the pointer into the parse tree in case of errors
        ast_end_copy = ast_end

        while ast_end_index is None:
            try:
                ast_end_index = object_index(fp_parent.content,
                                             ast_end)
            except ValueError:
                # ast_end is not a child of fp_parent so go up to its parent
                # and try again
                if hasattr(ast_end, "_parent") and ast_end._parent:
                    ast_end = ast_end._parent
                else:
                    raise InternalError(
                        "Failed to find the location of '{0}' in the fparser2 "
                        "Parse Tree:\n{1}\n".format(str(ast_end_copy),
                                                    str(fp_parent.content)))

        # Add the profiling-end call
        reader = FortranStringReader(
            "CALL {0}%PostEnd".format(var_name))
        # Tell the reader that the source is free format
        reader.set_format(FortranFormat(True, False))
        pecall = Fortran2003.Call_Stmt(reader)
        fp_parent.content.insert(ast_end_index+1, pecall)

        # Add the profiling-start call
        reader = FortranStringReader(
            "CALL {2}%PreStart('{0}', '{1}', 0, 0)".format(
                routine_name, region_name, var_name))
        reader.set_format(FortranFormat(True, False))
        pscall = Fortran2003.Call_Stmt(reader)
        fp_parent.content.insert(ast_start_index, pscall)
