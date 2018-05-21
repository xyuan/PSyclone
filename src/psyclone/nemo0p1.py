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
# ----------------------------------------------------------------------------
# Authors R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''
This module implements the PSyclone NEMO 0.1 API. It holds all
functionality related to the construction of a Schedule
representing a NEMO algorithm.
'''

from psyclone.psyGen import PSy, Invoke, Invokes, Schedule, Loop, Node, \
    SCHEDULE_COLOUR_MAP as _BASE_CMAP, Kern

# The base colour map doesn't have CodeBlock as that is currently
# a NEMO-API-specific entity.
import copy
NEMO_SCHEDULE_COLOUR_MAP = copy.deepcopy(_BASE_CMAP)
NEMO_SCHEDULE_COLOUR_MAP["CodeBlock"] = "red"

# The loop index variables we expect NEMO to use
NEMO_LOOP_VARS = ["ji", "jj", "jk"]

# The valid types of loop.
VALID_LOOP_TYPES = ["lon", "lat", "levels", "tracers"]

# Mapping from loop variable to loop type. This relies upong the NEMO
# coding conventions being adhered to.
NEMO_LOOP_TYPE_MAPPING = {"ji": "lon", "jj": "lat", "jk": "levels",
                          "jt": "tracers", "jn": "tracers"}


class NemoInvoke(Invoke):

    def __init__(self, ast, name):
        '''
        :param ast: The AST for the Fortran code to process
        :type ast: :py:class:`xml.dom.minidom.Node`
        :param str name: The name of the program unit
        '''
        self._schedule = None
        self._name = name
        self._psy_unique_vars = ["a_variable"]
        # Store the whole DOM
        self._ast = ast

        # Find the section of the tree containing the execution part
        # of the code
        # TODO allow for empty subroutines!
        body_nodes = ast.getElementsByTagName("body")
        exe_part = body_nodes[0]

        # Identify whether we have a Program, a Subroutine or a Function
        #self._routine_type = get_routine_type(ast)

        # Store the root of this routine's specification in the AST
        #self._spec_part = get_child(ast, Specification_Part)

        # We now walk through the AST and construct a Schedule view
        # using objects from the nemo0p1 module.
        self._schedule = NemoSchedule(self, exe_part)

    def gen(self):
        return str(self._ast)

    def gen_xml(self):
        '''
        Generates the updated DOM for this invoke
        '''
        if not self._schedule:
            return

        self.schedule.gen_xml()

    @property
    def psy_unique_var_names(self):
        return self._psy_unique_vars


class NemoInvokes(Invokes):

    def __init__(self, ast):
        '''
        :param ast:
        :type ast: :py:class:`xml.dom.minidom.Dom`
        '''        
        self.invoke_map = {}
        self.invoke_list = []
        # Keep a pointer to the whole AST
        self._ast = ast

        root = ast.firstChild
        # Find all the subroutines contained in the file
        routines = root.getElementsByTagName("FfunctionDefinition")

        # TODO Do we need to treat the main program as a special case?

        # Analyse each routine we've found
        idx = 0
        for subroutine in routines:
            # Get the name of this (sub)routine
            sub_name = "invoke_{0}".format(idx) # TODO
            idx += 1

            my_invoke = NemoInvoke(subroutine, name=sub_name)
            self.invoke_map[sub_name] = my_invoke
            self.invoke_list.append(my_invoke)

    def gen_xml(self):
        '''
        Generate the current XML DOM
        '''
        for invoke in self.invoke_list:
            invoke.gen_xml()
        return self._ast


class NemoPSy(PSy):
    ''' The NEMO 0.1-specific PSy class. This creates a NEMO-specific
        invokes object (which controls all the required invocation calls).
        Also overrides the PSy gen method so that we generate GOcean-
        specific PSy module code. '''
    
    def __init__(self, ast):

        self._name = "Nemo-PSY"  # TODO use a meaningful name
        self._ast = ast  # The XML DOM for this PSy object
        self._invokes = NemoInvokes(ast)
        
    def inline(self, module):
        # Override base-class method because we don't yet support it
        pass

    @property
    def gen(self):
        '''
        Generate PSy code for the NEMO api v.1.0.

        :rtype: ast

        '''
        # We don't need to create anything here because the original
        # AST (the XML DOM) already contains the full definition of
        # the parent module

        # In contrast to all of the other APIs, we call gen_xml() rather
        # than gen_code().
        return self.invokes.gen_xml()


class NemoSchedule(Schedule):
    '''
    '''

    def __init__(self, invoke, xdom=None):
        '''
        :param invoke: the Invoke to which this Schedule belongs
        :type invoke: :py:class:`psyclone.nemo0p1.NemoInvoke`
        :param xdom: Node representing body of Fortran routine
        :type xdom: :py:class:`xml.dom.minidom.Node`
        '''
        Node.__init__(self)

        self._invoke = invoke

        if not xdom:
            # This Schedule will be populated by a subsequent call
            # to load()
            # TODO - implement load()
            return

        # Store a pointer into the XML tree
        self._xml_node = xdom

        # List of nodes we will use to create 'codeBlocks' that we don't
        # attempt to understand
        code_block_nodes = []

        for child in xdom.childNodes:
            if child.nodeType == child.TEXT_NODE:
                # Skip over text nodes
                continue
#child.nodeType == child.ELEMENT_NODE and \
            if child.tagName == "FdoStatement":
                # The start of a loop is taken as the end of any exising
                # code block so we create that now
                _add_code_block(self, code_block_nodes)
                self.addchild(NemoLoop(child, parent=self))
            else:
                code_block_nodes.append(child)

        # Finish any open code block
        _add_code_block(self, code_block_nodes)

        return

    def gen_xml(self):
        '''
        Generate the updated DOM for this Schedule
        '''
        for child in self.children:
            child.gen_xml()


class NemoLoop(Loop):
    '''
    '''
    def __init__(self, xnode, parent=None):
        '''
        :param xnode: :py:class:`xml.dom.minidom.xxxxx`
        '''
        from psyclone.psyGen import Loop
        Loop.__init__(self, parent=parent,
                      valid_loop_types=VALID_LOOP_TYPES)
        # Keep a ptr to the corresponding node in the XCodeML DOM
        self._xml_node = xnode

        # Get the loop variable
        vars = xnode.getElementsByTagName("Var")
        self._loop_var = text_value(vars[0])

        # Identify the type of loop
        if self._loop_var in NEMO_LOOP_TYPE_MAPPING:
            self.loop_type = NEMO_LOOP_TYPE_MAPPING[self._loop_var]
        else:
            self.loop_type = "unknown"

        # List of nodes we will use to create 'code blocks' that we don't
        # attempt to understand
        code_block_nodes = []

        # Find the body of the loop
        loop_body = xnode.getElementsByTagName("body")[0]

        # Is this loop body a kernel?
        if NemoKern.is_kernel(loop_body):
            kern = NemoKern()
            kern.load(loop_body, parent=self)
            self.addchild(kern)
            return

        for child in loop_body.childNodes:
            if child.nodeType == child.TEXT_NODE:
                # Skip over text nodes
                continue
            if child.tagName == "FdoStatement":
                # The start of a loop is taken as the end of any exising
                # code block so we create that now
                _add_code_block(self, code_block_nodes)
                self.addchild(NemoLoop(child, parent=self))
            else:
                print child.tagName
                code_block_nodes.append(child)

        # Finish any open code block
        _add_code_block(self, code_block_nodes)

    def gen_xml(self):
        return


class NemoCodeBlock(Node):
    '''
    Node representing some generic Fortran code that PSyclone
    does not attempt to manipulate
    '''

    def __init__(self, statements, parent=None):
        Node.__init__(self, parent=parent)
        # Store a list of the parser objects holding the code
        # associated with this block
        self._statements = statements[:]

    @property
    def coloured_text(self):
        '''
        Return the name of this node type with control codes for
        terminal colouring
        :return: Name of node + control chars for colour
        :rtype: string
        '''
        from psyclone.psyGen import colored
        return colored("NemoCodeBlock", NEMO_SCHEDULE_COLOUR_MAP["CodeBlock"])

    def view(self, indent=0):
        ''' Print a representation of this node in the schedule '''
        print self.indent(indent) + self.coloured_text + "[" + \
            str(type(self._statements[0])) + "]"
        for entity in self._children:
            entity.view(indent=indent + 1)

    def __str__(self):
        return "CodeBlock[{0} statements]".format(len(self._statements))

    def gen_code(self, parent):
        ''' Convert this code block back to Fortran '''
        for statement in self._statements:
            # TODO each statement is an item from the fparser2 AST but
            # parent.add expects an f2pygen object.
            parent.add(statement)
        for entity in self._children:
            entity.gen_code(parent)

    def gen_xml(self):
        # Don't have to take any action for a code block since we leave
        # its content unchanged
        return


class NemoKern(Kern):
    ''' Stores information about NEMO kernels as extracted from the
    NEMO code. '''
    def __init__(self):
        ''' Create an empty NemoKern object. The object is given state via
        the load method '''
        # Create those member variables required for testing and to keep
        # pylint happy
        self._children = []
        self._name = ""
        # The Loop object created by fparser2 which holds the AST for the
        # section of code associated with this kernel
        self._loop = None
        # List of the loop variables, one for each loop
        self._loop_vars = []
        # A list of 2-tuples, one for each loop
        self._loop_ranges = []
        # List of variable names that must be thread-private
        self._private_vars = None
        # List of variable names that must be first-private because they
        # are scalars with a first access of read
        self._first_private_vars = None
        # Whether or not this kernel performs a reduction
        self._reduction = False
        # List of variables that are shared between threads
        self._shared_vars = None
        # Type of kernel (2D, 3D..)
        self._kernel_type = ""
        self._body = []
        # Will point to the corresponding node in the XCodeML dom
        self._xml_node = None

    @property
    def type(self):
        ''' Returns what type of kernel this is '''
        return self._kernel_type

    def load(self, node, parent=None):
        '''
        Populate the state of this NemoKern object

        :param node:
        :param parent:
        '''
        self._xml_node = node

        if parent and isinstance(parent, NemoLoop):
            self._load_from_loop(node, parent=parent)
#        elif isinstance(loop, Assignment_Stmt):
#            self._load_from_implicit_loop(loop, parent=parent)
#        else:
#            raise ParseError(
#                "Internal error: expecting either "
#                "Block_Nonlabel_Do_Construct or Assignment_Stmt but got "
#                "{0}".format(str(type(loop))))

    def _load_from_loop(self, loop, parent=None):
        ''' Populate the state of this NemoKern object from the body
        of a loop in the XCodeML IR '''

        # Keep a pointer to the original loop in the AST
        self._loop = loop

        # TODO Loop variables and ranges are properties of NemoLoop(s) now?
        #for ctrl in ctrls:
        #    self._loop_vars.append(str(ctrl.items[0]))
        #    self._loop_ranges.append( (str(ctrl.items[1][0]),
        #                               str(ctrl.items[1][1])) )

        # Now we find the content of this nested loop
        #nested_loops = walk_ast(loop.content, [Block_Nonlabel_Do_Construct])
        #inner_loop = nested_loops[-1]
        #if not isinstance(inner_loop.content[0], Nonlabel_Do_Stmt):
        #    raise ParseError("Internal error, expecting Nonlabel_Do_Stmt as "
        #                     "first child of Block_Nonlabel_Do_Construct but "
        #                     "got {0}".format(type(inner_loop.content[0])))

        # TODO not sure I need to store the kernel body like this?
        self._body = []
        for content in loop.childNodes:
            self._body.append(content)

        # I could get this by walking back up the tree and counting how
        # many NemoLoops I have as ancestors before I come across
        # something that is not a loop...
        #if len(self._loop_vars) == 2:
        #    self._kernel_type = "2D"
        #else:
        #    self._kernel_type = "3D"

        # Analyse the loop body to identify private and shared variables
        # TODO how to do this in the absence of Habakkuk for XCodeML?
        #from habakkuk.make_dag import dag_of_code_block
        # Create a DAG of the kernel code block using Habakkuk
        #kernel_dag = dag_of_code_block(inner_loop, "nemo_kernel")
        #inputs = kernel_dag.input_nodes()
        #outputs = kernel_dag.output_nodes()
        #print "Kernel has {0} outputs: ".format(len(outputs)) + \
        #    ",".join([node.variable.orig_name for node in outputs])
        self._shared_vars = set()
        self._first_private_vars = set()
        self._private_vars = set()
        # If there are scalar variables that are inputs to the DAG (other than
        # the loop counters) then they must be declared first-private.
        #for node in inputs:
        #    if not node.node_type:
        #        if node.name not in NEMO_LOOP_VARS:
        #            self._first_private_vars.add(node.name)
        #for key, node in kernel_dag._nodes.iteritems():
        #    if node.node_type == "array_ref":
        #        self._shared_vars.add(node.variable.orig_name)
        #    elif not node.node_type:
        #        self._private_vars.add(node.variable.orig_name)
        #self._private_vars -= self._first_private_vars
        #print "OpenMP shared vars: " + ",".join(self._shared_vars)
        #print "OpenMP private vars: " + ",".join(self._private_vars)
        #print "OpenMP first-private vars: " + \
        #    ",".join(self._first_private_vars)
        return
    
    def _load_from_implicit_loop(self, loop, parent=None):
        ''' Populate the state of this NemoKern object from an fparser2
        AST for an implicit loop (Fortran array syntax) '''
        from fparser.Fortran2003 import Section_Subscript_List
        # TODO implement this method!
        self._kernel_type = "Implicit"
        return

    @property
    def loop(self):
        ''' Returns the Fortran2003 loop object associated with this kernel '''
        return self._loop

    def tofortran(self, tab='', isfix=False):
        ''' Returns a string containing the Fortran representation of this
        kernel '''
        fort_lines = []
        tablen = len(tab)
        for idx, loop_var in enumerate(self._loop_vars):
            fort_lines.append(tablen*" "+"DO {0} = {1}, {2}".
                              format(loop_var,
                                     self._loop_ranges[idx][0],
                                     self._loop_ranges[idx][1]))
            tablen += 2
        for item in self._body:
            fort_lines.append(item.tofortran(tab=tablen*" ", isfix=isfix))
        for loop_var in self._loop_vars:
            tablen -= 2
            fort_lines.append(tablen*" "+"END DO")
        return "\n".join(fort_lines)

    def local_vars(self):
        '''Return a list of the variable (names) that are local to this loop
        (and must therefore be e.g. threadprivate if doing OpenMP)

        '''
        return []

    def view(self, indent=0):
        ''' Print representation of this node to stdout '''
        print (self.indent(indent) + self.coloured_text + "[" +
               self._kernel_type + "]")
        for entity in self._children:
            entity.view(indent=indent + 1)

    def gen_code(self, parent):
        '''
        Create the node(s) in the f2pygen AST that will generate the code
        for this object

        :param parent: parent node in the f2pygen AST
        :type parent: :py:class:`psyclone.f2pygen.DoGen`
        '''
        from psyclone.f2pygen2 import AssignGen
        for item in self._body:
            parent.add(AssignGen(item))

    @staticmethod
    def is_kernel(node):
        '''
        :param node: Node in XCodeML document to check
        :type node: xml.minidom.XXXX
        :returns: True if this node conforms to the rules for a kernel
        :rtype: bool
        '''
        child_loops = node.getElementsByTagName("FdoStatement")
        if child_loops:
            # A kernel cannot contain other loops
            return False

        # A kernel cannot contain writes or reads
        writes = node.getElementsByTagName("FwriteStatement")
        if writes:
            return False

        reads = node.getElementsByTagName("FreadStatement")
        if reads:
            return False

        return True


def _add_code_block(parent, statements):
    ''' Create a NemoCodeBlock for the supplied list of statements
    and then wipe the list of statements '''

    if not statements:
        return None
    
    code_block = NemoCodeBlock(statements,
                               parent=parent)
    parent.addchild(code_block)
    statements = []
    return code_block


def has_nontext_children(node):
    '''
    Checks whether the supplied node has any child nodes that are
    not text nodes
    :param node: node in XML document to check
    :type node: :py:class:`xml.dom.minidom.Node`
    :returns: True if a non-text child node is found, False otherwise
    :rtype: bool
    '''
    for child in node.childNodes:
        if child.nodeType == node.ELEMENT_NODE:
            return True
    return False


def text_value(node):
    '''
    :param node: node in XML document
    :type node: :py:class:`xml.dom.minidom.Node`
    :returns: the text contained in the node
    :rtype: str
    '''
    # TODO implement error checking
    return node.firstChild.data
