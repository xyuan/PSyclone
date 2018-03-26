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

from psyclone.psyGen import Schedule, Loop, Node, \
    SCHEDULE_COLOUR_MAP as _BASE_CMAP

# The base colour map doesn't have CodeBlock as that is currently
# a NEMO-API-specific entity.
import copy
NEMO_SCHEDULE_COLOUR_MAP = copy.deepcopy(_BASE_CMAP)
NEMO_SCHEDULE_COLOUR_MAP["CodeBlock"] = "red"

# The loop index variables we expect NEMO to use
NEMO_LOOP_VARS = ["ji", "jj", "jk"]

# The valid types of loop.
VALID_LOOP_TYPES = ["lon", "lat", "levels", "tracers"]

# Mapping from loop variable to loop type
NEMO_LOOP_TYPE_MAPPING = {"ji": "lon", "jj": "lat", "jk": "levels",
                          "jt": "tracers", "jn": "tracers"}


class NemoSchedule(Schedule):
    '''
    '''

    def __init__(self, xdom):
        '''
        :param xdom: :py:class:`xml.dom.minidom.Document`
        '''
        from psyclone.psyGen import Invoke
        Node.__init__(self)
        # TODO this is a fake Invoke as we don't have that
        # concept here
        self._invoke = Invoke(None, None, NemoSchedule)

        root = xdom.firstChild
        # Store a pointer into the XML tree
        self._xml_node = xdom

        # Find the beginning of the executable section
        func_nodes = root.getElementsByTagName("FfunctionDefinition")
        func = func_nodes[0]
        body_nodes = func.getElementsByTagName("body")
        body = body_nodes[0]

        # List of nodes we will use to create 'codeBlocks' that we don't
        # attempt to understand
        code_block_nodes = []

        for child in body.childNodes:
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
        return


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
        self._xml_node = xnode

        vars = xnode.getElementsByTagName("Var")
        loop_var = text_value(vars[0])

        if loop_var in NEMO_LOOP_TYPE_MAPPING:
            self.loop_type = NEMO_LOOP_TYPE_MAPPING[loop_var]
        else:
            self.loop_type = "unknown"


class NEMOCodeBlock(Node):
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
        return colored("NEMOCodeBlock", NEMO_SCHEDULE_COLOUR_MAP["CodeBlock"])

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


def _add_code_block(parent, statements):
    ''' Create a NEMOCodeBlock for the supplied list of statements
    and then wipe the list of statements '''

    if not statements:
        return None
    
    code_block = NEMOCodeBlock(statements,
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
