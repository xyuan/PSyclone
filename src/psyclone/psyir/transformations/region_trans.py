# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2019, Science and Technology Facilities Council.
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
#        J. Henrichs, Bureau of Meteorology
# Modified I. Kavcic, Met Office

'''This module contains the base class RegionTrans.
'''

import abc
import six

from psyclone.psyGen import Kern, Schedule, Transformation


@six.add_metaclass(abc.ABCMeta)
class RegionTrans(Transformation):
    # Avoid pylint warning about abstract functions (apply, name) not
    # overwritten:
    # pylint: disable=abstract-method,arguments-differ
    '''
    This abstract class is a base class for all transformations that act
    on a list of nodes. It gives access to a _validate function that
    makes sure that the nodes in the list are in the same order as in
    the original AST, no node is duplicated, and that all nodes have
    the same parent. We also check that all nodes to be enclosed are
    valid for this transformation - this requires that the sub-class
    populate the `valid_node_types` tuple.

    '''
    # The types of Node that we support within this region. Must be
    # populated by sub-class.
    valid_node_types = ()

    def validate(self, node_list, options=None):
        '''
        Checks that the nodes in node_list are valid for a region
        transformation.

        :param node_list: list of PSyIR nodes or a single Schedule.
        :type node_list: :py:class:`psyclone.psyGen.Schedule` or a \
                         list of :py:class:`psyclone.psyGen.Node`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None
        :param bool options["node-type-check"]: this flag controls if the \
                type of the nodes enclosed in the region should be tested \
                to avoid using unsupported nodes inside a region.

        :raises TransformationError: if the nodes in the list are not \
                in the original order in which they are in the AST, \
                a node is duplicated or the nodes have different parents.
        :raises TransformationError: if any of the nodes to be enclosed in \
                the region are of an unsupported type.
        :raises TransformationError: if the parent of the supplied Nodes is \
                                     not a Schedule or a Directive.
        :raises TransformationError: if the nodes are in a NEMO Schedule and \
                                     the transformation acts on the child of \
                                     a single-line If or Where statment.

        '''
        # pylint: disable=too-many-branches
        from psyclone.psyGen import IfBlock, Loop
        from psyclone.nemo import NemoInvokeSchedule
        from psyclone.psyir.transformations import TransformationError
        if not options:
            options = {}
        node_parent = node_list[0].parent
        prev_position = -1
        for child in node_list:
            if child.parent is not node_parent:
                raise TransformationError(
                    "Error in {0} transformation: supplied nodes "
                    "are not children of the same parent."
                    .format(self.name))
            if prev_position >= 0 and prev_position+1 != child.position:
                raise TransformationError(
                    "Children are not consecutive children of one parent: "
                    "child '{0}' has position {1}, but previous child had "
                    "position {2}."
                    .format(str(child), child.position, prev_position))
            prev_position = child.position

        # Check that the proposed region contains only supported node types
        if options.get("node-type-check", True):
            for child in node_list:
                # Stop at any instance of Kern to avoid going into the
                # actual kernels, e.g. in Nemo inlined kernels
                flat_list = [item for item in child.walk(object, Kern)
                             if not isinstance(item, Schedule)]
                for item in flat_list:
                    if not isinstance(item, self.valid_node_types):
                        raise TransformationError(
                            "Nodes of type '{0}' cannot be enclosed by a {1} "
                            "transformation".format(type(item), self.name))

        # If we've been passed a list that contains one or more Schedules
        # then something is wrong. e.g. two Schedules that are both children
        # of an IfBlock would imply that the transformation is being applied
        # around both the if-body and the else-body and that doesn't make
        # sense.
        if isinstance(node_list, list) and len(node_list) > 1 and \
           any([isinstance(node, Schedule) for node in node_list]):
            raise TransformationError(
                "Cannot apply a transformation to multiple nodes when one or "
                "more is a Schedule. Either target a single Schedule or the"
                " children of a Schedule.")

        # Sanity check that we've not been passed the condition part of
        # an If statement or the bounds of a Loop. If the parent node is
        # a Loop of IfBlock then we can only accept a single Schedule.
        # TODO #542 Once everything has a Schedule we can tidy this up
        # a little by requiring that either the parent be a Schedule or
        # that the node-list consists of a single Schedule.
        if isinstance(node_parent, (Loop, IfBlock)) and \
           not isinstance(node_list[0], Schedule):
            # We've already checked for lists with len > 1 that contain a
            # Schedule above so if the first item is a Schedule then that's
            # all the list contains.
            raise TransformationError(
                "Cannot apply transformation to the immediate children of a "
                "Loop/IfBlock unless it is to a single Schedule representing"
                " the Loop/If/Else body.")

        # The checks below this point only apply to the NEMO API and can be
        # removed once #435 is done.
        node = node_list[0]
        if not isinstance(node.root, NemoInvokeSchedule):
            return

        if_or_loop = node.ancestor((IfBlock, Loop))
        if if_or_loop and ("was_single_stmt" in if_or_loop.annotations
                           or "was_where" in if_or_loop.annotations):
            # This limitation is because the NEMO API currently relies on
            # manipulation of the fparser2 parse tree
            # TODO #435.
            raise TransformationError(
                "In the NEMO API a transformation cannot be applied to the "
                "children of either a single-line if statement or a PSyIR loop"
                " representing a WHERE construct.")