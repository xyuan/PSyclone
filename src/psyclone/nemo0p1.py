from psyclone.psyGen import Schedule, Loop

class NemoSchedule(Schedule):
    '''
    '''

    def __init__(self, xdom):
        '''
        :param xdom: :py:class:`xml.dom.minidom.Document`
        '''
        from psyclone.psyGen import Invoke, Node
        Node.__init__(self)
        # TODO this is a fake Invoke as we don't have that
        # concept here
        self._invoke = Invoke(None, None, NemoSchedule)

        root = xdom.firstChild
        # Store a pointer into the XML tree
        self._xml_node = xdom
        do_loops = root.getElementsByTagName("FdoStatement")
        for loop in do_loops:
            vars = loop.getElementsByTagName("Var")
            var_name = text_value(vars[0])
            if var_name in ["jk", "jj", "ji"]:
                self.addchild(NemoLoop(loop))

class NemoLoop(Loop):
    '''
    '''
    def __init__(self, xnode):
        '''
        :param xnode: :py:class:`xml.dom.minidom.xxxxx`
        '''
        from psyclone.psyGen import Loop
        Loop.__init__(self)
        self._xml_node = xnode


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
