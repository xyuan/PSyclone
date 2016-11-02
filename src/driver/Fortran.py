'''This file describes fortran code in a simple hierarchical way. It
allows fortran code generation using f2pygen but will be extended to
link to existing fortran and, in the future, will support the new
parser'''

from psyGen import Node


class NamedObject(object):
    ''' This baseclass is used to add a name method to a class '''
    def __init__(self):
        self._name = None

    @property
    def name(self):
        ''' return the name '''
        return self._name

    @name.setter
    def name(self, name):
        ''' set the name '''
        self._name = name


class Program(NamedObject):
    ''' keeps information about a fortran program '''
    def __init__(self):
        NamedObject.__init__(self)
        self._schedule = Schedule()
        self._declarations = []
        self._uses = []

    @property
    def schedule(self):
        ''' return the program's schedule '''
        return self._schedule

    @property
    def declarations(self):
        ''' return the program's data declarations '''
        return self._declarations

    @property
    def uses(self):
        ''' return any explicitly specified use statements. Note, use
        statements may also be created on the fly by a Call object'''
        return self._uses

    def gen(self):
        ''' generate the program code '''
        from f2pygen import ProgramGen
        driver_program = ProgramGen(self._name)
        for use in self._uses:
            use.gen_code(driver_program)
        for data in self._declarations:
            data.datatype.gen_code(driver_program, data.name)
        if self._schedule:
            self._schedule.gen_code(driver_program)
        return str(driver_program.root)


class Module(NamedObject):
    ''' keeps information about a fortran module '''
    def __init__(self):
        NamedObject.__init__(self)
        self._subroutines = []

    @property
    def subroutines(self):
        ''' return the module's subroutines '''
        return self._subroutines


class Use(NamedObject):
    ''' keeps information about a fortran Use statement '''
    def __init__(self):
        NamedObject.__init__(self)
        self._only_names = []

    @property
    def only_names(self):
        '''return any subroutine or function names explicitely specified for
        this use stattement'''
        return self._only_names

    def gen_code(self, parent):
        '''generate the appropriate Use statement code with the context of
        the parent'''
        from f2pygen import UseGen
        parent.add(UseGen(parent, name=self._name,
                          funcnames=self._only_names,
                          only=bool(self._only_names)))


class Subroutine(NamedObject):
    ''' keeps information about a fortran subroutine statement '''
    def __init__(self):
        NamedObject.__init__(self)
        self._arguments = []

    @property
    def arguments(self):
        ''' return the subroutines' arguments '''
        return self._arguments


class Data(object):
    ''' keeps information about fortran data '''
    def __init__(self):
        self._datatype = None

    @property
    def datatype(self):
        ''' return the datatype object associated with this data '''
        return self._datatype

    @datatype.setter
    def datatype(self, datatype):
        ''' set the datatype object associated with this data '''
        self._datatype = datatype


class Variable(Data, NamedObject):
    ''' keeps information about a fortran variable '''
    def __init__(self):
        Data.__init__(self)
        NamedObject.__init__(self)


class DerivedType(NamedObject):
    ''' keeps information about a fortran derived type '''
    def __init__(self):
        NamedObject.__init__(self)

    def gen_code(self, parent, varname):
        '''generate a type declaration for a variable of this type'''
        from f2pygen import TypeDeclGen
        parent.add(TypeDeclGen(parent, datatype=self.name,
                               entity_decls=[varname]))


class IntrinsicType(object):
    ''' keeps information about a fortran intrinsic type '''
    def __init__(self):
        self._datatype = None
        self._intent = ""
        self._pointer = False
        self._kind = ""
        self._dimension = ""
        self._allocatable = False

    @property
    def datatype(self):
        ''' return the datatype specified by this class '''
        return self._datatype

    @datatype.setter
    def datatype(self, datatype):
        ''' set the datatype specified by this class '''
        self._datatype = datatype

    @property
    def kind(self):
        ''' return the kind value for this datatype '''
        return self._kind

    @kind.setter
    def kind(self, kind):
        ''' set the kind value for this datatype '''
        self._kind = kind

    def gen_code(self, parent, varname):
        '''generate a declaration for a variable of this type'''
        from f2pygen import DeclGen
        parent.add(DeclGen(parent, datatype=self._datatype,
                           entity_decls=[varname], intent=self._intent,
                           pointer=self._pointer, kind=self._kind,
                           dimension=self._dimension,
                           allocatable=self._allocatable))


class Schedule(Node):
    ''' keeps information about a schedule of statements '''
    def __init__(self):
        Node.__init__(self)

    def gen_code(self, parent):
        ''' generate this schedule '''
        for entity in self._children:
            entity.gen_code(parent)


class Loop(Node):
    ''' keeps information about a fortran loop '''
    def __init__(self):
        Node.__init__(self)
        self._variable = None
        self._start = None
        self._stop = None

    @property
    def start(self):
        ''' return the start index of the loop'''
        return self._start

    @start.setter
    def start(self, start):
        ''' set the start index of the loop'''
        self._start = start

    @property
    def stop(self):
        ''' return the stop index of the loop'''
        return self._stop

    @stop.setter
    def stop(self, stop):
        ''' set the start index of the loop'''
        self._stop = stop

    @property
    def variable(self):
        ''' return the loop index variable '''
        return self._variable

    @variable.setter
    def variable(self, variable):
        ''' set the loop index variable '''
        self._variable = variable

    def set_bounds(self, start, stop):
        ''' set the start and stop loop bounds in one call '''
        self._start = start
        self._stop = stop

    def gen_code(self, parent):
        ''' generate the specified loop code '''
        from f2pygen import DoGen
        do_loop = DoGen(parent, self._variable.name, self._start, self._stop)
        parent.add(do_loop)
        for child in self.children:
            child.gen_code(do_loop)


class Call(Node, NamedObject):
    ''' keeps information about a fortran call '''
    def __init__(self):
        Node.__init__(self)
        NamedObject.__init__(self)
        self._arguments = []
        self._module = None

    @property
    def arguments(self):
        ''' return the arguments used by this call '''
        return self._arguments

    @property
    def module(self):
        ''' return the module associated with this call (if any). '''
        return self._module

    @module.setter
    def module(self, module):
        ''' set the module associated with this call (if there is one) '''
        self._module = module

    def gen_code(self, parent):
        ''' generate the call code and use statement (if the source module
        is specified) '''
        from f2pygen import CallGen
        argnames = []
        for argument in self._arguments:
            argnames.append(argument.name)
        parent.add(CallGen(parent, name=self._name, args=argnames))
        if self._module:
            from f2pygen import UseGen
            parent.add(UseGen(parent, name=self._module.name,
                              funcnames=[self._name], only=True))
