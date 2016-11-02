'''The LFRic data file. This file contains any LFRic specific
datatypes.'''

from Fortran import Variable, DerivedType, IntrinsicType


class LFRicField(Variable):
    '''Create an LFRic field object. This is a fortran derived type called
    gh_field'''
    def __init__(self):
        Variable.__init__(self)
        self.datatype = DerivedType()
        self.datatype.name = "gh_field"


class LFRicInteger(Variable):
    '''Create an LFRic integer object. This is a fortran integer of kind
    i_def'''
    def __init__(self):
        Variable.__init__(self)
        self.datatype = IntrinsicType()
        self.datatype.datatype = "integer"
        self.datatype.kind = "i_def"
