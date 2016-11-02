'''The LFRic driver file. This file contains any LFRic specific
classes used to describe and generate an LFRic driver.'''

from Fortran import Program


class LFRicDriver(Program):
    '''Create an LFRic driver object. This is assumed to be a Fortran
    program. At the moment there are no specific LFRic specialisations'''
    def __init__(self):
        Program.__init__(self)
