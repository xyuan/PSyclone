'''The LFRic algorithm file. This file contains any LFRic specific
classes used to describe and generate an LFRic algorithm.'''

from Fortran import Module


class LFRicAlgorithm(Module):
    '''A description of an LFRic Algorithm. This is implemented as a
    Fortran module so we inherit from that'''
    def __init__(self):
        Module.__init__(self)
