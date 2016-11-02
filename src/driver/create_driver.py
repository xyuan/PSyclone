'''This file gives an example of how we might generate a simple LFRic
driver'''

from algorithm import LFRicAlgorithm
from driver import LFRicDriver
from data import LFRicField, LFRicInteger
from Fortran import Loop, Subroutine, Call

# create any fields
TEMPERATURE = LFRicField()
TEMPERATURE.name = "temp"

# create algorithm descriptions (these would normally pre-exist)
LFRICSWRAD = LFRicAlgorithm()
LFRICSWRAD.name = "swrad_mod"
SWRADSUBROUTINE = Subroutine()
SWRADSUBROUTINE.name = "swrad"
SWRADSUBROUTINE.arguments.append(TEMPERATURE)
LFRICSWRAD.subroutines.append(SWRADSUBROUTINE)

LFRICLWRAD = LFRicAlgorithm()
LFRICLWRAD.name = "lwrad_mod"
LWRADSUBROUTINE = Subroutine()
LWRADSUBROUTINE.name = "lwrad"
LWRADSUBROUTINE.arguments.append(TEMPERATURE)
LFRICLWRAD.subroutines.append(LWRADSUBROUTINE)

# create any scalars
LOOP_INDEX = LFRicInteger()
LOOP_INDEX.name = "i"

# create the driver
DRIVER = LFRicDriver()
DRIVER.name = "driver"

# add fields to the driver so they will be declared
DRIVER.declarations.append(TEMPERATURE)
DRIVER.declarations.append(LOOP_INDEX)

# create a loop and add it to the drivers schedule
TS_LOOP = Loop()
TS_LOOP.start = "1"
TS_LOOP.stop = "10"
TS_LOOP.variable = LOOP_INDEX
DRIVER.schedule.addchild(TS_LOOP)

# create calls to our algorithm modules, both with the same field data
# being passed in, and add them inside the loop
CALL = Call()
CALL.module = LFRICSWRAD
CALL.name = SWRADSUBROUTINE.name
CALL.arguments.append(TEMPERATURE)
TS_LOOP.addchild(CALL)

CALL = Call()
CALL.module = LFRICLWRAD
CALL.name = LWRADSUBROUTINE.name
CALL.arguments.append(TEMPERATURE)
TS_LOOP.addchild(CALL)

# generate the resultant code and print it out
CODE = DRIVER.gen()
print CODE
