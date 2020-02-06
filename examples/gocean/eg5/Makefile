# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council
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
# ------------------------------------------------------------------------------
# Author: J. Henrichs, Bureau of Meteorology

F90 ?= gfortran
F90FLAGS ?=

DL_DIR = ../../../external/dl_esm_inf/finite_difference/src
PROFILE_DIR = ../../../lib/profiling/template
KERNELS = init_field_mod.o update_field_mod.o
EXEC = profile_test
all:	$(EXEC)

$(EXEC):	$(KERNELS) alg.o psy.o
	$(F90) $^ -o $(EXEC) -L$(PROFILE_DIR) -ldummy -L $(DL_DIR) -l_fd

alg.f90 psy.f90: test.x90
	psyclone --profile invokes -l -api "gocean1.0" \
			 --config ../../../config/psyclone.cfg \
		     -opsy psy.f90 -oalg alg.f90 test.x90

alg.o:	psy.o 

psy.o:	$(KERNELS)

%.o: %.f90
	$(F90) $(F90FLAGS) -I $(DL_DIR) -I $(PROFILE_DIR) -c $<

clean:
	rm -f *.o *.mod alg.f90 psy.f90 $(EXEC)