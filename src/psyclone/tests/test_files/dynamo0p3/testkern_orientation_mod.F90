! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2017-2020, Science and Technology Facilities Council
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!
! * Redistributions of source code must retain the above copyright notice, this
!   list of conditions and the following disclaimer.
!
! * Redistributions in binary form must reproduce the above copyright notice,
!   this list of conditions and the following disclaimer in the documentation
!   and/or other materials provided with the distribution.
!
! * Neither the name of the copyright holder nor the names of its
!   contributors may be used to endorse or promote products derived from
!   this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
! FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
! COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
! INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
! BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
! LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
! LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
! ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
! POSSIBILITY OF SUCH DAMAGE.
! -----------------------------------------------------------------------------
! Author R. W. Ford, STFC Daresbury Lab
! Modified I. Kavcic, Met Office

module testkern_orientation_mod

  use argument_mod
  use fs_continuity_mod
  use kernel_mod
  use constants_mod

  implicit none

  type, extends(kernel_type) :: testkern_orientation_type
    type(arg_type) :: meta_args(3) = (/                                  &
         arg_type(GH_FIELD,   GH_WRITE, W3),                             &
         arg_type(GH_FIELD,   GH_READ,  W2),                             &
         arg_type(GH_FIELD*3, GH_READ,  W0)                              &
         /)
    type(func_type) :: meta_funcs(3) = (/                                &
         func_type(W3, GH_BASIS),                                        &
         func_type(W2, GH_BASIS, GH_DIFF_BASIS, GH_ORIENTATION),         &
         func_type(W0, GH_BASIS, GH_DIFF_BASIS)                          &
         /)
    integer :: operates_on = CELL_COLUMN
    integer :: gh_shape = gh_quadrature_XYoZ
  contains
    procedure, nopass :: code => testkern_orientation_code
  end type testkern_orientation_type

contains

  subroutine testkern_orientation_code(nlayers, fld1, fld2,               &
                                       fld3_v1, fld3_v2, fld3_v3,         &
                                       ndf_w3, undf_w3, map_w3, basis_w3, &
                                       ndf_w2, undf_w2, map_w2, basis_w2, &
                                       diff_basis_w2, orientation_w2,     &
                                       ndf_w0, undf_w0, map_w0, basis_w0, &
                                       diff_basis_w0, np_xy, np_z,        &
                                       weights_xy, weights_z)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w0
    integer(kind=i_def), intent(in) :: ndf_w2
    integer(kind=i_def), intent(in) :: ndf_w3
    integer(kind=i_def), intent(in) :: undf_w3, undf_w2, undf_w0
    integer(kind=i_def), intent(in) :: np_xy, np_z
    integer(kind=i_def), intent(in), dimension(ndf_w2) :: orientation_w2
    integer(kind=i_def), intent(in), dimension(ndf_w0) :: map_w0
    integer(kind=i_def), intent(in), dimension(ndf_w2) :: map_w2
    integer(kind=i_def), intent(in), dimension(ndf_w3) :: map_w3
    real(kind=r_def), intent(out), dimension(undf_w3) :: fld1
    real(kind=r_def), intent(in), dimension(undf_w2)  :: fld2
    real(kind=r_def), intent(in), dimension(undf_w0)  :: fld3_v1
    real(kind=r_def), intent(in), dimension(undf_w0)  :: fld3_v2
    real(kind=r_def), intent(in), dimension(undf_w0)  :: fld3_v3
    real(kind=r_def), intent(in), dimension(1,ndf_w3,np_xy,np_z) :: basis_w3
    real(kind=r_def), intent(in), dimension(3,ndf_w2,np_xy,np_z) :: basis_w2
    real(kind=r_def), intent(in), dimension(1,ndf_w2,np_xy,np_z) :: diff_basis_w2
    real(kind=r_def), intent(in), dimension(1,ndf_w0,np_xy,np_z) :: basis_w0
    real(kind=r_def), intent(in), dimension(3,ndf_w0,np_xy,np_z) :: diff_basis_w0
    real(kind=r_def), intent(in), dimension(np_xy) :: weights_xy
    real(kind=r_def), intent(in), dimension(np_z)  :: weights_z

  end subroutine testkern_orientation_code

end module testkern_orientation_mod
