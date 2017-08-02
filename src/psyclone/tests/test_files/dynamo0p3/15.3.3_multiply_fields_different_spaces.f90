!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author A. R. Porter STFC Daresbury Lab
! Modified I. Kavcic Met Office

program single_invoke

  ! Description: multiply-field point-wise operation specified in an invoke call
  ! where the supplied fields can be deduced to be on different spaces
  use testkern_fs, only: testkern_fs_type
  use inf,         only: field_type
  implicit none
  type(field_type) :: f2, f3, f4, f5,f6, f7, f8
  real(r_def) :: a

  a = 0.5

  call invoke(                                             &
              testkern_fs_type(f2, f3, f4, f5,f6, f7, f8), &
              aX_plus_Y(f3, a, f2)                         &
             )

end program single_invoke
