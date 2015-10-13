!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

program multi_functions_multi_invokes

  ! Description: multiple invoke calls, each with a single function
  use testkern, only: testkern_type
  use testkern_qr, only: testkern_qr_type
  use inf,      only: field_type
  implicit none
  type(field_type) :: f1, f2, m1, m2
  type(quadrature_rule_type) :: qr

  call invoke(                            &
       testkern_type(f1,f2,m1,m2),        &
       testkern_type(f1,f2,m1,m2),        &
       testkern_qr_type(f1,f2,m1,m2,qr)   &
       )

  call invoke(                            &
       testkern_qr_type(f1,f2,m1,m2,qr),  &
       testkern_qr_type(f1,f2,m1,m2,qr),  &
       testkern_qr_type(f1,f2,m1,m2,qr)   &
       )

end program multi_functions_multi_invokes