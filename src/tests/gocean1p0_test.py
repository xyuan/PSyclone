# ----------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2015.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
# ----------------------------------------------------------------------------
# Author A. R. Porter, STFC Daresbury Lab

'''Tests for PSy-layer code generation that are specific to the
GOcean 1.0 API.'''

from parse import parse
from psyGen import PSyFactory
import pytest
import os
from generator import GenerationError, ParseError

API = "gocean1.0"


def test_field():
    ''' Tests that a kernel call with only fields produces correct code '''
    _, invoke_info = parse(os.path.join(os.path.
                                        dirname(os.path.
                                                abspath(__file__)),
                                        "test_files", "gocean1p0",
                                        "single_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_compute_cu(cu_fld, p_fld, u_fld)\n"
        "      TYPE(r2d_field), intent(inout) :: cu_fld, p_fld, u_fld\n"
        "      INTEGER nx, ny, istop, jstop\n"
        "      nx = p_fld%grid%nx\n"
        "      ny = p_fld%grid%ny\n"
        "      istop = p_fld%grid%simulation_domain%xstop\n"
        "      jstop = p_fld%grid%simulation_domain%ystop\n"
        "      CALL invoke_0_compute_cu_arrays(nx, ny, istop, jstop, "
        "cu_fld%data, p_fld%data, u_fld%data)\n"
        "    END SUBROUTINE invoke_0_compute_cu\n"
        "    SUBROUTINE invoke_0_compute_cu_arrays(nx, ny, istop, jstop, "
        "cu_fld, p_fld, u_fld)\n"
        "      USE compute_cu_mod, ONLY: compute_cu_code\n"
        "      INTEGER, intent(in) :: nx, ny, istop, jstop\n"
        "      REAL(KIND=wp), intent(inout), dimension(nx,ny) :: cu_fld, "
        "p_fld, u_fld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      DO j=2,jstop\n"
        "        DO i=2,istop+1\n"
        "          CALL compute_cu_code(i, j, cu_fld, p_fld, u_fld)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_compute_cu_arrays\n"
        "  END MODULE psy_single_invoke_test")
    print generated_code
    assert generated_code.find(expected_output) != -1


def test_field_no_deref():
    ''' Tests that a kernel call with only fields produces correct code 
    when the de-referencing routine is switched off'''
    _, invoke_info = parse(os.path.join(os.path.
                                        dirname(os.path.
                                                abspath(__file__)),
                                        "test_files", "gocean1p0",
                                        "single_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # Turn-off generation of de-referencing routine
    schedule.deref_routine = False
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_compute_cu(cu_fld, p_fld, u_fld)\n"
        "      USE compute_cu_mod, ONLY: compute_cu_code\n"
        "      TYPE(r2d_field), intent(inout) :: cu_fld, p_fld, u_fld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop, jstop\n"
        "      !\n"
        "      ! Look-up loop bounds\n"
        "      istop = cu_fld%grid%simulation_domain%xstop\n"
        "      jstop = cu_fld%grid%simulation_domain%ystop\n"
        "      !\n"
        "      DO j=2,jstop\n"
        "        DO i=2,istop+1\n"
        "          CALL compute_cu_code(i, j, cu_fld%data, p_fld%data, "
        "u_fld%data)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_compute_cu\n"
        "  END MODULE psy_single_invoke_test")

    assert generated_code.find(expected_output) != -1


def test_two_kernels():
    ''' Tests that an invoke containing two kernel calls with only
    fields as arguments produces correct code '''
    _, invoke_info = parse(os.path.join(os.path.
                                        dirname(os.path.
                                                abspath(__file__)),
                                        "test_files", "gocean1p0",
                                        "single_invoke_two_kernels.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    generated_code = psy.gen

    expected_output = (
        "  MODULE psy_single_invoke_two_kernels\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0(cu_fld, p_fld, u_fld, unew_fld, uold_fld)\n"
        "      TYPE(r2d_field), intent(inout) :: cu_fld, p_fld, u_fld, "
        "unew_fld, uold_fld\n"
        "      INTEGER nx, ny, istop, jstop\n"
        "      nx = p_fld%grid%nx\n"
        "      ny = p_fld%grid%ny\n"
        "      istop = p_fld%grid%simulation_domain%xstop\n"
        "      jstop = p_fld%grid%simulation_domain%ystop\n"
        "      CALL invoke_0_arrays(nx, ny, istop, jstop, cu_fld%data, "
        "p_fld%data, u_fld%data, unew_fld%data, uold_fld%data)\n"
        "    END SUBROUTINE invoke_0\n"
        "    SUBROUTINE invoke_0_arrays(nx, ny, istop, jstop, cu_fld, "
        "p_fld, u_fld, unew_fld, uold_fld)\n"
        "      USE time_smooth_mod, ONLY: time_smooth_code\n"
        "      USE compute_cu_mod, ONLY: compute_cu_code\n"
        "      INTEGER, intent(in) :: nx, ny, istop, jstop\n"
        "      REAL(KIND=wp), intent(inout), dimension(nx,ny) :: cu_fld, "
        "p_fld, u_fld, unew_fld, uold_fld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      DO j=2,jstop\n"
        "        DO i=2,istop+1\n"
        "          CALL compute_cu_code(i, j, cu_fld, p_fld, u_fld)\n"
        "        END DO \n"
        "      END DO \n"
        "      DO j=1,jstop+1\n"
        "        DO i=1,istop+1\n"
        "          CALL time_smooth_code(i, j, u_fld, unew_fld, uold_fld)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_arrays\n"
        "  END MODULE psy_single_invoke_two_kernels")
    print generated_code
    assert str(generated_code).find(expected_output) != -1


def test_two_kernels_no_deref():
    ''' Tests that an invoke containing two kernel calls with only
    fields as arguments produces correct code '''
    _, invoke_info = parse(os.path.join(os.path.
                                        dirname(os.path.
                                                abspath(__file__)),
                                        "test_files", "gocean1p0",
                                        "single_invoke_two_kernels.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # Turn-off generation of de-referencing routine
    schedule.deref_routine = False
    generated_code = psy.gen

    expected_output = (
        "  MODULE psy_single_invoke_two_kernels\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0(cu_fld, p_fld, u_fld, unew_fld, uold_fld)\n"
        "      USE time_smooth_mod, ONLY: time_smooth_code\n"
        "      USE compute_cu_mod, ONLY: compute_cu_code\n"
        "      TYPE(r2d_field), intent(inout) :: cu_fld, p_fld, u_fld, "
        "unew_fld, uold_fld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop, jstop\n"
        "      !\n"
        "      ! Look-up loop bounds\n"
        "      istop = cu_fld%grid%simulation_domain%xstop\n"
        "      jstop = cu_fld%grid%simulation_domain%ystop\n"
        "      !\n"
        "      DO j=2,jstop\n"
        "        DO i=2,istop+1\n"
        "          CALL compute_cu_code(i, j, cu_fld%data, p_fld%data, "
        "u_fld%data)\n"
        "        END DO \n"
        "      END DO \n"
        "      DO j=1,jstop+1\n"
        "        DO i=1,istop+1\n"
        "          CALL time_smooth_code(i, j, u_fld%data, unew_fld%data, "
        "uold_fld%data)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0\n"
        "  END MODULE psy_single_invoke_two_kernels")

    assert str(generated_code).find(expected_output) != -1


def test_grid_property():
    ''' Tests that an invoke containing a kernel call requiring
    a property of the grid produces correct code '''
    _, invoke_info = parse(os.path.join(os.path.
                                        dirname(os.path.
                                                abspath(__file__)),
                                        "test_files", "gocean1p0",
                                        "single_invoke_grid_props.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_with_grid_props_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_next_sshu(cu_fld, u_fld)\n"
        "      TYPE(r2d_field), intent(inout) :: cu_fld, u_fld\n"
        "      INTEGER nx, ny, istop, jstop\n"
        "      nx = u_fld%grid%nx\n"
        "      ny = u_fld%grid%ny\n"
        "      istop = u_fld%grid%simulation_domain%xstop\n"
        "      jstop = u_fld%grid%simulation_domain%ystop\n"
        "      CALL invoke_0_next_sshu_arrays(nx, ny, istop, jstop, "
        "cu_fld%data, u_fld%data, u_fld%grid%tmask, u_fld%grid%area_t, "
        "u_fld%grid%area_u)\n"
        "    END SUBROUTINE invoke_0_next_sshu\n"
        "    SUBROUTINE invoke_0_next_sshu_arrays(nx, ny, istop, jstop, "
        "cu_fld, u_fld, tmask, area_t, area_u)\n"
        "      USE kernel_requires_grid_props, ONLY: next_sshu_code\n"
        "      INTEGER, intent(in) :: nx, ny, istop, jstop\n"
        "      REAL(KIND=wp), intent(inout), dimension(nx,ny) :: cu_fld, "
        "u_fld\n"
        "      REAL(KIND=wp), intent(inout), dimension(nx,ny) :: area_t, "
        "area_u\n"
        "      INTEGER, intent(inout), dimension(nx,ny) :: tmask\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      DO j=2,jstop\n"
        "        DO i=2,istop-1\n"
        "          CALL next_sshu_code(i, j, cu_fld, u_fld, tmask, "
        "area_t, area_u)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_next_sshu_arrays\n"
        "  END MODULE psy_single_invoke_with_grid_props_test")
    print generated_code
    assert generated_code.find(expected_output) != -1


def test_grid_property_no_deref():
    ''' Tests that an invoke containing a kernel call requiring
    a property of the grid produces correct code '''
    _, invoke_info = parse(os.path.join(os.path.
                                        dirname(os.path.
                                                abspath(__file__)),
                                        "test_files", "gocean1p0",
                                        "single_invoke_grid_props.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # Turn-off generation of de-referencing routine
    schedule.deref_routine = False
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_with_grid_props_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_next_sshu(cu_fld, u_fld)\n"
        "      USE kernel_requires_grid_props, ONLY: next_sshu_code\n"
        "      TYPE(r2d_field), intent(inout) :: cu_fld, u_fld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop, jstop\n"
        "      !\n"
        "      ! Look-up loop bounds\n"
        "      istop = cu_fld%grid%simulation_domain%xstop\n"
        "      jstop = cu_fld%grid%simulation_domain%ystop\n"
        "      !\n"
        "      DO j=2,jstop\n"
        "        DO i=2,istop-1\n"
        "          CALL next_sshu_code(i, j, cu_fld%data, u_fld%data, "
        "u_fld%grid%tmask, u_fld%grid%area_t, u_fld%grid%area_u)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_next_sshu\n"
        "  END MODULE psy_single_invoke_with_grid_props_test")

    assert generated_code.find(expected_output) != -1


def test_scalar_int_arg():
    ''' Tests that an invoke containing a kernel call requiring
    an integer, scalar argument produces correct code '''
    _, invoke_info = parse(os.path.join(os.path.
                                        dirname(os.path.
                                                abspath(__file__)),
                                        "test_files", "gocean1p0",
                                        "single_invoke_scalar_int_arg.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_scalar_int_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_bc_ssh(ncycle, ssh_fld)\n"
        "      TYPE(r2d_field), intent(inout) :: ssh_fld\n"
        "      INTEGER, intent(inout) :: ncycle\n"
        "      INTEGER nx, ny, istop, jstop\n"
        "      nx = ssh_fld%grid%nx\n"
        "      ny = ssh_fld%grid%ny\n"
        "      istop = ssh_fld%grid%simulation_domain%xstop\n"
        "      jstop = ssh_fld%grid%simulation_domain%ystop\n"
        "      CALL invoke_0_bc_ssh_arrays(nx, ny, istop, jstop, ncycle, "
        "ssh_fld%data, ssh_fld%grid%tmask)\n"
        "    END SUBROUTINE invoke_0_bc_ssh\n"
        "    SUBROUTINE invoke_0_bc_ssh_arrays(nx, ny, istop, jstop, "
        "ncycle, ssh_fld, tmask)\n"
        "      USE kernel_scalar_int, ONLY: bc_ssh_code\n"
        "      INTEGER, intent(in) :: nx, ny, istop, jstop\n"
        "      REAL(KIND=wp), intent(inout), dimension(nx,ny) :: ssh_fld\n"
        "      INTEGER, intent(inout) :: ncycle\n"
        "      INTEGER, intent(inout), dimension(nx,ny) :: tmask\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      DO j=1,jstop+1\n"
        "        DO i=1,istop+1\n"
        "          CALL bc_ssh_code(i, j, ncycle, ssh_fld, tmask)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_bc_ssh_arrays\n"
        "  END MODULE psy_single_invoke_scalar_int_test")
    print generated_code
    assert generated_code.find(expected_output) != -1


def test_scalar_int_arg_no_deref():
    ''' Tests that an invoke containing a kernel call requiring
    an integer, scalar argument produces correct code when the
    de-referencing routine is switched off'''
    _, invoke_info = parse(os.path.join(os.path.
                                        dirname(os.path.
                                                abspath(__file__)),
                                        "test_files", "gocean1p0",
                                        "single_invoke_scalar_int_arg.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # Turn-off generation of de-referencing routine
    schedule.deref_routine = False
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_scalar_int_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_bc_ssh(ncycle, ssh_fld)\n"
        "      USE kernel_scalar_int, ONLY: bc_ssh_code\n"
        "      TYPE(r2d_field), intent(inout) :: ssh_fld\n"
        "      INTEGER, intent(inout) :: ncycle\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop, jstop\n"
        "      !\n"
        "      ! Look-up loop bounds\n"
        "      istop = ssh_fld%grid%simulation_domain%xstop\n"
        "      jstop = ssh_fld%grid%simulation_domain%ystop\n"
        "      !\n"
        "      DO j=1,jstop+1\n"
        "        DO i=1,istop+1\n"
        "          CALL bc_ssh_code(i, j, ncycle, ssh_fld%data, "
        "ssh_fld%grid%tmask)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_bc_ssh\n"
        "  END MODULE psy_single_invoke_scalar_int_test")

    assert generated_code.find(expected_output) != -1


def test_scalar_float_arg():
    ''' Tests that an invoke containing a kernel call requiring
    a real, scalar argument produces correct code '''
    _, invoke_info = parse(os.path.join(os.path.
                                        dirname(os.path.
                                                abspath(__file__)),
                                        "test_files", "gocean1p0",
                                        "single_invoke_scalar_float_arg.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_scalar_float_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_bc_ssh(a_scalar, ssh_fld)\n"
        "      TYPE(r2d_field), intent(inout) :: ssh_fld\n"
        "      REAL(KIND=wp), intent(inout) :: a_scalar\n"
        "      INTEGER nx, ny, istop, jstop\n"
        "      nx = ssh_fld%grid%nx\n"
        "      ny = ssh_fld%grid%ny\n"
        "      istop = ssh_fld%grid%simulation_domain%xstop\n"
        "      jstop = ssh_fld%grid%simulation_domain%ystop\n"
        "      CALL invoke_0_bc_ssh_arrays(nx, ny, istop, jstop, a_scalar, "
        "ssh_fld%data, ssh_fld%grid%tmask)\n"
        "    END SUBROUTINE invoke_0_bc_ssh\n"
        "    SUBROUTINE invoke_0_bc_ssh_arrays(nx, ny, istop, jstop, "
        "a_scalar, ssh_fld, tmask)\n"
        "      USE kernel_scalar_float, ONLY: bc_ssh_code\n"
        "      INTEGER, intent(in) :: nx, ny, istop, jstop\n"
        "      REAL(KIND=wp), intent(inout), dimension(nx,ny) :: ssh_fld\n"
        "      REAL(KIND=wp), intent(inout) :: a_scalar\n"
        "      INTEGER, intent(inout), dimension(nx,ny) :: tmask\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      DO j=1,jstop+1\n"
        "        DO i=1,istop+1\n"
        "          CALL bc_ssh_code(i, j, a_scalar, ssh_fld, tmask)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_bc_ssh_arrays\n"
        "  END MODULE psy_single_invoke_scalar_float_test")
    print generated_code
    assert generated_code.find(expected_output) != -1


def test_scalar_float_arg_no_deref():
    ''' Tests that an invoke containing a kernel call requiring
    a real, scalar argument produces correct code when the
    de-referencing routine is switched off'''
    _, invoke_info = parse(os.path.join(os.path.
                                        dirname(os.path.
                                                abspath(__file__)),
                                        "test_files", "gocean1p0",
                                        "single_invoke_scalar_float_arg.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # Turn-off generation of de-referencing routine
    schedule.deref_routine = False
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_scalar_float_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_bc_ssh(a_scalar, ssh_fld)\n"
        "      USE kernel_scalar_float, ONLY: bc_ssh_code\n"
        "      TYPE(r2d_field), intent(inout) :: ssh_fld\n"
        "      REAL(KIND=wp), intent(inout) :: a_scalar\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop, jstop\n"
        "      !\n"
        "      ! Look-up loop bounds\n"
        "      istop = ssh_fld%grid%simulation_domain%xstop\n"
        "      jstop = ssh_fld%grid%simulation_domain%ystop\n"
        "      !\n"
        "      DO j=1,jstop+1\n"
        "        DO i=1,istop+1\n"
        "          CALL bc_ssh_code(i, j, a_scalar, ssh_fld%data, "
        "ssh_fld%grid%tmask)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_bc_ssh\n"
        "  END MODULE psy_single_invoke_scalar_float_test")

    assert generated_code.find(expected_output) != -1


def test_ne_offset_cf_points():
    ''' Test that we can generate code for a kernel that expects a NE
    offset and writes to a field on CF points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test14_ne_offset_cf_updated_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_compute_vort(vort_fld, p_fld, u_fld, v_fld)\n"
        "      TYPE(r2d_field), intent(inout) :: vort_fld, p_fld, u_fld, "
        "v_fld\n"
        "      INTEGER nx, ny, istop, jstop\n"
        "      nx = p_fld%grid%nx\n"
        "      ny = p_fld%grid%ny\n"
        "      istop = p_fld%grid%simulation_domain%xstop\n"
        "      jstop = p_fld%grid%simulation_domain%ystop\n"
        "      CALL invoke_0_compute_vort_arrays(nx, ny, istop, jstop, "
        "vort_fld%data, p_fld%data, u_fld%data, v_fld%data)\n"
        "    END SUBROUTINE invoke_0_compute_vort\n"
        "    SUBROUTINE invoke_0_compute_vort_arrays(nx, ny, istop, jstop, "
        "vort_fld, p_fld, u_fld, v_fld)\n"
        "      USE kernel_ne_offset_cf_mod, ONLY: compute_vort_code\n"
        "      INTEGER, intent(in) :: nx, ny, istop, jstop\n"
        "      REAL(KIND=wp), intent(inout), dimension(nx,ny) :: vort_fld, "
        "p_fld, u_fld, v_fld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      DO j=1,jstop-1\n"
        "        DO i=1,istop-1\n"
        "          CALL compute_vort_code(i, j, vort_fld, p_fld, "
        "u_fld, v_fld)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_compute_vort_arrays\n"
        "  END MODULE psy_single_invoke_test")
    print generated_code
    assert expected_output in generated_code


def test_ne_offset_cf_points_no_deref():
    ''' Test that we can generate code for a kernel that expects a NE
    offset and writes to a field on CF points when the de-referencing
    routine is switched off'''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test14_ne_offset_cf_updated_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # Turn-off generation of de-referencing routine
    schedule.deref_routine = False

    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_compute_vort(vort_fld, p_fld, u_fld, v_fld)\n"
        "      USE kernel_ne_offset_cf_mod, ONLY: compute_vort_code\n"
        "      TYPE(r2d_field), intent(inout) :: vort_fld, p_fld, u_fld, "
        "v_fld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop, jstop\n"
        "      !\n"
        "      ! Look-up loop bounds\n"
        "      istop = vort_fld%grid%simulation_domain%xstop\n"
        "      jstop = vort_fld%grid%simulation_domain%ystop\n"
        "      !\n"
        "      DO j=1,jstop-1\n"
        "        DO i=1,istop-1\n"
        "          CALL compute_vort_code(i, j, vort_fld%data, p_fld%data, "
        "u_fld%data, v_fld%data)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_compute_vort\n"
        "  END MODULE psy_single_invoke_test")

    assert expected_output in generated_code


def test_ne_offset_ct_points():
    ''' Test that we can generate code for a kernel that expects a NE
    offset and writes to a field on CT points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test15_ne_offset_ct_updated_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_compute_vort(p_fld, u_fld, v_fld)\n"
        "      TYPE(r2d_field), intent(inout) :: p_fld, u_fld, v_fld\n"
        "      INTEGER nx, ny, istop, jstop\n"
        "      nx = u_fld%grid%nx\n"
        "      ny = u_fld%grid%ny\n"
        "      istop = u_fld%grid%simulation_domain%xstop\n"
        "      jstop = u_fld%grid%simulation_domain%ystop\n"
        "      CALL invoke_0_compute_vort_arrays(nx, ny, istop, jstop, "
        "p_fld%data, u_fld%data, v_fld%data)\n"
        "    END SUBROUTINE invoke_0_compute_vort\n"
        "    SUBROUTINE invoke_0_compute_vort_arrays(nx, ny, istop, jstop, "
        "p_fld, u_fld, v_fld)\n"
        "      USE kernel_ne_offset_ct_mod, ONLY: compute_vort_code\n"
        "      INTEGER, intent(in) :: nx, ny, istop, jstop\n"
        "      REAL(KIND=wp), intent(inout), dimension(nx,ny) :: p_fld, "
        "u_fld, v_fld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      DO j=2,jstop\n"
        "        DO i=2,istop\n"
        "          CALL compute_vort_code(i, j, p_fld, u_fld, v_fld)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_compute_vort_arrays\n"
        "  END MODULE psy_single_invoke_test")
    print generated_code
    assert expected_output in generated_code


def test_ne_offset_ct_points_no_deref():
    ''' Test that we can generate code (without a de-referencing
    routine for a kernel that expects a NE offset and writes to a
    field on CT points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test15_ne_offset_ct_updated_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # Turn-off generation of de-referencing routine
    schedule.deref_routine = False

    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_compute_vort(p_fld, u_fld, v_fld)\n"
        "      USE kernel_ne_offset_ct_mod, ONLY: compute_vort_code\n"
        "      TYPE(r2d_field), intent(inout) :: p_fld, u_fld, v_fld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop, jstop\n"
        "      !\n"
        "      ! Look-up loop bounds\n"
        "      istop = p_fld%grid%simulation_domain%xstop\n"
        "      jstop = p_fld%grid%simulation_domain%ystop\n"
        "      !\n"
        "      DO j=2,jstop\n"
        "        DO i=2,istop\n"
        "          CALL compute_vort_code(i, j, p_fld%data, u_fld%data, "
        "v_fld%data)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_compute_vort\n"
        "  END MODULE psy_single_invoke_test")
    print generated_code
    assert expected_output in generated_code


def test_ne_offset_all_cu_points():
    ''' Test that we can generate code for a kernel that expects a NE
    offset and writes to a field on all CU points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test16_ne_offset_cu_updated_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_bc_solid_u(u_fld)\n"
        "      TYPE(r2d_field), intent(inout) :: u_fld\n"
        "      INTEGER nx, ny, istop, jstop\n"
        "      nx = u_fld%grid%nx\n"
        "      ny = u_fld%grid%ny\n"
        "      istop = u_fld%grid%simulation_domain%xstop\n"
        "      jstop = u_fld%grid%simulation_domain%ystop\n"
        "      CALL invoke_0_bc_solid_u_arrays(nx, ny, istop, jstop, "
        "u_fld%data, u_fld%grid%tmask)\n"
        "    END SUBROUTINE invoke_0_bc_solid_u\n"
        "    SUBROUTINE invoke_0_bc_solid_u_arrays(nx, ny, istop, jstop, "
        "u_fld, tmask)\n"
        "      USE boundary_conditions_ne_offset_mod, ONLY: bc_solid_u_code\n"
        "      INTEGER, intent(in) :: nx, ny, istop, jstop\n"
        "      REAL(KIND=wp), intent(inout), dimension(nx,ny) :: u_fld\n"
        "      INTEGER, intent(inout), dimension(nx,ny) :: tmask\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      DO j=1,jstop+1\n"
        "        DO i=1,istop\n"
        "          CALL bc_solid_u_code(i, j, u_fld, tmask)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_bc_solid_u_arrays\n"
        "  END MODULE psy_single_invoke_test")
    print generated_code
    assert expected_output in generated_code


def test_ne_offset_all_cu_points_no_deref():
    '''Test that we can generate code (without a de-referencing routine
    for a kernel that expects a NE offset and writes to a field on all
    CU points

    '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test16_ne_offset_cu_updated_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # Turn-off generation of de-referencing routine
    schedule.deref_routine = False
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_bc_solid_u(u_fld)\n"
        "      USE boundary_conditions_ne_offset_mod, ONLY: bc_solid_u_code\n"
        "      TYPE(r2d_field), intent(inout) :: u_fld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop, jstop\n"
        "      !\n"
        "      ! Look-up loop bounds\n"
        "      istop = u_fld%grid%simulation_domain%xstop\n"
        "      jstop = u_fld%grid%simulation_domain%ystop\n"
        "      !\n"
        "      DO j=1,jstop+1\n"
        "        DO i=1,istop\n"
        "          CALL bc_solid_u_code(i, j, u_fld%data, u_fld%grid%tmask)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_bc_solid_u\n"
        "  END MODULE psy_single_invoke_test")
    print generated_code
    assert expected_output in generated_code


def test_ne_offset_all_cv_points():
    ''' Test that we can generate code for a kernel that expects a NE
    offset and writes to a field on all CV points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test17_ne_offset_cv_updated_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_bc_solid_v(v_fld)\n"
        "      TYPE(r2d_field), intent(inout) :: v_fld\n"
        "      INTEGER nx, ny, istop, jstop\n"
        "      nx = v_fld%grid%nx\n"
        "      ny = v_fld%grid%ny\n"
        "      istop = v_fld%grid%simulation_domain%xstop\n"
        "      jstop = v_fld%grid%simulation_domain%ystop\n"
        "      CALL invoke_0_bc_solid_v_arrays(nx, ny, istop, jstop, "
        "v_fld%data, v_fld%grid%tmask)\n"
        "    END SUBROUTINE invoke_0_bc_solid_v\n"
        "    SUBROUTINE invoke_0_bc_solid_v_arrays(nx, ny, istop, "
        "jstop, v_fld, tmask)\n"
        "      USE boundary_conditions_ne_offset_mod, ONLY: bc_solid_v_code\n"
        "      INTEGER, intent(in) :: nx, ny, istop, jstop\n"
        "      REAL(KIND=wp), intent(inout), dimension(nx,ny) :: v_fld\n"
        "      INTEGER, intent(inout), dimension(nx,ny) :: tmask\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      DO j=1,jstop\n"
        "        DO i=1,istop+1\n"
        "          CALL bc_solid_v_code(i, j, v_fld, tmask)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_bc_solid_v_arrays\n"
        "  END MODULE psy_single_invoke_test")
    print generated_code
    assert expected_output in generated_code


def test_ne_offset_all_cv_points_no_deref():
    ''' Test that we can generate code (without a de-referencing routine
    for a kernel that expects a NE offset and writes to a field on all CV
    points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test17_ne_offset_cv_updated_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # Turn-off generation of de-referencing routine
    schedule.deref_routine = False

    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_bc_solid_v(v_fld)\n"
        "      USE boundary_conditions_ne_offset_mod, ONLY: bc_solid_v_code\n"
        "      TYPE(r2d_field), intent(inout) :: v_fld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop, jstop\n"
        "      !\n"
        "      ! Look-up loop bounds\n"
        "      istop = v_fld%grid%simulation_domain%xstop\n"
        "      jstop = v_fld%grid%simulation_domain%ystop\n"
        "      !\n"
        "      DO j=1,jstop\n"
        "        DO i=1,istop+1\n"
        "          CALL bc_solid_v_code(i, j, v_fld%data, v_fld%grid%tmask)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_bc_solid_v\n"
        "  END MODULE psy_single_invoke_test")
    print generated_code
    assert expected_output in generated_code


def test_ne_offset_all_cf_points():
    ''' Test that we can generate code for a kernel that expects a NE
    offset and writes to a field on all CF points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test18_ne_offset_cf_updated_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_bc_solid_f(f_fld)\n"
        "      TYPE(r2d_field), intent(inout) :: f_fld\n"
        "      INTEGER nx, ny, istop, jstop\n"
        "      nx = f_fld%grid%nx\n"
        "      ny = f_fld%grid%ny\n"
        "      istop = f_fld%grid%simulation_domain%xstop\n"
        "      jstop = f_fld%grid%simulation_domain%ystop\n"
        "      CALL invoke_0_bc_solid_f_arrays(nx, ny, istop, jstop, "
        "f_fld%data, f_fld%grid%tmask)\n"
        "    END SUBROUTINE invoke_0_bc_solid_f\n"
        "    SUBROUTINE invoke_0_bc_solid_f_arrays(nx, ny, istop, jstop, "
        "f_fld, tmask)\n"
        "      USE boundary_conditions_ne_offset_mod, ONLY: bc_solid_f_code\n"
        "      INTEGER, intent(in) :: nx, ny, istop, jstop\n"
        "      REAL(KIND=wp), intent(inout), dimension(nx,ny) :: f_fld\n"
        "      INTEGER, intent(inout), dimension(nx,ny) :: tmask\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      DO j=1,jstop\n"
        "        DO i=1,istop\n"
        "          CALL bc_solid_f_code(i, j, f_fld, tmask)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_bc_solid_f_arrays\n"
        "  END MODULE psy_single_invoke_test")
    print generated_code
    assert expected_output in generated_code


def test_ne_offset_all_cf_points_no_deref():
    '''Test that we can generate code without a de-referencing routine
    for a kernel that expects a NE offset and writes to a field on all
    CF points

    '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test18_ne_offset_cf_updated_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # Turn-off generation of de-referencing routine
    schedule.deref_routine = False

    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_bc_solid_f(f_fld)\n"
        "      USE boundary_conditions_ne_offset_mod, ONLY: bc_solid_f_code\n"
        "      TYPE(r2d_field), intent(inout) :: f_fld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop, jstop\n"
        "      !\n"
        "      ! Look-up loop bounds\n"
        "      istop = f_fld%grid%simulation_domain%xstop\n"
        "      jstop = f_fld%grid%simulation_domain%ystop\n"
        "      !\n"
        "      DO j=1,jstop\n"
        "        DO i=1,istop\n"
        "          CALL bc_solid_f_code(i, j, f_fld%data, f_fld%grid%tmask)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_bc_solid_f\n"
        "  END MODULE psy_single_invoke_test")
    print generated_code
    assert expected_output in generated_code


def test_sw_offset_cf_points():
    ''' Test that we can generate code for a kernel that expects a SW
    offset and writes to a field on internal CF points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test19.1_sw_offset_cf_updated" +
                                "_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_compute_z(zfld, pfld, ufld, vfld)\n"
        "      TYPE(r2d_field), intent(inout) :: zfld, pfld, ufld, vfld\n"
        "      INTEGER nx, ny, istop, jstop\n"
        "      nx = pfld%grid%nx\n"
        "      ny = pfld%grid%ny\n"
        "      istop = pfld%grid%simulation_domain%xstop\n"
        "      jstop = pfld%grid%simulation_domain%ystop\n"
        "      CALL invoke_0_compute_z_arrays(nx, ny, istop, jstop, "
        "zfld%data, pfld%data, ufld%data, vfld%data, pfld%grid%dx, "
        "pfld%grid%dy)\n"
        "    END SUBROUTINE invoke_0_compute_z\n"
        "    SUBROUTINE invoke_0_compute_z_arrays(nx, ny, istop, jstop, "
        "zfld, pfld, ufld, vfld, dx, dy)\n"
        "      USE kernel_sw_offset_cf_mod, ONLY: compute_z_code\n"
        "      INTEGER, intent(in) :: nx, ny, istop, jstop\n"
        "      REAL(KIND=wp), intent(inout), dimension(nx,ny) :: zfld, "
        "pfld, ufld, vfld\n"
        "      REAL(KIND=wp), intent(inout), dimension(nx,ny) :: dx, dy\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      DO j=2,jstop+1\n"
        "        DO i=2,istop+1\n"
        "          CALL compute_z_code(i, j, zfld, pfld, ufld, vfld, dx, dy)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_compute_z_arrays\n"
        "  END MODULE psy_single_invoke_test")
    print generated_code
    assert expected_output in generated_code


def test_sw_offset_cf_points_no_deref():
    '''Test that we can generate code without a de-referencing routine
    for a kernel that expects a SW offset and writes to a field on
    internal CF points

    '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test19.1_sw_offset_cf_updated" +
                                "_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # Turn-off generation of de-referencing routine
    schedule.deref_routine = False
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_compute_z(zfld, pfld, ufld, vfld)\n"
        "      USE kernel_sw_offset_cf_mod, ONLY: compute_z_code\n"
        "      TYPE(r2d_field), intent(inout) :: zfld, pfld, ufld, vfld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop, jstop\n"
        "      !\n"
        "      ! Look-up loop bounds\n"
        "      istop = zfld%grid%simulation_domain%xstop\n"
        "      jstop = zfld%grid%simulation_domain%ystop\n"
        "      !\n"
        "      DO j=2,jstop+1\n"
        "        DO i=2,istop+1\n"
        "          CALL compute_z_code(i, j, zfld%data, pfld%data, "
        "ufld%data, vfld%data, pfld%grid%dx, pfld%grid%dy)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_compute_z\n"
        "  END MODULE psy_single_invoke_test")
    print generated_code
    assert expected_output in generated_code


def test_sw_offset_all_cf_points():
    ''' Test that we can generate code for a kernel that expects a SW
    offset and writes to a field on all CF points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test19.2_sw_offset_all_cf_updated" +
                                "_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_apply_bcs_f(zfld, pfld, ufld, vfld)\n"
        "      TYPE(r2d_field), intent(inout) :: zfld, pfld, ufld, vfld\n"
        "      INTEGER nx, ny, istop, jstop\n"
        "      nx = pfld%grid%nx\n"
        "      ny = pfld%grid%ny\n"
        "      istop = pfld%grid%simulation_domain%xstop\n"
        "      jstop = pfld%grid%simulation_domain%ystop\n"
        "      CALL invoke_0_apply_bcs_f_arrays(nx, ny, istop, jstop, "
        "zfld%data, pfld%data, ufld%data, vfld%data)\n"
        "    END SUBROUTINE invoke_0_apply_bcs_f\n"
        "    SUBROUTINE invoke_0_apply_bcs_f_arrays(nx, ny, istop, jstop, "
        "zfld, pfld, ufld, vfld)\n"
        "      USE kernel_sw_offset_cf_mod, ONLY: apply_bcs_f_code\n"
        "      INTEGER, intent(in) :: nx, ny, istop, jstop\n"
        "      REAL(KIND=wp), intent(inout), dimension(nx,ny) :: zfld, pfld, "
        "ufld, vfld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      DO j=1,jstop+1\n"
        "        DO i=1,istop+1\n"
        "          CALL apply_bcs_f_code(i, j, zfld, pfld, ufld, vfld)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_apply_bcs_f_arrays\n"
        "  END MODULE psy_single_invoke_test")
    print generated_code
    assert expected_output in  generated_code


def test_sw_offset_all_cf_points_no_deref():
    '''Test that we can generate code without a de-referencing routine
    for a kernel that expects a SW offset and writes to a field on all
    CF points

    '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test19.2_sw_offset_all_cf_updated" +
                                "_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # Turn-off generation of de-referencing routine
    schedule.deref_routine = False
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_apply_bcs_f(zfld, pfld, ufld, vfld)\n"
        "      USE kernel_sw_offset_cf_mod, ONLY: apply_bcs_f_code\n"
        "      TYPE(r2d_field), intent(inout) :: zfld, pfld, ufld, vfld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop, jstop\n"
        "      !\n"
        "      ! Look-up loop bounds\n"
        "      istop = zfld%grid%simulation_domain%xstop\n"
        "      jstop = zfld%grid%simulation_domain%ystop\n"
        "      !\n"
        "      DO j=1,jstop+1\n"
        "        DO i=1,istop+1\n"
        "          CALL apply_bcs_f_code(i, j, zfld%data, pfld%data, "
        "ufld%data, vfld%data)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_apply_bcs_f\n"
        "  END MODULE psy_single_invoke_test")
    print generated_code
    assert expected_output in  generated_code


def test_sw_offset_ct_points():
    ''' Test that we can generate code for a kernel that expects a SW
    offset and writes to a field on internal CT points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test20_sw_offset_ct_updated_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_compute_h(hfld, pfld, ufld, vfld)\n"
        "      TYPE(r2d_field), intent(inout) :: hfld, pfld, ufld, vfld\n"
        "      INTEGER nx, ny, istop, jstop\n"
        "      nx = pfld%grid%nx\n"
        "      ny = pfld%grid%ny\n"
        "      istop = pfld%grid%simulation_domain%xstop\n"
        "      jstop = pfld%grid%simulation_domain%ystop\n"
        "      CALL invoke_0_compute_h_arrays(nx, ny, istop, jstop, "
        "hfld%data, pfld%data, ufld%data, vfld%data)\n"
        "    END SUBROUTINE invoke_0_compute_h\n"
        "    SUBROUTINE invoke_0_compute_h_arrays(nx, ny, istop, jstop, "
        "hfld, pfld, ufld, vfld)\n"
        "      USE kernel_sw_offset_ct_mod, ONLY: compute_h_code\n"
        "      INTEGER, intent(in) :: nx, ny, istop, jstop\n"
        "      REAL(KIND=wp), intent(inout), dimension(nx,ny) :: hfld, pfld, "
        "ufld, vfld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      DO j=2,jstop\n"
        "        DO i=2,istop\n"
        "          CALL compute_h_code(i, j, hfld, pfld, ufld, vfld)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_compute_h_arrays\n"
        "  END MODULE psy_single_invoke_test")
    print generated_code
    assert expected_output in generated_code


def test_sw_offset_ct_points_no_deref():
    '''Test that we can generate code without a de-referencing routine
    for a kernel that expects a SW offset and writes to a field on
    internal CT points

    '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test20_sw_offset_ct_updated_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # Turn-off generation of de-referencing routine
    schedule.deref_routine = False
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_compute_h(hfld, pfld, ufld, vfld)\n"
        "      USE kernel_sw_offset_ct_mod, ONLY: compute_h_code\n"
        "      TYPE(r2d_field), intent(inout) :: hfld, pfld, ufld, vfld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop, jstop\n"
        "      !\n"
        "      ! Look-up loop bounds\n"
        "      istop = hfld%grid%simulation_domain%xstop\n"
        "      jstop = hfld%grid%simulation_domain%ystop\n"
        "      !\n"
        "      DO j=2,jstop\n"
        "        DO i=2,istop\n"
        "          CALL compute_h_code(i, j, hfld%data, pfld%data, ufld%data, "
        "vfld%data)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_compute_h\n"
        "  END MODULE psy_single_invoke_test")
    print generated_code
    assert expected_output in generated_code


def test_sw_offset_all_ct_points():
    ''' Test that we can generate code for a kernel that expects a SW
    offset and writes to a field on all CT points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test21_sw_offset_all_ct_updated" +
                                "_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_apply_bcs_h(hfld, pfld, ufld, vfld)\n"
        "      TYPE(r2d_field), intent(inout) :: hfld, pfld, ufld, vfld\n"
        "      INTEGER nx, ny, istop, jstop\n"
        "      nx = pfld%grid%nx\n"
        "      ny = pfld%grid%ny\n"
        "      istop = pfld%grid%simulation_domain%xstop\n"
        "      jstop = pfld%grid%simulation_domain%ystop\n"
        "      CALL invoke_0_apply_bcs_h_arrays(nx, ny, istop, jstop, "
        "hfld%data, pfld%data, ufld%data, vfld%data)\n"
        "    END SUBROUTINE invoke_0_apply_bcs_h\n"
        "    SUBROUTINE invoke_0_apply_bcs_h_arrays(nx, ny, istop, jstop, "
        "hfld, pfld, ufld, vfld)\n"
        "      USE kernel_sw_offset_ct_mod, ONLY: apply_bcs_h_code\n"
        "      INTEGER, intent(in) :: nx, ny, istop, jstop\n"
        "      REAL(KIND=wp), intent(inout), dimension(nx,ny) :: hfld, "
        "pfld, ufld, vfld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      DO j=1,jstop+1\n"
        "        DO i=1,istop+1\n"
        "          CALL apply_bcs_h_code(i, j, hfld, pfld, ufld, vfld)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_apply_bcs_h_arrays\n"
        "  END MODULE psy_single_invoke_test")
    print generated_code
    assert expected_output in generated_code


def test_sw_offset_all_ct_points_no_deref():
    '''Test that we can generate code without a de-referencing routine for
    a kernel that expects a SW offset and writes to a field on all CT
    points

    '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test21_sw_offset_all_ct_updated" +
                                "_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # Turn-off generation of de-referencing routine
    schedule.deref_routine = False
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_apply_bcs_h(hfld, pfld, ufld, vfld)\n"
        "      USE kernel_sw_offset_ct_mod, ONLY: apply_bcs_h_code\n"
        "      TYPE(r2d_field), intent(inout) :: hfld, pfld, ufld, vfld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop, jstop\n"
        "      !\n"
        "      ! Look-up loop bounds\n"
        "      istop = hfld%grid%simulation_domain%xstop\n"
        "      jstop = hfld%grid%simulation_domain%ystop\n"
        "      !\n"
        "      DO j=1,jstop+1\n"
        "        DO i=1,istop+1\n"
        "          CALL apply_bcs_h_code(i, j, hfld%data, pfld%data, "
        "ufld%data, vfld%data)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_apply_bcs_h\n"
        "  END MODULE psy_single_invoke_test")
    print generated_code
    assert expected_output in generated_code


def test_sw_offset_all_cu_points():
    ''' Test that we can generate code for a kernel that expects a SW
    offset and writes to a field on all CU points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test22_sw_offset_all_cu_updated" +
                                "_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_apply_bcs_u(ufld, vfld)\n"
        "      TYPE(r2d_field), intent(inout) :: ufld, vfld\n"
        "      INTEGER nx, ny, istop, jstop\n"
        "      nx = vfld%grid%nx\n"
        "      ny = vfld%grid%ny\n"
        "      istop = vfld%grid%simulation_domain%xstop\n"
        "      jstop = vfld%grid%simulation_domain%ystop\n"
        "      CALL invoke_0_apply_bcs_u_arrays(nx, ny, istop, jstop, "
        "ufld%data, vfld%data)\n"
        "    END SUBROUTINE invoke_0_apply_bcs_u\n"
        "    SUBROUTINE invoke_0_apply_bcs_u_arrays(nx, ny, istop, jstop, "
        "ufld, vfld)\n"
        "      USE kernel_sw_offset_cu_mod, ONLY: apply_bcs_u_code\n"
        "      INTEGER, intent(in) :: nx, ny, istop, jstop\n"
        "      REAL(KIND=wp), intent(inout), dimension(nx,ny) :: ufld, vfld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      DO j=1,jstop+1\n"
        "        DO i=1,istop+1\n"
        "          CALL apply_bcs_u_code(i, j, ufld, vfld)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_apply_bcs_u_arrays\n"
        "  END MODULE psy_single_invoke_test")
    print generated_code
    assert expected_output in generated_code


def test_sw_offset_all_cu_points_no_deref():
    '''Test that we can generate code without a de-referencing routine
    for a kernel that expects a SW offset and writes to a field on all CU
    points'''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test22_sw_offset_all_cu_updated" +
                                "_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # Turn-off generation of de-referencing routine
    schedule.deref_routine = False
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_apply_bcs_u(ufld, vfld)\n"
        "      USE kernel_sw_offset_cu_mod, ONLY: apply_bcs_u_code\n"
        "      TYPE(r2d_field), intent(inout) :: ufld, vfld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop, jstop\n"
        "      !\n"
        "      ! Look-up loop bounds\n"
        "      istop = ufld%grid%simulation_domain%xstop\n"
        "      jstop = ufld%grid%simulation_domain%ystop\n"
        "      !\n"
        "      DO j=1,jstop+1\n"
        "        DO i=1,istop+1\n"
        "          CALL apply_bcs_u_code(i, j, ufld%data, vfld%data)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_apply_bcs_u\n"
        "  END MODULE psy_single_invoke_test")
    print generated_code
    assert expected_output in generated_code


def test_sw_offset_all_cv_points():
    ''' Test that we can generate code for a kernel that expects a SW
    offset and writes to a field on all CV points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test23_sw_offset_all_cv_updated" +
                                "_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_apply_bcs_v(vfld, ufld)\n"
        "      TYPE(r2d_field), intent(inout) :: vfld, ufld\n"
        "      INTEGER nx, ny, istop, jstop\n"
        "      !\n"
        "      ! Look-up array extents\n"
        "      nx = ufld%grid%nx\n"
        "      ny = ufld%grid%ny\n"
        "      !\n"
        "      ! Look-up loop bounds\n"
        "      istop = ufld%grid%simulation_domain%xstop\n"
        "      jstop = ufld%grid%simulation_domain%ystop\n"
        "      !\n"
        "      CALL invoke_0_apply_bcs_v_arrays(nx, ny, istop, jstop, "
        "vfld%data, ufld%data)\n"
        "    END SUBROUTINE invoke_0_apply_bcs_v\n"
        "    SUBROUTINE invoke_0_apply_bcs_v_arrays(nx, ny, istop, jstop, "
        "vfld, ufld)\n"
        "      USE kernel_sw_offset_cv_mod, ONLY: apply_bcs_v_code\n"
        "      INTEGER, intent(in) :: nx, ny, istop, jstop\n"
        "      REAL(KIND=wp), intent(inout), dimension(nx,ny) :: vfld, ufld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      DO j=1,jstop+1\n"
        "        DO i=1,istop+1\n"
        "          CALL apply_bcs_v_code(i, j, vfld, ufld)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_apply_bcs_v_arrays\n"
        "  END MODULE psy_single_invoke_test")
    print generated_code
    assert expected_output in generated_code


def test_sw_offset_all_cv_points_no_deref():
    '''Test that we can generate code without a de-referencing routine
    for a kernel that expects a SW offset and writes to a field on all
    CV points

    '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test23_sw_offset_all_cv_updated" +
                                "_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # Turn-off generation of de-referencing routine
    schedule.deref_routine = False
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_apply_bcs_v(vfld, ufld)\n"
        "      USE kernel_sw_offset_cv_mod, ONLY: apply_bcs_v_code\n"
        "      TYPE(r2d_field), intent(inout) :: vfld, ufld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop, jstop\n"
        "      !\n"
        "      ! Look-up loop bounds\n"
        "      istop = ufld%grid%simulation_domain%xstop\n"
        "      jstop = ufld%grid%simulation_domain%ystop\n"
        "      !\n"
        "      DO j=1,jstop+1\n"
        "        DO i=1,istop+1\n"
        "          CALL apply_bcs_v_code(i, j, vfld%data, ufld%data)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_apply_bcs_v\n"
        "  END MODULE psy_single_invoke_test")
    print generated_code
    assert expected_output in generated_code


def test_offset_any_all_cu_points():
    ''' Test that we can generate code for a kernel that will operate
    with any offset and writes to a field on all cu points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test25_any_offset_all_cu_update" +
                                "_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)

    gen_code = str(psy.gen)
    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_compute_u(ufld, vfld, hfld)\n"
        "      TYPE(r2d_field), intent(inout) :: ufld, vfld, hfld\n"
        "      INTEGER nx, ny, istop, jstop\n"
        "      !\n"
        "      ! Look-up array extents\n"
        "      nx = vfld%grid%nx\n"
        "      ny = vfld%grid%ny\n"
        "      !\n"
        "      ! Look-up loop bounds\n"
        "      istop = vfld%grid%simulation_domain%xstop\n"
        "      jstop = vfld%grid%simulation_domain%ystop\n"
        "      !\n"
        "      CALL invoke_0_compute_u_arrays(nx, ny, istop, jstop, "
        "ufld%data, vfld%data, hfld%data)\n"
        "    END SUBROUTINE invoke_0_compute_u\n"
        "    SUBROUTINE invoke_0_compute_u_arrays(nx, ny, istop, jstop, "
        "ufld, vfld, hfld)\n"
        "      USE kernel_any_offset_cu_mod, ONLY: compute_u_code\n"
        "      INTEGER, intent(in) :: nx, ny, istop, jstop\n"
        "      REAL(KIND=wp), intent(inout), dimension(nx,ny) :: ufld, vfld,"
        " hfld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      DO j=1,jstop\n"
        "        DO i=1,istop\n"
        "          CALL compute_u_code(i, j, ufld, vfld, hfld)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_compute_u_arrays\n"
        "  END MODULE psy_single_invoke_test")
    assert expected_output in gen_code

    # Now repeat without the de-referencing routine
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # Turn-off generation of de-referencing routine
    schedule.deref_routine = False
    gen_code_no_deref = str(psy.gen)

    expected_output_no_deref = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_compute_u(ufld, vfld, hfld)\n"
        "      USE kernel_any_offset_cu_mod, ONLY: compute_u_code\n"
        "      TYPE(r2d_field), intent(inout) :: ufld, vfld, hfld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop, jstop\n"
        "      !\n"
        "      ! Look-up loop bounds\n"
        "      istop = vfld%grid%simulation_domain%xstop\n"
        "      jstop = vfld%grid%simulation_domain%ystop\n"
        "      !\n"
        "      DO j=1,jstop\n"
        "        DO i=1,istop\n"
        "          CALL compute_u_code(i, j, ufld%data, vfld%data, "
        "hfld%data)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_compute_u\n"
        "  END MODULE psy_single_invoke_test")
    print gen_code_no_deref
    assert gen_code_no_deref.find(expected_output_no_deref) != -1


def test_offset_any_all_points():
    ''' Test that we can generate code for a kernel that will operate
    with any offset and writes to a field on all points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test24_any_offset_all_update" +
                                "_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_copy(voldfld, vfld)\n"
        "      TYPE(r2d_field), intent(inout) :: voldfld, vfld\n"
        "      INTEGER nx, ny, istop, jstop\n"
        "      !\n"
        "      ! Look-up array extents\n"
        "      nx = vfld%grid%nx\n"
        "      ny = vfld%grid%ny\n"
        "      !\n"
        "      ! Look-up loop bounds\n"
        "      istop = vfld%grid%simulation_domain%xstop\n"
        "      jstop = vfld%grid%simulation_domain%ystop\n"
        "      !\n"
        "      CALL invoke_0_copy_arrays(nx, ny, istop, jstop, "
        "voldfld%data, vfld%data)\n"
        "    END SUBROUTINE invoke_0_copy\n"
        "    SUBROUTINE invoke_0_copy_arrays(nx, ny, istop, jstop, "
        "voldfld, vfld)\n"
        "      USE kernel_field_copy_mod, ONLY: field_copy_code\n"
        "      INTEGER, intent(in) :: nx, ny, istop, jstop\n"
        "      REAL(KIND=wp), intent(inout), dimension(nx,ny) :: voldfld, "
        "vfld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      DO j=1,jstop+1\n"
        "        DO i=1,istop+1\n"
        "          CALL field_copy_code(i, j, voldfld, vfld)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_copy_arrays\n"
        "  END MODULE psy_single_invoke_test")
    print generated_code
    assert generated_code.find(expected_output) != -1


def test_offset_any_all_points_no_deref():
    '''Test that we can generate code without a de-referencing routine
    for a kernel that will operate with any offset and writes to a
    field on all points

    '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test24_any_offset_all_update" +
                                "_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # Turn-off generation of de-referencing routine
    schedule.deref_routine = False
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_copy(voldfld, vfld)\n"
        "      USE kernel_field_copy_mod, ONLY: field_copy_code\n"
        "      TYPE(r2d_field), intent(inout) :: voldfld, vfld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop, jstop\n"
        "      !\n"
        "      ! Look-up loop bounds\n"
        "      istop = vfld%grid%simulation_domain%xstop\n"
        "      jstop = vfld%grid%simulation_domain%ystop\n"
        "      !\n"
        "      DO j=1,jstop+1\n"
        "        DO i=1,istop+1\n"
        "          CALL field_copy_code(i, j, voldfld%data, vfld%data)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_copy\n"
        "  END MODULE psy_single_invoke_test")
    print generated_code
    assert generated_code.find(expected_output) != -1


def test_goschedule_view(capsys):
    ''' Test that the GOSchedule::view() method works as expected '''
    _, invoke_info = parse(os.path.join(os.path.
                                        dirname(os.path.
                                                abspath(__file__)),
                                        "test_files", "gocean1p0",
                                        "single_invoke_two_kernels.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    invoke.schedule.view()

    # The view method writes to stdout and this is captured by py.test
    # by default. We have to query this captured output.
    out, _ = capsys.readouterr()

    expected_output = (
        """GOSchedule[invoke='invoke_0',Constant loop bounds=True]
    Loop[type='outer',field_space='cu',it_space='internal_pts']
        Loop[type='inner',field_space='cu',it_space='internal_pts']
            KernCall compute_cu_code(cu_fld,p_fld,u_fld) [module_inline=False]
    Loop[type='outer',field_space='every',it_space='internal_pts']
        Loop[type='inner',field_space='every',it_space='internal_pts']
            KernCall time_smooth_code(u_fld,unew_fld,uold_fld) [module_inline=False]""")

    assert expected_output in out


def test_goschedule_str():
    ''' Test that the GOSchedule::__str__ method works as expected '''
    _, invoke_info = parse(os.path.join(os.path.
                                        dirname(os.path.
                                                abspath(__file__)),
                                        "test_files", "gocean1p0",
                                        "single_invoke_two_kernels.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    expected_sched = (
        "GOSchedule(Constant loop bounds=True):\n"
        "Loop[]: j= lower=2,jstop,1\n"
        "Loop[]: i= lower=2,istop+1,1\n"
        "kern call: compute_cu_code\n"
        "EndLoop\n"
        "EndLoop\n"
        "Loop[]: j= lower=1,jstop+1,1\n"
        "Loop[]: i= lower=1,istop+1,1\n"
        "kern call: time_smooth_code\n"
        "EndLoop\n"
        "EndLoop\n"
        "End Schedule\n")

    sched_str = str(schedule)
    print sched_str
    assert sched_str in expected_sched

    # Switch-off constant loop bounds
    schedule.deref_routine = False
    schedule.const_loop_bounds = False
    sched_str = str(schedule)

    expected_sched = (
        "GOSchedule(Constant loop bounds=False):\n"
        "Loop[]: j= lower=cu_fld%internal%ystart,cu_fld%internal%ystop,1\n"
        "Loop[]: i= lower=cu_fld%internal%xstart,cu_fld%internal%xstop,1\n"
        "kern call: compute_cu_code\n"
        "EndLoop\n"
        "EndLoop\n"
        "Loop[]: j= lower=1,SIZE(uold_fld%data, 2),1\n"
        "Loop[]: i= lower=1,SIZE(uold_fld%data, 1),1\n"
        "kern call: time_smooth_code\n"
        "EndLoop\n"
        "EndLoop\n"
        "End Schedule\n")

    print sched_str
    assert sched_str in expected_sched


def test_gosched_ijstop():
    ''' Test that the GOSchedule.{i,j}loop_stop raise an error if
    constant loop bounds are not being used '''
    _, invoke_info = parse(os.path.join(os.path.
                                        dirname(os.path.
                                                abspath(__file__)),
                                        "test_files", "gocean1p0",
                                        "single_invoke_two_kernels.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # Turn off constant loop bounds
    schedule.deref_routine = False
    schedule.const_loop_bounds = False
    # Attempt to query the upper bound of the i loop
    with pytest.raises(GenerationError):
        _ = schedule.iloop_stop
    # Attempt to query the upper bound of the j loop
    with pytest.raises(GenerationError):
        _ = schedule.jloop_stop


def test_goloop_no_parent():
    ''' Attempt to generate code for a loop that has no GOSchedule
    as a parent '''
    from gocean1p0 import GOLoop
    goloop = GOLoop(loop_type="inner")
    # Try and generate the code for this loop even though it
    # has no parent schedule and no children
    with pytest.raises(GenerationError):
        goloop.gen_code(None)


def test_goloop_no_children():
    ''' Attempt to generate code for a loop that has no child
    kernel calls '''
    from gocean1p0 import GOLoop, GOSchedule
    gosched = GOSchedule([])
    gojloop = GOLoop(parent=gosched, loop_type="outer")
    goiloop = GOLoop(parent=gosched, loop_type="inner")
    gosched.addchild(gojloop)
    gojloop.addchild(goiloop)
    # Try and generate the code for this loop even though it
    # has no children
    with pytest.raises(GenerationError):
        goiloop.gen_code(None)


def test_goloop_unsupp_offset():
    ''' Attempt to generate code for a loop with constant bounds with
    an unsupported index offset '''
    from gocean1p0 import GOLoop, GOSchedule, GOKern
    gosched = GOSchedule([])
    gojloop = GOLoop(parent=gosched, loop_type="outer")
    goiloop = GOLoop(parent=gosched, loop_type="inner")
    gosched.addchild(gojloop)
    gojloop.addchild(goiloop)
    gokern = GOKern()
    # Set the index-offset of this kernel to a value that is not
    # supported when using constant loop bounds
    gokern._index_offset = "offset_se"
    goiloop.addchild(gokern)
    with pytest.raises(GenerationError):
        goiloop.gen_code(None)


def test_goloop_unmatched_offsets():
    ''' Attempt to generate code for a loop with constant bounds with
    two different index offsets '''
    from gocean1p0 import GOLoop, GOSchedule, GOKern
    gosched = GOSchedule([])
    gojloop = GOLoop(parent=gosched, loop_type="outer")
    goiloop = GOLoop(parent=gosched, loop_type="inner")
    gosched.addchild(gojloop)
    gojloop.addchild(goiloop)
    gokern1 = GOKern()
    gokern2 = GOKern()
    # Set the index-offset of this kernel to a value that is not
    # supported when using constant loop bounds
    gokern1._index_offset = "offset_ne"
    gokern2._index_offset = "offset_sw"
    goiloop.addchild(gokern1)
    goiloop.addchild(gokern2)
    with pytest.raises(GenerationError):
        goiloop.gen_code(None)

# -----------------------------------
# Parser Tests for the GOcean 1.0 API
# -----------------------------------


def t00p1_kernel_wrong_meta_arg_count():
    ''' Check that we raise an error if one of the meta-args in
    a kernel's meta-data has the wrong number of arguments '''
    with pytest.raises(ParseError):
        parse(os.path.
              join(os.path.dirname(os.path.abspath(__file__)),
                   "test_files", "gocean1p0",
                   "test00.1_invoke_kernel_wrong_meta_arg_count.f90"),
              api="gocean1.0")


def t00p2_kernel_invalid_meta_args():
    ''' Check that we raise an error if one of the meta-args in
    a kernel's meta-data is not 'arg' '''
    with pytest.raises(ParseError):
        parse(os.path.
              join(os.path.dirname(os.path.abspath(__file__)),
                   "test_files", "gocean1p0",
                   "test00.2_invoke_kernel_invalid_meta_args.f90"),
              api="gocean1.0")


def t00p3_kern_invalid_meta_arg_type():
    ''' Check that the parser catches the case where the type of
    one of the meta-args in the kernel meta-data is incorrect '''
    test_file = "test00.3_invoke_kernel_invalid_meta_arg_type.f90"
    with pytest.raises(ParseError):
        _, _ = parse(os.path.
                     join(os.path.
                          dirname(os.path.
                                  abspath(__file__)),
                          "test_files", "gocean1p0",
                          test_file),
                     api=API)


def t01_diff_kern_grid_offsets_one_invoke():
    ''' Check that the parser raises an error if two kernels in a
        single invoke specify different index offsets '''
    test_file = "test01_different_grid_offsets_one_invoke.f90"
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                test_file),
                           api=API)
    with pytest.raises(GenerationError):
        _ = PSyFactory(API).create(invoke_info)


def t02_diff_kern_grid_offsets_two_invokes():
    ''' Check that the parser raises an error if the two kernels
        in different invokes specify different index offsets. '''
    test_file = "test02_different_grid_offsets_two_invokes.f90"
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                test_file),
                           api=API)
    with pytest.raises(GenerationError):
        _ = PSyFactory(API).create(invoke_info)


def t03_kernel_missing_index_offset():
    ''' Check that we raise an error if a kernel's meta-data is
    missing the INDEX_OFFSET field. '''
    with pytest.raises(ParseError):
        parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                           "test_files", "gocean1p0",
                           "test03_invoke_kernel_missing_offset.f90"),
              api="gocean1.0")


def t04_kernel_invalid_index_offset():
    ''' Check that we raise an error if a kernel's meta-data is
    contains an invalid value for the INDEX_OFFSET field. '''
    with pytest.raises(ParseError):
        parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                           "test_files", "gocean1p0",
                           "test04_invoke_kernel_invalid_offset.f90"),
              api="gocean1.0")


def t05_kernel_missing_iterates_over():
    ''' Check that we raise an error if a kernel's meta-data is
    missing the ITERATES_OVER field. '''
    with pytest.raises(ParseError):
        parse(os.path.
              join(os.path.dirname(os.path.abspath(__file__)),
                   "test_files", "gocean1p0",
                   "test05_invoke_kernel_missing_iterates_over.f90"),
              api="gocean1.0")


def t05p1_kernel_invalid_iterates_over():
    ''' Check that we raise an error if a kernel's meta-data has
    an invalid ITERATES_OVER field. '''
    with pytest.raises(ParseError):
        parse(os.path.
              join(os.path.dirname(os.path.abspath(__file__)),
                   "test_files", "gocean1p0",
                   "test05.1_invoke_kernel_invalid_iterates_over.f90"),
              api="gocean1.0")


def t06_kernel_invalid_access():
    ''' Check that we raise an error if a kernel's meta-data specifies
    an unrecognised access type for a kernel argument (i.e. something
    other than READ,WRITE,READWRITE) '''
    with pytest.raises(ParseError):
        parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                           "test_files", "gocean1p0",
                           "test06_invoke_kernel_wrong_access.f90"),
              api="gocean1.0")


def t07_kernel_wrong_gridpt_type():
    ''' Check that we raise an error if a kernel's meta-data specifies
    an unrecognised grid-point type for a field argument (i.e.
    something other than C{U,V,F,T}, I_SCALAR or R_SCALAR) '''
    with pytest.raises(ParseError):
        parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                           "test_files", "gocean1p0",
                           "test07_invoke_kernel_wrong_gridpt_type.f90"),
              api="gocean1.0")


def t08_kernel_invalid_grid_property():
    ''' Check that the parser raises an error if a kernel's meta-data
    specifies an unrecognised grid property '''
    with pytest.raises(ParseError):
        parse(os.path.
              join(os.path.dirname(os.path.abspath(__file__)),
                   "test_files", "gocean1p0",
                   "test08_invoke_kernel_invalid_grid_property.f90"),
              api="gocean1.0")


def t08p1_kernel_without_fld_args():
    ''' Check that the parser raises an error if a kernel does not
    have a field object as an argument but requests a grid property '''
    with pytest.raises(ParseError):
        parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                           "test_files", "gocean1p0",
                           "test08.1_invoke_kernel_no_fld_args.f90"),
              api="gocean1.0")


def t09_kernel_missing_stencil_prop():
    '''Check that the parser raises an error if there is no stencil
    specified in the meta-data of a kernel

    '''
    with pytest.raises(ParseError):
        parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                           "test_files", "gocean1p0",
                           "test09_invoke_kernel_missing_stencil.f90"),
              api="gocean1.0")


def t10_kernel_invalid_stencil_prop():
    '''Check that the parser raises an error if there is no stencil
    specified in the meta-data of a kernel

    '''
    with pytest.raises(ParseError):
        parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                           "test_files", "gocean1p0",
                           "test10_invoke_kernel_invalid_stencil.f90"),
              api="gocean1.0")


def test13_kernel_invalid_fortran():
    ''' Check that the parser raises an error if the specified kernel
    code is not valid fortran '''
    with pytest.raises(ParseError):
        parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                           "test_files", "gocean1p0",
                           "test13_invoke_kernel_invalid_fortran.f90"),
              api="gocean1.0")
