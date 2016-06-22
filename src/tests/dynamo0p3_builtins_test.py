# Author A. R. Porter, STFC Daresbury Lab

''' This module tests the support for built-in operations in the Dynamo 0.3 API
    using pytest. Currently all built-in operations are 'pointwise' in that
    they iterate over DOFs. However this may change in the future. '''

# imports
import os
import pytest
from parse import parse, ParseError
from psyGen import PSyFactory, GenerationError


# constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3")

# functions


def test_dynbuiltin_missing_defs():
    ''' Check that we raise an appropriate error if we cannot find the
    file specifying meta-data for built-in kernels '''
    import dynamo0p3_builtins
    old_name = dynamo0p3_builtins.BUILTIN_DEFINITIONS_FILE[:]
    dynamo0p3_builtins.BUILTIN_DEFINITIONS_FILE = 'broken'
    with pytest.raises(ParseError) as excinfo:
        _, _ = parse(os.path.join(BASE_PATH,
                                  "15_single_pointwise_invoke.f90"),
                     api="dynamo0.3")
    dynamo0p3_builtins.BUILTIN_DEFINITIONS_FILE = old_name
    assert ("broken' containing the meta-data describing the "
            "Built-in operations" in str(excinfo.value))


def test_dynbuiltin_not_over_dofs():
    ''' Check that we raise an appropriate error if we encounter a
    built-in that does not iterate over dofs '''
    import dynamo0p3_builtins
    old_name = dynamo0p3_builtins.BUILTIN_DEFINITIONS_FILE[:]
    dynamo0p3_builtins.BUILTIN_DEFINITIONS_FILE = \
        os.path.join(BASE_PATH, "not_dofs_builtins_mod.f90")
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15_single_pointwise_invoke.f90"),
                           api="dynamo0.3")
    with pytest.raises(NotImplementedError) as excinfo:
        _ = PSyFactory("dynamo0.3",
                       distributed_memory=False).create(invoke_info)
    # Restore the original file name before doing the assert in case
    # it fails
    dynamo0p3_builtins.BUILTIN_DEFINITIONS_FILE = old_name
    assert ("built-in calls must iterate over DoFs but found cells for "
            "Built-in: Set field " in str(excinfo.value))


def test_dynbuiltincallfactory_str():
    ''' Check that the str method of DynBuiltInCallFactory works as
    expected '''
    from dynamo0p3_builtins import DynBuiltInCallFactory
    dyninf = DynBuiltInCallFactory()
    assert str(dyninf) == "Factory for a call to a Dynamo built-in"


def test_dynbuiltin_wrong_name():
    ''' Check that DynInfCallFactory.create() raises an error if it
    doesn't recognise the name of the kernel it is passed '''
    from dynamo0p3_builtins import DynBuiltInCallFactory
    dyninf = DynBuiltInCallFactory()
    # We use 'duck-typing' - rather than attempt to create a rather
    # complex Kernel object we use a ParseError object and monkey
    # patch it so that it has a func_name member.
    fake_kern = ParseError("blah")
    fake_kern.func_name = "pw_blah"
    with pytest.raises(ParseError) as excinfo:
        _ = dyninf.create(fake_kern)
    assert ("Unrecognised built-in call. Found 'pw_blah' but "
            "expected one of '[" in str(excinfo.value))


def test_dynbuiltin_no_dm():
    ''' Check that we raise an error if we encounter a call to a built-in
    kernel when distributed memory is enabled '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15_single_pointwise_invoke.f90"),
                           api="dynamo0.3")
    with pytest.raises(ParseError) as excinfo:
        _ = PSyFactory("dynamo0.3",
                       distributed_memory=True).create(invoke_info)
    assert ("built-in kernels are not supported when generating "
            "distributed-memory code" in str(excinfo.value))


def test_invalid_builtin_kernel():
    ''' Check that we raise an appropriate error if an unrecognised
    built-in is specified in the algorithm layer '''
    with pytest.raises(ParseError) as excinfo:
        _, _ = parse(os.path.join(BASE_PATH,
                                  "15.0.0_invalid_builtin_kernel.f90"),
                     api="dynamo0.3")
    assert ("kernel call 'set_field_scala' must either be named in a "
            "use statement or be a recognised built-in" in
            str(excinfo.value))


def test_dynbuiltin_str():
    ''' Check that we raise an error if we attempt to call the __str__
    method on the parent DynBuiltIn class '''
    from dynamo0p3_builtins import DynBuiltIn
    distmem = False
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15_single_pointwise_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=distmem).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].children[0]
    with pytest.raises(NotImplementedError) as excinfo:
        DynBuiltIn.__str__(kern)
    assert ("DynBuiltIn.__str__ must be overridden" in str(excinfo.value))


def test_dynbuiltin_gen_code():
    ''' Check that we raise an error if we attempt to call the gen_code()
    method on the parent DynBuiltIn class '''
    from dynamo0p3_builtins import DynBuiltIn
    distmem = False
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15_single_pointwise_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=distmem).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].children[0]
    with pytest.raises(NotImplementedError) as excinfo:
        DynBuiltIn.gen_code(kern, None)
    assert ("DynBuiltIn.gen_code must be overridden" in str(excinfo.value))


def test_dynbuiltfactory_str():
    ''' Check that the str method of DynBuiltInCallFactory works as
    expected. '''
    from dynamo0p3_builtins import DynBuiltInCallFactory
    factory = DynBuiltInCallFactory()
    assert "Factory for a call to a Dynamo built-in" in str(factory)


def test_builtin_set_str():
    ''' Check that the str method of DynSetFieldScalarKern returns the
    expected string '''
    distmem = False
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15_single_pointwise_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=distmem).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].children[0]
    assert str(kern) == "Built-in: Set field to a scalar value"


def test_builtin_set():
    ''' Tests that we generate correct code for a serial builtin
    set operation with a scalar passed by value'''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15_single_pointwise_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    code = str(psy.gen)
    print code
    output = (
        "    SUBROUTINE invoke_0(f1)\n"
        "      TYPE(field_type), intent(inout) :: f1\n"
        "      INTEGER df\n"
        "      INTEGER ndf_any_space_1, undf_any_space_1\n"
        "      INTEGER nlayers\n"
        "      TYPE(field_proxy_type) f1_proxy\n"
        "      !\n"
        "      ! Initialise field proxies\n"
        "      !\n"
        "      f1_proxy = f1%get_proxy()\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f1_proxy%vspace%get_nlayers()\n"
        "      !\n"
        "      ! Initialise sizes and allocate any basis arrays for "
        "any_space_1\n"
        "      !\n"
        "      ndf_any_space_1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_any_space_1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Call our kernels\n"
        "      !\n"
        "      DO df=1,undf_any_space_1\n"
        "        f1_proxy%data(df) = 0.0\n"
        "      END DO \n")
    assert output in code


def test_builtin_set_by_ref():
    ''' Tests that we generate correct code for a builtin
    set operation with a scalar passed by reference '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.0.1_single_builtin_set_by_ref.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    code = str(psy.gen)
    print code
    output = (
        "    SUBROUTINE invoke_0(fred, f1)\n"
        "      REAL(KIND=r_def), intent(in) :: fred\n"
        "      TYPE(field_type), intent(inout) :: f1\n"
        "      INTEGER df\n"
        "      INTEGER ndf_any_space_1, undf_any_space_1\n"
        "      INTEGER nlayers\n"
        "      TYPE(field_proxy_type) f1_proxy\n"
        "      !\n"
        "      ! Initialise field proxies\n"
        "      !\n"
        "      f1_proxy = f1%get_proxy()\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f1_proxy%vspace%get_nlayers()\n"
        "      !\n"
        "      ! Initialise sizes and allocate any basis arrays for "
        "any_space_1\n"
        "      !\n"
        "      ndf_any_space_1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_any_space_1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Call our kernels\n"
        "      !\n"
        "      DO df=1,undf_any_space_1\n"
        "        f1_proxy%data(df) = fred\n"
        "      END DO \n")
    assert output in code


@pytest.mark.xfail(reason="Invokes containing multiple kernels with "
                   "any-space arguments are not yet supported")
def test_multiple_builtin_set():
    ''' Tests that we generate correct code when we have an invoke
    containing multiple set operations '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.0.2_multiple_set_kernels.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    code = str(psy.gen)
    print code
    output = (
        "    SUBROUTINE invoke_0(f1, fred, f2, f3, ginger)\n"
        "      USE mesh_mod, ONLY: mesh_type\n"
        "      REAL(KIND=r_def), intent(inout) :: fred, ginger\n"
        "      TYPE(field_type), intent(inout) :: f1, f2, f3\n"
        "      INTEGER df\n"
        "      INTEGER ndf_any_space_1, undf_any_space_1\n"
        "      TYPE(mesh_type) mesh\n"
        "      INTEGER nlayers\n"
        "      TYPE(field_proxy_type) f1_proxy\n"
        "      !\n"
        "      ! Initialise field proxies\n"
        "      !\n"
        "      f1_proxy = f1%get_proxy()\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f1_proxy%vspace%get_nlayers()\n"
        "      !\n"
        "      ! Create a mesh object\n"
        "      !\n"
        "      mesh = f1%get_mesh()\n"
        "      !\n"
        "      ! Initialise sizes and allocate any basis arrays for "
        "any_space_1\n"
        "      !\n"
        "      ndf_any_space_1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_any_space_1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Call our kernels\n"
        "      !\n"
        "      DO df=1,undf_any_space_1\n"
        "        f1_proxy%data(df) = fred\n"
        "      END DO \n"
        "      DO df=1,undf_any_space_1\n"
        "        f2_proxy%data(df) = 3.0\n"
        "      END DO \n"
        "      DO df=1,undf_any_space_1\n"
        "        f3_proxy%data(df) = ginger\n"
        "      END DO \n")
    assert output in code


def test_builtin_set_plus_normal():
    ''' Tests that we generate correct code for a builtin
    set operation when the invoke also contains a normal kernel '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.1_builtin_and_normal_kernel_invoke.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    code = str(psy.gen)
    print code
    output = (
        "      ! Initialise sizes and allocate any basis arrays for w3\n"
        "      !\n"
        "      ndf_w3 = m2_proxy%vspace%get_ndf()\n"
        "      undf_w3 = m2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise sizes and allocate any basis arrays for "
        "any_space_1\n"
        "      !\n"
        "      ndf_any_space_1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_any_space_1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Call our kernels\n"
        "      !\n"
        "      DO cell=1,f1_proxy%vspace%get_ncell()\n"
        "        !\n"
        "        map_w1 => f1_proxy%vspace%get_cell_dofmap(cell)\n"
        "        map_w2 => f2_proxy%vspace%get_cell_dofmap(cell)\n"
        "        map_w3 => m2_proxy%vspace%get_cell_dofmap(cell)\n"
        "        !\n"
        "        CALL testkern_code(nlayers, ginger, f1_proxy%data, "
        "f2_proxy%data, "
        "m1_proxy%data, m2_proxy%data, ndf_w1, undf_w1, map_w1, ndf_w2, "
        "undf_w2, map_w2, ndf_w3, undf_w3, map_w3)\n"
        "      END DO \n"
        "      DO df=1,undf_any_space_1\n"
        "        f1_proxy%data(df) = 0.0\n"
        "      END DO ")
    assert output in code


def test_copy_str():
    ''' Check that the str method of DynCopyFieldKern returns the
    expected string '''
    distmem = False
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.2.0_copy_field_builtin.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=distmem).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].children[0]
    assert str(kern) == "Built-in: Copy field"


def test_copy():
    ''' Tests that we generate correct code for a builtin
    copy field operation '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.2.0_copy_field_builtin.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    code = str(psy.gen)
    print code
    output = (
        "    SUBROUTINE invoke_0(f1, f2)\n"
        "      TYPE(field_type), intent(inout) :: f2\n"
        "      TYPE(field_type), intent(in) :: f1\n"
        "      INTEGER df\n"
        "      INTEGER ndf_any_space_1, undf_any_space_1\n"
        "      INTEGER nlayers\n"
        "      TYPE(field_proxy_type) f1_proxy, f2_proxy\n"
        "      !\n"
        "      ! Initialise field proxies\n"
        "      !\n"
        "      f1_proxy = f1%get_proxy()\n"
        "      f2_proxy = f2%get_proxy()\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f1_proxy%vspace%get_nlayers()\n"
        "      !\n"
        "      ! Initialise sizes and allocate any basis arrays for "
        "any_space_1\n"
        "      !\n"
        "      ndf_any_space_1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_any_space_1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Call our kernels\n"
        "      !\n"
        "      DO df=1,undf_any_space_1\n"
        "        f2_proxy%data(df) = f1_proxy%data(df)\n"
        "      END DO")
    assert output in code


def test_subtract_fields_str():
    ''' Test that the str method of DynSubtractFieldsKern returns the
    expected string '''
    distmem = False
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.4.0_subtract_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=distmem).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].children[0]
    assert str(kern) == "Built-in: Subtract fields"


def test_subtract_fields():
    ''' Test that the str method of DynSubtractFieldsKern returns the
    expected string '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.4.0_subtract_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    code = str(psy.gen)
    print code
    output = (
        "      f1_proxy = f1%get_proxy()\n"
        "      f2_proxy = f2%get_proxy()\n"
        "      f3_proxy = f3%get_proxy()\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f1_proxy%vspace%get_nlayers()\n"
        "      !\n"
        "      ! Initialise sizes and allocate any basis arrays for "
        "any_space_1\n"
        "      !\n"
        "      ndf_any_space_1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_any_space_1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Call our kernels\n"
        "      !\n"
        "      DO df=1,undf_any_space_1\n"
        "        f3_proxy%data(df) = f1_proxy%data(df) - f2_proxy%data(df)\n"
        "      END DO")
    assert output in code


def test_add_fields_str():
    ''' Test that the str method of DynSubtractFieldsKern returns the
    expected string '''
    distmem = False
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.5.0_add_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=distmem).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].children[0]
    assert str(kern) == "Built-in: Add fields"


def test_add_fields():
    ''' Test that the str method of DynAddFieldsKern returns the
    expected string '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.5.0_add_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    code = str(psy.gen)
    print code
    output = (
        "      f1_proxy = f1%get_proxy()\n"
        "      f2_proxy = f2%get_proxy()\n"
        "      f3_proxy = f3%get_proxy()\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f1_proxy%vspace%get_nlayers()\n"
        "      !\n"
        "      ! Initialise sizes and allocate any basis arrays for "
        "any_space_1\n"
        "      !\n"
        "      ndf_any_space_1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_any_space_1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Call our kernels\n"
        "      !\n"
        "      DO df=1,undf_any_space_1\n"
        "        f3_proxy%data(df) = f1_proxy%data(df) + f2_proxy%data(df)\n"
        "      END DO")
    assert output in code


def test_divide_fields_str():
    ''' Test that the str method of DynDivideFieldsKern returns the
    expected string '''
    distmem = False
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.6.0_divide_fields_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=distmem).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].children[0]
    assert str(kern) == "Built-in: Divide fields"


def test_divide_fields():
    ''' Test that we generate correct code for the divide fields
    infrastructure kernel '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.6.0_divide_fields_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    code = str(psy.gen)
    print code
    output = (
        "      f1_proxy = f1%get_proxy()\n"
        "      f2_proxy = f2%get_proxy()\n"
        "      f3_proxy = f3%get_proxy()\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f1_proxy%vspace%get_nlayers()\n"
        "      !\n"
        "      ! Initialise sizes and allocate any basis arrays for "
        "any_space_1\n"
        "      !\n"
        "      ndf_any_space_1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_any_space_1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Call our kernels\n"
        "      !\n"
        "      DO df=1,undf_any_space_1\n"
        "        f3_proxy%data(df) = f1_proxy%data(df) / f2_proxy%data(df)\n"
        "      END DO")
    assert output in code


def test_divide_field_str():
    ''' Test that the str method of DynDivideFieldKern returns the
    expected string '''
    distmem = False
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.6.1_divide_field_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=distmem).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].children[0]
    assert str(kern) == "Built-in: Divide field by another"


def test_divide_field():
    ''' Test that we generate correct code for the divide field
    infrastructure kernel (x = x/y) '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.6.1_divide_field_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    code = str(psy.gen)
    print code
    output = (
        "      f1_proxy = f1%get_proxy()\n"
        "      f2_proxy = f2%get_proxy()\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f1_proxy%vspace%get_nlayers()\n"
        "      !\n"
        "      ! Initialise sizes and allocate any basis arrays for "
        "any_space_1\n"
        "      !\n"
        "      ndf_any_space_1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_any_space_1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Call our kernels\n"
        "      !\n"
        "      DO df=1,undf_any_space_1\n"
        "        f1_proxy%data(df) = f1_proxy%data(df) / f2_proxy%data(df)\n"
        "      END DO")
    assert output in code


def test_copy_scaled_field_str():
    ''' Test that the str method of DynCopyScaledFieldKern returns the
    expected string '''
    distmem = False
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.2.1_copy_scaled_field_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=distmem).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].children[0]
    assert str(kern) == "Built-in: Copy scaled field"


def test_copy_scaled_field():
    ''' Test that we generate correct code for the CopyScaledField
    (y = a*x) built-in '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.2.1_copy_scaled_field_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    code = str(psy.gen)
    print code
    output = (
        "      f1_proxy = f1%get_proxy()\n"
        "      f2_proxy = f2%get_proxy()\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f1_proxy%vspace%get_nlayers()\n"
        "      !\n"
        "      ! Initialise sizes and allocate any basis arrays for "
        "any_space_1\n"
        "      !\n"
        "      ndf_any_space_1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_any_space_1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Call our kernels\n"
        "      !\n"
        "      DO df=1,undf_any_space_1\n"
        "        f2_proxy%data(df) = a_scalar * f1_proxy%data(df)\n"
        "      END DO")
    assert output in code


def test_axpy_field_str():
    ''' Test that the str method of DynAXPYKern returns the
    expected string '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.3_axpy_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=False).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].children[0]
    assert str(kern) == "Built-in: AXPY"


def test_axpy():
    ''' Test that we generate correct code for the builtin
    operation f = a*x + y where 'a' is a scalar '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.3_axpy_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    code = str(psy.gen)
    print code
    output = (
        "      f1_proxy = f1%get_proxy()\n"
        "      f2_proxy = f2%get_proxy()\n"
        "      f3_proxy = f3%get_proxy()\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f1_proxy%vspace%get_nlayers()\n"
        "      !\n"
        "      ! Initialise sizes and allocate any basis arrays for "
        "any_space_1\n"
        "      !\n"
        "      ndf_any_space_1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_any_space_1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Call our kernels\n"
        "      !\n"
        "      DO df=1,undf_any_space_1\n"
        "        f3_proxy%data(df) = a*f1_proxy%data(df) + f2_proxy%data(df)\n"
        "      END DO \n"
        )
    assert output in code


def test_axpy_by_value():
    ''' Test that we generate correct code for the builtin
    operation y = a*x + y when a is passed by value'''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.3.2_axpy_invoke_by_value.f90"),
                           api="dynamo0.3")
    distmem = False
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=distmem).create(invoke_info)
    code = str(psy.gen)
    print code
    output = (
        "      f1_proxy = f1%get_proxy()\n"
        "      f2_proxy = f2%get_proxy()\n"
        "      f3_proxy = f3%get_proxy()\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f1_proxy%vspace%get_nlayers()\n"
        "      !\n")
    if distmem:
        output += (
            "      ! Create a mesh object\n"
            "      !\n"
            "      mesh = f1%get_mesh()\n"
            "      !\n")
    output += (
        "      ! Initialise sizes and allocate any basis arrays for "
        "any_space_1\n"
        "      !\n"
        "      ndf_any_space_1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_any_space_1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Call our kernels\n"
        "      !\n"
        "      DO df=1,undf_any_space_1\n"
        "        f3_proxy%data(df) = 0.5*f1_proxy%data(df) + "
        "f2_proxy%data(df)\n"
        "      END DO \n"
        )
    assert output in code


def test_inc_axpy_str():
    ''' Test the str method of DynIncAXPYKern'''
    distmem = False
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.4_inc_axpy_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=distmem).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].children[0]
    assert str(kern) == "Built-in: INC_AXPY"


def test_inc_axpy():
    ''' Test that we generate correct code for the built-in
    operation x = a*x + y '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.4_inc_axpy_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    code = str(psy.gen)
    print code
    output = (
        "      ndf_any_space_1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_any_space_1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Call our kernels\n"
        "      !\n"
        "      DO df=1,undf_any_space_1\n"
        "        f1_proxy%data(df) = a*f1_proxy%data(df) + "
        "f2_proxy%data(df)\n"
        "      END DO \n"
        )
    assert output in code


def test_axpby_field_str():
    ''' Test that the str method of DynAXPBYKern returns the
    expected string '''
    distmem = False
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.8.0_axpby_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=distmem).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].children[0]
    assert str(kern) == "Built-in: AXPBY"


def test_axpby():
    ''' Test that we generate correct code for the builtin
    operation f = a*x + b*y where 'a' and 'b' are scalars '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.8.0_axpby_invoke.f90"),
                           api="dynamo0.3")
    distmem = False
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=distmem).create(invoke_info)
    code = str(psy.gen)
    print code
    output = (
        "      f1_proxy = f1%get_proxy()\n"
        "      f2_proxy = f2%get_proxy()\n"
        "      f3_proxy = f3%get_proxy()\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f1_proxy%vspace%get_nlayers()\n"
        "      !\n")
    if distmem:
        output += (
            "      ! Create a mesh object\n"
            "      !\n"
            "      mesh = f1%get_mesh()\n"
            "      !\n")
    output += (
        "      ! Initialise sizes and allocate any basis arrays for "
        "any_space_1\n"
        "      !\n"
        "      ndf_any_space_1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_any_space_1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Call our kernels\n"
        "      !\n"
        "      DO df=1,undf_any_space_1\n"
        "        f3_proxy%data(df) = a*f1_proxy%data(df) + "
        "b*f2_proxy%data(df)\n"
        "      END DO \n"
        )
    assert output in code


def test_axpby_by_value():
    ''' Test that we generate correct code for the builtin
    operation z = a*x + b*y when a and b are passed by value'''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.8.1_axpby_invoke_by_value.f90"),
                           api="dynamo0.3")
    distmem = False
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=distmem).create(invoke_info)
    code = str(psy.gen)
    print code
    output = (
        "      f1_proxy = f1%get_proxy()\n"
        "      f2_proxy = f2%get_proxy()\n"
        "      f3_proxy = f3%get_proxy()\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f1_proxy%vspace%get_nlayers()\n"
        "      !\n")
    if distmem:
        output += (
            "      ! Create a mesh object\n"
            "      !\n"
            "      mesh = f1%get_mesh()\n"
            "      !\n")
    output += (
        "      ! Initialise sizes and allocate any basis arrays for "
        "any_space_1\n"
        "      !\n"
        "      ndf_any_space_1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_any_space_1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Call our kernels\n"
        "      !\n"
        "      DO df=1,undf_any_space_1\n"
        "        f3_proxy%data(df) = 0.5*f1_proxy%data(df) + "
        "0.8*f2_proxy%data(df)\n"
        "      END DO \n"
        )
    assert output in code


def test_inc_axpby_str():
    ''' Test the str method of DynIncAXPBYKern '''
    distmem = False
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.8.2_inc_axpby_invoke.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=distmem).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].children[0]
    assert str(kern) == "Built-in: INC_AXPBY"


def test_inc_axpby():
    ''' Test that we generate correct code for the built-in
    operation x = a*x + b*y where x and y are fields and a and b are
    scalars. '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.8.2_inc_axpby_invoke.f90"),
        api="dynamo0.3")
    distmem = False
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=distmem).create(invoke_info)
    code = str(psy.gen)
    print code
    output = (
        "      f1_proxy = f1%get_proxy()\n"
        "      f2_proxy = f2%get_proxy()\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f1_proxy%vspace%get_nlayers()\n"
        "      !\n")
    if distmem:
        output += (
            "      ! Create a mesh object\n"
            "      !\n"
            "      mesh = f1%get_mesh()\n"
            "      !\n")
    output += (
        "      ! Initialise sizes and allocate any basis arrays for "
        "any_space_1\n"
        "      !\n"
        "      ndf_any_space_1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_any_space_1 = f1_proxy%vspace%get_undf()\n"
        "      !\n")
    if distmem:
        output += "      ! Call kernels and communication routines\n"
    else:
        output += "      ! Call our kernels\n"
    output += (
        "      !\n"
        "      DO df=1,undf_any_space_1\n"
        "        f1_proxy%data(df) = a*f1_proxy%data(df) + "
        "b*f2_proxy%data(df)\n"
        "      END DO \n"
        )
    assert output in code


@pytest.mark.xfail(
    reason="Requires kernel-argument dependency analysis to deduce the "
    "spaces of the fields passed to the built-in kernel")
def test_multiply_fields_on_different_spaces():
    ''' Test that we raise an error if multiply_fields() is called for
    two fields that are on different spaces '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.3.3_multiply_fields_different_spaces.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    with pytest.raises(GenerationError) as excinfo:
        _ = str(psy.gen)
    assert "some string" in str(excinfo.value)


@pytest.mark.xfail(
    reason="Dependency analysis of kernel arguments within an invoke is "
    "not yet implemented")
def test_multiply_fields_deduce_space():
    ''' Test that we generate correct code if multiply_fields() is called
    in an invoke containing another kernel that allows the space of the
    fields to be deduced '''
    distmem = False
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.3.1_multiply_fields_deduce_space.f90"),
        distributed_memory=distmem,
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=distmem).create(invoke_info)
    code = str(psy.gen)
    print code
    output = (
        "some fortran\n"
    )
    assert output in code


def test_inc_field_str():
    ''' Test that the str method of DynIncFieldKern returns the
    expected string '''
    distmem = False
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.7.0_inc_field_invoke.f90"),
                           distributed_memory=distmem,
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=distmem).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].children[0]
    assert str(kern) == "Built-in: Increment field"


def test_inc_field():
    ''' Test that we generate correct code for the built-in y = y + x
    where x and y are both fields '''
    distmem = False
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.7.0_inc_field_invoke.f90"),
        distributed_memory=distmem,
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=distmem).create(invoke_info)
    code = str(psy.gen)
    print code
    output = (
        "      ndf_any_space_1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_any_space_1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Call our kernels\n"
        "      !\n"
        "      DO df=1,undf_any_space_1\n"
        "        f1_proxy%data(df) = f1_proxy%data(df) + f2_proxy%data(df)\n"
        "      END DO \n")
    assert output in code


def test_multiply_fields_str():
    ''' Test the str method of DynMultiplyFieldsKern '''
    distmem = False
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.3.0_multiply_fields.f90"),
        distributed_memory=distmem,
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=distmem).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].children[0]
    assert str(kern) == "Built-in: Multiply fields"


def test_multiply_fields():
    ''' Test that we generate correct code for the built-in z = x*y
    where x, y and z are fields '''
    distmem = False
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.3.0_multiply_fields.f90"),
        distributed_memory=distmem,
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=distmem).create(invoke_info)
    code = str(psy.gen)
    print code
    output = (
        "      ndf_any_space_1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_any_space_1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Call our kernels\n"
        "      !\n"
        "      DO df=1,undf_any_space_1\n"
        "        f3_proxy%data(df) = f1_proxy%data(df) * f2_proxy%data(df)\n"
        "      END DO \n")
    assert output in code


def test_scale_field_str():
    ''' Test the str method of DynScaleFieldKern '''
    distmem = False
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.2.2_scale_field_builtin.f90"),
        distributed_memory=distmem,
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=distmem).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].children[0]
    assert str(kern) == "Built-in: scale a field"


def test_scale_field():
    ''' Test that DynScaleFieldKern generates correct code '''
    distmem = False
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.2.2_scale_field_builtin.f90"),
        distributed_memory=distmem,
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=distmem).create(invoke_info)
    code = str(psy.gen)
    print code
    output = (
        "      ndf_any_space_1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_any_space_1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Call our kernels\n"
        "      !\n"
        "      DO df=1,undf_any_space_1\n"
        "        f1_proxy%data(df) = a_scalar*f1_proxy%data(df)\n"
        "      END DO \n"
        "      !\n")
    if distmem:
        output += (
            "      ! Set halos dirty for fields modified in the above loop\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()\n")
    assert output in code


def test_innerprod_str():
    ''' Test the str method of DynInnerProductKern '''
    distmem = False
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.9.0_inner_prod_builtin.f90"),
        distributed_memory=distmem,
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=distmem).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].children[0]
    assert str(kern) == "Built-in: inner_product"


def test_innerprod():
    ''' Test that we produce correct code for the inner product built-in '''
    distmem = False
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.9.0_inner_prod_builtin.f90"),
        distributed_memory=distmem,
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=distmem).create(invoke_info)
    code = str(psy.gen)
    print code
    output = (
        "      !\n"
        "      ! Zero summation variables\n"
        "      !\n"
        "      asum = 0.0_r_def\n"
        "      !\n"
        "      ! Initialise field proxies\n"
        "      !\n"
        "      f1_proxy = f1%get_proxy()\n"
        "      f2_proxy = f2%get_proxy()\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f1_proxy%vspace%get_nlayers()\n"
        "      !\n"
        "      ! Initialise sizes and allocate any basis arrays for "
        "any_space_1\n"
        "      !\n"
        "      ndf_any_space_1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_any_space_1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Call our kernels\n"
        "      !\n"
        "      DO df=1,undf_any_space_1\n"
        "        asum = asum+f1_proxy%data(df)*f2_proxy%data(df)\n"
        "      END DO \n"
        "      !\n")
    assert output in code


def test_sumfield_str():
    ''' Test the str method of DynSumFieldKern '''
    distmem = False
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.10.0_sum_field_builtin.f90"),
        distributed_memory=distmem,
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=distmem).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].children[0]
    assert str(kern) == "Built-in: sum_field"


def test_sumfield():
    ''' Test that the DynSumFieldKern produces correct code '''
    distmem = False
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.10.0_sum_field_builtin.f90"),
        distributed_memory=distmem,
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=distmem).create(invoke_info)
    code = str(psy.gen)
    print code
    output = (
        "      !\n"
        "      ! Zero summation variables\n"
        "      !\n"
        "      asum = 0.0_r_def\n"
        "      !\n"
        "      ! Initialise field proxies\n"
        "      !\n"
        "      f1_proxy = f1%get_proxy()\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f1_proxy%vspace%get_nlayers()\n"
        "      !\n"
        "      ! Initialise sizes and allocate any basis arrays for "
        "any_space_1\n"
        "      !\n"
        "      ndf_any_space_1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_any_space_1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Call our kernels\n"
        "      !\n"
        "      DO df=1,undf_any_space_1\n"
        "        asum = asum+f1_proxy%data(df)\n"
        "      END DO \n"
        "      !\n")
    assert output in code