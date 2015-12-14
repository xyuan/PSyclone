# -----------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2015.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
# -----------------------------------------------------------------------------
# Author R. Ford and A. R. Porter, STFC Daresbury Lab

''' Tests of transformations with the Dynamo 0.3 API '''

from parse import parse
from psyGen import PSyFactory
from transformations import TransformationError, \
    OMPParallelTrans, \
    Dynamo0p3ColourTrans, \
    Dynamo0p3OMPLoopTrans, \
    DynamoOMPParallelLoopTrans, \
    DynamoLoopFuseTrans, \
    KernelModuleInlineTrans, \
    DereferenceTrans
import os
import pytest

# The version of the API that the tests in this file
# exercise.
TEST_API = "dynamo0.3"


def test_colour_trans_declarations():
    ''' Check that we generate the correct variable declarations
    when doing a colouring transformation '''
    # test of the colouring transformation of a single loop
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0_testkern_type')
    schedule = invoke.schedule
    ctrans = Dynamo0p3ColourTrans()
    dtrans = DereferenceTrans()

    # Turn-on creation of de-referencing routine (PSy2)
    schedule, _ = dtrans.apply(schedule)

    # Colour the loop
    cschedule, _ = ctrans.apply(schedule.children[0])

    # Replace the original loop schedule with the transformed one
    invoke.schedule = cschedule

    # Store the results of applying this code transformation as
    # a string
    gen = str(psy.gen)
    # Fortran is not case sensitive
    gen = gen.lower()
    print gen

    # Check that we've declared the loop-related variables
    # and colour-map pointers
    assert "integer ncolour" in gen
    assert "integer colour" in gen
    assert "integer, pointer :: cmap_w1(:,:), ncp_colour_w1(:)" in gen
    # and that we're passing them into the deref routine
    assert "subroutine invoke_0_testkern_type_arrays(f1_proxy, f2_proxy, "
    "m1_proxy, m2_proxy, ncells, nlayers, ndf_w1, undf_w1, map_w1, ndf_w2, "
    "undf_w2, map_w2, ndf_w3, undf_w3, map_w3, ncolour_w1, ncp_colour_w1, "
    "cmap_w1)" in gen
    assert "integer, intent(in), dimension(ndf_w1,0:ncells) :: map_w1" in gen


def test_colour_trans_declarations_no_deref():
    ''' Check that we generate the correct variable declarations
    when doing a colouring transformation when the schedule has
    no de-referencing routine '''
    # test of the colouring transformation of a single loop
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0_testkern_type')
    schedule = invoke.schedule
    ctrans = Dynamo0p3ColourTrans()
    dtrans = DereferenceTrans()
    # Turn-off the de-referencing routine
    schedule, _ = dtrans.apply(schedule, deref=False)

    # Colour the loop
    cschedule, _ = ctrans.apply(schedule.children[0])

    # Replace the original loop schedule with the transformed one
    invoke.schedule = cschedule

    # Store the results of applying this code transformation as
    # a string
    gen = str(psy.gen)
    # Fortran is not case sensitive
    gen = gen.lower()
    print gen

    # Check that we've declared the loop-related variables
    # and colour-map pointers
    assert "integer ncolour" in gen
    assert "integer colour" in gen
    assert "integer, pointer :: cmap_w1(:,:), ncp_colour_w1(:)" in gen


def test_colour_trans():
    ''' test of the colouring transformation of a single loop '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0_testkern_type')
    schedule = invoke.schedule
    ctrans = Dynamo0p3ColourTrans()

    # Colour the loop
    cschedule, _ = ctrans.apply(schedule.children[0])

    # Replace the original loop schedule with the transformed one
    invoke.schedule = cschedule

    # Store the results of applying this code transformation as
    # a string
    gen = str(psy.gen)
    # Fortran is not case sensitive
    gen = gen.lower()
    print gen

    # Check that we're calling the API to get the no. of colours
    assert "f1_proxy%vspace%get_colours(" in gen

    col_loop_idx = -1
    cell_loop_idx = -1
    for idx, line in enumerate(gen.split('\n')):
        if "do colour=1,ncolour_w1" in line:
            col_loop_idx = idx
        if "do cell=1,ncp_colour_w1(colour)" in line:
            cell_loop_idx = idx

    assert cell_loop_idx - col_loop_idx == 1

    # Check that we're using the colour map when getting the cell dof maps
    assert "map_w1(:,cmap_w1(colour, cell))" in gen


def test_colour_trans_no_deref():
    ''' test of the colouring transformation of a single loop when we are
    not generating a de-referencing routine '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0_testkern_type')
    schedule = invoke.schedule
    ctrans = Dynamo0p3ColourTrans()
    dtrans = DereferenceTrans()

    # Turn off the dereferencing routine
    schedule, _ = dtrans.apply(schedule, deref=False)
    # Colour the loop
    cschedule, _ = ctrans.apply(schedule.children[0])

    # Replace the original loop schedule with the transformed one
    invoke.schedule = cschedule

    # Store the results of applying this code transformation as
    # a string
    gen = str(psy.gen)
    # Fortran is not case sensitive
    gen = gen.lower()
    print gen

    # Check that we're calling the API to get the no. of colours
    assert "f1_proxy%vspace%get_colours(" in gen

    col_loop_idx = -1
    cell_loop_idx = -1
    for idx, line in enumerate(gen.split('\n')):
        if "do colour=1,ncolour_w1" in line:
            col_loop_idx = idx
        if "do cell=1,ncp_colour_w1(colour)" in line:
            cell_loop_idx = idx

    assert cell_loop_idx - col_loop_idx == 1

    # Check that we're using the colour map when getting the cell dof maps
    assert "map_w1(:,cmap_w1(colour, cell))" in gen


def test_colouring_not_a_loop():
    ''' Test that we raise an appropriate error if we attempt to
    colour something that is not a loop '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0_testkern_type')
    schedule = invoke.schedule
    ctrans = Dynamo0p3ColourTrans()

    # Erroneously attempt to colour the schedule rather than the loop
    with pytest.raises(TransformationError):
        _, _ = ctrans.apply(schedule)


def test_omp_name():
    ''' Test the name property of the Dynamo0p3OMPLoopTrans class '''
    olooptrans = Dynamo0p3OMPLoopTrans()
    oname = olooptrans.name
    assert oname == "Dynamo0p3OMPLoopTrans"


def test_omp_str():
    ''' Test the str method of the Dynamo0p3OMPLoopTrans class '''
    olooptrans = Dynamo0p3OMPLoopTrans()
    oname = str(olooptrans)
    assert oname == "Add an OpenMP DO directive to a Dynamo 0.3 loop"


def test_omp_not_a_loop():
    ''' Test that we raise an appropriate error if we attempt to
    apply an OpenMP DO transformation to something that is not a loop '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0_testkern_type')
    schedule = invoke.schedule
    otrans = Dynamo0p3OMPLoopTrans()

    # Erroneously attempt to apply OpenMP to the schedule rather than
    # the loop
    with pytest.raises(TransformationError):
        _, _ = otrans.apply(schedule)


def test_omp_do_not_over_cells():
    ''' Test that we raise an appropriate error if we attempt to
    apply an OpenMP DO transformation to a loop that is not over cells '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1.4_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0_testkern_type')
    schedule = invoke.schedule
    otrans = Dynamo0p3OMPLoopTrans()

    with pytest.raises(TransformationError):
        _, _ = otrans.apply(schedule.children[0])


def test_omp_parallel_do_not_over_cells():
    ''' Test that we raise an appropriate error if we attempt to apply
    an OpenMP PARALLEL DO transformation to a loop that is not over cells '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1.4_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0_testkern_type')
    schedule = invoke.schedule
    otrans = DynamoOMPParallelLoopTrans()

    with pytest.raises(TransformationError):
        _, _ = otrans.apply(schedule.children[0])


def test_omp_parallel_not_a_loop():
    '''Test that we raise an appropriate error if we attempt to apply an
    OpenMP PARALLEL DO transformation to something that is not a loop

    '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0_testkern_type')
    schedule = invoke.schedule
    otrans = DynamoOMPParallelLoopTrans()

    # Erroneously attempt to apply OpenMP to the schedule rather than
    # the loop
    with pytest.raises(TransformationError):
        _, _ = otrans.apply(schedule)


def test_colour_name():
    ''' Test the name property of the Dynamo0p3ColourTrans class '''
    ctrans = Dynamo0p3ColourTrans()
    cname = ctrans.name
    assert cname == "Dynamo0p3LoopColourTrans"


def test_colour_str():
    ''' Test the str method of the Dynamo0p3ColourTrans class '''
    ctrans = Dynamo0p3ColourTrans()
    cstr = str(ctrans)
    assert cstr == "Split a Dynamo 0.3 loop into colours"


def test_omp_colour_trans():
    ''' Test the OpenMP transformation applied to a coloured loop '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0_testkern_type')
    schedule = invoke.schedule

    ctrans = Dynamo0p3ColourTrans()
    otrans = DynamoOMPParallelLoopTrans()

    # Colour the loop
    cschedule, _ = ctrans.apply(schedule.children[0])

    # Then apply OpenMP to the inner loop
    schedule, _ = otrans.apply(cschedule.children[0].children[0])

    invoke.schedule = schedule
    code = str(psy.gen)
    print code

    col_loop_idx = -1
    omp_idx = -1
    cell_loop_idx = -1
    for idx, line in enumerate(code.split('\n')):
        if "DO colour=1,ncolour_w1" in line:
            col_loop_idx = idx
        if "DO cell=1,ncp_colour_w1(colour)" in line:
            cell_loop_idx = idx
        if "!$omp parallel do" in line:
            omp_idx = idx

    assert cell_loop_idx - omp_idx == 1
    assert omp_idx - col_loop_idx == 1

    # Check that the list of private variables is correct
    assert "private(cell)" in code


def test_omp_colour_trans_no_deref():
    ''' Test the OpenMP transformation applied to a coloured loop when we
    do not have a de-referencing routine '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0_testkern_type')
    schedule = invoke.schedule

    ctrans = Dynamo0p3ColourTrans()
    otrans = DynamoOMPParallelLoopTrans()
    dtrans = DereferenceTrans()

    # Turn off the de-referencing routine
    schedule, _ = dtrans.apply(schedule, deref=False)

    # Colour the loop
    cschedule, _ = ctrans.apply(schedule.children[0])

    # Then apply OpenMP to the inner loop
    schedule, _ = otrans.apply(cschedule.children[0].children[0])

    invoke.schedule = schedule
    code = str(psy.gen)
    print code

    col_loop_idx = -1
    omp_idx = -1
    cell_loop_idx = -1
    for idx, line in enumerate(code.split('\n')):
        if "DO colour=1,ncolour_w1" in line:
            col_loop_idx = idx
        if "DO cell=1,ncp_colour_w1(colour)" in line:
            cell_loop_idx = idx
        if "!$omp parallel do" in line:
            omp_idx = idx

    assert cell_loop_idx - omp_idx == 1
    assert omp_idx - col_loop_idx == 1

    # Check that the list of private variables is correct
    assert "private(cell)" in code


def test_omp_colour_not_orient_trans():
    ''' Test the OpenMP transformation applied to a coloured loop
        when the kernel expects orientation information on a space
        other than the one being coloured '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "9_orientation.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0_testkern_orientation_type')
    schedule = invoke.schedule

    ctrans = Dynamo0p3ColourTrans()
    otrans = DynamoOMPParallelLoopTrans()
    dtrans = DereferenceTrans()

    # Colour the loop
    cschedule, _ = ctrans.apply(schedule.children[0])

    # Then apply OpenMP to the inner loop
    schedule, _ = otrans.apply(cschedule.children[0].children[0])

    invoke.schedule = schedule
    code = str(psy.gen)
    print code

    # Check that we're not using the colour map when getting the orientation
    # since in this case, the space for which the orientation is required
    # is not the one that is coloured
    assert "orientation_w2(:,cell)" in code

    # Check that the list of private variables is correct
    assert "private(cell)" in code

    # Repeat these checks after switching-off the dereferencing routine
    schedule, _ = dtrans.apply(schedule, deref=False)
    invoke.schedule = schedule
    code = str(psy.gen)
    assert "orientation_w2(:,cell)" in code
    assert "private(cell)" in code


def test_omp_colour_orient_trans():
    ''' Test the OpenMP transformation applied to a coloured loop
        when the kernel expects orientation information on the same
        space that has been coloured '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "9.1_orientation.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0_testkern_orientation_type')
    schedule = invoke.schedule

    ctrans = Dynamo0p3ColourTrans()
    otrans = DynamoOMPParallelLoopTrans()
    dtrans = DereferenceTrans()

    # Colour the loop
    cschedule, _ = ctrans.apply(schedule.children[0])

    # Then apply OpenMP to the inner loop
    schedule, _ = otrans.apply(cschedule.children[0].children[0])

    invoke.schedule = schedule
    code = str(psy.gen)
    print code

    # Check that we're not using the colour map when getting the orientation
    # since in this case, the space for which the orientation is required
    # is not the one that is coloured
    assert "=> f2_proxy%vspace%get_orientation()" in code

    # Check that the list of private variables is correct
    assert "private(cell)" in code

    # Repeat these checks after switching-off the dereferencing routine
    schedule, _ = dtrans.apply(schedule, deref=False)
    invoke.schedule = schedule
    code = str(psy.gen)
    assert "=> f2_proxy%vspace%get_orientation()" in code
    assert "private(cell)" in code


def test_omp_parallel_colouring_needed():
    '''Test that we raise an error when applying an OpenMP PARALLEL DO
    transformation to a loop that requires colouring (i.e. has a field
    with 'INC' access) but is not coloured.

    '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "11_any_space.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0_testkern_any_space_1_type')
    schedule = invoke.schedule

    otrans = DynamoOMPParallelLoopTrans()
    # Apply OpenMP to the loop
    with pytest.raises(TransformationError):
        schedule, _ = otrans.apply(schedule.children[0])


def test_omp_colouring_needed():
    '''Test that we raise an error when applying an OpenMP DO
    transformation to a loop that requires colouring (i.e. has a field
    with 'INC' access) but is not coloured.

    '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "11_any_space.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0_testkern_any_space_1_type')
    schedule = invoke.schedule

    otrans = Dynamo0p3OMPLoopTrans()
    # Apply OpenMP to the loop
    with pytest.raises(TransformationError):
        schedule, _ = otrans.apply(schedule.children[0])


def test_check_seq_colours_omp_parallel_do():
    '''Test that we raise an error if the user attempts to apply an
    OpenMP PARALLEL DO transformation to a loop over colours (since
    any such loop must be sequential)

    '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "9_orientation.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0_testkern_orientation_type')
    schedule = invoke.schedule

    ctrans = Dynamo0p3ColourTrans()
    otrans = DynamoOMPParallelLoopTrans()

    # Colour the loop
    cschedule, _ = ctrans.apply(schedule.children[0])

    # Then erroneously attempt to apply OpenMP to the loop over
    # colours
    with pytest.raises(TransformationError):
        schedule, _ = otrans.apply(cschedule.children[0])


def test_check_seq_colours_omp_do():
    '''Test that we raise an error if the user attempts to apply an OpenMP
    DO transformation to a loop over colours (since any such loop must
    be sequential)

    '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "9_orientation.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0_testkern_orientation_type')
    schedule = invoke.schedule

    ctrans = Dynamo0p3ColourTrans()
    otrans = Dynamo0p3OMPLoopTrans()

    # Colour the loop
    cschedule, _ = ctrans.apply(schedule.children[0])

    # Then erroneously attempt to apply OpenMP to the loop over
    # colours
    with pytest.raises(TransformationError):
        schedule, _ = otrans.apply(cschedule.children[0])


def test_colouring_after_openmp():
    ''' Test that we raise an error if the user attempts to
    colour a loop that is already within an OpenMP parallel region '''
    # For this test we must use a kernel that doesn't actually require
    # colouring as otherwise PSyclone won't let us apply the OpenMP
    # transformation first!
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "9_orientation.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0_testkern_orientation_type')
    schedule = invoke.schedule

    ctrans = Dynamo0p3ColourTrans()
    otrans = DynamoOMPParallelLoopTrans()

    # Apply OpenMP to the loop
    schedule, _ = otrans.apply(schedule.children[0])

    # Now attempt to colour the loop within this OpenMP region
    with pytest.raises(TransformationError):
        schedule, _ = ctrans.apply(schedule.children[0].children[0])


def test_colouring_multi_kernel():
    ''' Test that we correctly generate all the map-lookups etc.
    when an invoke contains more than one kernel '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4.6_multikernel_invokes.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0')
    schedule = invoke.schedule

    ctrans = Dynamo0p3ColourTrans()
    otrans = DynamoOMPParallelLoopTrans()
    dtrans = DereferenceTrans()

    for child in schedule.children:
        newsched, _ = ctrans.apply(child)

    # Apply OpenMP to each of the colour loops
    schedule = newsched
    for child in schedule.children:
        newsched, _ = otrans.apply(child.children[0])

    invoke.schedule = newsched
    gen = str(psy.gen)
    print gen
    # Check that we're calling the API to get the no. of colours
    assert "a_proxy%vspace%get_colours(" in gen
    assert "private(cell)" in gen

    # Repeat these checks after switching-off the dereferencing routine
    schedule, _ = dtrans.apply(newsched, deref=False)
    invoke.schedule = schedule
    gen = str(psy.gen)
    assert "a_proxy%vspace%get_colours(" in gen
    assert "private(cell)" in gen


def test_omp_region_omp_do():
    ''' Test that we correctly generate code for the case of a single
    OMP DO within an OMP PARALLEL region without colouring '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0_testkern_type')
    schedule = invoke.schedule
    olooptrans = Dynamo0p3OMPLoopTrans()
    ptrans = OMPParallelTrans()
    dtrans = DereferenceTrans()

    # Put an OMP PARALLEL around this loop
    child = schedule.children[0]
    oschedule, _ = ptrans.apply(child)

    # Put an OMP DO around this loop
    schedule, _ = olooptrans.apply(oschedule.children[0].children[0])

    # Replace the original loop schedule with the transformed one
    invoke.schedule = schedule

    # Store the results of applying this code transformation as a string
    code = str(psy.gen)
    expected = (
        "!$omp parallel default(shared), private(cell)\n"
        "      !$omp do schedule(static)\n"
        "      DO cell=1,ncells\n")
    assert expected in code

    # Repeat the test with the de-referencing routine switched off
    schedule, _ = dtrans.apply(schedule, deref=False)
    invoke.schedule = schedule
    code = str(psy.gen)
    print code
    assert expected in code


def test_multi_kernel_single_omp_region():
    ''' Test that we correctly generate all the map-lookups etc.
    when an invoke contains more than one kernel that are all contained
    within a single OMP region '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4_multikernel_invokes.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0')
    schedule = invoke.schedule

    otrans = Dynamo0p3OMPLoopTrans()
    rtrans = OMPParallelTrans()
    dtrans = DereferenceTrans()
    schedule, _ = dtrans.apply(schedule)

    # Apply OpenMP to each of the loops
    for child in schedule.children:
        newsched, _ = otrans.apply(child)

    # Enclose all of these OpenMP'd loops within a single region
    newsched, _ = rtrans.apply(newsched.children)

    invoke.schedule = newsched
    code = str(psy.gen)
    print code
    expected = (
        "      !$omp parallel default(shared), private(cell)\n"
        "      !$omp do schedule(static)\n"
        "      DO cell=1,ncells\n"
        "        CALL testkern_code(nlayers, f1_proxy, f2_proxy, m1_proxy, "
        "m2_proxy, ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, "
        "map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))\n"
        "      END DO \n"
        "      !$omp end do\n"
        "      !$omp do schedule(static)\n"
        "      DO cell=1,ncells\n"
        "        CALL testkern_code(nlayers, f1_proxy, f2_proxy, m1_proxy, "
        "m2_proxy, ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, "
        "map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))\n"
        "      END DO \n"
        "      !$omp end do\n"
        "      !$omp end parallel\n")
    assert expected in code


def test_multi_kernel_single_omp_region_no_deref():
    '''Test that we correctly generate all the map-lookups etc.  when an
    invoke (with no dereferencing routine) contains more than one
    kernel that are all contained within a single OMP region

    '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4_multikernel_invokes.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0')
    schedule = invoke.schedule

    otrans = Dynamo0p3OMPLoopTrans()
    rtrans = OMPParallelTrans()
    dtrans = DereferenceTrans()

    # Apply OpenMP to each of the loops
    for child in schedule.children:
        newsched, _ = otrans.apply(child)

    # Enclose all of these OpenMP'd loops within a single region
    newsched, _ = rtrans.apply(newsched.children)

    schedule, _ = dtrans.apply(newsched, deref=False)
    invoke.schedule = newsched
    code = str(psy.gen)
    print code
    expected = (
        "      !$omp parallel default(shared), private(cell)\n"
        "      !$omp do schedule(static)\n"
        "      DO cell=1,ncells\n"
        "        CALL testkern_code(nlayers, f1_proxy%data, f2_proxy%data, "
        "m1_proxy%data, m2_proxy%data, ndf_w1, undf_w1, map_w1(:,cell), "
        "ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))\n"
        "      END DO \n"
        "      !$omp end do\n"
        "      !$omp do schedule(static)\n"
        "      DO cell=1,ncells\n"
        "        CALL testkern_code(nlayers, f1_proxy%data, f2_proxy%data, "
        "m1_proxy%data, m2_proxy%data, ndf_w1, undf_w1, map_w1(:,cell), "
        "ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))\n"
        "      END DO \n"
        "      !$omp end do\n"
        "      !$omp end parallel\n")
    assert expected in code


def test_loop_fuse_different_spaces():
    ''' Test that we raise an appropriate error if the user attempts
    to fuse loops that are on different spaces '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4.7_multikernel_invokes.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0')
    schedule = invoke.schedule

    ftrans = DynamoLoopFuseTrans()

    with pytest.raises(TransformationError):
        _, _ = ftrans.apply(schedule.children[0],
                            schedule.children[1])


def test_loop_fuse_unexpected_error():
    ''' Test that we catch an unexpected error when loop fusing '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4_multikernel_invokes.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0')
    schedule = invoke.schedule

    ftrans = DynamoLoopFuseTrans()

    # cause an unexpected error
    schedule.children[0].children = None

    with pytest.raises(TransformationError) as excinfo:
        _, _ = ftrans.apply(schedule.children[0],
                            schedule.children[1])
    assert 'Unexpected exception' in str(excinfo.value)


def test_loop_fuse():
    ''' Test that we are able to fuse two loops together '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4_multikernel_invokes.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0')
    schedule = invoke.schedule

    ftrans = DynamoLoopFuseTrans()
    dtrans = DereferenceTrans()

    # Turn-on creation of de-referencing routine (PSy2)
    schedule, _ = dtrans.apply(schedule)

    # Fuse the loops
    nchildren = len(schedule.children)
    idx = 1
    fschedule = schedule
    while idx < nchildren:
        fschedule, _ = ftrans.apply(fschedule.children[idx-1],
                                    fschedule.children[idx])
        idx += 1

    invoke.schedule = fschedule
    gen = str(psy.gen)
    print gen
    expected = (
        "      DO cell=1,ncells\n"
        "        CALL testkern_code(nlayers, f1_proxy, f2_proxy, m1_proxy, "
        "m2_proxy, ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, "
        "map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))\n"
        "        CALL testkern_code(nlayers, f1_proxy, f2_proxy, m1_proxy, "
        "m2_proxy, ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, "
        "map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))\n"
        "      END DO \n")
    assert expected in gen


def test_loop_fuse_no_deref():
    ''' Test that we are able to fuse two loops together when not using
    a de-referencing routine '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4_multikernel_invokes.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0')
    schedule = invoke.schedule

    ftrans = DynamoLoopFuseTrans()
    dtrans = DereferenceTrans()

    # Turn-off the de-referencing routine
    schedule, _ = dtrans.apply(schedule, deref=False)

    # Fuse the loops
    nchildren = len(schedule.children)
    idx = 1
    fschedule = schedule
    while idx < nchildren:
        fschedule, _ = ftrans.apply(fschedule.children[idx-1],
                                    fschedule.children[idx])
        idx += 1

    invoke.schedule = fschedule
    gen = str(psy.gen)
    print gen
    expected = (
        "      DO cell=1,ncells\n"
        "        CALL testkern_code(nlayers, f1_proxy%data, f2_proxy%data, "
        "m1_proxy%data, m2_proxy%data, ndf_w1, undf_w1, map_w1(:,cell), "
        "ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))\n"
        "        CALL testkern_code(nlayers, f1_proxy%data, f2_proxy%data, "
        "m1_proxy%data, m2_proxy%data, ndf_w1, undf_w1, map_w1(:,cell), "
        "ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))\n"
        "      END DO \n")
    assert expected in gen


def test_loop_fuse_omp():
    '''Test that we can loop-fuse two loop nests and enclose them in an
       OpenMP parallel region'''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4_multikernel_invokes.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0')
    schedule = invoke.schedule

    ftrans = DynamoLoopFuseTrans()
    otrans = DynamoOMPParallelLoopTrans()
    dtrans = DereferenceTrans()

    # Turn on the creation of a de-referencing routine
    schedule, _ = dtrans.apply(schedule)

    # Fuse the loops
    nchildren = len(schedule.children)
    idx = 1
    fschedule = schedule
    while idx < nchildren:
        fschedule, _ = ftrans.apply(fschedule.children[idx-1],
                                    fschedule.children[idx])
        idx += 1

    fschedule, _ = otrans.apply(fschedule.children[0])

    invoke.schedule = fschedule
    code = str(psy.gen)
    print code

    # Check generated code
    expected = (
        "      !$omp parallel do default(shared), private(cell), "
        "schedule(static)\n"
        "      DO cell=1,ncells\n"
        "        CALL testkern_code(nlayers, f1_proxy, f2_proxy, m1_proxy, "
        "m2_proxy, ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, "
        "map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))\n"
        "        CALL testkern_code(nlayers, f1_proxy, f2_proxy, m1_proxy, "
        "m2_proxy, ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, "
        "map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))\n"
        "      END DO \n"
        "      !$omp end parallel do\n")
    assert expected in code


def test_loop_fuse_omp_no_deref():
    '''Test that we can loop-fuse two loop nests and enclose them in an
       OpenMP parallel region when we do not have a de-referencing routine '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4_multikernel_invokes.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0')
    schedule = invoke.schedule

    ftrans = DynamoLoopFuseTrans()
    otrans = DynamoOMPParallelLoopTrans()
    dtrans = DereferenceTrans()

    # Turn off the creation of a de-referencing routine
    schedule, _ = dtrans.apply(schedule, deref=False)

    # Fuse the loops
    nchildren = len(schedule.children)
    idx = 1
    fschedule = schedule
    while idx < nchildren:
        fschedule, _ = ftrans.apply(fschedule.children[idx-1],
                                    fschedule.children[idx])
        idx += 1

    fschedule, _ = otrans.apply(fschedule.children[0])

    invoke.schedule = fschedule
    code = str(psy.gen)
    print code
    # Check generated code
    expected = (
        "      !$omp parallel do default(shared), private(cell), "
        "schedule(static)\n"
        "      DO cell=1,ncells\n"
        "        CALL testkern_code(nlayers, f1_proxy%data, f2_proxy%data, "
        "m1_proxy%data, m2_proxy%data, ndf_w1, undf_w1, map_w1(:,cell), "
        "ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))\n"
        "        CALL testkern_code(nlayers, f1_proxy%data, f2_proxy%data, "
        "m1_proxy%data, m2_proxy%data, ndf_w1, undf_w1, map_w1(:,cell), "
        "ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))\n"
        "      END DO \n"
        "      !$omp end parallel do\n")
    assert expected in code


def test_fuse_colour_loops():
    ''' Test that we can fuse colour loops, enclose them in an OpenMP
    parallel region and preceed each by an OpenMP PARALLEL DO '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4.6_multikernel_invokes.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0')
    schedule = invoke.schedule

    ctrans = Dynamo0p3ColourTrans()
    otrans = Dynamo0p3OMPLoopTrans()
    rtrans = OMPParallelTrans()
    ftrans = DynamoLoopFuseTrans()
    dtrans = DereferenceTrans()
    # Turn-on creation of de-referencing routine PSy2
    schedule, _ = dtrans.apply(schedule)

    # Colour each of the loops
    for child in schedule.children:
        newsched, _ = ctrans.apply(child)

    # Fuse the (sequential) loops over colours
    nchildren = len(newsched.children)
    idx = 1
    fschedule = newsched
    while idx < nchildren:
        fschedule, _ = ftrans.apply(fschedule.children[idx-1],
                                    fschedule.children[idx])
        idx += 1

    # Enclose the colour loops within an OMP parallel region
    newsched, _ = rtrans.apply(fschedule.children[0].children)

    # Put an OMP DO around each of the colour loops
    for child in newsched.children[0].children[0].children:
        newsched, _ = otrans.apply(child)

    # Replace the original schedule with the transformed one and
    # generate the code
    invoke.schedule = newsched
    code = str(psy.gen)
    print code
    expected = (
        "      DO colour=1,ncolour_w2\n"
        "        !$omp parallel default(shared), private(cell)\n"
        "        !$omp do schedule(static)\n"
        "        DO cell=1,ncp_colour_w2(colour)\n"
        "          CALL ru_code(nlayers, a_proxy, b_proxy, c_proxy, "
        "d_proxy_1, d_proxy_2, d_proxy_3, ndf_w2, undf_w2, "
        "map_w2(:,cmap_w2(colour, cell)), basis_w2, diff_basis_w2, ndf_w3, "
        "undf_w3, map_w3(:,cell), basis_w3, ndf_w0, undf_w0, map_w0(:,cell), "
        "basis_w0, diff_basis_w0, nqp_h, nqp_v, wh, wv)\n"
        "        END DO \n"
        "        !$omp end do\n"
        "        !$omp do schedule(static)\n"
        "        DO cell=1,ncp_colour_w2(colour)\n"
        "          CALL ru_code(nlayers, f_proxy, b_proxy, c_proxy, "
        "d_proxy_1, d_proxy_2, d_proxy_3, ndf_w2, undf_w2, "
        "map_w2(:,cmap_w2(colour, cell)), basis_w2, diff_basis_w2, ndf_w3, "
        "undf_w3, map_w3(:,cell), basis_w3, ndf_w0, undf_w0, map_w0(:,cell), "
        "basis_w0, diff_basis_w0, nqp_h, nqp_v, wh, wv)\n"
        "        END DO \n"
        "        !$omp end do\n"
        "        !$omp end parallel\n"
        "      END DO \n")
    assert expected in code


def test_fuse_colour_loops_no_deref():
    '''Test that, in the absence of a de-referencing routine, we can fuse
    colour loops, enclose them in an OpenMP parallel region and
    preceed each by an OpenMP PARALLEL DO

    '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4.6_multikernel_invokes.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0')
    schedule = invoke.schedule

    ctrans = Dynamo0p3ColourTrans()
    otrans = Dynamo0p3OMPLoopTrans()
    rtrans = OMPParallelTrans()
    ftrans = DynamoLoopFuseTrans()
    dtrans = DereferenceTrans()

    # Switch-off the production of a de-referencing routine
    schedule, _ = dtrans.apply(schedule, deref=False)

    # Colour each of the loops
    for child in schedule.children:
        newsched, _ = ctrans.apply(child)

    # Fuse the (sequential) loops over colours
    nchildren = len(newsched.children)
    idx = 1
    fschedule = newsched
    while idx < nchildren:
        fschedule, _ = ftrans.apply(fschedule.children[idx-1],
                                    fschedule.children[idx])
        idx += 1

    # Enclose the colour loops within an OMP parallel region
    newsched, _ = rtrans.apply(fschedule.children[0].children)

    # Put an OMP DO around each of the colour loops
    for child in newsched.children[0].children[0].children:
        newsched, _ = otrans.apply(child)

    # Replace the original schedule with the transformed one and
    # generate the code
    invoke.schedule = newsched
    code = str(psy.gen)
    print code
    expected = (
        "      DO colour=1,ncolour_w2\n"
        "        !$omp parallel default(shared), private(cell)\n"
        "        !$omp do schedule(static)\n"
        "        DO cell=1,ncp_colour_w2(colour)\n"
        "          CALL ru_code(nlayers, a_proxy%data, b_proxy%data, "
        "c_proxy%data, d_proxy(1)%data, d_proxy(2)%data, d_proxy(3)%data, "
        "ndf_w2, undf_w2, map_w2(:,cmap_w2(colour, cell)), basis_w2, "
        "diff_basis_w2, ndf_w3, undf_w3, map_w3(:,cell), basis_w3, ndf_w0, "
        "undf_w0, map_w0(:,cell), basis_w0, diff_basis_w0, nqp_h, nqp_v, "
        "wh, wv)\n"
        "        END DO \n"
        "        !$omp end do\n"
        "        !$omp do schedule(static)\n"
        "        DO cell=1,ncp_colour_w2(colour)\n"
        "          CALL ru_code(nlayers, f_proxy%data, b_proxy%data, "
        "c_proxy%data, d_proxy(1)%data, d_proxy(2)%data, d_proxy(3)%data, "
        "ndf_w2, undf_w2, map_w2(:,cmap_w2(colour, cell)), basis_w2, "
        "diff_basis_w2, ndf_w3, undf_w3, map_w3(:,cell), basis_w3, ndf_w0, "
        "undf_w0, map_w0(:,cell), basis_w0, diff_basis_w0, nqp_h, nqp_v, "
        "wh, wv)\n"
        "        END DO \n"
        "        !$omp end do\n"
        "        !$omp end parallel\n"
        "      END DO \n")
    assert expected in code


def test_module_inline():
    '''Tests that correct results are obtained when a kernel is inlined
    into the psy-layer in the dynamo0.3 API. More in-depth tests can be
    found in the gocean1p0_transformations.py file'''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4.6_multikernel_invokes.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0')
    schedule = invoke.schedule
    kern_call = schedule.children[0].children[0]
    inline_trans = KernelModuleInlineTrans()

    schedule, _ = inline_trans.apply(kern_call)
    gen = str(psy.gen)
    # check that the subroutine has been inlined
    assert 'SUBROUTINE ru_code()' in gen
    # check that the associated psy "use" does not exist
    assert 'USE ru_kernel_mod, only : ru_code' not in gen


def test_module_inline_no_deref():
    '''Tests that correct results are obtained when a kernel is inlined
    into the psy-layer in the dynamo0.3 API. More in-depth tests can be
    found in the gocean1p0_transformations.py file'''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4.6_multikernel_invokes.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0')
    schedule = invoke.schedule

    inline_trans = KernelModuleInlineTrans()
    dtrans = DereferenceTrans()

    schedule, _ = dtrans.apply(schedule, deref=False)
    kern_call = schedule.children[0].children[0]
    schedule, _ = inline_trans.apply(kern_call)
    gen = str(psy.gen)
    # check that the subroutine has been inlined
    assert 'SUBROUTINE ru_code()' in gen
    # check that the associated psy "use" does not exist
    assert 'USE ru_kernel_mod, only : ru_code' not in gen
