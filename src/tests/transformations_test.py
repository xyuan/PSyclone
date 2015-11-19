# -------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2015.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
# -------------------------------------------------------------------------
# Authors R. Ford and A. R. Porter, STFC Daresbury Lab

''' Module containing tests of Transformations that are not specific to
    any API '''

from utils import get_invoke
import os
import pytest
from transformations import TransformationError, \
                            OpenACCParallelTrans, \
                            OpenACCDataTrans

GO_API = "gocean1.0"
DYN_API = "dynamo0.3"


def test_acc_parallel_not_a_loop():
    ''' Test that we raise an appropriate error if we attempt
    to apply the OpenACC Parallel transformation to something that
    is not a loop '''
    _, invoke = get_invoke(
        os.path.join("gocean1p0", "single_invoke_three_kernels.f90"),
        GO_API, 0)
    schedule = invoke.schedule

    acct = OpenACCParallelTrans()
    # Attempt to (erroneously) apply the OpenACC Parallel transformation
    # to the schedule rather than a loop
    with pytest.raises(TransformationError):
        _, _ = acct.apply(schedule)


def test_acc_parallel_trans():
    ''' Test that we can apply an OpenACC parallel transformation
    to a loop '''
    psy, invoke = get_invoke(
        os.path.join("gocean1p0", "single_invoke_three_kernels.f90"),
        GO_API, 0)
    schedule = invoke.schedule

    acct = OpenACCParallelTrans()
    # Apply the OpenACC Parallel transformation
    # to the first loop of the schedule
    new_sched, _ = acct.apply(schedule.children[0])

    invoke.shedule = new_sched

    code = str(psy.gen)

    acc_idx = -1
    acc_end_idx = -1
    do_idx = -1
    for idx, line in enumerate(code.split('\n')):
        if "!$acc parallel present(" in line:
            acc_idx = idx
        if (do_idx == -1) and "DO j" in line:
            do_idx = idx
        if "!$acc end parallel" in line:
            acc_end_idx = idx

    assert acc_idx != -1 and acc_end_idx != -1
    assert acc_end_idx > acc_idx
    assert do_idx == (acc_idx + 1)


def test_acc_data_not_a_schedule():
    ''' Test that we raise an appropriate error if we attempt to apply
    an OpenACC Data transformation to something that is not a Schedule '''
    psy, invoke = get_invoke(
        os.path.join("gocean1p0", "single_invoke_three_kernels.f90"),
        GO_API, 0)
    schedule = invoke.schedule

    acct = OpenACCDataTrans()

    with pytest.raises(TransformationError):
        _, _ = acct.apply(schedule.children[0])


def test_acc_data_correct_pcopy():
    ''' Test that we correctly generate the arguments to the pcopy
    clause of an OpenACC data region '''
    from psyGen import Loop
    psy, invoke = get_invoke(
        os.path.join("gocean1p0", "single_invoke_three_kernels.f90"),
        GO_API, 0)
    schedule = invoke.schedule

    accpt = OpenACCParallelTrans()
    accdt = OpenACCDataTrans()

    # Put each loop within an OpenACC parallel region
    for child in schedule.children:
        if isinstance(child, Loop):
            new_sched, _ = accpt.apply(child)

    # Create a data region for the whole schedule
    new_sched, _ = accdt.apply(new_sched)

    invoke.schedule = new_sched
    code = str(psy.gen)
    pcopy = "!$acc enter data pcopyin(cu_fld,p_fld,u_fld,cv_fld,v_fld,"
    "unew_fld,uold_fld)"

    assert pcopy in code


def test_acc_data_parallel_commute():
    '''Test that we can apply the OpenACC parallel and data
    transformations in either order'''
    from psyGen import Loop

    accpt = OpenACCParallelTrans()
    accdt = OpenACCDataTrans()

    psy, invoke = get_invoke(
        os.path.join("gocean1p0", "single_invoke_three_kernels.f90"),
        GO_API, 0)
    schedule = invoke.schedule

    # Put each loop within an OpenACC parallel region
    for child in schedule.children:
        if isinstance(child, Loop):
            new_sched, _ = accpt.apply(child)

    # Create a data region for the whole schedule
    new_sched, _ = accdt.apply(new_sched)

    invoke.schedule = new_sched
    code1 = str(psy.gen)

    # Repeat these transformations but create the region
    # before the parallel loops
    psy, invoke = get_invoke(
        os.path.join("gocean1p0", "single_invoke_three_kernels.f90"),
        GO_API, 0)
    schedule = invoke.schedule

    # Create a data region for the whole schedule
    new_sched, _ = accdt.apply(schedule)

    # Put each loop within an OpenACC parallel region
    for child in schedule.children:
        if isinstance(child, Loop):
            new_sched, _ = accpt.apply(child)

    invoke.schedule = new_sched
    code2 = str(psy.gen)

    assert code1 == code2


def test_accdata_duplicate():
    ''' Check that we raise an error if we attempt to add an OpenACC
    data directive to a schedule that already contains one '''
    from psyGen import Loop

    accdt = OpenACCDataTrans()
    accpt = OpenACCParallelTrans()

    psy, invoke = get_invoke(
        os.path.join("gocean1p0", "single_invoke_three_kernels.f90"),
        GO_API, 0)
    schedule = invoke.schedule

    # Create a data region for the whole schedule
    new_sched, _ = accdt.apply(schedule)

    # Put each loop within an OpenACC parallel region
    for child in schedule.children:
        if isinstance(child, Loop):
            new_sched, _ = accpt.apply(child)

    # Erroneously attempt to add a data region for the second time
    with pytest.raises(TransformationError):
        _, _ = accdt.apply(new_sched)
