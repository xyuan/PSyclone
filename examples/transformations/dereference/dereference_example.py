from transformations import DereferenceTrans
from psyGen import Loop

def trans(psy):
    dtrans = DereferenceTrans()

    # Loop over all of the Invokes in the PSy object
    for invoke in psy.invokes.invoke_list:

        print "Transforming invoke '"+invoke.name+"'..."
        schedule = invoke.schedule

        # Switch-on the generation of a de-referencing routine
        # for this invoke
        dtrans.apply(schedule)

        schedule.view()
        invoke.schedule = schedule

    return psy


if __name__ == "__main__":
    from parse import parse
    from psyGen import PSyFactory
    import os
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "..", "..", "..", "src", "tests",
                                 "test_files", "dynamo0p3",
                                 "1_single_invoke.f90"),
                    api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(info)

    trans(psy)

    print psy.gen
