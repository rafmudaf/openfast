
import openfast
import openfast_library
project_root = '/home/henry/Software/openFast/python_driver/openfast'
library_path = project_root + '/build/modules/openfast-library/libopenfastlib.so'
reg_test_path = project_root + "/reg_tests/r-test/glue-codes/openfast-cpp/5MW_Land_DLL_WTurb_cpp/5MW_Land_DLL_WTurb_cpp.fst"


def serial(input_file):
    input_file_name = input_file # "/Users/rmudafor/Development/weis/reg_tests/r-test/glue-codes/openfast/AOC_YFix_WSt/AOC_YFix_WSt.fst"
    openfastlib = openfast_library.FastLibAPI(library_path, input_file_name)
    # openfastlib.fast_run()
    of = openfast.OpenFASTStandAlone(openfastlib, input_file_name, 1, 0, 11.)
    of.fast_run()
    print(of.output_values[:,0])

    # Display the outputs
    # for i, c in enumerate(openfastlib.output_channel_names):
    #     print(i, c)
    # print(openfastlib.output_channel_names)
    # print(openfastlib.output_values)
    # print(openfastlib.output_values[:,0])   # Prints the time steps

def openfoam(input_file_name):
    openfastlib = openfast_library.FastLibAPI(library_path, input_file_name)

    of = openfast.OpenFastCoupled(openfastlib, input_file_name, 1, 0, 11., 10, 10, (0,0,0), 0.5, 1, 1.225)
    of.fast_end()

def parallel():
    ## Parallel with MPI
    from mpi4py import MPI
    comm = MPI.COMM_WORLD
    rank = comm.Get_rank()

    if rank == 0:
        input_file_name = "{}/reg_tests/r-test/glue-codes/openfast/AOC_WSt/AOC_WSt.fst".format(project_root)
        openfastlib = openfast_library.FastLibAPI(library_path, input_file_name)
        of = openfast.OpenFASTStandAlone(openfastlib, input_file_name, 1, 0, 11.0)
        of.fast_run()
    elif rank == 1:
        input_file_name = "{}/reg_tests/r-test/glue-codes/openfast/AOC_YFix_WSt/AOC_YFix_WSt.fst".format(project_root)
        openfastlib = openfast_library.FastLibAPI(library_path, input_file_name)
        of = openfast.OpenFASTStandAlone(openfastlib, input_file_name, 1, 0, 11.0)
        of.fast_run()

    elif rank == 2:
        input_file_name = "{}/reg_tests/r-test/glue-codes/openfast/AOC_YFree_WTurb/AOC_YFree_WTurb.fst".format(project_root)
        openfastlib = openfast_library.FastLibAPI(library_path, input_file_name)
        of = openfast.OpenFASTStandAlone(openfastlib, input_file_name, 1, 0, 11.0)
        of.fast_run()

    elif rank == 3:
        input_file_name = "{}/reg_tests/r-test/glue-codes/openfast/AWT_YFix_WSt/AWT_YFix_WSt.fst".format(project_root)
        openfastlib = openfast_library.FastLibAPI(library_path, input_file_name)
        of = openfast.OpenFASTStandAlone(openfastlib, input_file_name, 1, 0, 11.0)
        of.fast_run()

if __name__=="__main__":
    openfoam(reg_test_path)
