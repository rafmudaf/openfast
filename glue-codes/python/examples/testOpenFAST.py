#%%
import OpenFAST as OF
import numpy as np
from pathlib import Path
project_root = (Path(__file__).parent/ "../../../").resolve()
library_path = str(project_root / 'install/lib/libopenfastlib.so')
reg_test_path = str(project_root / "reg_tests/r-test/glue-codes/openfast-cpp/5MW_Land_DLL_WTurb_cpp/5MW_Land_DLL_WTurb_cpp.fst")
# reg_test_path = project_root + "/reg_tests/r-test/glue-codes/openfast/AOC_YFix_WSt/AOC_YFix_WSt.fst"


def serial(input_file):
    input_file_name = input_file  # "/Users/rmudafor/Development/weis/reg_tests/r-test/glue-codes/openfast/AOC_YFix_WSt/AOC_YFix_WSt.fst"
    # openfastlib.fast_run()
    of = OF.OpenFASTStandAlone(input_file_name, 1, 0, 11.)
    of.fast_run()
    print(of.output_values[:,0])

    # Display the outputs
    # for i, c in enumerate(openfastlib.output_channel_names):
    #     print(i, c)
    # print(openfastlib.output_channel_names)
    # print(openfastlib.output_values)
    # print(openfastlib.output_values[:,0])   # Prints the time steps

def openfoam(input_file_name):
    n_points_per_blade = 32
    n_points_tower = 10
    turbine_position = (0,0,100)
    rotor_area = np.pi*63**2
    air_density = 1.225

    v_0 = 9

    of = OF.OpenFastCoupled(input_file_name, 1, 0, 25, n_points_per_blade, n_points_tower, turbine_position, 0.5, rotor_area, air_density)
    for i in range(of.total_time_steps):
        of.set_velocity(0, 0, 0,
                        np.ones(n_points_per_blade*3)*v_0,  np.zeros(n_points_per_blade*3), np.zeros(n_points_per_blade*3), 
                        np.ones(n_points_tower)*v_0, np.zeros(n_points_tower),np.zeros(n_points_tower))
        of.fast_step()
    fx, fy, fz = of.get_blade_forces()
    print(fx)

    of.fast_end()

def parallel():
    ## Parallel with MPI
    from mpi4py import MPI
    comm = MPI.COMM_WORLD
    rank = comm.Get_rank()

    if rank == 0:
        input_file_name = "{}/reg_tests/r-test/glue-codes/openfast/AOC_WSt/AOC_WSt.fst".format(project_root)
        of = OF.OpenFASTStandAlone(input_file_name, 1, 0, 11.0)
        of.fast_run()
    elif rank == 1:
        input_file_name = "{}/reg_tests/r-test/glue-codes/openfast/AOC_YFix_WSt/AOC_YFix_WSt.fst".format(project_root)
        of = OF.OpenFASTStandAlone(input_file_name, 1, 0, 11.0)
        of.fast_run()

    elif rank == 2:
        input_file_name = "{}/reg_tests/r-test/glue-codes/openfast/AOC_YFree_WTurb/AOC_YFree_WTurb.fst".format(project_root)
        of = OF.OpenFASTStandAlone(input_file_name, 1, 0, 11.0)
        of.fast_run()

    elif rank == 3:
        input_file_name = "{}/reg_tests/r-test/glue-codes/openfast/AWT_YFix_WSt/AWT_YFix_WSt.fst".format(project_root)
        of = OF.OpenFASTStandAlone(input_file_name, 1, 0, 11.0)
        of.fast_run()

if __name__=="__main__":
    openfoam(reg_test_path)

# %%
