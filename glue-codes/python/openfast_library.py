from ctypes import (
    CDLL,
    POINTER,
    create_string_buffer,
    byref,
    cast,
    c_int,
    c_double,
    c_char,
    c_bool,
    c_float,
    Structure
)
import os
from typing import Callable, Optional, Tuple
import numpy as np

ERROR_MESSAGE_LENGTH = 1025


def make_int(name: str):
    return (name, POINTER(c_int))


def make_float(name: str):
    return (name, POINTER(c_float))


def make_double(name: str):
    return (name, POINTER(c_double))


def make_char(name: str):
    return (name, POINTER(c_char))


def make_arr(name: str, make_pointer: Callable):
    return make_pointer(name), make_int(name+"_Len")


def make_int_arr(name: str):
    return make_arr(name, make_int)


def make_float_arr(name: str):
    return make_arr(name, make_float)


def make_double_arr(name: str):
    return make_arr(name, make_double)


def make_char_arr(name: str):
    return make_arr(name, make_char)


class OpFM_InputType_C(Structure):  # output from fast
    _fields_ = [
        *make_float_arr("pxVel"),
        *make_float_arr("pyVel"),
        *make_float_arr("pzVel"),
        *make_float_arr("pxForce"),
        *make_float_arr("pyForce"),
        *make_float_arr("pzForce"),
        *make_float_arr("xdotForce"),
        *make_float_arr("ydotForce"),
        *make_float_arr("zdotForce"),
        *make_float_arr("pOrientation"),
        *make_float_arr("fx"),
        *make_float_arr("fy"),
        *make_float_arr("fz"),
        *make_float_arr("momentx"),
        *make_float_arr("momenty"),
        *make_float_arr("momentz"),
        *make_float_arr("forceNodesChord")
    ]


class OpFM_OutputType_C(Structure):  # input to fast
    _fields_ = [
        *make_float_arr("u"),
        *make_float_arr("v"),
        *make_float_arr("w"),
        *make_float_arr("WriteOutput"),
    ]


class SC_DX_InputType_C(Structure):
    _fields_ = [
        *make_float_arr("toSC")
    ]


class SC_DX_OutputType_C(Structure):
    _fields_ = [
        *make_float_arr("fromSc"),
        *make_float_arr("fromSCglob")
    ]


class FastLibAPI(CDLL):

    def __init__(self, library_path: str, input_file_name: str):
        super().__init__(library_path)
        self.library_path = library_path
        self.input_file_name = create_string_buffer(
            os.path.abspath(input_file_name).encode('utf-8'))

        self._initialize_routines()

        self.abort_error_level = c_int(99)

        # The inputs are meant to be from Simulink.
        # If < 8, FAST_SetExternalInputs simply returns,
        # but this behavior may change to an error
        self._num_inputs = c_int(51)
        self._inp_array = np.zeros(53, dtype=np.float64)
        self._inp_array[0] = -1.0  # Sensor type -

        self.ended = False

        self.allocated = False

    def _initialize_routines(self):
        self.FAST_AllocateTurbines.argtypes = [
            POINTER(c_int),
            POINTER(c_int),
            POINTER(c_char)
        ]
        self.FAST_AllocateTurbines.restype = c_int

        self.FAST_Sizes.argtypes = [
            POINTER(c_int),         # iTurb IN
            POINTER(c_char),        # InputFileName_c IN
            POINTER(c_int),         # AbortErrLev_c OUT
            POINTER(c_int),         # NumOuts_c OUT
            POINTER(c_double),      # dt_c OUT
            POINTER(c_double),      # tmax_c OUT
            POINTER(c_int),         # ErrStat_c OUT
            POINTER(c_char),        # ErrMsg_c OUT
            POINTER(c_char),        # ChannelNames_c OUT
            POINTER(c_double),      # TMax OPTIONAL IN
            POINTER(c_double)       # InitInpAry OPTIONAL IN
        ]
        self.FAST_Sizes.restype = c_int

        self.FAST_Start.argtypes = [
            POINTER(c_int),         # iTurb IN
            POINTER(c_int),         # NumInputs_c IN
            POINTER(c_int),         # NumOutputs_c IN
            np.ctypeslib.ndpointer(dtype=np.float64),      # InputAry IN
            np.ctypeslib.ndpointer(dtype=np.float64),      # OutputAry OUT
            POINTER(c_int),         # ErrStat_c OUT
            POINTER(c_char)         # ErrMsg_c OUT
        ]
        self.FAST_Start.restype = c_int

        self.FAST_Update.argtypes = [
            POINTER(c_int),         # iTurb IN
            POINTER(c_int),         # NumInputs_c IN
            POINTER(c_int),         # NumOutputs_c IN
            np.ctypeslib.ndpointer(dtype=np.float64),      # InputAry IN
            np.ctypeslib.ndpointer(dtype=np.float64),      # OutputAry OUT
            POINTER(c_bool),        # EndSimulationEarly OUT
            POINTER(c_int),         # ErrStat_c OUT
            POINTER(c_char)         # ErrMsg_c OUT
        ]
        self.FAST_Update.restype = c_int

        self.FAST_DeallocateTurbines.argtypes = [
            POINTER(c_int),         # ErrStat_c OUT
            POINTER(c_char),        # ErrMsg_c OUT
        ]
        self.FAST_DeallocateTurbines.restype = c_int

        self.FAST_End.argtypes = [
            POINTER(c_int),         # iTurb IN
            POINTER(c_bool),        # StopTheProgram IN
        ]
        self.FAST_End.restype = c_int

        self.FAST_OpFM_Init.argtypes = [
            POINTER(c_int),         # iTurb IN
            POINTER(c_double),      # TMax IN
            POINTER(c_char),        # InputFileName IN
            POINTER(c_int),         # TurbID IN
            POINTER(c_int),         # NumSC2CtrlGlob IN
            POINTER(c_int),         # NumSC2Ctrl IN
            POINTER(c_int),         # NumCtrl2SC IN
            POINTER(c_float),       # InitScOutputsGlob IN
            POINTER(c_float),       # InitScOutputsTurbine IN
            POINTER(c_int),         # NumActForcePtsBlade IN
            POINTER(c_int),         # NumActForcePtsTower IN
            POINTER(c_float),       # TurbPosn IN
            POINTER(c_int),         # AbortErrLev_c OUT
            POINTER(c_double),      # dt_c OUT
            POINTER(c_int),         # NumBl OUT
            POINTER(c_int),         # NumBlElem OUT
            POINTER(OpFM_InputType_C),      # OpFM_Input_from_FAST INOUT
            POINTER(OpFM_OutputType_C),     # OpFM_Output_to_FAST INOUT
            POINTER(SC_DX_InputType_C),        # SC_Input_from_FAST INOUT
            POINTER(SC_DX_OutputType_C),       # SC_Output_to_FAST INOUT
            POINTER(c_int),         # ErrStat_c OUT
            POINTER(c_char)         # ErrMsg_c(IntfStrLen) OUT
        ]

        self.FAST_OpFM_Init.restype = c_int

        self.FAST_OpFM_Solution0.argtypes = [
            POINTER(c_int),         # iTurb IN
            POINTER(c_int),         # ErrStat_c OUT
            POINTER(c_char)         # ErrMsg_c OUT
        ]

        self.FAST_OpFM_Solution0.restype = c_int

        self.FAST_OpFM_Step.argtypes = [
            POINTER(c_int),         # iTurb IN
            POINTER(c_int),         # ErrStat_c OUT
            POINTER(c_char)         # ErrMsg_c OUT
        ]

        self.FAST_OpFM_Step.restype = c_int

    def fatal_error(self, error_status: c_int) -> bool:
        return error_status.value >= self.abort_error_level.value

    def check_error(self, error_status: c_int, error_message):
        if self.fatal_error(error_status):
            raise RuntimeError(f"Error {error_status.value}: {error_message}")

    def allocate_turbines(self, n_turbines):
        error_status = c_int(0)
        error_message = create_string_buffer(ERROR_MESSAGE_LENGTH)
        self.FAST_AllocateTurbines(
            byref(c_int(n_turbines)),
            byref(error_status),
            error_message
        )
        self.check_error(error_status, error_message)
        self.allocated = True

    def sizes(self, i_turb: int, input_file_name: str, t_max: Optional[float] = None):
        num_outs = c_int(0)
        dt = c_double(0)
        t_max_read = c_double(0)
        channel_names = create_string_buffer(20 * 4000)
        error_status = c_int(0)
        error_message = create_string_buffer(ERROR_MESSAGE_LENGTH)
        self.FAST_Sizes(
            byref(c_int(i_turb)),
            create_string_buffer(input_file_name.encode("UTF-8")),
            byref(self.abort_error_level),
            byref(num_outs),
            byref(dt),
            byref(t_max_read),
            byref(error_status),
            error_message,
            channel_names,
            None,
            None
        )
        self.check_error(error_status, error_message)

        output_channel_names = [n.decode('UTF-8')
                                for n in channel_names.value.split()]

        return num_outs.value, dt.value, t_max_read.value, output_channel_names

    def start(self, i_turb: int, num_outputs: int):
        output_array = np.zeros(num_outputs, np.float64)
        error_status = c_int(0)
        error_message = create_string_buffer(ERROR_MESSAGE_LENGTH)
        self.FAST_Start(
            byref(c_int(i_turb)),
            byref(self._num_inputs),
            byref(c_int(num_outputs)),
            self._inp_array,
            output_array,
            byref(error_status),
            error_message
        )
        self.check_error(error_status, error_message)
        return output_array

    def update(self, i_turb: int, num_outputs: int):
        output_array = np.zeros(num_outputs, np.float64)
        end_early = c_bool(False)
        error_status = c_int(0)
        error_message = create_string_buffer(ERROR_MESSAGE_LENGTH)
        self.FAST_Update(
            byref(c_int(i_turb)),
            byref(self._num_inputs),
            byref(c_int(num_outputs)),
            self._inp_array,
            output_array,
            byref(end_early),
            byref(error_status),
            error_message
        )
        self.check_error(error_status, error_message)
        return end_early, output_array

    def opFM_init(self, i_turb: int,
                  t_max: float,
                  input_file_name: str,
                  turb_id: int,
                  num_sc_2_ctrl_global: int,
                  num_sc_2_ctrl: int,
                  num_ctrl_2_sc: int,
                  num_act_force_pts_bld: int,
                  num_act_force_pts_twr: int,
                  init_sc_outputs_global: np.ndarray,
                  init_sc_outputs_turbine: np.ndarray,
                  turbine_position: np.ndarray):
        dt = c_double(0)
        num_bl = c_int(0)
        num_bl_elem = c_int(0)
        opFM_input = OpFM_InputType_C()
        opFM_output = OpFM_OutputType_C()
        sc_input = SC_DX_InputType_C()
        sc_output = SC_DX_OutputType_C()
        error_status = c_int(0)
        error_message = create_string_buffer(ERROR_MESSAGE_LENGTH)
        self.FAST_OpFM_Init(
            byref(c_int(i_turb)),
            byref(c_double(t_max)),
            create_string_buffer(input_file_name.encode("UTF-8")),
            byref(c_int(len(input_file_name))),
            byref(c_int(turb_id)),
            byref(c_int(num_sc_2_ctrl_global)),
            byref(c_int(num_sc_2_ctrl)),
            byref(c_int(num_ctrl_2_sc)),
            init_sc_outputs_global,
            init_sc_outputs_turbine,
            byref(c_int(num_act_force_pts_bld)),
            byref(c_int(num_act_force_pts_twr)),
            turbine_position,
            byref(self.abort_error_level),
            byref(dt),
            byref(num_bl),
            byref(num_bl_elem),
            byref(opFM_input),
            byref(opFM_output),
            byref(sc_input),
            byref(sc_output),
            byref(error_status),
            error_message
        )
        self.check_error(error_status, error_message)
        return dt.value, num_bl.value, num_bl_elem.value, opFM_input, opFM_output

    def opFM_solution0(self, i_turb: int):
        error_status = c_int(0)
        error_message = create_string_buffer(ERROR_MESSAGE_LENGTH)
        self.FAST_OpFM_Solution0(
            byref(c_int(i_turb)),
            byref(error_status),
            error_message
        )
        self.check_error(error_status, error_message)

    def opFM_step(self, i_turb: int):
        error_status = c_int(0)
        error_message = create_string_buffer(ERROR_MESSAGE_LENGTH)
        self.FAST_OpFM_Step(
            byref(c_int(i_turb)),
            byref(error_status),
            error_message
        )
        self.check_error(error_status, error_message)

    def end(self, i_turb: int):
        self.FAST_End(
            byref(c_int(i_turb)),
            byref(c_bool(False))
        )

    def deallocate_turbines(self):
        if self.ended:
            return
        error_status = c_int(0)
        error_message = create_string_buffer(ERROR_MESSAGE_LENGTH)
        self.FAST_DeallocateTurbines(
            byref(error_status),
            error_message
        )
        self.check_error(error_status, error_message)
        self.ended = True


class OpenFAST:
    def __init__(self, fast_lib: FastLibAPI, input_file_name: str, n_turbines: int, i_turb: int, t_max: float):
        self.input_file_name = input_file_name
        self.t_max = t_max
        self.fast_lib = fast_lib
        self.n_turbines = n_turbines
        self.i_turb = i_turb
        self.allocated_turbines = 0

    def fast_init(self):
        if not self.fast_lib.allocated:
            self.fast_lib.allocate_turbines(self.n_turbines)
        self.allocated_turbines += 1

    def fast_end(self):
        self.fast_lib.end(self.i_turb)
        self.allocated_turbines -= 1
        if self.allocated_turbines == 0:
            self.fast_lib.deallocate_turbines()


class OpenFASTStandAlone(OpenFAST):
    def __init__(self, fast_lib: FastLibAPI, input_file_name: str, n_turbines: int, i_turb: int, t_max: float):
        super().__init__(fast_lib, input_file_name, n_turbines, i_turb, t_max)

    def fast_init(self):
        super().fast_init()
        self.num_outs, self.dt, self.t_max, self.output_channel_names = self.fast_lib.sizes(
            self.i_turb, self.input_file_name)

        self.output_values = np.empty(
            (self.total_time_steps, self.num_outs))

    def fast_sim(self):
        self.output_values[0] = self.fast_lib.start(self.i_turb, self.num_outs)
        for i in range(1, self.total_time_steps):
            end_early, self.output_values[i] = self.fast_lib.update(
                self.i_turb, self.num_outs)
            if end_early:
                break

    def fast_run(self):
        self.fast_init()
        self.fast_sim()
        self.fast_end()

    @property
    def total_time_steps(self):
        # From FAST_Subs FAST_Init:
        # p%n_TMax_m1  = CEILING( ( (p%TMax - t_initial) / p%DT ) ) - 1 ! We're going to go from step 0 to n_TMax (thus the -1 here)
        # Then in FAST_Prog:
        # TIME_STEP_LOOP:  DO n_t_global = Restart_step, Turbine(1)%p_FAST%n_TMax_m1
        #
        # Note that Fortran indexing starts at 1 and includes the upper bound
        # Python indexing starts at 0 and does not include the upper bound
        # The for-loop in this interface begins at 1 (there's an init step before)
        # and that's why we have the +1 below
        #
        # We assume here t_initial is always 0
        return int(np.ceil(self.t_max / self.dt) + 1)
