import os
from ctypes import (CDLL, POINTER, byref, c_bool, c_char, c_double,
                    c_int, create_string_buffer)
from typing import Optional

import numpy as np
import openfast_types as of_types
ERROR_MESSAGE_LENGTH = 1025


class FastLibAPI(CDLL):

    def __init__(self, library_path: str):
        super().__init__(library_path)
        self.library_path = library_path

        self._initialize_routines()

        self.abort_error_level = c_int(99)

        # The inputs are meant to be from Simulink.
        # If < 8, FAST_SetExternalInputs simply returns,
        # but this behavior may change to an error
        self._num_inputs = c_int(51)
        self._inp_array = np.zeros(53, dtype=np.float64)
        self._inp_array[0] = -1.0  # Sensor type -

        self.ended = False
        self.allocated_turbines = 0

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
            # InitScOutputsGlob IN
            np.ctypeslib.ndpointer(dtype=np.float32),
            # InitScOutputsTurbine IN
            np.ctypeslib.ndpointer(dtype=np.float32),
            POINTER(c_int),         # NumActForcePtsBlade IN
            POINTER(c_int),         # NumActForcePtsTower IN
            np.ctypeslib.ndpointer(dtype=np.float32),       # TurbPosn IN
            POINTER(c_int),         # AbortErrLev_c OUT
            POINTER(c_double),      # dt_c OUT
            POINTER(c_int),         # NumBl OUT
            POINTER(c_int),         # NumBlElem OUT
            # OpFM_Input_from_FAST INOUT
            POINTER(of_types.OpFM_InputType_C),
            # OpFM_Output_to_FAST INOUT
            POINTER(of_types.OpFM_OutputType_C),
            # SC_Input_from_FAST INOUT
            POINTER(of_types.SC_DX_InputType_C),
            # SC_Output_to_FAST INOUT
            POINTER(of_types.SC_DX_OutputType_C),
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

    def allocate_turbines(self, n_turbines: int):
        error_status = c_int(0)
        error_message = create_string_buffer(ERROR_MESSAGE_LENGTH)
        self.FAST_AllocateTurbines(
            byref(c_int(n_turbines)),
            byref(error_status),
            error_message
        )
        self.check_error(error_status, error_message)
        self.allocated_turbines = n_turbines

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
            t_max if t_max is None else byref(c_double(t_max)),
            None,
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
        opFM_input = of_types.OpFM_InputType()
        opFM_output = of_types.OpFM_OutputType()
        sc_input = of_types.SC_DX_InputType()
        sc_output = of_types.SC_DX_OutputType()
        error_status = c_int(0)
        error_message = create_string_buffer(ERROR_MESSAGE_LENGTH)
        self.FAST_OpFM_Init(
            byref(c_int(i_turb)),
            byref(c_double(t_max)),
            create_string_buffer(input_file_name.encode("UTF-8")),
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
            byref(opFM_input.c_struct),
            byref(opFM_output.c_struct),
            byref(sc_input.c_struct),
            byref(sc_output.c_struct),
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
