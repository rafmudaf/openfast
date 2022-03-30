from ctypes import (
    CDLL,
    POINTER,
    create_string_buffer,
    byref,
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

class OpenFastCoupled(OpenFAST):
    def __init__(self, fast_lib: FastLibAPI, 
                 input_file_name: str, 
                 n_turbines: int, i_turb: int, 
                 t_max: float,
                 num_actuator_force_points_blade: int,
                 num_actuator_force_points_tower: int,
                 turbine_position: Tuple[float, float, float],
                 nacelle_cd: float, rotor_area: float, density: float, 
                 restart=False):
        super().__init__(fast_lib, input_file_name, n_turbines, i_turb, t_max)
        self.i_turb = i_turb
        self.num_controller_inputs_from_supercontroller = 0
        self.num_controller_outputs_to_supercontroller = 0
        self.num_actuator_force_points_blade = num_actuator_force_points_blade
        self.num_actuator_force_points_tower = num_actuator_force_points_tower
        self.turbine_position = turbine_position
        self.nacelle_cd = nacelle_cd
        self.rotor_area = rotor_area,
        self.density = density
        # correction from OpenFAST.cpp but simplified??
        self.nacelle_correction = (8/7)**2

        # NOTE: first velocity point is velocity at nacelle, then blades, then tower

        self.u = np.zeros(0)
        self.v = np.zeros(0)
        self.w = np.zeros(0)

        self.fx = np.zeros(0)
        self.fy = np.zeros(0)
        self.fz = np.zeros(0)

        if restart:
            self.fast_restart()
        else:
            self.fast_init()

    def fast_restart(self):
        raise NotImplementedError("Restart not yet implemented")

    def fast_init(self):
        super().fast_init()
        self.dt, self.n_blades, self.n_blade_elements, self.opFM_input, self.opFM_output = self.fast_lib.opFM_init(self.i_turb,
                                                                                                                   self.t_max,
                                                                                                                   self.input_file_name,
                                                                                                                   self.i_turb,
                                                                                                                   self.num_controller_inputs_from_supercontroller,
                                                                                                                   self.num_controller_outputs_to_supercontroller,
                                                                                                                   self.num_actuator_force_points_blade,
                                                                                                                   self.num_actuator_force_points_tower,
                                                                                                                   self.turbine_position)
        self.n_nodes = self.opFM_output.u_Len
        self.n_vel_points_blades = self.n_blades*self.n_blade_elements
        self.n_vel_points_tower = self.n_nodes - self.n_vel_points_blades - 1

        self.n_force_points_blade = self.n_force_points_blade*self.n_blades
        self.n_force_points_tower = self.num_actuator_force_points_tower

    def get_vel_coordinates(self):
        return (
            np.ctypeslib.as_array(self.opFM_input.pxVel),
            np.ctypeslib.as_array(self.opFM_input.pyVel),
            np.ctypeslib.as_array(self.opFM_input.pzVel)
        )

    def get_force_coordinates(self):
        return (
            np.ctypeslib.as_array(self.opFM_input.pxForce),
            np.ctypeslib.as_array(self.opFM_input.pyForce),
            np.ctypeslib.as_array(self.opFM_input.pzForce)
        )

    def get_velocities(self):
        return (
            np.ctypeslib.as_array(self.opFM_input.u),
            np.ctypeslib.as_array(self.opFM_input.v),
            np.ctypeslib.as_array(self.opFM_input.w)
        )

    def get_forces(self):
        return (
            np.ctypeslib.as_array(self.opFM_input.fx),
            np.ctypeslib.as_array(self.opFM_input.fy),
            np.ctypeslib.as_array(self.opFM_input.fz)
        )

    def set_vel_coordinates(self, x: np.ndarray, y: np.ndarray, z: np.ndarray):
        self.opFM_input.pxVel[:] = np.asfortranarray(x, dtype=np.float64)
        self.opFM_input.pxVel[:] = np.asfortranarray(y, dtype=np.float64)
        self.opFM_input.pxVel[:] = np.asfortranarray(z, dtype=np.float64)

    def set_force_coordinates(self, x: np.ndarray, y: np.ndarray, z: np.ndarray):
        self.opFM_input.pxForce[:] = np.asfortranarray(x, dtype=np.float64)
        self.opFM_input.pxForce[:] = np.asfortranarray(y, dtype=np.float64)
        self.opFM_input.pxForce[:] = np.asfortranarray(z, dtype=np.float64)

    def set_velocity(self, u_nacelle: float, v_nacelle: float, w_nacelle: float, 
    u_blades: np.ndarray, v_blades: np.ndarray, w_blades: np.ndarray, 
    u_tower: np.ndarray, v_tower: np.ndarray, w_tower: np.ndarray):
        u = np.concatenate(np.array([u_nacelle]), u_blades, u_tower)
        v = np.concatenate(np.array([v_nacelle]), v_blades, v_tower)
        w = np.concatenate(np.array([w_nacelle]), w_blades, w_tower)
        self.interpolate_vel_from_force_nodes_to_vel_nodes(u, v, w)

    def _set_velocities(self, u: np.ndarray, v: np.ndarray, w: np.ndarray):
        self.opFM_output.u[:] = np.asfortranarray(u, dtype=np.float64)
        self.opFM_output.v[:] = np.asfortranarray(v, dtype=np.float64)
        self.opFM_output.w[:] = np.asfortranarray(w, dtype=np.float64)

    def interpolate_vel_from_force_nodes_to_vel_nodes(self, u_f: np.ndarray, v_f: np.ndarray, w_f: np.ndarray):
        u = np.zeros(self.n_nodes)
        v = np.zeros(self.n_nodes)
        w = np.zeros(self.n_nodes)

        # Nacelle
        u[0] = u_f[0]
        v[0] = v_f[0]
        w[0] = w_f[0]

        n_blade_nodes = self.n_blades*self.n_force_points_blade

        pos_x, pos_y, pos_z = self.get_force_coordinates()
        rel_pos_blade_x = pos_x[1:n_blade_nodes+1] - pos_x[0]
        rel_pos_blade_y = pos_y[1:n_blade_nodes+1] - pos_y[0]
        rel_pos_blade_z = pos_z[1:n_blade_nodes+1] - pos_z[0]
        rel_pos_tower_x = pos_x[n_blade_nodes+1:] - pos_x[n_blade_nodes+1]
        rel_pos_tower_y = pos_y[n_blade_nodes+1:] - pos_y[n_blade_nodes+1]
        rel_pos_tower_z = pos_z[n_blade_nodes+1:] - pos_z[n_blade_nodes+1]

        dist_forces_blades = np.sqrt(rel_pos_blade_x*rel_pos_blade_x +
                                     rel_pos_blade_y*rel_pos_blade_y + rel_pos_blade_z*rel_pos_blade_z)
        dist_forces_tower = np.sqrt(rel_pos_tower_x*rel_pos_tower_x +
                                    rel_pos_tower_y*rel_pos_tower_y + rel_pos_tower_z*rel_pos_tower_z)

        pos_x, pos_y, pos_z = self.get_vel_coordinates()
        rel_pos_blade_x = pos_x[1:n_blade_nodes+1] - pos_x[0]
        rel_pos_blade_y = pos_y[1:n_blade_nodes+1] - pos_y[0]
        rel_pos_blade_z = pos_z[1:n_blade_nodes+1] - pos_z[0]
        rel_pos_tower_x = pos_x[n_blade_nodes+1:] - pos_x[n_blade_nodes+1]
        rel_pos_tower_y = pos_y[n_blade_nodes+1:] - pos_y[n_blade_nodes+1]
        rel_pos_tower_z = pos_z[n_blade_nodes+1:] - pos_z[n_blade_nodes+1]

        dist_vels_blades = np.sqrt(rel_pos_blade_x*rel_pos_blade_x +
                                   rel_pos_blade_y*rel_pos_blade_y + rel_pos_blade_z*rel_pos_blade_z)
        dist_vels_tower = np.sqrt(rel_pos_tower_x*rel_pos_tower_x +
                                  rel_pos_tower_y*rel_pos_tower_y + rel_pos_tower_z*rel_pos_tower_z)

        u[1:n_blade_nodes + 1] = np.interp(dist_vels_blades, dist_forces_blades, u_f)
        v[1:n_blade_nodes + 1] = np.interp(dist_vels_blades, dist_forces_blades, v_f)
        w[1:n_blade_nodes + 1] = np.interp(dist_vels_blades, dist_forces_blades, w_f)

        u[n_blade_nodes+1] = np.interp(dist_vels_tower, dist_forces_tower, u_f)
        v[n_blade_nodes+1] = np.interp(dist_vels_tower, dist_forces_tower, v_f)
        w[n_blade_nodes+1] = np.interp(dist_vels_tower, dist_forces_tower, w_f)

        self._set_velocities(u, v, w)

    def get_nacelle_force(self, u, v, w):
        v_mag = u*u + v*v + w*w
        c = 0.5*self.density*self.nacelle_cd * \
            self.rotor_area*v_mag*self.nacelle_correction
        return c*u, c*v, c*w

    def get_blade_forces(self):
        fx, fy, fz = self.get_forces()
        n_nodes = self.n_blades*self.n_blade_elements
        return (
            fx[1:n_nodes+1], fy[1:n_nodes+1], fz[1:n_nodes+1]
        )

    def get_tower_forces(self):
        fx, fy, fz = self.get_forces()
        return (fx[self.n_vel_points_blades+1:],
                fy[self.n_vel_points_blades+1:],
                fz[self.n_vel_points_blades+1:]
                )

    def solution0(self):
        self.fast_lib.opFM_solution0(self.i_turb)

    def step(self):
        self.fast_lib.opFM_step(self.i_turb)

    def get_hub_position(self):
        return (self.opFM_input.pxVel[0] + self.turbine_position[0],
                self.opFM_input.pyVel[0] + self.turbine_position[1],
                self.opFM_input.pzVel[0] + self.turbine_position[1])
