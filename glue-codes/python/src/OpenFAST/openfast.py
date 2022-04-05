from typing import Tuple

import numpy as np

from .openfast_library import FastLibAPI


class OpenFAST:
    def __init__(self, input_file_name: str, n_turbines: int, i_turb: int, t_max: float):
        """Base class to interact with individual turbines in OpenFAST

        Parameters
        ----------
        input_file_name : str
            Path to the input file for Fast
        n_turbines : int
            Total number of turbines
        i_turb : int
            Index of the turbine modelled by this instance
        t_max : float
            Maximum time of the simulation in s
        """
        self.fast_lib = FastLibAPI()
        self.input_file_name = input_file_name
        self.t_max = t_max
        self.n_turbines = n_turbines
        self.i_turb = i_turb

    def fast_init(self):
        """Initialize Fast. If this is the first turbine also allocates all turbines in Fast.
        """
        if self.fast_lib.allocated_turbines == 0:
            self.fast_lib.allocate_turbines(self.n_turbines)

    def fast_end(self):
        """End Fast. If this is the last turbine allocated in Fast also deallocates all turbines."""
        self.fast_lib.end(self.i_turb)
        self.fast_lib.allocated_turbines -= 1
        if self.fast_lib.allocated_turbines == 0:
            self.fast_lib.deallocate_turbines()


class OpenFASTStandAlone(OpenFAST):
    def __init__(self, input_file_name: str, n_turbines: int, i_turb: int, t_max: float):
        """Run Fast as standalone

        Parameters
        ----------
        fast_lib : openfast_library.FastLibAPI
            Instance of FastLibApi
        input_file_name : str
            Path to the input file for Fast
        n_turbines : int
            Total number of turbines
        i_turb : int
            Index of the turbine modelled by this instance
        t_max : float
            Maximum time of the simulation in s
        """
        super().__init__(input_file_name, n_turbines, i_turb, t_max)

    def fast_init(self):
        super().fast_init()
        self.num_outs, self.dt, self.t_max, self.output_channel_names = \
            self.fast_lib.sizes(self.i_turb, self.input_file_name)

        self.output_values = np.empty((self.total_time_steps, self.num_outs))

    def fast_sim(self):
        self.output_values[0] = self.fast_lib.start(self.i_turb, self.num_outs)
        for i in range(1, self.total_time_steps):
            end_early, self.output_values[i] = \
                self.fast_lib.update(self.i_turb, self.num_outs)
            if end_early:
                break

    def fast_run(self):
        """Run Fast from init to end.
        """
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
    def __init__(self,
                 input_file_name: str,
                 n_turbines: int, i_turb: int,
                 t_max: float,
                 num_actuator_force_points_blade: int,
                 num_actuator_force_points_tower: int,
                 turbine_position: Tuple[float, float, float],
                 nacelle_cd: float, rotor_area: float, density: float,
                 restart=False):
        """Run Fast with external velocities

        Parameters
        ----------
        fast_lib : openfast_library.FastLibAPI
            Instance of FastLibApi.
        input_file_name : str
            Path to the input file for Fast.
        n_turbines : int
            Total number of turbines.
        i_turb : int
            Index of the turbine modelled by this instance.
        t_max : float
            Maximum time of the simulation in s.
        num_actuator_force_points_blade : int
            Number of points per blade for which to calculate the forces.
        num_actuator_force_points_tower : int
            Number of points on the tower for which to calculate the forces.
        turbine_position : Tuple[float, float, float]
            Position of the turbine in m.
        nacelle_cd : float
            Drag coefficient of the nacelle, used to calculate the force exerted by the nacelle.
        rotor_area : float
            Total area of the rotor in m^2.
        density : float
            Density of air in kg/m^3.
        restart : bool, optional
            If True restarts the simulation from checkpoint, not yet implemented, by default False.

        Raises
        ------
        ValueError
            _description_
        """
        super().__init__(input_file_name, n_turbines, i_turb, t_max)
        if i_turb >= n_turbines:
            raise ValueError(
                "n_turbines has to be bigger than i_turb (i_turb runs from 0 to n_turbines-1)")
        self.i_turb = i_turb
        self.num_controller_inputs_from_supercontroller = 0
        self.num_controller_outputs_to_supercontroller = 0
        self.num_actuator_force_points_blade = num_actuator_force_points_blade
        self.num_actuator_force_points_tower = num_actuator_force_points_tower
        self.turbine_position = np.array(list(turbine_position), np.float32)
        self.nacelle_cd = nacelle_cd
        self.rotor_area = rotor_area
        self.density = density
        # correction from OpenFAST.cpp but simplified??
        self.nacelle_correction = (8/7)**2

        if restart:
            self.fast_restart()
        else:
            self.fast_init()

        self.fast_solution0()

    def fast_restart(self):
        raise NotImplementedError("Restart not yet implemented")

    def fast_init(self):
        super().fast_init()
        sc_out_glob = np.zeros(0, np.float32)
        sc_out_turbine = np.zeros(0, np.float32)
        self.dt, self.n_blades, self.n_blade_elements, self.opFM_input, self.opFM_output = \
            self.fast_lib.opFM_init(self.i_turb,
                                    self.t_max,
                                    self.input_file_name,
                                    self.i_turb,
                                    0, 0, 0,
                                    self.num_actuator_force_points_blade,
                                    self.num_actuator_force_points_tower,
                                    sc_out_glob,
                                    sc_out_turbine,
                                    self.turbine_position)

        self.n_vel_points = self.opFM_output.u.shape[0]

        self.n_vel_points_blade = self.n_blade_elements
        self.n_vel_points_tower = self.n_vel_points - \
            self.n_vel_points_blade*self.n_blades - 1

        self.n_force_points_blade = self.num_actuator_force_points_blade
        self.n_force_points_tower = self.num_actuator_force_points_tower

    def fast_solution0(self):
        """Calculate initial solution."""
        self.fast_lib.opFM_solution0(self.i_turb)

    def fast_step(self):
        """Advance turbine one timestep."""
        self.fast_lib.opFM_step(self.i_turb)

    def get_vel_coordinates(self):
        """Get the coordinates of the velocity points.

        Returns
        -------
        pxVel: np.ndarray    
            X-coordinates in m.
        pyVel: np.ndarray    
            Y-coordinates in m.
        pzVel: np.ndarray    
            Z-coordinates in m.
        """
        return (
            self.opFM_input.pxVel,
            self.opFM_input.pyVel,
            self.opFM_input.pzVel
        )

    def get_force_coordinates(self):
        """Get the coordinates of the force points.

        Returns
        -------
        pxForce: np.ndarray    
            X-coordinates in m.
        pyForce: np.ndarray    
            Y-coordinates in m.
        pzForce: np.ndarray    
            Z-coordinates in m.
        """
        return (
            self.opFM_input.pxForce,
            self.opFM_input.pyForce,
            self.opFM_input.pzForce
        )

    def get_velocities(self):
        """Get the velocities at the velocity points.

        Returns
        -------
        u: np.ndarray    
            U-component in m/s.
        v: np.ndarray    
            V-component in m/s.
        w: np.ndarray    
            W-component in m/s.
        """
        return (
            self.opFM_input.u,
            self.opFM_input.v,
            self.opFM_input.w
        )

    def get_relative_velocity_at_force_nodes(self, u_f: np.ndarray, v_f: np.ndarray, w_f: np.ndarray):
        """Get the velocities at the force points relative to the turbine blade.

        Returns
        -------
        u: np.ndarray    
            U-component in m/s.
        v: np.ndarray    
            V-component in m/s.
        w: np.ndarray    
            W-component in m/s.
        """
        return (
            u_f - self.opFM_input.xdotForce,
            v_f - self.opFM_input.ydotForce,
            w_f - self.opFM_input.zdotForce
        )

    def get_forces(self):
        """Get the forces at the force points.

        Returns
        -------
        fx: np.ndarray    
            X-component in N.
        fy: np.ndarray    
            Y-component in N.
        fz: np.ndarray    
            Z-component in N.
        """
        return (
            self.opFM_input.fx,
            self.opFM_input.fy,
            self.opFM_input.fz
        )

    def set_vel_coordinates(self, x: np.ndarray, y: np.ndarray, z: np.ndarray):
        """Set the coordinates of the velocity points.

        Parameters
        ----------
        x : np.ndarray
            New x-coordinate in m.
        y : np.ndarray
            New y-coordinate in m.
        z : np.ndarray
            New z-coordinate in m.
        """
        np.copyto(self.opFM_output.pxVel, x.astype(np.float64))
        np.copyto(self.opFM_output.pyVel, y.astype(np.float64))
        np.copyto(self.opFM_output.pzVel, z.astype(np.float64))

    def set_force_coordinates(self, x: np.ndarray, y: np.ndarray, z: np.ndarray):
        """Set the coordinates of the velocity points.

        Parameters
        ----------
        x : np.ndarray
            New x-coordinate in m.
        y : np.ndarray
            New y-coordinate in m.
        z : np.ndarray
            New z-coordinate in m.
        """
        np.copyto(self.opFM_output.pxForce, x.astype(np.float64))
        np.copyto(self.opFM_output.pyForce, y.astype(np.float64))
        np.copyto(self.opFM_output.pzForce, z.astype(np.float64))

    def set_velocity(self,
                     u_nacelle: float, v_nacelle: float, w_nacelle: float,
                     u_blades: np.ndarray, v_blades: np.ndarray, w_blades: np.ndarray,
                     u_tower: np.ndarray, v_tower: np.ndarray, w_tower: np.ndarray):
        """Set the velocity at the forcing points. Automatically interpolates to the velocity points.

        Parameters
        ----------
        u_nacelle : float
            U-component of the velocity at the nacelle in m/s.
        v_nacelle : float
            V-component of the velocity at the nacelle in m/s.
        w_nacelle : float
            W-component of the velocity at the nacelle in m/s.
        u_blades : np.ndarray
            U-component of the velocity at the blades in m/s.
        v_blades : np.ndarray
            V-component of the velocity at the blades in m/s.
        w_blades : np.ndarray
            W-component of the velocity at the blades in m/s.
        u_tower : np.ndarray
            U-component of the velocity at the tower in m/s.
        v_tower : np.ndarray
            V-component of the velocity at the tower in m/s.
        w_tower : np.ndarray
            W-component of the velocity at the tower in m/s.
        """
        u = np.concatenate((np.array([u_nacelle, ]), u_blades, u_tower))
        v = np.concatenate((np.array([v_nacelle, ]), v_blades, v_tower))
        w = np.concatenate((np.array([w_nacelle, ]), w_blades, w_tower))
        self.interpolate_vel_from_force_nodes_to_vel_nodes(u, v, w)

    def set_velocities(self, u: np.ndarray, v: np.ndarray, w: np.ndarray):
        """Set velocities at the velocity points

        Parameters
        ----------
        u : np.ndarray
            U-component of the velocity in m/s.
        v : np.ndarray
            V-component of the velocity at the nacelle in m/s.
        w : np.ndarray
            w-component of the velocity at the nacelle in m/s.
        """
        np.copyto(self.opFM_output.u, u.astype(np.float32))
        np.copyto(self.opFM_output.v, v.astype(np.float32))
        np.copyto(self.opFM_output.w, w.astype(np.float32))

    def get_relative_distances(self, pos_x: np.ndarray, pos_y: np.ndarray, pos_z: np.ndarray, n_nodes_per_blade: int):
        """Calculate the relative distances of the blade and tower nodes.
        Assumes that the first node is at the center of the rotor
        and the first node of the tower nodes is at the bottom.

        Parameters
        ----------
        pos_x : np.ndarray
            X-component of the absolute position of the blade and tower nodes in m.
        pos_y : np.ndarray
            Y-component of the absolute position of the blade and tower nodes in m.
        pos_z : np.ndarray
            Z-component of the absolute position of the blade and tower nodes in m.
        n_nodes_per_blade : int
            Number of nodes per blade.

        Returns
        -------
        dist_blades : np.ndarray
            Distances of the blade nodes to the center of the rotor in m.
        dist_tower : np.ndarray
            Distances of the tower nodes to the bottom node of the tower in m.
        """
        n_blade_nodes = self.n_blades*n_nodes_per_blade

        rel_pos_blade_x = pos_x[1:n_blade_nodes+1] - pos_x[0]
        rel_pos_blade_y = pos_y[1:n_blade_nodes+1] - pos_y[0]
        rel_pos_blade_z = pos_z[1:n_blade_nodes+1] - pos_z[0]
        rel_pos_tower_x = pos_x[n_blade_nodes+1:] - pos_x[n_blade_nodes+1]
        rel_pos_tower_y = pos_y[n_blade_nodes+1:] - pos_y[n_blade_nodes+1]
        rel_pos_tower_z = pos_z[n_blade_nodes+1:] - pos_z[n_blade_nodes+1]

        dist_blades = np.sqrt(rel_pos_blade_x*rel_pos_blade_x +
                              rel_pos_blade_y*rel_pos_blade_y + rel_pos_blade_z*rel_pos_blade_z)
        dist_tower = np.sqrt(rel_pos_tower_x*rel_pos_tower_x +
                             rel_pos_tower_y*rel_pos_tower_y + rel_pos_tower_z*rel_pos_tower_z)

        return dist_blades, dist_tower

    def interpolate_vel_from_force_nodes_to_vel_nodes(self, u_f: np.ndarray, v_f: np.ndarray, w_f: np.ndarray):
        """Interpolates the given velocities from the force nodes to the velocity nodes.

        Parameters
        ----------
        u_f : np.ndarray
            u-component of the velocity at the force-nodes in m/s.
        v_f : np.ndarray
            u-component of the velocity at the force-nodes in m/s.
        w_f : np.ndarray
            u-component of the velocity at the force-nodes in m/s.
        """
        u = np.zeros(self.n_vel_points)
        v = np.zeros(self.n_vel_points)
        w = np.zeros(self.n_vel_points)

        # Nacelle
        u[0] = u_f[0]
        v[0] = v_f[0]
        w[0] = w_f[0]

        pos_x, pos_y, pos_z = self.get_force_coordinates()
        dist_forces_blades, dist_forces_tower = self.get_relative_distances(
            pos_x, pos_y, pos_z, self.n_force_points_blade)

        pos_x, pos_y, pos_z = self.get_vel_coordinates()
        dist_vels_blades, dist_vels_tower = self.get_relative_distances(
            pos_x, pos_y, pos_z, self.n_vel_points_blade)

        end_force = 0
        end_vel = 0
        for _ in range(self.n_blades):
            start_force = end_force
            start_vel = end_vel

            end_force = start_force + self.n_force_points_blade
            end_vel = start_vel + self.n_vel_points_blade

            u[1+start_vel:1+end_vel] = np.interp(dist_vels_blades[start_vel:end_vel],
                                                 dist_forces_blades[start_force:end_force],
                                                 u_f[1+start_force:1+end_force])
            v[1+start_vel:1+end_vel] = np.interp(dist_vels_blades[start_vel:end_vel],
                                                 dist_forces_blades[start_force:end_force],
                                                 v_f[1+start_force:1+end_force])
            w[1+start_vel:1+end_vel] = np.interp(dist_vels_blades[start_vel:end_vel],
                                                 dist_forces_blades[start_force:end_force],
                                                 w_f[1+start_force:1+end_force])

        u[1+end_vel:] = np.interp(dist_vels_tower,
                                  dist_forces_tower, u_f[1+end_force:])
        v[1+end_vel:] = np.interp(dist_vels_tower,
                                  dist_forces_tower, v_f[1+end_force:])
        w[1+end_vel:] = np.interp(dist_vels_tower,
                                  dist_forces_tower, w_f[1+end_force:])

        self.set_velocities(u, v, w)

    def get_nacelle_force(self, u: float, v: float, w: float):
        """Calculates the drag force by the nacelle.

        Parameters
        ----------
        u : float
            u-component of the velocity at the nacelle in m/s.
        v : float
            v-component of the velocity at the nacelle in m/s.
        w : float
            w-component of the velocity at the nacelle in m/s.

        Returns
        -------
        fx : float
            x-component of the force in N.        
        fy : float
            y-component of the force in N.        
        fz : float
            z-component of the force in N.
        """
        v_mag = u*u + v*v + w*w
        c = 0.5*self.density*self.nacelle_cd * \
            self.rotor_area*v_mag*self.nacelle_correction
        return c*u, c*v, c*w

    def get_blade_forces(self):
        """Get the forces at the blades.

        Returns
        -------
        fx : np.ndarray
            x-component of the force in N.        
        fy : np.ndarray
            y-component of the force in N.        
        fz : np.ndarray
            z-component of the force in N.
        """
        fx, fy, fz = self.get_forces()
        n_nodes = self.n_blades*self.n_blade_elements
        return (
            fx[1:n_nodes+1], fy[1:n_nodes+1], fz[1:n_nodes+1]
        )

    def get_tower_forces(self):
        """Get the forces at the tower.

        Returns
        -------
        fx : np.ndarray
            x-component of the force in N.        
        fy : np.ndarray
            y-component of the force in N.        
        fz : np.ndarray
            z-component of the force in N.
        """
        fx, fy, fz = self.get_forces()
        return (
            fx[self.n_vel_points_blades+1:],
            fy[self.n_vel_points_blades+1:],
            fz[self.n_vel_points_blades+1:]
        )

    def get_hub_position(self):
        """Get the hub position.

        Returns
        -------
        x : float
            x-component of the position in m.        
        y : float
            y-component of the position in m.        
        z :float
            z-component of the position in m.
        """
        return (
            self.opFM_input.pxVel[0] + self.turbine_position[0],
            self.opFM_input.pyVel[0] + self.turbine_position[1],
            self.opFM_input.pzVel[0] + self.turbine_position[1]
        )
