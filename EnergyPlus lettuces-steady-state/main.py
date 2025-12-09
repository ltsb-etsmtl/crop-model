import sys, pickle
from Type205 import *

sys.path.append("C:\EnergyPlusV22-1-0")
from pyenergyplus.plugin import EnergyPlusPlugin

class type205(EnergyPlusPlugin):

    Type205 = Type205
    def __init__(self):
        super().__init__()

        # Read GUI generated parameters
        with open(r'..\Type205_params.pkl', 'rb') as handle:
            self.params = pickle.load(handle)

        # Flags
        self.handles_set = False

        # Input handles
        self.handles_zone_temperature = None
        self.handles_zone_humidity = None
        self.handles_zone_area = None

        ### Outputs back to E+ ###
        self.handle_zone_sensible_rate = None
        self.handle_zone_latent_rate = None

        # ### Reporting ###
        # self.handle_zone_vegetation_temperature = None
        # self.handle_zone_reflected_radiation = None

    def on_begin_new_environment(self, state) -> int:
        """
        Sets handles for vegetation temp output and Zone Area
        """

        self.handles_zone_area = self.api.exchange.get_internal_variable_handle(state, "Zone Floor Area", self.params["zone_name"])
        self.veg_temp_handle = self.api.exchange.get_global_handle(state, "VegTemp")
        self.current_zone_area = self.api.exchange.get_internal_variable_value(state, self.handles_zone_area)

        return 0
    def on_inside_hvac_system_iteration_loop(self, state) -> int:
        """
        Sets handles for inputs and outputs
        Calls the Type205 with specified LAI and CAC
        Returns qs,ql,qr to EnergyPlus simulation
        """
        # Retrieve handles from E+
        if not self.handles_set:

            self.handles_lights = self.api.exchange.get_variable_handle(state,"Schedule Value", "Lights")

            self.handles_zone_temperature = self.api.exchange.get_variable_handle(state, "Zone Air Temperature", self.params["zone_name"])
            self.handles_zone_humidity = self.api.exchange.get_variable_handle(state, "Zone Air Relative Humidity", self.params["zone_name"])

            self.handle_zone_sensible_rate = self.api.exchange.get_actuator_handle(state,"OtherEquipment","Power Level","OTHEQ_SENSIBLE")
            self.handle_zone_latent_rate = self.api.exchange.get_actuator_handle(state,"OtherEquipment","Power Level","OTHEQ_LATENT")
            self.handle_zone_rad_rate = self.api.exchange.get_actuator_handle(state,"OtherEquipment","Power Level","OTHEQ_RAD")

            self.handles_set = True

        # Read Inputs from E+
        current_zone_air_temperature = self.api.exchange.get_variable_value(state, self.handles_zone_temperature)
        current_zone_air_humidity = self.api.exchange.get_variable_value(state, self.handles_zone_humidity)
        lights = self.api.exchange.get_variable_value(state, self.handles_lights)

        # Run modified type 205
        qs, ql, qr = self.Type205(state, current_zone_air_temperature, current_zone_air_humidity,lights, area = self.current_zone_area, **self.params)

        # Send output to E+
        self.api.exchange.set_actuator_value(state, self.handle_zone_sensible_rate, qs)
        self.api.exchange.set_actuator_value(state, self.handle_zone_latent_rate, ql)
        self.api.exchange.set_actuator_value(state, self.handle_zone_rad_rate, qr)

        return 0

type205()