import math

def Type205(self, state, T_a, RH,lights, **kwargs):
    P_LED = kwargs["P_el"] * lights / kwargs["area"] # Total LED Power [W/m^2]
    A_gr = kwargs["area"]  # Floor area [m^2]
    LAI = kwargs["LAI"]  # LeafAreaIndex [m^2_leaves/m^2_cultivated area]
    PPE = kwargs["PPE"]

    if LAI > 2.45:
        CAC = 0.95
    elif LAI <= 2.45:
        CAC = 0.3874 * LAI# Coverage of the floor of the cultivated area [-]

    Afv = kwargs["CA"] / kwargs["area"]  # Cultivated fraction [-]
    rho_v = kwargs["rho_v"]   # Lettuce relfectivity [-]
    LED_eff = kwargs["f_v"]  # LED efficiency [-]

    #  #Outputs
    # q_sens #Sensible gain to air from vegetation [kJ/hr]
    # q_lat #Latent gain to air from vegetation [kg/hr]
    # T_s #Vegetation temperature [degC]
    # q_loss #Light energy that got reflected [W]

    # Parameters
    rho_a = 1.2  # Air density [kg/m^3]
    c_p = 1.006  # Air specific heat capacity [kJ/kgK]
    lmbda = 2489  # Latent heat vapor of water [kJ/kg]
    gamma = 66.5  # Psychometric constant [Pa/K]

    # -----------------------------------------------------------------------------------------------------------------------
    # Variables
    # -----------------------------------------------------------------------------------------------------------------------

    # PPFD #nu_mol/m^2*s
    # I_light #LED power convert to short-wave radiation [W/m^2]
    # Rnet #Short-wave radiation absorbed by vegetation [W/m^2]
    # r_a #Aerodynamic stomatal resistance [s/m]
    # r_s #Surface stomatal resistance [s/m]
    # e_star #Saturated vapor pressure [kPa]
    # Xa_star #Saturated vapor concentration [g/m3]
    # e #Air vapor pressure [kPa]
    # Xa #Air vapor concentration [g/m3]
    # delta #Slope of the relationship between the saturation vapour pressure and air temperature [kPa/degC]
    # epsilon #Vapour concentration
    # Xs #Vapour concentration at the canopy level [g/m3]
    # q_sens_watt #Sensible gain to air from vegetation [W/m^2]
    # q_lat_watt #Latent gain to air from vegetation [W/m^2]

    # -----------------------------------------------------------------------------------------------------------------------
    # Check the Inputs for Problems
    # -----------------------------------------------------------------------------------------------------------------------

    if Afv < 0 or Afv > 10:
        self.api.runtime.issue_severe(state, "The Cultivated fraction area of floor must be between 0 and 10 (1000%)")
        raise

    if rho_a < 0:
        self.api.runtime.issue_severe(state, "The air density must greater than 0")
        raise

    if c_p < 0:
        self.api.runtime.issue_severe(state, "The air specific heat capacity must greater than 0")
        raise

    if lmbda < 0:
        self.api.runtime.issue_severe(state, "The latent heat vapor of water must be greater than 0")
        raise

    if rho_v < 0 or rho_v > 1:
        self.api.runtime.issue_severe(state, "The lettuce leaf light reflectivity must be between 0 and 1")
        raise

    if LED_eff < 0 or LED_eff > 1:
        self.api.runtime.issue_severe(state, "The LED efficiency must be between 0 and 1")
        raise

    if T_a < -273.15:
        self.api.runtime.issue_severe(state, "The input temperature is less than 0 K")
        raise

    if P_LED < 0:
        self.api.runtime.issue_severe(state, "The total power input has to be greater than 0")
        raise

    if RH > 100 or RH < 0:
        self.api.runtime.issue_severe(state, "The relative humidity must be between 0 and 100")
        raise

    if A_gr < 0:
        self.api.runtime.issue_severe(state, "The agriculture space area have to be greater than 0")
        raise

    # -----------------------------------------------------------------------------------------------------------------------
    #    *** PERFORM ALL THE CALCULATION HERE FOR THIS MODEL. ***
    # -----------------------------------------------------------------------------------------------------------------------

    PPFD = P_LED * PPE
    I_light = LED_eff * P_LED
    Rnet = (1 - rho_v) * I_light * CAC
    q_loss = (I_light - Rnet) * (A_gr * Afv) # [W]

    r_a = 100
    r_s = 60 * (1500 + PPFD) / (200 + PPFD)

    e_star = 0.611 * math.exp(17.4 * T_a / (T_a + 239))
    Xa_star = rho_a * c_p / lmbda / (gamma / 1000) * e_star * 1000  # [g/m^3]
    e = RH / 100 * e_star
    Xa = 7.4 * e  # [g/m^3]
    delta = 0.04145 * pow(e, (0.06088 * T_a))
    epsilon = delta / (gamma / 1000)

    T_s = T_a - 2
    res = 1  # To get in the loop (initial value)
    i=0

    def h(T_s):
        Xs = Xa_star + rho_a * 1000 * c_p / lmbda * epsilon * (T_s - T_a)
        q_lat_watt = LAI * lmbda * (Xs - Xa) / (r_s + r_a)  # W/m^2
        q_sens_watt = (LAI * rho_a * c_p * 1000) / r_a * (T_s - T_a)
        return -Rnet + q_lat_watt + q_sens_watt

    try:
        while (abs(res) > 0.00001) and i<100:
            Xs = Xa_star + rho_a * 1000 * c_p / lmbda * epsilon * (T_s - T_a)
            q_lat_watt = LAI * lmbda * (Xs - Xa) / (r_s + r_a)  # W/m^2
            q_sens_watt = Rnet - q_lat_watt

            T_s_star = q_sens_watt * r_a / (LAI * rho_a * c_p  * 1000) +T_a

            res = T_s_star - T_s
            T_s = T_s_star

            i+=1

        # if i >= 100:
        #     T_s = opt.brenth(h, 0, 50)
        #     Xs = Xa_star + rho_a * 1000 * c_p / lmbda * epsilon * (T_s - T_a)
        #     q_lat_watt = LAI * lmbda * (Xs - Xa) / (r_s + r_a)  # W/m^2
        #     q_sens_watt = (LAI * rho_a * c_p * 1000) / r_a * (T_s - T_a)

    except Exception:
        self.api.runtime.issue_severe(state, "CEA Solver Failed")
        raise

    self.api.exchange.set_global_value(state,self.veg_temp_handle,T_s)

    # -----------------------------------------------------------------------------------------------------------------------
    # Convert outputs units
    # -----------------------------------------------------------------------------------------------------------------------

    q_sens = q_sens_watt * (A_gr * Afv) #[W]
    q_lat = q_lat_watt * (A_gr * Afv) #[W]

    return q_sens,q_lat,q_loss
