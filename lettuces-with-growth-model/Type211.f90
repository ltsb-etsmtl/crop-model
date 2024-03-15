      Subroutine Type211
      Use TrnsysConstants
      Use TrnsysFunctions

!DEC$Attributes DLLexport :: Type211
!-----------------------------------------------------------------------------------------------------------------------
!Type 211 was developped at Ecole de technologie superieure (ETS Montreal).
! Written by Marie-Helene Talbot and Timtohé Lalonde based on Graamans et al. (2017)

! Type 211 is based on Type 204 and includes:
! 1- Growth model adapted from Van Henten (1994). See Talbot & Monfet (2023) for details and calibrated parameter's values.
! 2- Modification proposed by Adam Wills for the optmization of the energy balance model:
!     - Implementation of calls to the intrinsic TRNSYS subroutines MoistAirProperties and AirProp to determine the moist air properties at each timestep
!     - The system of equations of the crops energy balance is solved using the modified secant method.
! 3- Modification proposed by Timothé Lalonde to use either electric lighting or solar light in the energy balance model.

!-----------------------------------------------------------------------------------------------------------------------
!Local variables declarations
Implicit None !force explicit declaration of local variables

!Inputs
Double precision T_a !Air temperature [degC]
Double precision RH !Relative humidity [-]
Double precision CO2_a  !CO2 concentration [ppm]
Double precision Q_sol  !Incident solar radiation [W/m^2]
Double precision status_EL ! ON:1 or OFF:0 
Double precision r_a !Aerodynamic stomatal resistance [s/m]

!Outputs
Double precision q_sens !Sensible gain to air from vegetation [kJ/hr]
Double precision q_lat !Latent gain to air from vegetation [kg/hr]
Double precision q_sens_watt !Sensible gain to air from vegetation [W/m^2]
Double precision q_lat_watt !Latent gain to air from vegetation [W/m^2]
Double precision T_s !Vegetation temperature [degC]
Double precision Rnet !Solar radiation avaiblable to the canopy [kJ/h]
Double precision Rsol !Net radiation absorb by vegetation [kJ/hr]
Double precision q_SW_EL !SW radiative heat reaching floor[kJ/h]
Double precision Y_DW !Total (shoot and root) dry weight per plant [g/plant]
Double precision SHT_FW !Total (shoot and root) dry weight per plant [g/plant]
Double precision FW_yield_total !Shoot fresh weight for the whole space[kg]
Double precision DW_yield_total !Total dry weight for the whole space[kg]
Double precision LAI !Leaf area index [m2_leaves/m2_cultivated floor area]

!Parameters
Double precision footprint !Percentage of the floor covered by the cultivation system
Double precision no_tiers !Number of tiers
Double precision PPFD_EL !PPFD of the electric lighting [-]
Double precision PAR_EL !Photosynthetically active radiation flux [W/m2] = Installed electric power * LED_eff
Double precision A_gr !Total area of greenhouse floor [m^2]
Double precision Crop_density !Number of crops per unit area [crops/m2]
Double precision k_P !Extinction coefficient for electric lighting [-]
Double precision c_beta !Constant from lettuce growth model (Van Henten, 1994)[-]
Double precision c_epsilon !Light use efficiency at very high CO2 concentrations (Van Henten, 1994)[g/J]
Double precision c_grmax   !Constant from lettuce growth model (Van Henten, 1994)[1/s]
Double precision SLA !Specific leaf area SLA[m2/g]
Double precision DW_content !Dry weight content 0 - 1 [-]
Double precision SHT_FW_yield !Shoot fresh weight of a yield per plant [g]
Double precision X_sdwfirst !Initial value of the structural dry weight [g/m^2]
Double precision X_nsdwfirst !Initial value of the non-structural dry weight [g/m^2]

!TRNSYS-derived
Double Precision Time,Timestep
Integer CurrentUnit,CurrentType

!Variables
Integer iStat
Integer iIter
Double precision PPFD !Total (solar and electrical) PPFD available for growth model [W/m^2]
Double precision CO2  !CO2 concentration [g/m3]
Double precision PAR !Total (solar and electrical) light available for growth model [W/m^2]
Double precision g_car !Carboxylation conductance[m/s]
Double precision g_CO2 !Canopy conductance to CO2 diffusion [m/s]
Double precision g_stm  !Stomatal conducance (Van Henten & Van Straten, 1994)[m/s]
Double precision eta !CO2 compensation point [ppm]
Double precision eps !Light use efficiency [g/J]
Double precision f_photmax ![g/(m^2*s^-1)]
Double precision f_phot !g/(m^2*s^-1)
Double precision f_resp!g/(m^2*s^-1)
Double precision r_gr
Double precision dXsdw !Additionnal structural dry weight [g/m^2]
Double precision dXnsdw !Additionnal non-structural dry weight [g/m^2]
Double precision Xsdw_initial !Structural dry weight from previous timestep [g/m^2]
Double precision Xnsdw_initial !Non-structural dry weight from previous timestep[g/m^2]
Double precision Xsdw !Structural dry weight[g/m^2]
Double precision Xnsdw !Non-structural dry weight[g/m^2]
Double precision Xsdw_transplant !Structural weight at the begining of the growth cycle [g/m^2]
Double precision Xnsdw_transplant!Non-structural weight at the begining of the growth cycle [g/m^2]
Double precision LA !Leaf area  [m2_leaves/plant]
Double precision SHT_DW !Dry weight (shoot only) [g/plant]
Double precision Y_DW_yield !Dry weight at harvest [g/m2]
Double precision num_of_yield
Double precision num_of_yield_initial
Double precision CD !Cultivated density[m^2_cultivated/m^2_floor]
Double precision CAC_rho !Cultivated Cover Area muliplied by light absorptivity of crops[-]
Double precision CAC_rho_PAR !Cultivated Cover Area muliplied by light absorptivity of crops[-]
Double precision R_EL
Double precision R_sol !Solar radiation avaiblable to the canopy [kJ/h]
Double precision G_EL 
Double precision G_sol 
Double precision r_s !Surface stomatal resistance [s/m]
Double precision Xa !Air vapor concentration [g/m3]
Double precision Xs !Vapour concentration at the canopy level [g/m3]
Double precision Xs_Delta !Vapour concentration at the canopy level [g/m3]
Double precision CmoistAir !Moist air specifc heat capacitance [kJ/m3K]
Double precision q_sens_watt_Delta !Sensible gain to air from vegetation [W/m^2]
Double precision q_lat_watt_Delta !Latent gain to air from vegetation [W/m^2]
Double precision T_s_Old  ! Transpiration surface temperature from previous iteration
Double precision T_s_Delta  ! Transpiration surface temperature perterbation
Double precision fRelErr  ! Relative error [%]
Double precision, dimension(9) :: psydat
Double precision, dimension(5) :: airprops
Double Precision :: lambda
Double Precision :: getWaterEvapEnth


!Constants
Double precision, Parameter :: c_car1 = -1.32d-5!Constant from lettuce growth model (Van Henten, 1994)[m/(s*degC^2)]
Double precision, Parameter :: c_car2 = 5.94d-4 !Constant from lettuce growth model (Van Henten, 1994)[m/(s*degC)]
Double precision, Parameter :: c_car3 = -2.64d-3 !Constant from lettuce growth model (Van Henten, 1994)[m/s]
Double precision, Parameter :: c_alpha = 0.68d0 !Constant from lettuce growth model (Van Henten, 1994)[-]
Double precision, Parameter :: g_bnd = 0.01d0 !Boundary layer conductance (Graamans et al., 2017)[m/s]
Double precision, Parameter :: c_eta = 7.32d-2 !CO2 compensation point at 20degC (Van Henten & Van Straten (1994)) 
Double precision, Parameter :: c_Q10eta = 2.0d0  !Constant from lettuce growth model (Van Henten, 1994)[-]
Double precision, Parameter :: c_respsht = 3.47d-7 !Constant from lettuce growth model (Van Henten, 1994)[1/s]
Double precision, Parameter :: c_teta = 0.15d0 !Constant from lettuce growth model for lettuces grown in soil (Van Henten, 1994)[-]
Double precision, Parameter :: c_resprt = 1.16d-7 !Constant from lettuce growth model (Van Henten, 1994)[1/s]
Double precision, Parameter :: c_Q10resp = 2 !Constant from lettuce growth model (Van Henten, 1994)[-]
Double precision, Parameter :: c_Q10gr = 1.6d0   !Constant from lettuce growth model (Van Henten, 1994)[-]
Double precision, Parameter :: k_T = 0.58 !Extinction coefficient for total solar radiation (Tei et al., 1996) [-]
Double Precision, Parameter :: gamma = 66.5d0 ! Psychometric constant [Pa/K]
Double Precision, Parameter :: Press = 1.d0   ! atmospheric pressure [atm]
Double Precision, Parameter :: fApproxErr = 0.000001d0 ! Maximum allowable approximate error [%]
Integer, Parameter :: iIterMax = 1000 ! Maximum number of iterations
Double Precision, Parameter :: fDelta = 0.00001d0

!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Get the Global Trnsys Simulation Variables
      Time=getSimulationTime()
      Timestep=getSimulationTimeStep()
      CurrentUnit = getCurrentUnit()
      CurrentType = getCurrentType()
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Set the Version Number for This Type
      If(getIsVersionSigningTime()) Then
		Call SetTypeVersion(17)
		Return
      EndIf
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Do Any Last Call Manipulations Here
      !If(getIsLastCallofSimulation()) Then
		!Return
      !EndIf
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Perform Any "After Convergence" Manipulations That May Be Required at the End of Each Timestep
      !If(getIsConvergenceReached()) Then
		!Return
      !EndIf
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Do All of the "Very First Call of the Simulation Manipulations" Here
      If(getIsFirstCallofSimulation()) Then

		!Tell the TRNSYS Engine How This Type Works
		Call SetNumberofParameters(15)          !The number of parameters that the the model wants
		Call SetNumberofInputs(6)              !The number of inputs that the the model wants
		Call SetNumberofDerivatives(0)         !The number of derivatives that the the model wants
		Call SetNumberofOutputs(13)             !The number of outputs that the the model produces
		Call SetIterationMode(1)               !An indicator for the iteration mode (default=1).  Refer to section 8.4.3.5 of the documentation for more details.
		Call SetNumberStoredVariables(2,3)     !The number of static variables that the model wants stored in the global storage array and the number of dynamic variables that the model wants stored in the global storage array
		Call SetNumberofDiscreteControls(0)    !The number of discrete control functions set by this model (a value greater than zero requires the user to use Solver 1: Powell's method)

        Call SetInputUnits(1,'TE1') !Air temperature [degC]
        Call SetInputUnits(2,'PC1') !Relative humidity [-]
        Call SetInputUnits(3,'DM1') !CO2 concentration [ppm]
        Call SetInputUnits(4,'IR2') !Available solar radiation [W/m^2]
        Call SetInputUnits(5,'DM1') !Electric lighting status [-]
        Call SetInputUnits(6,'DM1') !Aerodynamic boundary layer resistance [s/m]
        
        
        Call SetOutputUnits(1,'PW1') !Sensible gain to air from vegetation [kJ/hr]
        Call SetOutputUnits(2,'MF11') !Latent gain to air from vegetation [kg/hr]
        Call SetOutputUnits(3,'PW1') !Sensible gain to air from vegetation [W/m^2]
        Call SetOutputUnits(4,'PW1') !Latent gain to air from vegetation [W/m^2]
        Call SetOutputUnits(5,'TE1') !Vegetation temperature [degC]
        Call SetOutputUnits(6,'PW1') !Net radiation absorb by vegetation [kJ/hr]
		Call SetOutputUnits(7,'PW1') !Solar radiation absorbed by the vegetation [kJ/hr]
		Call SetOutputUnits(8,'PW1') !Electric lighting heat gain (SW portion) [kJ/hr]
		Call SetOutputUnits(9,'MA2') !Total (shoot and root) dry weight per plant [g/plant]
		Call SetOutputUnits(10,'MA2') !Shoot fresh weight per plant [g/plant]
		Call SetOutputUnits(11,'MA1') !Total shoot fresh weight [kg]
		Call SetOutputUnits(12,'MA1') !Total dry fresh weight [kg]
        Call SetOutputUnits(13,'DM1') !Leaf area index [m2_leaves/m2_cultivated floor area]
        
		Return

      EndIf
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Do All of the First Timestep Manipulations Here - There Are No Iterations at the Intial Time
      If (getIsStartTime()) Then
	  !Read in the Values of the Parameters from the Input File
          footprint = getParameterValue(1)!Percentage of the floor covered by the cultivation system
		  no_tiers = getParameterValue(2)!Number of tiers
		  PPFD_EL = getParameterValue(3)!PPFD of the electric lighting [-]
		  PAR_EL = getParameterValue(4)!Photosynthetically active radiation flux [W/m2] = Installed electric power * LED_eff
		  A_gr = getParameterValue(5)!Total area of greenhouse floor [m^2]
		  Crop_density = getParameterValue(6)!Number of crops per unit area [crops/m2]
		  k_P = getParameterValue(7)!Extinction coefficient for electric lighting [-]
		  c_beta = getParameterValue(8) !Constant from lettuce growth model (Van Henten, 1994)[-]
		  c_epsilon = getParameterValue(9) !Light use efficiency at very high CO2 concentrations (Van Henten, 1994)[g/J]
		  c_grmax  = getParameterValue(10) !Constant from lettuce growth model (Van Henten, 1994)[1/s]
		  SLA = getParameterValue(11)!Specific leaf area SLA [m2/g]
		  DW_content = getParameterValue(12)!Dry weight content 0 - 1 [-]
		  SHT_FW_yield = getParameterValue(13) !Shoot fresh weight of a yield per plant [g]
          X_sdwfirst = getParameterValue(14)!Initial value of the structural dry weight [g/m^2]
          X_nsdwfirst= getParameterValue(15)!Initial value of the non-structural dry weight [g/m^2]  	  
		  
		  !If Needed, Set the Initial Values of the Static Storage Variables (#,Value)
		  !Sample Code: SetStaticArrayValue(1,0.d0)
		  CALL setStaticArrayValue(1,X_sdwfirst)
		  CALL setStaticArrayValue(2,X_nsdwfirst)
		  
		  !If Needed, Set the Initial Values of the Dynamic Storage Variables (#,Value)
		  !Sample Code: Call SetDynamicArrayValueThisIteration(1,20.d0)
		  CALL setDynamicArrayInitialValue(1,X_sdwfirst)
		  CALL setDynamicArrayInitialValue(2,X_nsdwfirst)
		  CALL setDynamicArrayInitialValue(3,0.0d0)
		  
      !Check the Parameters for Problems (#,ErrorType,Text)
	 IF ((footprint < 0.) .or. ( footprint > 1.)) CALL foundBadParameter(1, 'Fatal', &
      'The footprint of the hydroponic system must be between 0 and 1')
	 IF ((no_tiers < 1.) .or. ( footprint > 20.)) CALL foundBadParameter(2, 'Fatal', &
      ' number of tiers is limited between 1 tier and 20 tiers')
	 IF ((PPFD_EL < 0.) .or. ( PPFD_EL > 750.)) CALL foundBadParameter(3, 'Fatal', &
      'The PPFD of the electric lighting should be between 0 and 750 umol/m^2/s.')
	 IF (PAR_EL < 0.) CALL foundBadParameter(4, 'Fatal', &
      'The PAR from electric lighting must be greater than 0')   
	 IF (A_gr < 0.d0) CALL foundBadParameter(5, 'Fatal', &
      'The florr area must greater than 0')
	 IF (Crop_density < 0.) CALL foundBadParameter(6, 'Fatal', &
      'The planting crop density must be greater than 0')   	  
	 IF ((k_P < 0.) .or. ( k_P > 1.)) CALL foundBadParameter(7, 'Fatal', &
      'The lettuce extinction coefficient to PAR from electric lighting must be between 0 and 1')
	 IF ((c_beta < 0.) .or. ( c_beta > 1.)) CALL foundBadParameter(8, 'Fatal', &
      'c_beta must be between 0 and 1')
	 IF (c_epsilon < 0.d0) CALL foundBadParameter(9, 'Fatal', &
      'c_epsilom must greater than 0')
	 IF (c_grmax < 0.d0) CALL foundBadParameter(10, 'Fatal', &
      'c_grmax must greater than 0')
	 IF (SLA < 0.d0) CALL foundBadParameter(11, 'Fatal', &
      'c_grmax must greater than 0') 
	 IF ((DW_content < 0.) .or. ( DW_content > 1.)) CALL foundBadParameter(12, 'Fatal', &
      'The dry weight content must be between 0 and 1')
	 IF (SHT_FW_yield < 0.d0) CALL foundBadParameter(13, 'Fatal', &
      'The shoot fresh weight of a yield per plant must greater than 0')
	 
	 IF (ErrorFound()) RETURN 
     
      !Set the Initial Values of the Outputs (#,Value)
      CALL setOutputValue(1, 0.0d0) !Sensible gain to air from vegetation [kJ/hr]
      CALL setOutputValue(2, 0.0d0) !Latent gain to air from vegetation [kg/hr]
      CALL setOutputValue(3, 0.0d0) !Sensible gain to air from vegetation [W/m^2]
      CALL setOutputValue(4, 0.0d0) !Latent gain to air from vegetation [W/m^2]
      CALL setOutputValue(5, 17.0d0) !Vegetation temperature [degC]
      CALL setOutputValue(6, 0.0d0) !Solar radiation avaiblable to the canopy [kJ/h]
	  CALL setOutputValue(7, 0.0d0) !Net radiation absorb by vegetation [kJ/hr]
	  CALL setOutputValue(8, 0.0d0) !SW radiative heat reaching floor[kJ/h]
	  CALL setOutputValue(9, 0.0d0) !Total (shoot and root) dry weight per plant [g/plant]
	  CALL setOutputValue(10, 0.0d0)!Total (shoot and root) dry weight per plant [g/plant]
	  CALL setOutputValue(11, 0.0d0) !Shoot fresh weight for the whole space[kg]
	  CALL setOutputValue(12, 0.0d0) !Total dry weight for the whole space[kg]
      CALL setOutputValue(13, 0.0d0) !Leaf area index [m2_leaves/m2_cultivated floor area]
	  
		Return	  
	  EndIf
 
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!ReRead the Parameters if Another Unit of This Type Has Been Called Last
      If(getIsReReadParameters()) Then
        !ReRead in the Values of the Parameters from the Input File
          footprint = getParameterValue(1)!Percentage of the floor covered by the cultivation system
		  no_tiers = getParameterValue(2)!Number of tiers
		  PPFD_EL = getParameterValue(3)!PPFD of the electric lighting [-]
		  PAR_EL = getParameterValue(4)!Photosynthetically active radiation flux [W/m2] = Installed electric power * LED_eff
		  A_gr = getParameterValue(5)!Total area of greenhouse floor [m^2]
		  Crop_density = getParameterValue(6)!Number of crops per unit area [crops/m2]
		  k_P = getParameterValue(7)!Extinction coefficient for electric lighting [-]
		  c_beta = getParameterValue(8) !Constant from lettuce growth model (Van Henten, 1994)[-]
		  c_epsilon = getParameterValue(9) !Light use efficiency at very high CO2 concentrations (Van Henten, 1994)[g/J]
		  c_grmax  = getParameterValue(10) !Constant from lettuce growth model (Van Henten, 1994)[1/s]
		  SLA = getParameterValue(11)!Specific leaf area SLA [m2/g]
		  DW_content = getParameterValue(12)!Dry weight content 0 - 1 [-]
		  SHT_FW_yield = getParameterValue(13) !Shoot fresh weight of a yield per plant [g]
          X_sdwfirst = getParameterValue(14)!Initial value of the structural dry weight [g/m^2]
          X_nsdwfirst= getParameterValue(15)!Initial value of the non-structural dry weight [g/m^2]
      EndIf
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Get the Input Values
      T_a = getInputValue(1) !Air temperature [degC]
      RH = getInputValue(2) !Relative humidity [-]
      CO2_a = getInputValue(3) !CO2 concentration [ppm]
      Q_sol = getInputValue(4) !Available solar radiation [W/m^2]
	  status_EL = getInputValue(5) !Electric lighting status [-]
      r_a = getInputValue(6) !Aerodynamic stomatal resistance [s/m]
	  
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Check the Inputs for Problems (#,ErrorType,Text)

      IF (T_a < -273.15d0) CALL foundBadInput(1, 'Fatal', & 
      'The input temperature is less than 0 K')
      IF ((RH > 100.d0) .or. (RH < 0.)) CALL foundBadInput(2, 'Fatal', & 
      'The relative humidity must be between 0 and 100.')
      IF (CO2_a < 0.) CALL foundBadInput(3, 'Fatal', & 
      'The CO2 concentration cannot be lower than 0')
      IF (Q_sol < 0.) CALL foundBadInput(4, 'Fatal', & 
      'The incident solar radiation cannot be lower than 0')
      IF ((status_EL > 1.) .or. (status_EL < 0.)) CALL foundBadInput(5, 'Fatal', & 
      'The relative humidity must be between 0 and 1')
	  IF (r_a <= 0.d0) CALL foundBadInput(6, 'Fatal', & 
      'The aerodynamic boundary layer resistance must be greater than 0.')
      
	  If(ErrorFound()) Return
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!    *** PERFORM ALL THE CALCULATION HERE FOR THIS MODEL. ***
!-----------------------------------------------------------------------------------------------------------------------

    !-----------------------------------------------------------------------------------------------------------------------
	!Lettuce growth model (Van Henten, 1994)
	!-----------------------------------------------------------------------------------------------------------------------    
    
    !-----------------------------------------------------------------------------------------------------------------------
	!If Needed, Get the Values from the Global Storage Array for the Static Variables (#)
	!Sample Code: STATIC1=getStaticArrayValue(1)
	!-----------------------------------------------------------------------------------------------------------------------
      Xsdw_transplant = getStaticArrayValue(1)
      Xnsdw_transplant = getStaticArrayValue(2)
    
    !-----------------------------------------------------------------------------------------------------------------------
	!If Needed, Get the Initial Values of the Dynamic Variables from the Global Storage Array (#)
	!Sample Code: T_INITIAL_1=getDynamicArrayValueLastTimestep(1)
	!-----------------------------------------------------------------------------------------------------------------------
      Xsdw_initial = getDynamicArrayValueLastTimestep(1)
      Xnsdw_initial = getDynamicArrayValueLastTimestep(2)
      num_of_yield_initial = getDynamicArrayValueLastTimestep(3)
    
    !-----------------------------------------------------------------------------------------------------------------------
	!Light available
	!-----------------------------------------------------------------------------------------------------------------------  
      PPFD = PPFD_EL*status_EL+Q_sol*2.02 !Total PPFD
	  PAR = 0.217*(PPFD) !W/m^2 eq to PAR solar radiation
	!-----------------------------------------------------------------------------------------------------------------------
	!Lettuce growth model (Van Henten, 1994)
	!-----------------------------------------------------------------------------------------------------------------------      
      
	  CO2 = CO2_a*0.00183d0  ![ppm] convert to [g/m^3]
      g_car= c_car1*T_a**2.0d0+c_car2*T_a+c_car3 !m/s
	  g_stm = (200d0+PPFD)/(60.0d0*(1500.0d0+PPFD))
      g_CO2 = (g_bnd**(-1.0d0)+g_stm**(-1.0d0)+g_car**(-1.0d0))**(-1.0d0) !m/s
      eta=c_eta*c_Q10eta**((T_a-20)/10) !ppm
      eps = c_epsilon*(CO2-eta)/(CO2+2.0d0*eta) !g/J
      f_photmax=eps*PAR*g_CO2*(CO2-eta)/(eps*PAR+g_CO2*(CO2-eta)) !g/(m^2*s^-1)	  
	  f_phot=f_photmax*(1-exp(-k_P*(SLA*(1-c_teta)*0.92d0*(Xsdw_initial+Xnsdw_initial))))!g/(m^2*s^-1) !ATTENTION, si on utilise de la lumière natturelle, changer la valeur de k_P
      f_resp=(c_respsht*(1-c_teta)*Xsdw_initial + c_resprt*c_teta*Xsdw_initial)*c_Q10resp**((T_a-25)/10) !g/(m^2*s^-1)
      r_gr=c_grmax*Xnsdw_initial/(Xsdw_initial + Xnsdw_initial)*c_Q10gr**((T_a-20)/10)
      
      dXsdw = r_gr*Xsdw_initial*Timestep*3600
      dXnsdw = (c_alpha*f_phot-r_gr*Xsdw_initial-f_resp-(1-c_beta)/c_beta*r_gr*Xsdw_initial)*Timestep*3600
	  Xsdw = Xsdw_initial + dXsdw
      Xnsdw = Xnsdw_initial + dXnsdw
      Y_DW = (Xsdw + Xnsdw)/Crop_density
      LAI = SLA*(1-c_teta)*0.92d0*(Xsdw+Xnsdw)
	  LA = LAI/Crop_density 
      SHT_DW = (1-c_teta)*Y_DW
	  SHT_FW = SHT_DW/DW_content
	   
	  IF (SHT_FW > SHT_FW_yield) THEN
      Y_DW_yield=Y_DW
      num_of_yield = num_of_yield_initial +1.0d0
	  Xsdw = Xsdw_transplant
      Xnsdw= Xnsdw_transplant
      Y_DW = (Xsdw + Xnsdw)/Crop_density
      LAI = SLA*(1-c_teta)*0.92d0*(Xsdw+Xnsdw)
	  LA = LAI/Crop_density 
      SHT_DW = (1-c_teta)*Y_DW
	  SHT_FW = SHT_DW/DW_content
      END IF
      
      FW_yield_total = (SHT_FW+num_of_yield*SHT_FW_yield)*Crop_density*CD*A_gr/1000
      DW_yield_total = (Y_DW+num_of_yield*Y_DW_yield)*Crop_density*CD*A_gr/1000
      
    !-----------------------------------------------------------------------------------------------------------------------
	!Lettuce energy balance (adapted from Graamans et al, 2017)
	!-----------------------------------------------------------------------------------------------------------------------    

IF(LAI>0) THEN    
      
      CAC_rho=1-exp(-k_T*LAI)
      CAC_rho_PAR=1-exp(-k_P*LAI)
      
      if ((CAC_rho>0.95d0) .or. (CAC_rho_PAR>0.95d0))then
        CAC_rho=0.95d0
		CAC_rho_PAR=0.95d0		
      end if

      
      ! Determine knowns from inputs and parameters
      R_EL = CAC_rho_PAR*PAR_EL*status_EL
	  R_sol = CAC_rho * Q_sol
	  Rnet = R_EL + R_sol ! Fraction of total emitted light absorbed by vegetation [W/m^2_cultivated]
	  G_EL = (1-CAC_rho_PAR)*PAR_EL*status_EL !PAR emitted but not absorbed 
	  G_sol = (1-CAC_rho) * Q_sol !PAR emitted but not absorbed 
	  
      r_s = 1/g_stm ! surface (stomatal) resistance [s/m]
      
	  ! Get ambient air properties
      psydat(1) = Press
      psydat(2) = T_a
      psydat(4) = RH/100.d0
      Call MoistAirProperties(CurrentUnit,CurrentType,1,2,0,psydat,1,iStat)
      Xa = psydat(9)*psydat(6)*1000.d0  ! g_water/m3
      Call AirProp((T_a+273.15d0),(Press*101.325d0),airprops)
      CmoistAir = (psydat(9)*airprops(5))+(Xa*1.82d0/1000.d0) ! Moist air specifc heat capacitance [kJ/m3K]

      ! Intialize iteration variables
      T_s = T_a-2.d0
      T_s_Old = T_s
      psydat(4) = 1.d0 ! Set to saturation
      fRelErr = 100.d0 ! Initialize relative error
      iIter = 0 ! Initialize iteration counter
      
      ! Iterate using the open modified secant method to find the root
      do while ((iIter<iIterMax) .and. (fRelErr>fApproxErr))
          ! Get saturated vapour concentration at transpiration surface
          psydat(2) = T_s
          Call MoistAirProperties(CurrentUnit,CurrentType,1,2,0,psydat,1,iStat)
          Xs = psydat(9)*psydat(6)*1000.d0  ! g_water/m3
          T_s_Delta = T_s+(T_s*fDelta)
          psydat(2) = T_s_Delta
          Call MoistAirProperties(CurrentUnit,CurrentType,1,2,0,psydat,1,iStat)
          Xs_Delta = psydat(9)*psydat(6)*1000.d0  ! g_water/m3
          
          ! Generate new guess of T_s
          lambda = getWaterEvapEnth(T_s)
          q_sens_watt = LAI*CmoistAir*(T_s-T_a)/r_a*1000.0d0 ! [W/m^2_cultivated]
          q_lat_watt = LAI*lambda*(Xs-Xa)/(r_s+r_a) ! [W/m^2_cultivated]
          q_sens_watt_Delta = LAI*CmoistAir*(T_s_Delta-T_a)/r_a*1000.0d0
          q_lat_watt_Delta = LAI*lambda*(Xs_Delta-Xa)/(r_s+r_a)
          T_s = T_s-((T_s*fDelta*(Rnet-q_sens_watt-q_lat_watt))/  &
            ((Rnet-q_sens_watt_Delta-q_lat_watt_Delta)-(Rnet-q_sens_watt-q_lat_watt)))
      
          ! Update iteration parameters
          fRelErr = abs((T_s-T_s_Old)/T_s)*100.d0
          iIter = iIter + 1
          T_s_Old = T_s
      end do
      If(iIter>=iIterMax) Then
          Call Messages(-1,'Maximum number of iterations exceeded','Fatal',CurrentUnit,CurrentType)
          Return 1
      EndIf
      
	  ! Convert W/m^2 to kJ/hr
      CD=footprint*no_tiers
      q_sens = q_sens_watt * 3.6d0 * A_gr * CD! Convert W/m^2_cultivated to kJ/hr 
      q_lat = q_lat_watt * 3.6d0 / lambda * A_gr * CD! Convert W/m^2_cultivated to kg/hr 
      Rnet = Rnet * 3.6d0 * A_gr * CD! Convert W/m^2 to kJ/hr
	  Rsol = R_sol * 3.6d0 * A_gr * CD! Convert W/m^2 to kJ/hr
	  q_sw_EL = G_EL * 3.6d0 * A_gr * CD! Convert W/m^2 to kJ/hr
ELSE
      q_sens = 0.0d0
      q_lat = 0.0d0
      Rnet = 0.0d0
      Rsol = 0.0d0
      q_sw_EL = PAR_EL*status_EL * 3.6d0 * A_gr * CD! Convert W/m^2 to kJ/hr
ENDIF     

!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Set the Outputs from this Model (#,Value)
!-----------------------------------------------------------------------------------------------------------------------

      CALL setOutputValue(1, q_sens)!Sensible gain to air from vegetation [kJ/hr]
      CALL setOutputValue(2, q_lat)!Latent gain to air from vegetation [kg/hr]
      CALL setOutputValue(3, q_sens_watt)!Sensible gain to air from vegetation [W/m^2]
      CALL setOutputValue(4, q_lat_watt)!Sensible gain to air from vegetation [W/m^2]
      CALL setOutputValue(5, T_s)!Vegetation temperature [degC]
      CALL setOutputValue(6, Rnet)!Net radiation absorbed by the crop [kJ/hr]
	  CALL setOutputValue(7, Rsol)!Net solar radiation absorbed by the crop [kJ/hr]
	  CALL setOutputValue(8, q_SW_EL)!SW radiative heat reaching floor[kJ/h]
	  CALL setOutputValue(9, Y_DW)!Total (shoot and root) dry weight per plant [g/plant]
	  CALL setOutputValue(10, SHT_FW)!Total (shoot and root) dry weight per plant [g/plant]
      CALL setOutputValue(11, FW_yield_total)!Shoot fresh weight for the whole space[kg]
      CALL setOutputValue(12, DW_yield_total)!Total dry weight for the whole space[kg]
      CALL setOutputValue(13, LAI)!Leaf area index [m2_leaves/m2_cultivated floor area]
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!If Needed, Store the Final value of the Dynamic Variables in the Global Storage Array (#,Value)
!Sample Code:  Call SetValueThisIteration(1,T_FINAL_1)
!-----------------------------------------------------------------------------------------------------------------------
      CALL setDynamicArrayValueThisIteration(1,Xsdw)
      CALL setDynamicArrayValueThisIteration(2,Xnsdw)
      CALL setDynamicArrayValueThisIteration(3,num_of_yield)
      Return
      End
!-----------------------------------------------------------------------------------------------------------------------


!-----------------------------------------------------------------------------------------------------------------------
! Function which returns the enthapy of vapourization [kJ/kg] for water as a function of temperature [oC]
! Reference: Cengel, Y.A. & Boles, M.A. (2008). "Thermodynamics: An Engineering Approach", 6th Ed. McGraw-Hill, New York
!-----------------------------------------------------------------------------------------------------------------------
Double Precision Function getWaterEvapEnth(fTemp)
    Implicit None
    Double Precision, Intent(in) :: fTemp 
    getWaterEvapEnth = ((-3.6D-3)*(fTemp**2.0))-(2.0272D0*fTemp)+2495.2D0
End Function getWaterEvapEnth


