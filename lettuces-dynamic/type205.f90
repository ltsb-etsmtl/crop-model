      Subroutine Type205
      Use TrnsysConstants
      Use TrnsysFunctions

!DEC$Attributes DLLexport :: Type205
!-----------------------------------------------------------------------------------------------------------------------
! Type205 developped at Ecole de technologie superieure (ETS in Montreal).
! Written by Marie-Helene Talbot.
! The previous version, Type 204, was based on Graamans et al. (2017) work.      
!
! Type 205 is a dynamic version of Type 204 that consider crops growing with time in the estimation of crops heat gains.
!     - LAI was added as an input (instead of being a parameter). 
!     - LAI can be linked to a text file to consider LAI and CAC varying with time.
!     - As an example, a text file is available on GitHub:
!        * First column: LAI calculated according to Shimizu et al. (2018)
!        * Second column : CAC calculated according to Tei, Scaife, and Aikman (1996)
!
!-----------------------------------------------------------------------------------------------------------------------
!Local variables declarations
Implicit None !force explicit declaration of local variables

!Inputs
Double precision T_a !Air temperature [degC]
Double precision RH !Relative humidity [-]
Double precision P_LED !LED power density [W/m2]
Double precision A_gr !Floor area [m^2]
Double precision LAI !LeafAreaIndex [m^2_leaves/m^2_cultivated area]
Double precision CAC !Coverage of the floor of the cultivated area [-]

!Outputs
Double precision q_sens !Sensible gain to air from vegetation [kJ/hr]
Double precision q_lat !Latent gain to air from vegetation [kg/hr]
Double precision T_s !Vegetation temperature [degC]
Double precision q_loss !Light energy that got reflected [W]

!Parameters
Double precision Afv !Cultivated fraction [-]
Double precision rho_a !Air density [kg/m^3]
Double precision c_p !Air specific heat capacity [kJ/kgK]
Double precision lambda !Latent heat vapor of water [kJ/kg]
Double precision gamma !Psychometric constant [Pa/K]
Double precision rho_v !Lettuce relfectivity [-]
Double precision LED_eff !LED efficiency [-]

!TRNSYS-derived
Double Precision Time,Timestep

!Variables
Double precision PPFD !nu_mol/m^2*s
Double precision I_light !LED power convert to short-wave radiation [W/m^2]
Double precision Rnet !Short-wave radiation absorbed by vegetation [W/m^2]
Double precision r_a !Aerodynamic stomatal resistance [s/m]
Double precision r_s !Surface stomatal resistance [s/m]
Double precision e_star !Saturated vapor pressure [kPa]
Double precision Xa_star !Saturated vapor concentration [g/m3]
Double precision e !Air vapor pressure [kPa]
Double precision Xa !Air vapor concentration [g/m3]
Double precision delta !Slope of the relationship between the saturation vapour pressure and air temperature [kPa/degC]
Double precision epsilon !Vapour concentration
Double precision Xs !Vapour concentration at the canopy level [g/m3]
Double precision q_sens_watt !Sensible gain to air from vegetation [W/m^2]
Double precision q_lat_watt !Latent gain to air from vegetation [W/m^2]
Double precision T_s_final
Double precision Res

!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Get the Global Trnsys Simulation Variables
      Time=getSimulationTime()
      Timestep=getSimulationTimeStep()
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
		Call SetNumberofParameters(7)          !The number of parameters that the the model wants
		Call SetNumberofInputs(6)              !The number of inputs that the the model wants
		Call SetNumberofDerivatives(0)         !The number of derivatives that the the model wants
		Call SetNumberofOutputs(4)             !The number of outputs that the the model produces
		Call SetIterationMode(1)               !An indicator for the iteration mode (default=1).  Refer to section 8.4.3.5 of the documentation for more details.
		Call SetNumberStoredVariables(0,0)     !Only one dynamic variable: T_s
        !The number of static variables that the model wants stored in the global storage array and the number of dynamic variables that the model wants stored in the global storage array
		Call SetNumberofDiscreteControls(0)    !The number of discrete control functions set by this model (a value greater than zero requires the user to use Solver 1: Powell's method)

        Call SetInputUnits(1,'TE1') !Air temperature [degC]
        Call SetInputUnits(2,'PC1') !Relative humidity [-]
        Call SetInputUnits(3,'PW2') !Total LED Power [W]
        Call SetInputUnits(4,'AR1') !Agriculture space area [m^2]
		Call SetInputUnits(5,'DM1') !Leaf Area Index [m^2_leaves/m^2_cultivated area]
        Call SetInputUnits(6,'DM1') !Coverage of the floor of the cultivated area [-]
        
        Call SetOutputUnits(1,'PW1') !Sensible gain to air from vegetation [kJ/hr]
        Call SetOutputUnits(2,'MF11') !Latent gain to air from vegetation [kg/hr]
        Call SetOutputUnits(3,'TE1') !Vegetation temperature [degC]
        Call SetOutputUnits(4,'PW1') !Light energy that got reflected [W]

		Return

      EndIf
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Do All of the First Timestep Manipulations Here - There Are No Iterations at the Intial Time
      If (getIsStartTime()) Then
      !Read in the Values of the Parameters from the Input File
      !Sample Code: PAR1 = getParameterValue(1)
          Afv = getParameterValue(1) !Cultivated fraction area of floor [-]
          rho_a = getParameterValue(2) !Air density [kg/m^2]
          c_p = getParameterValue(3) !Air specific heat capacity [kJ/kgK]
          lambda = getParameterValue(4) !Latent heat vapor of water [kJ/kg]
          gamma = getParameterValue(5) !Psychometric constant [Pa/K]
          rho_v = getParameterValue(6) !Lettuce relfectivity [-]
          LED_eff = getParameterValue(7) !LED efficiency [-]
      
      !Check the Parameters for Problems (#,ErrorType,Text)
      !Sample Code: If( PAR1 <= 0.) Call FoundBadParameter(1,'Fatal','The first parameter provided to this model is not acceptable.')  
      IF ((Afv < 0.) .or. (Afv > 10.)) CALL foundBadParameter(1, 'Fatal', &
         'The Cultivated fraction area of floor must be between 0 and 10 (1000%)')
      IF (rho_a < 0.) CALL foundBadParameter(2, 'Fatal', &
      'The air density must greater than 0')
      IF (c_p < 0.) CALL foundBadParameter(3, 'Fatal', &
      'The air specific heat capacity must greater than 0')
      IF (lambda < 0.) CALL foundBadParameter(4, 'Fatal', &
      'The latent heat vapor of water must be greater than 0')
     IF ((rho_v < 0.) .or. ( rho_v > 1.)) CALL foundBadParameter(6, 'Fatal', &
      'The lettuce leaf light reflectivity must be between 0 and 1')
     IF ((LED_eff < 0.) .or. (LED_eff > 1.)) CALL foundBadParameter(7, 'Fatal', &
         'The LED efficiency must be between 0 and 1') 

     IF (ErrorFound()) RETURN 
     
      !Set the Initial Values of the Outputs (#,Value)
      !Sample Code: Call SetOutputValue(1,getInputValue(1))
      CALL setOutputValue(1, 0.0d0) !Sensible gain to air from vegetation [kJ/hr]
      CALL setOutputValue(2, 0.0d0) !Latent gain to air from vegetation [kg/hr]
      CALL setOutputValue(3, 17.0d0) !Vegetation temperature [degC]
      CALL setOutputValue(4, 0.0d0) !Light energy that got reflected [W]
      !CALL setOutputValue(5, 0.5d0) !Lead area index [-]

      !If Needed, Set the Initial Values of the Static Storage Variables (#,Value)
      !Sample Code: SetStaticArrayValue(1,0.d0)

      !If Needed, Set the Initial Values of the Dynamic Storage Variables (#,Value)
      !Sample Code: Call SetDynamicArrayValueThisIteration(1,20.d0)
      
      !Call SetDynamicArrayValueThisIteration(1,17.0d0)

      !If Needed, Set the Initial Values of the Discrete Controllers (#,Value)
      !Sample Code for Controller 1 Set to Off: Call SetDesiredDiscreteControlState(1,0) 

		Return

      EndIf
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!ReRead the Parameters if Another Unit of This Type Has Been Called Last
      If(getIsReReadParameters()) Then
        !ReRead in the Values of the Parameters from the Input File
        !Sample Code: PAR1 = getParameterValue(1)
          Afv = getParameterValue(1) !Cultivated fraction area of floor [-]          
		  rho_a = getParameterValue(2) !Air density [kg/m^2]
          c_p = getParameterValue(3) !Air specific heat capacity [kJ/kgK]
          lambda = getParameterValue(4) !Latent heat vapor of water [kJ/kg]
          gamma = getParameterValue(5) !Psychometric constant [Pa/K]
          rho_v = getParameterValue(6) !Lettuce relfectivity [-]
          LED_eff = getParameterValue(7) !LED efficiency [-]
      EndIf

!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Get the Input Values
     !Sample Code: IN1=getInputValue(1)
      T_a = getInputValue(1) !Air temperature [degC]
      RH = getInputValue(2) !Relative humidity [-]
      P_LED = getInputValue(3) !Total LED Power [W]
      A_gr = getInputValue(4) !BIAs area [W]
	  LAI = getInputValue(5) !LAI [-]
	  CAC = getInputValue(6) !CAC [-]

!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Check the Inputs for Problems (#,ErrorType,Text)
	  !Sample Code: If( IN1 <= 0.) Call FoundBadInput(1,'Fatal','The first input provided to this model is not acceptable.')

      IF (T_a < -273.15d0) CALL foundBadInput(1, 'Fatal', & 
      'The input temperature is less than 0 K')
      IF (P_LED < 0.) CALL foundBadInput(2, 'Fatal', & 
      'The total power input have to be greater than 0')
      IF ((RH > 100.) .or. (RH < 0.)) CALL foundBadInput(3, 'Fatal', & 
      'The relative humidity must be between 0 and 100')
      IF (A_gr < 0.) CALL foundBadInput(4, 'Fatal', & 
      'The agriculture space area have to be greater than 0')
      If(ErrorFound()) Return
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!    *** PERFORM ALL THE CALCULATION HERE FOR THIS MODEL. ***
!-----------------------------------------------------------------------------------------------------------------------

	!-----------------------------------------------------------------------------------------------------------------------
	!If Needed, Get the Initial Values of the Dynamic Variables from the Global Storage Array (#)
	!Sample Code: T_INITIAL_1=getDynamicArrayValueLastTimestep(1)
	!-----------------------------------------------------------------------------------------------------------------------
      !T_s = getDynamicArrayValueLastTimestep(1)
      
      PPFD = P_LED*LED_eff*5.0d0
      I_light = LED_eff*P_LED
      Rnet = (1-rho_v)*I_light*CAC
      q_loss= I_light - Rnet
      
      r_a = 100
      r_s = 60*(1500+PPFD)/(200+PPFD)
      
      T_s = T_a-2.0d0
      res = 1.0d0 !To get in the loop
      
do while (res>0.0001d0) 
    T_s=T_s+0.0001d0
    e_star = 0.611d0*exp(17.4d0*T_a/(T_a+239.0d0));
    Xa_star = rho_a*c_p/lambda/(gamma/1000.0d0)*e_star*1000.0d0 ![g/m^3]
    e = RH/100*e_star
    Xa = 7.4d0*e  ![g/m^3]
    delta = 0.04145d0*e**(0.06088d0*T_a)
    epsilon = delta/(gamma/1000.0d0)
    Xs = Xa_star+rho_a*1000.0d0*c_p/lambda*epsilon*(T_s-T_a)
    q_lat_watt = LAI*lambda*(Xs-Xa)/(r_s+r_a) !W/m^2
    q_sens_watt = LAI*rho_a*c_p*(T_s-T_a)/r_a*1000.0d0;
    res = Rnet-q_sens_watt-q_lat_watt
      
    !T_s_final = T_a + q_sens*r_a*1000.0d0/rho_a/LAI/c_p
end do  
      ! Convert units
      q_sens = q_sens_watt * 3.6d0 * (A_gr*Afv)! Convert W/m^2 to kJ/hr 
      q_lat = q_lat_watt * 3.6d0 / lambda * (A_gr*Afv)
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Set the Outputs from this Model (#,Value)
    !Sample Code: OUT1=setOutputValue(1,0.d0)
!-----------------------------------------------------------------------------------------------------------------------

      CALL setOutputValue(1, q_sens)!Sensible gain to air from vegetation [kJ/hr]
      CALL setOutputValue(2, q_lat)!Latent gain to air from vegetation [kg/hr]
      CALL setOutputValue(3, T_s)!Vegetation temperature [degC]
      CALL setOutputValue(4, q_loss)!Light energy that got reflected [W]

!-----------------------------------------------------------------------------------------------------------------------
!If Needed, Store the Desired Disceret Control Signal Values for this Iteration (#,State)
!Sample Code:  Call SetDesiredDiscreteControlState(1,1)
!-----------------------------------------------------------------------------------------------------------------------
      
!-----------------------------------------------------------------------------------------------------------------------
!If Needed, Store the Final value of the Dynamic Variables in the Global Storage Array (#,Value)
!Sample Code:  Call SetValueThisIteration(1,T_FINAL_1)
!-----------------------------------------------------------------------------------------------------------------------
      !CALL setDynamicArrayValueThisIteration(1, T_s_final)
      Return
      End
!-----------------------------------------------------------------------------------------------------------------------
