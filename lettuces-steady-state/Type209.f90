      Subroutine Type209
      Use TrnsysConstants
      Use TrnsysFunctions

!DEC$Attributes DLLexport :: Type209
!-----------------------------------------------------------------------------------------------------------------------
! Type204 developped at Ecole de technologie superieure (ETS in Montreal).
! Written by Marie-Helene Talbot based on Luuks Graamans article
! Type 209 is a version of Type 204 that include modification proposed by A. Wills:
!     - Implementation of calls to the intrinsic TRNSYS subroutines MoistAirProperties and AirProp to determine the moist air properties at each timestep
!     - The system of equations of the crops energy balance is solved using the modified secant method.
!-----------------------------------------------------------------------------------------------------------------------
!Local variables declarations
Implicit None !force explicit declaration of local variables

!Inputs
Double precision T_a !Air temperature [degC]
Double precision RH !Relative humidity [-]
Double precision PAR !Photosynthetically active radiation flux [W/m2]
Double precision PPFD !nu_mol/m^2*s
Double precision r_a !Aerodynamic stomatal resistance [s/m]

!Outputs
Double precision q_sens !Sensible gain to air from vegetation [kJ/hr]
Double precision q_lat !Latent gain to air from vegetation [kg/hr]
Double precision q_sens_watt !Sensible gain to air from vegetation [W/m^2]
Double precision q_lat_watt !Latent gain to air from vegetation [W/m^2]
Double precision T_s !Vegetation temperature [degC]
Double precision Rnet !Net radiation absorb by vegetation [kJ/hr]



!Parameters
Double precision LAI !Leaf Area index [-]
Double precision Afv !Cultivated fraction [-]
Double precision rho_v!Lettuce relfectivity [-]
Double precision LED_eff !LED efficiency [-]
Double precision CAC !Coverage of the floor of the cultivated area [-]
Double precision A_gr !Total area of greenhouse floor [m^2]



!TRNSYS-derived
Double Precision Time,Timestep
Integer CurrentUnit,CurrentType

!Variables
Integer iStat
Integer iIter
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
		Call SetNumberofParameters(5)          !The number of parameters that the the model wants
		Call SetNumberofInputs(5)              !The number of inputs that the the model wants
		Call SetNumberofDerivatives(0)         !The number of derivatives that the the model wants
		Call SetNumberofOutputs(6)             !The number of outputs that the the model produces
		Call SetIterationMode(1)               !An indicator for the iteration mode (default=1).  Refer to section 8.4.3.5 of the documentation for more details.
		Call SetNumberStoredVariables(0,0)     !Only one dynamic variable (T_s)
        !The number of static variables that the model wants stored in the global storage array and the number of dynamic variables that the model wants stored in the global storage array
		Call SetNumberofDiscreteControls(0)    !The number of discrete control functions set by this model (a value greater than zero requires the user to use Solver 1: Powell's method)

        Call SetInputUnits(1,'TE1') !Air temperature [degC]
        Call SetInputUnits(2,'PC1') !Relative humidity [-]
        Call SetInputUnits(3,'IR2') !Photosynthetically active radiation flux [W/m2]
        Call SetInputUnits(4,'DM1') !nu_mol/m^2*s !Ratio de 5 provient des r�sultats de Gramaans
        Call SetInputUnits(5,'DM1') !Aerodynamic boundary layer resistance [s/m]
        
        Call SetOutputUnits(1,'PW1') !Sensible gain to air from vegetation [kJ/hr]
        Call SetOutputUnits(2,'MF11') !Latent gain to air from vegetation [kg/hr]
        Call SetOutputUnits(3,'PW1') !Sensible gain to air from vegetation [W/m^2]
        Call SetOutputUnits(4,'PW1') !Sensible gain to air from vegetation [W/m^2]
        Call SetOutputUnits(5,'TE1') !Vegetation temperature [degC]
        Call SetOutputUnits(6,'PW1') !Net radiation absorb by vegetation [kJ/hr]
        
		Return

      EndIf
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Do All of the First Timestep Manipulations Here - There Are No Iterations at the Intial Time
      If (getIsStartTime()) Then
      !Read in the Values of the Parameters from the Input File
          LAI = getParameterValue(1) !Leaf Area Index [-]
          Afv = getParameterValue(2) !Cultivated fraction area of floor [-]
          rho_v = getParameterValue(3) !Lettuce relfectivity [-]
          CAC = getParameterValue(4) !Coverage of the floor of the cultivated area [-]
          A_gr = getParameterValue(5) !Total area of greenhouse floor [m^2]
      
      !Check the Parameters for Problems (#,ErrorType,Text)
      IF (LAI< 0.d0) CALL foundBadParameter(1, 'Fatal', &
         'The LAI must be greater than 0')      
      IF ((Afv < 0.d0) .or. (Afv > 10.d0)) CALL foundBadParameter(2, 'Fatal', &
         'The Cultivated fraction area of floor must be between 0 and 10 (1000%)')
     IF ((rho_v < 0.d0) .or. ( rho_v > 1.)) CALL foundBadParameter(3, 'Fatal', &
      'The lettuce leaf light reflectivity must be between 0 and 1')
     IF ((CAC < 0.d0) .or. (CAC > 1.)) CALL foundBadParameter(4, 'Fatal', &
         'The coverage of the floor of the cultivated area must be between 0 and 1')
     IF (A_gr < 0.d0) CALL foundBadParameter(5, 'Fatal', &
      'Total area of greenhouse floor must greater than 0')

     IF (ErrorFound()) RETURN 
     
      !Set the Initial Values of the Outputs (#,Value)
      CALL setOutputValue(1, 0.0d0) !Sensible gain to air from vegetation [kJ/hr]
      CALL setOutputValue(2, 0.0d0) !Latent gain to air from vegetation [kg/hr]
      CALL setOutputValue(3, 0.0d0) !Sensible gain to air from vegetation [kJ/hr]
      CALL setOutputValue(4, 0.0d0) !Latent gain to air from vegetation [kg/hr]
      CALL setOutputValue(5, 17.0d0) !Vegetation temperature [degC]
      CALL setOutputValue(6, 0.0d0) !Light energy that got absorbed [kJ/hr]

		Return

      EndIf
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!ReRead the Parameters if Another Unit of This Type Has Been Called Last
      If(getIsReReadParameters()) Then
        !ReRead in the Values of the Parameters from the Input File
          LAI = getParameterValue(1) !Leaf Area Index [m2 leaf/m2 cultivated area]
          Afv = getParameterValue(2) !Cultivated fraction area of floor [-]
          rho_v = getParameterValue(3) !Lettuce relfectivity [-]
          CAC = getParameterValue(4) !Coverage of the floor of the cultivated area 2-D projection [-]
          A_gr = getParameterValue(5) !Total area of greenhouse floor [m^2]
      EndIf
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Get the Input Values
      T_a = getInputValue(1) !Air temperature [degC]
      RH = getInputValue(2) !Relative humidity [-]
      PAR = getInputValue(3) !Photosynthetically active radiation flux [W/m2]
      PPFD = getInputValue(4) ! Photosynthetic photon flux density [�mol/m2s]
      r_a = getInputValue(5) !Aerodynamic stomatal resistance [s/m]

!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Check the Inputs for Problems (#,ErrorType,Text)

      IF (T_a < -273.15d0) CALL foundBadInput(1, 'Fatal', & 
      'The input temperature is less than 0 K')
      IF ((RH > 100.d0) .or. (RH < 0.)) CALL foundBadInput(2, 'Fatal', & 
      'The relative humidity must be between 0 and 100.')
      IF (PAR < 0.d0) CALL foundBadInput(3, 'Fatal', & 
      'The Photosynthetically active radiation flux input has to be greater than 0.')
      IF (PPFD < 0.d0) CALL foundBadInput(4, 'Fatal', & 
      'The photosynthetic photon flux density cannot be less than 0.')
      IF (r_a <= 0.d0) CALL foundBadInput(5, 'Fatal', & 
      'The aerodynamic boundary layer resistance must be greater than 0.')
      If(ErrorFound()) Return
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!    *** PERFORM ALL THE CALCULATION HERE FOR THIS MODEL. ***
!-----------------------------------------------------------------------------------------------------------------------
      ! Determine knowns from inputs and parameters
      Rnet = (1.d0-rho_v)*PAR*CAC ! Fraction of total emitted light absorbed by vegetation [W/m^2_cultivated]
      r_s = 60.d0*(1500.d0+PPFD)/(200.d0+PPFD) ! surface (stomatal) resistance [s/m]
      !    Get ambient air properties
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
          ! Get saturated vapour concentration at tranpiration surface
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
      ! Convert units
      q_sens = q_sens_watt * 3.6d0 * (A_gr*Afv)! Convert W/m^2_cultivated to kJ/hr 
      q_lat = q_lat_watt * 3.6d0 / lambda * (A_gr*Afv)! Convert W/m^2_cultivated to kg/hr 
      Rnet = Rnet * 3.6d0 * (A_gr*Afv)! Convert W/m^2 to kJ/hr
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

