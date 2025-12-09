#
# Needed E+ objects to performs CEA simulation
#

def addition(zone_name, f_v, f_LW, LL):
    s = """!###################################
  
    Schedule:Constant ,
      AlwaysOn , !- Name
      On/Off , !- Schedule Type Limits Name
      1.0; !- Hourly Value
  
    OtherEquipment,
      OTHEQ_SENSIBLE, !- Name
      , !- Fuel Use Type
      {zone}, !- Zone or ZoneList or Space or SpaceList Name
      ALWAYSON , !- SCHEDULE Name
      EquipmentLevel , !- Design Level calculation method
      0, !- Design Level {{W}}
      , !- Power per Zone Floor Area {{watts/m2}}
      , !- Power per Person {{watts/person}}
      0, !- Fraction Latent
      , !- Fraction Radiant
      0, !- Fraction Lost
      0, !- Carbon Dioxide Generation Rate
      ; !- End -Use Subcategory
  
    OtherEquipment,
      OTHEQ_LATENT, !- Name
      , !- Fuel Use Type
      {zone}, !- Zone or ZoneList or Space or SpaceList Name
      ALWAYSON , !- SCHEDULE Name
      EquipmentLevel , !- Design Level calculation method
      0, !- Design Level {{W}}
      , !- Power per Zone Floor Area {{watts/m2}}
      , !- Power per Person {{watts/person}}
      1, !- Fraction Latent
      , !- Fraction Radiant
      0, !- Fraction Lost
      0, !- Carbon Dioxide Generation Rate
      ; !- End -Use Subcategory
  
    OtherEquipment,
      OTHEQ_RAD, !- Name
      , !- Fuel Use Type
      {zone}, !- Zone or ZoneList or Space or SpaceList Name
      ALWAYSON , !- SCHEDULE Name
      EquipmentLevel , !- Design Level calculation method
      0, !- Design Level {{W}}
      , !- Power per Zone Floor Area {{watts/m2}}
      , !- Power per Person {{watts/person}}
      , !- Fraction Latent
      1, !- Fraction Radiant
      0, !- Fraction Lost
      0, !- Carbon Dioxide Generation Rate
      ; !- End -Use Subcategory
  
    Output:Variable,{zone},Zone Air Temperature,timestep;
    Output:Variable,{zone},Zone Air Relative Humidity,timestep;
    Output:Variable,Vegetation Temperature,PythonPlugin:OutputVariable,Timestep;
  
    PythonPlugin:Instance,
      CEA_Sim,  !- Name
      Yes,                     !- Run During Warmup Days
      main,  !- Python Module Name
      type205;         !- Plugin Class Name
  
    PythonPlugin:Variables,
      MyGlobals,               !- Name
      VegTemp;     !- Variable Name 1
  
    PythonPlugin:OutputVariable,
      Vegetation Temperature,  !- Name
      VegTemp,     !- Python Plugin Variable Name
      Averaged,                !- Type of Data in Variable
      ZoneTimestep,            !- Update Frequency
      C;                       !- Units
      
    Output:Variable,
      Lights,                  !- Key Value
      Schedule Value,          !- Variable Name
      timestep;                !- Reporting Frequency
    
    Schedule:Compact,
      Lights,                  !- Name
      OnOff,                   !- Schedule Type Limits Name
      Through: 12/31,          !- Field 1
      For: AllDays,            !- Field 2
      Until: 04:00,            !- Field 3
      0,                       !- Field 4
      Until: 20:00,            !- Field 5
      1,                       !- Field 6
      Until: 24:00,            !- Field 7
      0;                       !- Field 8
    
    Lights,
      LEDs,                    !- Name
      Thermal Zone 1,          !- Zone or ZoneList or Space or SpaceList Name
      Lights,                  !- Schedule Name
      LightingLevel,              !- Design Level Calculation Method
      {LL},                        !- Lighting Level {{W}}
      ,                  !- Watts per Zone Floor Area {{W/m2}}
      ,                        !- Watts per Person {{W/person}}
      ,                        !- Return Air Fraction
      {f_LW},                    !- Fraction Radiant
      {f_v},                     !- Fraction Visible
      ,                        !- Fraction Replaceable
      General,                 !- End-Use Subcategory
      No;                      !- Return Air Fraction Calculated from Plenum Temperature
          
      """

    return s.format(zone=zone_name, f_LW=f_LW, f_v=f_v, LL=LL)
