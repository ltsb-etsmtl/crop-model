// C++ Headers
#include <iostream>
#include <math.h>
#include <string>
#include <stdexcept>
#include <fstream>
#include <sstream>

using namespace std;

main(){
std::string str;

// std::ofstream out;
// out.open("out.txt");
// out << string("LAI,CAC,Sensible heat flux [kJ/hr],Latent heat flux [kg/hr]") << std::endl ;


std::ifstream inf;
inf.open("Example_LAI_CAC.txt");

std::ofstream out;
out.open("out.txt");
out << string("LAI,CAC,Sensible heat flux (kJ/hr),Latent heat flux (kg/hr)") << std::endl;

while (std::getline(inf, str))
{
double arr[2];
stringstream ssin(str);
int i=0;
while (ssin.good() && i < 2){
        ssin >> arr[i];
        ++i;
    }
//Inputs
double T_a = 15; //Air temperature [degC]
double RH = 20; //Relative humidity [-]
double P_LED = 120; //Total LED Power [W]
double A_gr = 2; //Floor area [m^2]
double LAI = arr[0]; //LeafAreaIndex [m^2_leaves/m^2_cultivated area]
double CAC = arr[1]; //Coverage of the floor of the cultivated area [-]

//Outputs
double q_sens; //Sensible gain to air from vegetation [kJ/hr]
double q_lat; //Latent gain to air from vegetation [kg/hr]
double T_s; //Vegetation temperature [degC]
double q_loss; //Light energy that got reflected [W]

//Parameters
double Afv=0.1; //Cultivated fraction [-]
double rho_a = 1.2; //Air density [kg/m^3]
double c_p = 1.006; //Air specific heat capacity [kJ/kgK]
double lambda = 2489; //Latent heat vapor of water [kJ/kg]
double gamma = 66.5; //Psychometric constant [Pa/K]
double rho_v = 0.05; //Lettuce relfectivity [-]
double LED_eff = 0.52; //LED efficiency [-]

//Variables
double PPFD; //nu_mol/m^2*s
double I_light; //LED power convert to short-wave radiation [W/m^2]
double Rnet; //Short-wave radiation absorbed by vegetation [W/m^2]
double r_a; //Aerodynamic stomatal resistance [s/m]
double r_s; //Surface stomatal resistance [s/m]
double e_star; //Saturated vapor pressure [kPa]
double Xa_star; //Saturated vapor concentration [g/m3]
double e; //Air vapor pressure [kPa]
double Xa; //Air vapor concentration [g/m3]
double delta; //Slope of the relationship between the saturation vapour pressure and air temperature [kPa/degC]
double epsilon; //Vapour concentration
double Xs; //Vapour concentration at the canopy level [g/m3]
double q_sens_watt; //Sensible gain to air from vegetation [W/m^2]
double q_lat_watt; //Latent gain to air from vegetation [W/m^2]
double T_s_final;
double res;

//-----------------------------------------------------------------------------------------------------------------------
//Check the Inputs for Problems (//,ErrorType,Text)
//-----------------------------------------------------------------------------------------------------------------------

if((Afv < 0) or (Afv > 10)){
  throw std::invalid_argument(string("Fatal ") + string("The Cultivated fraction area of floor must be between 0 and 10 (1000%)"));
 }
if(rho_a < 0.){
  throw std::invalid_argument(string("Fatal ") + string("The air density must greater than 0"));
}

if(c_p < 0.){
  throw std::invalid_argument(string("Fatal ") + string("The air specific heat capacity must greater than 0"));
}

if(lambda < 0.) {
  throw std::invalid_argument(string("Fatal ") + string("The latent heat vapor of water must be greater than 0"));
}

if((rho_v < 0) or ( rho_v > 1)) {
  throw std::invalid_argument(string("Fatal ") + string("The lettuce leaf light reflectivity must be between 0 and 1"));
}

if((LED_eff < 0) or (LED_eff > 1)) {
  throw std::invalid_argument(string("Fatal ") + string("The LED efficiency must be between 0 and 1"));
}

if (T_a < -273.15){
  throw std::invalid_argument(string("Fatal ") + string("The input temperature is less than 0 K"));
}

if (P_LED < 0 ){
  throw std::invalid_argument(string("Fatal ") + string("The total power input has to be greater than 0"));
}

if ((RH > 100) or (RH < 0)){
  throw std::invalid_argument(string("Fatal ") + string("The relative humidity must be between 0 and 100"));
}

if (A_gr < 0){
  throw std::invalid_argument(string("Fatal ") + string("The agriculture space area have to be greater than 0"));
}

//-----------------------------------------------------------------------------------------------------------------------
//    *** PERFORM ALL THE CALCULATION HERE FOR THIS MODEL. ***
//-----------------------------------------------------------------------------------------------------------------------

PPFD = P_LED*LED_eff*5;
I_light = LED_eff*P_LED;
Rnet = (1-rho_v)*I_light*CAC;
q_loss= I_light - Rnet;
//

r_a = 100;
r_s = 60*(1500+PPFD)/(200+PPFD);

T_s = T_a-2;
res = 1; //To get in the loop (initial value)

while (res>0.0001){
    T_s=T_s+0.0001;
    e_star = 0.611*exp(17.4*T_a/(T_a+239));
    Xa_star = rho_a*c_p/lambda/(gamma/1000)*e_star*1000; //[g/m^3]
    e = RH/100*e_star;
    Xa = 7.4*e; //[g/m^3]
    delta = 0.04145*pow(e,(0.06088*T_a));
    epsilon = delta/(gamma/1000);
    Xs = Xa_star+rho_a*1000*c_p/lambda*epsilon*(T_s-T_a);
    q_lat_watt = LAI*lambda*(Xs-Xa)/(r_s+r_a); //W/m^2
    q_sens_watt = LAI*rho_a*c_p*(T_s-T_a)/r_a*1000;
    res = Rnet-q_sens_watt-q_lat_watt;
}

//-----------------------------------------------------------------------------------------------------------------------
// Convert outputs units
//-----------------------------------------------------------------------------------------------------------------------

q_sens = q_sens_watt * 3.6 * (A_gr*Afv);// Convert W/m^2 to kJ/hr
q_lat = q_lat_watt * 3.6 / lambda * (A_gr*Afv);

out << to_string(LAI) + string(",")+ to_string(CAC)+string(",")+ to_string(q_sens) + string(",") + to_string(q_lat)  << std::endl ;

}
out.close();
return 0;
}
