Authors : Marie-Hélène Talbot & Danielle Monfet ETS Montreal

This project includes crop models in folders that were used in various studied that were published by members of LTSB. The crop models are TRNSYS components that can be use to estimate the latent heat gain and convective heat gain/loss induced by crops. It can be used as inputs to the Type56 Multizone Building to include crops as internal heat gains/losses to a thermal zone. The latest version also include a growth model to predict growth and heat gain/loss from crops that increase as the crops growth.

DESCRIPTION OF THE CROP (LETTUCE) DYNAMIC MODEL WITH AN INTEGRATED GROWTH MODEL
-----------------------------------
It is a dynamic model that can predict both the heat gain/loss from crops and crop yield. 
It models lettuces growing inside a controlled environnement in a hydroponic production system according to a validated algorithm for the energy balance (Graamans et al., 2017) and a calibrated algorithm for the growth sub-model based on Van Henten (1994) model. Some modifications were added to the energy balance to improve the model's versatility.Regressions based on the calibrated values for (9) sets of conditions are available in TRNSYS through the .tpf file in the description of the variables. The (9) sets of conditions are the combinations of (3) temperatures - 20, 24 and 28°C and (3) lighting intensities - 200, 400, 750 μmol⋅m<sup>-2</sup>⋅s<sup>-1</sup>.

CITATION
-----------------------------------
Please cite when using the model :

Talbot, M.-H., & Monfet, D. (2024). Development of a crop growth model for the energy analysis of controlled agriculture environment spaces. Biosystems Engineering, 238, 38-50. doi:https://doi.org/10.1016/j.biosystemseng.2023.12.012
