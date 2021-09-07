Author : Marie-Hélène Talbot & Danielle Monfet

ETS Montreal

This project includes crop models in folders that were used in various studied that were published by members of LTSB.
The crop models are TRNSYS components that can be use to estimate the latent heat gain and convective heat gain/loss induced by crops. 
It can be used as inputs to the Type56 Multizone Building to include crops as internal heat gains/losses in a thermal zone.

-----------------------------------
DESCRIPTION OF THE CROP (LETTUCE) DYNAMIC MODEL
-----------------------------------
- It models lettuces growing inside a controlled environnement in a hydroponic production system according to a validated algorithm (Graamans et al., 2017).
- It is a dynamic model since the size of crops may vary with the time; thus the Leaf Area Index (LAI) and the cultivation is set as an input in this version of the crop model.
- This version of crop model can only be used in a space where LED lamps are used to grow crops. 
- A text file can be used to consider LAI and CAC varying with time. As an example, a text file (Example_LAI_CAC.txt) is available on GitHub:
        * First column: LAI calculated according to Shimizu et al. (2018)
        * Second column : CAC calculated according to Tei, Scaife, and Aikman (1996)


-----------------------------------
CITATION
-----------------------------------
Please cite when using the model :

<i> Talbot, M.H. and D. Monfet. 2021. Estimated energy demand and sensible heat ratio of a controlled-environment agriculture space for a growth cycle. ASHRAE Transactions 127(2):??-??. [In press] </i> 
