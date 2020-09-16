
## Statistical models for carbon allocation and N cycle
### Author: Yunke Peng

###### The data has provided a global dataset of carbon allocations (at different compartments), plant functional traits, soil traits and climate variables. It includes 672 samples, 200 sites overall. The variable name and unit below, are relevant to these four csv, beacuse we used these 4 csv as a start of NPP statistical model analysis.
* NPP_SaraVicca <- read.csv(file="~/data/NPP_Yunke/NPP_SaraVicca/NPP_SaraVicca.csv")
* NPP_Malhi <- read.csv(file="~/data/NPP_Yunke/NPP_Malhi/NPP_Malhi.csv")
* NPP_Keith <- read.csv(file="~/data/NPP_Yunke/NPP_Keith/NPP_Keith.csv")
* NPP_Forc <- read.csv(file="~/data/NPP_Yunke/NPP_ForC/NPP_ForC.csv")

###### within ~/data/NPP_Yunke, NPP_statistical_model.Rmd shows how NPP/GPP, ANPP/GPP, ANPP-foliage/ANPP was predicted, based on input site-based carbon allocation, climate and soil data. Then, for subfiles: climate/ included all necessary climate input of samples, as predicted from geographically weighted regressions in R. NPP_Forc/; NPP_Keith/; NPP_Malhi/; NPP_Saravicca/ included 4 NPP original data sources. Within each of them, it has included orig/ that stores original data, and also included code or readme detailing how original data was restructered or subsetted, before the start-up of NPP model. Then, four csv files above, are the dataframe that prepared as a start-up of NPP models.

###### The original reference of dataset included: 
* Terra A-P project database (organized by Dr. Sara Vicca and updated by Dr. Keith Bloomfield; non-public)
* Malhi et al. 2011 Phil. Trans. R. Soc. B. (public available)
* Malhi et al. 2017 New Phytologist (public available)
* ForC dataset -  https://github.com/forc-db/ForC (public available)

### Variables
#### Measured biomass production
* Foliar.biomass (gC/m2): biomass production within foliage (leaf)
* Branch.biomass (gC/m2) : biomass production within branch
* Stem.biomass (gC/m2) : biomass production within stem
* Stump.biomass (gC/m2): biomass production within stump
* Wood.biomass (gC/m2): biomass production within wood (branch + stem)
* Coarse.root.biomass (gC/m2): biomass production within corase-root
* Fine.root.biomass (gC/m2): biomass production within fine-root
* Total.aboveground.biomass(gC/m2): / biomass production 
* Total.belowground.biomass(gC/m2): belowground biomass production (coarse-root + fine-root)

#### Measured carbon flux (NPP)
* NPP.stem (gC/m2/yr): Net-primary-production within stem
* NPP.foliage (gC/m2/yr) :  Net-primary-production within foliage
* ANPP_1 (gC/m2/yr):  Net-primary-production within stem and foliage (NPP.stem + NPP.foliage)
* NPP.branch (gC/m2/yr): Net-primary-production within branch
* NPP.wood (gC/m2/yr): Net-primary-production within wood (NPP.stem + NPP.branch)
* ANPP_2 (gC/m2/yr):  Net-primary-production within stem, branch and foliage (NPP.stem + NPP.branch +NPP.foliage)
* NPP.coarse (gC/m2/yr): Net-primary-production within coarse-root
* NPP.fine (gC/m2/yr): Net-primary-production within fine-root
* BNPP_1 (gC/m2/yr): Net-primary-production in belowground (NPP.coarse + NPP.fine)
* TNPP_1 (gC/m2/yr): Total NPP_1 (ANPP_2 + BNPP_1)
* NPP.understory (gC/m2/yr): Net-primary-production within understory
* TNPP_2 (gC/m2/yr): Total NPP_2 (TNPP_1 + NPP.understory)
* NPP.reproduction (gC/m2/yr): Net-primary-production within reproduction compartment
* TNPP_3 (gC/m2/yr): Total NPP_3 (TNPP_2+ NPP.reproduction)
* NPP.herbivory (gC/m2/yr): Net-primary-production within herbivory compartment
* TNPP_4 (gC/m2/yr): Total NPP_4 (TNPP_3 + NPP.herbivory)
* NPP.exudation (gC/m2/yr): Net-primary-production within exudation compartment
* TNPP_5 (gC/m2/yr): Total NPP_5 (TNPP_4 + NPP.exudation)
* NPP.VOC (gC/m2/yr): Net-primary-production within volatile organic compounds 
* TNPP_6 (gC/m2/yr): Total NPP_6 (TNPP_5 + NPP.VOC)
* GPP: Measured Gross-primary production

#### Measured leaf traits
* LAI (/): Measured Leaf-area-index 
* observedfAPAR (/): Measured fAPAR (derived from measured LAI using Beer's law where k = 0.5)

#### Preidiction field
* alpha (/): The ratio of actual evaportranspiration transport (AET) to potential evaportranspiration transport (PET) derived from SPLASH
* vpd (kPa): vapor-pressure-deficient, derived from CRU ts 4.01 at growth season in measurement year.
* Tg (°C): Growth-temperature, derived from CRU ts 4.01 (tmx, tmin) at growth season in measurement year
* PPFD (umol/m2/s): Time-integrated monthly instantaneous PPFD, averaged over the 24-hour period at growth season in measurement year

#### Measured other traits
* CN (/): Measured soil C per mass in relative to soil N per mass
* pH (/): Measured soil pH
* management (/): Management types (M: Managed, UM: unmanaged, RD: Recently disturbed, FI: Fertilized, NA: unknown)
* age (yrs): Measured stand-age 