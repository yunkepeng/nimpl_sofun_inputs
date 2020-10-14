
## Statistical models for carbon allocation and N cycle
### Author: Yunke Peng

###### The data has provided a global dataset of carbon allocations (at different compartments), plant functional traits, soil traits and climate variables. It includes 672 samples, 200 sites overall..

###### The original reference of dataset included: 
* Terra A-P project database (organized by Dr. Sara Vicca and updated by Dr. Keith Bloomfield; non-public)
* Malhi et al. 2011 Phil. Trans. R. Soc. B. (public available)
* Malhi et al. 2017 New Phytologist (public available)
* ForC dataset -  https://github.com/forc-db/ForC (public available).

###### Pre-processing: In NPP/GPP analysis, there was one sample (see site: TRU08) indicating higher TNPP_1 (total NPP) than GPP, which is not reasonable. This sample was therefore considered as NA in this analysis. 

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
* lma_original_final (g/m2): Measured leaf-mass-per-area

#### Preidiction field
* alpha (/): The ratio of actual evaportranspiration transport (AET) to potential evaportranspiration transport (PET) derived from SPLASH
* fAPAR2 (/): fAPAR obtained from MODIS data 
* finalfAPAR (/): fAPAR primarily derived from observedfAPAR and alternatively from fAPAR2
* vpd (kPa): vapor-pressure-deficient, derived from CRU ts 4.01 at growth season in measurement year.
* Tg (°C): Growth-temperature, derived from CRU ts 4.01 (tmx, tmin) at growth season in measurement year
* PPFD (umol/m2/s): Time-integrated monthly instantaneous PPFD, averaged over the 24-hour period at growth season in measurement year
* PPFD2 (umol/m2/s): Total values of PPFD in the whole year
* new_ppfd (umol/m2/s): PPFD derived from measured radiation data at local climate station.
* cwd (mm/month): cumulative water deficit derived from 20 years dataset (Assisted by Dr. Beni Stocker).
* Forest (characteristics): Plant functional types (PFTs) derived from MODIS.
* lma_3km_v1 (g/m2): leaf-mass-per-area derived from Global Traits Map
* LPC_3km_mgg (mg/g): leaf P per mass derived from Global Traits Map
* LNC_3km_mgg (mg/g): leaf N per mass derived from Global Traits Map
* lma_final (g/m2): leaf-mass-per-area primarily from mesurement and alternatively from Global Traits Map.

#### Measured other traits
* CN (/): Measured soil C per mass in relative to soil N per mass
* pH (/): Measured soil pH
* management (/): Management types (M: Managed, UM: unmanaged, RD: Recently disturbed, FI: Fertilized, NA: unknown)
* agee (yrs): Measured stand-age 



#### Furthermore, the data has provided a large global dataset of leaf C/N, LMA, Vcmax25, which was extracted from Peng et al. 2020 submitted to GCB. It has inlcluded 89 sites and 997 samples.The original reference of dataset included: 
* Smith et al. 2019 Ecology Letters (non-public; authored by Nick Smith)
* Field measurement data in Gongga (Xu et al. 2020 unpublished; authored by Wang Han)
* TROBIT database (non-public; authored by Jon Lloyd and Thomas Domingues)
* Cernusak et al. 2011 Agricultural and Forest Meteorology (non-public)
* Wang et al. 2018 Ecology (public available)

#### Variables included:
* final_species: Genus species 
* Vcmax.25 (umol/m2/s): Maximum rate of carboxylation capacity
* cn (unitless): leaf carbon to nitrogen ratio
* lma (g/m2): leaf mass-per-area
