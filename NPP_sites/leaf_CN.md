
## Statistical models for carbon allocation and N cycle
### Author: Yunke Peng

###### The data has provided a large global dataset of leaf C/N, LMA, Vcmax25, which was extracted from Peng et al. 2020 submitted to GCB. It has inlcluded 89 sites and 997 samples.

###### The original reference of dataset included: 
* Smith et al. 2019 Ecology Letters (non-public; authored by Nick Smith)
* Field measurement data in Gongga (Xu et al. 2020 unpublished; authored by Wang Han)
* TROBIT database (non-public; authored by Jon Lloyd and Thomas Domingues)
* Cernusak et al. 2011 Agricultural and Forest Meteorology (non-public)
* Wang et al. 2018 Ecology (public available)

#### Variables
* final_species: Genus species 
* Vcmax.25 (umol/m2/s): Maximum rate of carboxylation capacity at 25 degree celcius
* Vcmax.Tg (umol/m2/s): Maximum rate of carboxylation capacity measured at leaf temperature or growth tempearture
* c%: leaf C percentage
* n%: leaf N percentage
* cn (unitless): leaf carbon to nitrogen ratio
* lma (g/m2): leaf mass-per-area
* narea (g/m2): leaf nitrogen-per-area
* parea (g/m2): leaf phosphorus-per-area
* tleaf (degree celcius): leaf temperature
* Source: Original sources
* species/species2: Some data are "Genus species" contained in one column, while some are contained in two columns. They were keeping consistent in first step of relevant analysis in Rmd.

###### Some outliers were excluded in advance, see Rmd code.