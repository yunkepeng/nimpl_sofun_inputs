rm(list=ls())
#library(ingestr)
library(dplyr)
library(tidyverse)  # depends
library(ncmeta)
library(viridis)
library(ggthemes)
library(LSD)
library(yardstick)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(gplots)
library(tidyselect)
library(extrafont)
library(rbeni)
library(raster)
library(spgwr)
library(maps)
library(rworldmap)
library(cowplot)


load(file = "~/yunkepeng/nimpl_sofun_inputs/forest/reprocessing_forest_site_simulation.Rdata")
#at the end of code...

# (1) pre-processing NPP
# rbind data (more data details see https://github.com/yunkepeng/nimpl_sofun_inputs/blob/master/NPP/NPP_statistical_model.Rmd)
NPP_SaraVicca <- read.csv(file="~/data/NPP_Yunke/NPP_SaraVicca/NPP_SaraVicca.csv")
NPP_Malhi <- read.csv(file="~/data/NPP_Yunke/NPP_Malhi/NPP_Malhi.csv")
NPP_Keith <- read.csv(file="~/data/NPP_Yunke/NPP_Keith/NPP_Keith.csv")
NPP_Forc <- read.csv(file="~/data/NPP_Yunke/NPP_Forc/NPP_Forc.csv")
NPP_Schulze <- read.csv(file="~/data/NPP_Yunke/NPP_Schulze/NPP_Schulze.csv")

NPP_all <- rbind(NPP_SaraVicca,NPP_Malhi,NPP_Keith,NPP_Forc,NPP_Schulze)

#add pft data derived from orginal data provided from Sara Vicca, and Schulze's book.
Evergreen<- read.csv(file="~/data/NPP_Yunke/NPP_SaraVicca/orig/pft.csv")
NPP_all2 <- merge(NPP_all,Evergreen,by=c("site"),all.x=TRUE)
NPP_all3 <- NPP_all2[,c("site","lon","lat","z","file","Begin_year","End_year",
                        "Source","GPP","TNPP_1","ANPP_2","BNPP_1","NPP.foliage","NPP.wood","pft")]

# (2) add data from Tian Di (pft = grassland for all data)
#firstly, clean our current data
Tiandi_df <- read.csv(file="~/data/npp_stoichiometry_grasslands_tiandi/npp_stoichiometry_china_grassland_CN_stoichiometry_with_matched_NPP_data_from_Prof_Fang_group_20201026.csv")
#as proved in Beni's ref, there is no big diff about lon_stoichmenistry and lon_npp, so we used the lon_npp because it is npp analyses now!
Tiandi_npp <- Tiandi_df[,c("Original_Site_Label_stoichiometry","Longitude_stoichiometry","Latitude_stoichiometry","Altitude_stoichiometry","Sample_time_NPP","Sample_time_NPP","TNPP","ANPP","ANPP","BNPP","CNratio_leaf","CNratio_root","CNratio_soil")]
#we will go back to c/n ratio later! It is NPP now only...
names(Tiandi_npp) <- c("site","lon","lat","z","Begin_year","End_year","TNPP_1","ANPP_2","NPP.foliage","BNPP_1","CN_leaf","CN_root","CN_soil")

#correct measurement year!
for (i in 1:nrow(Tiandi_npp)){
  if (is.na(Tiandi_npp$Begin_year[i]) == TRUE){ #if measruement year not available
    Tiandi_npp$Begin_year[i] <- 1991#convert to long-term
    Tiandi_npp$End_year[i] <- 2010 #convert to long-term 
  } else {
    Tiandi_npp$Begin_year[i] <- as.numeric(substr(Tiandi_npp$Begin_year[i], start = 1, stop = 4))
    Tiandi_npp$End_year[i] <- as.numeric(substr(Tiandi_npp$End_year[i], start = nchar(Tiandi_npp$End_year[i])-3, stop = nchar(Tiandi_npp$End_year[i]))) #1-4 or 6-9
  }
}

Tiandi_npp$Begin_year <- as.numeric(Tiandi_npp$Begin_year)
Tiandi_npp$End_year <- as.numeric(Tiandi_npp$End_year)
summary(Tiandi_npp$Begin_year)
summary(Tiandi_npp$End_year)
summary(Tiandi_npp)

# XXX Beni:
Tiandi_npp %>% 
  ggplot(aes(x = CN_leaf, y = ..count..)) +
  geom_histogram()

#XXX: These values are much lower than the ones in the Terra-P dataset. We discussed that this may be a real difference. To be sure, could you please check with Di Tian whether the data here is also in units of gC / gN?
#YYY: Yes, exactly. But I have already checked with Tian Di that the data is gC/gN. Not sure why it is smaller, will investigate their reasons further. 

# (3) add data from Campioli (pft = grassland for all data)
Cam_df <- read.csv(file="~/data/campioli/grasslands_MCampioli_20160111.csv")
#correct coordinates firstly
for (i in 1:nrow(Cam_df)){
  if (Cam_df$latitude_sign[i] == "S"){
    Cam_df$lat[i] <- -(Cam_df$latitude_value[i])
  } else {
    Cam_df$lat[i] <- Cam_df$latitude_value[i]
  }
  if (Cam_df$longitude_sign[i] == "W"){
    Cam_df$lon[i] <- -(Cam_df$longitude_value[i])
  } else {
    Cam_df$lon[i] <- Cam_df$longitude_value[i]
  }
}

Cam_npp <- Cam_df[,c("site","lon","lat","elevation","period_start","period_end","tnpp","anpp","anpp","bnpp","managment","biome")]
names(Cam_npp) <- c("site","lon","lat","z","Begin_year","End_year","TNPP_1","ANPP_2","NPP.foliage","BNPP_1","management_MCampioli","biome_MCampioli")

#rbind them and manually add some input
NPP_final <- dplyr::bind_rows(NPP_all3, Tiandi_npp,Cam_npp) 

NPP_final$file[685:1598] <- "Tiandi"
NPP_final$file[1599:1739] <- "MCampioli"

NPP_final$Source[685:1598] <- "Tiandi in Euler (Tibet dataset)"
NPP_final$Source[1599:1739] <- "MCampioli in Euler"

NPP_final$pft[685:1739] <- "Grassland"

NPP_final$lnf_obs <- NPP_final$NPP.foliage/NPP_final$CN_leaf
NPP_final$bnf_obs <- NPP_final$BNPP_1/NPP_final$CN_root

for (i in 1:nrow(NPP_final)){
  if (NPP_final$pft[i] == "Deciduous"|NPP_final$pft[i] == "Evergreen"|NPP_final$pft[i] == "Forest"|NPP_final$pft[i] == "Mixed"){
    NPP_final$pft2[i] <- "Forest"
  } else {
    NPP_final$pft2[i] <- "Grassland"
  }
}
summary(NPP_final)
dim(NPP_final)

#XXX Source information is missing for sites from Tian Di’s and Matteo’s datasets (1055 rows).
#YYY: Tiandi's dataset citation now in "~/data/npp_stoichiometry_grasslands_tiandi/README" AND "~/data/npp_stoichiometry_forests_tiandi/README.md"
#YYY: Matteo’s dataset's citation now in "~/data/campioli/README.md"
#YYY: NOT sure what is source information for each sites from Tian Di and Matteo's dataset --> I was just using their original data. I think it is safe to just use their referred citation suggested in the whole dataset.

#Finally, add more forest sites from corrected Sara Vicca’s dataset, including anpp, npp.leaf and npp.wood (but no measured environmental covariates available).

#XXX Please provide citation for this added dataset. How were duplicates with previously included data avoided? XXX File name seems odd.

#YYY: the citation and introduction of this added dataset is available in "~/data/NPP_Yunke/NPP_SaraVicca/orig/validation_data/README.md".
# For this additional validation data provided by Sara Vicca, it included more sites in anpp, leaf npp, wood npp and leaf C/N. She suggested that we can cite them as: Berner&Law 2016 https://www.nature.com/articles/sdata20162

#finally, add more forest sites from corrected Sara Vicca's dataset, including anpp, npp.leaf and npp.wood. (leafcn data will be included below)
Sara2_df <- read.csv(file="~/data/NPP_Yunke/NPP_SaraVicca/orig/validation_data/CORRECTIONS_CascadeHead_Andrews.csv")
Sara2_df2 <- subset(Sara2_df,Repeat=="no") #remove repeated data as inputted in NPP_SaraVicca
Sara2_NPP <- Sara2_df2[,c("LONGITUDE","LATITUDE","ELEVATION","YEAR","YEAR","AG_PROD_TREE_TOTAL_AS_CARBON","AG_PROD_TREE_FOLIAGE_AS_CARBON","AG_PROD_TREE_WOOD_AS_CARBON")]
names(Sara2_NPP) <- c("lon","lat","z","Begin_year","End_year","ANPP_2","NPP.foliage","NPP.wood")
Sara2_NPP$ANPP_2[Sara2_NPP$ANPP_2<=0] <- NA
Sara2_NPP$NPP.foliage[Sara2_NPP$NPP.foliage<=0] <- NA
Sara2_NPP$NPP.wood[Sara2_NPP$NPP.wood <=0] <- NA
Sara2_NPP$Source <- "Sara Vicca Validation data"
Sara2_NPP$file <- "~/data/NPP_Yunke/NPP_SaraVicca/orig/validation_data"
Sara2_NPP$pft<-"Forest"
Sara2_NPP$pft2<-"Forest"
summary(Sara2_NPP)

#create site name
Sara2_NPP_sitename <- aggregate(Sara2_NPP,by=list(Sara2_NPP$lon,Sara2_NPP$lat,Sara2_NPP$z), mean,na.rm=TRUE)
for (i in 1:nrow(Sara2_NPP_sitename)){
  Sara2_NPP_sitename$site[i] <- paste("Sara2_NPP",i,sep = "")
}

Sara2_NPP_sitename <- Sara2_NPP_sitename[,c("lon","lat","z","site")]

Sara2_NPP$no <- c(1:nrow(Sara2_NPP))

Sara2_NPP2 <- merge(Sara2_NPP,Sara2_NPP_sitename,by=c("lon","lat","z"),all.x=TRUE)

Sara2_NPP2 <- Sara2_NPP2[order(Sara2_NPP2$no), ]

Sara2_NPP2 <- Sara2_NPP2[,c(1:12,14)] #remove no

NPP_final2 <- dplyr::bind_rows(NPP_final, Sara2_NPP2) 
summary(NPP_final2)

###Now, merge leaf c/n from various sources

# (1) Add Schulz - unit: all in g/m2

#XXX is this m2 ground area? Please specify units in the README.
#YYY Done

#XXX Distinction between stem and branches appears quite important (very different C:N ratios!). Is this disinction made in other datasets? What does ‘wood’ represent in the other datasets?
#Yes, agree that stem and branch is quite important here with different C/N so I have included them both. For wood or stem C/N ratio, this is the ONLY dataset we hold.
CN_Schulz <- read.csv(file="~/data/NPP_Yunke/npp_cn/CN_Schulze.csv")
CN_Schulz2 <- CN_Schulz[,c(5,48:58)]

CN_Schulz2$CN_leaf_Schulz <- CN_Schulz2$c_leaf/CN_Schulz2$n_leaf
CN_Schulz2$CN_root_Schulz <- (CN_Schulz2$c_coarseroot+CN_Schulz2$c_fineroot)/CN_Schulz2$n_root
#???(CN_Schulz2$c_fineroot + CN_Schulz2$c_coarseroot) / CN_Schulz2$n_root

CN_Schulz2$CN_stem_Schulz <- CN_Schulz2$c_stem/CN_Schulz2$n_stem
CN_Schulz2$CN_wood_Schulz <- (CN_Schulz2$c_stem+CN_Schulz2$c_branch)/(CN_Schulz2$n_stem+CN_Schulz2$n_branch)

CN_Schulz2 <- CN_Schulz2[,c("site","CN_leaf_Schulz","CN_root_Schulz","CN_stem_Schulz","CN_wood_Schulz")]
CN_Schulz2

# (2) Add Malhi data - assume cmass as constant 0.48 g/g; narea in gm-2, lma in gm-2
CN_Malhi <- read.csv(file="~/data/NPP_Yunke/npp_cn/CN_Malhi.csv")

#No original data of cmass but we can assume cmass = 48%, because (1) it is consistent with what we find in mean values of ~/data/CN_leaf/final_leafCN.csv, equals to 47% and (2) see Enquist et al. 2017 https://onlinelibrary.wiley.com/doi/full/10.1111/geb.12645 - fig.2, overall the cmass was within a very small variance through this elevation transect, and we can just assume this value as 0.48!

#XXX information on this dataset lacking in README (~/data/NPP_Yunke/npp_cn/README.md). Original citation?
#YYY: Done

#XXX Why not use the value of cmass provided in that same dataset (0.4638)?
#YYY: Done. See below.  

CN_Malhi$CN_leaf_alt_malhi <- 0.4638/(CN_Malhi$narea/CN_Malhi$lma) # use CN_leaf_alt as a new and alternative variable, by storing data when Cmass or (c%) is lacking.

CN_Malhi <- na.omit(CN_Malhi)

newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-180, 180), ylim = c(-75, 75), asp = 1)
points(CN_Malhi$lon,CN_Malhi$lat, col="red", pch=16,cex=1)

CN_Malhi2 <- CN_Malhi[,c("site","CN_leaf_alt_malhi")]
CN_Malhi2

#(3) Add Species-based traits data, as provided from Sara Vicca, including around 40 forest sites that have species-based leaf c/n
#The data is c% and n%

#XXX add variable descriptions for file NACP_TERRA_PNW_leaf_trait.csv in the README, sitting in the same directory.
#YYY: Done

# XXX: Could you describe in words what is done by the chunk below and why?

# YYY: The step is (see comment below):
#(1) aggregate leaf C/N based on lon + lat, according to a leaf traits dataset provided by Sara Vicca. There are more than one 1 record in a site, beacuse it measured many trees (individuals) in one site. See their original csv.
CN_SaraVicca <- read.csv(file="~/data/NPP_Yunke/NPP_SaraVicca/orig/validation_data/NACP_TERRA_PNW_leaf_trait.csv")
CN_SaraVicca <- CN_SaraVicca[,c("LONGITUDE","LATITUDE","LEAF_CN")]
CN_SaraVicca$LEAF_CN[CN_SaraVicca$LEAF_CN<0] <- NA
CN_SaraVicca$LONGITUDE <- as.numeric(CN_SaraVicca$LONGITUDE)
CN_SaraVicca$LATITUDE <- as.numeric(CN_SaraVicca$LATITUDE)

CN_SaraVicca2 <- aggregate(CN_SaraVicca, by=list(CN_SaraVicca$LONGITUDE,CN_SaraVicca$LATITUDE), mean,na.rm=TRUE)
CN_SaraVicca2 <- CN_SaraVicca2[,c("LONGITUDE","LATITUDE","LEAF_CN")]
names(CN_SaraVicca2) <- c("lon","lat","CN_leaf_sara")
dim(CN_SaraVicca2)

#(2) Merge C/N to NPP dataset STRICTLY based on lon and lat
test <- merge(NPP_final2,CN_SaraVicca2,by=c("lon","lat"),all.x=TRUE)
dim(subset(test,CN_leaf_sara>0))
test1 <- subset(test,CN_leaf_sara>0)
test1 <- test1[,c("lon","lat","z","CN_leaf_sara")]
dim(test1)
test1 <- aggregate(test1, by=list(test1$lon,test1$lat,test1$z), mean,na.rm=TRUE)
dim(test1)
test1<- test1[,c("lon","lat","z","CN_leaf_sara")]


#(3) Merge C/N to NPP dataset based on lon and lat, but not strictly --> we assume that it can be best merged within 0.01 lon/lat resolution
# I would persist using this way because it could help to generate more leaf NPP * C/N sites, it would help us generate more validation sites
NPP_old <- NPP_final[1:1739,]
NPP_old$lon <- round(NPP_old$lon,2)
NPP_old$lat <- round(NPP_old$lat,2)

CN_SaraVicca3 <- CN_SaraVicca
names(CN_SaraVicca3) <- c("lon","lat","CN_SaraVicca_old")

CN_SaraVicca3$lon <- round(CN_SaraVicca3$lon,2)
CN_SaraVicca3$lat <- round(CN_SaraVicca3$lat,2)

CN_SaraVicca4 <- aggregate(CN_SaraVicca3, by=list(CN_SaraVicca3$lon,CN_SaraVicca3$lat), mean,na.rm=TRUE)
CN_SaraVicca4 <- CN_SaraVicca4[,c(3,4,5)]

test_new <- merge(NPP_old,CN_SaraVicca4,by=c("lon","lat"),all.x=TRUE) # merging with "old" Sara Vicca's dataset, as far lon and lat both agrees within 0.01 degree.
nrow(test_new)-nrow(NPP_old)

test2 <- subset(test_new,CN_SaraVicca_old>0)
test2 <- test2[,c("site","CN_SaraVicca_old")]
dim(test2)
test2 <- aggregate(test2, by=list(test2$site), mean,na.rm=TRUE)
dim(test2)
test2 <- test2[,c(1,3)]
names(test2) <- c("site","CN_SaraVicca_old")

#(5) Lastly, add Forc leaf, stem and root data (on site basis) - this should be after (3) and (4).
#But no data matched to current site.
#Forc <- read.csv(file="~/data/NPP_Yunke/NPP_ForC/orig/ForC_measurements_final.csv")
#Forc2 <- aggregate(mean~sites.sitename+Begin_year1+End_year1+variable.name,data=Forc,mean)
#variablelist <- read.csv(file="~/data/NPP_Yunke/NPP_ForC/orig/site_need.csv")
#variable <- as.character(variablelist$variable.name)
#mylist <- vector(mode = "list", length = 41)
#for (i in 1:41){mylist[[i]] <- subset(Forc2,variable.name==variable[i])}
##leaf_pC <- as.data.frame(mylist[21])
#leaf_C2N <- as.data.frame(mylist[22])
##leaf_pN <- as.data.frame(mylist[23])
##foliage_pN <- as.data.frame(mylist[24]) 
##leaf_pP <- as.data.frame(mylist[25]) 
##stem_pC <- as.data.frame(mylist[26]) 
##stem_pN <- as.data.frame(mylist[27])
##root_pN <- as.data.frame(mylist[28]) 
##root_pC <- as.data.frame(mylist[29]) 
#CN_Forc <- aggregate(leaf_C2N, by=list(leaf_C2N$sites.sitename), mean,na.rm=TRUE)
#CN_Forc2 <- CN_Forc[,c(1,6)]
#names(CN_Forc2) <- c("site","CN_leaf_Forc")
#CN_Forc2

#now, merge with NPP_final2 - 4 objects need to be merged, at this stage. 
NPP_final2_site <- NPP_final2[,c("lon","lat","z","site","CN_leaf","CN_root","pft2")]

NPP_final2_site$no <- 1:nrow(NPP_final2_site)

npp_cn1 <-Reduce(function(x,y) merge(x = x, y = y, by = c("site"),all.x=TRUE),
                 list(NPP_final2_site,CN_Schulz2,CN_Malhi2,test2))
nrow(npp_cn1) - nrow(NPP_final2_site)

npp_cn2 <-Reduce(function(x,y) merge(x = x, y = y, by = c("lon","lat","z"),all.x=TRUE),
                 list(npp_cn1,test1))

npp_cn2 <- npp_cn2[order(npp_cn2$no), ]
#one column have a issue when both test and test2 have data, remove test2
subset(npp_cn2,CN_leaf_sara>0 & CN_SaraVicca_old>0)$no
npp_cn2$CN_SaraVicca_old[npp_cn2$no==subset(npp_cn2,CN_leaf_sara>0 & CN_SaraVicca_old>0)$no] <- NA

summary(npp_cn2$lon - NPP_final2$lon)
summary(npp_cn2)
npp_cn2$CN_stem_final <- npp_cn2$CN_stem_Schulz
npp_cn2$CN_wood_final <- npp_cn2$CN_wood_Schulz

#now, it is the time to combine
npp_cn3 <- npp_cn2 %>% mutate(CN_leaf_final = coalesce(CN_leaf,CN_leaf_Schulz,CN_leaf_alt_malhi,CN_SaraVicca_old,CN_leaf_sara)) %>% # including additional data by secondry C/N merging by Sara
  mutate(CN_leaf_near_final = coalesce(CN_leaf,CN_leaf_Schulz,CN_leaf_alt_malhi,CN_SaraVicca_old,CN_leaf_sara)) %>% # ignore this object
  mutate(CN_leaf_org = coalesce(CN_leaf,CN_leaf_Schulz,CN_leaf_alt_malhi,CN_leaf_sara)) %>% #NOT including additional data by secondry C/N merging by Sara
  mutate(CN_root_final = coalesce(CN_root,CN_root_Schulz))

summary(npp_cn3$CN_leaf_org)
summary(npp_cn3$CN_leaf_final) # it would help us generate 37 more sites

npp_cn4 <- npp_cn3[,c("CN_stem_final","CN_wood_final","CN_leaf_final","CN_leaf_near_final","CN_leaf_org","CN_root_final")]
hist(npp_cn4$CN_leaf_final)
hist(npp_cn4$CN_stem_final)
hist(npp_cn4$CN_wood_final)
hist(npp_cn4$CN_root_final)

NPP_final3 <- cbind(NPP_final2,npp_cn4)

NPP_final3$lnf_obs_final <-NPP_final3$NPP.foliage/NPP_final3$CN_leaf_final
NPP_final3$lnf_obs_nearfinal <-NPP_final3$NPP.foliage/NPP_final3$CN_leaf_near_final
NPP_final3$lnf_obs_org <-NPP_final3$NPP.foliage/NPP_final3$CN_leaf_org
NPP_final3$bnf_obs_final  <- NPP_final3$BNPP_1/NPP_final3$CN_root_final
NPP_final3$wnf_obs_final  <- NPP_final3$NPP.wood/NPP_final3$CN_wood_final

summary(NPP_final3) 
#Important!!!Input repeated data info
rep_info <- read.csv("~/data/NPP_Yunke/NPP_final_rep.csv")
summary(rep_info$lat-NPP_final3$lat)
summary(rep_info$lon-NPP_final3$lon)
summary(rep_info$z-NPP_final3$z)
summary(rep_info$GPP-NPP_final3$GPP)

NPP_final3$rep_info <- rep_info$rep_info

####Finally, add Tiandi's latest forest data (the last data we need!)
tiandi_forest <- read.csv(file="~/data/npp_stoichiometry_forests_tiandi/Site_level_forest_CN_NPP_China_TD_20210104_for_Beni_Yunke.csv")
tiandi_forest <- tiandi_forest[,c(1,2,3,4,11,14,17,18,19,20,21,22)]
head(tiandi_forest)
names(tiandi_forest) <- c("site","lon","lat","z","CN_leaf_final","CN_stem_final","CN_root_final","TNPP_1","ANPP_2",
                          "NPP.foliage","NPP.wood","BNPP_1")

#extend CN_leaf version, as classified in NPP_final3
tiandi_forest$CN_leaf <- tiandi_forest$CN_leaf_final
tiandi_forest$CN_leaf_near_final <- tiandi_forest$CN_leaf_final
tiandi_forest$CN_leaf_org <- tiandi_forest$CN_leaf_final
tiandi_forest$CN_root <- tiandi_forest$CN_root_final


#convert unit from tC/ha/yr to gC/m2/yr --> *100
tiandi_forest$TNPP_1 <- 100*tiandi_forest$TNPP_1
tiandi_forest$ANPP_2 <- 100*tiandi_forest$ANPP_2
tiandi_forest$NPP.foliage <- 100*tiandi_forest$NPP.foliage
tiandi_forest$NPP.wood <- 100*tiandi_forest$NPP.wood
tiandi_forest$BNPP_1 <- 100*tiandi_forest$BNPP_1
tiandi_forest$file <- "~/data/npp_stoichiometry_forests_tiandi/"
tiandi_forest$Begin_year <- 2006
tiandi_forest$End_year <- 2015
tiandi_forest$Source <- "Fang, Wang, Tian Di prepared forest data in China"
tiandi_forest$pft <- "Forest"
tiandi_forest$pft2 <- "Forest"

tiandi_forest$lnf_obs_final <-tiandi_forest$NPP.foliage/tiandi_forest$CN_leaf_final 
tiandi_forest$lnf_obs_nearfinal <-tiandi_forest$NPP.foliage/tiandi_forest$CN_leaf_final
tiandi_forest$lnf_obs_org <-tiandi_forest$NPP.foliage/tiandi_forest$CN_leaf_final  
tiandi_forest$bnf_obs_final  <- tiandi_forest$BNPP_1/tiandi_forest$CN_root_final
tiandi_forest$wnf_obs_final  <- tiandi_forest$NPP.wood/tiandi_forest$CN_stem_final #assume stem ratio as wood ratio here?

NPP_final4 <- dplyr::bind_rows(NPP_final3, tiandi_forest) 

NPP <- NPP_final4


###used for site simulation of grassland npp, including use ingestr for fapar and climate forcing, and rsofun (see relevant contents in grassland/)
NPP_grassland <- subset(NPP,pft2=="Grassland")
dim(NPP_grassland)
#csvfile <- paste("~/data/grassland_npp/NPP_grassland.csv")
#write_csv(NPP_grassland, path = csvfile)

###used for site simulation of forest npp, including use ingestr for fapar and climate forcing, and rsofun (directly see below!!!)
NPP_Forest <- subset(NPP,pft2=="Forest")
dim(NPP_Forest)

#before output it as csv - correct it firstly - for some flipped lon/lat sites in Malhi et al. 2011
NPP_Forest_corrected <- NPP_Forest

NPP_Forest_corrected$lon[NPP_Forest$site=="BCI Plateau, Panama?"] <- NPP_Forest$lat[NPP_Forest$site=="BCI Plateau, Panama?"] ; NPP_Forest_corrected$lat[NPP_Forest$site=="BCI Plateau, Panama?"] <- NPP_Forest$lon[NPP_Forest$site=="BCI Plateau, Panama?"]
NPP_Forest_corrected$lon[NPP_Forest$site=="BDFFP Fazenda"] <- NPP_Forest$lat[NPP_Forest$site=="BDFFP Fazenda"] ; NPP_Forest_corrected$lat[NPP_Forest$site=="BDFFP Fazenda"] <- NPP_Forest$lon[NPP_Forest$site=="BDFFP Fazenda"]
NPP_Forest_corrected$lon[NPP_Forest$site=="Bionte, Brazil"] <- NPP_Forest$lat[NPP_Forest$site=="Bionte, Brazil"] ; NPP_Forest_corrected$lat[NPP_Forest$site=="Bionte, Brazil"] <- NPP_Forest$lon[NPP_Forest$site=="Bionte, Brazil"]
NPP_Forest_corrected$lon[NPP_Forest$site=="Mocambo, Brazil"] <- NPP_Forest$lat[NPP_Forest$site=="Mocambo, Brazil"] ; NPP_Forest_corrected$lat[NPP_Forest$site=="Mocambo, Brazil"] <- NPP_Forest$lon[NPP_Forest$site=="Mocambo, Brazil"]
NPP_Forest_corrected$lon[NPP_Forest$site== "San Carlos caatinga"] <- NPP_Forest$lat[NPP_Forest$site== "San Carlos caatinga"] ; NPP_Forest_corrected$lat[NPP_Forest$site== "San Carlos caatinga"] <- NPP_Forest$lon[NPP_Forest$site== "San Carlos caatinga"]
NPP_Forest_corrected$lon[NPP_Forest$site== "San Carlos terra firme"] <- NPP_Forest$lat[NPP_Forest$site== "San Carlos terra firme"] ; NPP_Forest_corrected$lat[NPP_Forest$site== "San Carlos terra firme"] <- NPP_Forest$lon[NPP_Forest$site== "San Carlos terra firme"]
NPP_Forest_corrected$lon[NPP_Forest$site=="Tapajo?s, Brazil"][1] <- NPP_Forest$lat[NPP_Forest$site=="Tapajo?s, Brazil"][1] ; NPP_Forest_corrected$lat[NPP_Forest$site=="Tapajo?s, Brazil"][1] <- NPP_Forest$lon[NPP_Forest$site=="Tapajo?s, Brazil"][1]

#see the difference - now changed
plot(newmap, xlim = c(-180, 180), ylim = c(-75, 75), asp = 1)
points(NPP_Forest$lon,NPP_Forest$lat, col="red", pch=16,cex=1)
points(NPP_Forest_corrected$lon,NPP_Forest_corrected$lat, col="blue", pch=16,cex=1) 

NPP_Forest <- NPP_Forest_corrected

#csvfile <- paste("~/data/forest_npp/NPP_Forest_corrected_Malhi_coord.csv")
#write_csv(NPP_Forest_corrected, path = csvfile)

########add sitename (same in current df) and sitename_fapar (lon + lat), which will be used in rsofun
#for info about how to create sitename_fapar, check: /Users/yunpeng/yunkepeng/nimpl_sofun_inputs/forest/Reprocessing_fpar_climates_forest.R

NPP_final2 <- read.csv("~/data/forest_npp/NPP_Forest_corrected_Malhi_coord.csv")
#pass some sitename and sitename2 data
NPP_Forest$sitename <- NPP_final2$sitename
NPP_Forest$sitename_fpar <- NPP_final2$sitename_fpar

#check two df is consistent..
#summary(NPP_Forest$lat-NPP_final2$lat)
#summary(NPP_Forest$lon-NPP_final2$lon)
#summary(NPP_Forest$z-NPP_final2$z)
#summary(NPP_Forest$Begin_year-NPP_final2$Begin_year)
#summary(NPP_Forest$End_year-NPP_final2$End_year)
#summary(NPP_Forest$GPP-NPP_final2$GPP)
#summary(NPP_Forest$TNPP_1 - NPP_final2$TNPP_1)
#summary(NPP_Forest$ANPP_2 - NPP_final2$ANPP_2)
#summary(NPP_Forest$NPP.foliage - NPP_final2$NPP.foliage)

#before start everything - convert the measurement year before 1980 to 1980-1989 (which is consistent with what we set in climate forcing), so that we can run them sucessfully in rsofun later on.
NPP_Forest$year_start <-NPP_Forest$Begin_year
NPP_Forest$year_end <-NPP_Forest$End_year

NPP_Forest$year_start[NPP_Forest$Begin_year<=1980] <- 1980
NPP_Forest$year_end[NPP_Forest$End_year<=1980] <- 1989

####now, input forcing data from two times simulation
forcing_df <- list.files("~/data/forest_npp/reprocessing_climates/",full.names = T)
length(forcing_df)

fapar_df <- list.files("~/data/forest_npp/reprocessing_fpar/",full.names = T)
length(fapar_df)

fapar_org_df <- list.files("~/data/forest_npp/reprocessing_fpar_raw/",full.names = T)
length(fapar_org_df)

#1. fapar - input

#1. fpar - check missing data - and also, input all years fapar (2001-2015), which will be selected in measurement year only later on 
for (i in 1:length(fapar_df)){
  df1 <- read.csv(fapar_df[i])
  df1$date <- as.Date(df1$date)
  df1 <- df1[!(format(df1$date,"%m") == "02" & format(df1$date, "%d") == "29"), , drop = FALSE]
  df2 <- df1[,c("date","modisvar_filled")]
  assign(substr(sub('.*daily_', '', fapar_df[i]),1,nchar(sub('.*daily_', '', fapar_df[i]))-4), df2) 
}

#check fapar missing data
for (i in 1:nrow(NPP_Forest)){
  NPP_Forest$forcing_avil[i] <- exists(paste(NPP_Forest$sitename_fpar[i]))
}

na_fapar <- (subset(NPP_Forest,forcing_avil=="FALSE" ))
dim(na_fapar)
na_fapar$sitename

library(rworldmap)
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-180, 180), ylim = c(-75, 75), asp = 1)
points(na_fapar$lon,na_fapar$lat, col="red", pch=16,cex=1)
#3 samples were missing fapar in the edge, which was expected.

#check climate forcing missing data
empty_vec <- c()

#check existed climate files
for (i in 1:(length(forcing_df))){
  empty_vec[i] <- as.numeric(gsub("[^0-9]", "",  forcing_df[i]))
}

diff <- setdiff(1:935, empty_vec)
diff
NPP_Forest[diff,]

points(NPP_Forest$lon[338],NPP_Forest$lat[338], col="blue", pch=16,cex=1)
points(NPP_Forest$lon[390],NPP_Forest$lat[390], col="blue", pch=16,cex=1)

#totally 5 sites were missing:
na_fapar$sitename
NPP_Forest[diff,]$sitename
all_na_points <- c(na_fapar$sitename,NPP_Forest[diff,]$sitename)
#NPP_F556, NPP_F697, NPP_F700 (due to fapar) and NPP_F338, NPP_F390 (due to climate forcing)


#2. forcing - combing fapar and climates into a df.
for (i in 1:length(forcing_df)){
  df1 <- read.csv(forcing_df[i])
  df1$date <- as.Date(df1$date)
  
  sitename_climate <- subset(NPP_Forest,NPP_Forest$sitename == df1$sitename[1])$sitename
  sitename_fapar <- subset(NPP_Forest,NPP_Forest$sitename == df1$sitename[1])$sitename_fpar
  # 5 points were missing - let's clarify them firstly
  if (sitename_climate %in% all_na_points){
    print (sitename_climate)
    print ("this site is not available")
  } else {
    fapar <- (eval(parse(text=sitename_fapar)))
    fapar$Year <- year(fapar$date)
    
    yr_start <- subset(NPP_Forest,NPP_Forest$sitename == df1$sitename[1])$year_start
    yr_end <- subset(NPP_Forest,NPP_Forest$sitename == df1$sitename[1])$year_end
    
    if (yr_start<=2002) { # if measurement year before 2002 for a certain site --> calculating 2003-2012 average of MCD15A3H (since this product only available after the 2003, as entire year)
      df1a <- fapar[fapar$date >= "2003-01-01" & fapar$date <= "2012-12-31",c("date","modisvar_filled")]
      df1b <- df1a %>% mutate(ymonth = month(date),
                              yday = day(date)) %>% 
        group_by(ymonth, yday) %>% 
        summarise(fpar = mean(modisvar_filled, na.rm = TRUE))
      df1b <- as.data.frame(df1b)[,3] # averaged fapar from 2003 - 2012 (365 length of data)
      df2 <- rep(df1b,(yr_end- yr_start+1)) # repeated it to multiple years, the number of years is consitent to what we collect climate forcing
    } else { # if measurement year bewteen 2003 and 2015 --> use such years directly where consistent with climate forcing
      df1a <- subset(fapar,Year>=yr_start & Year<=yr_end)
      df2 <- df1a$modisvar_filled }
    
    fpar <- df2
    
    df3 <- cbind(df1,fpar)
    df3 <- df3[,c("date","temp","prec","rain","snow","vpd","ppfd","patm","ccov_int","ccov","fpar","co2")]
    names(df3)[names(df3) == 'rain'] <- 'rainf'
    names(df3)[names(df3) == 'snow'] <- 'snowf'
    names(df3)[names(df3) == 'fpar'] <- 'fapar'
    assign(paste("final",df1$sitename[1],sep="_"), as_tibble(df3))
  }
}



#3. rsofun to predict gpp
df_soiltexture <- bind_rows(
  top    = tibble(layer = "top",    fsand = 0.4, fclay = 0.3, forg = 0.1, fgravel = 0.1),
  bottom = tibble(layer = "bottom", fsand = 0.4, fclay = 0.3, forg = 0.1, fgravel = 0.1))
params_modl <- list(
  kphio           = 0.09423773,
  soilm_par_a     = 0.33349283,
  soilm_par_b     = 1.45602286)

NPP_Forest$whc = 170

NPP_Forest$pred_gpp_c3 <- NA
#NPP_Forest$pred_gpp_c4 <- NA
NPP_Forest$max_vcmax25_c3 <- NA
#NPP_Forest$max_vcmax25_c4 <- NA


#using rsofun
for (i in 1:nrow(NPP_Forest)) {
  tryCatch({
    #c3
    forcing <- (eval(parse(text=(paste("final",NPP_Forest$sitename[i],sep="_")))))
    modlist <- run_pmodel_f_bysite( 
      NPP_Forest$sitename[i], 
      params_siml <- list(
        spinup             = TRUE,
        spinupyears        = 10,
        recycle            = 1,
        soilmstress        = TRUE,
        tempstress         = TRUE,
        calc_aet_fapar_vpd = FALSE,
        in_ppfd            = TRUE,
        in_netrad          = FALSE,
        outdt              = 1,
        ltre               = FALSE,
        ltne               = FALSE,
        ltrd               = FALSE,
        ltnd               = FALSE,
        lgr3               = TRUE,
        lgn3               = FALSE,
        lgr4               = FALSE,
        firstyeartrend = NPP_Forest$year_start[i],
        nyeartrend = NPP_Forest$year_end[i]-NPP_Forest$year_start[i]+1), 
      siteinfo = NPP_Forest[i,], 
      forcing, 
      df_soiltexture, 
      params_modl = params_modl, 
      makecheck = TRUE)
    
    pred_gpp_list <- modlist %>% mutate(ymonth = month(date),yday = day(date)) %>% group_by(ymonth, yday) %>% summarise(gpp = mean(gpp, na.rm = TRUE))
    max_vcmax25 <- max(modlist$vcmax25)*1000000
    
    NPP_Forest[i,c("pred_gpp_c3")] <- sum(pred_gpp_list$gpp)
    NPP_Forest[i,c("max_vcmax25_c3")] <- max_vcmax25
  }, error=function(e){})} 

#this sites have no gpp data - must because their fapar in n_focal = 0, we need to fill them by alternatively applying n_focal = 1, then 2...
subset(NPP_Forest,pred_gpp_c3=="NaN")$sitename_fpar

#we have newly interpolate their fapar primarily based on n_focal = 1, then n_focal = 2, and saved it in "/Users/yunpeng/data/forest_npp/reprocessing_fpar_raw/"
#the code of this is available at L90-110 in forest/Reprocessing_fpar_climates_forest.R

#now, reprocessing such values - by updating such fapar 
fapar_df_new <- list.files("~/data/forest_npp/reprocessing_fpar_raw/",full.names = T)

for (i in 1:(length(fapar_df_new)-1)){
  df1 <- read.csv(fapar_df_new[i])
  df1$date <- as.Date(df1$date)
  df1 <- df1[!(format(df1$date,"%m") == "02" & format(df1$date, "%d") == "29"), , drop = FALSE]
  df2 <- df1[,c("date","modisvar_filled")]
  assign(substr(sub('.*daily_', '', fapar_df_new[i]),1,nchar(sub('.*daily_', '', fapar_df_new[i]))-4), df2) 
}


#2. forcing - combing fapar and climates into a dataframe.
for (i in 1:length(forcing_df)){
  df1 <- read.csv(forcing_df[i])
  df1$date <- as.Date(df1$date)
  
  sitename_climate <- subset(NPP_Forest,NPP_Forest$sitename == df1$sitename[1])$sitename
  sitename_fapar <- subset(NPP_Forest,NPP_Forest$sitename == df1$sitename[1])$sitename_fpar
  # 5 points were missing - let's clarify them firstly
  if (sitename_climate %in% all_na_points){
    print (sitename_climate)
    print ("this site is not available")
  } else {
    fapar <- (eval(parse(text=sitename_fapar)))
    fapar$Year <- year(fapar$date)
    
    yr_start <- subset(NPP_Forest,NPP_Forest$sitename == df1$sitename[1])$year_start
    yr_end <- subset(NPP_Forest,NPP_Forest$sitename == df1$sitename[1])$year_end
    
    if (yr_start<=2002) { # if measurement year before 2002 for a certain site --> calculating 2003-2012 average of MCD15A3H (since this product only available after the 2003, as entire year)
      df1a <- fapar[fapar$date >= "2003-01-01" & fapar$date <= "2012-12-31",c("date","modisvar_filled")]
      df1b <- df1a %>% mutate(ymonth = month(date),
                              yday = day(date)) %>% 
        group_by(ymonth, yday) %>% 
        summarise(fpar = mean(modisvar_filled, na.rm = TRUE))
      df1b <- as.data.frame(df1b)[,3] # averaged fapar from 2003 - 2012 (365 length of data)
      df2 <- rep(df1b,(yr_end- yr_start+1)) # repeated it to multiple years, the number of years is consitent to what we collect climate forcing
    } else { # if measurement year bewteen 2003 and 2015 --> use such years directly where consistent with climate forcing
      df1a <- subset(fapar,Year>=yr_start & Year<=yr_end)
      df2 <- df1a$modisvar_filled }
    
    fpar <- df2
    
    df3 <- cbind(df1,fpar)
    df3 <- df3[,c("date","temp","prec","rain","snow","vpd","ppfd","patm","ccov_int","ccov","fpar","co2")]
    names(df3)[names(df3) == 'rain'] <- 'rainf'
    names(df3)[names(df3) == 'snow'] <- 'snowf'
    names(df3)[names(df3) == 'fpar'] <- 'fapar'
    assign(paste("final",df1$sitename[1],sep="_"), as_tibble(df3))
  }
}

#3. rsofun to predict gpp
df_soiltexture <- bind_rows(
  top    = tibble(layer = "top",    fsand = 0.4, fclay = 0.3, forg = 0.1, fgravel = 0.1),
  bottom = tibble(layer = "bottom", fsand = 0.4, fclay = 0.3, forg = 0.1, fgravel = 0.1))
params_modl <- list(
  kphio           = 0.09423773,
  soilm_par_a     = 0.33349283,
  soilm_par_b     = 1.45602286)

NPP_Forest$whc = 170

NPP_Forest$pred_gpp_c3 <- NA
#NPP_Forest$pred_gpp_c4 <- NA
NPP_Forest$max_vcmax25_c3 <- NA
#NPP_Forest$max_vcmax25_c4 <- NA


#using rsofun
for (i in 1:nrow(NPP_Forest)) {
  tryCatch({
    #c3
    forcing <- (eval(parse(text=(paste("final",NPP_Forest$sitename[i],sep="_")))))
    modlist <- run_pmodel_f_bysite( 
      NPP_Forest$sitename[i], 
      params_siml <- list(
        spinup             = TRUE,
        spinupyears        = 10,
        recycle            = 1,
        soilmstress        = TRUE,
        tempstress         = TRUE,
        calc_aet_fapar_vpd = FALSE,
        in_ppfd            = TRUE,
        in_netrad          = FALSE,
        outdt              = 1,
        ltre               = FALSE,
        ltne               = FALSE,
        ltrd               = FALSE,
        ltnd               = FALSE,
        lgr3               = TRUE,
        lgn3               = FALSE,
        lgr4               = FALSE,
        firstyeartrend = NPP_Forest$year_start[i],
        nyeartrend = NPP_Forest$year_end[i]-NPP_Forest$year_start[i]+1), 
      siteinfo = NPP_Forest[i,], 
      forcing, 
      df_soiltexture, 
      params_modl = params_modl, 
      makecheck = TRUE)
    
    pred_gpp_list <- modlist %>% mutate(ymonth = month(date),yday = day(date)) %>% group_by(ymonth, yday) %>% summarise(gpp = mean(gpp, na.rm = TRUE))
    max_vcmax25 <- max(modlist$vcmax25)*1000000
    
    NPP_Forest[i,c("pred_gpp_c3")] <- sum(pred_gpp_list$gpp)
    NPP_Forest[i,c("max_vcmax25_c3")] <- max_vcmax25
  }, error=function(e){})} 

subset(NPP_Forest,is.na(pred_gpp_c3)==TRUE)$sitename_fpar

plot(newmap, xlim = c(-180, 180), ylim = c(-75, 75), asp = 1)
points(subset(NPP_Forest,is.na(pred_gpp_c3)==TRUE)$lon,subset(NPP_Forest,is.na(pred_gpp_c3)==TRUE)$lat, col="red", pch=16,cex=1)
#these points were missing, either due to fapar or climate forcing missing

NPP_Forest_all_flux <- NPP_Forest
#stop at here and save - where "NPP_Forest_all_flux" can be used furtherly, without the need to re-load

#now, extracting site values from Tg, alpha, c/n.....
elev_nc <- read_nc_onefile("~/data/watch_wfdei/WFDEI-elevation.nc")
elev <- as.data.frame(nc_to_df(elev_nc, varnam = "elevation"))

Tg <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/nimpl_sofun_inputs/map/Final_ncfile/Tg.nc"),
  varnam = "Tg"))

PPFD <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/nimpl_sofun_inputs/map/Final_ncfile/PPFD.nc"),
  varnam = "PPFD"))

vpd <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/nimpl_sofun_inputs/map/Final_ncfile/vpd.nc"),
  varnam = "vpd"))

alpha <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/nimpl_sofun_inputs/map/Final_ncfile/alpha.nc"),
  varnam = "alpha"))

fAPAR <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/nimpl_sofun_inputs/map/Final_ncfile/fAPAR.nc"),
  varnam = "fAPAR"))

age <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/nimpl_sofun_inputs/map/Final_ncfile/age.nc"),
  varnam = "age"))

CNrt <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/nimpl_sofun_inputs/map/Final_ncfile/CNrt.nc"),
  varnam = "CNrt"))

LMA <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/nimpl_sofun_inputs/map/Final_ncfile/LMA.nc"),
  varnam = "LMA"))

#2. Create a function to specify path, loop many years nc file and output a dataframe (lon, lat, var).
inputnc <- function(name,start_year,end_year){
  output_allyears <- data.frame(matrix(NA))
  # first, include all years annual data into a daframe
  for (i in firstyr_data:endyr_data){
    if (name == "npp"){
      nc <- read_nc_onefile(alloutput_list[grepl("a.npp.nc", list.files(location,full.names = T))][i-firstyr_data+1]) #we only rely this to filter npp.nc file...
    } else {
      nc <- read_nc_onefile(alloutput_list[grepl(name, list.files(location,full.names = T))][i-firstyr_data+1]) #Input nc
    }
    output_year <- nc_to_df(nc, varnam = name)[,3] #Yearly output
    output_allyears[1:259200,i-firstyr_data+1] <- output_year #here first column represents first year of data file 's output
  }
  names(output_allyears) <- paste(name,firstyr_data:endyr_data,sep="")
  #this variable above (output_allyears), could be end of the function, which is variable at multiple years. But for our purporses, we need mean of select years
  #then, only calculate means of selected years
  output_selected_yrs <- rowMeans(output_allyears[,(start_year-firstyr_data+1):(end_year-firstyr_data+1)],na.rm = TRUE) # only calculated means based on selected start and end year (see function)
  coord <- nc_to_df(nc, varnam = name)[,1:2] # obtain lon and lat
  final_output <- cbind(coord,elev[,3],output_selected_yrs) # combine lon, lat,z with rowmeans variable
  names(final_output) <- c("lon","lat","z",name)
  return(final_output)
  #-----------------------------------------------------------------------
  # Output: output_final: the output data (259200 * 3) including lon, lat and value
  #-----------------------------------------------------------------------
}

#cbind all predictors, and its lon, lat, z
all_predictors <- cbind(elev,Tg$myvar,PPFD$myvar,vpd$myvar,
                        alpha$myvar,fAPAR$myvar,age$myvar,
                        CNrt$myvar,LMA$myvar)

names(all_predictors) <- c("lon","lat","z","Tg","PPFD","vpd",
                           "alpha","fAPAR","age","CNrt","LMA")

Tg_df <- all_predictors[,c("lon","lat","z","Tg")]
PPFD_df <- all_predictors[,c("lon","lat","z","PPFD")]
vpd_df <- all_predictors[,c("lon","lat","z","vpd")]
alpha_df <- all_predictors[,c("lon","lat","z","alpha")]
fAPAR_df <- all_predictors[,c("lon","lat","z","fAPAR")]
age_df <- all_predictors[,c("lon","lat","z","age")]
CNrt_df <- all_predictors[,c("lon","lat","z","CNrt")]
LMA_df <- all_predictors[,c("lon","lat","z","LMA")]

#now, apply gwr to extract site predictors' value
NPP_Forest$Tg <- NA
NPP_Forest$PPFD <- NA
NPP_Forest$vpd <- NA
NPP_Forest$alpha <- NA
NPP_Forest$fAPAR <- NA
NPP_Forest$age <- NA
NPP_Forest$CNrt <- NA
NPP_Forest$LMA <- NA

a <- 1.5 # which degree (distance) of grid when interpolating gwr from global grids
#Extract Tg, PPFD, vpd, alpha,fAPAR,age,CNrt,LMA, max-vcmax25
for (i in 1:nrow(NPP_Forest)) {
  tryCatch({
    #Tg
    Tg_global <- na.omit(Tg_df)
    NRE_part <- subset(Tg_global,lon>(NPP_Forest[i,"lon"]-a)&lon<(NPP_Forest[i,"lon"]+a)&
                         lat>(NPP_Forest[i,"lat"]-a)&lat<(NPP_Forest[i,"lat"]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- NPP_Forest[i,c("lon","lat","z")]
    coordinates(NRE_coord) <- c("lon","lat")
    NPP_Forest[i,c("Tg")] <- (gwr(Tg ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #ppfd
    PPFD_global <- na.omit(PPFD_df)
    NRE_part <- subset(PPFD_global,lon>(NPP_Forest[i,"lon"]-a)&lon<(NPP_Forest[i,"lon"]+a)&
                         lat>(NPP_Forest[i,"lat"]-a)&lat<(NPP_Forest[i,"lat"]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- NPP_Forest[i,c("lon","lat","z")]
    coordinates(NRE_coord) <- c("lon","lat")
    NPP_Forest[i,c("PPFD")] <- (gwr(PPFD ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #vpd
    vpd_global <- na.omit(vpd_df)
    NRE_part <- subset(vpd_global,lon>(NPP_Forest[i,"lon"]-a)&lon<(NPP_Forest[i,"lon"]+a)&
                         lat>(NPP_Forest[i,"lat"]-a)&lat<(NPP_Forest[i,"lat"]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- NPP_Forest[i,c("lon","lat","z")]
    coordinates(NRE_coord) <- c("lon","lat")
    NPP_Forest[i,c("vpd")] <- (gwr(vpd ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #alpha
    alpha_global <- na.omit(alpha_df)
    NRE_part <- subset(alpha_global,lon>(NPP_Forest[i,"lon"]-a)&lon<(NPP_Forest[i,"lon"]+a)&
                         lat>(NPP_Forest[i,"lat"]-a)&lat<(NPP_Forest[i,"lat"]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- NPP_Forest[i,c("lon","lat","z")]
    coordinates(NRE_coord) <- c("lon","lat")
    NPP_Forest[i,c("alpha")]  <- (gwr(alpha ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #fAPAR
    fAPAR_global <- na.omit(fAPAR_df)
    NRE_part <- subset(fAPAR_global,lon>(NPP_Forest[i,"lon"]-a)&lon<(NPP_Forest[i,"lon"]+a)&
                         lat>(NPP_Forest[i,"lat"]-a)&lat<(NPP_Forest[i,"lat"]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- NPP_Forest[i,c("lon","lat","z")]
    coordinates(NRE_coord) <- c("lon","lat")
    NPP_Forest[i,c("fAPAR")]<- (gwr(fAPAR ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #age
    age_global <- na.omit(age_df)
    NRE_part <- subset(age_global,lon>(NPP_Forest[i,"lon"]-a)&lon<(NPP_Forest[i,"lon"]+a)&
                         lat>(NPP_Forest[i,"lat"]-a)&lat<(NPP_Forest[i,"lat"]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- NPP_Forest[i,c("lon","lat","z")]
    coordinates(NRE_coord) <- c("lon","lat")
    NPP_Forest[i,c("age")]  <- (gwr(age ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #CNrt
    CNrt_global <- na.omit(CNrt_df)
    NRE_part <- subset(CNrt_global,lon>(NPP_Forest[i,"lon"]-a)&lon<(NPP_Forest[i,"lon"]+a)&
                         lat>(NPP_Forest[i,"lat"]-a)&lat<(NPP_Forest[i,"lat"]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- NPP_Forest[i,c("lon","lat","z")]
    coordinates(NRE_coord) <- c("lon","lat")
    NPP_Forest[i,c("CNrt")]  <- (gwr(CNrt ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #LMA
    LMA_global <- na.omit(LMA_df)
    NRE_part <- subset(LMA_global,lon>(NPP_Forest[i,"lon"]-a)&lon<(NPP_Forest[i,"lon"]+a)&
                         lat>(NPP_Forest[i,"lat"]-a)&lat<(NPP_Forest[i,"lat"]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- NPP_Forest[i,c("lon","lat","z")]
    coordinates(NRE_coord) <- c("lon","lat")
    NPP_Forest[i,c("LMA")]  <- (gwr(LMA ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
  }, error=function(e){})} 

summary(NPP_Forest)
NPP_Forest$vpd[NPP_Forest$vpd<0] <- NA
NPP_Forest$age[NPP_Forest$age<0] <- NA

newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-180, 180), ylim = c(-75, 75), asp = 1)
points(subset(NPP_Forest,is.na(LMA)==TRUE)$lon,subset(NPP_Forest,is.na(LMA)==TRUE)$lat, col="red", pch=16,cex=1)
#these sites have no data for LMA, CNrt age and fAPAR.

#now, using several statistical models to predict npp, anpp, npp.leaf....
NPP_Forest$pred_npp <- NPP_Forest$pred_gpp_c3 * (1/(1 + exp(-(-0.3677 * log(NPP_Forest$CNrt) +
                                                               -0.1552 * log(NPP_Forest$age) + 
                                                               0.5791 * NPP_Forest$fAPAR+
                                                               1.9144 *NPP_Forest$alpha + -1.1052))))

NPP_Forest$pred_anpp <- NPP_Forest$pred_gpp * (1/(1 + exp(-(-0.6075 * log(NPP_Forest$CNrt) +
                                                                -0.1798 * log(NPP_Forest$age) + 
                                                                0.8617 * NPP_Forest$fAPAR+
                                                                2.1287 *NPP_Forest$alpha + -1.3528))))

NPP_Forest$pred_bnpp <- NPP_Forest$pred_npp - NPP_Forest$pred_anpp

NPP_Forest$pred_lnpp <- NPP_Forest$pred_anpp * (1/(1 + exp(-(1.2350* log(NPP_Forest$PPFD) +
                                                                 0.0731 * (NPP_Forest$Tg) + 
                                                                 -1.0192 * log(NPP_Forest$vpd) + -9.2375))))

NPP_Forest$pred_wnpp <- NPP_Forest$pred_anpp - NPP_Forest$pred_lnpp

#use rsofun - site-species
#NPP_Forest$pred_leafnc <- (0.0162/0.5) + (0.0039/0.5) * NPP_Forest$max_vcmax25/NPP_Forest$LMA
NPP_Forest$pred_leafnc <- (0.01599/0.5) + (0.005992/0.5) * NPP_Forest$max_vcmax25/NPP_Forest$LMA

NPP_Forest$pred_lnf <- NPP_Forest$pred_lnpp*NPP_Forest$pred_leafnc

#summary(NPP_Forest$CN_wood_final)
NPP_Forest$pred_wnf <- NPP_Forest$pred_wnpp/97

hist(NPP_Forest$CN_root_final)
summary(NPP_Forest$CN_root_final)#using median of root.

NPP_Forest$pred_bnf <- NPP_Forest$pred_bnpp/122

siteinfo_final$CN_leaf_final[siteinfo_final$CN_leaf_final>100] <- NA

#the difference between cn_leaf_org and cn_leaf_final is that the previous one has not include "repeated merge" (with 36 less sites)

#correct new dataset's rep_info
NPP_Forest$rep_info[875:935] <- ""
NPP_Forest2 <- subset(NPP_Forest,rep_info!="rep" & rep_info!="rep1"& rep_info!="rep3")

NPP_Forest2_sitemean <- aggregate(NPP_Forest2,by=list(NPP_Forest2$lon,NPP_Forest2$lat,NPP_Forest2$z), FUN=mean, na.rm=TRUE) #site-mean

My_Theme = theme(
  axis.title.x = element_text(size = 20),
  axis.text.x = element_text(size = 20),
  axis.title.y = element_text(size = 20),
  axis.text.y = element_text(size = 20))

#check
#analyse_modobs2(forest_site2,"pred_gpp", "GPP",type = "points")
ggplot(data=NPP_Forest2, aes(x=pred_gpp_c3, y=GPP)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()+My_Theme
summary(lm(GPP~pred_gpp_c3,NPP_Forest2))


#analyse_modobs2(forest_site2,"pred_npp", "TNPP_1",type = "points")
ggplot(data=NPP_Forest2, aes(x=pred_npp, y=TNPP_1)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()+My_Theme
summary(lm(TNPP_1~pred_npp,NPP_Forest2))

#analyse_modobs2(forest_site2,"pred_anpp", "ANPP_2",type = "points")
ggplot(data=NPP_Forest2, aes(x=pred_anpp, y=ANPP_2)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()+My_Theme
summary(lm(ANPP_2~pred_anpp,NPP_Forest2))

#analyse_modobs2(forest_site2,"pred_lnpp", "NPP.foliage",type = "points")
ggplot(data=NPP_Forest2, aes(x=pred_lnpp, y=NPP.foliage)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()+My_Theme
summary(lm(NPP.foliage~pred_lnpp,NPP_Forest2))

#analyse_modobs2(forest_site,"pred_wnpp", "NPP.wood",type = "points")
ggplot(data=NPP_Forest2, aes(x=pred_wnpp, y=NPP.wood)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()+My_Theme
summary(lm(NPP.wood~pred_wnpp,NPP_Forest2))

#analyse_modobs2(forest_site2,"pred_bnpp", "BNPP_1",type = "points")
ggplot(data=NPP_Forest2, aes(x=pred_bnpp, y=BNPP_1)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()+My_Theme
summary(lm(BNPP_1~pred_bnpp,NPP_Forest2))

#analyse_modobs2(forest_site,"pred_lnf", "lnf_obs",type = "points") 
ggplot(data=NPP_Forest2, aes(x=pred_lnf, y=lnf_obs_final)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()+My_Theme
summary(lm(lnf_obs_final~pred_lnf,NPP_Forest2))

#without second interpolation --> use this!
ggplot(data=NPP_Forest2, aes(x=pred_lnf, y=lnf_obs_org)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()+My_Theme
summary(lm(lnf_obs_org~pred_lnf,NPP_Forest2))

#wnf - assuming constant wood/cn = 97
ggplot(data=NPP_Forest2, aes(x=pred_wnf, y=wnf_obs_final)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()+My_Theme
summary(lm(wnf_obs_final~pred_wnf,NPP_Forest2))

#bnf - assuming constant root/cn = 122
ggplot(data=NPP_Forest2, aes(x=pred_bnf, y=bnf_obs_final)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()+My_Theme
summary(lm(bnf_obs_final~pred_bnf,NPP_Forest2))

#leaf cn
#(9) leafcn
#check leaf c/n
SP_input <- read.csv(file="/Users/yunpeng/data/leaf_traits/combined_leaf_traits.csv") #new one 
SP_input2 <- SP_input[,c("lat","lon","z","Vcmax25","narea","lma")]
sitemean <- aggregate(SP_input2,by=list(SP_input2$lon,SP_input2$lat), FUN=mean, na.rm=TRUE) 
dim(sitemean)

sitemean$pred_leafn <- (0.01599) + (0.005992)* sitemean$Vcmax25/sitemean$lma
sitemean$obs_leafn <- sitemean$narea/sitemean$lma

ggplot(data=sitemean, aes(x=pred_leafn, y=obs_leafn)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Predicted Nmass (mg/g)")+ylab("Measured Nmass (mg/g)")+theme_classic()+My_Theme
summary(lm(obs_leafn~pred_leafn,sitemean))

#nuptake
NPP_Forest2$pred_nuptake <- NPP_Forest2$pred_lnf + NPP_Forest2$pred_bnf + NPP_Forest2$pred_wnf
NPP_Forest2$obs_nuptake <- NPP_Forest2$lnf_obs_org + NPP_Forest2$bnf_obs_final + NPP_Forest2$wnf_obs_final

ggplot(data=NPP_Forest2, aes(x=pred_nuptake, y=obs_nuptake)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()+My_Theme
summary(lm(pred_nuptake~obs_nuptake,NPP_Forest2))

#nre
#check mean of NRE
NRE_Du <- read.csv(file="~/data/NRE_various/NRE_Du/NRE_Du.csv")
NRE_Dong <- read.csv(file="~/data/NRE_various/NRE_Deng/NRE_Deng.csv")

NRE_Du_df <- NRE_Du[,c("lon","lat","NRE","MAT","MAP")]
NRE_Du_df <- aggregate(NRE_Du_df,by=list(NRE_Du_df$lon,NRE_Du_df$lat), FUN=mean, na.rm=TRUE) #site-mean
NRE_Du_df <- NRE_Du_df[,c(3:7)]
head(NRE_Du_df)
dim(NRE_Du_df)

NRE_Dong_df <- NRE_Dong[,c("Longitude","Latitude","NRE.nitrogen.resorption.efficiency.","MAT","MAP")]
names(NRE_Dong_df) <- c("lon","lat","NRE","MAT","MAP")
head(NRE_Dong_df)
NRE_Dong_df <- aggregate(NRE_Dong_df,by=list(NRE_Dong_df$lon,NRE_Dong_df$lat), FUN=mean, na.rm=TRUE) #site-mean
NRE_Dong_df <- NRE_Dong_df[,c(3:7)]
dim(NRE_Dong_df)


NRE_Dong_df$source <- "Dong"
NRE_Du_df$source <- "Du"
NRE_df <- rbind(NRE_Du_df,NRE_Dong_df)
summary(NRE_df)

#check repeated data, and remove 6 repeated points from Du et al. paper
NRE_df$repeated <- duplicated(NRE_df[,c("lon","lat")])
summary(NRE_df$repeated)
NRE_df <- subset(NRE_df,repeated==FALSE)

#project data
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-180, 180), ylim = c(-75, 75), asp = 1)

points(NRE_df$lon,NRE_df$lat, col="red", pch=16,cex=1)

#3. add elevation in this df, based on ingtestr 
siteinfo <- NRE_df[,c("lon","lat")] # present x and y separately
siteinfo$date_start <- lubridate::ymd(paste0(1982, "-01-01"))
siteinfo$date_end <- lubridate::ymd(paste0(2011, "-12-31"))
siteinfo$sitename <- paste0("s", 1:nrow(siteinfo),sep="")
siteinfo <- as_tibble(siteinfo)

devtools::load_all("/Users/yunpeng/yunkepeng/Grassland_new_ingestr_rsofun_20210326/ingestr/")


df_etopo <- ingest(
  siteinfo,
  source = "etopo1",
  dir = "~/data/etopo/" 
)

NRE_df$elevation <- as.numeric(as.data.frame(df_etopo$data))
subset(NRE_df,elevation<0)
#Some grids > 0, lets' assume -3062 as NA, and others as 0 firstly?
NRE_df$elevation[NRE_df$elevation< -50] <- NA
NRE_df$elevation[NRE_df$elevation< 0] <- 0

summary(NRE_df)

#for nre
names(NRE_df) <- c("lon","lat","NRE","MAT","MAP","source","repeated","z")
NRE_df$Tg <- NA
NRE_df$vpd <- NA
a <- 1.5

for (i in 1:nrow(NRE_df)) {
  tryCatch({
    #Tg
    Tg_global <- na.omit(Tg_df)
    NRE_part <- subset(Tg_global,lon>(NRE_df[i,1]-a)&lon<(NRE_df[i,1]+a)&
                         lat>(NRE_df[i,2]-a)&lat<(NRE_df[i,2]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- NRE_df[i,c("lon","lat","z")]
    coordinates(NRE_coord) <- c("lon","lat")
    NRE_df[i,c("Tg")] <- (gwr(Tg ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #vpd
    vpd_global <- na.omit(vpd_df)
    NRE_part <- subset(vpd_global,lon>(NRE_df[i,1]-a)&lon<(NRE_df[i,1]+a)&
                         lat>(NRE_df[i,2]-a)&lat<(NRE_df[i,2]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- NRE_df[i,c("lon","lat","z")]
    coordinates(NRE_coord) <- c("lon","lat")
    NRE_df[i,c("vpd")] <- (gwr(vpd ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
  }, error=function(e){})} 

NRE_df$pred_nre <- NA
NRE_df$vpd[NRE_df$vpd<0] <- NA
NRE_df$pred_nre <- (1/(1+exp(-(-0.0679 *NRE_df$Tg + 0.4217 * log(NRE_df$vpd) + 1.4541))))

NRE_df$NRE <- NRE_df$NRE/100

ggplot(data=NRE_df, aes(x=pred_nre, y=NRE)) + xlim(c(0.25,1))+ylim(c(0.25,1))+
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()+My_Theme

summary(lm(NRE~pred_nre,NRE_df))

#now, newly adding fluxnet gpp sites
fluxnet_site <- ingestr::siteinfo_fluxnet2015
fluxnet_site$lat <- as.numeric(format(round(fluxnet_site$lat, 2), nsmall = 2))
fluxnet_site$lon <- as.numeric(format(round(fluxnet_site$lon, 2), nsmall = 2))

GMD_GPP <- read.csv(file="~/data/gpp_gmd/gpp_pmodel_fluxnet2015_stocker19gmd_spatial.csv")
GMD_GPP <- subset(GMD_GPP,setup=="FULL")

fluxnet_GPP <- merge(fluxnet_site,GMD_GPP,by=c("sitename"),all.x=TRUE)

fluxnet_GPP2 <-fluxnet_GPP[,c("lon","lat","sitename","gpp")]
fluxnet_GPP2 <- na.omit(fluxnet_GPP2)

NPP_site <- NPP_Forest[,c("lon","lat","z","site")]
NPP_site$lat <- as.numeric(format(round(NPP_site$lat, 2), nsmall = 2))
NPP_site$lon <- as.numeric(format(round(NPP_site$lon, 2), nsmall = 2))


NPP_test <- merge(NPP_site,fluxnet_GPP2,by=c("lon","lat"),all.x=TRUE)
NPP_test<- na.omit(NPP_test)

fluxnet_gpp_final <- aggregate(NPP_test,by=list(NPP_test$site), mean,na.rm=TRUE)
fluxnet_gpp_final <- fluxnet_gpp_final[,c(1,7)]
names(fluxnet_gpp_final) <- c("site","flux_gpp")
fluxnet_gpp_final

#now, interpolating 2nd version (with the inclusion of fluxnet predicted gpp)
NPP_Forest_by_flux <- merge(NPP_Forest,fluxnet_gpp_final,by=c("site"),all.x=TRUE)

NPP_Forest_by_flux$new_gpp <- NA

for (i in 1:nrow(NPP_Forest_by_flux)){
  if (is.na(NPP_Forest_by_flux$flux_gpp[i]) == TRUE){ 
    NPP_Forest_by_flux$new_gpp[i] <- NPP_Forest_by_flux$pred_gpp_c3[i]
  } else {
    NPP_Forest_by_flux$new_gpp[i] <- NPP_Forest_by_flux$flux_gpp[i]
  }
}
summary(NPP_Forest_by_flux$flux_gpp)

#now, repeat


#now, using several statistical models to predict npp, anpp, npp.leaf....
NPP_Forest_by_flux$pred_npp <- NPP_Forest_by_flux$new_gpp * (1/(1 + exp(-(-0.3677 * log(NPP_Forest_by_flux$CNrt) +
                                                                -0.1552 * log(NPP_Forest_by_flux$age) + 
                                                                0.5791 * NPP_Forest_by_flux$fAPAR+
                                                                1.9144 *NPP_Forest_by_flux$alpha + -1.1052))))

NPP_Forest_by_flux$pred_anpp <- NPP_Forest_by_flux$pred_gpp * (1/(1 + exp(-(-0.6075 * log(NPP_Forest_by_flux$CNrt) +
                                                              -0.1798 * log(NPP_Forest_by_flux$age) + 
                                                              0.8617 * NPP_Forest_by_flux$fAPAR+
                                                              2.1287 *NPP_Forest_by_flux$alpha + -1.3528))))

NPP_Forest_by_flux$pred_bnpp <- NPP_Forest_by_flux$pred_npp - NPP_Forest_by_flux$pred_anpp

NPP_Forest_by_flux$pred_lnpp <- NPP_Forest_by_flux$pred_anpp * (1/(1 + exp(-(1.2350* log(NPP_Forest_by_flux$PPFD) +
                                                               0.0731 * (NPP_Forest_by_flux$Tg) + 
                                                               -1.0192 * log(NPP_Forest_by_flux$vpd) + -9.2375))))

NPP_Forest_by_flux$pred_wnpp <- NPP_Forest_by_flux$pred_anpp - NPP_Forest_by_flux$pred_lnpp

#use rsofun - site-species
#NPP_Forest_by_flux$pred_leafnc <- (0.0162/0.5) + (0.0039/0.5) * NPP_Forest_by_flux$max_vcmax25/NPP_Forest_by_flux$LMA
NPP_Forest_by_flux$pred_leafnc <- (0.01599/0.5) + (0.005992/0.5) * NPP_Forest_by_flux$max_vcmax25/NPP_Forest_by_flux$LMA


NPP_Forest_by_flux$pred_lnf <- NPP_Forest_by_flux$pred_lnpp*NPP_Forest_by_flux$pred_leafnc

NPP_Forest_by_flux$pred_wnf <- NPP_Forest_by_flux$pred_wnpp/97

NPP_Forest_by_flux$pred_bnf <- NPP_Forest_by_flux$pred_bnpp/122

#correct new dataset's rep_info
NPP_Forest_by_flux2 <- subset(NPP_Forest_by_flux,rep_info!="rep" & rep_info!="rep1"& rep_info!="rep3")

NPP_Forest_by_flux2_sitemean <- aggregate(NPP_Forest_by_flux2,by=list(NPP_Forest_by_flux2$lon,NPP_Forest_by_flux2$lat,NPP_Forest_by_flux2$z), FUN=mean, na.rm=TRUE) #site-mean

#check
#analyse_modobs2(forest_site2,"pred_gpp", "GPP",type = "points")
ggplot(data=NPP_Forest_by_flux2, aes(x=new_gpp, y=GPP)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()
summary(lm(GPP~new_gpp,NPP_Forest_by_flux2))

ggplot(data=subset(NPP_Forest_by_flux2,file!="Sara Vicca_stand level"), aes(x=new_gpp, y=GPP)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()
#those data are somewhere repeated?
summary(lm(GPP~new_gpp,NPP_Forest_by_flux2,file!="Sara Vicca_stand level"))

#analyse_modobs2(forest_site2,"pred_npp", "TNPP_1",type = "points")
ggplot(data=NPP_Forest_by_flux2, aes(x=pred_npp, y=TNPP_1)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()
summary(lm(TNPP_1~pred_npp,NPP_Forest_by_flux2))

#analyse_modobs2(forest_site2,"pred_anpp", "ANPP_2",type = "points")
ggplot(data=NPP_Forest_by_flux2, aes(x=pred_anpp, y=ANPP_2)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()
summary(lm(ANPP_2~pred_anpp,NPP_Forest_by_flux2))

#analyse_modobs2(forest_site2,"pred_lnpp", "NPP.foliage",type = "points")
ggplot(data=NPP_Forest_by_flux2, aes(x=pred_lnpp, y=NPP.foliage)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()
summary(lm(NPP.foliage~pred_lnpp,NPP_Forest_by_flux2))

#analyse_modobs2(forest_site,"pred_wnpp", "NPP.wood",type = "points")
ggplot(data=NPP_Forest_by_flux2, aes(x=pred_wnpp, y=NPP.wood)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()
summary(lm(NPP.wood~pred_wnpp,NPP_Forest_by_flux2))

#analyse_modobs2(forest_site2,"pred_bnpp", "BNPP_1",type = "points")
ggplot(data=NPP_Forest_by_flux2, aes(x=pred_bnpp, y=BNPP_1)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()
summary(lm(BNPP_1~pred_bnpp,NPP_Forest_by_flux2))

#analyse_modobs2(forest_site,"pred_lnf", "lnf_obs",type = "points") 
ggplot(data=NPP_Forest_by_flux2, aes(x=pred_lnf, y=lnf_obs_final)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()
summary(lm(lnf_obs_final~pred_lnf,NPP_Forest_by_flux2))

#without second interpolation --> use this!
ggplot(data=NPP_Forest_by_flux2, aes(x=pred_lnf, y=lnf_obs_org)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()+My_Theme
summary(lm(lnf_obs_org~pred_lnf,NPP_Forest_by_flux2))

#wnf - assuming constant wood/cn = 97
ggplot(data=NPP_Forest_by_flux2, aes(x=pred_wnf, y=wnf_obs_final)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()
summary(lm(wnf_obs_final~pred_wnf,NPP_Forest_by_flux2))

#bnf - assuming constant root/cn = 122
ggplot(data=NPP_Forest_by_flux2, aes(x=pred_bnf, y=bnf_obs_final)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()
summary(lm(bnf_obs_final~pred_bnf,NPP_Forest_by_flux2))

#bnf - assuming constant root/cn = 122
NPP_Forest_by_flux2$pred_nuptake <- NPP_Forest_by_flux2$pred_lnf + NPP_Forest_by_flux2$pred_bnf + NPP_Forest_by_flux2$pred_wnf
NPP_Forest_by_flux2$obs_nuptake <- NPP_Forest_by_flux2$lnf_obs_org + NPP_Forest_by_flux2$bnf_obs_final + NPP_Forest_by_flux2$wnf_obs_final

ggplot(data=NPP_Forest_by_flux2, aes(x=pred_nuptake, y=obs_nuptake)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()
summary(lm(obs_nuptake~pred_nuptake,NPP_Forest_by_flux2))

save.image(file = "~/yunkepeng/nimpl_sofun_inputs/forest/reprocessing_forest_site_simulation.Rdata")

#now, adding Nuptake from additional source
NPP_Forest_by_flux2_Nuptake <- subset(NPP_Forest_by_flux2,obs_nuptake>0)
NPP_Forest_by_flux2_Nuptake <- NPP_Forest_by_flux2_Nuptake[,c("lon","lat","pred_nuptake","obs_nuptake")]
NPP_Forest_by_flux2_Nuptake$method <- "tissues C allocations and C/N"
Nuptake_new <- read.csv("/Users/yunpeng/data/NPP_Yunke/Nuptake_gcme/All_Nuptake.csv")
Nuptake_new <- Nuptake_new[,c("lon","lat","pred_nuptake","obs_nuptake","method")]

Nuptake_final <- dplyr::bind_rows(NPP_Forest_by_flux2_Nuptake, Nuptake_new)

ggplot(data=Nuptake_final, aes(x=pred_nuptake, y=obs_nuptake)) +
  geom_point(aes(x=pred_nuptake, y=obs_nuptake,color=factor(method)))+geom_abline(intercept=0,slope=1)+geom_smooth(aes(x=pred_nuptake, y=obs_nuptake,color=factor(method)),method = "lm", se = TRUE)+
  xlab("Predicted N uptake")+ylab("Measured N uptake")+theme_classic()+My_Theme

ggplot(data=Nuptake_final, aes(x=pred_nuptake, y=obs_nuptake)) +
  geom_point(aes(x=pred_nuptake, y=obs_nuptake,color=factor(method)))+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Predicted N uptake")+ylab("Measured N uptake")+theme_classic()+My_Theme
summary(lm(obs_nuptake~pred_nuptake,Nuptake_final))

Nuptake_final_miner <- subset(Nuptake_final,method=="Net minerlization (Finzi paper)")
ggplot(data=Nuptake_final_miner, aes(x=pred_nuptake, y=obs_nuptake)) +
  geom_point(aes(x=pred_nuptake, y=obs_nuptake,color=factor(method)))+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Predicted N uptake")+ylab("Measured N uptake")+theme_classic()+My_Theme
summary(lm(obs_nuptake~pred_nuptake,Nuptake_final_miner))

ggplot(data=Nuptake_final, aes(x=pred_nuptake, y=obs_nuptake)) +
  geom_point(aes(x=pred_nuptake, y=obs_nuptake,color=factor(method)))+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Predicted N uptake")+ylab("Measured N uptake")+theme_classic()+My_Theme
summary(lm(obs_nuptake~pred_nuptake,Nuptake_final))

newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-180, 180), ylim = c(-75, 75), asp = 1)

#net minerlization
points(Nuptake_final$lon,Nuptake_final$lat, col="red", pch=16,cex=1)
points(Nuptake_final$lon[1:18],Nuptake_final$lat[1:18], col="green", pch=16,cex=1)
points(Nuptake_final$lon[259:346],Nuptake_final$lat[259:346], col="blue", pch=16,cex=1)
