rm(list=ls())
library(ingestr)
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
library(spgwr)


#load(file = "/Users/yunpeng/yunkepeng/nimpl_sofun_inputs/forest/Forest_site_check.Rdata")
#at the end of code...


#read complete dataset for measurement, after L1-L352 in /Users/yunpeng/yunkepeng/nimpl_sofun_inputs/output_check/Forest_Global_check.Rmd
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
Sara2_NPP$file <- "/Users/yunpeng/data/NPP_Yunke/NPP_SaraVicca/orig/validation_data"
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

#No original data of cmass but we can assume cmass = 48%, because (1) it is consistent with what we find in mean values of /Users/yunpeng/data/CN_leaf/final_leafCN.csv, equals to 47% and (2) see Enquist et al. 2017 https://onlinelibrary.wiley.com/doi/full/10.1111/geb.12645 - fig.2, overall the cmass was within a very small variance through this elevation transect, and we can just assume this value as 0.48!

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
rep_info <- read.csv("/Users/yunpeng/data/NPP_Yunke/NPP_final_rep.csv")
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
tiandi_forest$file <- "/Users/yunpeng/data/npp_stoichiometry_forests_tiandi/"
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

#extract forest only -->with corrected sitename in all (no NA)
NPP_Forest <- subset(NPP,pft2=="Forest")


########add sitename and sitename2, which was created when used for dowloading climate forcing and fapar forcing separately
#for details about how to create this sitename, please run /Users/yunpeng/yunkepeng/nimpl_sofun_inputs/forest/forest_sitename_preparation.R
#for info about climate forcing and fapar code, please have a look at example in  /Users/yunpeng/yunkepeng/nimpl_sofun_inputs/forest/forcing_fpar.R

NPP_final2 <- read.csv("/Users/yunpeng/data/forest_npp/forest_forcing_info_all.csv")
#pass some sitename and sitename2 data
NPP_Forest$sitename <- NPP_final2$sitename
NPP_Forest$sitename2 <- NPP_final2$sitename2

#check two df is consistent..
summary(NPP_Forest$lat-NPP_final2$lat)
summary(NPP_Forest$lon-NPP_final2$lon)
summary(NPP_Forest$z-NPP_final2$z)
summary(NPP_Forest$Begin_year-NPP_final2$Begin_year)
summary(NPP_Forest$End_year-NPP_final2$End_year)
summary(NPP_Forest$GPP-NPP_final2$GPP)
summary(NPP_Forest$TNPP_1 - NPP_final2$TNPP_1)
summary(NPP_Forest$ANPP_2 - NPP_final2$ANPP_2)
summary(NPP_Forest$NPP.foliage - NPP_final2$NPP.foliage)

NPP_final2 <- NPP_Forest
dim(NPP_final2)
####now, input forcing data from two times simulation
forcing_df <- list.files("/Users/yunpeng/data/forest_npp/forcing/",full.names = T)
length(forcing_df) # all data was included
length(NPP_Forest$sitename) # all data was included

fapar_df <- list.files("/Users/yunpeng/data/forest_npp/modis_subsets_all/",full.names = T)
length(fapar_df)

fapar_org_df <- list.files("/Users/yunpeng/data/forest_npp/modis_orig/",full.names = T)
length(fapar_org_df)

#1. fapar - input - 12 samples were missing
for (i in 1:length(fapar_df)){
  df1 <- read.csv(fapar_df[i])
  df1$date <- as.Date(df1$date)
  df1 <- df1[!(format(df1$date,"%m") == "02" & format(df1$date, "%d") == "29"), , drop = FALSE]
  df2 <- df1 %>% mutate(ymonth = month(date),
                        yday = day(date)) %>% 
    group_by(ymonth, yday) %>% 
    summarise(fpar = mean(modisvar_filled, na.rm = TRUE))
  assign(substr(sub('.*daily_', '', fapar_df[i]),1,nchar(sub('.*daily_', '', fapar_df[i]))-4), df2) 
}

for (i in 1:nrow(NPP_final2)){
  NPP_final2$forcing_avil[i] <- exists(paste(NPP_final2$sitename2[i]))
}

na_fapar <- (subset(NPP_final2,forcing_avil=="FALSE" ))
summary(na_fapar)
na_fapar$sitename2
dim(na_fapar)
dim(aggregate(NPP_final2,by=list(NPP_final2$sitename2), FUN=mean, na.rm=TRUE))

#12 sitename2 (plots), but 13 samples were missing
#I have checked the literatures, and data is all originally correct. It might be because the literatures have confused lon/lat in their data from (Malhi et al. 2011).
#However, there is no way to just change their lon and lat. Therefore, we just disregard this points, and I confirmed that we have also already disregarded those points in statistical model of anpp.leaf/anpp and in output check.
library(rworldmap)
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-180, 180), ylim = c(-75, 75), asp = 1)
points(na_fapar$lon,na_fapar$lat, col="red", pch=16,cex=1)
points(na_fapar$lon[c(1,2,3,5,8,9,10)],na_fapar$lat[c(1,2,3,5,8,9,10)], col="blue", pch=16,cex=1)
#13 samples were missing fapar, which was expected.

#2. forcing - input - all 935 points were available
for (i in 1:length(forcing_df)){
  tryCatch({
    df1 <- read.csv(forcing_df[i])
    df1$date <- as.Date(df1$date)
    conveted_fpar_text <- NPP_final2$sitename2[NPP_final2$sitename==df1$sitename[1]] #convert forcing file name to fapar file name
    fpar <- as.data.frame(eval(parse(text=conveted_fpar_text)))[,3]
    fapar <- rep(fpar,nrow(df1)/365)
    df2 <- cbind(df1,fapar)
    df3 <- df2[,c("date","temp","prec","vpd","ppfd","patm","ccov_int","ccov","fapar","co2")]
    assign(paste("final",df1$sitename[1],sep="_"), as_tibble(df3))
  }, error=function(e){})} 

#predict gpp

df_soiltexture <- bind_rows(
  top    = tibble(layer = "top",    fsand = 0.4, fclay = 0.3, forg = 0.1, fgravel = 0.1),
  bottom = tibble(layer = "bottom", fsand = 0.4, fclay = 0.3, forg = 0.1, fgravel = 0.1))
params_modl <- list(
  kphio           = 0.09423773,
  soilm_par_a     = 0.33349283,
  soilm_par_b     = 1.45602286)

NPP_final2$pred_gpp_c3 <- NA

siteinfo_final <- NPP_final2

siteinfo_final$year_start <- siteinfo_final$Begin_year
siteinfo_final$year_end <- siteinfo_final$End_year
siteinfo_final$year_start[siteinfo_final$year_start<=1980] <- 1980
siteinfo_final$year_end[siteinfo_final$year_start<=1980] <- 1989

#using rsofun based on last commit of the year 2020. See below info from git log
#commit d27871b91ea8b951bcfc65dec17f44ae0c340b4a (**HEAD -> master**, **upstream/master**)
#Author: stineb <benjamin.stocker@gmail.comm>
#Date:   Thu Dec 17 16:46:37 2020 +0100

for (i in 1:nrow(siteinfo_final)) {
  tryCatch({
    #c3
    forcing <- (eval(parse(text=(paste("final",siteinfo_final$sitename[i],sep="_")))))
    modlist <- run_pmodel_f_bysite( 
      siteinfo_final$sitename[i], 
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
        firstyeartrend = siteinfo_final$year_start[i],
        nyeartrend = siteinfo_final$year_end[i]-siteinfo_final$year_start[i]+1), 
      siteinfo = siteinfo_final[i,], 
      forcing, 
      df_soiltexture, 
      params_modl = params_modl, 
      makecheck = TRUE)
    
    pred_gpp_list <- modlist %>% mutate(ymonth = month(date),yday = day(date)) %>% group_by(ymonth, yday) %>% summarise(gpp = mean(gpp, na.rm = TRUE))
    
    siteinfo_final[i,c("pred_gpp_c3")] <- sum(pred_gpp_list$gpp)
  }, error=function(e){})} 

#for vcmax25
siteinfo_final$max_vcmax25 <- NA

for (i in 1:nrow(siteinfo_final)) {
  tryCatch({
    #c3
    forcing <- (eval(parse(text=(paste("final",siteinfo_final$sitename[i],sep="_")))))
    modlist <- run_pmodel_f_bysite( 
      siteinfo_final$sitename[i], 
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
        firstyeartrend = siteinfo_final$year_start[i],
        nyeartrend = siteinfo_final$year_end[i]-siteinfo_final$year_start[i]+1), 
      siteinfo = siteinfo_final[i,], 
      forcing, 
      df_soiltexture, 
      params_modl = params_modl, 
      makecheck = TRUE)
    
    max_vcmax25 <- max(modlist$vcmax25)
    
    siteinfo_final[i,c("max_vcmax25")] <- max_vcmax25
  }, error=function(e){})} 

siteinfo_final$max_vcmax25 <- siteinfo_final$max_vcmax25*1000000
summary(siteinfo_final$max_vcmax25) #13 points were missing, as expected

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

#Vcmax25 <- as.data.frame(nc_to_df(read_nc_onefile( "~/data/nimpl_sofun_inputs/map/Final_ncfile/Vcmax25.nc"),varnam = "Vcmax25"))

#use max vcmax25 from nimpl output
firstyr_data <- 1982 # In data file, which is the first year
endyr_data <- 2011 # In data file, which is the last year
location <- "~/data/output/latest_noNRE_forest/"
alloutput_list <- list.files(location,full.names = T)

#input elevation nc file, which will be cbind with global df directly
elev_nc <- read_nc_onefile("~/data/watch_wfdei/WFDEI-elevation.nc")
#elev_nc <- read_nc_onefile("D:/PhD/nimpl_sofun_inputs/Data/Elevation/WFDEI-elevation.nc")
elev <- as.data.frame(nc_to_df(elev_nc, varnam = "elevation"))
head(elev) # this is consistent with df coord below

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

#vcmax25_df <- inputnc("annualvcmax25",1982,2011)
vcmax25_df <- inputnc("vcmax25",1982,2011)
leafcn_df <- inputnc("leafcn",1982,2011)


#cbind all predictors, and its lon, lat, z
all_predictors <- cbind(elev,Tg$myvar,PPFD$myvar,vpd$myvar,
                        alpha$myvar,fAPAR$myvar,age$myvar,
                        CNrt$myvar,LMA$myvar,vcmax25_df$vcmax25,leafcn_df$leafcn)

names(all_predictors) <- c("lon","lat","z","Tg","PPFD","vpd",
                           "alpha","fAPAR","age","CNrt","LMA","Vcmax25","leafcn")

Tg_df <- all_predictors[,c("lon","lat","z","Tg")]
PPFD_df <- all_predictors[,c("lon","lat","z","PPFD")]
vpd_df <- all_predictors[,c("lon","lat","z","vpd")]
alpha_df <- all_predictors[,c("lon","lat","z","alpha")]
fAPAR_df <- all_predictors[,c("lon","lat","z","fAPAR")]
age_df <- all_predictors[,c("lon","lat","z","age")]
CNrt_df <- all_predictors[,c("lon","lat","z","CNrt")]
LMA_df <- all_predictors[,c("lon","lat","z","LMA")]
Vcmax25_df <- all_predictors[,c("lon","lat","z","Vcmax25")]
leafcn_df <- all_predictors[,c("lon","lat","z","leafcn")]


#now, apply gwr to extract site predictors' value
forest_site <- siteinfo_final[,c("lon","lat","z")]
forest_site$Tg <- NA
forest_site$PPFD <- NA
forest_site$vpd <- NA
forest_site$alpha <- NA
forest_site$age <- NA
forest_site$fAPAR <- NA
forest_site$CNrt <- NA
forest_site$LMA <- NA
forest_site$Vcmax25 <- NA

a <- 1.5 # which degree (distance) of grid when interpolating gwr from global grids
dim(forest_site)
#Extract Tg, PPFD, vpd, alpha,fAPAR,age,CNrt,LMA, max-vcmax25
for (i in 1:nrow(forest_site)) {
  tryCatch({
    #Tg
    Tg_global <- na.omit(Tg_df)
    NRE_part <- subset(Tg_global,lon>(forest_site[i,1]-a)&lon<(forest_site[i,1]+a)&
                         lat>(forest_site[i,2]-a)&lat<(forest_site[i,2]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- forest_site[i,1:3]
    coordinates(NRE_coord) <- c("lon","lat")
    forest_site[i,c("Tg")] <- (gwr(Tg ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #ppfd
    PPFD_global <- na.omit(PPFD_df)
    NRE_part <- subset(PPFD_global,lon>(forest_site[i,1]-a)&lon<(forest_site[i,1]+a)&
                         lat>(forest_site[i,2]-a)&lat<(forest_site[i,2]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- forest_site[i,1:3]
    coordinates(NRE_coord) <- c("lon","lat")
    forest_site[i,c("PPFD")] <- (gwr(PPFD ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #vpd
    vpd_global <- na.omit(vpd_df)
    NRE_part <- subset(vpd_global,lon>(forest_site[i,1]-a)&lon<(forest_site[i,1]+a)&
                         lat>(forest_site[i,2]-a)&lat<(forest_site[i,2]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- forest_site[i,1:3]
    coordinates(NRE_coord) <- c("lon","lat")
    forest_site[i,c("vpd")] <- (gwr(vpd ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #alpha
    alpha_global <- na.omit(alpha_df)
    NRE_part <- subset(alpha_global,lon>(forest_site[i,1]-a)&lon<(forest_site[i,1]+a)&
                         lat>(forest_site[i,2]-a)&lat<(forest_site[i,2]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- forest_site[i,1:3]
    coordinates(NRE_coord) <- c("lon","lat")
    forest_site[i,c("alpha")]  <- (gwr(alpha ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #fAPAR
    fAPAR_global <- na.omit(fAPAR_df)
    NRE_part <- subset(fAPAR_global,lon>(forest_site[i,1]-a)&lon<(forest_site[i,1]+a)&
                         lat>(forest_site[i,2]-a)&lat<(forest_site[i,2]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- forest_site[i,1:3]
    coordinates(NRE_coord) <- c("lon","lat")
    forest_site[i,c("fAPAR")]<- (gwr(fAPAR ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #age
    age_global <- na.omit(age_df)
    NRE_part <- subset(age_global,lon>(forest_site[i,1]-a)&lon<(forest_site[i,1]+a)&
                         lat>(forest_site[i,2]-a)&lat<(forest_site[i,2]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- forest_site[i,1:3]
    coordinates(NRE_coord) <- c("lon","lat")
    forest_site[i,c("age")]  <- (gwr(age ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #CNrt
    CNrt_global <- na.omit(CNrt_df)
    NRE_part <- subset(CNrt_global,lon>(forest_site[i,1]-a)&lon<(forest_site[i,1]+a)&
                         lat>(forest_site[i,2]-a)&lat<(forest_site[i,2]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- forest_site[i,1:3]
    coordinates(NRE_coord) <- c("lon","lat")
    forest_site[i,c("CNrt")]  <- (gwr(CNrt ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #LMA
    LMA_global <- na.omit(LMA_df)
    NRE_part <- subset(LMA_global,lon>(forest_site[i,1]-a)&lon<(forest_site[i,1]+a)&
                         lat>(forest_site[i,2]-a)&lat<(forest_site[i,2]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- forest_site[i,1:3]
    coordinates(NRE_coord) <- c("lon","lat")
    forest_site[i,c("LMA")]  <- (gwr(LMA ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #Vcmax25
    Vcmax25_global <- na.omit(Vcmax25_df)
    NRE_part <- subset(Vcmax25_global,lon>(forest_site[i,1]-a)&lon<(forest_site[i,1]+a)&
                         lat>(forest_site[i,2]-a)&lat<(forest_site[i,2]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- forest_site[i,1:3]
    coordinates(NRE_coord) <- c("lon","lat")
    forest_site[i,c("Vcmax25")]  <- (gwr(Vcmax25 ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
  }, error=function(e){})} 

#now, using several statistical models to predict npp, anpp, npp.leaf....
forest_site$pred_gpp <- siteinfo_final$pred_gpp_c3
forest_site$max_vcmax25 <- siteinfo_final$max_vcmax25
  
forest_site$pred_npp <- forest_site$pred_gpp * (1/(1 + exp(-(-0.3677 * log(forest_site$CNrt) +
                                                               -0.1552 * log(forest_site$age) + 
                                                               0.5791 * forest_site$fAPAR+
                                                               1.9144 *forest_site$alpha + -1.1052))))

forest_site$pred_anpp <- forest_site$pred_gpp * (1/(1 + exp(-(-0.6075 * log(forest_site$CNrt) +
                                                                -0.1798 * log(forest_site$age) + 
                                                                0.8617 * forest_site$fAPAR+
                                                                2.1287 *forest_site$alpha + -1.3528))))

forest_site$pred_bnpp <- forest_site$pred_npp - forest_site$pred_anpp

#use a new model - based on new coefficients?


forest_site$pred_lnpp <- forest_site$pred_anpp * (1/(1 + exp(-(1.2350* log(forest_site$PPFD) +
                                                                 0.0731 * (forest_site$Tg) + 
                                                                 -1.0192 * log(forest_site$vpd) + -9.2375))))

forest_site$pred_wnpp <- forest_site$pred_anpp - forest_site$pred_lnpp


#use rsofun - site-species
forest_site$pred_leafnc <- (0.0161/0.5) + (0.0041/0.5) * forest_site$max_vcmax25/forest_site$LMA


forest_site$pred_lnf <- forest_site$pred_lnpp*forest_site$pred_leafnc

#summary(NPP_Forest$CN_wood_final)
forest_site$pred_wnf <- forest_site$pred_wnpp/97

hist(NPP_Forest$CN_root_final)
summary(NPP_Forest$CN_root_final)#using median of root.

forest_site$pred_bnf <- forest_site$pred_bnpp/122

forest_site$TNPP_1 <- siteinfo_final$TNPP_1
forest_site$ANPP_2 <- siteinfo_final$ANPP_2
forest_site$BNPP_1 <- siteinfo_final$BNPP_1
forest_site$NPP.foliage <- siteinfo_final$NPP.foliage
forest_site$NPP.wood <- siteinfo_final$NPP.wood

siteinfo_final$CN_leaf_final[siteinfo_final$CN_leaf_final>100] <- NA

forest_site$CN_leaf_final <- siteinfo_final$CN_leaf_final
forest_site$lnf_obs_final <- siteinfo_final$lnf_obs_final
forest_site$lnf_obs_org <- (siteinfo_final$lnf_obs_org) # not including secondary merging from sara vicca (within 0.01 resolution), with 36 less sites

forest_site$wnf_obs_final <- siteinfo_final$wnf_obs_final
forest_site$bnf_obs_final <- siteinfo_final$bnf_obs_final
forest_site$GPP <- siteinfo_final$GPP

forest_site$site <- siteinfo_final$site
forest_site$file <- siteinfo_final$file

forest_site$rep_info <- siteinfo_final$rep_info
#correct new dataset's rep_info
forest_site$rep_info[875:935] <- ""
forest_site2 <- subset(forest_site,rep_info!="rep" & rep_info!="rep1"& rep_info!="rep3")

forest_site3 <- aggregate(forest_site2,by=list(forest_site2$lon,forest_site2$lat,forest_site2$z), FUN=mean, na.rm=TRUE) #site-mean

#check
#analyse_modobs2(forest_site2,"pred_gpp", "GPP",type = "points")
ggplot(data=forest_site2, aes(x=pred_gpp, y=GPP)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()
summary(lm(GPP~pred_gpp,forest_site2))

#analyse_modobs2(forest_site2,"pred_npp", "TNPP_1",type = "points")
ggplot(data=forest_site2, aes(x=pred_npp, y=TNPP_1)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()
summary(lm(TNPP_1~pred_npp,forest_site2))


#analyse_modobs2(forest_site2,"pred_anpp", "ANPP_2",type = "points")
ggplot(data=forest_site2, aes(x=pred_anpp, y=ANPP_2)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()
summary(lm(ANPP_2~pred_anpp,forest_site2))

#analyse_modobs2(forest_site2,"pred_lnpp", "NPP.foliage",type = "points")
ggplot(data=forest_site2, aes(x=pred_lnpp, y=NPP.foliage)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()
summary(lm(NPP.foliage~pred_lnpp,forest_site2))

#analyse_modobs2(forest_site,"pred_wnpp", "NPP.wood",type = "points")
ggplot(data=forest_site2, aes(x=pred_wnpp, y=NPP.wood)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()
summary(lm(NPP.wood~pred_wnpp,forest_site2))

#analyse_modobs2(forest_site2,"pred_bnpp", "BNPP_1",type = "points")
ggplot(data=forest_site2, aes(x=pred_bnpp, y=BNPP_1)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()
summary(lm(BNPP_1~pred_bnpp,forest_site2))

#analyse_modobs2(forest_site,"pred_lnf", "lnf_obs",type = "points") 
ggplot(data=forest_site2, aes(x=pred_lnf, y=lnf_obs_final)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()
summary(lm(lnf_obs_final~pred_lnf,forest_site2))

#not including additional merge sites within 0.01 deg, with 36 less sites

ggplot(data=forest_site2, aes(x=pred_lnf, y=lnf_obs_org)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()
summary(lm(lnf_obs_org~pred_lnf,forest_site2))


#wnf - assuming constant wood/cn = 97
ggplot(data=forest_site2, aes(x=pred_wnf, y=wnf_obs_final)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()
summary(lm(wnf_obs_final~pred_wnf,forest_site2))

#bnf - assuming constant root/cn = 122
ggplot(data=forest_site2, aes(x=pred_bnf, y=bnf_obs_final)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()
summary(lm(bnf_obs_final~pred_bnf,forest_site2))

#leaf cn
#(9) leafcn
#check leaf c/n
SP_input <- read_csv(file="~/data/CN_leaf/final_individuals.csv") #all individuals
SP_input2 <- SP_input[,c("lat","lon","Elevation","Vcmax.25","narea","lma")]
sitemean <- aggregate(SP_input2,by=list(SP_input2$lon,SP_input2$lat), FUN=mean, na.rm=TRUE) 
dim(sitemean)

sitemean$pred_leafnc <- (0.0161/0.5) + (0.0041/0.5)* sitemean$Vcmax.25/sitemean$lma
sitemean$obs_leafnc <- sitemean$narea/sitemean$lma/0.5

ggplot(data=sitemean, aes(x=pred_leafnc, y=obs_leafnc)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()+My_Theme
summary(lm(obs_leafnc~pred_leafnc,sitemean))

#nuptake
forest_site2$pred_nuptake <- forest_site2$pred_lnf + forest_site2$pred_bnf + forest_site2$pred_wnf
forest_site2$obs_nuptake <- forest_site2$lnf_obs_org + forest_site2$bnf_obs_final + forest_site2$wnf_obs_final

ggplot(data=forest_site2, aes(x=pred_nuptake, y=obs_nuptake)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()+My_Theme
summary(lm(pred_nuptake~obs_nuptake,forest_site2))



#fit a new model where C/N leaf 
#library(lme4)
#library(nlme)
#library(lmerTest)
#library("PerformanceAnalytics")
#library(MuMIn)
#r.squaredGLMM(lmer(CN_leaf_final ~ max_vcmax25+ LMA + (1|site),data=forest_site2))
#summary(lmer(CN_leaf_final ~ max_vcmax25+ LMA + (1|site),data=forest_site2))
#forest_site2$pred_fitted_leafcn <- 0.30275 * forest_site2$max_vcmax25 + 0.21866*forest_site2$LMA -4.92165

#forest_site2$CN_leaf_final[forest_site2$CN_leaf_final>100] <- NA
#ggplot(data=forest_site2, aes(x=pred_fitted_leafcn, y=CN_leaf_final)) +
#  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
#  xlab("Prediction")+ylab("Observation")+theme_classic()

#summary(lm(CN_leaf_final~pred_fitted_leafcn,forest_site2))

#forest_site2$pred_fit_lnf <- forest_site2$pred_lnpp/forest_site2$pred_fitted_leafcn

#ggplot(data=forest_site2, aes(x=pred_fit_lnf, y=lnf_obs_final)) +
#  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
#  xlab("Prediction")+ylab("Observation")+theme_classic()

#summary(lm(lnf_obs_final~pred_fit_lnf,forest_site2))

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

ggplot(data=NRE_df, aes(x=pred_nre, y=NRE)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()

summary(lm(NRE~pred_nre,NRE_df))

save.image(file = "/Users/yunpeng/yunkepeng/nimpl_sofun_inputs/forest/Forest_site_check.Rdata")

