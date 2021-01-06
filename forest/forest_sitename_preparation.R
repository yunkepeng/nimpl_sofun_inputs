
##################forest###
######forest - first round
library(dplyr)
library(ingestr)
library(rbeni)
NPP_SaraVicca <- read.csv(file="~/data/NPP_Yunke/NPP_SaraVicca/NPP_SaraVicca.csv")
NPP_Malhi <- read.csv(file="~/data/NPP_Yunke/NPP_Malhi/NPP_Malhi.csv")
NPP_Keith <- read.csv(file="~/data/NPP_Yunke/NPP_Keith/NPP_Keith.csv")
NPP_Forc <- read.csv(file="~/data/NPP_Yunke/NPP_Forc/NPP_Forc.csv")
NPP_Schulze <- read.csv(file="~/data/NPP_Yunke/NPP_Schulze/NPP_Schulze.csv")

NPP_all <- rbind(NPP_SaraVicca,NPP_Malhi,NPP_Keith,NPP_Forc,NPP_Schulze)

#add pft data derived from orginal data provided from Sara Vicca, and Schulze's book.
Evergreen <- read.csv(file="~/data/NPP_Yunke/NPP_SaraVicca/orig/pft.csv")
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



#Lastly, create an unique site name, which will be used in ingestr function later on.

#select forest sites only -> this object is what we used to extract all climate forcing
NPP_forest <- subset(NPP_final,pft2=="Forest")
for (i in 1:nrow(NPP_forest)){
  NPP_forest$sitename[i] <- paste("NPP_forest",i,sep = "")
}


NPP_final2_forest <- aggregate(NPP_forest,by=list(NPP_forest$lon,NPP_forest$lat), FUN=mean, na.rm=TRUE) #site-mean


for (i in 1:nrow(NPP_final2_forest)){
  NPP_final2_forest$sitename2[i] <- paste("forest",i,sep = "")
}

siteinfo <- data.frame(
  sitename = NPP_final2_forest$sitename2,
  lon = NPP_final2_forest$lon,
  lat = NPP_final2_forest$lat,
  year_start = rep(2010,nrow(NPP_final2_forest)),
  year_end = rep(2019,nrow(NPP_final2_forest))
)

#create time frame
siteinfo <-  siteinfo %>% dplyr::mutate(date_start = lubridate::ymd(paste0(year_start, "-01-01"))) %>%
  dplyr::mutate(date_end = lubridate::ymd(paste0(year_end, "-12-31"))) 

coord_forest <- siteinfo[,c("sitename","lon","lat")]
names(coord_forest) <- c("sitename2","lon","lat")

NPP_forest$no <- c(1:nrow(NPP_forest))
NPP_forest_all <-Reduce(function(x,y) merge(x = x, y = y, by = c("lon","lat"),all.x=TRUE), 
                        list(NPP_forest,coord_forest))

NPP_forest_all <- NPP_forest_all[order(NPP_forest_all$no), ]


##########Forest - second round -->start from i = 642, fpar = 270
#second round
#finally, add more forest sites from corrected Sara Vicca's dataset, including anpp, npp.leaf and npp.wood.
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
dim(Sara2_NPP)

NPP_forest2 <- subset(Sara2_NPP,pft2=="Forest")
for (i in 1:nrow(NPP_forest2)){
  NPP_forest2$sitename[i] <- paste("NPP_forest_more",i+642,sep = "")
  NPP_forest2$sitename2[i] <- paste("forest_more",i+642,sep = "")
}

NPP_final2 <- dplyr::bind_rows(NPP_forest_all, NPP_forest2) 

NPP_final2$no <- c(1:nrow(NPP_final2))

###Now, merge leaf c/n from various sources

# (1) Add Schulz - unit: all in g/m2
CN_Schulz <- read.csv(file="~/data/NPP_Yunke/npp_cn/CN_Schulze.csv")
CN_Schulz2 <- CN_Schulz[,c(5,48:58)]

CN_Schulz2$CN_leaf_Schulz <- CN_Schulz2$c_leaf/CN_Schulz2$n_leaf
CN_Schulz2$CN_root_Schulz <- CN_Schulz2$c_fineroot/CN_Schulz2$n_root
CN_Schulz2$CN_stem_Schulz <- CN_Schulz2$c_stem/CN_Schulz2$n_stem
CN_Schulz2$CN_wood_Schulz <- (CN_Schulz2$c_stem+CN_Schulz2$c_branch)/(CN_Schulz2$n_stem+CN_Schulz2$n_branch)

CN_Schulz2 <- CN_Schulz2[,c("site","CN_leaf_Schulz","CN_root_Schulz","CN_stem_Schulz","CN_wood_Schulz")]
CN_Schulz2

# (2) Add Malhi data - assume cmass as constant 0.48 g/g; narea in gm-2, lma in gm-2
CN_Malhi <- read.csv(file="~/data/NPP_Yunke/npp_cn/CN_Malhi.csv")

#No original data of cmass but we can assume cmass = 48%, because (1) it is consistent with what we find in mean values of /Users/yunpeng/data/CN_leaf/final_leafCN.csv, equals to 47% and (2) see Enquist et al. 2017 https://onlinelibrary.wiley.com/doi/full/10.1111/geb.12645 - fig.2, overall the cmass was within a very small variance through this elevation transect, and we can just assume this value as 0.48!

CN_Malhi$CN_leaf_alt_malhi <- 0.48/(CN_Malhi$narea/CN_Malhi$lma) # use CN_leaf_alt as a new and alternative variable, by storing data when Cmass or (c%) is lacking.

CN_Malhi <- na.omit(CN_Malhi)

newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-180, 180), ylim = c(-75, 75), asp = 1)
points(CN_Malhi$lon,CN_Malhi$lat, col="red", pch=16,cex=1)

CN_Malhi2 <- CN_Malhi[,c("site","CN_leaf_alt_malhi")]
CN_Malhi2

#(3) Add Species-based traits data, as provided from Sara Vicca, including around 40 forest sites that have species-based leaf c/n
#The data is c% and n%
CN_SaraVicca <- read.csv(file="~/data/NPP_Yunke/NPP_SaraVicca/orig/validation_data/NACP_TERRA_PNW_leaf_trait.csv")
CN_SaraVicca <- CN_SaraVicca[,c("LONGITUDE","LATITUDE","LEAF_CN")]
CN_SaraVicca$LEAF_CN[CN_SaraVicca$LEAF_CN<0] <- NA
CN_SaraVicca$LONGITUDE <- as.numeric(CN_SaraVicca$LONGITUDE)
CN_SaraVicca$LATITUDE <- as.numeric(CN_SaraVicca$LATITUDE)

CN_SaraVicca2 <- aggregate(CN_SaraVicca, by=list(CN_SaraVicca$LONGITUDE,CN_SaraVicca$LATITUDE), mean,na.rm=TRUE)
CN_SaraVicca2 <- CN_SaraVicca2[,c("LONGITUDE","LATITUDE","LEAF_CN")]
names(CN_SaraVicca2) <- c("lon","lat","CN_leaf_sara")
dim(CN_SaraVicca2)

test <- merge(NPP_final2,CN_SaraVicca2,by=c("lon","lat"),all.x=TRUE)
dim(subset(test,CN_leaf_sara>0))
test1 <- subset(test,CN_leaf_sara>0)
test1 <- test1[,c("lon","lat","z","CN_leaf_sara")]
dim(test1)
test1 <- aggregate(test1, by=list(test1$lon,test1$lat,test1$z), mean,na.rm=TRUE)
dim(test1)
test1<- test1[,c("lon","lat","z","CN_leaf_sara")]


#(4) need to merge with more sites? As far as its coordinates are within 0.01 deg?
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
npp_cn3 <- npp_cn2 %>% mutate(CN_leaf_final = coalesce(CN_leaf,CN_leaf_Schulz,CN_leaf_alt_malhi,CN_SaraVicca_old,CN_leaf_sara)) %>% 
  mutate(CN_leaf_near_final = coalesce(CN_leaf,CN_leaf_Schulz,CN_SaraVicca_old,CN_leaf_sara)) %>%
  mutate(CN_leaf_org = coalesce(CN_leaf,CN_leaf_Schulz,CN_leaf_sara)) %>%
  mutate(CN_root_final = coalesce(CN_root,CN_root_Schulz))


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

summary(NPP_final4)
NPP_final4$sitename[875:935] <- NPP_final4$site[875:935]
NPP_final4$sitename2[875:935] <- NPP_final4$site[875:935]

#output data forcing information file (forest1,2,3...) to a dataframe and save
csvfile <- paste("/Users/yunpeng/data/forest_npp/forest_forcing_info_all.csv")
write.csv(NPP_final4, csvfile, row.names = TRUE)