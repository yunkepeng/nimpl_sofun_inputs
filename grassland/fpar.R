#fapar for forest and grassland
#Original and final data for fapar, in forest, were saved in "/Users/yunpeng/data/forest_npp/modis_orig" and "/Users/yunpeng/data/forest_npp/modis_subsets_all"
#Original and final data for fapar, in grassland, were saved in "/Users/yunpeng/data/grassland_npp/modis_subsets_orig" and "/Users/yunpeng/data/grassland_npp/modis_subsets_all"

# fapar -> an example given in forest sites.
library(dplyr)
library(ingestr)

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
Tiandi_npp <- Tiandi_df[,c("Original_Site_Label_stoichiometry","Longitude_NPP","Latitude_NPP","Altitude_stoichiometry","Sample_time_NPP","Sample_time_NPP","TNPP","ANPP","ANPP","BNPP","CNratio_leaf","CNratio_root","CNratio_soil")]
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

#we stop at here and not aggregate yet, otherwise the measurement year is easy to be NOT integar -> we will aggregate to site only when finally compare gpp on site basis (more details see npp_leafcn_check.Rmd).
#check if all Begin_year and End_year are integar 
stopifnot( all(NPP_final$Begin_year == floor(NPP_final$Begin_year)) )
stopifnot( all(NPP_final$End_year == floor(NPP_final$End_year)) )
#great!

#Lastly, create an unique site name, which will be used in ingestr function later on.
NPP_final$sitename <- NA
for (i in 1:nrow(NPP_final)){
  NPP_final$sitename[i] <- paste("NPP_forest",i,sep = "")
}


settings_modis <- get_settings_modis(
  bundle            = "modis_fpar",
  data_path         = "~/data/modis_forest/",
  method_interpol   = "loess",
  keep              = TRUE,
  overwrite_raw     = FALSE,
  overwrite_interpol= TRUE
)

#select forest sites only -> this object is what we used to extract all climate forcing
NPP_forest <- subset(NPP_final,pft2=="Forest")
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

coord_forest <- siteinfo[c("sitename","lon","lat")]

NPP_forest$no <- c(1:nrow(NPP_forest))
NPP_forest_all <-Reduce(function(x,y) merge(x = x, y = y, by = c("lon","lat"),all.x=TRUE), 
                         list(NPP_forest,coord_forest))

NPP_forest_all <- NPP_forest_all[order(NPP_forest_all$no), ]
#df_modis_fpar <- ingest(siteinfo = siteinfo,source= "modis",settings  = settings_modis,verbose   = FALSE,parallel = TRUE,ncore = 8)
summary(siteinfo)

library(doSNOW)
NumberOfCluster <- 8
cl <- makeCluster(NumberOfCluster, type='SOCK')
registerDoSNOW(cl)
x0 <- foreach(i = c(1:nrow(siteinfo)),.combine = "rbind") %dopar% {
  library(ingestr)
  library(dplyr)
  df_modis_fpar <- ingest_bysite(
    sitename  = siteinfo[i,c("sitename")],  # can be any name
    source    = "modis",
    year_start = 2010,
    year_end  = 2019,
    lon       = siteinfo[i,c("lon")],
    lat       = siteinfo[i,c("lat")],
    settings  = settings_modis,
    verbose   = FALSE
  )
  print(i)
}
stopCluster(cl)

forcing_list <- list.files("/Users/yunpeng/data/modis_forest",full.names = T)

nrow(siteinfo) - (length(forcing_list)-1)
#11 sites are missing


#another version, for grassland
#NPP_grassland <- subset(NPP_final,pft2=="Grassland")
#NPP_final2 <- aggregate(NPP_grassland,by=list(NPP_grassland$lon,NPP_grassland$lat), FUN=mean, na.rm=TRUE) #site-mean
#for (i in 1:nrow(NPP_final2)){NPP_final2$sitename2[i] <- paste("grassland",i,sep = "")}
#siteinfo <- data.frame(sitename = NPP_final2$sitename2,lon = NPP_final2$lon,lat = NPP_final2$lat,year_start = rep(2010,nrow(NPP_final2)),year_end = rep(2019,nrow(NPP_final2)) )
#siteinfo <-  siteinfo %>% dplyr::mutate(date_start = lubridate::ymd(paste0(year_start, "-01-01"))) %>%dplyr::mutate(date_end = lubridate::ymd(paste0(year_end, "-12-31"))) 
#then apply parallel computing to start 



