##################Grassland###
######For grassland
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

NPP_final2 <- dplyr::bind_rows(NPP_final, Sara2_NPP) 
summary(NPP_final2)

NPP_grassland <- subset(NPP_final2,pft2=="Grassland")
NPP_grassland$sitename <- NA
for (i in 1:nrow(NPP_grassland)){
  NPP_grassland$sitename[i] <- paste("NPP",i,sep = "")
}


siteinfo <- data.frame(
  sitename = NPP_grassland$sitename,
  lon = NPP_grassland$lon,
  lat = NPP_grassland$lat,
  elv = NPP_grassland$z,
  year_start = NPP_grassland$Begin_year,
  year_end = NPP_grassland$End_year
)
siteinfo$no <- c(1:nrow(siteinfo))

siteinfo$year_start[siteinfo$year_start<=1980] <- 1980
siteinfo$year_end[siteinfo$year_start<=1980] <- 1989

siteinfo <-  siteinfo %>% dplyr::mutate(date_start = lubridate::ymd(paste0(year_start, "-01-01"))) %>%
  dplyr::mutate(date_end = lubridate::ymd(paste0(year_end, "-12-31"))) 

#aggregate by lon, lat, z, year_start, year_end
siteinfo2 <- aggregate(siteinfo,by=list(siteinfo$lon,siteinfo$lat,siteinfo$elv,siteinfo$year_start,siteinfo$year_end), FUN=mean, na.rm=TRUE) #site-mean
for (i in 1:nrow(siteinfo2)){
  siteinfo2$sitename2[i] <- paste("ingestr",i,sep = "")
}

siteinfo3 <- siteinfo2[,c("sitename2","lon","lat","elv","year_start","year_end")]

NPP_grassland_all <-Reduce(function(x,y) merge(x = x, y = y, by = c("lon","lat","elv","year_start","year_end"),all.x=TRUE), 
                           list(siteinfo,siteinfo3))

NPP_grassland_all <- NPP_grassland_all[order(NPP_grassland_all$no), ]
#we will use above data at the end
summary(NPP_grassland_all)

siteinfo_final <- siteinfo2[,c("sitename2","lon","lat","elv","year_start","year_end","date_start","date_end")]
names(siteinfo_final) <- c("sitename","lon","lat","elv","year_start","year_end","date_start","date_end")
summary(siteinfo_final)

#last check
stopifnot( all(siteinfo_final$year_start == floor(siteinfo_final$year_start)) )
stopifnot( all(siteinfo_final$year_end == floor(siteinfo_final$year_end)) )

#site forcing --> please see /Users/yunpeng/Desktop/phd/code/tempory_nimpl/forcing_euler.R. It was making the speed faster.
#forcing
calc_vpd_inst <- function( qair=NA, tc=NA, patm=NA, elv=NA  ){
  ##-----------------------------------------------------------------------
  ## Ref:      Eq. 5.1, Abtew and Meleese (2013), Ch. 5 Vapor Pressure 
  ##           Calculation Methods, in Evaporation and Evapotranspiration: 
  ##           Measurements and Estimations, Springer, London.
  ##             vpd = 0.611*exp[ (17.27 tc)/(tc + 237.3) ] - ea
  ##             where:
  ##                 tc = average daily air temperature, deg C
  ##                 eact  = actual vapor pressure, Pa
  ##-----------------------------------------------------------------------
  kTo = 288.15   # base temperature, K (Prentice, unpublished)
  kR  = 8.3143   # universal gas constant, J/mol/K (Allen, 1973)
  kMv = 18.02    # molecular weight of water vapor, g/mol (Tsilingiris, 2008)
  kMa = 28.963   # molecular weight of dry air, g/mol (Tsilingiris, 2008)
  
  ## calculate the mass mixing ratio of water vapor to dry air (dimensionless)
  wair <- qair / (1 - qair)
  
  ## calculate water vapor pressure 
  rv <- kR / kMv
  rd <- kR / kMa
  eact = patm * wair * rv / (rd + wair * rv)  
  
  ## calculate saturation water vapour pressure in Pa
  esat <- 611.0 * exp( (17.27 * tc)/(tc + 237.3) )
  
  ## calculate VPD in units of Pa
  vpd <- ( esat - eact )    
  
  ## this empirical equation may lead to negative values for VPD (happens very rarely). assume positive...
  vpd <- max( 0.0, vpd )
  
  return( vpd )
  
}

for (i in 1:nrow(siteinfo_final)) {
  tryCatch({
    df_watch <- ingestr::ingest(
      siteinfo  = siteinfo_final[i,],
      source    = "watch_wfdei",
      getvars   = list(temp = "Tair",prec = "Rainf", vpd = "Qair", ppfd = "SWin"), 
      dir       = "/Volumes/Seagate Backup Plus Drive/data/watch_wfdei/"
    )
    
    df_cru <- ingestr::ingest(
      siteinfo  = siteinfo_final[i,],
      source    = "cru",
      getvars   = list(ccov = "cld"),
      dir       = "/Volumes/Seagate Backup Plus Drive/data/cru/ts_4.01/"
    )
    
    df_co2 <- ingestr::ingest(
      siteinfo_final[i,],
      source  = "co2_mlo",
      verbose = FALSE
    )
    
    df_co2_final <- as.data.frame(df_co2$data)
    
    df_co2_final2 <- df_co2_final[!(format(df_co2_final$date,"%m") == "02" & format(df_co2_final$date, "%d") == "29"), , drop = FALSE]
    
    co2 <- df_co2_final2$co2
    
    ddf_meteo <- as_tibble(cbind(as.data.frame(df_watch$data)[,c("date","temp","prec","qair","ppfd")],as.data.frame(df_cru$data)[,c("ccov_int","ccov")],co2))
    
    elv <- siteinfo_final$elv[i]
    
    ddf_meteo$patm <- calc_patm(elv=elv, patm0 = 101325 )
    
    for (b in 1:length(ddf_meteo$qair)){
      ddf_meteo$vpd[b] <- calc_vpd_inst( qair=ddf_meteo$qair[b], tc=ddf_meteo$temp[b], ddf_meteo$patm[b], elv=elv)
    }
    
    ddf_meteo_final <- ddf_meteo[,c("date","temp","prec","qair","vpd","ppfd","patm","ccov_int","ccov","co2")]
    ddf_meteo_final$prec <- ddf_meteo_final$prec/86400
    ddf_meteo_final$ppfd <- ddf_meteo_final$ppfd/86400
    ddf_meteo_final$sitename <- siteinfo_final$sitename[i]
    csvfile <- paste("/Users/yunpeng/data/grassland_new/forcing/",siteinfo_final$sitename[i],".csv",sep = "")
    write.csv(ddf_meteo_final, csvfile, row.names = TRUE)
    print(i)    
  }, error=function(e){})} 


settings_modis <- get_settings_modis(
  bundle            = "modis_fpar",
  data_path         = "~/data/grassland_new/fpar_all/",
  method_interpol   = "loess",
  keep              = TRUE,
  overwrite_raw     = FALSE,
  overwrite_interpol= TRUE
)

#now, fapar
library(doSNOW)
NumberOfCluster <- 8
cl <- makeCluster(NumberOfCluster, type='SOCK')
registerDoSNOW(cl)
x0 <- foreach(i = c(1:485),.combine = "rbind") %dopar% {
  library(ingestr)
  library(dplyr)
  df_modis_fpar <- ingestr::ingest_bysite(
    sitename  = siteinfo_final[i,c("sitename")],  # can be any name
    source    = "modis",
    year_start = 2010,
    year_end  = 2019,
    lon       = siteinfo_final[i,c("lon")],
    lat       = siteinfo_final[i,c("lat")],
    settings  = settings_modis,
    verbose   = FALSE
  )
}

stopCluster(cl)

dim(siteinfo_final)

fapar_df <- list.files("/Users/yunpeng/data/grassland_npp/fpar_all",full.names = T)
length(fapar_df)-1

fapar_org_df <- list.files("/Users/yunpeng/data/grassland_npp/fpar_raw",full.names = T)
length(fapar_org_df)

forcing_df <- list.files("/Users/yunpeng/data/grassland_npp/forcing",full.names = T)
length(forcing_df)



##################forest###
######forest - first round
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
#df_modis_fpar <- ingest(siteinfo = siteinfo,source= "modis",settings  = settings_modis,verbose   = FALSE,parallel = TRUE,ncore = 8)
summary(siteinfo)
head(NPP_forest_all)

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


#Forcing - forest -->recreate siteinfo
head(NPP_forest_all)
dim(NPP_forest_all)

siteinfo <- data.frame(
  sitename = NPP_forest_all$sitename,
  lon = NPP_forest_all$lon,
  lat = NPP_forest_all$lat,
  elv = NPP_forest_all$z,
  year_start = NPP_forest_all$Begin_year,
  year_end = NPP_forest_all$End_year
)

siteinfo$year_start[siteinfo$year_start<=1980] <- 1980
siteinfo$year_end[siteinfo$year_start<=1980] <- 1989

siteinfo <-  siteinfo %>% dplyr::mutate(date_start = lubridate::ymd(paste0(year_start, "-01-01"))) %>%
  dplyr::mutate(date_end = lubridate::ymd(paste0(year_end, "-12-31"))) 

#last check
stopifnot( all(siteinfo$year_start == floor(siteinfo$year_start)) )
stopifnot( all(siteinfo$year_end == floor(siteinfo$year_end)) )

calc_vpd_inst <- function( qair=NA, tc=NA, patm=NA, elv=NA  ){
  ##-----------------------------------------------------------------------
  ## Ref:      Eq. 5.1, Abtew and Meleese (2013), Ch. 5 Vapor Pressure 
  ##           Calculation Methods, in Evaporation and Evapotranspiration: 
  ##           Measurements and Estimations, Springer, London.
  ##             vpd = 0.611*exp[ (17.27 tc)/(tc + 237.3) ] - ea
  ##             where:
  ##                 tc = average daily air temperature, deg C
  ##                 eact  = actual vapor pressure, Pa
  ##-----------------------------------------------------------------------
  kTo = 288.15   # base temperature, K (Prentice, unpublished)
  kR  = 8.3143   # universal gas constant, J/mol/K (Allen, 1973)
  kMv = 18.02    # molecular weight of water vapor, g/mol (Tsilingiris, 2008)
  kMa = 28.963   # molecular weight of dry air, g/mol (Tsilingiris, 2008)
  
  ## calculate the mass mixing ratio of water vapor to dry air (dimensionless)
  wair <- qair / (1 - qair)
  
  ## calculate water vapor pressure 
  rv <- kR / kMv
  rd <- kR / kMa
  eact = patm * wair * rv / (rd + wair * rv)  
  
  ## calculate saturation water vapour pressure in Pa
  esat <- 611.0 * exp( (17.27 * tc)/(tc + 237.3) )
  
  ## calculate VPD in units of Pa
  vpd <- ( esat - eact )    
  
  ## this empirical equation may lead to negative values for VPD (happens very rarely). assume positive...
  vpd <- max( 0.0, vpd )
  
  return( vpd )
  
}

#library(doSNOW)
#NumberOfCluster <- 8
#cl <- makeCluster(NumberOfCluster, type='SOCK')
#registerDoSNOW(cl)
for (i in 1:nrow(siteinfo)) {
  tryCatch({
    df_watch <- ingestr::ingest(
      siteinfo  = siteinfo[i,],
      source    = "watch_wfdei",
      getvars   = list(temp = "Tair",prec = "Rainf", vpd = "Qair", ppfd = "SWin"), 
      dir       = "/Volumes/Seagate Backup Plus Drive/data/watch_wfdei/"
    )
    
    df_cru <- ingestr::ingest(
      siteinfo  = siteinfo[i,],
      source    = "cru",
      getvars   = list(ccov = "cld"),
      dir       = "/Volumes/Seagate Backup Plus Drive/data/cru/ts_4.01/"
    )
    
    df_co2 <- ingestr::ingest(
      siteinfo[i,],
      source  = "co2_mlo",
      verbose = FALSE
    )
    
    df_co2_final <- as.data.frame(df_co2$data)
    
    df_co2_final2 <- df_co2_final[!(format(df_co2_final$date,"%m") == "02" & format(df_co2_final$date, "%d") == "29"), , drop = FALSE]
    
    co2 <- df_co2_final2$co2
    
    ddf_meteo <- as_tibble(cbind(as.data.frame(df_watch$data)[,c("date","temp","prec","qair","ppfd")],as.data.frame(df_cru$data)[,c("ccov_int","ccov")],co2))
    
    elv <- siteinfo$elv[i]
    
    ddf_meteo$patm <- calc_patm(elv=elv, patm0 = 101325 )
    
    for (b in 1:length(ddf_meteo$qair)){
      ddf_meteo$vpd[b] <- calc_vpd_inst( qair=ddf_meteo$qair[b], tc=ddf_meteo$temp[b], ddf_meteo$patm[b], elv=elv)
    }
    
    ddf_meteo_final <- ddf_meteo[,c("date","temp","prec","qair","vpd","ppfd","patm","ccov_int","ccov","co2")]
    ddf_meteo_final$prec <- ddf_meteo_final$prec/86400
    ddf_meteo_final$ppfd <- ddf_meteo_final$ppfd/86400
    ddf_meteo_final$sitename <- NPP_forest_all$sitename[i]
    ddf_meteo_final$sitename2 <- NPP_forest_all$sitename2[i]
    csvfile <- paste("/Users/yunpeng/data/forest_npp/forcing/",siteinfo$sitename[i],".csv",sep = "")
    write.csv(ddf_meteo_final, csvfile, row.names = TRUE)
    print(i)    
  }, error=function(e){})} 


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

siteinfo <- data.frame(
  sitename = NPP_forest2$sitename2,
  lon = NPP_forest2$lon,
  lat = NPP_forest2$lat,
  year_start = rep(2010,nrow(NPP_forest2)),
  year_end = rep(2019,nrow(NPP_forest2))
)

#create time frame
siteinfo <-  siteinfo %>% dplyr::mutate(date_start = lubridate::ymd(paste0(year_start, "-01-01"))) %>%
  dplyr::mutate(date_end = lubridate::ymd(paste0(year_end, "-12-31"))) 

#df_modis_fpar <- ingest(siteinfo = siteinfo,source= "modis",settings  = settings_modis,verbose   = FALSE,parallel = TRUE,ncore = 8)
summary(siteinfo)

settings_modis <- get_settings_modis(
  bundle            = "modis_fpar",
  data_path         = "~/data/modis_forest2/",
  method_interpol   = "loess",
  keep              = TRUE,
  overwrite_raw     = FALSE,
  overwrite_interpol= TRUE
)

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


#Forcing - forest -->recreate siteinfo
dim(NPP_forest2)

siteinfo <- data.frame(
  sitename = NPP_forest2$sitename,
  lon = NPP_forest2$lon,
  lat = NPP_forest2$lat,
  elv = NPP_forest2$z,
  year_start = NPP_forest2$Begin_year,
  year_end = NPP_forest2$End_year
)

siteinfo$year_start[siteinfo$year_start<=1980] <- 1980
siteinfo$year_end[siteinfo$year_start<=1980] <- 1989

siteinfo <-  siteinfo %>% dplyr::mutate(date_start = lubridate::ymd(paste0(year_start, "-01-01"))) %>%
  dplyr::mutate(date_end = lubridate::ymd(paste0(year_end, "-12-31"))) 

csvfile <- paste("/Users/yunpeng/forest2_siteinfo.csv")
write.csv(siteinfo, csvfile, row.names = TRUE)

#last check
stopifnot( all(siteinfo$year_start == floor(siteinfo$year_start)) )
stopifnot( all(siteinfo$year_end == floor(siteinfo$year_end)) )

calc_vpd_inst <- function( qair=NA, tc=NA, patm=NA, elv=NA  ){
  ##-----------------------------------------------------------------------
  ## Ref:      Eq. 5.1, Abtew and Meleese (2013), Ch. 5 Vapor Pressure 
  ##           Calculation Methods, in Evaporation and Evapotranspiration: 
  ##           Measurements and Estimations, Springer, London.
  ##             vpd = 0.611*exp[ (17.27 tc)/(tc + 237.3) ] - ea
  ##             where:
  ##                 tc = average daily air temperature, deg C
  ##                 eact  = actual vapor pressure, Pa
  ##-----------------------------------------------------------------------
  kTo = 288.15   # base temperature, K (Prentice, unpublished)
  kR  = 8.3143   # universal gas constant, J/mol/K (Allen, 1973)
  kMv = 18.02    # molecular weight of water vapor, g/mol (Tsilingiris, 2008)
  kMa = 28.963   # molecular weight of dry air, g/mol (Tsilingiris, 2008)
  
  ## calculate the mass mixing ratio of water vapor to dry air (dimensionless)
  wair <- qair / (1 - qair)
  
  ## calculate water vapor pressure 
  rv <- kR / kMv
  rd <- kR / kMa
  eact = patm * wair * rv / (rd + wair * rv)  
  
  ## calculate saturation water vapour pressure in Pa
  esat <- 611.0 * exp( (17.27 * tc)/(tc + 237.3) )
  
  ## calculate VPD in units of Pa
  vpd <- ( esat - eact )    
  
  ## this empirical equation may lead to negative values for VPD (happens very rarely). assume positive...
  vpd <- max( 0.0, vpd )
  
  return( vpd )
  
}



#library(doSNOW)
#NumberOfCluster <- 8
#cl <- makeCluster(NumberOfCluster, type='SOCK')
#registerDoSNOW(cl)



for (i in 1:nrow(siteinfo)) {
  tryCatch({
    df_watch <- ingestr::ingest(
      siteinfo  = siteinfo[i,],
      source    = "watch_wfdei",
      getvars   = list(temp = "Tair",prec = "Rainf", vpd = "Qair", ppfd = "SWin"), 
      dir       = "/Volumes/Seagate Backup Plus Drive/data/watch_wfdei/"
    )
    
    df_cru <- ingestr::ingest(
      siteinfo  = siteinfo[i,],
      source    = "cru",
      getvars   = list(ccov = "cld"),
      dir       = "/Volumes/Seagate Backup Plus Drive/data/cru/ts_4.01/"
    )
    
    df_co2 <- ingestr::ingest(
      siteinfo[i,],
      source  = "co2_mlo",
      verbose = FALSE
    )
    
    df_co2_final <- as.data.frame(df_co2$data)
    
    df_co2_final2 <- df_co2_final[!(format(df_co2_final$date,"%m") == "02" & format(df_co2_final$date, "%d") == "29"), , drop = FALSE]
    
    co2 <- df_co2_final2$co2
    
    ddf_meteo <- as_tibble(cbind(as.data.frame(df_watch$data)[,c("date","temp","prec","qair","ppfd")],as.data.frame(df_cru$data)[,c("ccov_int","ccov")],co2))
    
    elv <- siteinfo$elv[i]
    
    ddf_meteo$patm <- calc_patm(elv=elv, patm0 = 101325 )
    
    for (b in 1:length(ddf_meteo$qair)){
      ddf_meteo$vpd[b] <- calc_vpd_inst( qair=ddf_meteo$qair[b], tc=ddf_meteo$temp[b], ddf_meteo$patm[b], elv=elv)
    }
    
    ddf_meteo_final <- ddf_meteo[,c("date","temp","prec","qair","vpd","ppfd","patm","ccov_int","ccov","co2")]
    ddf_meteo_final$prec <- ddf_meteo_final$prec/86400
    ddf_meteo_final$ppfd <- ddf_meteo_final$ppfd/86400
    ddf_meteo_final$sitename <- NPP_forest_all$sitename[i]
    ddf_meteo_final$sitename2 <- NPP_forest_all$sitename2[i]
    csvfile <- paste("/Users/yunpeng/data/forest_npp/forcing/",siteinfo$sitename[i],".csv",sep = "")
    write.csv(ddf_meteo_final, csvfile, row.names = TRUE)
    print(i)    
  }, error=function(e){})} 