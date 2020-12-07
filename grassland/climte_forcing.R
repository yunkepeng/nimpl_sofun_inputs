###line 1 to 144 - same as above.

#firstly, including all sites that need to extract fapar
##it includes two components:
#type a (sitename: a1, a2, a3....) - all grassland sites, which will be used to calculate its site GPP later on, to investigate its statistical model of npp/gpp and anpp/gpp.
#type b (sitename: b1, b2, b3....) - all forest sites (used in data validation), because we already finished global simulation predidction vs. observation - but we now further want to see if site simulated GPP would make model performance better.


#1. Input data
library(dplyr)
library(ingestr)

rm(list=ls())
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
  NPP_final$sitename[i] <- paste("NPP",i,sep = "")
}

#select grassland sites only -> this object is what we used to extract all climate forcing
NPP_grassland <- subset(NPP_final,pft2=="Grassland")
dim(NPP_grassland)

#extract site mean of grasslands -> this object is what we used to extract fapar
NPP_final2 <- aggregate(NPP_grassland,by=list(NPP_grassland$lon,NPP_grassland$lat), FUN=mean, na.rm=TRUE) #site-mean

for (i in 1:nrow(NPP_final2)){
  NPP_final2$sitename2[i] <- paste("grassland",i,sep = "")
}

summary(NPP_grassland)

######SITE FORCING
#options(warn=-1)#remove warning - please return back on below!!!!
#options(warn=0)
#now, start preparing siteinfo for climate focring, based on NPP_grassland

#merge NPP_grassland with NPP_final2 firstly, to get site file name of fapar
head(NPP_final2)
NPP_final3 <- NPP_final2[,c("lon","lat","sitename2")]

NPP_grassland2 <- merge(NPP_grassland,NPP_final3, all.x = TRUE)

head(NPP_grassland2) #now, added attribute of fapar file
dim(NPP_grassland) 

siteinfo <- data.frame(
  sitename = NPP_grassland2$sitename,
  lon = NPP_grassland2$lon,
  lat = NPP_grassland2$lat,
  elv = NPP_grassland2$z,
  year_start = NPP_grassland2$Begin_year,
  year_end = NPP_grassland2$End_year
)

#create time frame
siteinfo <-  siteinfo %>% dplyr::mutate(date_start = lubridate::ymd(paste0(year_start, "-01-01"))) %>%
  dplyr::mutate(date_end = lubridate::ymd(paste0(year_end, "-12-31")))  ## add info

siteinfo

nrow(siteinfo)

#vpd function
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

#4. get input
library(doSNOW)
NumberOfCluster <- 8
cl <- makeCluster(NumberOfCluster, type='SOCK')
registerDoSNOW(cl)
x0 <- foreach(a = 1:nrow(siteinfo),.combine = "rbind") %dopar% {
  library(ingestr)
  library(dplyr)
  df_watch <- ingestr::ingest(
    siteinfo  = siteinfo[a,],
    source    = "watch_wfdei",
    getvars   = list(temp = "Tair",prec = "Rainf", vpd = "Qair", ppfd = "SWin"), 
    dir       = "/Volumes/Seagate Backup Plus Drive/data/watch_wfdei/"
  )
  
  df_cru <- ingestr::ingest(
    siteinfo  = siteinfo[a,],
    source    = "cru",
    getvars   = list(ccov = "cld"),
    dir       = "/Volumes/Seagate Backup Plus Drive/data/cru/ts_4.01/"
  )
  
  df_co2 <- ingestr::ingest(
    siteinfo[a,],
    source  = "co2_mlo",
    verbose = FALSE
  )
  
  df_co2_final <- as.data.frame(df_co2$data)
  
  df_co2_final2 <- df_co2_final[!(format(df_co2_final$date,"%m") == "02" & format(df_co2_final$date, "%d") == "29"), , drop = FALSE]
  
  co2 <- df_co2_final2$co2
  
  ddf_meteo <- as_tibble(cbind(as.data.frame(df_watch$data)[,c("date","temp","prec","qair","ppfd")],as.data.frame(df_cru$data)[,c("ccov_int","ccov")],co2))
  
  ddf_meteo$patm <- calc_patm(elv=siteinfo$elv[a], patm0 = 101325 )
  
  for (b in 1:length(ddf_meteo$qair)){
    ddf_meteo$vpd[b] <- calc_vpd_inst( qair=ddf_meteo$qair[b], tc=ddf_meteo$temp[b], ddf_meteo$patm[b], elv=siteinfo$elv[b])
  }
  
  ddf_meteo_final <- ddf_meteo[,c("date","temp","prec","vpd","ppfd","patm","ccov_int","ccov","co2")]
  ddf_meteo_final$prec <- ddf_meteo_final$prec/86400
  ddf_meteo_final$ppfd <- ddf_meteo_final$ppfd/86400
  ddf_meteo_final$sitename <- NPP_grassland2$sitename[a]
  ddf_meteo_final$sitename2 <- NPP_grassland2$sitename2[a]
  csvfile <- paste("/Users/yunpeng/data/grassland/forcing/",siteinfo$sitename[a],".csv",sep = "")
  write.csv(ddf_meteo_final, csvfile, row.names = TRUE)
}
stopCluster(cl)

#nor run...
'''
test <- vector()
for (i in 1:length(climatelist)){
  test[i] <- as.data.frame(read.csv(climatelist[[i]]))[2,11]
}
test2 <- as.data.frame(test)
names(test2) <- c("sitename")
test2$info <- "yes"

test3 <- merge(siteinfo,test2, all.x = TRUE)

siteinfo2 <- subset(test3,is.na(info)==TRUE)
dim(siteinfo2)
siteinfo3 <- siteinfo2[,1:8]
head(siteinfo3)

fpar_site <-NPP_grassland2[,c("sitename","sitename2")]
head(fpar_site)

siteinfo4 <- merge(siteinfo3,fpar_site,all.x=TRUE)

siteinfo3 <- siteinfo4[,1:8]
head(siteinfo3)

for (a in 1:nrow(siteinfo3)) {
  tryCatch({
    df_watch <- ingestr::ingest(
      siteinfo  = siteinfo3[a,],
      source    = "watch_wfdei",
      getvars   = list(temp = "Tair",prec = "Rainf", vpd = "Qair", ppfd = "SWin"), 
      dir       = "/Volumes/Seagate Backup Plus Drive/data/watch_wfdei/"
    )
    
    df_cru <- ingestr::ingest(
      siteinfo  = siteinfo3[a,],
      source    = "cru",
      getvars   = list(ccov = "cld"),
      dir       = "/Volumes/Seagate Backup Plus Drive/data/cru/ts_4.01/"
    )
    
    df_co2 <- ingestr::ingest(
      siteinfo3[a,],
      source  = "co2_mlo",
      verbose = FALSE
    )
    
    df_co2_final <- as.data.frame(df_co2$data)
    
    df_co2_final2 <- df_co2_final[!(format(df_co2_final$date,"%m") == "02" & format(df_co2_final$date, "%d") == "29"), , drop = FALSE]
    
    co2 <- df_co2_final2$co2
    
    ddf_meteo <- as_tibble(cbind(as.data.frame(df_watch$data)[,c("date","temp","prec","qair","ppfd")],as.data.frame(df_cru$data)[,c("ccov_int","ccov")],co2))
    
    ddf_meteo$patm <- calc_patm(elv=siteinfo3$elv[a], patm0 = 101325 )
    
    for (b in 1:length(ddf_meteo$qair)){
      ddf_meteo$vpd[b] <- calc_vpd_inst( qair=ddf_meteo$qair[b], tc=ddf_meteo$temp[b], ddf_meteo$patm[b], elv=siteinfo3$elv[b])
    }
    
    ddf_meteo_final <- ddf_meteo[,c("date","temp","prec","vpd","ppfd","patm","ccov_int","ccov","co2")]
    ddf_meteo_final$prec <- ddf_meteo_final$prec/86400
    ddf_meteo_final$ppfd <- ddf_meteo_final$ppfd/86400
    ddf_meteo_final$sitename <- siteinfo3$sitename[a]
    ddf_meteo_final$sitename2 <- siteinfo4$sitename2[a]
    csvfile <- paste("/Users/yunpeng/data/grassland/forcing2/",siteinfo3$sitename[a],".csv",sep = "")
    write.csv(ddf_meteo_final, csvfile, row.names = TRUE)
    print(a)
  }, error=function(e){})} 

siteinfo5 <- subset(siteinfo3,year_start<1979)
siteinfo5$year_start = 1980
siteinfo5$year_end = 1989

siteinfo5 <-  siteinfo5 %>% dplyr::mutate(date_start = lubridate::ymd(paste0(year_start, "-01-01"))) %>%
  dplyr::mutate(date_end = lubridate::ymd(paste0(year_end, "-12-31")))  ## add info

siteinfo5

siteinfo6 <- merge(siteinfo5,fpar_site,all.x=TRUE)

siteinfo5 <- siteinfo6[,1:8]


for (a in 1:nrow(siteinfo5)) {
  tryCatch({
    df_watch <- ingestr::ingest(
      siteinfo  = siteinfo5[a,],
      source    = "watch_wfdei",
      getvars   = list(temp = "Tair",prec = "Rainf", vpd = "Qair", ppfd = "SWin"), 
      dir       = "/Volumes/Seagate Backup Plus Drive/data/watch_wfdei/"
    )
    
    df_cru <- ingestr::ingest(
      siteinfo  = siteinfo5[a,],
      source    = "cru",
      getvars   = list(ccov = "cld"),
      dir       = "/Volumes/Seagate Backup Plus Drive/data/cru/ts_4.01/"
    )
    
    df_co2 <- ingestr::ingest(
      siteinfo5[a,],
      source  = "co2_mlo",
      verbose = FALSE
    )
    
    df_co2_final <- as.data.frame(df_co2$data)
    
    df_co2_final2 <- df_co2_final[!(format(df_co2_final$date,"%m") == "02" & format(df_co2_final$date, "%d") == "29"), , drop = FALSE]
    
    co2 <- df_co2_final2$co2
    
    ddf_meteo <- as_tibble(cbind(as.data.frame(df_watch$data)[,c("date","temp","prec","qair","ppfd")],as.data.frame(df_cru$data)[,c("ccov_int","ccov")],co2))
    
    ddf_meteo$patm <- calc_patm(elv=siteinfo5$elv[a], patm0 = 101325 )
    
    for (b in 1:length(ddf_meteo$qair)){
      ddf_meteo$vpd[b] <- calc_vpd_inst( qair=ddf_meteo$qair[b], tc=ddf_meteo$temp[b], ddf_meteo$patm[b], elv=siteinfo5$elv[b])
    }
    
    ddf_meteo_final <- ddf_meteo[,c("date","temp","prec","vpd","ppfd","patm","ccov_int","ccov","co2")]
    ddf_meteo_final$prec <- ddf_meteo_final$prec/86400
    ddf_meteo_final$ppfd <- ddf_meteo_final$ppfd/86400
    ddf_meteo_final$sitename <- siteinfo5$sitename[a]
    ddf_meteo_final$sitename2 <- siteinfo6$sitename2[a]
    csvfile <- paste("/Users/yunpeng/data/grassland/forcing3/",siteinfo5$sitename[a],".csv",sep = "")
    write.csv(ddf_meteo_final, csvfile, row.names = TRUE)
    print(a)
  }, error=function(e){})} 

