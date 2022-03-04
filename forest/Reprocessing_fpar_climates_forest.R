#read forest csv as obtained from site simulation of forest (see forest_npp/Forest_site_check.R, L1-L410)
rm(list=ls())
library(dplyr)

#it has now newly correcting coordinates
NPP_Forest_corrected  <- read.csv("/Users/yunpeng/data/forest_npp/NPP_Forest_corrected_Malhi_coord.csv")
NPP_Forest_corrected$no <- c(1:nrow(NPP_Forest_corrected))
#NPP_Forest_old  <- read.csv("/Users/yunpeng/data/forest_npp/NPP_forest.csv")
#NPP_Forest_corrected$lon - NPP_Forest_old$lon
#summary(NPP_Forest_corrected$GPP - NPP_Forest_old$GPP)
#summary(NPP_Forest_corrected$TNPP_1 - NPP_Forest_old$TNPP_1)
#summary(NPP_Forest_corrected$lnf_obs_final - NPP_Forest_old$lnf_obs_final)

NPP_Forest_corrected$sitename <- NA
for (i in 1:nrow(NPP_Forest_corrected)){
  NPP_Forest_corrected$sitename[i] <- paste("NPP_F",i,sep = "")
}


siteinfo <- data.frame(
  sitename = NPP_Forest_corrected$sitename,
  lon = NPP_Forest_corrected$lon,
  lat = NPP_Forest_corrected$lat,
  elv = NPP_Forest_corrected$z,
  year_start = NPP_Forest_corrected$Begin_year,
  year_end = NPP_Forest_corrected$End_year
)
siteinfo$no <- c(1:nrow(siteinfo))

siteinfo$year_start[siteinfo$year_start<=1980] <- 1980
siteinfo$year_end[siteinfo$year_start<=1980] <- 1989

#aggregate to siteinfo2 - which will be used in fapar collection
siteinfo2 <- aggregate(siteinfo,by=list(siteinfo$lon,siteinfo$lat), FUN=mean, na.rm=TRUE) #site-mean

dim(siteinfo2)

for (i in 1:nrow(siteinfo2)){
  siteinfo2$sitename_fpar[i] <- paste("ingestr_fpar",i,sep = "")
}

siteinfo3 <- siteinfo2[,c("sitename_fpar","lon","lat")]

NPP_f_all <-Reduce(function(x,y) merge(x = x, y = y, by = c("lon","lat"),all.x=TRUE), 
                           list(siteinfo,siteinfo3))

NPP_f_all <- NPP_f_all[order(NPP_f_all$no), ]

#output and copy it to "/Users/yunpeng/data/forest_npp/NPP_Forest_corrected_Malhi_coord.csv" to make it better to be used in further rsofun site simulation in forest
csvfile <- paste("~/data/forest_npp/NPP_Forest_sitename_fpar.csv")
write_csv(NPP_f_all, path = csvfile)

#install projects from /Users/yunpeng/yunkepeng/Grassland_new_ingestr_rsofun_20210326/ingestr - commit: 20200324

#1. ingest fpar
settings_modis <- get_settings_modis(
  bundle            = "modis_fpar",
  data_path         = "~/data/forest_npp/reprocessing_fpar/",
  method_interpol   = "loess",
  keep              = TRUE,
  overwrite_raw     = FALSE,
  overwrite_interpol= TRUE,
  n_focal = 0
)


#now, fapar
head(siteinfo3)
dim(siteinfo3)

library(doSNOW)
NumberOfCluster <- 8
cl <- makeCluster(NumberOfCluster, type='SOCK')
registerDoSNOW(cl)
x0 <- foreach(i = c(1:nrow(siteinfo3)),.combine = "rbind") %dopar% {
  devtools::load_all(".")
  library(dplyr)
  df_modis_fpar <- ingest_bysite(
    sitename  = siteinfo3[i,c("sitename_fpar")],
    source    = "modis",
    year_start = 2001,
    year_end  =  2015,
    lon       = siteinfo3[i,c("lon")],
    lat       = siteinfo3[i,c("lat")],
    settings  = settings_modis,
    verbose   = FALSE
  )
}

#some data is not available for n_focal = 0 , change it to n_focal = 1, then n_focal =2, and save it.
#n_focal = 1
settings_modis <- get_settings_modis(
  bundle            = "modis_fpar",data_path = "~/data/forest_npp/reprocessing_fpar_raw/",method_interpol = "loess",keep = TRUE,overwrite_raw = FALSE,overwrite_interpol= TRUE,
  n_focal = 1)

missing_nfocal <- c(499,475,80,54,108,465,449,450,448,55,196,137,249,101,125)

for (i in missing_nfocal) {
  tryCatch({
    df_modis_fpar <- ingest_bysite(sitename  = siteinfo3[i,c("sitename_fpar")],source= "modis",year_start = 2001,year_end  =  2015,lon= siteinfo3[i,c("lon")],lat       = siteinfo3[i,c("lat")],
      settings  = settings_modis,
      verbose   = FALSE
    )
  }, error=function(e){})} 

#n_focal = 2
settings_modis <- get_settings_modis(
  bundle            = "modis_fpar",data_path = "~/data/forest_npp/reprocessing_fpar_raw/",method_interpol = "loess",keep = TRUE,overwrite_raw = FALSE,overwrite_interpol= TRUE,
  n_focal = 2)
missing_nfocal <- c(249,55,80,448,449,450,125)

for (i in missing_nfocal) {
  tryCatch({
    df_modis_fpar <- ingest_bysite(sitename  = siteinfo3[i,c("sitename_fpar")],source= "modis",year_start = 2001,year_end  =  2015,lon= siteinfo3[i,c("lon")],lat       = siteinfo3[i,c("lat")],
                                   settings  = settings_modis,
                                   verbose   = FALSE
    )
  }, error=function(e){})} 

#125,449,448 works for n_focal =2,rest of them is na


##################
#now, for climate forcing
#change some parameters in dir, since we newly copied Psurf now.
#newly including org. dataset, using original measurement year (<1980, shall still be corrected. Of course)

#this version above is consistent with what we processing in last simulation

siteinfo_final <- siteinfo[,c("sitename","lon","lat","elv","year_start","year_end")] 
head(siteinfo_final)
dim(siteinfo_final) 

siteinfo_final <-  siteinfo %>% dplyr::mutate(date_start = lubridate::ymd(paste0(year_start, "-01-01"))) %>%
  dplyr::mutate(date_end = lubridate::ymd(paste0(year_end, "-12-31"))) 



#last check
stopifnot( all(siteinfo_final$year_start == floor(siteinfo_final$year_start)) )
stopifnot( all(siteinfo_final$year_end == floor(siteinfo_final$year_end)) )


#devtools::load_all(".")

siteinfo_final$whc <- 170

library(doSNOW)
NumberOfCluster <- 4
cl <- makeCluster(NumberOfCluster, type='SOCK')
registerDoSNOW(cl)
x0 <- foreach(i = diff,.combine = "rbind") %dopar% {
  devtools::load_all(".")
  df_watch <- ingest(
    siteinfo  = siteinfo_final[i,],
    source    = "watch_wfdei",
    getvars   = c("temp", "prec", "ppfd", "vpd", "patm"), 
    dir       = "/Volumes/My Passport/data/watch_wfdei/",
    settings  = list(correct_bias = "worldclim", dir_bias = "/Volumes/My Passport/data/worldclim/"))
  
  df_cru <- ingest(
    siteinfo  = siteinfo_final[i,],
    source    = "cru",
    getvars   = "ccov",
    dir       = "/Volumes/My Passport/data/cru/ts_4.01/")
  
  df_co2 <- ingest(
    siteinfo  = siteinfo_final[i,],
    source  = "co2_mlo",
    verbose = FALSE)
  
  df_co2_final <- as.data.frame(df_co2$data)
  
  df_co2_final2 <- df_co2_final[!(format(df_co2_final$date,"%m") == "02" & format(df_co2_final$date, "%d") == "29"), , drop = FALSE] # make columns consistent
  
  co2 <- df_co2_final2$co2
  
  ddf_meteo <- as_tibble(cbind(as.data.frame(df_watch$data),as.data.frame(df_cru$data)[,c("ccov_int","ccov")],co2))
  
  ddf_meteo$sitename <- siteinfo_final$sitename[i]
  csvfile <- paste("~/data/forest_npp/reprocessing_climates/",siteinfo_final$sitename[i],".csv",sep = "")
  write.csv(ddf_meteo, csvfile, row.names = TRUE)
  print(i)    
}

stopCluster(cl)


#check missing points
climate_df <- list.files("~/data/forest_npp/reprocessing_climates/",full.names = T)
empty_vec <- c()
#check existed climate files
for (i in 1:(length(climate_df))){
  empty_vec[i] <- as.numeric(gsub("[^0-9]", "",  climate_df[i]))
}

diff <- setdiff(1:935, empty_vec)
length(diff)



for (i in diff) {
  tryCatch({
    df_watch <- ingest(
      siteinfo  = siteinfo_final[i,],
      source    = "watch_wfdei",
      getvars   = c("temp", "prec", "ppfd", "vpd", "patm"), 
      dir       = "/Volumes/My Passport/data/watch_wfdei/",
      settings  = list(correct_bias = "worldclim", dir_bias = "/Volumes/My Passport/data/worldclim/"))
    
    df_cru <- ingest(
      siteinfo  = siteinfo_final[i,],
      source    = "cru",
      getvars   = "ccov",
      dir       = "/Volumes/My Passport/data/cru/ts_4.01/")
    
    df_co2 <- ingest(
      siteinfo  = siteinfo_final[i,],
      source  = "co2_mlo",
      verbose = FALSE)
    
    df_co2_final <- as.data.frame(df_co2$data)
    
    df_co2_final2 <- df_co2_final[!(format(df_co2_final$date,"%m") == "02" & format(df_co2_final$date, "%d") == "29"), , drop = FALSE] # make columns consistent
    
    co2 <- df_co2_final2$co2
    
    ddf_meteo <- as_tibble(cbind(as.data.frame(df_watch$data),as.data.frame(df_cru$data)[,c("ccov_int","ccov")],co2))
    
    ddf_meteo$sitename <- siteinfo_final$sitename[i]
    csvfile <- paste("~/data/forest_npp/reprocessing_climates/",siteinfo_final$sitename[i],".csv",sep = "")
    write.csv(ddf_meteo, csvfile, row.names = TRUE)
    print(i)    
  }, error=function(e){})} 


###reprocessing Nmin rate's climate and fapar forcing. 
#It includes both forest and grassland - need division here.

#(1) firstly, include Finzi's 81 points.
Finzi <- read.csv("/Users/yunpeng/data/NPP_Yunke/Nmin_ Finzi/Nmin_Finzi.csv")
names(Finzi)[names(Finzi) == "Lat"] <- "lat"
names(Finzi)[names(Finzi) == "Long"] <- "lon"

#See Gill and Finzi Fig3 for pft info 
#Grassland
Finzi_Grassland <- subset(Finzi, Biome=="temp grass")
Finzi_Grassland_sitemean <- aggregate(Finzi_Grassland,by=list(Finzi_Grassland$lon,Finzi_Grassland$lat), FUN=mean, na.rm=TRUE) #site-mean
dim(Finzi_Grassland_sitemean)
for (i in 1:nrow(Finzi_Grassland_sitemean)){
  Finzi_Grassland_sitemean$sitename[i] <- paste("Finzi_Grass",i,sep = "") # this is also sitename for fpar
  Finzi_Grassland_sitemean$sitename_climate[i] <- paste("Finzi_Grass_climate",i,sep = "")
}
df_etopo <- ingest(Finzi_Grassland_sitemean,source = "etopo1",dir = "~/data/etopo/" )
Finzi_Grassland_sitemean$elv <- as.numeric(as.data.frame(df_etopo$data))
Finzi_Grassland_sitemean

#Forest
Finzi_Forest <- subset(Finzi, Biome!="temp grass")
Finzi_Forest_sitemean <- aggregate(Finzi_Forest,by=list(Finzi_Forest$lon,Finzi_Forest$lat), FUN=mean, na.rm=TRUE) #site-mean
dim(Finzi_Forest_sitemean)
for (i in 1:nrow(Finzi_Forest_sitemean)){
  Finzi_Forest_sitemean$sitename[i] <- paste("Finzi_Forest",i,sep = "") # this is also sitename for fpar
  Finzi_Forest_sitemean$sitename_climate[i] <- paste("Finzi_Forest_climate",i,sep = "")
  
}
df_etopo <- ingest(Finzi_Forest_sitemean,source = "etopo1",dir = "~/data/etopo/" )
Finzi_Forest_sitemean$elv <- as.numeric(as.data.frame(df_etopo$data))
Finzi_Forest_sitemean$elv[Finzi_Forest_sitemean$elv< 0] <- 0
Finzi_Forest_sitemean

#let's bind them firstly - and all saved it into ~/data/forest_npp/reprocessing_Nmin/fapar. Just to save our work!
Finzi_final <- dplyr::bind_rows(Finzi_Forest_sitemean, Finzi_Grassland_sitemean)
library(maps)
library(rworldmap)
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-180, 180), ylim = c(-75, 75), asp = 1)
points(Finzi_final$lon,Finzi_final$lat, col="red", pch=16,cex=1)

#prepare siteinfo - the earlist and latest publication is 1984 and 2012. So we choose 1984-2013 (30 years)
siteinfo_Finzi <- data.frame(
  sitename = Finzi_final$sitename,
  sitename_climate = Finzi_final$sitename_climate,
  lon = Finzi_final$lon,
  lat = Finzi_final$lat,
  elv = Finzi_final$elv,
  year_start = 1984,
  year_end = 2013
)

#1. ingest fpar
settings_modis <- get_settings_modis(
  bundle            = "modis_fpar",
  data_path         = "~/data/NPP_Yunke/Nmin_Finzi/reprocessing_Nmin/fapar/",
  method_interpol   = "loess",
  keep              = TRUE,
  overwrite_raw     = FALSE,
  overwrite_interpol= TRUE,
  n_focal = 2
)

library(doSNOW)
NumberOfCluster <- 4
cl <- makeCluster(NumberOfCluster, type='SOCK')
registerDoSNOW(cl)
x0 <- foreach(i = c(5,6,19,43,49,20,44,7,26),.combine = "rbind") %dopar% {
  devtools::load_all("/Users/yunpeng/yunkepeng/Grassland_new_ingestr_rsofun_20210326/ingestr/")
  library(dplyr)
  df_modis_fpar <- ingest_bysite(
    sitename  = siteinfo_Finzi[i,c("sitename")],
    source    = "modis",
    year_start = 2001,
    year_end  =  2015,
    lon       = siteinfo_Finzi[i,c("lon")],
    lat       = siteinfo_Finzi[i,c("lat")],
    settings  = settings_modis,
    verbose   = FALSE
  )
}
stopCluster(cl)

#2. ingest climate
climate_df <- list.files("/Users/yunpeng/data/NPP_Yunke/Nmin_Finzi/reprocessing_Nmin/climates/",full.names = T)
empty_vec <- c()
#check existed climate files
for (i in 1:(length(climate_df))){
  empty_vec[i] <- as.numeric(gsub("[^0-9]", "",  climate_df[i]))
}

diff <- setdiff(1:87, empty_vec)
diff

siteinfo_Finzi$whc <- 170
library(doSNOW)
NumberOfCluster <- 2
cl <- makeCluster(NumberOfCluster, type='SOCK')
registerDoSNOW(cl)
x0 <- foreach(i = c(1:105),.combine = "rbind") %dopar% {
  devtools::load_all(".")
  df_watch <- ingest(
    siteinfo  = siteinfo_Finzi[i,],
    source    = "watch_wfdei",
    getvars   = c("temp", "prec", "ppfd", "vpd", "patm"), 
    dir       = "/Volumes/My Passport/data/watch_wfdei/",
    settings  = list(correct_bias = "worldclim", dir_bias = "/Volumes/My Passport/data/worldclim/"))
  
  df_cru <- ingest(
    siteinfo  = siteinfo_Finzi[i,],
    source    = "cru",
    getvars   = "ccov",
    dir       = "/Volumes/My Passport/data/cru/ts_4.01/")
  
  df_co2 <- ingest(
    siteinfo  = siteinfo_Finzi[i,],
    source  = "co2_mlo",
    verbose = FALSE)
  
  df_co2_final <- as.data.frame(df_co2$data)
  
  df_co2_final2 <- df_co2_final[!(format(df_co2_final$date,"%m") == "02" & format(df_co2_final$date, "%d") == "29"), , drop = FALSE] # make columns consistent
  
  co2 <- df_co2_final2$co2
  
  ddf_meteo <- as_tibble(cbind(as.data.frame(df_watch$data),as.data.frame(df_cru$data)[,c("ccov_int","ccov")],co2))
  
  ddf_meteo$sitename <- siteinfo_Finzi$sitename_climate[i]
  csvfile <- paste("~/data/NPP_Yunke/Nmin_ Finzi/reprocessing_Nmin/climates/",siteinfo_Finzi$sitename_climate[i],".csv",sep = "")
  write.csv(ddf_meteo, csvfile, row.names = TRUE)
  print(i)    
}
stopCluster(cl)

#now saved to /Users/yunpeng/data/NPP_Yunke/Nmin_ Finzi/reprocessing_Nmin

####
#add more, from gcme project Nuptake
gcme_nuptake <- read.csv("/Users/yunpeng/data/NPP_Yunke/Nuptake_gcme/gcme_nuptake_coord_interpolated.csv")
library(maps)
library(rworldmap)
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-180, 180), ylim = c(-75, 75), asp = 1)
points(gcme_nuptake$lon,gcme_nuptake$lat, col="red", pch=16,cex=1)

#prepare siteinfo - the earlist and latest publication is 1984 and 2012. So we choose 1984-2013 (30 years)
for (i in 1:nrow(gcme_nuptake)){
  gcme_nuptake$sitename[i] <- paste("gcme",i,sep = "") # this is also sitename for fpar
  gcme_nuptake$sitename_climate[i] <- paste("gcme_climate",i,sep = "")
  
}

#elv
df_etopo <- ingest(gcme_nuptake,source = "etopo1",dir = "~/data/etopo/" )
gcme_nuptake$elv <- as.numeric(as.data.frame(df_etopo$data))
gcme_nuptake$elv[gcme_nuptake$elv<0] <- 0
gcme_nuptake$elv

siteinfo_gcme <- data.frame(
  sitename = gcme_nuptake$sitename,
  sitename_climate = gcme_nuptake$sitename_climate,
  lon = gcme_nuptake$lon,
  lat = gcme_nuptake$lat,
  elv = gcme_nuptake$elv,
  year_start = gcme_nuptake$start_yr,
  year_end = gcme_nuptake$start_yr + gcme_nuptake$Year_long -1
)

#1. ingest fpar
settings_modis <- get_settings_modis(
  bundle            = "modis_fpar",
  data_path         = "/Users/yunpeng/data/NPP_Yunke/Nuptake_gcme/fapar/",
  method_interpol   = "loess",
  keep              = TRUE,
  overwrite_raw     = FALSE,
  overwrite_interpol= TRUE,
  n_focal = 2
)

library(doSNOW)
NumberOfCluster <- 4
cl <- makeCluster(NumberOfCluster, type='SOCK')
registerDoSNOW(cl)
x0 <- foreach(i = c(17,32),.combine = "rbind") %dopar% {
  devtools::load_all("/Users/yunpeng/yunkepeng/Grassland_new_ingestr_rsofun_20210326/ingestr/")
  library(dplyr)
  df_modis_fpar <- ingest_bysite(
    sitename  = siteinfo_gcme[i,c("sitename")],
    source    = "modis",
    year_start = 2001,
    year_end  =  2015,
    lon       = siteinfo_gcme[i,c("lon")],
    lat       = siteinfo_gcme[i,c("lat")],
    settings  = settings_modis,
    verbose   = FALSE
  )
}

stopCluster(cl)

#2. ingest climate
siteinfo_gcme$whc <- 170
library(doSNOW)
NumberOfCluster <- 4
cl <- makeCluster(NumberOfCluster, type='SOCK')
registerDoSNOW(cl)
x0 <- foreach(i = 1:nrow(siteinfo_gcme),.combine = "rbind") %dopar% {
  devtools::load_all(".")
  df_watch <- ingest(
    siteinfo  = siteinfo_gcme[i,],
    source    = "watch_wfdei",
    getvars   = c("temp", "prec", "ppfd", "vpd", "patm"), 
    dir       = "/Volumes/My Passport/data/watch_wfdei/",
    settings  = list(correct_bias = "worldclim", dir_bias = "/Volumes/My Passport/data/worldclim/"))
  
  df_cru <- ingest(
    siteinfo  = siteinfo_gcme[i,],
    source    = "cru",
    getvars   = "ccov",
    dir       = "/Volumes/My Passport/data/cru/ts_4.01/")
  
  df_co2 <- ingest(
    siteinfo  = siteinfo_gcme[i,],
    source  = "co2_mlo",
    verbose = FALSE)
  
  df_co2_final <- as.data.frame(df_co2$data)
  
  df_co2_final2 <- df_co2_final[!(format(df_co2_final$date,"%m") == "02" & format(df_co2_final$date, "%d") == "29"), , drop = FALSE] # make columns consistent
  
  co2 <- df_co2_final2$co2
  
  ddf_meteo <- as_tibble(cbind(as.data.frame(df_watch$data),as.data.frame(df_cru$data)[,c("ccov_int","ccov")],co2))
  
  ddf_meteo$sitename <- siteinfo_gcme$sitename_climate[i]
  csvfile <- paste("~/data/NPP_Yunke/Nuptake_gcme/climates/",siteinfo_gcme$sitename_climate[i],".csv",sep = "")
  write.csv(ddf_meteo, csvfile, row.names = TRUE)
  print(i)    
}
stopCluster(cl)

