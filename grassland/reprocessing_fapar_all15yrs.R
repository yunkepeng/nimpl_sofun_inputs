rm(list=ls())
library(dplyr)
NPP_grassland  <- read.csv("/Users/yunpeng/data/grassland_npp/NPP_grassland.csv")

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
dim(siteinfo_final) #485 sites to be obtained

#last check
stopifnot( all(siteinfo_final$year_start == floor(siteinfo_final$year_start)) )
stopifnot( all(siteinfo_final$year_end == floor(siteinfo_final$year_end)) )

#1. ingest fpar based on n_focal = 0
settings_modis <- get_settings_modis(
  bundle            = "modis_fpar",
  data_path         = "~/data/grassland_npp/reprocessing_fapar_01_15/nfocal_0/",
  method_interpol   = "loess",
  keep              = TRUE,
  overwrite_raw     = FALSE,
  overwrite_interpol= TRUE,
  n_focal = 0
)


#now, fapar
library(doSNOW)
NumberOfCluster <- 8
cl <- makeCluster(NumberOfCluster, type='SOCK')
registerDoSNOW(cl)
x0 <- foreach(i = c(1:485),.combine = "rbind") %dopar% {
  devtools::load_all(".")
  library(dplyr)
  df_modis_fpar <- ingest_bysite(
    sitename  = siteinfo_final[i,c("sitename")],
    source    = "modis",
    year_start = 2001,
    year_end  = 2015,
    lon       = siteinfo_final[i,c("lon")],
    lat       = siteinfo_final[i,c("lat")],
    settings  = settings_modis,
    verbose   = FALSE
  )
}
stopCluster(cl)


#some ingestr sites were (1) missing for csv (2) by-passing or (3) all values for outputted csv's modisvar_filled in NA (due to n_focal = 0?).
#Let's fill them
fapar_df <- list.files("~/data/grassland_npp/reprocessing_fapar_01_15/nfocal_0/",full.names = T)
length(fapar_df)-1

#fapar
for (i in 1:(length(fapar_df)-1)){
  df1 <- read.csv(fapar_df[i])
  df1$date <- as.Date(df1$date)
  df1 <- df1[!(format(df1$date,"%m") == "02" & format(df1$date, "%d") == "29"), , drop = FALSE]
  df2 <- df1 %>% mutate(ymonth = month(date),
                        yday = day(date)) %>% 
    group_by(ymonth, yday) %>% 
    summarise(fpar = mean(modisvar_filled, na.rm = TRUE))
  assign(substr(sub('.*daily_', '', fapar_df[i]),1,nchar(sub('.*daily_', '', fapar_df[i]))-4), df2) 
}

#fapar - check missing csv and output it into ingestr1,2,3... check if na happens within csv and output it as na_ingestr1.
for (i in 1:nrow(siteinfo_final)){
  siteinfo_final$fpar_avil[i] <- exists(paste("ingestr",i,sep = ""))
  if (siteinfo_final$fpar_avil[i] == TRUE) {
    na_check <- (eval(parse(text=(paste("ingestr",i,sep = "")))))
    siteinfo_final$na_avil[i] <- mean(na_check$fpar)
  } else {
    siteinfo_final$na_avil[i] <- "NaN"
  }
}

#select (1) missing condition
subset(siteinfo_final,fpar_avil=="FALSE")
dim(subset(siteinfo_final,fpar_avil=="FALSE")) #missing csv

subset(siteinfo_final,na_avil=="NaN")
dim(subset(siteinfo_final,na_avil=="NaN")) #missing csv and csv's value are NA (due to n_focal) --> 42 sites!

#find such row name (list of number that will be used next round fapar ingestr)
siteinfo_final$no <- c(1:485)
na_number <- (subset(siteinfo_final,na_avil=="NaN")$no)
na_number

#now, re-ingesting fapar --> n_focal = 0 again, just have a double check (if anyone was wrongly by-passing)!
stopCluster(cl)
library(doSNOW)
NumberOfCluster <- 8
cl <- makeCluster(NumberOfCluster, type='SOCK')
registerDoSNOW(cl)

settings_modis <- get_settings_modis(
  bundle            = "modis_fpar",
  data_path         = "~/data/grassland_npp/reprocessing_fapar_01_15/nfocal_0_again/",
  method_interpol   = "loess",
  keep              = TRUE,
  overwrite_raw     = FALSE,
  overwrite_interpol= TRUE,
  n_focal = 0
)

x0 <- foreach(i = na_number,.combine = "rbind") %dopar% {
  devtools::load_all(".")
  library(dplyr)
  df_modis_fpar <- ingest_bysite(
    sitename  = siteinfo_final[i,c("sitename")],
    source    = "modis",
    year_start = 2001,
    year_end  = 2015,
    lon       = siteinfo_final[i,c("lon")],
    lat       = siteinfo_final[i,c("lat")],
    settings  = settings_modis,
    verbose   = FALSE
  )
}
stopCluster(cl)


#now, now n_focal = 1, to fill such na values from csv!
library(doSNOW)
NumberOfCluster <- 8
cl <- makeCluster(NumberOfCluster, type='SOCK')
registerDoSNOW(cl)

settings_modis <- get_settings_modis(
  bundle            = "modis_fpar",
  data_path         = "~/data/grassland_npp/reprocessing_fapar_01_15/nfocal_1/",
  method_interpol   = "loess",
  keep              = TRUE,
  overwrite_raw     = FALSE,
  overwrite_interpol= TRUE,
  n_focal = 1
)

x0 <- foreach(i = na_number,.combine = "rbind") %dopar% {
  devtools::load_all(".")
  library(dplyr)
  df_modis_fpar <- ingest_bysite(
    sitename  = siteinfo_final[i,c("sitename")],
    source    = "modis",
    year_start = 2001,
    year_end  = 2015,
    lon       = siteinfo_final[i,c("lon")],
    lat       = siteinfo_final[i,c("lat")],
    settings  = settings_modis,
    verbose   = FALSE
  )
}
stopCluster(cl)

#now, now n_focal = 2 (after we still see some NA from csvs when n_focal =1), to fill such na values from csv!
library(doSNOW)
NumberOfCluster <- 8
cl <- makeCluster(NumberOfCluster, type='SOCK')
registerDoSNOW(cl)

settings_modis <- get_settings_modis(
  bundle            = "modis_fpar",
  data_path         = "~/data/grassland_npp/reprocessing_fapar_01_15/nfocal_2/",
  method_interpol   = "loess",
  keep              = TRUE,
  overwrite_raw     = FALSE,
  overwrite_interpol= TRUE,
  n_focal = 2
)

x0 <- foreach(i = na_number,.combine = "rbind") %dopar% {
  devtools::load_all(".")
  library(dplyr)
  df_modis_fpar <- ingest_bysite(
    sitename  = siteinfo_final[i,c("sitename")],
    source    = "modis",
    year_start = 2001,
    year_end  = 2015,
    lon       = siteinfo_final[i,c("lon")],
    lat       = siteinfo_final[i,c("lat")],
    settings  = settings_modis,
    verbose   = FALSE
  )
}
stopCluster(cl)

#now, examine and re-collected our study sites for collecting fapar (all from 2001-2015)
#primarily - based on n_focal_0, and n_focal_0_again (some sites were by-passed due to internet): /Users/yunpeng/data/grassland_npp/reprocessing_fapar_01_15/nfocal_0
#secondly - based on n_focal = 1: /Users/yunpeng/data/grassland_npp/reprocessing_fapar_01_15/nfocal_1
#thirdly - based on n_focal = 2: /Users/yunpeng/data/grassland_npp/reprocessing_fapar_01_15/nfocal_2

######The site information below has already been carefully checked in its separate original file (see above), to make sure that it strictly follows n_focal =0, then 1, then 2, then missing (10 points left)

#n_focal_again includes addtional site (15): 68, 69, 74, 75, 76, 77, 78, 80, 83, 84, 85, 86, 87, 89, 130
#n_focal1 includes addtional site (14): 29, 48, 57, 73, 79, 102, 128, 161, 172, 227, 282, 301, 348, 369, 373
#n_focal2 includes addtional site (2) : 41, 160
#Missing sites (from n_focal = 0, 1, 2 all not works; 10 points): 13, 81, 214, 223, 224, 274, 317, 333, 375, 416
#All others are n_focal0

#The final file was saved as: /Users/yunpeng/data/grassland_npp/reprocessing_fapar_final, with a README


##################
#now, for climate forcing
#change some parameters in dir, since we newly copied Psurf now.
#newly including org. dataset, using original measurement year (<1980, shall still be corrected. Of course)
rm(list=ls())
NPP_grassland  <- read.csv("/Users/yunpeng/data/grassland_npp/NPP_grassland.csv")

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
dim(siteinfo_final) #485 sites to be obtained

#last check
stopifnot( all(siteinfo_final$year_start == floor(siteinfo_final$year_start)) )
stopifnot( all(siteinfo_final$year_end == floor(siteinfo_final$year_end)) )

#this version above is consistent with what we processing in last simulation
devtools::load_all(".")

siteinfo_final$whc <- 170

library(doSNOW)
NumberOfCluster <- 4
cl <- makeCluster(NumberOfCluster, type='SOCK')
registerDoSNOW(cl)
x0 <- foreach(i = 1:485,.combine = "rbind") %dopar% {
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
  csvfile <- paste("~/data/grassland_npp/reprocessing_climate/",siteinfo_final$sitename[i],".csv",sep = "")
  write.csv(ddf_meteo, csvfile, row.names = TRUE)
  print(i)    
}

stopCluster(cl)
