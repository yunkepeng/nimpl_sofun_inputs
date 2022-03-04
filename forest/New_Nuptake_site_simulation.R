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
devtools::load_all("/Users/yunpeng/yunkepeng/Grassland_new_ingestr_rsofun_20210326/rsofun/")

load(file = "~/yunkepeng/nimpl_sofun_inputs/forest/New_Nuptake_site_simulation.Rdata")

#### Input N uptake
#(1) newly added Nmin rate data from Finzi
Finzi <- read.csv("/Users/yunpeng/data/NPP_Yunke/Nmin_Finzi/Nmin_Finzi.csv")
names(Finzi)[names(Finzi) == "Lat"] <- "lat"
names(Finzi)[names(Finzi) == "Long"] <- "lon"
devtools::load_all("/Users/yunpeng/yunkepeng/Grassland_new_ingestr_rsofun_20210326/ingestr/")

#See Gill and Finzi Fig3 for pft info 
#Grassland
#Finzi_Grassland <- subset(Finzi, Biome=="temp grass")
#Finzi_Grassland_sitemean <- aggregate(Finzi_Grassland,by=list(Finzi_Grassland$lon,Finzi_Grassland$lat), FUN=mean, na.rm=TRUE) #site-mean
#dim(Finzi_Grassland_sitemean)
#for (i in 1:nrow(Finzi_Grassland_sitemean)){
#  Finzi_Grassland_sitemean$sitename[i] <- paste("Finzi_Grass",i,sep = "") # this is also sitename for fpar
#  Finzi_Grassland_sitemean$sitename_climate[i] <- paste("Finzi_Grass_climate",i,sep = "")
#}
#df_etopo <- ingest(Finzi_Grassland_sitemean,source = "etopo1",dir = "~/data/etopo/" )
#Finzi_Grassland_sitemean$elv <- as.numeric(as.data.frame(df_etopo$data))
#Finzi_Grassland_sitemean

#Forest - only merging forest this time
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
#Finzi_final <- dplyr::bind_rows(Finzi_Forest_sitemean, Finzi_Grassland_sitemean)

Finzi_Forest_sitemean2 <- Finzi_Forest_sitemean[,c("lon","lat","elv","sitename","sitename_climate")]
dim(Finzi_Forest_sitemean2)
Finzi_all <-Reduce(function(x,y) merge(x = x, y = y, by = c("lon","lat"),all.x=TRUE), 
                   list(Finzi,Finzi_Forest_sitemean2))

Finzi_all_forest <- subset(Finzi_all, Biome!="temp grass" & is.na(lon)==FALSE)
summary(Finzi_all_forest)
Finzi_all_forest$year_start <- 1984
Finzi_all_forest$year_end <- 2013

#(2) Nuptake from gcme
gcme_nuptake <- read.csv("/Users/yunpeng/data/NPP_Yunke/Nuptake_gcme/gcme_nuptake_coord_interpolated.csv")
for (i in 1:nrow(gcme_nuptake)){
  gcme_nuptake$sitename[i] <- paste("gcme",i,sep = "") # this is also sitename for fpar
  gcme_nuptake$sitename_climate[i] <- paste("gcme_climate",i,sep = "")
}

#elv
df_etopo <- ingest(gcme_nuptake,source = "etopo1",dir = "~/data/etopo/" )
gcme_nuptake$elv <- as.numeric(as.data.frame(df_etopo$data))
gcme_nuptake$elv[gcme_nuptake$elv<0] <- 0

siteinfo_gcme <- data.frame(
  sitename = gcme_nuptake$sitename,
  sitename_climate = gcme_nuptake$sitename_climate,
  lon = gcme_nuptake$lon,
  lat = gcme_nuptake$lat,
  elv = gcme_nuptake$elv,
  year_start = gcme_nuptake$start_yr,
  year_end = gcme_nuptake$start_yr + gcme_nuptake$Year_long -1
)

siteinfo_gcme$exp_nam <- gcme_nuptake$exp_nam

gcme_data <- read.csv("/Users/yunpeng/data/NPP_Yunke/Nuptake_gcme/gcme_nuptake_data.csv")
gcme_data <- gcme_data[,c("exp_nam","ambient","Unit")]
#all converting to gN/m2/yr
gcme_data$ambient[gcme_data$Unit=="Kg_N_ha-1"] <- gcme_data$ambient[gcme_data$Unit=="Kg_N_ha-1"]/10
gcme_data$ambient[gcme_data$Unit=="kg_N/ha"] <- gcme_data$ambient[gcme_data$Unit=="kg_N/ha"]/10
gcme_data$ambient[gcme_data$Unit=="mg_N/kg*day_"] <- NA #quite weired about the unit, for one site. Disregard them first
hist(gcme_data$ambient)
#why some values are too low?
gcme_data <- subset(gcme_data,ambient>0)
subset(gcme_data,ambient<1) %>% group_by(exp_nam) %>% summarise(number = n())
#after look, "RiceFACE_Japan_A_1998_39,40_141" looks fine, as most of them were still in good range. But remove the other 5 sites (they may be wrong due to measurement mistakes or unit errors, we don't know)
gcme_data_final <- subset(gcme_data,exp_nam!="Michigan_UNDERC_bog" & exp_nam!="Michigan_UNDERC_intermFen" & exp_nam!="Michigan_UNDERC_richFen"&
                            exp_nam!="RiceFACE_China_32N_120E_Or_Tr_7"& exp_nam!="TL_7")
hist(gcme_data_final$ambient)

#finally, mergeing them to obtain siteinfo

gcme_data_final_forest <-Reduce(function(x,y) merge(x = x, y = y, by = c("exp_nam"),all.x=TRUE),list(gcme_data_final,siteinfo_gcme))

#rbind them to get final Nmin. data

Finzi_all_forest <- Finzi_all_forest[,c("lon","lat","elv","sitename","sitename_climate","year_start","year_end","Nmin")]
Finzi_all_forest <- rename(Finzi_all_forest, obs_nuptake = Nmin)
Finzi_all_forest$method <- "Net minerlization (Finzi paper)"
gcme_data_final_forest <- gcme_data_final_forest[,c("lon","lat","elv","sitename","sitename_climate","year_start","year_end","ambient")]
gcme_data_final_forest <- rename(gcme_data_final_forest, obs_nuptake = ambient)
gcme_data_final_forest$method <- "Total Nuptake reported in GCME, at ambient condition"

Nmin_final <- dplyr::bind_rows(Finzi_all_forest,gcme_data_final_forest)
Nmin_final


#input Filzi's climates
forcing_df <- list.files("~/data/NPP_Yunke/Nmin_Finzi/reprocessing_Nmin/climates/",full.names = T) # 2 points were missing, as expected
length(forcing_df)

fapar_df <- list.files("~/data/NPP_Yunke/Nmin_Finzi/reprocessing_Nmin/fapar/",full.names = T)
length(fapar_df)-1

fapar_org_df <- list.files("~/data/NPP_Yunke/Nmin_Finzi/reprocessing_Nmin/fapar/raw/",full.names = T)
length(fapar_org_df)

#1. fapar - input

#1. fpar - check missing data - and also, input all years fapar (2001-2015), which will be selected in measurement year only later on 
for (i in 1:(length(fapar_df)-1)){
  df1 <- read.csv(fapar_df[i])
  df1$date <- as.Date(df1$date)
  df1 <- df1[!(format(df1$date,"%m") == "02" & format(df1$date, "%d") == "29"), , drop = FALSE]
  df2 <- df1[,c("date","modisvar_filled")]
  assign(substr(sub('.*daily_', '', fapar_df[i]),1,nchar(sub('.*daily_', '', fapar_df[i]))-4), df2) 
}

#2. forcing - combing fapar and climates into a df.
for (i in 1:(length(forcing_df)-18)){ # remove the later 18 sites where is grassland
  df1 <- read.csv(forcing_df[i])
  df1$date <- as.Date(df1$date)
  
  sitename_climate <- subset(Nmin_final,Nmin_final$sitename_climate == df1$sitename[1])$sitename_climate
  sitename_fapar <- subset(Nmin_final,Nmin_final$sitename_climate == df1$sitename[1])$sitename
    
  fapar <- (eval(parse(text=sitename_fapar)))
  fapar$Year <- year(fapar$date)
  
  yr_start <- 1984
  yr_end <- 2013
    
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

#3. rsofun to predict gpp
df_soiltexture <- bind_rows(
  top    = tibble(layer = "top",    fsand = 0.4, fclay = 0.3, forg = 0.1, fgravel = 0.1),
  bottom = tibble(layer = "bottom", fsand = 0.4, fclay = 0.3, forg = 0.1, fgravel = 0.1))
params_modl <- list(
  kphio           = 0.09423773,
  soilm_par_a     = 0.33349283,
  soilm_par_b     = 1.45602286)

Nmin_final$whc = 170

Nmin_final$pred_gpp_c3 <- NA
#NPP_Forest$pred_gpp_c4 <- NA
Nmin_final$max_vcmax25_c3 <- NA
#NPP_Forest$max_vcmax25_c4 <- NA


#using rsofun
for (i in 1:nrow(Nmin_final)) {
  tryCatch({
    #c3
    forcing <- (eval(parse(text=(paste("final",Nmin_final$sitename_climate[i],sep="_")))))
    modlist <- run_pmodel_f_bysite( 
      Nmin_final$sitename_climate[i], 
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
        firstyeartrend = Nmin_final$year_start[i],
        nyeartrend = Nmin_final$year_end[i]-Nmin_final$year_start[i]+1), 
      siteinfo = Nmin_final[i,], 
      forcing, 
      df_soiltexture, 
      params_modl = params_modl, 
      makecheck = TRUE)
    
    pred_gpp_list <- modlist %>% mutate(ymonth = month(date),yday = day(date)) %>% group_by(ymonth, yday) %>% summarise(gpp = mean(gpp, na.rm = TRUE))
    max_vcmax25 <- max(modlist$vcmax25)*1000000
    
    Nmin_final[i,c("pred_gpp_c3")] <- sum(pred_gpp_list$gpp)
    Nmin_final[i,c("max_vcmax25_c3")] <- max_vcmax25
  }, error=function(e){})} 
#for fapar - if not available for n_focal = 0, then changing to 1, then 2...


#input gcme climates
forcing_df <- list.files("~/data/NPP_Yunke/Nuptake_gcme/climates/",full.names = T) # 2 points were missing, as expected due to 
length(forcing_df)

fapar_df <- list.files("~/data/NPP_Yunke/Nuptake_gcme/fapar/",full.names = T)
length(fapar_df)-1

#1. fapar - input

#1. fpar - check missing data - and also, input all years fapar (2001-2015), which will be selected in measurement year only later on 
for (i in 1:(length(fapar_df)-1)){
  df1 <- read.csv(fapar_df[i])
  df1$date <- as.Date(df1$date)
  df1 <- df1[!(format(df1$date,"%m") == "02" & format(df1$date, "%d") == "29"), , drop = FALSE]
  df2 <- df1[,c("date","modisvar_filled")]
  assign(substr(sub('.*daily_', '', fapar_df[i]),1,nchar(sub('.*daily_', '', fapar_df[i]))-4), df2) 
}

#2. forcing - combing fapar and climates into a df.
for (i in 1:(length(forcing_df))) {
  tryCatch({
    df1 <- read.csv(forcing_df[i])
    df1$date <- as.Date(df1$date)
    
    sitename_climate <- subset(Nmin_final,Nmin_final$sitename_climate == df1$sitename[1])$sitename_climate[1]
    sitename_fapar <- subset(Nmin_final,Nmin_final$sitename_climate == df1$sitename[1])$sitename[1]
    
    fapar <- (eval(parse(text=sitename_fapar)))
    fapar$Year <- year(fapar$date)
    
    yr_start <- subset(Nmin_final,Nmin_final$sitename_climate == df1$sitename[1])$year_start[1]
    yr_end <- subset(Nmin_final,Nmin_final$sitename_climate == df1$sitename[1])$year_end[1]
    
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
  }, error=function(e){})} 


#3. rsofun to predict gpp
df_soiltexture <- bind_rows(
  top    = tibble(layer = "top",    fsand = 0.4, fclay = 0.3, forg = 0.1, fgravel = 0.1),
  bottom = tibble(layer = "bottom", fsand = 0.4, fclay = 0.3, forg = 0.1, fgravel = 0.1))
params_modl <- list(
  kphio           = 0.09423773,
  soilm_par_a     = 0.33349283,
  soilm_par_b     = 1.45602286)

Nmin_final$whc = 170

#using rsofun for gcme sites only
for (i in 240:328) {
  tryCatch({
    #c3
    forcing <- (eval(parse(text=(paste("final",Nmin_final$sitename_climate[i],sep="_")))))
    modlist <- run_pmodel_f_bysite( 
      Nmin_final$sitename_climate[i], 
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
        firstyeartrend = Nmin_final$year_start[i],
        nyeartrend = Nmin_final$year_end[i]-Nmin_final$year_start[i]+1), 
      siteinfo = Nmin_final[i,], 
      forcing, 
      df_soiltexture, 
      params_modl = params_modl, 
      makecheck = TRUE)
    
    pred_gpp_list <- modlist %>% mutate(ymonth = month(date),yday = day(date)) %>% group_by(ymonth, yday) %>% summarise(gpp = mean(gpp, na.rm = TRUE))
    max_vcmax25 <- max(modlist$vcmax25)*1000000
    
    Nmin_final[i,c("pred_gpp_c3")] <- sum(pred_gpp_list$gpp)
    Nmin_final[i,c("max_vcmax25_c3")] <- max_vcmax25
  }, error=function(e){})} 

Nmin_final

NPP_Forest <- Nmin_final
#already checked here -all NA in pred_gpp_c3 is due to NA of fAPAR in orig/, either in nfocal = 0, 1 and 2.

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
NPP_Forest$z <- NPP_Forest$elv
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

#now, using several statistical models to predict npp, anpp, npp.leaf....
#now, using several statistical models to predict npp, anpp, npp.leaf....
NPP_Forest$pred_npp <- NPP_Forest$pred_gpp_c3 * (1/(1 + exp(-(-0.36075 * log(NPP_Forest$CNrt) +
                                                                -0.16213 * log(NPP_Forest$age) + 
                                                                0.72793 * NPP_Forest$fAPAR+ 0.57014))))

NPP_Forest$pred_anpp <- NPP_Forest$pred_gpp_c3 * (1/(1 + exp(-(-0.55151 * log(NPP_Forest$CNrt) +
                                                                 -0.20050 * log(NPP_Forest$age) + 
                                                                 1.06611 * NPP_Forest$fAPAR+ 0.35817))))

NPP_Forest$pred_bnpp <- NPP_Forest$pred_npp - NPP_Forest$pred_anpp

NPP_Forest$pred_lnpp <- NPP_Forest$pred_anpp * (1/(1 + exp(-(0.97093* log(NPP_Forest$PPFD) +
                                                               0.06453 * (NPP_Forest$Tg) + 
                                                               -0.80397 * log(NPP_Forest$vpd) + -7.47165))))

NPP_Forest$pred_wnpp <- NPP_Forest$pred_anpp - NPP_Forest$pred_lnpp

#use rsofun - site-species
#NPP_Forest$pred_leafnc <- (0.0162/0.5) + (0.0039/0.5) * NPP_Forest$max_vcmax25/NPP_Forest$LMA
NPP_Forest$pred_leafnc <- (0.01599/0.46) + (0.005992/0.46) * NPP_Forest$max_vcmax25/NPP_Forest$LMA

NPP_Forest$pred_lnf <- NPP_Forest$pred_lnpp*NPP_Forest$pred_leafnc

#summary(NPP_Forest$CN_wood_final)
NPP_Forest$pred_wnf <- NPP_Forest$pred_wnpp/100
NPP_Forest$pred_bnf <- NPP_Forest$pred_bnpp/94
#root mean was obtained from median of NPP forest

NPP_Forest$pred_nre <- (1/(1+exp(-(-0.064460 *NPP_Forest$Tg + 0.402850 * log(NPP_Forest$vpd) + 1.368935))))
hist(NPP_Forest$pred_nre)

NPP_Forest$pred_nuptake <- NPP_Forest$pred_lnf*(1-NPP_Forest$pred_nre) + NPP_Forest$pred_wnf +NPP_Forest$pred_bnf  


My_Theme = theme(
  axis.title.x = element_text(size = 20),
  axis.text.x = element_text(size = 20),
  axis.title.y = element_text(size = 20),
  axis.text.y = element_text(size = 20))

#check
#analyse_modobs2(forest_site2,"pred_gpp", "GPP",type = "points")
ggplot(data=NPP_Forest, aes(x=pred_nuptake, y=obs_nuptake)) +
  geom_point(aes(x=pred_nuptake, y=obs_nuptake,color=factor(method)))+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()+My_Theme

Nmin_all <- subset(NPP_Forest,method=="Net minerlization (Finzi paper)")

ggplot(data=Nmin_all, aes(x=pred_nuptake, y=obs_nuptake)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+xlim(0,25)+ylim(0,25)+
  xlab("Prediction")+ylab("Observation")+theme_classic()+My_Theme
summary(lm(obs_nuptake~pred_nuptake,Nmin_all))
csvfile <- paste("/Users/yunpeng/data/NPP_final/Nmin_validation.csv")
write.csv(Nmin_all, csvfile, row.names = TRUE)


subset(Nmin_all,is.na(pred_nuptake)==TRUE) %>% group_by(sitename) %>% 
  summarise (number=n())
#10 sites were missing - due to missing fapar (even when n_focal =2) or missing age (when becoming negative)
Nmin_all_sm <- aggregate(Nmin_all,by=list(Nmin_all$lon,Nmin_all$lat), mean,na.rm=TRUE)

ggplot(data=Nmin_all_sm, aes(x=pred_nuptake, y=obs_nuptake)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()+My_Theme

csvfile <- paste("~/data/NPP_Yunke/Nuptake_gcme/All_Nuptake.csv",sep = "")
write.csv(NPP_Forest, csvfile, row.names = TRUE)

save.image(file = "~/yunkepeng/nimpl_sofun_inputs/forest/New_Nuptake_site_simulation.Rdata")

#load Nuptake map
library(rbeni)
inputnc <- function(name,start_year,end_year){
  #-----------------------------------------------------------------------
  # Input: 
  # name: gpp, npp, anpp, vcmax25, leafcn, nuptake...
  # start_year: e.g. 1981
  # end_year: e.g. 2016
  # location: e.g "D:/PhD/nimpl_sofun_inputs/Data/output/" or in Euler: "~/yunkebranch_units/outputnc/"
  #-----------------------------------------------------------------------
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
firstyr_data <- 1982 # In data file, which is the first year
endyr_data <- 2011 # In data file, which is the last year
location <- "/Users/yunpeng/data/output/latest_forest/"
alloutput_list <- list.files(location,full.names = T)

#input elevation nc file, which will be cbind with global df directly
elev_nc <- read_nc_onefile("~/data/watch_wfdei/WFDEI-elevation.nc")
#elev_nc <- read_nc_onefile("D:/PhD/nimpl_sofun_inputs/Data/Elevation/WFDEI-elevation.nc")
elev <- as.data.frame(nc_to_df(elev_nc, varnam = "elevation"))
head(elev) # this is consistent with df coord below

gg <- plot_map3(nuptake_df2[,c(1,2,4)], 
                varnam = "nuptake",plot_title = " Total N uptake in ecosystem (gN/m2/yr)",
                latmin = -65, latmax = 85, combine = FALSE)
gg$ggmap + geom_point(data=subset(Nmin_all,pred_nuptake>0),aes(lon,lat),size=3,col="red")
gg$gglegend

#calculate uncertainty
all_predictors

all_predictors$b <- (-0.3677 * log(all_predictors$CNrt) +
    -0.1552 * log(all_predictors$age) + 
    0.5791 * all_predictors$fAPAR+
    1.9144 *all_predictors$alpha + -1.1052)


all_predictors$npp_gpp_uncertainty <- 1.87 * exp(-all_predictors$b) / ((1+exp(-all_predictors$b))^2)

bbb <- all_predictors[,c("lon","lat","npp_gpp_uncertainty")]

gg <- plot_map3(bbb, 
                varnam = "npp_gpp_uncertainty",plot_title = " b ",
                latmin = -65, latmax = 85, combine = TRUE)
gg
