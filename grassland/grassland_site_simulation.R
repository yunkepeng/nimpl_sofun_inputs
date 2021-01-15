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

load(file = "/Users/yunpeng/yunkepeng/nimpl_sofun_inputs/grassland/grassland_site_simulation.Rdata")

#read complete dataset for measurement, after L1-L300 in /Users/yunpeng/yunkepeng/nimpl_sofun_inputs/forest/Forest_Global_check.Rmd
NPP <- read.csv("/Users/yunpeng/data/forest_npp/NPP_final_all.csv")
#extract forest only
NPP_grassland <- subset(NPP,pft2=="Grassland")

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

#extract fapar and forcing --》 see forcing_fpar.R

#after forcing and fapar -> 10 sites are missing fapar, though org fapar is available, but is is all NA based on its information in qc.
fapar_df <- list.files("/Users/yunpeng/data/grassland_npp/fpar_all",full.names = T)
length(fapar_df)-1

fapar_org_df <- list.files("/Users/yunpeng/data/grassland_npp/fpar_raw",full.names = T)
length(fapar_org_df)

forcing_df <- list.files("/Users/yunpeng/data/grassland_npp/forcing",full.names = T)
length(forcing_df)

#fapar - input - (fpar data structure rank: fpar2, fpar, fpar3, fpar4)
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

#fapar - check na
for (i in 1:nrow(siteinfo_final)){
  siteinfo_final$fpar_avil[i] <- exists(paste("ingestr",i,sep = ""))
}

#forcing - input
for (i in 1:length(forcing_df)){
  tryCatch({
    df1 <- read.csv(forcing_df[i])
    df1$date <- as.Date(df1$date)
    fpar <- as.data.frame(eval(parse(text=df1$sitename[1])))[,3]
    fapar <- rep(fpar,nrow(df1)/365)
    df2 <- cbind(df1,fapar)
    df3 <- df2[,c("date","temp","prec","vpd","ppfd","patm","ccov_int","ccov","fapar","co2")]
    assign(paste("final",df1$sitename[1],sep="_"), as_tibble(df3))
  }, error=function(e){})} 

#forcing - check na when either forcing or fapar missing
for (i in 1:nrow(siteinfo_final)){
  siteinfo_final$forcing_avil[i] <- exists(paste("final",siteinfo_final$sitename[i],sep="_"))
}

#show na points (n = 10) for without fapar data.
na_data <- subset(siteinfo_final,fpar_avil=="FALSE")
dim(na_data)
library(rworldmap)
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-180, 180), ylim = c(-75, 75), asp = 1)
points(na_data$lon,na_data$lat, col="red", pch=16,cex=1)


#now, we can work on predicting gpp
df_soiltexture <- bind_rows(
  top    = tibble(layer = "top",    fsand = 0.4, fclay = 0.3, forg = 0.1, fgravel = 0.1),
  bottom = tibble(layer = "bottom", fsand = 0.4, fclay = 0.3, forg = 0.1, fgravel = 0.1))
params_modl <- list(
  kphio           = 0.09423773,
  soilm_par_a     = 0.33349283,
  soilm_par_b     = 1.45602286)



#now, run for loop for collecting each site - by pass points above that do not have fapar
dim(na_data) #this point needs to be passed
siteinfo_final$pred_gpp_c3 <- NA
siteinfo_final$pred_gpp_c4 <- NA
siteinfo_final$max_vcmax25_c3 <- NA
siteinfo_final$max_vcmax25_c4 <- NA


for (i in 1:nrow(siteinfo_final)) {
  tryCatch({
    #c3 gpp
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
    
    #c4 gpp
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
        lgr3               = FALSE,
        lgn3               = FALSE,
        lgr4               = TRUE,
        firstyeartrend = siteinfo_final$year_start[i],
        nyeartrend = siteinfo_final$year_end[i]-siteinfo_final$year_start[i]+1), 
      siteinfo = siteinfo_final[i,], 
      forcing, 
      df_soiltexture, 
      params_modl = params_modl, 
      makecheck = TRUE)
    pred_gpp_list <- modlist %>% mutate(ymonth = month(date),yday = day(date)) %>% group_by(ymonth, yday) %>% summarise(gpp = mean(gpp, na.rm = TRUE))
    
    siteinfo_final[i,c("pred_gpp_c4")] <- sum(pred_gpp_list$gpp)
    
    #max_vcmax25 - c3
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
    max_vcmax25 <- max(modlist$vcmax25)*1000000
    siteinfo_final[i,c("max_vcmax25_c3")] <- max_vcmax25
    
    #max_vcmax25 - c4
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
        lgr3               = FALSE,
        lgn3               = FALSE,
        lgr4               = TRUE,
        firstyeartrend = siteinfo_final$year_start[i],
        nyeartrend = siteinfo_final$year_end[i]-siteinfo_final$year_start[i]+1), 
      siteinfo = siteinfo_final[i,], 
      forcing, 
      df_soiltexture, 
      params_modl = params_modl, 
      makecheck = TRUE)
    max_vcmax25 <- max(modlist$vcmax25)*1000000
    siteinfo_final[i,c("max_vcmax25_c4")] <- max_vcmax25
  }, error=function(e){})} 

#collect gpp and combine it into NPP_grassland
siteinfo_final_gpp <- siteinfo_final[,c("sitename","pred_gpp_c3","pred_gpp_c4","max_vcmax25_c3","max_vcmax25_c4")]
names(siteinfo_final_gpp) <- c("sitename2","pred_gpp_c3","pred_gpp_c4","max_vcmax25_c3","max_vcmax25_c4")
NPP_grassland_all2 <-Reduce(function(x,y) merge(x = x, y = y, by = c("sitename2"),all.x=TRUE), 
                            list(NPP_grassland_all,siteinfo_final_gpp))

NPP_grassland_all2 <- NPP_grassland_all2[order(NPP_grassland_all2$no), ]

NPP_grassland$pred_gpp_c3 <- NPP_grassland_all2$pred_gpp_c3
NPP_grassland$pred_gpp_c4 <- NPP_grassland_all2$pred_gpp_c4
NPP_grassland$max_vcmax25_c3 <- NPP_grassland_all2$max_vcmax25_c3
NPP_grassland$max_vcmax25_c4 <- NPP_grassland_all2$max_vcmax25_c4

dim(subset(NPP_grassland,pred_gpp_c3>TNPP_1)) #157
dim(subset(NPP_grassland,pred_gpp_c3<TNPP_1)) #34
aaa$weightedgpp_all
outlier<- (subset(NPP_grassland,pred_gpp_c3<TNPP_1))

newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-180, 180), ylim = c(-75, 75), asp = 1)
points(outlier$lon,outlier$lat, col="red", pch=16,cex=1)
#a few sites are outliers: 34/154

#now, extract all predictors
#firstly, alpha, Tg, vpd, PPFD

#input nc file
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

#cbind all predictors, and its lon, lat, z
all_predictors <- cbind(elev,Tg$myvar,PPFD$myvar,vpd$myvar,
                        alpha$myvar,fAPAR$myvar,age$myvar)

names(all_predictors) <- c("lon","lat","z","Tg","PPFD","vpd",
                           "alpha","fAPAR","age")

Tg_df <- all_predictors[,c("lon","lat","z","Tg")]
PPFD_df <- all_predictors[,c("lon","lat","z","PPFD")]
vpd_df <- all_predictors[,c("lon","lat","z","vpd")]
alpha_df <- all_predictors[,c("lon","lat","z","alpha")]
fAPAR_df <- all_predictors[,c("lon","lat","z","fAPAR")]
age_df <- all_predictors[,c("lon","lat","z","age")]

#now, apply gwr to extract site predictors' value
grassland_site <- NPP_grassland[,c("lon","lat","z")]
grassland_site$Tg <- NA
grassland_site$PPFD <- NA
grassland_site$vpd <- NA
grassland_site$alpha <- NA
#grassland_site$age <- NA
grassland_site$fapar <- NA

a <- 1.5 # which degree (distance) of grid when interpolating gwr from global grids

#Extract Tg, PPFD, vpd, alpha,fAPAR

for (i in 1:nrow(grassland_site)) {
  tryCatch({
    #Tg
    Tg_global <- na.omit(Tg_df)
    NRE_part <- subset(Tg_global,lon>(grassland_site[i,1]-a)&lon<(grassland_site[i,1]+a)&
                         lat>(grassland_site[i,2]-a)&lat<(grassland_site[i,2]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- grassland_site[i,1:3]
    coordinates(NRE_coord) <- c("lon","lat")
    grassland_site[i,c("Tg")] <- (gwr(Tg ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #ppfd
    PPFD_global <- na.omit(PPFD_df)
    NRE_part <- subset(PPFD_global,lon>(grassland_site[i,1]-a)&lon<(grassland_site[i,1]+a)&
                         lat>(grassland_site[i,2]-a)&lat<(grassland_site[i,2]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- grassland_site[i,1:3]
    coordinates(NRE_coord) <- c("lon","lat")
    grassland_site[i,c("PPFD")] <- (gwr(PPFD ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #vpd
    vpd_global <- na.omit(vpd_df)
    NRE_part <- subset(vpd_global,lon>(grassland_site[i,1]-a)&lon<(grassland_site[i,1]+a)&
                         lat>(grassland_site[i,2]-a)&lat<(grassland_site[i,2]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- grassland_site[i,1:3]
    coordinates(NRE_coord) <- c("lon","lat")
    grassland_site[i,c("vpd")] <- (gwr(vpd ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #alpha
    alpha_global <- na.omit(alpha_df)
    NRE_part <- subset(alpha_global,lon>(grassland_site[i,1]-a)&lon<(grassland_site[i,1]+a)&
                         lat>(grassland_site[i,2]-a)&lat<(grassland_site[i,2]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- grassland_site[i,1:3]
    coordinates(NRE_coord) <- c("lon","lat")
    grassland_site[i,c("alpha")]  <- (gwr(alpha ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #fAPAR
    fAPAR_global <- na.omit(fAPAR_df)
    NRE_part <- subset(fAPAR_global,lon>(grassland_site[i,1]-a)&lon<(grassland_site[i,1]+a)&
                         lat>(grassland_site[i,2]-a)&lat<(grassland_site[i,2]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- grassland_site[i,1:3]
    coordinates(NRE_coord) <- c("lon","lat")
    grassland_site[i,c("fapar")]<- (gwr(fAPAR ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
  }, error=function(e){})} 


summary(grassland_site)

library(raster)
library(rgdal)
library(dplyr)
library(rbeni)
library(ncdf4)
soil <- raster('~/data/ISRIC/data_orig/data/raster/w001000.adf')
NRE_lonlat <- grassland_site[,c("lon","lat","z")]

sp_sites <- SpatialPoints(NRE_lonlat[,c("lon","lat","z")]) # only select lon and lat

#change its variable name to SUID, this is a unique code that could be used to merged with soil data, which will be further merged with csv below.
NRE_lonlat2 <- raster::extract(soil, sp_sites, sp = TRUE) %>% as_tibble() %>% 
  right_join(NRE_lonlat, by = c("lon", "lat","z")) %>% 
  dplyr::rename( SUID = w001000)

#input soil information data csv
ISRIC.data<-read.csv(file="~/data/ISRIC/data_orig/data/HW30s_FULL.csv",header=TRUE,sep=";",dec = ".") # Now, input ISRIC database
data.soil.extract <- merge(NRE_lonlat2,ISRIC.data,by='SUID',all.x=TRUE) # merge site with soil variables by using SUID
data.soil.extract2 <- subset(data.soil.extract,CNrt>0) # select available CNrt
data.soil.extract3 <- data.soil.extract2[,c("lon","lat","z","CNrt")] # only select CNrt variable

# note that in each site there might be more than 1 samples measured, so we should aggregate them which make sures that one grid holds one data only.
ss1 <- aggregate(data.soil.extract3,by=list(data.soil.extract3$lon,data.soil.extract3$lat,data.soil.extract3$z), FUN=mean, na.rm=TRUE) 
ss2 <- ss1[,c("lon","lat","z","CNrt")] # now,select lon, lat, z and CNrt only

# finally, merging site-based soil c/n data into our current dataframe
grassland_site$no <- c(1:nrow(grassland_site))
grassland_site2 <-Reduce(function(x,y) merge(x = x, y = y, by = c("lon","lat","z"),all.x=TRUE), 
                         list(grassland_site,ss2))

grassland_site3 <- grassland_site2[order(grassland_site2$no), ]

head(grassland_site3)

NPP_grassland_final <- cbind(NPP_grassland,grassland_site3[,c(4,5,6,7,8,10)])
summary(NPP_grassland_final)



#now, process final data, and used in final simulations
NPP_grassland_final$fapar[NPP_grassland_final$fapar>1] <- NA
NPP_grassland_final$alpha[NPP_grassland_final$alpha>1] <- NA
#NPP_grassland_final$age[NPP_grassland_final$age<0] <- NA
summary(NPP_grassland_final)


#1. aggregate based on lat, lon and z
site_all <- (aggregate(NPP_grassland_final,by=list(NPP_grassland_final$lon,NPP_grassland_final$lat,NPP_grassland_final$z), FUN=mean, na.rm=TRUE))
site_all <- site_all[,c("lon","lat","z")]
for (i in 1:nrow(site_all)){
  site_all$site_xyz[i] <- paste("a",i,sep="")
}
dim(site_all)
# merge back to FINAL INDIVIDUALS DATA
NPP_grassland_final$no <- c(1:nrow(NPP_grassland_final))

NPP_grassland_final2 <-Reduce(function(x,y) merge(x = x, y = y, by = c("lon","lat","z"),all.x=TRUE), 
                              list(NPP_grassland_final,site_all))

NPP_grassland_final2 <- NPP_grassland_final2[order(NPP_grassland_final2$no), ]

#2. aggregate based on lon and lat
site_all2 <- (aggregate(NPP_grassland_final2,by=list(NPP_grassland_final2$lon,NPP_grassland_final2$lat), FUN=mean, na.rm=TRUE))
site_all2 <- site_all2[,c("lon","lat")]
for (i in 1:nrow(site_all2)){
  site_all2$site_xy[i] <- paste("b",i,sep="")
}
dim(site_all2)
# merge back to FINAL INDIVIDUALS DATA

NPP_grassland_final3 <-Reduce(function(x,y) merge(x = x, y = y, by = c("lon","lat"),all.x=TRUE), 
                              list(NPP_grassland_final2,site_all2))

NPP_grassland_final3 <- NPP_grassland_final3[order(NPP_grassland_final3$no), ]

NPP_grassland_final3$ppfd_fapar <- NPP_grassland_final3$PPFD*NPP_grassland_final3$fapar

NPP_grassland_final3$filter[NPP_grassland_final3$management == "M"] <- "removal"
NPP_grassland_final3$filter[NPP_grassland_final3$management == "T"] <- "removal"
NPP_grassland_final3$filter[NPP_grassland_final3$biome == "marsh"] <- "removal"
NPP_grassland_final3$filter[NPP_grassland_final3$biome == "savannah"] <- "removal"

###########
#now, it is the time to merge with c3c4 information from three different sources
###########


#1. Tian Di's data
tiandi_df_sp <- read.csv("/Users/yunpeng/data/npp_stoichiometry_grasslands_tiandi/China_grassland_CN_stoichiometry_with_matched_NPP_species_legume_20201214.csv")

list_df <- vector(mode = "list", length = nrow(tiandi_df_sp))

for (i in (1:nrow(tiandi_df_sp))){
  list_df[[i]] <- strsplit(tiandi_df_sp$Species_CN[i], "_", fixed = FALSE, perl = FALSE, useBytes = FALSE)
  
}

for (a in (1:nrow(tiandi_df_sp))){
  tiandi_df_sp[a,21:33] <- list_df[[a]][[1]][1:13]
}

t1 <- tiandi_df_sp[,21:33] 
for (i in (1:nrow(t1))){
  t1$no[i] <- i
}

library(reshape)
t2 <- melt(t1, id.vars=c('no'),var='species')
t3 <- na.omit(t2)
t4 <- t3[order(t3$no), ]
t5 <- t4[,c("no","value")]
dim(t5) # number of individuals overall

final_species <- aggregate(no~value,FUN=mean,na.rm=TRUE,data=t5)
dim(final_species) #number of species type

#separate into genus species
for (i in (1:nrow(final_species))){
  final_species[i,3] <- strsplit(final_species$value[i], " ", fixed = FALSE, perl = FALSE, useBytes = FALSE)[[1]][1] #genus
  final_species[i,4] <- strsplit(final_species$value[i], " ", fixed = FALSE, perl = FALSE, useBytes = FALSE)[[1]][2] #species
}
head(final_species)
final_species <- final_species[,c(1,3,4)] 
names(final_species) <- c("speciesname","genus","species")
dim(final_species)
#csvfile <- paste("/Users/yunpeng/data/npp_stoichiometry_grasslands_tiandi/species_name.csv")
#write.csv(final_species, csvfile, row.names = TRUE)

#now, input c3/c4 information from TRY database
c3c4 <- read.csv("/Users/yunpeng/data/c3c4_species/Try20201218143430TRY_Categorical_Traits_Lookup_Table_2012_03_17_TestRelease/TRY_Categorical_Traits_Lookup_Table_2012_03_17_TestRelease.csv")
data1 <- c3c4[,c(2,4,5,18)]
dim(data1)
names(data1) <- c("speciesname","genus","species","c3")


final_species2 <- merge(final_species,data1,by=c("speciesname"),all.x=TRUE)

#after having a look at original TRY data, for NA data of final_species2: if the same Genus in TRY database all have recorded c3, then we transfer our NA of same Genus to c3;
# if c3/c4 existed in the same Genus, or Genus is missing, then we set to unknown.
final_species2$c3_final <- final_species2$c3
final_species2$c3_final[6] <- "C4"
final_species2$c3_final[c(16,17,69,70,91,98,100,106,108,110,111,113,114,
                          115,116,117,122,123,124,133,142,153,154,177,178,179,180,206)] <- "unknown"
final_species2$c3_final[final_species2$c3_final==""] <- "tranfered_c3"
final_species2$c3_final[is.na(final_species2$c3_final)==TRUE] <- "tranfered_c3"

#have a look at finalspecies c3c4 data and create a new variable name for percentage
final_species2$c3_percentage <- NA

final_species2$c3_percentage[final_species2$c3_final=="C3"] <- 1
final_species2$c3_percentage[final_species2$c3_final=="tranfered_c3"] <- 1 #with same genus in TRY that all = c3, then also converted to C3
final_species2$c3_percentage[final_species2$c3_final=="C4"] <- 0
final_species2$c3_percentage[final_species2$c3_final=="unknown"] <- NA

#now, time to merge with all individuals data
final_sp_tian <- final_species2[,c("speciesname","c3_percentage")]
names(t5) <- c("no","speciesname")

all_individuals_tian <- merge(t5,final_sp_tian,by=c("speciesname"),all.x=TRUE)
all_individuals_tian <- all_individuals_tian[order(all_individuals_tian$no), ]
#if na occured in certain speciees, then omit (na.rm=TRUE)
all_individuals_tian2 <- aggregate(all_individuals_tian,by=list(all_individuals_tian$no), FUN=mean, na.rm=TRUE)
summary(all_individuals_tian2)
all_individuals_tian2 <- all_individuals_tian2[order(all_individuals_tian2$no), ]

hist(all_individuals_tian2$c3_percentage)

#finally, cbind to original data
tiandi_df_sp2 <- tiandi_df_sp[,1:20]
tiandi_df_sp2$c3_percentage <- all_individuals_tian2$c3_percentage
summary(tiandi_df_sp2)
subset(tiandi_df_sp2,is.na(c3_percentage)==TRUE)

tiandi_df_sp3 <- tiandi_df_sp2[,c("Longitude_CN","Latitude_CN","Altitude_CN","CN_ratio_leaf","ANPP","c3_percentage")]
names(tiandi_df_sp3) <- c("lon","lat","z","CN_leaf","ANPP_2","c3_percentage_tiandi")

head(NPP_grassland_final3)

NPP_grassland_final4 <- merge(NPP_grassland_final3,tiandi_df_sp3,by=c("lon","lat","z","CN_leaf","ANPP_2"),all.x=TRUE)
summary(NPP_grassland_final4)


#2. Keith's data
keith_c3c4 <- read.csv("/Users/yunpeng/data/NPP_Yunke/NPP_Keith/orig/ABPE.csv")
keith2 <- keith_c3c4[,c("Site","ANPP","C_cycle")]
keith2$c3_percentage_keith[keith2$C_cycle=="C3"] <- 1
keith2$c3_percentage_keith[keith2$C_cycle=="C4"] <- 0
keith2$c3_percentage_keith[keith2$C_cycle=="NA"] <- NA

keith2 <- keith2[,c("Site","ANPP","c3_percentage_keith")]
names(keith2) <- c("site","ANPP_2","c3_percentage_keith")
#merged with site and ANPP_2 (so that each individual is unique).
NPP_grassland_final5 <- merge(NPP_grassland_final4,keith2,by=c("site","ANPP_2"),all.x=TRUE)
summary(NPP_grassland_final5) # 1097-1053 = 44 points were filled now

#3. Campioli
Campioli_c3c4 <- read.csv("/Users/yunpeng/data/campioli/structured_Database1Grassland.csv")
Campioli2 <- Campioli_c3c4[,c("ID","type")]
Campioli2$c3_percentage_Campioli[Campioli2$type=="c3"] <- 1
Campioli2$c3_percentage_Campioli[Campioli2$type=="c4"] <- 0
Campioli2$c3_percentage_Campioli[Campioli2$type=="c3c4"] <- NA # we don't know how much percentage they have (i.e. species number) so we can only extract them from c3c4 percentage map.
Campioli2$c3_percentage_Campioli[Campioli2$type=="NA"] <- NA

Campioli2 <- Campioli2[,c("ID","c3_percentage_Campioli")]
names(Campioli2) <- c("site","c3_percentage_Campioli")
NPP_grassland_final6 <- merge(NPP_grassland_final5,Campioli2,by=c("site"),all.x=TRUE)
summary(NPP_grassland_final6) 
#1097-1012 = 86 points were filled now, which is much less than 142 --> have a look at na table and see which site' missing c3c4 can be filled manually (because the site information given by Cambiopli in two times email are NOT completely the same!!!)
atest <- subset(NPP_grassland_final6,file=="MCampioli" & is.na(c3_percentage_Campioli)==TRUE)
#below is the manually step to fill the c3c4 based on orig c3c4 data given (it was missing in merge because the sitename was not perfectly matached)
NPP_grassland_final6$c3_percentage_Campioli[c(18,19,20,21,22,23,27,28,29,30,31,32,858,859)] <- 1
NPP_grassland_final6$c3_percentage_Campioli[c(807,808,809,810,846,853,856,857,877,878)] <- 0 #US-ccc-D01, US-ccc-D02,US-Kon-D05, US-paw-D01,US-Seg-D01,VE-ori-D01,VE-ori-D02
summary(NPP_grassland_final6) 

##Finally, combine the three above to one dataset
NPP_grassland_final7 <- NPP_grassland_final6 %>% 
  mutate(c3_percentage = coalesce(c3_percentage_tiandi,c3_percentage_keith,c3_percentage_Campioli))
summary(NPP_grassland_final7)

#Alternatively (last), now let's use c3c4 map to fill in those 82 points.
c4_still <- as.data.frame(nc_to_df(read_nc_onefile(
  "/Users/yunpeng/data/c4_still/final/c4_percentage.nc"),
  varnam = "c4"))

#use extract function to extract c4 (not gwr!)
names(c4_still) <- c("lon","lat","c4")
coordinates(c4_still) <- ~lon+lat 
gridded(c4_still) <- TRUE
rc4_global <- raster(c4_still, "c4") 

#aggregate based on lon and lat firstly
NPP_grassland_final7_site <- aggregate(NPP_grassland_final7,by=list(NPP_grassland_final7$lon,NPP_grassland_final7$lat), FUN=mean, na.rm=TRUE) #site-mean
NPP_grassland_final7_site <- NPP_grassland_final7_site[,c("lon","lat")]

sp_sites <- SpatialPoints(NPP_grassland_final7_site) # only select lon and lat

NPP_grassland_final7_site <- raster::extract(rc4_global, sp_sites, sp = TRUE) %>% as_tibble() %>% 
  right_join(NPP_grassland_final7_site, by = c("lon", "lat")) %>% 
  dplyr::rename( c4_percentage_map = c4)
dim(NPP_grassland_final7_site)
hist(NPP_grassland_final7_site$c4_percentage_map) # most c4 percentage =0, which is great.

#now, merge back to site
NPP_grassland_final7_site$c3_precentage_map <- 1-NPP_grassland_final7_site$c4_percentage_map
NPP_grassland_final7_site <- NPP_grassland_final7_site[,c("lon","lat","c3_precentage_map")]

NPP_grassland_final8 <- merge(NPP_grassland_final7,NPP_grassland_final7_site,by=c("lon","lat"),all.x=TRUE)
dim(NPP_grassland_final8)

summary(NPP_grassland_final8)

#compare measured and predicted c3 percentage --> very different!
plot(NPP_grassland_final8$c3_percentage~NPP_grassland_final8$c3_precentage_map)

NPP_grassland_final8 <- NPP_grassland_final8[order(NPP_grassland_final8$no), ]

#now, combine them: primary based on measured data, alternatively based on map data.
NPP_grassland_final9 <- NPP_grassland_final8 %>% 
  mutate(c3_percentage_final = coalesce(c3_percentage,c3_precentage_map))
#which points were filled by map
subset(NPP_grassland_final9,is.na(c3_percentage)==TRUE)$c3_percentage_final



##########NOW, final calculating gpp based on weighted sum method --> the first is only based on measured c3c4, the second is the combination of measured + map c3c4.
NPP_grassland_final9$weightedgpp_measured_c3 <- (NPP_grassland_final9$pred_gpp_c3 * NPP_grassland_final9$c3_percentage)+(NPP_grassland_final9$pred_gpp_c4 * (1-NPP_grassland_final9$c3_percentage))
NPP_grassland_final9$weightedgpp_all <- (NPP_grassland_final9$pred_gpp_c3 * NPP_grassland_final9$c3_percentage_final)+(NPP_grassland_final9$pred_gpp_c4 * (1-NPP_grassland_final9$c3_percentage_final))


#NPP_grassland_final9$weightedgpp_all_npp <- NPP_grassland_final9$weightedgpp_all*0.39
#analyse_modobs2(subset(NPP_grassland_final9,TNPP_1 >0 & weightedgpp_all_npp>0 ),"TNPP_1","weightedgpp_all_npp", type = "points")


#NPP_grassland_final9$npp_gpp <- NPP_grassland_final9$TNPP_1/NPP_grassland_final9$weightedgpp_all  
#NPP_grassland_final9$npp_gpp[NPP_grassland_final9$npp_gpp>1] <- NA
#NPP_grassland_final9$anpp_gpp <- NPP_grassland_final9$ANPP_2/NPP_grassland_final9$weightedgpp_all  
#NPP_grassland_final9$anpp_gpp[NPP_grassland_final9$anpp_gpp>1] <- NA
#NPP_grassland_final9$anpp_npp <- NPP_grassland_final9$ANPP_2/NPP_grassland_final9$TNPP_1  


#additional step: 
#add more gpp data from Compioli et al. SI table 1, and remove repeated data
#NPP_grassland_final9_v2
NPP_grassland_final9$GPP[NPP_grassland_final9$site=="IT-bea-D02"] <- 1568
NPP_grassland_final9$GPP[NPP_grassland_final9$site=="US-che-D01"] <- 626
NPP_grassland_final9$GPP[NPP_grassland_final9$site=="DE-gri-D01"] <- 1233#repeated
NPP_grassland_final9$GPP[NPP_grassland_final9$site=="CN-Hab-F01"] <- 634
NPP_grassland_final9$GPP[NPP_grassland_final9$site=="RU-ha1-F01"] <- 519#repeated
NPP_grassland_final9$GPP[NPP_grassland_final9$site=="RU-ha3-F01"] <- 526 #repeated
NPP_grassland_final9$GPP[NPP_grassland_final9$site=="CN-Inn-D01_C"] <- 182
NPP_grassland_final9$GPP[NPP_grassland_final9$site=="US-kbs-D01"] <- 1015
NPP_grassland_final9$GPP[NPP_grassland_final9$site=="US-kbs-D04"] <- 512
NPP_grassland_final9$GPP[NPP_grassland_final9$site=="US-kbs-D05"] <- 374
NPP_grassland_final9$GPP[NPP_grassland_final9$site=="US-kbs-D03"] <- 793
NPP_grassland_final9$GPP[NPP_grassland_final9$site=="US-jas-D01"] <- 516
NPP_grassland_final9$GPP[NPP_grassland_final9$site=="US-kon-D05"] <- 1151
NPP_grassland_final9$GPP[NPP_grassland_final9$site=="RU-krs-D01"] <- 1611
NPP_grassland_final9$GPP[NPP_grassland_final9$site=="CA-Let-F01"] <- 280
NPP_grassland_final9$GPP[NPP_grassland_final9$site=="CA-mat-D01"] <- 786
NPP_grassland_final9$GPP[NPP_grassland_final9$site=="US-osg-D01"] <- 1890
NPP_grassland_final9$GPP[NPP_grassland_final9$site=="CG-tch-D01"] <- 1572
NPP_grassland_final9$GPP[NPP_grassland_final9$site=="US-Spe-D01"] <- 829

#interpolate gpp from keith's source
#NPP_grassland_final9$GPP[NPP_grassland_final9$site=="CN-Inn-F01"] <- 204 #considering two below, don't interpolate firstly
#NPP_grassland_final9$GPP[NPP_grassland_final9$site=="KZ-shr-D01"] <- 357 #incosistent between keith and cambiopli's data!
#NPP_grassland_final9$GPP[NPP_grassland_final9$site=="RU-ha2-F01"] <- 648 #incosistent between keith and cambiopli's data!

#remove repeated data from keith (that has already been inputted in Cambioli's dataset - about its npp, anpp, bnpp)
NPP_grassland_final10 <- subset(NPP_grassland_final9,rep_info!="rep" & rep_info!="rep1"& rep_info!="rep3")

summary(NPP_grassland_final10)

dim(subset(NPP_grassland_final10,TNPP_1<weightedgpp_all))
dim(subset(NPP_grassland_final10,TNPP_1>weightedgpp_all))

#only filter grassland in Mcampioli data
NPP_grassland_final10$filter2[NPP_grassland_final10$biome_MCampioli == "marsh"] <- "removal2"
NPP_grassland_final10$filter2[NPP_grassland_final10$biome_MCampioli == "savannah"] <- "removal2"

NPP_grassland_final11 <- subset(NPP_grassland_final10,is.na(filter2)==TRUE)

dim(subset(NPP_grassland_final11,TNPP_1<weightedgpp_all))
dim(subset(NPP_grassland_final11,TNPP_1>weightedgpp_all))
#20% points are outlier 1
outlier1 <- subset(NPP_grassland_final11,TNPP_1>weightedgpp_all)
library(rworldmap)
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-180, 180), ylim = c(-75, 75), asp = 1)
points(outlier1$lon,outlier1$lat, col="red", pch=16,cex=1)
title("Outlier plots where measured NPP > predicted GPP")

hist(outlier1$BNPP_1/outlier1$TNPP_1,main="bnpp/npp in those outlier plots")

#12% points are outlier 2
outlier2 <- subset(NPP_grassland_final11,TNPP_1/weightedgpp_all<0.2)
library(rworldmap)
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-180, 180), ylim = c(-75, 75), asp = 1)
points(outlier2$lon,outlier2$lat, col="red", pch=16,cex=1)
title("Outlier plots where measured NPP / predicted GPP < 0.2")

hist(outlier2$BNPP_1/outlier2$TNPP_1,main="bnpp/npp in those outlier plots")

#make models to have a look --> CUE = 0.40462 --> reasonable
summary(lmer((TNPP_1)~-1+(weightedgpp_all)+(1|site),data=NPP_grassland_final11))
r.squaredGLMM(lmer((TNPP_1)~-1+(weightedgpp_all)+(1|site),data=NPP_grassland_final11))

#summary(lmer((TNPP_1)~-1+(weightedgpp_all)+(1|site_xyz),data=NPP_grassland_final11))

summary(lmer(log((ANPP_2/TNPP_1)/(1-(ANPP_2/TNPP_1)))~Tg+alpha+(1|site),data=NPP_grassland_final11))
r.squaredGLMM(lmer(log((ANPP_2/TNPP_1)/(1-(ANPP_2/TNPP_1)))~Tg+alpha+(1|site),data=NPP_grassland_final11))

#summary(lmer(log((ANPP_2/TNPP_1)/(1-(ANPP_2/TNPP_1)))~Tg+alpha+(1|site_xyz),data=NPP_grassland_final11))

#aaa <- subset(NPP_grassland_final11,ANPP_2>0&TNPP_1>0&Tg>0&alpha>0)

summary(NPP_grassland_final11$CN_root_final)


#calculate weighted MAX vcmax25 from max_vcmax25_c3 and max_vcmax25_c4, based on final measured c3/c4 percentage.
NPP_grassland_final11$maxvcmax25_all <- (NPP_grassland_final11$max_vcmax25_c3 * NPP_grassland_final11$c3_percentage_final)+
  (NPP_grassland_final11$max_vcmax25_c4 * (1-NPP_grassland_final11$c3_percentage_final))


NPP_grassland_final11$pred_npp <- NPP_grassland_final11$weightedgpp_all * 0.4046
NPP_grassland_final11$pred_anpp <- NPP_grassland_final11$pred_npp * 
  (1/(1+exp(-(1.8720 * NPP_grassland_final11$alpha + 0.0385 * NPP_grassland_final11$Tg + -2.5642))))

NPP_grassland_final11$pred_bnpp <- NPP_grassland_final11$pred_npp - NPP_grassland_final11$pred_anpp

#input max vcmax25

firstyr_data <- 1982 # In data file, which is the first year
endyr_data <- 2011 # In data file, which is the last year
location <- "~/data/output/latest/"
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

Vcmax25_df <- cbind(elev,vcmax25_df$vcmax25)
names(Vcmax25_df) <- c("lon","lat","z","Vcmax25")

LMA <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/nimpl_sofun_inputs/map/Final_ncfile/LMA.nc"),varnam = "LMA"))

LMA_df <- cbind(elev,LMA$myvar)
names(LMA_df) <- c("lon","lat","z","LMA")
a <- 1.5

NPP_grassland_final11$Vcmax25 <- NA
NPP_grassland_final11$LMA <- NA

for (i in 1:nrow(NPP_grassland_final11)) {
  tryCatch({
   #LMA
    LMA_global <- na.omit(LMA_df)
    NRE_part <- subset(LMA_global,lon>(NPP_grassland_final11[i,1]-a)&lon<(NPP_grassland_final11[i,1]+a)&
                         lat>(NPP_grassland_final11[i,2]-a)&lat<(NPP_grassland_final11[i,2]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- NPP_grassland_final11[i, c("lon","lat","z")]
    coordinates(NRE_coord) <- c("lon","lat")
    NPP_grassland_final11[i,c("LMA")]  <- (gwr(LMA ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #Vcmax25
    Vcmax25_global <- na.omit(Vcmax25_df)
    NRE_part <- subset(Vcmax25_global,lon>(NPP_grassland_final11[i,1]-a)&lon<(NPP_grassland_final11[i,1]+a)&
                         lat>(NPP_grassland_final11[i,2]-a)&lat<(NPP_grassland_final11[i,2]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- NPP_grassland_final11[i, c("lon","lat","z")]
    coordinates(NRE_coord) <- c("lon","lat")
    NPP_grassland_final11[i,c("Vcmax25")]  <- (gwr(Vcmax25 ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
  }, error=function(e){})} 
  
#using simulated vcmax25
NPP_grassland_final11$pred_leafnc <- (0.0161/0.5) + (0.0041/0.5) * NPP_grassland_final11$Vcmax25/NPP_grassland_final11$LMA 

#using weighted max vcmax25 from rsofun, from c3/c4 measured info
NPP_grassland_final11$pred_leafnc <- (0.0161/0.5) + (0.0041/0.5) * NPP_grassland_final11$maxvcmax25_all/NPP_grassland_final11$LMA 

#a new statistical model for grassland only?
cnmodel <- NPP_grassland_final11
cnmodel$nc <- 1/ cnmodel$CN_leaf_final
cnmodel$vc25_lma <- cnmodel$maxvcmax25_all/cnmodel$LMA
cnmodel2 <- aggregate(cnmodel,by=list(cnmodel$site), FUN=mean, na.rm=TRUE) #site-mean

summary(lmer((nc)~vc25_lma+(1|site_xy),data=cnmodel))
NPP_grassland_final11$pred_leafnc <- (0.160673/0.5) + (-0.071485/0.5) * NPP_grassland_final11$maxvcmax25_all/NPP_grassland_final11$LMA 

summary(lmer((nc)~maxvcmax25_all+LMA+(1|site_xy),data=cnmodel))
NPP_grassland_final11$pred_leafnc <- 7.956e-02 -9.698e-04*NPP_grassland_final11$maxvcmax25_all + 1.114e-03*NPP_grassland_final11$LMA 

summary(NPP_grassland_final11$CN_leaf_final)
NPP_grassland_final11$pred_leafnc <- 1/19.100

NPP_grassland_final11$pred_lnf <- NPP_grassland_final11$pred_anpp*NPP_grassland_final11$pred_leafnc
NPP_grassland_final11$pred_bnf <- NPP_grassland_final11$pred_bnpp/46

#Now, time to examine our data

analyse_modobs2(NPP_grassland_final11,
                "pred_lnf", "lnf_obs_final",type = "points")
ggplot(NPP_grassland_final11, aes(x=pred_lnf, y=lnf_obs_final)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+ xlim(c(0,10))+
  xlab("Prediction")+ylab("Observation")+theme_classic()


analyse_modobs2(subset(NPP_grassland_final11,TNPP_1/weightedgpp_all > 0.2 & TNPP_1/weightedgpp_all <1),
                "pred_lnf", "lnf_obs_final",type = "points")

hist(NPP_grassland_final11$lnf_obs_final)

analyse_modobs2(NPP_grassland_final11,"weightedgpp_all", "GPP",type = "points")
ggplot(NPP_grassland_final11, aes(x=weightedgpp_all, y=GPP)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()


analyse_modobs2(subset(NPP_grassland_final11,TNPP_1/weightedgpp_all > 0.2 & TNPP_1/weightedgpp_all <1),"pred_npp", "TNPP_1",type = "points")
analyse_modobs2(NPP_grassland_final11,"pred_npp", "TNPP_1",type = "points")

NPP_grassland_final11$file[NPP_grassland_final11$file=="Tiandi"] <- "China Grassland"
NPP_grassland_final11$file[NPP_grassland_final11$file=="MCampioli"] <- "M.Campioli et al. 2015 Nature Geoscience"

ggplot(NPP_grassland_final11, aes(x=pred_npp, y=TNPP_1)) +
  geom_point(aes(color=factor(file)))+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()  + ggtitle("Observed NPP vs. Predicted NPP")

My_Theme = theme(
  axis.title.x = element_text(size = 14),
  axis.text.x = element_text(size = 20),
  axis.title.y = element_text(size = 14),
  axis.text.y = element_text(size = 20))

test <- subset(NPP_grassland_final11,file !="Keith")

ggplot(test, aes(x=weightedgpp_all, y=TNPP_1)) +
  geom_point(aes(color=factor(file)))+geom_abline(intercept=0,slope=0.8)+
  geom_abline(intercept=0,slope=0.2)+
  xlab(" Predicted GPP (gC/m2/yr)")+ylab("Measured NPP (gC/m2/yr)")+theme_classic() +My_Theme +
  ggtitle("Measured NPP vs. Predicted GPP
          
      Two lines are NPP/GPP = 0.8 and 0.2 separately")

sara <- subset(NPP_grassland_final11,file != "China Grassland")
outlier_sara <- subset(sara,TNPP_1/weightedgpp_all >= 0.8 |
                         TNPP_1/weightedgpp_all < 0.2)
outlier_sara2 <- outlier_sara[,c("site","lon","lat","z","file","TNPP_1","ANPP_2","BNPP_1","pft","weightedgpp_all","GPP")]
names(outlier_sara2) <- c("site","lon","lat","z","file","TNPP_1","ANPP_2","BNPP_1","pft","predicted_gpp","measured_gpp")
#csvfile <- paste("/Users/yunpeng/data/outlier_sara.csv")
#write.csv(outlier_sara2, csvfile, row.names = TRUE)

#analyse_modobs2(subset(NPP_grassland_final11,TNPP_1/weightedgpp_all > 0.2 & TNPP_1/weightedgpp_all <1),"pred_anpp", "ANPP_2",type = "points")

#analyse_modobs2(NPP_grassland_final11,"pred_anpp", "ANPP_2",type = "points")

#remove keith's palanation and cropland
NPP_grassland_final12 <- subset(NPP_grassland_final11,pft !="Plantation" & pft !="Cropland")

ggplot(NPP_grassland_final12, aes(x=pred_anpp, y=ANPP_2)) +
  geom_point(aes(color=factor(file)))+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Predicted ANPP")+ylab("Measured ANPP")+theme_classic() + My_Theme +ggtitle("Observed ANPP vs. Predicted ANPP")



analyse_modobs2(NPP_grassland_final11,
                "pred_bnpp", "BNPP_1",type = "points")

analyse_modobs2(NPP_grassland_final11,
                "pred_bnf", "bnf_obs_final",type = "points")

save.image(file = "/Users/yunpeng/yunkepeng/nimpl_sofun_inputs/grassland/grassland_site_simulation.Rdata")
