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


load(file = "/Users/yunpeng/yunkepeng/nimpl_sofun_inputs/forest/Forest_site_check.Rdata")
#at the end of code...


#read complete dataset for measurement, after L1-L352 in /Users/yunpeng/yunkepeng/nimpl_sofun_inputs/output_check/Forest_Global_check.Rmd
NPP <- read.csv("/Users/yunpeng/data/forest_npp/NPP_final_all.csv")
#extract forest only -->with corrected sitename in all (no NA)
NPP_Forest <- subset(NPP,pft2=="Forest")


########read sitename and sitename2, which was created when used for dowloading climate forcing and fapar forcing separately
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
summary(NPP_Forest$lnf_obs_final - NPP_final2$lnf_obs_final)
summary(NPP_Forest$wnf_obs - NPP_final2$wnf_obs)

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

forest_site$pred_wnf <- forest_site$pred_wnpp/97
forest_site$pred_bnf <- forest_site$pred_bnpp/42

forest_site$TNPP_1 <- siteinfo_final$TNPP_1
forest_site$ANPP_2 <- siteinfo_final$ANPP_2
forest_site$BNPP_1 <- siteinfo_final$BNPP_1
forest_site$NPP.foliage <- siteinfo_final$NPP.foliage
forest_site$NPP.wood <- siteinfo_final$NPP.wood
forest_site$CN_leaf_final <- siteinfo_final$CN_leaf_final
forest_site$lnf_obs_final <- siteinfo_final$lnf_obs_final
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

# predict leaf cn separately
forest_site2 <- subset(forest_site,rep_info!="rep" & rep_info!="rep1"& rep_info!="rep3")

#fit a new model
library(lme4)
library(nlme)
library(lmerTest)
library("PerformanceAnalytics")
library(MuMIn)
r.squaredGLMM(lmer(CN_leaf_final ~ max_vcmax25+ LMA + (1|site),data=forest_site2))
summary(lmer(CN_leaf_final ~ max_vcmax25+ LMA + (1|site),data=forest_site2))
forest_site2$pred_fitted_leafcn <- 0.30275 * forest_site2$max_vcmax25 + 0.21866*forest_site2$LMA -4.92165

forest_site2$CN_leaf_final[forest_site2$CN_leaf_final>100] <- NA
ggplot(data=forest_site2, aes(x=pred_fitted_leafcn, y=CN_leaf_final)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()

summary(lm(CN_leaf_final~pred_fitted_leafcn,forest_site2))

forest_site2$pred_fit_lnf <- forest_site2$pred_lnpp/forest_site2$pred_fitted_leafcn

ggplot(data=forest_site2, aes(x=pred_fit_lnf, y=lnf_obs_final)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()

summary(lm(lnf_obs_final~pred_fit_lnf,forest_site2))

####new design###
#leafnc predicted by max vcmax25
forest_site2$vc25_lma <-forest_site2$max_vcmax25/forest_site2$LMA
forest_site2$vc25_lma <-forest_site2$Vcmax25/forest_site2$LMA
forest_site2$ncleaf_final <- 1/forest_site2$CN_leaf_final

summary(lmer(ncleaf_final ~ max_vcmax25 + (1|site),data=forest_site2))

summary(lmer(ncleaf_final ~ vc25_lma + (1|site),data=forest_site2))

#aggregate to sites
forest_site2_sitemean <- aggregate(forest_site2,by=list(forest_site2$site), FUN=mean, na.rm=TRUE) #site-mean
summary(lm(ncleaf_final ~ max_vcmax25 ,data=forest_site2_sitemean))

summary(lm(ncleaf_final ~ vc25_lma ,data=forest_site2_sitemean))

#forest_site2$pred_leafnc_new <- -2.421e-04 * forest_site2$max_vcmax25 + 4.706e-02 # all species
forest_site2$pred_leafnc_new <- 0.028026 + 0.022874 * forest_site2$max_vcmax25/forest_site2$LMA # all species

#forest_site2$pred_leafnc_new <- 1/(1.782e+01 -4.394e-02 * forest_site2$max_vcmax25 + 1.019e-01*forest_site2$LMA) # fitted by real data's model

forest_site2$nc_leaf_obs <- 1/forest_site2$CN_leaf_final

ggplot(data=forest_site2, aes(x=pred_leafnc_new, y=nc_leaf_obs)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()
summary(lm(nc_leaf_obs~pred_leafnc_new,forest_site2))


forest_site2$pred_fit_lnf_new <- forest_site2$pred_lnpp * forest_site2$pred_leafnc_new

ggplot(data=forest_site2, aes(x=pred_fit_lnf_new, y=lnf_obs_final)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()

summary(lm(lnf_obs_final~pred_fit_lnf_new,forest_site2))

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

