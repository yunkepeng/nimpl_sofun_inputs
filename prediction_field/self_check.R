#This is just self check for authors, to make sure that output of Prediction_field.Rmd (before CDO preprecessing) can be best merged with final_ncfile/, and data is consistent!

library(rbeni)
require(data.table)
library(maps)
library(lme4)
library(MuMIn)
library(lmerTest)
library(elevatr)
library(raster)
library(devtools)
library(ingestr)
library(tibble)
library(spgwr)
library(rworldmap)
library(colorRamps)

#input elevation for global grids
elev_nc <- read_nc_onefile("~/data/watch_wfdei/WFDEI-elevation.nc")
elev <- as.data.frame(nc_to_df(elev_nc, varnam = "elevation"))
head(elev) 


Tg <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/nimpl_sofun_inputs/map/Final_ncfile/Tg.nc"),
  varnam = "Tg"))
Tg$myvar[Tg$myvar==9999] <- NA

PPFD <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/nimpl_sofun_inputs/map/Final_ncfile/PPFD.nc"),
  varnam = "PPFD"))
PPFD$myvar[PPFD$myvar==9999] <- NA

vpd <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/nimpl_sofun_inputs/map/Final_ncfile/vpd.nc"),
  varnam = "vpd"))
vpd$myvar[vpd$myvar==9999] <- NA

Vcmax25 <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/nimpl_sofun_inputs/map/Final_ncfile/Vcmax25.nc"),
  varnam = "Vcmax25"))
Vcmax25$myvar[Vcmax25$myvar==9999] <- NA

age <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/nimpl_sofun_inputs/map/Final_ncfile/age.nc"),
  varnam = "age"))
age$myvar[age$myvar==9999] <- NA

alpha <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/nimpl_sofun_inputs/map/Final_ncfile/alpha.nc"),
  varnam = "alpha"))
alpha$myvar[alpha$myvar==9999] <- NA

CNrt <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/nimpl_sofun_inputs/map/Final_ncfile/CNrt.nc"),
  varnam = "CNrt"))
CNrt$myvar[CNrt$myvar==9999] <- NA

fAPAR <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/nimpl_sofun_inputs/map/Final_ncfile/fAPAR.nc"),
  varnam = "fAPAR"))
fAPAR$myvar[fAPAR$myvar==9999] <- NA

LMA <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/nimpl_sofun_inputs/map/Final_ncfile/LMA.nc"),
  varnam = "LMA"))
LMA$myvar[LMA$myvar==9999] <- NA

#cbind all predictors, and its lon, lat, z
all_predictors <- cbind(elev,Tg$myvar,PPFD$myvar,vpd$myvar,
                        alpha$myvar,Vcmax25$myvar,LMA$myvar,
                        fAPAR$myvar,CNrt$myvar,age$myvar)

names(all_predictors) <- c("lon","lat","z","Tg","PPFD","vpd",
                           "alpha","Vcmax25","LMA","fAPAR","CNrt","age")
all_predictors

#already confirmed that df_Tg, df_PPFD....from Prediction_field.Rmd, is all consistent with all_predictors$Tg, all_predictors$PPFD here. Which is great
#LMA needs more detection. Why original data from gis?
