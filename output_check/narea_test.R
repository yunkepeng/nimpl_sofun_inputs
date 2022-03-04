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
library(raster)
library(spgwr)
library(maps)
library(devtools)
#install_github("stineb/rbeni")
library(rbeni)
#1. In our path (with multiple years data), identify which is the first year and end year of those files
firstyr_data <- 2007 # In data file, which is the first year
endyr_data <- 2018 # In data file, which is the last year
#location <- "~/data/output_leafcn/species/" #species-based coefficients
location <- "~/data/output_leafcn/" # site-based coefficients

alloutput_list <- list.files(location,full.names = T)

#input elevation nc file, which will be cbind with global df directly
elev_nc <- read_nc_onefile("~/data/watch_wfdei/WFDEI-elevation.nc")
#elev_nc <- read_nc_onefile("D:/PhD/nimpl_sofun_inputs/Data/Elevation/WFDEI-elevation.nc")
elev <- as.data.frame(nc_to_df(elev_nc, varnam = "elevation"))
head(elev) # this is consistent with df coord below

#2. Create a function to specify path, loop many years nc file and output a dataframe (lon, lat, var).
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

leafcn_df <- inputnc("leafcn",2007,2018) # this is actually leaf n/c. We used this time period, because this is when all leaf C/N data collected
leafcn_df$leafcn <- 1/leafcn_df$leafcn # we converted it to leaf c/n here.
summary(leafcn_df$leafcn)

#input a large data
SP_input <- read.csv(file="/Users/yunpeng/Desktop/VcmaxMSdata/final_individuals.csv") #all individuals

sitemean <- aggregate(SP_input,by=list(SP_input$lon,SP_input$lat), FUN=mean, na.rm=TRUE) #site-mean
dim(sitemean)

leafcn_site <- sitemean[,c("lon","lat","Elevation","narea","lma")]
names(leafcn_site) <- c("lon","lat","z","narea","lma")
summary(leafcn_site)

dim(leafcn_site) # we will use this database for model validation later on.


#4. Start geographically weighted regressions 
# Now, both global_df and site_df have included four columns: lon, lat, z, var
a <- 1.5 # which degree (distance) of grid when interpolating gwr from global grids
i <- 1

#use gwr to obtain site-level leaf c/n. then calculating leaf nmass for both pred and obs.
for (i in c(1:nrow(leafcn_site))){
  leafcn_global <- na.omit(leafcn_df)
  leafcn_part <- subset(leafcn_global,lon>(leafcn_site[i,1]-a)&lon<(leafcn_site[i,1]+a)&
                          lat>(leafcn_site[i,2]-a)&lat<(leafcn_site[i,2]+a))
  coordinates(leafcn_part) <- c("lon","lat")
  gridded(leafcn_part) <- TRUE
  
  leafcn_coord <- leafcn_site[i,1:3]
  coordinates(leafcn_coord) <- c("lon","lat")
  leafcn_site$pred[i] <- (gwr(leafcn ~ z, leafcn_part, bandwidth = 1.06, fit.points =leafcn_coord,predictions=TRUE))$SDF$pred
}
leafcn_site$nmass_obs <- 1000*(leafcn_site$narea/leafcn_site$lma)

leafcn_site$nmass_pred <- 1000*0.4638/leafcn_site$pred
dim(leafcn_site)
final <- leafcn_site[,c("nmass_obs","nmass_pred")]
hist(leafcn_site$nmass_obs)
hist(leafcn_site$nmass_pred)

#still not good when looking all sites
analyse_modobs2(final,"nmass_obs","nmass_pred", type = "points")


