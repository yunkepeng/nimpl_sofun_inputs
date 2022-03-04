##1.Sara Vicca
#1.1 NPP
Sara_NPP <- read.csv("/Users/yunpeng/data/NPP_Yunke/NPP_Vicca/orig/Forests_Colin_NPP.csv")
Sara_NPP$Source_NPP <- Sara_NPP$Source
Sara_NPP <- Sara_NPP[,c("Plot","Begin.year","End.year","NPP.foliage","NPP.stem","NPP.wood","NPP.fine","NPP.coarse","ANPP_2","BNPP_1","TNPP_1","Source_NPP")]
#TNPP_1 = ANPP_2 + BNPP_1 (not including understory)
#ANPP_2 = NPP.foliage + NPP.wood

#convert NA begin year
Sara_NPP$Begin.year[Sara_NPP$Begin.year==9999] <- Sara_NPP$End.year[Sara_NPP$Begin.year==9999]
Sara_NPP$Begin.year[Sara_NPP$Begin.year==9999] <- Sara_NPP$End.year[Sara_NPP$Begin.year==9999]

#Sara_NPP$End.year - Sara_NPP$Begin.year

Sara_NPP$no <- c(1:nrow(Sara_NPP))


#1.2 siteinfo
Sara_NPP_siteinfo <- read.csv("/Users/yunpeng/data/NPP_Yunke/NPP_Vicca/orig/Forests_Colin_siteinfo.csv")
Sara_NPP_siteinfo$Plot <- Sara_NPP_siteinfo$Plot.name

for (i in 1:nrow(Sara_NPP_siteinfo)){
  if (Sara_NPP_siteinfo$Direction.Latitude[i] == "S"){
    Sara_NPP_siteinfo$lat[i] <- -(Sara_NPP_siteinfo$Latitude[i])
  } else {
    Sara_NPP_siteinfo$lat[i] <- Sara_NPP_siteinfo$Latitude[i]
  }
  if (Sara_NPP_siteinfo$Direction.Longitude[i] == "W"){
    Sara_NPP_siteinfo$lon[i] <- -(Sara_NPP_siteinfo$Longitude[i])
  } else {
    Sara_NPP_siteinfo$lon[i] <- Sara_NPP_siteinfo$Longitude[i]
  }
}
Sara_NPP_siteinfo$Source_siteinfo <- Sara_NPP_siteinfo$Source.1
Sara_NPP_siteinfo <- Sara_NPP_siteinfo[,c("Plot","lon","lat","Elevation","Evergreen.Deciduous","Management.code","Management","Source_siteinfo")]

Sara_NPP2 <- merge(Sara_NPP,Sara_NPP_siteinfo,by=c("Plot"),all.x=TRUE)

#interpolate missing elevation - let's interpolate them by etopo
Sara_NPP2_elv_missing <- subset(Sara_NPP2,is.na(Elevation)==TRUE)
Sara_NPP2_elv_missing_Plot <- aggregate(Sara_NPP2_elv_missing,by=list(Sara_NPP2_elv_missing$Plot), FUN=mean, na.rm=TRUE)
Sara_NPP2_elv_missing_Plot <-Sara_NPP2_elv_missing_Plot[,c("Group.1","lon","lat")]
names(Sara_NPP2_elv_missing_Plot) <- c("sitename","lon","lat")
devtools::load_all("/Users/yunpeng/yunkepeng/Grassland_new_ingestr_rsofun_20210326/ingestr/")
df_etopo <- ingest(
  Sara_NPP2_elv_missing_Plot,
  source = "etopo1",
  dir = "~/data/etopo/" 
)
Sara_NPP2_elv_missing_Plot$Elevation <- as.numeric(as.data.frame(df_etopo$data))
Sara_NPP2_elv_missing_Plot$Elevation
names(Sara_NPP2_elv_missing_Plot) <- c("Plot","lon","lat","Elevation_etopo")

#now, interpolate those NA elevation by etopo
Sara_NPP2 <- merge(Sara_NPP2,Sara_NPP2_elv_missing_Plot,by=c("Plot","lon","lat"),all.x=TRUE)
Sara_NPP2$Elevation[is.na(Sara_NPP2$Elevation)==TRUE] <- Sara_NPP2$Elevation_etopo[is.na(Sara_NPP2$Elevation)==TRUE]
Sara_NPP2$Elevation

#Now, add site-level stand-age
Sara_age <- read.csv("/Users/yunpeng/data/NPP_Yunke/NPP_Vicca/orig/Forests_Colin_age.csv")
Sara_age <- Sara_age[,c("Plot","Stand.age")]
Sara_age_site <- aggregate(Sara_age,by=list(Sara_age$Plot), FUN=mean, na.rm=TRUE)
Sara_age_site <- subset(Sara_age_site,Stand.age>0)
Sara_age_site <- Sara_age_site[,c(1,3)]
names(Sara_age_site) <- c("Plot","age")
summary(Sara_age_site)

Sara_NPP3 <- merge(Sara_NPP2,Sara_age_site,by=c("Plot"),all.x=TRUE)

#Now, add site-level LAI
Sara_LAI <- read.csv("/Users/yunpeng/data/NPP_Yunke/NPP_Vicca/orig/Forests_Colin_LAI.csv")
Sara_LAI <- Sara_LAI[,c("Plot","LAI")]
Sara_LAI_site <- aggregate(Sara_LAI,by=list(Sara_LAI$Plot), FUN=mean, na.rm=TRUE)
Sara_LAI_site <- Sara_LAI_site[,c(1,3)]
names(Sara_LAI_site) <- c("Plot","LAI")
Sara_LAI_site$observedfAPAR <- 1-exp(-0.5 * Sara_LAI_site$LAI)
hist(Sara_LAI_site$observedfAPAR)

Sara_NPP4 <- merge(Sara_NPP3,Sara_LAI_site,by=c("Plot"),all.x=TRUE)

#now, merged with GPP (1) primarily based on plot + start.year + end.year and (2) based on average of plot
#firstly, aggregate based on sitename, start.year and end.year
Sara_GPP <- read.csv("/Users/yunpeng/data/NPP_Yunke/NPP_Vicca/orig/Forests_Colin_GPP.csv")
Sara_GPP <- Sara_GPP[,c("Plot","GPP","Begin.year","End.year")]
Sara_GPP$Begin.year[Sara_GPP$Begin.year==9999] <- Sara_GPP$End.year[Sara_GPP$Begin.year==9999]
Sara_GPP <- subset(Sara_GPP,GPP>0)
Sara_GPP_site <- aggregate(Sara_GPP,by=list(Sara_GPP$Plot,Sara_GPP$Begin.year,Sara_GPP$End.year), FUN=mean, na.rm=TRUE)
Sara_GPP_site <- Sara_GPP_site[,c(1,2,3,5)]
names(Sara_GPP_site) <- c("Plot","Begin.year","End.year","GPP")
Sara_NPP5 <- merge(Sara_NPP4,Sara_GPP_site,by=c("Plot","Begin.year","End.year"),all.x=TRUE)

#alternatively, aggregate based on site only
Sara_GPP <- read.csv("/Users/yunpeng/data/NPP_Yunke/NPP_Vicca/orig/Forests_Colin_GPP.csv")
Sara_GPP <- Sara_GPP[,c("Plot","GPP")]
Sara_GPP_site <- aggregate(Sara_GPP,by=list(Sara_GPP$Plot), FUN=mean, na.rm=TRUE)
Sara_GPP_site <- subset(Sara_GPP_site,GPP>0)
Sara_GPP_site <- Sara_GPP_site[,c(1,3)]
names(Sara_GPP_site) <- c("Plot","GPP2")
Sara_NPP6 <- merge(Sara_NPP5,Sara_GPP_site,by=c("Plot"),all.x=TRUE)

for (i in 1:nrow(Sara_NPP6)){
  if (is.na(Sara_NPP6$GPP[i]) == TRUE){ 
    Sara_NPP6$GPP[i] <- Sara_NPP6$GPP2[i]
  } else {
    Sara_NPP6$GPP[i] <- Sara_NPP6$GPP[i]
  }
}

Sara_NPP6 <- Sara_NPP6[,!(names(Sara_NPP6) %in% "GPP2")]

#add alpha - as obtained earlier in SPLASH
Sara_NPP6 <- Sara_NPP6[order(Sara_NPP6$no), ]
alphalist3 <- read.csv(file="/Users/yunpeng/data/NPP_Yunke/NPP_Vicca/climates_alpha.csv")$alpha
Sara_NPP6$alpha <- alphalist3

#add site-level soil C/N
Sara_CN <- read.csv(file="/Users/yunpeng/data/NPP_Yunke/NPP_Vicca/orig/References_Yunke_soilCN.csv")
Sara_CN <- Sara_CN[,c("Plot.name","Soil.C.N")]
Sara_CN$soilCN <- (as.numeric(gsub(",",".",Sara_CN[,2])))
hist(Sara_CN$soilCN)
Sara_CN_site <- aggregate(Sara_CN,by=list(Sara_CN$Plot.name), FUN=mean, na.rm=TRUE)
Sara_CN_site <- subset(Sara_CN_site,soilCN>0)
Sara_CN_site$Plot <- Sara_CN_site$Group.1
Sara_CN_site <- Sara_CN_site[,c("Plot","soilCN")]

Sara_NPP7 <- merge(Sara_NPP6,Sara_CN_site,by=c("Plot"),all.x=TRUE)
Sara_NPP7 <- Sara_NPP7[order(Sara_NPP7$no), ]
Sara_NPP7$pft <- "Forest"
Sara_NPP7$file <- "Sara Vicca"

#statistical model
library(lme4)
library(nlme)
library(lmerTest)
library("PerformanceAnalytics")
library(MuMIn)
library(tidyverse)
mod_anpp <- lmer(log((ANPP_2/GPP)/(1-(ANPP_2/GPP))) ~ log(soilCN) + log(age) + alpha + observedfAPAR + (1|Plot), data = Sara_NPP7)
summary(mod_anpp)
r.squaredGLMM(mod_anpp)

mod_tnpp <- lmer(log((TNPP_1/GPP)/(1-(TNPP_1/GPP))) ~ log(soilCN) + log(age) + observedfAPAR  + (1|Plot), data = Sara_NPP7)
summary(mod_tnpp)
r.squaredGLMM(mod_tnpp)

#now, add Malhi
NPP_Malhi <- read.csv("/Users/yunpeng/data/NPP_Yunke/NPP_Malhi/NPP_Malhi.csv")
NPP_Malhi <- NPP_Malhi[,c("site","lon","lat","z","file","Begin_year","End_year","Source","NPP.foliage","NPP.stem","NPP.wood","NPP.fine","NPP.coarse","ANPP_2","BNPP_1","TNPP_1","GPP")]
names(NPP_Malhi) <- c("Plot","lon","lat","Elevation","file","Begin.year","End.year","Source_NPP","NPP.foliage","NPP.stem","NPP.wood","NPP.fine","NPP.coarse","ANPP_2","BNPP_1","TNPP_1","GPP")
NPP_Malhi$Management.code <- "UM"
NPP_Malhi$pft <-"Forest"

#now, add Keith (take care about rep)
NPP_Keith <- read.csv("/Users/yunpeng/data/NPP_Yunke/NPP_Keith/orig/ABPE.csv")
NPP_Keith <- NPP_Keith[,c("Site","Ecosystem","age","lat","long","Elevation","Mgmt_code","ANPP","GPP","Source")]
names(NPP_Keith) <- c("Plot","pft","age","lat","lon","Elevation","Management.code","ANPP_2","GPP","Source_NPP")
NPP_Keith$Begin.year <- 1991
NPP_Keith$End.year <- 2010
NPP_Keith$file <- "Keith"

NPP_Sara_Malhi_Keith <- dplyr::bind_rows(Sara_NPP7, NPP_Malhi,NPP_Keith) 

NPP_Sara_Malhi_Keith <- NPP_Sara_Malhi_Keith %>% 
  rename(
    site = Plot,
    Begin_year = Begin.year,
    End_year = End.year,
    z = Elevation)

head(NPP_Sara_Malhi_Keith)

#now, add ForC (take care about rep)
#1. Input original dataset of Forc

Forc <- read.csv(file="~/data/NPP_Yunke/NPP_ForC/orig/ForC_measurements.csv")
variablelist <- c("GPP_C","NPP_1_C","ANPP_woody_stem_C","ANPP_foliage_C","ANPP_woody_C","ANPP_2_C","BNPP_root_C","BNPP_root_coarse_C","BNPP_root_fine_C")
length(variablelist)
mylist <- vector(mode = "list", length = length(variablelist))

for (i in 1:length(variablelist)){
  mylist[[i]] <- subset(Forc,variable.name==variablelist[i])
}

GPP_C <- as.data.frame(mylist[1])
NPP_1_C <- as.data.frame(mylist[2])
ANPP_woody_stem_C <- as.data.frame(mylist[3])
ANPP_foliage_C <- as.data.frame(mylist[4])
ANPP_woody_C <- as.data.frame(mylist[5])
ANPP_2_C <- as.data.frame(mylist[6])
BNPP_root_C <- as.data.frame(mylist[7])
BNPP_root_coarse_C <- as.data.frame(mylist[8])
BNPP_root_fine_C <- as.data.frame(mylist[9])

#firstly, find new GPP sites.
sites_NPP <- NPP_Sara_Malhi_Keith
sites_NPP$repeated <- 1
sites_NPP2 <- aggregate(repeated~site,data=sites_NPP,mean)
dim(sites_NPP2)

GPP_C <- as.data.frame(mylist[1])
GPP_C <- GPP_C[,c("sites.sitename","mean","date","start.date","end.date")]
names(GPP_C) <-c("site","GPP","date","start.date","end.date")

GNPP_final2 <- merge(GPP_C,sites_NPP2,by=c("site"),all.x=TRUE) # now, merge to get new GPP sites 
new_GPP <- subset(GNPP_final2,is.na(repeated)==TRUE)
new_GPP$year_start <- NA
new_GPP$year_end <- NA
for (i in 1:nrow(new_GPP)){
  if (is.na(new_GPP$start.date[i]) == FALSE){
    new_GPP$year_start[i] <- new_GPP$start.date[i]
    new_GPP$year_end[i] <- new_GPP$end.date[i]} else {
      new_GPP$year_start[i] <- new_GPP$date[i]
      new_GPP$year_end[i] <- new_GPP$date[i]}}

new_GPP$year_start <- round(as.numeric(new_GPP$year_start))  
new_GPP$year_end <- round(as.numeric(new_GPP$year_end))  
new_GPP$year_start[is.na(new_GPP$year_start)==TRUE] <- 1991
new_GPP$year_end[is.na(new_GPP$year_end)==TRUE] <- 2010
new_GPP <- new_GPP[,c("site","GPP","year_start","year_end")]
new_GPP$GPP<-100*new_GPP$GPP
hist(new_GPP$GPP)



#get a npp dataset - which is independent to GPP, because we don't need GPP paralled to NPP sites here! We only used this for data validation
#write a function to convert to correct measurement yaer - and aggrgated based on mean~sites.sitename+year_start+year_end. So that it can be merged later on
object_correct_years <- function(object_npp){
  object_npp$year_start <- NA
  object_npp$year_end <- NA
  for (i in 1:nrow(object_npp)){
    if (is.na(object_npp$start.date[i]) == FALSE){
      object_npp$year_start[i] <- object_npp$start.date[i]
      object_npp$year_end[i] <- object_npp$end.date[i]} else {
        object_npp$year_start[i] <- object_npp$date[i]
        object_npp$year_end[i] <- object_npp$date[i]}}
  
  object_npp$year_start <- round(as.numeric(object_npp$year_start))  
  object_npp$year_end <- round(as.numeric(object_npp$year_end))  
  object_npp$year_start[is.na(object_npp$year_start)==TRUE] <- 1991
  object_npp$year_end[is.na(object_npp$year_end)==TRUE] <- 2010
  object_npp <- object_npp[,c("sites.sitename","year_start","year_end","mean")]
  object_npp$mean <- object_npp$mean * 100 #convert to consistent unit gC/m2/yr
  object_npp2 <- aggregate(mean~sites.sitename+year_start+year_end,data=object_npp,mean,na.rm=TRUE)
  return(object_npp2)
}

NPP_1_C <- object_correct_years(NPP_1_C)
ANPP_woody_stem_C <- object_correct_years(ANPP_woody_stem_C)
ANPP_foliage_C<- object_correct_years(ANPP_foliage_C)
ANPP_woody_C<- object_correct_years(ANPP_woody_C)
ANPP_2_C<- object_correct_years(ANPP_2_C)
BNPP_root_C<- object_correct_years(BNPP_root_C)
BNPP_root_coarse_C<- object_correct_years(BNPP_root_coarse_C)
BNPP_root_fine_C<- object_correct_years(BNPP_root_fine_C)

names(NPP_1_C) <- c("sites.sitename","year_start","year_end","TNPP_1")
names(ANPP_woody_stem_C) <- c("sites.sitename","year_start","year_end","NPP.stem")
names(ANPP_foliage_C) <- c("sites.sitename","year_start","year_end","NPP.foliage")
names(ANPP_woody_C) <- c("sites.sitename","year_start","year_end","NPP.wood")
names(ANPP_2_C) <- c("sites.sitename","year_start","year_end","ANPP_2")
names(BNPP_root_C) <- c("sites.sitename","year_start","year_end","BNPP_1")
names(BNPP_root_coarse_C) <- c("sites.sitename","year_start","year_end","NPP.coarse")
names(BNPP_root_fine_C) <- c("sites.sitename","year_start","year_end","NPP.fine")


NPP_all <-Reduce(function(x,y) merge(x = x, y = y, by = c("sites.sitename","year_start","year_end"),all.x=TRUE),
                 list(NPP_1_C,ANPP_woody_stem_C,ANPP_foliage_C,ANPP_woody_C,ANPP_2_C,BNPP_root_C,BNPP_root_coarse_C,BNPP_root_fine_C))
names(sites_NPP2) <- c("sites.sitename","repeated")
NPP_final2 <- merge(NPP_all,sites_NPP2,by=c("sites.sitename"),all.x=TRUE) # now, merge to get new GPP sites 
new_NPP <- subset(NPP_final2,is.na(repeated)==TRUE)
dim(new_NPP)

#now, new_NPP and new_GPP both ready - let's merged a few gpp to npp dataset, then rest of gpp just alone!
head(new_NPP)
head(new_GPP)
names(new_GPP) <- c("sites.sitename","GPP","year_start","year_end")
ForC_all <- dplyr::bind_rows(new_NPP, new_GPP) 
ForC_all <- ForC_all[,!(names(ForC_all) %in% "repeated")]

#finally, combine with coordinates
forc_coord <- read.csv("/Users/yunpeng/data/NPP_Yunke/NPP_ForC/orig/ForC_sites.csv")
forc_coord <- forc_coord[,c("sites.sitename","lat","lon","masl")]
ForC_all_coord <- merge(ForC_all,forc_coord,by=c("sites.sitename"),all.x=TRUE) # now, merge to get new GPP sites 
summary(ForC_all_coord)
# a few sites have missed coordinates - needs combined manually, based on ForC_sites.csv above
ForC_all_coord$lat[ForC_all_coord$sites.sitename=="Cascade Head 1 "] <- 45.1024
ForC_all_coord$lon[ForC_all_coord$sites.sitename=="Cascade Head 1 "]<- -123.8816
ForC_all_coord$masl[ForC_all_coord$sites.sitename=="Cascade Head 1 "]<- 205

ForC_all_coord$lat[ForC_all_coord$sites.sitename=="Deer Canyon Preserve Pinyon Juniper Woodland "]<- 34.36
ForC_all_coord$lon[ForC_all_coord$sites.sitename=="Deer Canyon Preserve Pinyon Juniper Woodland "]<--106.27
ForC_all_coord$masl[ForC_all_coord$sites.sitename=="Deer Canyon Preserve Pinyon Juniper Woodland "]<-2126

ForC_all_coord$lat[ForC_all_coord$sites.sitename=="O site "] <- 44.5
ForC_all_coord$lon[ForC_all_coord$sites.sitename=="O site "] <- -121.617
ForC_all_coord$masl[ForC_all_coord$sites.sitename=="O site "] <- 915

ForC_all_coord$lat[ForC_all_coord$sites.sitename=="Tablelands Juniper Savanna "]<-34.43
ForC_all_coord$lon[ForC_all_coord$sites.sitename=="Tablelands Juniper Savanna "]<- -105.86
ForC_all_coord$masl[ForC_all_coord$sites.sitename=="Tablelands Juniper Savanna "] <- 1926

ForC_all_coord$lat[ForC_all_coord$sites.sitename=="University of Michigan Biological Station (UMBS) "]<- 45.583
ForC_all_coord$lon[ForC_all_coord$sites.sitename=="University of Michigan Biological Station (UMBS) "]<- -84.7
ForC_all_coord$masl[ForC_all_coord$sites.sitename=="University of Michigan Biological Station (UMBS) "]<- NA

ForC_all_coord$lat[ForC_all_coord$sites.sitename=="Valles Caldera Mixed Conifer "] <- 35.89
ForC_all_coord$lon[ForC_all_coord$sites.sitename=="Valles Caldera Mixed Conifer "] <- -106.53
ForC_all_coord$masl[ForC_all_coord$sites.sitename=="Valles Caldera Mixed Conifer "] <- 3049

ForC_all_coord$lat[ForC_all_coord$sites.sitename=="Valles Caldera Ponderosa Pine "] <- 35.86
ForC_all_coord$lon[ForC_all_coord$sites.sitename=="Valles Caldera Ponderosa Pine "] <- -106.6
ForC_all_coord$masl[ForC_all_coord$sites.sitename=="Valles Caldera Ponderosa Pine "] <- 2486

ForC_all_coord$lat[ForC_all_coord$sites.sitename=="Willow Creek (WC)-Chequamegon National Forest "]<- 45.783
ForC_all_coord$lon[ForC_all_coord$sites.sitename=="Willow Creek (WC)-Chequamegon National Forest "]<- -90.083
ForC_all_coord$masl[ForC_all_coord$sites.sitename=="Willow Creek (WC)-Chequamegon National Forest "]<- 480

ForC_all_coord$masl <- as.numeric(ForC_all_coord$masl)
ForC_all_coord$masl

ForC_elv_missing <- subset(ForC_all_coord,is.na(masl)==TRUE)
ForC_elv_missing_Plot <- aggregate(ForC_elv_missing,by=list(ForC_elv_missing$sites.sitename), FUN=mean, na.rm=TRUE)
ForC_elv_missing_Plot <-ForC_elv_missing_Plot[,c("Group.1","lon","lat")]
names(ForC_elv_missing_Plot) <- c("sitename","lon","lat")
head(ForC_elv_missing_Plot)

df_etopo <- ingest(
  ForC_elv_missing_Plot,
  source = "etopo1",
  dir = "~/data/etopo/" 
)
ForC_elv_missing_Plot$Elevation <- as.numeric(as.data.frame(df_etopo$data))
names(ForC_elv_missing_Plot) <- c("sites.sitename","lon","lat","Elevation_etopo")

#now, interpolate those NA elevation by etopo
ForC_all_coord2 <- merge(ForC_all_coord,ForC_elv_missing_Plot,by=c("sites.sitename","lon","lat"),all.x=TRUE)
ForC_all_coord2$Elevation <- ForC_all_coord2$masl
ForC_all_coord2$Elevation[is.na(ForC_all_coord2$masl)==TRUE] <- ForC_all_coord2$Elevation_etopo[is.na(ForC_all_coord2$masl)==TRUE]

ForC_all_coord2 <- ForC_all_coord2[,!(names(ForC_all_coord2) %in% c("masl","Elevation_etopo"))]
names(ForC_all_coord2) <- c("site","lon","lat","Begin_year","End_year","TNPP_1","NPP.stem","NPP.foliage","NPP.wood","ANPP_2","BNPP_1","NPP.coarse","NPP.fine","GPP","z")
ForC_all_coord2$pft <- "Forest"
ForC_all_coord2$file <- "ForC"

NPP_Sara_Malhi_Keith_Forc <- dplyr::bind_rows(NPP_Sara_Malhi_Keith, ForC_all_coord2) 

summary(NPP_Sara_Malhi_Keith_Forc)

#extract Tg, PPFD and vpd from those samples
NPP_leaf_model <- subset(NPP_Sara_Malhi_Keith_Forc,ANPP_2>NPP.foliage)[,c("lon","lat","z","site","ANPP_2","NPP.foliage","file")]
dim(NPP_leaf_model)

#now, add Tg, PPFD and vpd
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
all_predictors <- cbind(elev,Tg$myvar,PPFD$myvar,vpd$myvar)

names(all_predictors) <- c("lon","lat","z","Tg","PPFD","vpd")

Tg_df <- all_predictors[,c("lon","lat","z","Tg")]
PPFD_df <- all_predictors[,c("lon","lat","z","PPFD")]
vpd_df <- all_predictors[,c("lon","lat","z","vpd")]

#now, apply gwr to extract site predictors' value
NPP_Forest <- NPP_leaf_model
NPP_Forest$Tg <- NA
NPP_Forest$PPFD <- NA
NPP_Forest$vpd <- NA
library(spgwr)
a <- 1.5 # which degree (distance) of grid when interpolating gwr from global grids
#Extract Tg, PPFD, vpd
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
  }, error=function(e){})} 
summary(NPP_Forest)

#using ForC dataset only!!!
summary(lmer(log((NPP.foliage/ANPP_2)/(1-(NPP.foliage/ANPP_2))) ~ log(PPFD) + Tg + log(vpd) + (1|site), data = subset(NPP_Forest,file!="Malhi 2011" & file!="Malhi 2016")))
aaa<-subset(NPP_Forest,file!="Malhi 2011" & file!="Malhi 2016")

#Now, add data from Tian Di (pft = grassland for all data)
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

# XXX Beni:
Tiandi_npp %>% 
  ggplot(aes(x = CN_leaf, y = ..count..)) +
  geom_histogram()

#XXX: These values are much lower than the ones in the Terra-P dataset. We discussed that this may be a real difference. To be sure, could you please check with Di Tian whether the data here is also in units of gC / gN?
#YYY: Yes, exactly. But I have already checked with Tian Di that the data is gC/gN. Not sure why it is smaller, will investigate their reasons further. 

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

#XXX Source information is missing for sites from Tian Di’s and Matteo’s datasets (1055 rows).
#YYY: Tiandi's dataset citation now in "~/data/npp_stoichiometry_grasslands_tiandi/README" AND "~/data/npp_stoichiometry_forests_tiandi/README.md"
#YYY: Matteo’s dataset's citation now in "~/data/campioli/README.md"
#YYY: NOT sure what is source information for each sites from Tian Di and Matteo's dataset --> I was just using their original data. I think it is safe to just use their referred citation suggested in the whole dataset.

#Finally, add more forest sites from corrected Sara Vicca’s dataset, including anpp, npp.leaf and npp.wood (but no measured environmental covariates available).

#XXX Please provide citation for this added dataset. How were duplicates with previously included data avoided? XXX File name seems odd.

#YYY: the citation and introduction of this added dataset is available in "~/data/NPP_Yunke/NPP_SaraVicca/orig/validation_data/README.md".
# For this additional validation data provided by Sara Vicca, it included more sites in anpp, leaf npp, wood npp and leaf C/N. She suggested that we can cite them as: Berner&Law 2016 https://www.nature.com/articles/sdata20162

#finally, add more forest sites from corrected Sara Vicca's dataset, including anpp, npp.leaf and npp.wood. (leafcn data will be included below)
Sara2_df <- read.csv(file="~/data/NPP_Yunke/NPP_SaraVicca/orig/validation_data/CORRECTIONS_CascadeHead_Andrews.csv")
Sara2_df2 <- subset(Sara2_df,Repeat=="no") #remove repeated data as inputted in NPP_SaraVicca
Sara2_NPP <- Sara2_df2[,c("LONGITUDE","LATITUDE","ELEVATION","YEAR","YEAR","AG_PROD_TREE_TOTAL_AS_CARBON","AG_PROD_TREE_FOLIAGE_AS_CARBON","AG_PROD_TREE_WOOD_AS_CARBON")]
names(Sara2_NPP) <- c("lon","lat","z","Begin_year","End_year","ANPP_2","NPP.foliage","NPP.wood")
Sara2_NPP$ANPP_2[Sara2_NPP$ANPP_2<=0] <- NA
Sara2_NPP$NPP.foliage[Sara2_NPP$NPP.foliage<=0] <- NA
Sara2_NPP$NPP.wood[Sara2_NPP$NPP.wood <=0] <- NA
Sara2_NPP$Source <- "Sara Vicca Validation data"
Sara2_NPP$file <- "~/data/NPP_Yunke/NPP_SaraVicca/orig/validation_data"
Sara2_NPP$pft<-"Forest"
Sara2_NPP$pft2<-"Forest"
summary(Sara2_NPP)

#create site name
Sara2_NPP_sitename <- aggregate(Sara2_NPP,by=list(Sara2_NPP$lon,Sara2_NPP$lat,Sara2_NPP$z), mean,na.rm=TRUE)
for (i in 1:nrow(Sara2_NPP_sitename)){
  Sara2_NPP_sitename$site[i] <- paste("Sara2_NPP",i,sep = "")
}

Sara2_NPP_sitename <- Sara2_NPP_sitename[,c("lon","lat","z","site")]

Sara2_NPP$no <- c(1:nrow(Sara2_NPP))

Sara2_NPP2 <- merge(Sara2_NPP,Sara2_NPP_sitename,by=c("lon","lat","z"),all.x=TRUE)

Sara2_NPP2 <- Sara2_NPP2[order(Sara2_NPP2$no), ]

Sara2_NPP2 <- Sara2_NPP2[,c(1:12,14)] #remove no

NPP_final2 <- dplyr::bind_rows(NPP_final, Sara2_NPP2) 
summary(NPP_final2)

###Now, merge leaf c/n from various sources

# (1) Add Schulz - unit: all in g/m2

#XXX is this m2 ground area? Please specify units in the README.
#YYY Done

#XXX Distinction between stem and branches appears quite important (very different C:N ratios!). Is this disinction made in other datasets? What does ‘wood’ represent in the other datasets?
#Yes, agree that stem and branch is quite important here with different C/N so I have included them both. For wood or stem C/N ratio, this is the ONLY dataset we hold.
CN_Schulz <- read.csv(file="~/data/NPP_Yunke/npp_cn/CN_Schulze.csv")
CN_Schulz2 <- CN_Schulz[,c(5,48:58)]

CN_Schulz2$CN_leaf_Schulz <- CN_Schulz2$c_leaf/CN_Schulz2$n_leaf
CN_Schulz2$CN_root_Schulz <- (CN_Schulz2$c_coarseroot+CN_Schulz2$c_fineroot)/CN_Schulz2$n_root
#???(CN_Schulz2$c_fineroot + CN_Schulz2$c_coarseroot) / CN_Schulz2$n_root

CN_Schulz2$CN_stem_Schulz <- CN_Schulz2$c_stem/CN_Schulz2$n_stem
CN_Schulz2$CN_wood_Schulz <- (CN_Schulz2$c_stem+CN_Schulz2$c_branch)/(CN_Schulz2$n_stem+CN_Schulz2$n_branch)

CN_Schulz2 <- CN_Schulz2[,c("site","CN_leaf_Schulz","CN_root_Schulz","CN_stem_Schulz","CN_wood_Schulz")]
CN_Schulz2

# (2) Add Malhi data - assume cmass as constant 0.48 g/g; narea in gm-2, lma in gm-2
CN_Malhi <- read.csv(file="~/data/NPP_Yunke/npp_cn/CN_Malhi.csv")

#No original data of cmass but we can assume cmass = 48%, because (1) it is consistent with what we find in mean values of ~/data/CN_leaf/final_leafCN.csv, equals to 47% and (2) see Enquist et al. 2017 https://onlinelibrary.wiley.com/doi/full/10.1111/geb.12645 - fig.2, overall the cmass was within a very small variance through this elevation transect, and we can just assume this value as 0.48!

#XXX information on this dataset lacking in README (~/data/NPP_Yunke/npp_cn/README.md). Original citation?
#YYY: Done

#XXX Why not use the value of cmass provided in that same dataset (0.4638)?
#YYY: Done. See below.  

CN_Malhi$CN_leaf_alt_malhi <- 0.4638/(CN_Malhi$narea/CN_Malhi$lma) # use CN_leaf_alt as a new and alternative variable, by storing data when Cmass or (c%) is lacking.

CN_Malhi <- na.omit(CN_Malhi)

newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-180, 180), ylim = c(-75, 75), asp = 1)
points(CN_Malhi$lon,CN_Malhi$lat, col="red", pch=16,cex=1)

CN_Malhi2 <- CN_Malhi[,c("site","CN_leaf_alt_malhi")]
CN_Malhi2

#(3) Add Species-based traits data, as provided from Sara Vicca, including around 40 forest sites that have species-based leaf c/n
#The data is c% and n%

#XXX add variable descriptions for file NACP_TERRA_PNW_leaf_trait.csv in the README, sitting in the same directory.
#YYY: Done

# XXX: Could you describe in words what is done by the chunk below and why?

# YYY: The step is (see comment below):
#(1) aggregate leaf C/N based on lon + lat, according to a leaf traits dataset provided by Sara Vicca. There are more than one 1 record in a site, beacuse it measured many trees (individuals) in one site. See their original csv.
CN_SaraVicca <- read.csv(file="~/data/NPP_Yunke/NPP_SaraVicca/orig/validation_data/NACP_TERRA_PNW_leaf_trait.csv")
CN_SaraVicca <- CN_SaraVicca[,c("LONGITUDE","LATITUDE","LEAF_CN")]
CN_SaraVicca$LEAF_CN[CN_SaraVicca$LEAF_CN<0] <- NA
CN_SaraVicca$LONGITUDE <- as.numeric(CN_SaraVicca$LONGITUDE)
CN_SaraVicca$LATITUDE <- as.numeric(CN_SaraVicca$LATITUDE)

CN_SaraVicca2 <- aggregate(CN_SaraVicca, by=list(CN_SaraVicca$LONGITUDE,CN_SaraVicca$LATITUDE), mean,na.rm=TRUE)
CN_SaraVicca2 <- CN_SaraVicca2[,c("LONGITUDE","LATITUDE","LEAF_CN")]
names(CN_SaraVicca2) <- c("lon","lat","CN_leaf_sara")
dim(CN_SaraVicca2)

#(2) Merge C/N to NPP dataset STRICTLY based on lon and lat
test <- merge(NPP_final2,CN_SaraVicca2,by=c("lon","lat"),all.x=TRUE)
dim(subset(test,CN_leaf_sara>0))
test1 <- subset(test,CN_leaf_sara>0)
test1 <- test1[,c("lon","lat","z","CN_leaf_sara")]
dim(test1)
test1 <- aggregate(test1, by=list(test1$lon,test1$lat,test1$z), mean,na.rm=TRUE)
dim(test1)
test1<- test1[,c("lon","lat","z","CN_leaf_sara")]


#(3) Merge C/N to NPP dataset based on lon and lat, but not strictly --> we assume that it can be best merged within 0.01 lon/lat resolution
# I would persist using this way because it could help to generate more leaf NPP * C/N sites, it would help us generate more validation sites
NPP_old <- NPP_final[1:1739,]
NPP_old$lon <- round(NPP_old$lon,2)
NPP_old$lat <- round(NPP_old$lat,2)

CN_SaraVicca3 <- CN_SaraVicca
names(CN_SaraVicca3) <- c("lon","lat","CN_SaraVicca_old")

CN_SaraVicca3$lon <- round(CN_SaraVicca3$lon,2)
CN_SaraVicca3$lat <- round(CN_SaraVicca3$lat,2)

CN_SaraVicca4 <- aggregate(CN_SaraVicca3, by=list(CN_SaraVicca3$lon,CN_SaraVicca3$lat), mean,na.rm=TRUE)
CN_SaraVicca4 <- CN_SaraVicca4[,c(3,4,5)]

test_new <- merge(NPP_old,CN_SaraVicca4,by=c("lon","lat"),all.x=TRUE) # merging with "old" Sara Vicca's dataset, as far lon and lat both agrees within 0.01 degree.
nrow(test_new)-nrow(NPP_old)

test2 <- subset(test_new,CN_SaraVicca_old>0)
test2 <- test2[,c("site","CN_SaraVicca_old")]
dim(test2)
test2 <- aggregate(test2, by=list(test2$site), mean,na.rm=TRUE)
dim(test2)
test2 <- test2[,c(1,3)]
names(test2) <- c("site","CN_SaraVicca_old")

#(5) Lastly, add Forc leaf, stem and root data (on site basis) - this should be after (3) and (4).
#But no data matched to current site.
#Forc <- read.csv(file="~/data/NPP_Yunke/NPP_ForC/orig/ForC_measurements_final.csv")
#Forc2 <- aggregate(mean~sites.sitename+Begin_year1+End_year1+variable.name,data=Forc,mean)
#variablelist <- read.csv(file="~/data/NPP_Yunke/NPP_ForC/orig/site_need.csv")
#variable <- as.character(variablelist$variable.name)
#mylist <- vector(mode = "list", length = 41)
#for (i in 1:41){mylist[[i]] <- subset(Forc2,variable.name==variable[i])}
##leaf_pC <- as.data.frame(mylist[21])
#leaf_C2N <- as.data.frame(mylist[22])
##leaf_pN <- as.data.frame(mylist[23])
##foliage_pN <- as.data.frame(mylist[24]) 
##leaf_pP <- as.data.frame(mylist[25]) 
##stem_pC <- as.data.frame(mylist[26]) 
##stem_pN <- as.data.frame(mylist[27])
##root_pN <- as.data.frame(mylist[28]) 
##root_pC <- as.data.frame(mylist[29]) 
#CN_Forc <- aggregate(leaf_C2N, by=list(leaf_C2N$sites.sitename), mean,na.rm=TRUE)
#CN_Forc2 <- CN_Forc[,c(1,6)]
#names(CN_Forc2) <- c("site","CN_leaf_Forc")
#CN_Forc2

#now, merge with NPP_final2 - 4 objects need to be merged, at this stage. 
NPP_final2_site <- NPP_final2[,c("lon","lat","z","site","CN_leaf","CN_root","pft2")]

NPP_final2_site$no <- 1:nrow(NPP_final2_site)

npp_cn1 <-Reduce(function(x,y) merge(x = x, y = y, by = c("site"),all.x=TRUE),
                 list(NPP_final2_site,CN_Schulz2,CN_Malhi2,test2))
nrow(npp_cn1) - nrow(NPP_final2_site)

npp_cn2 <-Reduce(function(x,y) merge(x = x, y = y, by = c("lon","lat","z"),all.x=TRUE),
                 list(npp_cn1,test1))

npp_cn2 <- npp_cn2[order(npp_cn2$no), ]
#one column have a issue when both test and test2 have data, remove test2
subset(npp_cn2,CN_leaf_sara>0 & CN_SaraVicca_old>0)$no
npp_cn2$CN_SaraVicca_old[npp_cn2$no==subset(npp_cn2,CN_leaf_sara>0 & CN_SaraVicca_old>0)$no] <- NA

summary(npp_cn2$lon - NPP_final2$lon)
summary(npp_cn2)
npp_cn2$CN_stem_final <- npp_cn2$CN_stem_Schulz
npp_cn2$CN_wood_final <- npp_cn2$CN_wood_Schulz

#now, it is the time to combine
npp_cn3 <- npp_cn2 %>% mutate(CN_leaf_final = coalesce(CN_leaf,CN_leaf_Schulz,CN_leaf_alt_malhi,CN_SaraVicca_old,CN_leaf_sara)) %>% # including additional data by secondry C/N merging by Sara
  mutate(CN_leaf_near_final = coalesce(CN_leaf,CN_leaf_Schulz,CN_leaf_alt_malhi,CN_SaraVicca_old,CN_leaf_sara)) %>% # ignore this object
  mutate(CN_leaf_org = coalesce(CN_leaf,CN_leaf_Schulz,CN_leaf_alt_malhi,CN_leaf_sara)) %>% #NOT including additional data by secondry C/N merging by Sara
  mutate(CN_root_final = coalesce(CN_root,CN_root_Schulz))

summary(npp_cn3$CN_leaf_org)
summary(npp_cn3$CN_leaf_final) # it would help us generate 37 more sites

npp_cn4 <- npp_cn3[,c("CN_stem_final","CN_wood_final","CN_leaf_final","CN_leaf_near_final","CN_leaf_org","CN_root_final")]
hist(npp_cn4$CN_leaf_final)
hist(npp_cn4$CN_stem_final)
hist(npp_cn4$CN_wood_final)
hist(npp_cn4$CN_root_final)

NPP_final3 <- cbind(NPP_final2,npp_cn4)

NPP_final3$lnf_obs_final <-NPP_final3$NPP.foliage/NPP_final3$CN_leaf_final
NPP_final3$lnf_obs_nearfinal <-NPP_final3$NPP.foliage/NPP_final3$CN_leaf_near_final
NPP_final3$lnf_obs_org <-NPP_final3$NPP.foliage/NPP_final3$CN_leaf_org
NPP_final3$bnf_obs_final  <- NPP_final3$BNPP_1/NPP_final3$CN_root_final
NPP_final3$wnf_obs_final  <- NPP_final3$NPP.wood/NPP_final3$CN_wood_final
