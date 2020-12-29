
##################forest###
######forest - first round
library(dplyr)
library(ingestr)
library(rbeni)
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



#Lastly, create an unique site name, which will be used in ingestr function later on.

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

NPP_final2 <- dplyr::bind_rows(NPP_forest_all, NPP_forest2) 

NPP_final2$no <- c(1:nrow(NPP_final2))

#output data forcing information file (forest1,2,3...) to a dataframe and save
csvfile <- paste("/Users/yunpeng/data/forest_npp/forest_forcing_info.csv")
write.csv(NPP_final2, csvfile, row.names = TRUE)









#now, input forcing data from two times simulation

forcing_df <- list.files("/Users/yunpeng/data/forest_npp/forcing/",full.names = T)
length(forcing_df) # all data was included

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

#I have checked the literatures, and data is all originally correct. It might be because the literatures have confused lon/lat in their data from (Malhi et al. 2011).
#However, there is no way to just change their lon and lat. Therefore, we just disregard this points, and I confirmed that we have also already disregarded those points in statistical model of anpp.leaf/anpp and in output check.
library(rworldmap)
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-180, 180), ylim = c(-75, 75), asp = 1)
points(na_fapar$lon,na_fapar$lat, col="red", pch=16,cex=1)
points(na_fapar$lon[c(1,2,3,5,8,9,10)],na_fapar$lat[c(1,2,3,5,8,9,10)], col="blue", pch=16,cex=1)
#12 samples were missing fapar, which is under expected.

#2. forcing - input - all 842 points were available
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
NPP_final2$pred_gpp_c4 <- NA

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
    
    #c4
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
summary(siteinfo_final$max_vcmax25) #12 points were missing, as expected

analyse_modobs2(siteinfo_final,"pred_gpp_c3", "GPP",type = "points")
#analyse_modobs2(siteinfo_final, "GPP","pred_gpp_c3",type = "points")

ggplot(data=siteinfo_final, aes(x=pred_gpp_c3, y=GPP)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+xlim(0,4000)+ylim(0,4000)

#site-mean
siteinfo_final2 <- aggregate(siteinfo_final,by=list(siteinfo_final$lon,siteinfo_final$lat,siteinfo_final$z), FUN=mean, na.rm=TRUE) #site-mean

for (i in 1:nrow(siteinfo_final2)){
  siteinfo_final2$sitename3[i] <- paste("forest_long",i,sep = "")
}

siteinfo_final3 <- siteinfo_final2[,c("lon","lat","z","sitename3")]

test <-Reduce(function(x,y) merge(x = x, y = y, by = c("lon","lat","z"),all.x=TRUE), 
                        list(siteinfo_final,siteinfo_final3))
#####siteinfo_final3 was used in latest forcing extraction (for 30 yrs that get better max vcmax25, will come back later on!!!)

analyse_modobs2(siteinfo_final2, "pred_gpp_c3","GPP",type = "points")


#Great results! Let's input leaf c/n firstly! This csv file was outputted from /Users/yunpeng/yunkepeng/nimpl_sofun_inputs/output_check/Nitrogen_uptake_check.Rmd around in L282
npp_cn <- read.csv("/Users/yunpeng/data/forest_npp/forest_leafcn.csv")
forest_cn <- subset(npp_cn,pft2=="Forest")
dim(forest_cn)
summary(forest_cn$lon - siteinfo_final$lon)
summary(forest_cn$lat - siteinfo_final$lat)
summary(forest_cn$z - siteinfo_final$z)
summary(forest_cn$TNPP_1 - siteinfo_final$TNPP_1)
summary(forest_cn$ANPP_2 - siteinfo_final$ANPP_2)
summary(forest_cn$GPP - siteinfo_final$GPP)
summary(forest_cn$NPP.foliage - siteinfo_final$NPP.foliage)
#####above is to double check the data is all right and can be cbinded.

#now, cbind them and calculate final data
siteinfo_final$CN_leaf <- forest_cn$CN_leaf_final
#siteinfo_final$CN_leaf <- forest_cn$CN_leaf_near_final
siteinfo_final$CN_root <- forest_cn$CN_root_final
siteinfo_final$CN_wood <- forest_cn$CN_wood_final
siteinfo_final$CN_stem <- forest_cn$CN_stem_final

siteinfo_final$lnf_obs <- siteinfo_final$NPP.foliage/siteinfo_final$CN_leaf
siteinfo_final$bnf_obs <- siteinfo_final$BNPP_1/siteinfo_final$CN_root
siteinfo_final$wnf_obs <- siteinfo_final$NPP.wood/siteinfo_final$CN_wood

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
location <- "~/data/output/new/"
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

vcmax25_df <- inputnc("annualvcmax25",1982,2011)
summary(vcmax25_df)

leafcn_df <- inputnc("leafcn",1982,2011)
summary(leafcn_df)


#cbind all predictors, and its lon, lat, z
all_predictors <- cbind(elev,Tg$myvar,PPFD$myvar,vpd$myvar,
                        alpha$myvar,fAPAR$myvar,age$myvar,
                        CNrt$myvar,LMA$myvar,vcmax25_df$annualvcmax25,leafcn_df$leafcn)

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

#extract lma by direct method
LMA_global <- na.omit(LMA_df)
coordinates(LMA_global) <- ~lon+lat 
gridded(LMA_global) <- TRUE
LMA_global <- raster(LMA_global, "LMA") 

siteinfo_final2 <- aggregate(siteinfo_final,by=list(siteinfo_final$lon,siteinfo_final$lat,siteinfo_final$z), FUN=mean, na.rm=TRUE) 
sp_sites <- SpatialPoints(siteinfo_final2[,c("lon","lat","z")]) # only select lon and lat

extract_LMA <- raster::extract(LMA_global, sp_sites, sp = TRUE) %>% as_tibble() %>% 
  right_join(siteinfo_final, by = c("lon", "lat","z")) %>% 
  dplyr::rename( extract_LMA = LMA)
summary(extract_LMA$extract_LMA)
extract_LMA <- extract_LMA[order(extract_LMA$no), ]

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

#Extract Tg, PPFD, vpd, alpha,fAPAR,age,CNrt,LMA, max-vcmax25
library(spgwr)
library(raster)


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

forest_site$pred_npp <- forest_site$pred_gpp * (1/(1 + exp(-(-0.3677 * log(forest_site$CNrt) +
                                                               -0.1552 * log(forest_site$age) + 
                                                               0.5791 * forest_site$fAPAR+
                                                               1.9144 *forest_site$alpha + -1.1052))))

forest_site$pred_anpp <- forest_site$pred_gpp * (1/(1 + exp(-(-0.6075 * log(forest_site$CNrt) +
                                                              -0.1798 * log(forest_site$age) + 
                                                               0.8617 * forest_site$fAPAR+
                                                               2.1287 *forest_site$alpha + -1.3528))))

forest_site$pred_bnpp <- forest_site$pred_npp - forest_site$pred_anpp

forest_site$pred_lnpp <- forest_site$pred_anpp * (1/(1 + exp(-(1.2350* log(forest_site$PPFD) +
                                                                 0.0731 * (forest_site$Tg) + 
                                                                 -1.0192 * log(forest_site$vpd) + -9.2375))))

forest_site$pred_wnpp <- forest_site$pred_anpp - forest_site$pred_lnpp


#extract leafcn from simulation
forest_site$simul_leafnc <- NA
a <- 1.5
for (i in 1:nrow(forest_site)) {
  tryCatch({
    #leafcn
    leafcn_global <- na.omit(leafcn_df)
    NRE_part <- subset(leafcn_global,lon>(forest_site[i,1]-a)&lon<(forest_site[i,1]+a)&
                         lat>(forest_site[i,2]-a)&lat<(forest_site[i,2]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- forest_site[i,1:3]
    coordinates(NRE_coord) <- c("lon","lat")
    forest_site[i,c("simul_leafnc")]  <- (gwr(leafcn ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
  }, error=function(e){})} 

#extract max vcmax25 from 30 years simulation

inputnc_all <- function(name,start_year,end_year){
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
  output_selected_yrs <- output_allyears # only calculated means based on selected start and end year (see function)
  coord <- nc_to_df(nc, varnam = name)[,1:2] # obtain lon and lat
  final_output <- cbind(coord,elev[,3],output_selected_yrs) # combine lon, lat,z with rowmeans variable
  names(final_output) <- c("lon","lat","z",name)
  return(final_output)
  #-----------------------------------------------------------------------
  # Output: output_final: the output data (259200 * 3) including lon, lat and value
  #-----------------------------------------------------------------------
}
vcmax25_all_df <- inputnc_all("annualvcmax25",1982,2011)
vcmax25_all <- vcmax25_all_df[,c(4:33)]
colMax <- apply(vcmax25_all, 1, function(x) max(x))

final_output <- cbind(elev,colMax) # combine lon, lat,z with rowmeans variable
names(final_output) <- c("lon","lat","z","max_vcmax25_30yrs")
summary(final_output)

forest_site$max_vcmax25_30yrs <- NA
a <- 1.5
for (i in 1:nrow(forest_site)) {
  tryCatch({
    maxvc25_global <- na.omit(final_output)
    NRE_part <- subset(maxvc25_global,lon>(forest_site[i,1]-a)&lon<(forest_site[i,1]+a)&
                         lat>(forest_site[i,2]-a)&lat<(forest_site[i,2]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- forest_site[i,1:3]
    coordinates(NRE_coord) <- c("lon","lat")
    forest_site[i,c("max_vcmax25_30yrs")]  <- (gwr(max_vcmax25_30yrs ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
  }, error=function(e){})} 

#max??? needs to interpolate site max vcmax25 from rsofun???

#use 30yrs map average's site -->changing cmass to 0.6
#forest_site$pred_leafnc <- (0.0144/0.5) + (0.0050/0.5) * forest_site$Vcmax25/forest_site$LMA

#use rsofun - site-mean
#forest_site$pred_leafnc <- (0.0144/0.5) + (0.0050/0.5) * siteinfo_final$max_vcmax25/forest_site$LMA

#use rsofun - site-species
forest_site$pred_leafnc <- (0.0161/0.5) + (0.0041/0.5) * siteinfo_final$max_vcmax25/forest_site$LMA

#use max vcmax25 of 30yrs simulation map - even worse
#forest_site$pred_leafnc <- (0.0144/0.5) + (0.0050/0.5) * forest_site$max_vcmax25_30yrs/forest_site$LMA


#model 2: Nmass ~ Tg + PPFD + LMA - not a big difference
#forest_site$pred_leafnc = (0.02035+ (-0.0001095)*forest_site$Tg + (0.00001875)*(forest_site$PPFD) +(-0.00006863)*(forest_site$LMA))/0.5  #it is actually leaf n/c here...the alternative model for PPFD + Tg + LMA     

#model 3: nmass/cmass (constant=0.5) ~ vcmax25 + lma
#forest_site$pred_leafnc = 4.808e-02 + 5.760e-05*siteinfo_final$max_vcmax25 + -1.276e-04*forest_site$LMA

#use rsofun + extracted LMA
#forest_site$pred_leafnc <- (0.0144/0.8) + (0.0050/0.8) * siteinfo_final$max_vcmax25/extract_LMA$extract_LMA

#cmass = 0.87, for nmass in site-mean of vcmax25 project (N=5000) * a constant c/n ratio 50
#cmass = 0.5, as directly extracted from Sara Vicca's dataset (file="~/data/NPP_Yunke/NPP_SaraVicca/orig/validation_data/NACP_TERRA_PNW_leaf_trait.csv")
#cmass = 0.47, as directly extracted from /Users/yunpeng/data/CN_leaf/final_leafCN.csv. For all site-species with leaf c/n


forest_site$pred_lnf <- forest_site$pred_lnpp*forest_site$pred_leafnc

#using simulated leafnc
#forest_site$pred_lnf <- forest_site$pred_lnpp*forest_site$simul_leafnc #no improvement for siulated leafnc

forest_site$pred_wnf <- forest_site$pred_wnpp/97
forest_site$pred_bnf <- forest_site$pred_bnpp/42


#combine 

forest_site$TNPP_1 <- siteinfo_final$TNPP_1
forest_site$ANPP_2 <- siteinfo_final$ANPP_2
forest_site$BNPP_1 <- siteinfo_final$BNPP_1
forest_site$NPP.foliage <- siteinfo_final$NPP.foliage
forest_site$NPP.wood <- siteinfo_final$NPP.wood
forest_site$CN_leaf <- siteinfo_final$CN_leaf
forest_site$lnf_obs <- siteinfo_final$lnf_obs
forest_site$wnf_obs <- siteinfo_final$wnf_obs
forest_site$bnf_obs <- siteinfo_final$bnf_obs
forest_site$GPP <- siteinfo_final$GPP

#include repeated data information now
rep_info <- read.csv("/Users/yunpeng/data/NPP_Yunke/NPP_final_rep.csv")
rep_info2 <- subset(rep_info,pft2=="Forest")
summary(rep_info2$lat-forest_site$lat)
summary(rep_info2$lon-forest_site$lon)
forest_site$rep_info <- rep_info2$rep_info
forest_site2 <- (subset(forest_site,rep_info!="rep"&rep_info!="rep1"))
analyse_modobs2(forest_site2,"pred_lnf", "lnf_obs",type = "points") #don't worry! Because we has not used max vcmax25 here!!!! Will be changed in rsofun later


forest_site3 <- aggregate(forest_site2,by=list(forest_site2$lon,forest_site2$lat), FUN=mean, na.rm=TRUE) #site-mean
analyse_modobs2(forest_site3,"pred_gpp", "GPP",type = "points")

analyse_modobs2(forest_site2,"pred_gpp", "GPP",type = "points")
ggplot(data=forest_site, aes(x=pred_gpp, y=GPP)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()

analyse_modobs2(forest_site2,"pred_npp", "TNPP_1",type = "points")
ggplot(data=forest_site, aes(x=pred_npp, y=TNPP_1)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()

analyse_modobs2(forest_site2,"pred_anpp", "ANPP_2",type = "points")
ggplot(data=forest_site, aes(x=pred_anpp, y=ANPP_2)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()

analyse_modobs2(forest_site2,"pred_lnpp", "NPP.foliage",type = "points")
ggplot(data=forest_site, aes(x=pred_lnpp, y=NPP.foliage)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()

analyse_modobs2(forest_site,"pred_wnpp", "NPP.wood",type = "points")
ggplot(data=forest_site, aes(x=pred_wnpp, y=NPP.wood)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()

analyse_modobs2(forest_site2,"pred_bnpp", "BNPP_1",type = "points")
ggplot(data=forest_site, aes(x=pred_bnpp, y=BNPP_1)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()

analyse_modobs2(forest_site,"pred_lnf", "lnf_obs",type = "points") #don't worry! Because we has not used max vcmax25 here!!!! Will be changed in rsofun later
ggplot(data=forest_site, aes(x=pred_lnf, y=lnf_obs)) +
  geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method = "lm", se = TRUE)+
  xlab("Prediction")+ylab("Observation")+theme_classic()

forest_site$pred_lnf_siml <- NPP_Forest$pred_lnf
analyse_modobs2(forest_site,"pred_lnf", "lnf_obs",type = "points") #don't worry! Because we has not used max vcmax25 here!!!! Will be changed in rsofun later

aaa <- forest_site[,c("pred_lnf","lnf_obs","pred_lnf_siml")]

forest_site2 <- aggregate(forest_site,by=list(forest_site$lon,forest_site$lat), FUN=mean, na.rm=TRUE) #site-mean
analyse_modobs2(forest_site2,"pred_lnf", "lnf_obs",type = "points") #don't worry! Because we has not used max vcmax25 here!!!! Will be changed in rsofun later
