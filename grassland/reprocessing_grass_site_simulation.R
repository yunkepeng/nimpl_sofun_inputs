#re-processing fapar; input sites firstly
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

#1. input fapar


#2. Input climate forcing
forcing_df <- list.files("/Users/yunpeng/data/grassland_npp/reprocessing_climate/",full.names = T)
length(forcing_df)

#forcing - input
for (i in 1:length(forcing_df)){
  tryCatch({
    df1 <- read.csv(forcing_df[i])
    df1$date <- as.Date(df1$date)
    fpar <- as.data.frame(eval(parse(text=df1$sitename[1])))[,3]
    df2 <- cbind(df1,fapar)
    df3 <- df2[,c("date","temp","prec","rain","snow","vpd","ppfd","patm","ccov_int","ccov","fapar","co2","fapar")]
    assign(paste("final",df1$sitename[1],sep="_"), as_tibble(df3))
  }, error=function(e){})} 

