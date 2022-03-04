library(rworldmap)
library(colorRamps)

gpp_all <- gpp_site
analyse_modobs2(subset(gpp_all,pred>0),"GPP","pred", type = "points")

bias_gpp <- subset(gpp_all,pred>0)
bias_gpp$bias <- 100*(bias_gpp$pred - bias_gpp$GPP)/(bias_gpp$GPP)
bias_gpp <- bias_gpp[,c("lon","lat","bias")]

bias_npp <- subset(npp_site,pred>0)
bias_npp$bias <- 100*(bias_npp$pred - bias_npp$TNPP_1)/(bias_npp$TNPP_1)
bias_npp <- bias_npp[,c("lon","lat","bias")]


bias_anpp <- subset(anpp_site,pred>0)
bias_anpp$bias <- 100*(bias_anpp$pred - bias_anpp$ANPP_2)/(bias_anpp$ANPP_2)
bias_anpp <- bias_anpp[,c("lon","lat","bias")]


#set color gradient
#rr <- range(bias_gpp$bias)
#svals <- (bias_gpp$bias-rr[1])/diff(rr)
#f <- colorRamp(c("white", "red"))
#bias_gpp$colors <- rgb(f(svals)/255)
#newmap <- getMap(resolution = "low")
#plot(newmap, xlim = c(-180, 180), ylim = c(-75, 75), asp = 1)
#points(bias_gpp$lon, bias_gpp$lat, col = bias_gpp$colors,pch=16,cex=1.5)

analyse_modobs2(subset(gpp_all,pred>0),"GPP","pred", type = "points")
hist(bias_gpp$bias)
plot(newmap, xlim = c(-180, 180), ylim = c(-75, 75), asp = 1)
negative <- subset(bias_gpp,bias<0)
points(negative$lon, negative$lat, col = "blue",pch=16,cex=1.5)
medium <- subset(bias_gpp,bias<=100&bias>=0)
points(medium$lon, medium$lat, col = "black",pch=16,cex=1.5)
highly_positive <- subset(bias_gpp,bias>100)
points(highly_positive$lon, highly_positive$lat, col = "red",pch=16,cex=1.5)
title("Blue (bias < 0), Red (Bias > 100), Black (bias 0-100)")

summary(bias_gpp)


#combine NPP(GPP dataset) with pft, method of GPP
NPP_SaraVicca <- read.csv(file="~/data/NPP/NPP_SaraVicca/NPP_SaraVicca.csv")
NPP_Malhi <- read.csv(file="~/data/NPP/NPP_Malhi/NPP_Malhi.csv")
NPP_Keith <- read.csv(file="~/data/NPP/NPP_Keith/NPP_Keith.csv")
NPP_Forc <- read.csv(file="~/data/NPP/NPP_Forc/NPP_Forc.csv")
NPP_all <- rbind(NPP_SaraVicca,NPP_Malhi,NPP_Keith,NPP_Forc)

#add pft data
Evergreen <- read.csv(file="~/data/NPP/NPP_SaraVicca/orig/pft.csv")
#Evergreen <- read.csv(file="D:/PhD/nimpl_sofun_inputs/Data/NPP/NPP_SaraVicca/orig/pft.csv")
pft_type <- merge(NPP_all,Evergreen,by=c("site"),all.x=TRUE)

#add method of gpp
method_no <- read.csv(file="~/data/NPP/NPP_SaraVicca/orig/methodology_number.csv")
head(method_no)
method_site <- read.csv(file="~/data/NPP/NPP_SaraVicca/orig/methodology_site.csv")

npp_method1 <- merge(pft_type,method_site[,1:2],by=c("site"),all.x=TRUE)
npp_method2 <- merge(npp_method1,method_no,by=c("methodology"),all.x=TRUE)
summary(npp_method2)

#check gpp's method
gpp_method <- subset(npp_method2,GPP.x>0)

gpp_method$no <- 1
gpp_method$Eddy.covariance
gpp_method %>% 
  group_by(GPP.y) %>% 
  summarise(sum(no))
gpp_method %>% 
  group_by(Eddy.covariance) %>% 
  summarise(sum(no))


#merge bias of gpp to npp dataset
npp_method_npp <- merge(npp_method2,bias_gpp,by=c("lon","lat"),all.x=TRUE)
summary(npp_method_npp)
largebias <- subset(npp_method_npp,bias>100)
largebias$Source.y
test <- largebias[,45:60]


#now, for npp

npp_all <- npp_site
analyse_modobs2(subset(npp_all,pred>0),"TNPP_1","pred", type = "points")

bias_npp <- subset(npp_all,pred>0)
bias_npp$bias <- 100*(bias_npp$pred - bias_npp$TNPP_1)/(bias_npp$TNPP_1)
bias_gpp <- bias_gpp[,c("lon","lat","bias")]


hist(bias_npp$bias)
plot(newmap, xlim = c(-180, 180), ylim = c(-75, 75), asp = 1)
negative <- subset(bias_npp,bias<0)
points(negative$lon, negative$lat, col = "blue",pch=16,cex=1.5)
medium <- subset(bias_npp,bias<=100&bias>=0)
points(medium$lon, medium$lat, col = "black",pch=16,cex=1.5)
highly_positive <- subset(bias_npp,bias>100)
points(highly_positive$lon, highly_positive$lat, col = "red",pch=16,cex=1.5)
title("Blue (bias < 0), Red (Bias > 100), Black (bias 0-100)")


