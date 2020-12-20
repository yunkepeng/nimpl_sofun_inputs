#working on tian di's database
#c3 c4 information classification
tiandi_df_sp <- read.csv("/Users/yunpeng/data/npp_stoichiometry_grasslands_tiandi/China_grassland_CN_stoichiometry_with_matched_NPP_species_legume_20201214.csv")

list_df <- vector(mode = "list", length = nrow(tiandi_df_sp))

for (i in (1:nrow(tiandi_df_sp))){
  list_df[[i]] <- strsplit(tiandi_df_sp$Species_CN[i], "_", fixed = FALSE, perl = FALSE, useBytes = FALSE)
  
}
dim(tiandi_df_sp)

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
dim(t5)

final_species <- aggregate(no~value,FUN=mean,na.rm=TRUE,data=t5)
head(final_species)

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


#now, input c3/c4 information 
c3c4 <- read.csv("/Users/yunpeng/data/c3c4_species/Try20201218143430TRY_Categorical_Traits_Lookup_Table_2012_03_17_TestRelease/TRY_Categorical_Traits_Lookup_Table_2012_03_17_TestRelease.csv")
data1 <- c3c4[,c(2,4,5,18)]
head(data1)
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

#csvfile <- paste("/Users/yunpeng/species2.csv")
#write.csv(final_species2, csvfile, row.names = TRUE)
