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

csvfile <- paste("/Users/yunpeng/data/npp_stoichiometry_grasslands_tiandi/species_name.csv")
write.csv(final_species, csvfile, row.names = TRUE)
