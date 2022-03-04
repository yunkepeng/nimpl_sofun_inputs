climate_df <- list.files("~/data/forest_npp/reprocessing_climates/",full.names = T)

empty_vec <- c()

#check existed climate files
for (i in 1:(length(climate_df))){
  empty_vec[i] <- as.numeric(gsub("[^0-9]", "",  climate_df[i]))
}

diff <- setdiff(1:935, empty_vec)
length(diff)
