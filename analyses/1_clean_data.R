
# Chargement des données brutes
obs <- data.table::fread("data/observations.csv", sep = ";", dec = ".", header = TRUE)
obs <- obs[, c("ID", "Date", "Location")]

# On reconstruit le tableau avec une ligne par obs
obs_clean <- NA
for (i in 1:nrow(obs)){
  
  dates_requin <- strsplit(obs$Date[i], ";")[[1]]
  
  obs_requin <- data.frame("ID" = obs$ID[i],
                           "Location" = obs$Location[i],
                           "Date" = dates_requin)
  
  obs_clean <- rbind(obs_clean,
                     obs_requin)
  
}

obs_clean <- obs_clean[-1,]

# On met au propre les années
date_clean <- sapply(1:nrow(obs_clean), function(i){
  
  date <- obs_clean$Date[i]
  
  annee <- strsplit(date, "/")[[1]][3]
  
  if (nchar(annee) == 2){
    
    date <- substr(x = date, start = 1, stop = 6)
    
    date <- paste0(date, "20", annee)
    
  }
  
  return(date)
  
})

obs_clean$Date <- date_clean

# On rajoute une colonne fiabilité
obs_clean$IC <- NA

# Export du tableau
write.table(obs_clean, "outputs/observations.csv", sep = ";", dec = ".", row.names = FALSE)
