
library(maptools)
library(tidyverse)
library(actel)
library(RSP)
library(sf)
library(move)
library(cmocean)
library(ggspatial)
library(gganimate)
library(ggmap)
library(ggsn)
library(rosm)
library(patchwork)
library(magick)
library(wk)
library(doParallel)

load("../TAMATAROA analysis/Data/rsp.run.RData")

##Attention a bien changer le scr origine (ESPG 3857) ver cible (ESPG 4326) sur Qgis avant de la charger
img_satellite <- terra::rast("raster_rangiroa_WGS84.tif")  # Remplace par le chemin vers ton fichier raster (image satellite)

# water <- shapeToRaster(shape ="../TAMATAROA analysis/Data/land_rangi_tikehau.shp",
#                        size = 0.001,# Pixel size for the rendered raster in meters 
#                        buffer = 1,
#                        spatial = "spatial.csv",
#                        coord.x = "Longitude",
#                        coord.y = "Latitude",
#                        type = "land")

#plotTracks(input = rsp.run, base.raster = water, land.col = "#CDAA7D", size = 1 )+
#  addStations(rsp.run)


#Arguments de la fonction animate custom
input = rsp.run
base.raster = img_satellite
tags = "A69-9001-48923"
drop.groups = NULL
by.group = FALSE
start.time = "2024-03-31 00:00:00"
stop.time = "2024-10-20 00:00:00"
land.col = "#BABCBF80"
add.legend = FALSE
add.stations = TRUE
save.gif = TRUE
gif.name = "Animation.gif"
height = 1800
width = 3750
xlim = NULL
ylim = NULL
nframes = 500
fps = 2

animateTracks_2 <- function(input, base.raster, tags = NULL, drop.groups = NULL, 
                            by.group = FALSE, start.time, stop.time, land.col = "#BABCBF80", 
                            add.legend = TRUE, add.stations = FALSE, save.gif = FALSE, 
                            gif.name = "Animation.gif", height = 720, width = 720, xlim = NULL, 
                            ylim = NULL, nframes = 100, fps = 10) {
  
  message("Preparing RSP data for the animation...")
  
  # Préparation des données des détections
  detections <- do.call(rbind.data.frame, input$detections)
  detections$Group <- input$bio$Group[match(detections$Transmitter, input$bio$Transmitter)]
  detections$Signal_Track <- paste(detections$Signal, detections$Track, sep = "_")
  
  # Filtrage des groupes
  if (!is.null(drop.groups)) {
    if (length(unique((drop.groups %in% unique(detections$Group)))) > 1) {
      stop("One or more 'drop.groups' selected could not be found in the RSP input.\n", call. = FALSE)
    } else {
      detections <- detections[-which(detections$Group %in% drop.groups), ]
    }
  }
  
  # Filtrage par tags
  if (!is.null(tags)) {
    if (length(unique((tags %in% unique(detections$Transmitter)))) > 1) {
      stop("One or more tags selected could not be found in the RSP input.\n", call. = FALSE)
    } else {
      detections <- detections[which(detections$Transmitter %in% tags), ]
    }
  }
  
  # Filtrage par date (si start.time et stop.time sont spécifiés)
  if (!missing(start.time) && !grepl("^[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-5][0-9]:[0-5][0-9]", start.time)) 
    stop("'start.time' must be in 'yyyy-mm-dd hh:mm:ss' format.\n", call. = FALSE)
  
  if (!missing(stop.time) && !grepl("^[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-5][0-9]:[0-5][0-9]", stop.time)) 
    stop("'stop.time' must be in 'yyyy-mm-dd hh:mm:ss' format.\n", call. = FALSE)
  
  # Filtrage des données de détection par date
  if (!missing(start.time) & missing(stop.time)) 
    message("M: Discarding detection data previous to ", start.time, " per user command.")
  
  if (missing(start.time) & !missing(stop.time)) 
    message("M: Discarding detection data posterior to ", stop.time, " per user command.")
  
  if (!missing(start.time) & !missing(stop.time)) {
    if (stop.time < start.time) 
      stop("'stop.time' must be after 'start.time'.", call. = FALSE)
    
    message(paste0("M: Discarding detection data previous to ", start.time, " and posterior to ", stop.time, " per user command."))
    detections <- detections[which(detections$Timestamp >= as.POSIXct(start.time, format = "%Y-%m-%d %H:%M:%S", 
                                                                      tz = attr(detections$Timestamp, "tzone")) & 
                                     detections$Timestamp <= as.POSIXct(stop.time, format = "%Y-%m-%d %H:%M:%S", 
                                                                        tz = attr(detections$Timestamp, "tzone"))), ]
  }
  
  # Filtrage des trajectoires invalides (ceux avec une seule latitude unique)
  tracks <- unique(detections$Signal_Track)
  track.save <- NULL
  for (i in 1:length(tracks)) {
    aux <- detections[which(detections$Signal_Track == tracks[i]), ]
    if (length(unique(aux$Latitude)) == 1) 
      track.save <- c(track.save, tracks[i])
  }
  
  if (length(track.save) > 0) 
    detections <- detections[-which(detections$Signal_Track %in% track.save), ]
  
  # Transformation du raster en dataframe avec couleurs en RGB
  df <- terra::as.data.frame(base.raster, xy = TRUE)
  
  # Normaliser les valeurs entre 0 et 1 si c'est un raster RGB
  if (ncol(df) > 3) {  # Supposons que les bandes sont dans les colonnes 3, 4 et 5
    df <- df %>%
      mutate(
        R = raster_rangiroa_WGS84_1 / max(raster_rangiroa_WGS84_1, na.rm = TRUE),
        G = raster_rangiroa_WGS84_2 / max(raster_rangiroa_WGS84_2, na.rm = TRUE),
        B = raster_rangiroa_WGS84_3 / max(raster_rangiroa_WGS84_3, na.rm = TRUE)
      )
  }
  
  df <- df[,c("x","y","R","G","B")]
  
  colnames(df) <- c("Longitude", "Latitude","R","G","B")
  
  ### PLOT DE BASE POUR TOUS LES FRAMES
  
  # Initialisation de ggplot
  p <- ggplot2::ggplot()
  
  # Ajout du fond raster (carte satellite)
  p <- p + ggplot2::geom_raster(data = df, ggplot2::aes(x = Longitude, y = Latitude, fill = rgb(R, G, B)), 
                                show.legend = FALSE, alpha = 1) + 
    scale_fill_identity()
  
  if (add.stations) 
    p <- p + addStations(input,size = 5)  # Assure-toi que addStations() est défini ailleurs
  
  p <- p + ggplot2::theme_bw()
  
  # Légende et axes
  p <- p + ggplot2::theme(legend.position = "none")
  
  # Ajout des tailles pour les axes
  p <- p + ggplot2::theme(
    axis.text = ggplot2::element_text(size = 40),      # Taille des graduations
    axis.title = ggplot2::element_text(size = 50)     # Taille des titres des axes
  )
  
  # Contrôle des limites des axes
  p <- p + ggplot2::scale_x_continuous(expand = c(0, 0)) + 
    ggplot2::scale_y_continuous(expand = c(0, 0))
  
  # Gestion des limites (si spécifiées)
  if (!is.null(xlim) & !is.null(ylim)) {
    p <- p + ggplot2::coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE)
  }
  
  ## Initialisation de la barre de progression
  pb <- progress_bar$new(
    format = "Creating frames [:bar] :percent Elapsed: :elapsed ETA: :eta",
    total = length(indices), clear = FALSE, width = 60
  )
 
  # Création d'un dossier temporaire pour exporter les frames
  dir.create("temp_plot", showWarnings = FALSE)
  
  # Indices du timestamp
  indices <- seq(from = 5, to = nrow(detections), by = (nrow(detections) - 1)/(nframes - 1))
  indices <- round(indices)
  
  ## PRODUCTION DES FRAMES UN PAR UN DANS LE DOSSIER TEMPORAIRE
  
  # Tentative de parallelisation
  
  cl <- makeCluster(detectCores()-2)
  registerDoParallel(cl)
  
  clusterExport(cl, list("rsp.run","img_satellite"), envir = environment())
  
  #Load the library "rgeos" to each cluster
  clusterEvalQ(cl, library(ggplot2))
  clusterEvalQ(cl, library(RSP))
  clusterEvalQ(cl, library(sf))
  
  clusterApply(cl, p, x = indices, fun = function(x){

    # Position courante
    q <- p + ggplot2::geom_point(data = detections[indices[i],], ggplot2::aes(x = Longitude, y = Latitude, colour = Transmitter, 
                                                                     group = Signal_Track), size = 12)
    
    # Positions précédentes pour faire la queue
    if (i > 1)
      q <- q + ggplot2::geom_point(data = detections[indices[i-1],], ggplot2::aes(x = Longitude, y = Latitude, colour = Transmitter, 
                                                                     group = Signal_Track), size = 10)
    
    if (i > 2)
    q <- q + ggplot2::geom_point(data = detections[indices[i-2],], ggplot2::aes(x = Longitude, y = Latitude, colour = Transmitter, 
                                                                     group = Signal_Track), size = 8)
    
    if (i > 3)
    q <- q + ggplot2::geom_point(data = detections[indices[i-3],], ggplot2::aes(x = Longitude, y = Latitude, colour = Transmitter, 
                                                                     group = Signal_Track), size = 6)
    
    if (i > 4)
    q <- q + ggplot2::geom_point(data = detections[indices[i-4],], ggplot2::aes(x = Longitude, y = Latitude, colour = Transmitter, 
                                                                     group = Signal_Track), size = 4)
    
    if (i > 5)
    q <- q + ggplot2::geom_point(data = detections[indices[i-5],], ggplot2::aes(x = Longitude, y = Latitude, colour = Transmitter, 
                                                                     group = Signal_Track), size = 2)
    
    # Convertir le format du timestamp pour l'affichage
    formatted_timestamp <- format(as.POSIXct(detections$Timestamp[indices[i]], format = "%Y-%m-%d %H:%M:%OS", 
                                             tz = attr(detections$Timestamp, "tzone")), 
                                  "%d %B %Y")
    
    # Ajouter le titre avec le timestamp formaté
    q <- q + ggplot2::labs(title = formatted_timestamp) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 50, face = "bold"))  # Taille et style du titre
    
    # # Titre du graphique
    # q <- q + ggplot2::labs(title = detections$Timestamp[indices[i]]) +
    #                          ggplot2::theme(plot.title = ggplot2::element_text(size = 50, face = "bold"))  # Taille et style du titre
    
    jpeg(filename = paste0("temp_plot/frame_", i,".jpg"), width = width, height = height)
    
    print(q)
    
    dev.off()
    
  })#eo for i in indices
  

  ## PRODUCTION DU GIF
  
  ## list file names and read in
  imgs <- list.files(path = "temp_plot/", pattern = "*.jpg", full.names = TRUE)
  img_list <- lapply(imgs, magick::image_read)
  
  ## join the images together
  img_joined <- magick::image_join(img_list)
  
  ## animate at 2 frames per second
  img_animated <- magick::image_animate(img_joined, fps = fps, optimize = FALSE)

  ## save to disk
  magick::image_write(image = img_animated,
                      path = gif.name)
  
  
  # On supprime le dossier temporaire contenant les frames
  fs::dir_delete("temp_plot/")
  
}






animateTracks_2(input = rsp.run,
                base.raster = img_satellite,
                tags = "A69-9001-48923",
                drop.groups = NULL,
                by.group = FALSE,
                start.time = "2024-03-31 00:00:00",
                stop.time = "2024-10-20 00:00:00",
                land.col = "#BABCBF80",
                add.legend = TRUE,
                add.stations = TRUE,
                save.gif = TRUE,
                gif.name = "Animation.gif",
                height = 1200,
                width = 2500,
                xlim = NULL,
                ylim = NULL,
                nframes = 500,
                fps = 2)

