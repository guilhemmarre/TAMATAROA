
###########################################
### CREATE GEOREFERENCED MAPS WITH DATE ###
###########################################

# List all orthos
orthos <- list.files(path = "Y:/Guilhem", pattern = "*.tif", full.names = TRUE)

ext <- raster::extent(raster::raster(orthos[1]))

for (ortho in orthos){
  
  cat(ortho, "\n")
  
  # Load
  r <- terra::rast(ortho)
  
  # Extract date
  date <- strsplit(ortho, "Larvotto_")[[1]][2]
  date <- gsub(".tif", "", date)
  date <- gsub("_", "-", date)
  
  # Start export
  jpeg(filename = gsub(".tif",".jpg", ortho), width = 10000, height = 5000)
  
  # Plot ortho
  terra::plotRGB(r, xlim = c(ext@xmin, ext@xmax), ylim = c(43.7436, 43.74375), maxcell = round((dim(r)[1] * dim(r)[2] / 10)))
  
  # Add date label
  graphics::text(x = 7.43395, y = 43.7435, labels = date, cex = 25) 
  
  dev.off()

}#eo for ortho



###########################################
### CONVERT IMAGES INTO AN ANIMATED GIF ###
###########################################

# References
# https://www.nagraj.net/notes/gifs-in-r/
# https://cran.r-project.org/web/packages/magick/vignettes/intro.html
# https://stackoverflow.com/questions/65571964/create-fade-in-and-fade-out-gif-in-r-magick-package
# https://ezgif.com/maker

## list file names and read in
imgs <- list.files(path = "Y:/Guilhem", pattern = "*.jpg", full.names = TRUE)
img_list <- lapply(imgs, magick::image_read)

## join the images together
img_joined <- magick::image_join(img_list)

## animate at 2 frames per second
img_animated <- magick::image_animate(img_joined, fps = 0.5, optimize = FALSE)

## view animated image
#img_animated

## save to disk
magick::image_write(image = img_animated,
            path = "Y:/Guilhem/Larvotto2.gif")

# Test with kind of dissolve between frames
# library(dplyr)
# library(magick)
# img_list <- sapply(imgs, magick::image_read)
# my_gif <-  image_resize(c(img_list[[1]], img_list[[2]]), '1000x500!') %>%
#   image_background('white') %>%
#   image_morph(1) %>% # Creates X transition images between the real images
#   image_animate(fps=2, optimize = TRUE) %>%
#   image_browse(my_gif)
# 
# magick::image_write(image = img_animated,
#                     path = "Y:/Guilhem/Larvotto_dissolve.gif")


