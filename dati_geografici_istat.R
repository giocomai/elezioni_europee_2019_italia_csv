pacman::p_load("tidyverse")
pacman::p_load("sf")

dir.create("shapefiles", showWarnings = FALSE)

#https://www.istat.it/it/archivio/222527

limiti_shp_zip <- file.path("shapefiles", "Limiti01012019_g.zip")

if (file.exists(limiti_shp_zip)==FALSE) {
  download.file(url = "http://www.istat.it/storage/cartografia/confini_amministrativi/generalizzati/Limiti01012019_g.zip", 
                destfile = limiti_shp_zip)
  unzip(zipfile = limiti_shp_zip)
  file.rename(from = "Limiti01012019_g", to = "shapefiles")
  fs::dir_copy(path = "Limiti01012019_g", new_path = fs::path("shapefiles", "Limiti01012019_g"))
  fs::dir_delete("Limiti01012019_g")
}


         