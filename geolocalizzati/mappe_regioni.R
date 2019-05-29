if (!require(pacman)) install.packages("pacman")
pacman::p_load("tidyverse")
pacman::p_load("sf")

regione_geo <- sf::read_sf(fs::path("shapefiles", "Limiti01012019_g", "Reg01012019_g"))

scrutini_regione <- readr::read_csv(file = "scrutini/scrutini_regione_full.csv")

scrutini_regione_geo <- scrutini_regione %>% 
  transmute(desc_lis, voti, perc = as.numeric(str_replace(perc, ",", ".")), desc_reg = stringr::str_squish(desc_reg)) %>% 
  left_join(y = regione_geo %>% 
              mutate(desc_reg = stringr::str_squish(stringr::str_to_upper(DEN_REG))) %>% 
              mutate(desc_reg = stringr::str_replace(desc_reg, "FRIULI VENEZIA", "FRIULI-VENEZIA")) %>% 
              select(desc_reg, COD_REG), by = "desc_reg") %>% 
  sf::st_sf()

saveRDS(object = scrutini_regione_geo, file = file.path("geolocalizzati", "scrutini_regione_geo.rds"))

plot(st_geometry(scrutini_regione_geo))

