options(HTTPUserAgent=readLines(con = "useragent.txt", warn = FALSE))

if (!require(pacman)) install.packages("pacman")
pacman::p_load("tidyverse")
pacman::p_load("jsonlite")


read_json(path = paste0("https://eleapi.interno.gov.it/siel/PX/scrutiniEE/TE/01/NA/223"), simplifyVector = TRUE)

nazioni <- readr::read_csv(fs::path("elenchi", "circoscrizioni_estero_nazioni.csv"))

estrai_scrutini_estero_per_nazione <- function(cod_circ, full_metadata = FALSE) {
  pb <- progress_estimated(length(cod_circ))
  
  if (full_metadata==TRUE) {
    purrr::map_dfr(.x = cod_circ,
                   .f = function(x) {
                     pb$tick()$print()
                     scrutini_temp_base <- jsonlite::read_json(path = paste0("https://eleapi.interno.gov.it/siel/PX/scrutiniEE/TE/01/NA/", x),
                                                               simplifyVector = TRUE)
                     
                     info_temp <- tibble(descrizione = names(scrutini_temp_base$int),
                                         dato = as.character(scrutini_temp_base$int)) %>% 
                       spread(key = descrizione, value = dato)
                     
                     cbind(scrutini_temp_base$liste, info_temp)
                     
                   } )
  } else {
    purrr::map_dfr(.x = cod_circ,
                   .f = function(x) {
                     pb$tick()$print()
                     scrutini_temp_base <- jsonlite::read_json(path = paste0("https://eleapi.interno.gov.it/siel/PX/scrutiniEE/TE/01/NA/", x),
                                                               simplifyVector = TRUE)
                     
                     info_temp <- tibble(descrizione = names(scrutini_temp_base$int),
                                         dato = as.character(scrutini_temp_base$int)) %>% 
                       spread(key = descrizione, value = dato)
                     
                     scrutini_temp_base$liste %>% 
                       mutate(cod_circ = info_temp$cod_circ, desc_circ = info_temp$desc_circ)
                     
                   }, .id = "cod_circ")
  }
}

scrutini_estero_per_nazione <- estrai_scrutini_estero_per_nazione(cod_circ = nazioni$cod_circoscrizione, full_metadata = TRUE)

write_csv(x = scrutini_estero_per_nazione, path = fs::path("scrutini", "scrutini_estero_per_nazione.csv"))