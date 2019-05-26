options(HTTPUserAgent=readLines(con = "useragent.txt", warn = FALSE))

if (!require(pacman)) install.packages("pacman")
pacman::p_load("tidyverse")
pacman::p_load("jsonlite")
dir.create(path = "scrutini", showWarnings = FALSE)

if (!require(pacman)) install.packages("pacman")
pacman::p_load("tidyverse")
pacman::p_load("jsonlite")

dir.create(path = "scrutini", showWarnings = FALSE)

##### circoscrizione ##### 

estrai_scrutini_circoscrizioni <- function(cod_circ, full_metadata = FALSE) {
  pb <- progress_estimated(length(cod_circ))
  
  if (full_metadata==TRUE) {
    purrr::map_dfr(.x = cod_circ,
                   .f = function(x) {
                     pb$tick()$print()
                     scrutini_temp_base <- jsonlite::read_json(path = paste0("https://eleapi.interno.gov.it/siel/PX/scrutiniEI/TE/01/CR/", x),
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
                     scrutini_temp_base <- jsonlite::read_json(path = paste0("https://eleapi.interno.gov.it/siel/PX/scrutiniEI/TE/01/CR/", x),
                                                               simplifyVector = TRUE)
                     
                     info_temp <- tibble(descrizione = names(scrutini_temp_base$int),
                                         dato = as.character(scrutini_temp_base$int)) %>% 
                       spread(key = descrizione, value = dato)
                     
                     scrutini_temp_base$liste %>% 
                       mutate(cod_circ = info_temp$cod_circ, desc_circ = info_temp$desc_circ)
                     
                   } )
  }
}

### esempi

# estrai_scrutini_circoscrizioni(cod_circ = 2)
# estrai_scrutini_circoscrizioni(cod_circ = 3, full_metadata = TRUE)
scrutini_circoscrizioni_full <- estrai_scrutini_circoscrizioni(cod_circ = circoscrizioni$cod_circoscrizione, full_metadata = TRUE)



write_csv(x = scrutini_circoscrizioni_full, path = file.path("scrutini", "scrutini_circoscrizioni_full.csv"))
##### regione ##### 

cod_regione <- regioni$cod_regione

estrai_scrutini_regione <- function(cod_regione, full_metadata = FALSE) {
  cod_regione <- stringr::str_pad(string = cod_regione, width = 2, side = "left", pad = "0")
  pb <- progress_estimated(length(cod_regione))
  
  if (full_metadata==TRUE) {
    purrr::map_dfr(.x = cod_regione,
                   .f = function(x) {
                     pb$tick()$print()
                     scrutini_temp_base <- jsonlite::read_json(path = paste0("https://eleapi.interno.gov.it/siel/PX/scrutiniEI/TE/01/RE/", x),
                                                               simplifyVector = TRUE)
                     
                     info_temp <- tibble(descrizione = names(scrutini_temp_base$int),
                                         dato = as.character(scrutini_temp_base$int)) %>% 
                       spread(key = descrizione, value = dato)
                     
                     cbind(scrutini_temp_base$liste, info_temp)
                     
                   } )
  } else {
    purrr::map_dfr(.x = cod_regione,
                   .f = function(x) {
                     pb$tick()$print()
                     scrutini_temp_base <- jsonlite::read_json(path = paste0("https://eleapi.interno.gov.it/siel/PX/scrutiniEI/TE/01/RE/", x),
                                                               simplifyVector = TRUE)
                     
                     info_temp <- tibble(descrizione = names(scrutini_temp_base$int),
                                         dato = as.character(scrutini_temp_base$int)) %>% 
                       spread(key = descrizione, value = dato)
                     
                     scrutini_temp_base$liste %>% 
                       mutate(cod_reg = info_temp$cod_reg, desc_reg = info_temp$desc_reg)
                     
                   } )
  }
}

#esempio

estrai_scrutini_regione(10)

scrutini_regione <- estrai_scrutini_regione(cod_regione = regioni$cod_regione)

write_csv(x = scrutini_regione, path = file.path("scrutini", "scrutini_regione.csv"))

scrutini_regione_full <- estrai_scrutini_regione(cod_regione = regioni$cod_regione, full_metadata = TRUE)

write_csv(x = scrutini_regione_full, path = file.path("scrutini", "scrutini_regione_full.csv"))

##### provincia #####

# https://eleapi.interno.gov.it/siel/PX/scrutiniEI/TE/01/PR/081 

##### comune #####

# https://eleapi.interno.gov.it/siel/PX/scrutiniEI/TE/01/PR/081/CM/2620

cod_combo <- comuni %>% 
  transmute(cod_comune = as.character(stringr::str_pad(string = cod_comune, width = 4, side = "left", pad = "0")),
            cod_provincia = as.character(stringr::str_pad(string = cod_provincia, width = 3, side = "left", pad = "0")),
            cod_regione = as.character(stringr::str_pad(string = cod_regione, width = 2, side = "left", pad = "0")),
            cod_circoscrizione = as.character(cod_circoscrizione)) %>% 
  as_tibble()



estrai_scrutini_comune <- function(cod_combo, full_metadata = FALSE) {
  
  pb <- progress_estimated(nrow(cod_combo))
  dir.create("scrutini_comuni_json", showWarnings = FALSE)
  all_urls <- paste0("https://eleapi.interno.gov.it/siel/PX/scrutiniEI/TE/01/PR/", cod_combo$cod_provincia, "/CM/", cod_combo$cod_comune)
  dest_file_comuni <- paste0("scrutini_comuni_json/", cod_combo$cod_provincia, "_CM_", cod_combo$cod_comune, ".json")
  
  purrr::walk2(.x = all_urls, .y = dest_file_comuni, .f = ~ download.file(url = .x, destfile = .y))
  purrr::walk(.x = dest_file_comuni, .f = ~ writeLines(iconv(readLines(.), from = "ISO-8859-1", to = "UTF8"), .))
  
  
  if (full_metadata==TRUE) {
    purrr::map_dfr(.x = dest_file_comuni,
                   .f = function(x) {
                     pb$tick()$print()
                     
                     scrutini_temp_base <- jsonlite::read_json(path = x,
                                                               simplifyVector = TRUE)
                     
                     
                     
                     info_temp <- tibble(descrizione = names(scrutini_temp_base$int),
                                         dato = as.character(scrutini_temp_base$int)) %>% 
                       spread(key = descrizione, value = dato)
                     
                     cbind(scrutini_temp_base$liste, info_temp)
                   } )
  } else {
    purrr::map_dfr(.x = dest_file_comuni,
                   .f = function(x) {
                     pb$tick()$print()
                     scrutini_temp_base <- jsonlite::read_json(path = x,
                                                               simplifyVector = TRUE)
                     
                     info_temp <- tibble(descrizione = names(scrutini_temp_base$int),
                                         dato = as.character(scrutini_temp_base$int)) %>% 
                       spread(key = descrizione, value = dato)
                     
                     scrutini_temp_base$liste %>% 
                       mutate(circ_sto = info_temp$circ_sto,
                              cod_prov = info_temp$cod_prov,
                              desc_prov = info_temp$desc_prov,
                              cod_com = info_temp$cod_com,
                              desc_com = info_temp$desc_com
                       )
                     
                   } )
  }
}

prova <- estrai_scrutini_comune(cod_combo = head(cod_combo, 20))
prova_full <- estrai_scrutini_comune(cod_combo = head(cod_combo, 20), full_metadata = TRUE)

# quanto tempo?

inizio <- Sys.time()

comuni_full <- estrai_scrutini_comune(cod_combo = cod_combo, full_metadata = TRUE)

fine <- Sys.time()

fine-inizio

write_csv(x = comuni_full, path = file.path("scrutini", "scrutini_comuni_full.csv"))

