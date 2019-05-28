options(HTTPUserAgent=readLines(con = "useragent.txt", warn = FALSE))
pacman::p_load("tidyverse")

liste_per_circoscrizione <- jsonlite::read_json(path = paste0("https://eleapi.interno.gov.it/siel/PX/getlisteE/TE/01"),
                                                simplifyVector = FALSE)

combo_liste_circoscrizione <- purrr::map_dfr(.x = 1:5, .f = function(x) {
  
  vec <- purrr::pluck(liste_per_circoscrizione, "circ", x, "liste")  
  
  purrr::map_dfr(.x = seq_along(vec), .f = ~ vec[[.]] %>% unlist() %>% bind_rows()) %>% 
    mutate(circ = x)
  
}) %>% select(-img_lis, -lis_coll)


comuni <- readr::read_csv(file = fs::path("elenchi", "comuni.csv"))

cod_combo <- comuni %>% 
  transmute(cod_comune = as.character(stringr::str_pad(string = cod_comune, width = 4, side = "left", pad = "0")),
            cod_provincia = as.character(stringr::str_pad(string = cod_provincia, width = 3, side = "left", pad = "0")),
            cod_regione = as.character(stringr::str_pad(string = cod_regione, width = 2, side = "left", pad = "0")),
            cod_circoscrizione = as.character(cod_circoscrizione)) %>% 
  as_tibble() %>% 
  left_join(combo_liste_circoscrizione %>%
              mutate(cod_circoscrizione = as.character(circ)) %>% 
              mutate(pos = as.character(stringr::str_pad(string = pos, width = 4, side = "left", pad = "0"))) %>% 
              select(-circ), by = "cod_circoscrizione")


estrai_preferenze_comune <- function(cod_combo, full_metadata = TRUE) {
  pb <- progress_estimated(nrow(cod_combo))
  dir.create("scrutini_comuni_preferenze_json", showWarnings = FALSE)
  all_urls <- paste0("https://eleapi.interno.gov.it/siel/PX/prefeEI/TE/01/PR/", cod_combo$cod_provincia, "/CM/", cod_combo$cod_comune, "/AG/", cod_combo$pos)
  dest_file_comuni <- paste0("scrutini_comuni_preferenze_json/", cod_combo$cod_provincia, "_CM_", cod_combo$cod_comune, "_AG_", cod_combo$pos, ".json")
  
  purrr::walk2(.x = all_urls, .y = dest_file_comuni, .f = ~ if(file.exists(.y) == FALSE) download.file(url = .x, destfile = .y))
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
                     
                     cbind(scrutini_temp_base$cand, scrutini_temp_base$liste, info_temp)
                   } )
  }
}

scrutini_preferenze_comune <- estrai_preferenze_comune(cod_combo = cod_combo)

write_csv(x = scrutini_preferenze_comune, path = file.path("scrutini", "scrutini_preferenze_comune_full.csv"))

##### preferenze provincia #####


estrai_preferenze_provincia <- function(cod_combo, full_metadata = TRUE) {
  cod_combo <- cod_combo %>% 
    distinct(cod_provincia, pos)
  
  pb <- progress_estimated(nrow(cod_combo))
  
  dir.create("scrutini_provincia_preferenze_json", showWarnings = FALSE)
  all_urls <- paste0("https://eleapi.interno.gov.it/siel/PX/prefeEI/TE/01/PR/", cod_combo$cod_provincia, "/AG/", cod_combo$pos)
  dest_file_provincia <- paste0("scrutini_provincia_preferenze_json/", cod_combo$cod_provincia, "_AG_", cod_combo$pos, ".json")
  
  purrr::walk2(.x = all_urls, .y = dest_file_provincia, .f = ~ if(file.exists(.y) == FALSE) download.file(url = .x, destfile = .y))
  purrr::walk(.x = dest_file_provincia, .f = ~ writeLines(iconv(readLines(.), from = "ISO-8859-1", to = "UTF8"), .))
  
  
  if (full_metadata==TRUE) {
    purrr::map_dfr(.x = dest_file_provincia,
                   .f = function(x) {
                     pb$tick()$print()
                     
                     scrutini_temp_base <- jsonlite::read_json(path = x,
                                                               simplifyVector = TRUE)
                     
                     
                     
                     info_temp <- tibble(descrizione = names(scrutini_temp_base$int),
                                         dato = as.character(scrutini_temp_base$int)) %>% 
                       spread(key = descrizione, value = dato)
                     
                     cbind(scrutini_temp_base$cand, scrutini_temp_base$liste, info_temp)
                   } )
  }
}

scrutini_preferenze_provincia <- estrai_preferenze_provincia(cod_combo = cod_combo)

write_csv(x = scrutini_preferenze_provincia, path = file.path("scrutini", "scrutini_preferenze_provincia_full.csv"))


##### preferenze regione #####



estrai_preferenze_regione <- function(cod_combo, full_metadata = TRUE) {
  cod_combo <- cod_combo %>% 
    distinct(cod_regione, pos)
  
  pb <- progress_estimated(nrow(cod_combo))
  
  dir.create("scrutini_regione_preferenze_json", showWarnings = FALSE)
  all_urls <- paste0("https://eleapi.interno.gov.it/siel/PX/prefeEI/TE/01/RE/", cod_combo$cod_regione, "/AG/", cod_combo$pos)
  dest_file_regione <- paste0("scrutini_regione_preferenze_json/", cod_combo$cod_regione, "_AG_", cod_combo$pos, ".json")
  
  purrr::walk2(.x = all_urls, .y = dest_file_regione, .f = ~ if(file.exists(.y) == FALSE) download.file(url = .x, destfile = .y))
  purrr::walk(.x = dest_file_regione, .f = ~ writeLines(iconv(readLines(.), from = "ISO-8859-1", to = "UTF8"), .))
  
  
  if (full_metadata==TRUE) {
    purrr::map_dfr(.x = dest_file_regione,
                   .f = function(x) {
                     pb$tick()$print()
                     
                     scrutini_temp_base <- jsonlite::read_json(path = x,
                                                               simplifyVector = TRUE)
                     
                     
                     
                     info_temp <- tibble(descrizione = names(scrutini_temp_base$int),
                                         dato = as.character(scrutini_temp_base$int)) %>% 
                       spread(key = descrizione, value = dato)
                     
                     cbind(scrutini_temp_base$cand, scrutini_temp_base$liste, info_temp)
                   } )
  }
}

scrutini_preferenze_regione <- estrai_preferenze_regione(cod_combo = cod_combo)

write_csv(x = scrutini_preferenze_regione, path = file.path("scrutini", "scrutini_preferenze_regione_full.csv"))

