options(HTTPUserAgent=readLines(con = "useragent.txt", warn = FALSE))

if (!require(pacman)) install.packages("pacman")
pacman::p_load("tidyverse")
pacman::p_load("jsonlite")

dir.create("elenchi", showWarnings = FALSE)


#### circoscrizioni ####
# trova tutte le circoscrizioni
circoscrizioni <- read_json(path = "https://eleapi.interno.gov.it/siel/PX/elenchiEX/TE/01", simplifyVector = TRUE)[["enti"]] %>% 
  rename(cod_circoscrizione = cod_ente, circoscrizione = desc_ente)

circoscrizioni

write_csv(x = circoscrizioni, path = file.path("elenchi","circoscrizioni.csv"))


#### regioni ####
# trova tutte le regioni per ogni circoscrizione


regioni <- purrr::map_dfr(.x = circoscrizioni$cod_circoscrizione,
                          .f = ~ read_json(path = paste0("https://eleapi.interno.gov.it/siel/PX/elenchiEI/TE/01/CR/", .),
                                           simplifyVector = TRUE)[["enti"]], .id = "cod_circoscrizione") %>% 
  dplyr::rename(cod_regione = cod_ente, regione = desc_ente) %>% 
  mutate(cod_circoscrizione = as.integer(cod_circoscrizione)) %>% 
  left_join(y = circoscrizioni %>% select(cod_circoscrizione, circoscrizione), by = "cod_circoscrizione")

regioni

write_csv(x = regioni, path = file.path("elenchi","regioni.csv"))

####  province #### 

province <- purrr::map_dfr(.x = str_pad(string = regioni$cod_regione, width = 2, side = "left", pad = "0"),
                           .f = ~ read_json(path = paste0("https://eleapi.interno.gov.it/siel/PX/elenchiEI/TE/01/RE/", .),
                                            simplifyVector = TRUE)[["enti"]], .id = "id_process_cod_regione") %>% 
  mutate(id_process_cod_regione = as.integer(id_process_cod_regione)) %>% 
  left_join(y = tibble(cod_regione = regioni$cod_regione) %>% 
              mutate(id_process_cod_regione = dplyr::row_number()), by = "id_process_cod_regione") %>% 
  select(-id_process_cod_regione) %>% 
  dplyr::rename(cod_provincia = cod_ente, provincia = desc_ente) %>% 
  mutate(cod_regione = as.integer(cod_regione)) %>% 
  left_join(y = regioni %>% select(cod_circoscrizione, circoscrizione, cod_regione, regione), by = "cod_regione")

write_csv(x = province, path = file.path("elenchi","province.csv"))

#### comuni #### 

url_elenco_comuni <- paste("https://eleapi.interno.gov.it/siel/PX/elenchiEI/TE/01/RE",
                           str_pad(string = province$cod_regione, width = 2, side = "left", pad = "0"),
                           "PR", 
                           str_pad(string = province$cod_provincia, width = 3, side = "left", pad = "0"),
                           sep = "/")

dest_file_comuni <- file.path("elenco_comuni_json", paste0(paste(
  str_pad(string = province$cod_regione, width = 2, side = "left", pad = "0"),
  "PR", 
  str_pad(string = province$cod_provincia, width = 3, side = "left", pad = "0"),
  sep = "_"), ".json"))

# salva file locali per evitare problema di encoding
dir.create("elenco_comuni_json", showWarnings = FALSE)

purrr::walk2(.x = url_elenco_comuni, .y = dest_file_comuni, .f = ~ download.file(url = .x, destfile = .y))

purrr::walk(.x = dest_file_comuni, .f = ~ writeLines(iconv(readLines(.), from = "ISO-8859-1", to = "UTF8"), .))

comuni <- purrr::map_dfr(.x = dest_file_comuni,
                         .f = ~ read_json(path = .,
                                          simplifyVector = TRUE)[["enti"]], .id = "id_process_cod_provincia") %>% 
  mutate(id_process_cod_provincia = as.integer(id_process_cod_provincia)) %>% 
  left_join(y = tibble(cod_provincia = province$cod_provincia) %>% 
              mutate(id_process_cod_provincia = dplyr::row_number()), by = "id_process_cod_provincia") %>% 
  select(-id_process_cod_provincia) %>% 
  dplyr::rename(cod_comune = cod_ente, comune = desc_ente) %>% 
  left_join(y = province %>% select(cod_circoscrizione, circoscrizione, cod_regione, regione, cod_provincia, provincia), by = "cod_provincia")

write_csv(x = comuni, path = file.path("elenchi","comuni.csv"))


#####  Europee Estero\circoscrizione    (elenco circoscrizioni estero) #####


circoscrizioni_estero_per_circoscrizione <- read_json(path = "https://eleapi.interno.gov.it/siel/PX/elenchiEE/TE/01", simplifyVector = TRUE)[["enti"]] %>% 
  rename(cod_circoscrizione = cod_ente, circoscrizione = desc_ente)

circoscrizioni_estero_per_circoscrizione

write_csv(x = circoscrizioni, path = file.path("elenchi","circoscrizioni_estero_per_circoscrizione.csv"))

##### Europee Estero\circoscrizione    (elenco singola nazione per circoscrizioni estero) #####


circoscrizioni_estero_nazioni <- read_json(path = "https://eleapi.interno.gov.it/siel/PX/elenchiEEN/TE/01", simplifyVector = TRUE)[["enti"]] %>% 
  rename(cod_circoscrizione = cod_ente, circoscrizione = desc_ente)

write_csv(x = circoscrizioni_estero_nazioni, path = file.path("elenchi","circoscrizioni_estero_nazioni.csv"))



######  Europee Estero\circoscrizione    (elenco singola circoscrizione tutte le nazioni estero)  ######

circoscrizioni_per_nazione <- purrr::map_dfr(.x = 1:5, .f = function(x) {
  read_json(path = paste0("https://eleapi.interno.gov.it/siel/PX/elenchiEE/TE/01/CR/", x), simplifyVector = TRUE)[["enti"]] %>% 
    rename(cod_circoscrizione = cod_ente, circoscrizione = desc_ente)
}, .id = "cod_circoscrizione_ita")



write_csv(x = circoscrizioni_per_nazione, path = file.path("elenchi","circoscrizioni_per_nazione.csv"))



