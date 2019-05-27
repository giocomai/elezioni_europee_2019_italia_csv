pacman::p_load("tidyverse")


scrutini_regione <- read_csv(file = fs::path("scrutini", "scrutini_regione.csv"))

scrutini_provincia <- read_csv(file = fs::path("scrutini", "scrutini_provincia_full.csv"))



scrutini_comuni <- read_csv(file = fs::path("scrutini", "scrutini_comuni_full.csv"))

full_join(x = scrutini_provincia %>% 
            group_by(desc_pr) %>% 
            summarise(voti_totali_prov = sum(voti)) %>% 
            rename(desc_prov = desc_pr),
          y = scrutini_comuni %>% 
            group_by(desc_prov) %>% 
            summarise(voti_totali_prov_da_comuni = sum(voti)),
          by = "desc_prov") %>% 
  mutate(check = voti_totali_prov == voti_totali_prov_da_comuni) %>% 
  filter(check == FALSE)



