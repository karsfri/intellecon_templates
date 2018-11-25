library(sf)
library(tidyverse)
sf_location <- "C:\\Users\\ksf\\Google Drive\\data\\map\\IS50V_MORK_SHP\\"
sf_file <- "is50v_mork_umdaemi_svaedi_17062018.shp"

sf_file <- paste0(sf_location, sf_file)

sf_obj <- st_read(sf_file)

transform_ice_map <- function(x, ...){
    sf::st_transform(sf_obj, crs = 4258, ...)
}


sf_obj %>%
    transform_ice_map() %>% 
    mutate(test = runif(n = n(), 0, 1)) %>% 
    ggplot(aes(fill = NRUMDSYSLU %>% as.factor)) +
    theme_void() +
    geom_sf() +
    theme(panel.grid = element_blank()) +
    scale_fill_manual(values = intellecon_colours %>% rep(2)) +
    # coord_sf(xlim = c(-22.1, -21.6), ylim = c(64.05, 64.2)) +
    NULL

pop %>% 
    # filter(Sveitarfélag == "Reykjavík",
    #        Aldur %in% c("0-4 ára", "5-9 ára")) %>% 
    ggplot(aes(x = Ár, y = `Mannfjöldi eftir sveitarfélagi, kyni og aldri 1. janúar 1998-2018`, fill = Aldur)) +
    geom_point(position = position_dodge())