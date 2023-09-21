
# add mapview to change X and Y to standard crs
new_metadata2 <- metadata %>% 
  cbind(., as.data.frame(ens)) %>% 
  filter(Temp != "NA") %>% 
  # add small value to coordinates for one of the campaigns
  # so they do not overlap completely on the map  
  dplyr::filter(str_detect(sample_season, "fall")) %>% 
  dplyr::select("ens", "X", "Y") %>% 
  drop_na() %>% 
  mutate( ens_cat = cut(ens, b=6) )  

new_metadata2 %>% 
  rownames_to_column("sample_season") %>% 
  mutate(cluster = case_when( sample_season %in% c("11.13/3H_spring", 
                                         "11.13_21_fall", 
                                         "16_31_fall", 
                                         "17_1_5T_fall",
                                         "22-174_fall", 
                                         "22.17/4_fall", 
                                         "2205_fall", 
                                         "6-10_fall",
                                         "Baldass_fall", 
                                         "KB6_fall") ~ "compositional outlier",
                    TRUE ~ "regular microbiome")  )  -> new_metadata2


#mapviewVectorColors","mapviewRasterColors", "mapviewSpectralColors" or "mapviewTopoColors"

pal <-  mapviewPalette("mapviewTopoColors")

#CartoDB.Positron, CartoDB.DarkMatter, OpenStreetMap, Esri.WorldImagery, OpenTopoMap
sf_data <- st_as_sf(new_metadata2, coords = c("X", "Y"),  crs = 31253)
mapview(sf_data,  map.types = "CartoDB.Positron", col.regions = pal(2),
        zcol= "cluster", zoom = 12, color="black", 
        cex = 5, alpha = 0.7 ) 
