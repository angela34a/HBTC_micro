# season variable
new_metadata  <- new_metadata %>% 
  mutate(season = case_when( str_detect(sample_season, "_fall") ~ "fall",
                             str_detect(sample_season,"_spring")~ "spring") ) 


# temperature categories
new_metadata$Cat3 <- cut(new_metadata$Temp, 
                         breaks=c(0, 10, 12, 14, 16, 18, 20, 30), 
                         labels=c('<10', '10-12', '12-14', '14-16', 
                                  '16-18', '18-20', '20<'))
# find reference samples (the one in Marchfeld)
reference <- new_metadata %>% 
  filter(sample_season %in% c("22-246_fall", "EM10_spring",  "EM62_spring", "22-52_fall", 
                              "22-52_spring", "22.7/2_spring", "22.7/2_fall"))

# distinguish reference samples from the 10-12C
new_metadata <- new_metadata %>%
  mutate( reference = case_when(sample_season %in% reference$sample_season ~ "referential", 
                                TRUE ~ "other")) 
new_metadata$ref_cat <- paste(new_metadata$Cat3, new_metadata$reference, sep="_")
new_metadata <- new_metadata %>% 
  mutate( category = case_when(ref_cat == "<10_other" ~ "<10",
                               ref_cat == "10-12_other" ~ "10-12",
                               ref_cat == "12-14_other" ~ "12-14",
                               ref_cat == "14-16_other" ~ "14-16",
                               ref_cat == "16-18_other" ~ "16-18",
                               ref_cat == "18-20_other" ~ "18-20",
                               ref_cat == "20<_other" ~ "20<",
                               TRUE ~ "ref_cat")) %>% 
  dplyr::select(!c("Cat3" , "reference" ,    "ref_cat"))


# option a) vegan
asv_table_final <- asv_table_final[, colSums(asv_table_final) >= 2000] 
# remove those that have less than 2000
srs_asv_all <- rrarefy(t(asv_table_final), sample = 2000) %>% t() %>% as.data.frame()
new_metadata <- new_metadata[which(new_metadata$JMF.ID %in% colnames(srs_asv_all)  ),]
