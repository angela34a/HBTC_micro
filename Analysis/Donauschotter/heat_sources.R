spring_data_additional %>% 
  dplyr::select("ens", "int_ATP", "TCC",  
                # "hts_GWWP",
                #"hts_EWS", 
                #"hts_UB",
                "hts_kanal",
                "hts_FW", 
                #"hts_Altlast",
                #"hts_OGFliess", 
                #"hts_OGSteh", 
                "hts_Versie", #
                #"hts_No_UHI" , 
                "hts_UHI", #
                #"hts_count", 
                "wastew_beb",
                "Redox_beb",
                "OF_beb"    ) %>% 
 # change values to not be 1 and 0 but rather text 
  mutate( #hts_UB = ifelse(hts_UB == 1, "present", "not present"),
          hts_kanal =  ifelse(hts_kanal == 1, "present", "not present"),
          hts_FW = ifelse(hts_FW == 1, "present", "not present"), 
          hts_Versie  = ifelse(hts_Versie == 1, ">25%", "<25%"), 
          wastew_beb = ifelse(wastew_beb == 1, "present", "not present"),
          Redox_beb = case_when(
            Redox_beb == 1 ~ "Stage2",
            Redox_beb == 2 ~ "Stage3",
            Redox_beb == 3 ~ "Stage4",
            Redox_beb == 0 ~ "Stage1"),
          OF_beb  = case_when(
            OF_beb == 2 ~ "running w.",
            OF_beb == 3 ~ "stagnant w.",
            OF_beb == 1 ~ "no effect" )
) %>% 
    
    
#    Effect = ifelse(Effect == 1, "effect present", "no effect")) 
  
  
  #mutate(int_ATP = log(int_ATP), TCC = log(TCC)) %>% 
  na.omit() %>% 
  pivot_longer(cols = -c("ens", "int_ATP", "TCC") ,
               names_to = "heat_sources",
               values_to = "values") %>% 
  
  
  pivot_longer(cols = - c("heat_sources", "values"),
               names_to = "variables_ens_atp",
               values_to = "values_ens_atp") %>% 


  ggplot(aes(x= values , y= values_ens_atp)) +
  geom_jitter(width = 0.2, size = 2, shape=21, fill = "violet") +
  geom_boxplot(fill = "violet", alpha=0.5, outlier.shape = NA) +
  facet_grid( variables_ens_atp  ~ heat_sources, 
              #switch = "y",
              scales = "free",
              labeller = my_labeller) +
  theme(strip.text = element_text(size = 8, face = "bold"),
        axis.text.x = element_text(size = 7.2)) +
  labs(x = NULL, y= NULL)


my_labeller <- as_labeller(
  c(
  ens = "Shannon~diversity~index",
  int_ATP = "ATP ~  (pM/L)",
  TCC = "TCC ~ (cells/mL)", 
  #"hts_GWWP" = "Geothermal~energy~use",
  #"hts_EWS", 
  "hts_UB" = "Subsurface~infrastructure",
  "hts_kanal"  = "Channel~influence",
  "hts_FW" = "District~heating~pipe", 
  #"hts_Altlast",
  #"hts_OGFliess", 
  #"hts_OGSteh", 
  "hts_Versie" = "Surface~sealing", #
  #"hts_No_UHI" , 
  "hts_UHI" = "Number~of~heat~sources", #
  #"hts_count", 
  "wastew_beb" = "Waste~water~influence",
  "Redox_beb" = "Redox~stage",
  "OF_beb" = "Surface~water"
  
  ),
  
  default = label_parsed)
  


# test


spring_data_additional %>% 
  dplyr::select("ens", "int_ATP", "TCC",  
                # "hts_GWWP",
                #"hts_EWS", 
                #"hts_UB",
                "hts_kanal",
                "hts_FW", 
                #"hts_Altlast",
                #"hts_OGFliess", 
                #"hts_OGSteh", 
                "hts_Versie", #
                #"hts_No_UHI" , 
                "hts_UHI", #
                #"hts_count", 
                "wastew_beb",
                "Redox_beb",
                "OF_beb"    ) %>% 
  na.omit() %>% 
  
  pivot_longer(cols = -c("ens", "int_ATP", "TCC") ,
               names_to = "heat_sources",
               values_to = "values")   -> data_for_test

data_for_test %>% 
  group_by(heat_sources) %>% 
  kruskal_test(ens ~ values) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  dplyr::filter(p <0.05)


data_for_test %>% 
  group_by(heat_sources) %>% 
  kruskal_test(int_ATP      ~ values) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  dplyr::filter(p <0.05)


data_for_test %>% 
  group_by(heat_sources) %>% 
  kruskal_test(TCC  ~ values) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  dplyr::filter(p <0.05)


