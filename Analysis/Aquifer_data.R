
# plotting ####

my_labeller <- as_labeller(c(

  DIC = "DIC ~ (mg/L)", 
  DOC = "DOC ~ (mg/L)", 
  NO3 = "NO[3] ~ (mg/L)",
  SO4 = "SO[4]^{2} ~ (mg/L)",
  Cl = "Cl ~ (mg/L)" ,
  Ca = "Ca^{2} ~ (mg/L)",
  Na = "Na ~ (mg/L)",
  K = "K ~ (mg/L)",
  Mg ="Mg^{2} ~ (mg/L) " ,
  Cond = "EC ~ (µS ~x ~10^3)",
  pH = "pH",
  int_ATP = "log(ATP) ~  (pM/L)",
  TCC = "log(TCC) ~ (cells/mL)" ,
  PO4="log(PO[4]^{3}) ~ (µg/L)",
  O2="O[2] ~ (mg/L)"),
  
  default = label_parsed)



metadata %>% 
# remove the very big environmental outliers
  dplyr::filter(! sample_season %in% c("6-12_fall", 
                                       "6-12_spring",
                                       "4007/3_spring")) %>% 
  
# choose only the numeric ones  
  dplyr::select(where(is.numeric) ) %>%
# but add the aquifer type data  
  cbind(., 
        
        ( metadata %>% dplyr::filter(! sample_season %in% c("6-12_fall", 
                                                            "6-12_spring",
                                                            "4007/3_spring")) %>% 
        dplyr::select("Geol_beb") )   ) %>%
  
  
 
# remove the unneeded variables
  dplyr::select(!c("X", "Y", "Extraction_depth", "NH4", 
                   "Watercolumn", "GW_Table", "Well_depth")) %>% 
# choose only aquifers with 5> samples
  dplyr::filter(! Geol_beb %in%  c("n.b.", "lokaler Aquifer",
                                   "Wiental", "Zubringer", "OF")) %>% 
# do not differentiate between the different Donauschotter samples 
  mutate(Geol_beb = case_when( 
    grepl('DS', Geol_beb) ~ "Donauschotter", 
    TRUE ~ Geol_beb)) %>% 
  
# before continuing transform the TCC and ATP to log scale
# and the phosphate because of the few very high levels  
  mutate(TCC = log(TCC), int_ATP = log(int_ATP), PO4 = log(PO4))  %>%  
 
# and make the EC on the nicer scale
  mutate(Cond = Cond/100)  %>%   
  
  
  pivot_longer(cols = - c(Geol_beb, Temp ), names_to = "env_var", values_to = "value" ) %>% 

# plot    
  ggplot(aes(x = Temp , y = value, group = Geol_beb)) + 
  geom_smooth(aes(color = Geol_beb), method = "lm", 
              se = FALSE) + 
  
  geom_point( aes(color = Geol_beb), alpha = 0.2, size = 1 ) +
  
# distinguish between the different variables  
  facet_wrap(vars(env_var), scales = "free_y", 
             ncol = 3,  strip.position = "left", 
             labeller = my_labeller) + 
  
  labs(x="Temperature [°C]", y= "Concentrations") + 
  scale_color_manual(values = c( "darkgreen", "orange", "darkviolet" )) +
  theme(legend.position = "none", 
        axis.title = element_text(size=10,face="bold")) 




# alternatively!
# since i would like to not log transform data, but only the axes
# this is not possible with facet_wrap

# plot1 with not log
# plot2 with log axes of TCC, ATP and PO4



#plot1

my_labeller_1<- as_labeller(c(
  
  DIC = "DIC ~ (mg/L)", 
  DOC = "DOC ~ (mg/L)", 
  NO3 = "NO[3] ~ (mg/L)",
  SO4 = "SO[4]^{2} ~ (mg/L)",
  Cl = "Cl ~ (mg/L)" ,
  Ca = "Ca^{2} ~ (mg/L)",
  Na = "Na ~ (mg/L)",
  K = "K ~ (mg/L)",
  Mg ="Mg^{2} ~ (mg/L) " ,
  Cond = "EC ~ (µS ~x ~10^3)",
  pH = "pH",
  O2="O[2] ~ (mg/L)"),
  
  default = label_parsed)



metadata %>% 
  # remove the very big environmental outliers
  dplyr::filter(! sample_season %in% c("6-12_fall", 
                                       "6-12_spring",
                                       "4007/3_spring")) %>% 
  
  # choose only the numeric ones  
  dplyr::select(where(is.numeric) ) %>%
  # remove the log ones
  dplyr::select(!c("TCC", "int_ATP","PO4"))  %>%  
  # but add the aquifer type data  
  cbind(., 
        
        ( metadata %>% dplyr::filter(! sample_season %in% c("6-12_fall", 
                                                            "6-12_spring",
                                                            "4007/3_spring")) %>% 
            dplyr::select("Geol_beb") )   ) %>%
  
  
  
  # remove the unneeded variables
  dplyr::select(!c("X", "Y", "Extraction_depth", "NH4", 
                   "Watercolumn", "GW_Table", "Well_depth")) %>% 
  # choose only aquifers with 5> samples
  dplyr::filter(! Geol_beb %in%  c("n.b.", "lokaler Aquifer",
                                   "Wiental", "Zubringer", "OF")) %>% 
  # do not differentiate between the different Donauschotter samples 
  mutate(Geol_beb = case_when( 
    grepl('DS', Geol_beb) ~ "Donauschotter", 
    TRUE ~ Geol_beb)) %>% 
  
  
  # and make the EC on the nicer scale
  mutate(Cond = Cond/100)  %>%   
  
  
  pivot_longer(cols = - c(Geol_beb, Temp ), names_to = "env_var", values_to = "value" ) %>% 
  
  # plot    
  ggplot(aes(x = Temp , y = value, group = Geol_beb)) + 
  geom_smooth(aes(color = Geol_beb), method = "lm", 
              se = FALSE) + 
  
  geom_point( aes(color = Geol_beb), alpha = 0.2, size = 1 ) +
  
  # distinguish between the different variables  
  facet_wrap(vars(env_var), scales = "free_y", 
             ncol = 3,  strip.position = "left", 
             labeller = my_labeller_1) + 
  
  labs(x="Temperature [°C]", y= "Concentrations") + 
  scale_color_manual(values = c( "darkgreen", "orange", "darkviolet" )) +
  theme(legend.position = "none", 
        plot.margin = unit(c(0.1, 0,0,0), "cm"),
        axis.title = element_text(size=10,face="bold")) -> plot_env1


# plot 2



my_labeller_2<- as_labeller(c(
  int_ATP = "ATP ~  (pM/L)",
  TCC = "TCC ~ (cells/mL)" ,
  PO4="PO[4]^{3} ~ (µg/L)"),
  
  default = label_parsed)



metadata %>% 
  # remove the very big environmental outliers
  dplyr::filter(! sample_season %in% c("6-12_fall", 
                                       "6-12_spring",
                                       "4007/3_spring")) %>% 
  
  # choose only the numeric ones  
  dplyr::select(where(is.numeric) ) %>%
  # take the log ones
  dplyr::select("TCC", "int_ATP","PO4", "Temp")  %>%  
  # but add the aquifer type data  
  cbind(., 
        
        ( metadata %>% dplyr::filter(! sample_season %in% c("6-12_fall", 
                                                            "6-12_spring",
                                                            "4007/3_spring")) %>% 
            dplyr::select("Geol_beb") )   ) %>%
  
  # choose only aquifers with 5> samples
  dplyr::filter(! Geol_beb %in%  c("n.b.", "lokaler Aquifer",
                                   "Wiental", "Zubringer", "OF")) %>% 
  # do not differentiate between the different Donauschotter samples 
  mutate(Geol_beb = case_when( 
    grepl('DS', Geol_beb) ~ "Donauschotter", 
    TRUE ~ Geol_beb)) %>% 
  
  
  pivot_longer(cols = - c(Geol_beb, Temp ), names_to = "env_var", values_to = "value" ) %>% 
  
  # plot    
  ggplot(aes(x = Temp , y = value, group = Geol_beb)) + 
  geom_smooth(aes(color = Geol_beb), method = "lm", 
              se = FALSE) + 
  
  geom_point( aes(color = Geol_beb), alpha = 0.2, size = 1 ) +
  
  # distinguish between the different variables  
  facet_wrap(vars(env_var), scales = "free_y", 
             ncol = 3,  strip.position = "left", 
             labeller = my_labeller_2) + 
  
  labs(x="Temperature [°C]", y= "Concentrations") + 
  scale_color_manual(values = c( "darkgreen", "orange", "darkviolet" )) +
  scale_y_log10() +
  theme(legend.position = "none", 
        axis.title = element_blank(),
        axis.ticks.x  = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 8), 
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(0, "lines")) -> plot_env2


 plot_env2  / plot_env1 + plot_layout(heights = c(1, 4))

 

# testing significance ####
 
 metadata %>% 
   dplyr::filter(! sample_season %in% c("6-12_fall", 
                                        "6-12_spring",
                                        "4007/3_spring")) %>% 
   dplyr::select(where(is.numeric) ) %>%
   cbind(., 
         ( metadata %>% dplyr::filter(! sample_season 
                                      %in% c("6-12_fall",                                                            "6-12_spring",                                                          "4007/3_spring")) %>% 
             dplyr::select("Geol_beb") )   ) %>%
   
   dplyr::select(!c("X", "Y", "Extraction_depth", "NH4", 
                    "Watercolumn", "GW_Table", "Well_depth")) %>% 
   # choose only aquifers with 5> samples
   dplyr::filter(! Geol_beb %in%  c("n.b.", "lokaler Aquifer",
                                    "Wiental", "Zubringer", "OF")) %>% 
   # do not differentiate between the different Donauschotter samples 
   mutate(Geol_beb = case_when( 
     grepl('DS', Geol_beb) ~ "Donauschotter", 
     TRUE ~ Geol_beb)) %>%
   pivot_longer(cols = - c(Geol_beb, Temp ), 
                names_to = "env_var", values_to = "value" ) %>% 
   group_by(Geol_beb, env_var) %>% 
   cor_test("Temp","value", method = "spearman") %>% 
   ungroup() %>% 
   dplyr::filter(p<0.05) %>% 
   mutate(signif = gtools::stars.pval(p))-> correlation_data
 
 # impact of temperature on sulfate ions!
 lm(metadata[metadata$Geol_beb == "Schichtwasser M","SO4"]~ metadata[metadata$Geol_beb == "Schichtwasser M","Temp"]) %>% summary()
 cor.test(metadata[metadata$Geol_beb == "Schichtwasser M","SO4"], metadata[metadata$Geol_beb == "Schichtwasser M","Temp"], method = "spearman")
 
 

 