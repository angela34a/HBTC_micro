
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

