# 1 ####
spring_data_additional %>% 
  dplyr::select(where(is.numeric) ) %>% 
  dplyr::select(!c("K", "Na", "Mg", "Cond", "S", "NH4",
                   "X", "Y", "Cl", "DIC")) %>% 
  pivot_longer(cols = -Temp, values_to = "values", names_to = "names") %>% 
  ggplot(aes(x= values,y=Temp)) +
    geom_point() +
    geom_smooth(method = "lm") +
    facet_wrap(~names, scales = "free")
 
# 2 ####

spring_data_additional %>% 
  mutate( #hts_UB = ifelse(hts_UB == 1, "present", "not present"),
    hts_kanal =  ifelse(hts_kanal == 1, "present", "not present"),
    hts_FW = ifelse(hts_FW == 1, "present", "not present"), 
    hts_Versie  = ifelse(hts_Versie == 1, ">25%", "<25%"), 
    hts_UB = ifelse(hts_UB == 1, "present", "not present")
  ) %>% 
  dplyr::select("Temp",  
                #"hts_GWWP",
                #"hts_EWS", 
                "hts_UB",
                "hts_kanal",
                "hts_FW", 
                #"hts_Altlast",
                #"hts_OGFliess", 
                #"hts_OGSteh", 
                "hts_Versie", #
                #"hts_No_UHI" , 
                "hts_UHI", #
                #"hts_count", 
                #"wastew_beb",
                #"Redox_beb",
                #"OF_beb"    
                ) %>% 
 
#mutate(int_ATP = log(int_ATP), TCC = log(TCC)) %>% 
 na.omit() %>% 
  pivot_longer(cols = -Temp ,
               names_to = "heat_sources",
               values_to = "values") %>% 
  
  
  ggplot(aes(x= values , y= Temp)) +
  geom_jitter(width = 0.2, size = 2, shape=21, fill = "violet") +
  geom_boxplot(fill = "violet", alpha=0.5, outlier.shape = NA) +
  facet_wrap(  ~ heat_sources, 
              #switch = "y",
              scales = "free_x", 
              nrow = 2, 
              labeller = my_labeller) +
  theme(strip.text = element_text(size = 11, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold")) +
  labs(y = "Temperature [°C]", x= NULL)


# test
spring_data_additional %>% 
dplyr::select("Temp",
              "hts_UB",
              "hts_kanal",
              "hts_FW", 
              "hts_Versie",
              "hts_UHI") %>% 
  pivot_longer(cols = -Temp) %>% 
  group_by(name) %>% 
  kruskal_test(Temp ~ value)


# 3 ####

df_temp <- metadata %>%  
  dplyr::select(where(is.numeric)) %>% 
  dplyr::select(-c("NH4", # many NAs
                   "X", "Y")) %>% # coordinates
  na.omit() %>% 
  scale(center = T, scale = T) %>% 
  as.data.frame()


# run individual regressions for each variable
varlist <- names(df_temp)[-17]
models <- lapply(varlist, function(x) {
  form <- formula(paste("Temp ~", x))
  lm(form, data=df_temp)
})
# tidy up the results
lapply(models, function(x) tidy(x)) %>% bind_rows() -> ind_models
ind_models %>% filter(term != "(Intercept)") %>% filter(p.value<0.05)
# sign  "DIC" "NO3" "PO4" "SO4" "Cl" "Na" "K" "Ca" "Mg"
# "GW_Table" "Watercolumn" "Cond" "pH" "int_ATP"    




# 4 ####

fullModel = lm(Temp ~ ., data=df_temp) # model with all  variables

# remove high vif variables
car::vif(fullModel)  %>% as.data.frame() %>% 
  dplyr::rename("vif"=".") %>% 
  filter(vif<10)  %>% 
  rownames_to_column("envs") %>% 
  dplyr::select("envs")  -> colnames

df_Temp2 <- df_temp %>% dplyr::select("Temp", 
                                        which(colnames(df_temp) %in% colnames$envs ))

fullModel = lm(Temp ~ .  , data = df_Temp2) # model with all  variables


#choose
step.model<- MASS::stepAIC(fullModel, # start with a model containing no variables
                           direction = 'both', # run forward selection
                           trace = 0) # dont show step-by-step process of model selection

#summary(step.model) # none
RsquareAdj(step.model) # <2%

summary(step.model)
