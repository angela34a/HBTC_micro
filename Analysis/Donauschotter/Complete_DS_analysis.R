
spring_data_additional %>% 
  column_to_rownames("sample_season") %>% 
  mutate(OF_beb = as.factor(OF_beb),
         Redox_beb = as.factor(Redox_beb),
         wastew_beb = as.factor(wastew_beb),
         hts_GWWP = as.factor(hts_GWWP),
         hts_EWS = as.factor(hts_EWS),
         hts_No_UHI = as.factor(hts_No_UHI),
         hts_UB = as.factor(hts_UB),
         hts_kanal = as.factor(hts_kanal),
         hts_FW = as.factor(hts_FW),
         hts_Altlast = as.factor(hts_Altlast),
         hts_OGFliess = as.factor(hts_OGFliess),
         hts_OGSteh = as.factor(hts_OGSteh),
         hts_Versie = as.factor(hts_Versie),
         hts_UHI = as.factor(hts_UHI),
         hts_count = as.factor(hts_count)) -> spring_data_additional


# add shannon diversity data
merge(as.data.frame(ens), spring_data_additional, 
      by = "row.names")  -> spring_data_additional

# filter only DS subset
spring_data_additional %>% 
  dplyr::filter(grepl('DS', Geol_beb)) %>% 
  column_to_rownames("Row.names") -> spring_data_additional



# ens vs env vars ####

split_labels <- str_wrap(c("no effect",            # for "1"
                           "running water effect", # for "2"
                           "unclear"),             # for "3"
                         width = 8)

p1 <- spring_data_additional %>% 
  dplyr::filter(OF_beb != "NA") %>% 
  ggplot(aes(x=OF_beb, y=ens)) + 
  geom_jitter(width = 0.2, size = 3, shape=21, fill = "violet") +
  geom_boxplot(fill = "violet", alpha=0.5, outlier.shape = NA) +
  labs(x="Surface water impact", 
       y= "Shannons diversity index") +
  scale_x_discrete(labels = split_labels)


# redox  (Redox_beb)
split_labels <- str_wrap(c("o2 > 2", "o2 < 2", 
                           "Mn>0.15 & Fe>0.15", 
                           "s>0.1"), width = 8)

p2 <- spring_data_additional %>% 
  #dplyr::filter(OF_beb != "NA") %>% 
  ggplot(aes(x=Redox_beb, y=ens)) + 
  geom_jitter(width = 0.2, size = 3, shape=21, fill = "violet") +
  geom_boxplot(fill = "violet", alpha=0.5, outlier.shape = NA) +
  labs(x="Redox cascade", 
       y= NULL) +
  scale_x_discrete(labels = split_labels)


# wastewater  (wastew_beb)
split_labels <- str_wrap(c("no effect",            # for "1"
                           "effect present", # for "2"
                           "unclear"), width = 8)

p3 <- spring_data_additional %>% 
  #dplyr::filter(OF_beb != "NA") %>% 
  ggplot(aes(x=wastew_beb, y=ens)) + 
  geom_jitter(width = 0.2, size = 3, shape=21, fill = "violet") +
  geom_boxplot(fill = "violet", alpha=0.5, outlier.shape = NA) +
  labs(x="Wastewater impact", 
       y= NULL) +
  scale_x_discrete(labels = split_labels)

p1 + p2 + p3



# ens vs heat sources ####


# surface sealing
p4 <- spring_data_additional %>% 
  dplyr::filter(hts_Versie != "NA") %>% 
  ggplot(aes(x=hts_Versie, y=ens)) + 
  geom_jitter(width = 0.2, size = 3, shape=21, fill = "violet") +
  geom_boxplot(fill = "violet", alpha=0.5, outlier.shape = NA) + 
  labs(x="Surface sealing", 
       y= "Shannons diversity index") +
  scale_x_discrete(labels = c(">25%", "+25%"))


# number of heat sources
p5 <- spring_data_additional %>% 
  dplyr::filter(hts_UHI != "NA") %>% 
  ggplot(aes(x=hts_UHI, y = ens)) + 
  geom_jitter(width = 0.2, size = 3, shape=21, fill = "violet") +
  geom_boxplot(fill = "violet", alpha=0.5, outlier.shape = NA) + 
  labs(x="UHI count", 
       y= NULL) 


p4 + p5

# ens vs metals ####
spring_data_additional %>% 
  dplyr::select("ens", "Al", "Cr", "Fe_57", "Ni", "Cu", "Zn", 
                "As", "Ag", "Cd", "Hg", "Pb", "Mean_CO2", "Mean_CH4") %>% 
  na.omit() %>% 
  scale(center = T, scale = T) %>% 
  as.data.frame() -> df_spring


# remove the highly correlated ones
cor_matrix <- cor(df_spring)
highly_correlated <- caret::findCorrelation(cor_matrix, cutoff = 0.9)
# no variables with coeff > 0.9

# run individual regressions for each variable
varlist <- names(df_spring)[- 1] 
# remove ens, the independant var
models <- lapply(varlist, function(x) {
  form <- formula(paste("ens ~", x))
  lm(form, data=df_spring)
})
# tidy up the results
lapply(models, function(x) tidy(x)) %>% bind_rows() -> ind_models
ind_models %>% filter(term != "(Intercept)") %>% filter(p.value<0.05)



spring_data_additional %>% 
  dplyr::select("ens", "Ni", "Ag", "Mean_CH4") %>% 
  na.omit() %>% 
  pivot_longer(cols= -ens, values_to = "values", 
               names_to = "vars") %>% 
  #plot  
  
  ggplot(aes(y=ens, x=values)) + 
  geom_point(shape=21, size = 3, fill = "violet", 
             alpha = 0.9, stroke = 0.6) + 
  geom_smooth(method = "lm", se= FALSE, color = "black") + 
  ggpmisc::stat_poly_eq(use_label(c("adj.R2", "p")), 
                        small.p = TRUE,
                        label.y = "bottom", size = 3.4) + 
  facet_wrap(~vars, scales = "free") + 
  labs(y = "Shannon diversity index", 
       x ="Environmental measurements") +
  theme(legend.position = "none",
        axis.title = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(angle = 90)) 


# ens - mlr ####

spring_data_additional %>% 
  dplyr::select(where(is.numeric)) %>% 
  dplyr::select(-c("NH4", "S",      # many NAs
                   "X", "Y",                # coordinates
                   "N15_NO3", "O18_NO3",    # many NAs
                   "CH4",                   # remove raw, keep sdCH4
                   "d13_CO2")            
  ) %>% 
  na.omit() %>% 
  scale(center = T, scale = T) %>% 
  as.data.frame() -> df_spring_all


fullModel = lm(ens ~ ., data=df_spring_all) # model with all  variables

# remove high vif variables
car::vif(fullModel)  %>% as.data.frame() %>% 
  dplyr::rename("vif"=".") %>% 
  filter(vif<10)  %>% 
  rownames_to_column("envs") %>% 
  dplyr::select("envs")  -> colnames

df_spring_all2 <- df_spring_all %>% dplyr::select("ens", 
                                                  which(colnames(df_spring_all) %in% colnames$envs ))

fullModel = lm(ens ~ .  , data = df_spring_all2) # model with all  variables


#choose
step.model<- MASS::stepAIC(fullModel, # start with a model containing no variables
                           direction = 'both', # run forward selection
                           trace = 0) # dont show step-by-step process of model selection

# summary(step.model) 
# none sign, but intATP borderline
RsquareAdj(step.model) # 7%


# communties of DS ####
# env clean
# df_spring_all - already scaled and selected the important variables

# aggregate to genus
# asv_aggregated - already done for nmds

# asv clean
# because some samples get removed because of NAs 
# and we need only the spring samples
asv_aggregated [, which  ( colnames(asv_aggregated) %in% 
                             rownames(df_spring_all)) ] -> sp.dat

# transform hellinger (or clr sometimes)
decostand(sp.dat, method = "hellinger") -> sp.dat
#clr(sp.dat) -> sp.dat

# 1. first model
simpleRDA <- capscale(t(sp.dat) ~  ., data = df_spring_all ,
                      distance = "bray",
                      scaling = "sites")
# take only that are vif<10
env_vars_low_vif <- vif.cca(simpleRDA) %>% 
  as.data.frame() %>% 
  dplyr::rename("env" = ".") %>% 
  dplyr::filter(env < 10) 

# make new environmental data with only low vif variables
df_spring_all[,which(colnames(df_spring_all) %in%  
                       rownames(env_vars_low_vif) )] -> env.dat


# 2. model without them
simpleRDA <- capscale(t(sp.dat) ~  ., 
                      data=  env.dat ,
                      distance = "bray",
                      scaling = "sites")


# test all variables and axes
# canonical axes
anova.model <- anova.cca(simpleRDA, by='axis', step=1000)  
# no axes sign

# the whole model
anova.model2 <- anova.cca(simpleRDA, step=1000) #is sign
RsquareAdj(simpleRDA)$adj.r.squared # and explains <3%

# env parameters
anova.model3 <- anova(simpleRDA, step=1000, by = "term") 
# none sign
anova.model3


