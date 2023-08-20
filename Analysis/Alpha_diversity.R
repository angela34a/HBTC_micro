# color palettes ####
pal3 = c("#253494", "#225ea8", "#1d91c0", "#7fcdbb" , 
         "#fdbb84", "#d7301f", "#7f0000", "gray")

# for easier uniform plotting of categories 
name_vector <- c( "ref_cat", "<10", "10-12", "12-14", "14-16", 
                  "16-18",  "18-20",  "20<" )


# calcute ####
# calculate Shannon's index with vegan
shannondiv <- vegan::diversity(t(asv_table))
# turning Shannon to effective number of species 
# to be more comparable across studies 
ens <- exp(shannondiv)


p1_en <- cbind(as.data.frame(ens), as.factor(metadata$category)) %>% 
  as.data.frame() %>%
  rename("div"= "ens",
         "temp"="as.factor(metadata$category)") %>%
  filter(temp != "NA") %>% 

# plot  
  ggplot( aes(x = temp, y = div, fill = temp)) +
  geom_jitter(width = 0.2, size = 3, shape=21, aes(fill = temp) ) +
  geom_boxplot(alpha=0.5, outlier.shape = NA) +
  # add custom palette  
  scale_fill_manual(values = pal3) +
  # make  the figure more readable  
  theme(legend.position = "none",
        axis.title=element_text(size=14,face="bold"),
        axis.text.y = element_text(size=10,face="bold"),
        axis.text.x = element_text(angle = 90)) +
  labs(x = "Temperature category",
       y = "Shannon diversity index",
       title = "Diversity by temp. category") +
  scale_x_discrete(limits = name_vector)  

p1_en



# test ####

# 1. assumption - normal distribution: 
# Each population to be compared should be normally distributed 

cbind( as.data.frame(ens), metadata$category) %>%
  rename("cat" = "metadata$category" ) %>% 
  filter(cat != "NA") %>% 
  group_by(cat) %>% 
  do(tidy(shapiro.test(.$ens))) %>% 
  dplyr::filter(p.value <0.06)

# I would rather do a non-parametric test
# since >20 borders not normally distributed



# 2. assumption - homogeneity of variance: 
# Variance in the populations compared should be the same/similar. 
cbind( as.data.frame(ens), metadata$category) %>%
  rename("cat" = "metadata$category" ) %>% 
  filter(cat != "NA") %>% 
  levene_test(ens ~ cat) %>% 
  dplyr::select("p") 


# assumption met
# but irrelevant
# do non-parametric anova
cbind( as.data.frame(ens), metadata$category) %>%
  rename("cat" = "metadata$category" ) %>% 
  filter(cat != "NA") %>% 
  kruskal_test(ens ~ cat)
# significant difference between groups


# do post-hoc to see which
cbind( as.data.frame(ens), metadata$category) %>%
  rename("cat" = "metadata$category" ) %>% 
  filter(cat != "NA") %>% 
  dunn_test(ens ~ cat) %>% 
  dplyr::filter(p.adj < 0.05)
# post-hoc does not show individual difference
# so no adding to the plot



# linear trend ####

p1_en_li <- cbind(as.data.frame(ens), metadata$Temp) %>% 
  as.data.frame() %>%
  rename("div"= "ens","temp"="metadata$Temp") %>%
  filter(temp != "NA") %>% 
  
  # plot
  ggplot(aes(temp,div)) + 
  geom_point(shape=21, size = 3, fill = "violet", alpha = 0.9, stroke = 0.6) + 
  geom_smooth(method = "lm", se= FALSE, color = "black") + 
  ggpmisc::stat_poly_eq(use_label(c("adj.R2", "p")), 
                        label.y = "top", size = 4,
                        small.p = TRUE) + 
  labs(y = "Shannon diversity index", x ="Temperature", 
       title = "Linear trend in diversity index") +
  theme(legend.position = "none",
        axis.title=element_text(size=14,face="bold"),
        axis.text.y = element_text(size=10,face="bold"),
        axis.text.x = element_text(angle = 90)) 

p1_en_li



# spatial distribution ####

# add mapview to change X and Y to standard crs
new_metadata2 <- metadata %>% 
  cbind(., as.data.frame(ens)) %>% 
  filter(Temp != "NA") %>% 
  # add small value to coordinates for one of the campaigns
  # so they do not overlap completely on the map  
  mutate(X = ifelse(grepl("fall", sample_season) , X + 45, X)) %>% 
  dplyr::select("ens", "X", "Y") %>% 
  drop_na() %>% 
  mutate( ens_cat = cut(ens, b=6) )  


pal <-  mapviewPalette("mapviewSpectralColors")

sf_data <- st_as_sf(new_metadata2, coords = c("X", "Y"),  crs = 31253)
mapview(sf_data,  map.types = "Stamen.Terrain", col.regions = pal(10),
        zcol= "ens_cat", zoom = 12, color="black", 
        cex = 5, alpha = 0.7 ) 





# aquifer distribution ####


cbind( metadata, ens ) %>% 
  # filter out aquifers that have less than 5 sample points 
  dplyr::filter(! Geol_beb %in%  c("n.b.", "lokaler Aquifer",
                                   "Wiental", "Zubringer", "OF")) %>% 
  # do not differentiate between the different Donauschotter samples 
  mutate(Geol_beb = case_when( 
    grepl('DS', Geol_beb) ~ "Donauschotter", 
    TRUE ~ Geol_beb)) %>% 
  dplyr::select("ens", "Geol_beb", "Temp") %>% 
  na.omit() %>% 
  # plot
  ggplot(aes(x=Temp, y=ens)) + 
  geom_point(shape=21, size = 3, fill = "violet", alpha = 0.9, stroke = 0.5) + 
  geom_smooth(method = "lm", se= FALSE, color = "black") +
  facet_wrap(~ Geol_beb, scales = "free_y", ncol = 2) + 
  stat_poly_eq(use_label(c("eq", "adj.R2", "p")), 
               label.y = "bottom", small.p = TRUE,
               size = 3) + 
  labs(x = "Temperature [°C]",
       y = "Sshannon's diversity index") +
  theme(axis.title.x = element_text(hjust = 0), 
        legend.position = "none",
        axis.title=element_text(size=14,face="bold"),
        axis.text.y = element_text(size=10,face="bold"),
        axis.text.x = element_text(angle = 90),
        strip.text = element_text(size = 12))




# individual regressions ####

df_alpha <- metadata %>%  
  cbind(as.data.frame(ens)) %>% 
  dplyr::select(where(is.numeric)) %>% 
  dplyr::select(-c("NH4", # many NAs
                   "X", "Y")) %>% # coordinates
  na.omit() %>% 
  scale(center = T, scale = T) %>% 
  as.data.frame()


# remove the highly correlated ones
cor_matrix <- cor(df_alpha)
highly_correlated <- caret::findCorrelation(cor_matrix, cutoff = 0.9)
df_alpha <- df_alpha[, -highly_correlated] 

# run individual regressions for each variable
varlist <- names(df_alpha)[-19]
models <- lapply(varlist, function(x) {
  form <- formula(paste("ens ~", x))
  lm(form, data=df_alpha)
})
# tidy up the results
lapply(models, function(x) tidy(x)) %>% bind_rows() -> ind_models
ind_models %>% filter(term != "(Intercept)") %>% filter(p.value<0.05)
# none are sign




# multiple linear regression ####

fullModel = lm(ens ~ ., data=df_alpha) # model with all  variables

# remove high vif variables
car::vif(fullModel)  %>% as.data.frame() %>% 
  dplyr::rename("vif"=".") %>% 
  filter(vif<10)  %>% 
  rownames_to_column("envs") %>% 
  dplyr::select("envs")  -> colnames

df_alpha2 <- df_alpha %>% dplyr::select("ens", 
                                        which(colnames(df_alpha) %in% colnames$envs ))

fullModel = lm(ens ~ .  , data = df_alpha2) # model with all  variables


#choose
step.model<- MASS::stepAIC(fullModel, # start with a model containing no variables
                           direction = 'both', # run forward selection
                           trace = 0) # dont show step-by-step process of model selection

#summary(step.model) # none
RsquareAdj(step.model) # <2%



