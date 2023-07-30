shannondiv <- vegan::diversity(t(srs_asv_all))
ens <- exp(shannondiv)

p1_en <- cbind(as.data.frame(ens), as.factor(new_metadata$category)) %>% 
  as.data.frame() %>%
  rename("div"= "ens","temp"="as.factor(new_metadata$category)") %>%
  filter(div > 0) %>% 
  ggplot( aes(x = temp, y = div, fill = temp, color=temp)) +
  #stat_boxplot(geom ='errorbar') + 
  geom_boxplot(alpha=0.5) +
  scale_fill_manual(values = pal3)+
  scale_color_manual(values = pal3)+
  theme_bw()+
  theme(legend.position = "none",
        axis.title=element_text(size=14,face="bold"),
        axis.text.y = element_text(size=10,face="bold"),
        axis.text.x = element_text(angle = 90)) +
  labs(x = "Temperature category",
       y = "Shannon diversity index") +
  scale_x_discrete(limits = c( "ref_cat", "<10", "10-12", "12-14", "14-16", 
                               "16-18",  "18-20",  "20<" ))  

p1_en



# since there is only 1 sample in the 20< i must remove it before testing
data_for_alpha <- cbind( as.data.frame(ens), new_metadata$category) %>% 
  filter(new_metadata$category != "20<") %>% 
  rename("category" = "new_metadata$category")


# perform ANOVA?
## 1. assumption: Normal distribution: 
## Each population to be compared should be normally distributed (NOT THE ENTIRE DATASET).
shapiro.test (  data_for_alpha[which(data_for_alpha$category == "<10"), "ens"] )
# W = 0.89814, p-value = 0.3997
shapiro.test (  data_for_alpha[which(data_for_alpha$category == "10-12"), "ens"] )
# W = 0.77218, p-value = 0.04715 !!!
shapiro.test (  data_for_alpha[which(data_for_alpha$category == "12-14"), "ens"] )
# W = 0.97895, p-value = 0.4823
shapiro.test (  data_for_alpha[which(data_for_alpha$category == "14-16"), "ens"] )
# W = 0.9595, p-value = 0.05755
shapiro.test (  data_for_alpha[which(data_for_alpha$category == "16-18"), "ens"] )
# W = 0.93248, p-value = 0.1543
shapiro.test (  data_for_alpha[which(data_for_alpha$category == "18-20"), "ens"] )
# W = 0.58055, p-value = 0.0003393 !!!
shapiro.test (  data_for_alpha[which(data_for_alpha$category == "ref_cat"), "ens"] )
# W = 0.981, p-value = 0.9642

# would be better to do a non-parametric test

## 2. assumption: Homogeneity of variance: 
## Variance in the populations compared should be the same/similar. 
bartlett.test(ens ~ category, data= data_for_alpha)
# Bartlett's K-squared = 0.94125, df = 6, p-value = 0.9877
# assumption met

kruskal.test (ens ~ category, data= data_for_alpha) 
# Kruskal-Wallis chi-squared = 11.995, df = 6, p-value = 0.06208


library(afex) ## to run ANOVA

alph <- cbind(as.data.frame(ens), as.factor(new_metadata$category)) %>% 
  as.data.frame() %>%
  rename("div"= "ens","temp"="as.factor(new_metadata$category)") 

ggbetweenstats( data  = alph, x = temp, y = div,
                type = "np",
                xlab = "Temperature category", 
                ylab = "Shannon diversity index",
                ggtheme = ggplot2::theme_classic(),
                title = "Comparison of microbial diversity across temperature increase",
                #boxplot.args = list(width = 0),
                #violin.args = list(width = 3, alpha = 4),
                point.args = list(position = ggplot2::position_jitterdodge
                                  (dodge.width = 1), alpha = 0.4, 
                                  size = 3, stroke = 2, na.rm = TRUE),
                var.equal = TRUE) + 
  
  ggplot2::scale_x_discrete(limits = c( "ref_cat", "<10", "10-12", "12-14", "14-16", 
                                        "16-18",  "18-20",  "20<" )) +
  ggplot2::scale_color_manual(values=pal3)


# linear trend 
p1_en_li <- cbind(as.data.frame(ens), new_metadata$Temp) %>% 
  as.data.frame() %>%
  rename("div"= "ens","temp"="new_metadata$Temp") %>%
  ggplot(aes(temp,div)) + 
  geom_point(shape=21) + 
  geom_smooth(method = "lm", se= FALSE, color = "blue") + 
  stat_poly_eq(use_label(c("eq", "adj.R2", "p")), label.y = "bottom") + 
  theme_bw() + 
  labs(y = "Shannon diversity index", x ="Temperature")

p1_en_li


# spatial distribution

# add mapview to change X and Y to standard crs
new_metadata2 <- new_metadata %>% 
  cbind(new_metadata, as.data.frame(ens)) %>% 
  dplyr::select("ens", "X", "Y") %>% drop_na() %>% 
  mutate( ens_cat = cut(ens, b=6) )  

pal <-  mapviewPalette("mapviewSpectralColors")

sf_data <- st_as_sf(new_metadata2, coords = c("X", "Y"),  crs = 31253)
mapview(sf_data,  map.types = "Stamen.Terrain", col.regions = pal(10),
        zcol= "ens_cat", zoom = 12, color="black", cex = 4, alpha = 0.9 ) 


# aquifer distributuion


# ANY AQUIFER THAT HAS MORE THAN 5 POINTS
cbind(new_metadata, ens ) %>% 
  dplyr::filter(! Geol_beb %in%  c("n.b.", "lokaler Aquifer",
                                   "Wiental", "Zubringer")) %>% 
  mutate(Geol_beb = case_when( grepl('DS', Geol_beb) ~ "Donauschotter", 
                               TRUE ~ Geol_beb)) %>% 
  ggplot(aes(x=Temp, y=ens)) + 
  geom_point(shape=21, size = 2) + 
  geom_smooth(method = "lm", se= FALSE, color = "blue") + 
  facet_wrap(~ Geol_beb, scales = "free") + 
  #stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "adj.R2", "p")), label.y = "bottom",
               size = 3.4) + 
  theme_bw() + 
  labs(x = "Temperature",
       y = "Effective number of species") 


# modelling 

data_for_reg <- cbind(new_metadata) %>% dplyr::select(where(is.numeric)) %>% 
  scale(center = T, scale = T) %>% cbind(., ens) %>% data.frame() %>% 
  dplyr::select(! c("DIC", "SO4", "Cl", "Na", "Ca", "Mg", "Lf",
                    "NO3", "NH3", "Fe", "X", "Y", "DOM_bix") )


nullModel = lm(ens ~ 1, data=data_for_reg) # model with the intercept only
fullModel = lm(ens ~ ., data=data_for_reg) # model with all  variables

#choose
step.model<-stepAIC(fullModel, # start with a model containing no variables
                    direction = 'both', # run forward selection
                    trace = 0) # dont show step-by-step process of model selection
#summary(step.model) # DOC, K, Depth
RsquareAdj(step.model) # 0.043-> 4.3 %
summary(lm(ens ~ DOC + K + Depth, data = data_for_reg)) 


plot(ens ~ DOC , data = data_for_reg) + abline(lm(ens ~ DOC , data = data_for_reg))
plot(ens ~  K , data = data_for_reg)+ abline(lm(ens ~ K , data = data_for_reg))
plot(ens ~  Depth, data = data_for_reg) + abline(lm(ens ~ Depth , data = data_for_reg))
