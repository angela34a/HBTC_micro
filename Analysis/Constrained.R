# env clean

new_metadata %>% dplyr::select(where(is.numeric)) %>% 
  dplyr::select(!c("Fe", "NO3", "NH3", "DOM_bix", "X", "Y")) %>% 
  scale() %>% as.data.frame()  -> env.dat



# aggregate to class
sp.dat <- asv_aggregated # already done for nmds


# asv clean
sp.dat [, which  ( colnames(sp.dat) %in% rownames(env.dat)) ] -> sp.dat

# transform hellinger (or clr sometimes)
decostand(sp.dat, method = "hellinger") -> sp.dat
#clr(sp.dat) -> sp.dat


simpleRDA <- capscale(t(sp.dat) ~  ., data=env.dat ,
                      distance = "bray",
                      scaling = "sites")

# summary(simpleRDA)
#9.6% constrained but look at vif

env_vars_low_vif <- vif.cca(simpleRDA) %>% 
  as.data.frame() %>% 
  dplyr::rename("env" = ".") %>% 
  dplyr::filter(env < 10) 

env.dat[,which(colnames(env.dat) %in%  rownames(env_vars_low_vif) )] -> env.dat


# without them
simpleRDA <- capscale(t(sp.dat) ~  ., 
                      data=  env.dat ,
                      distance = "bray",
                      scaling = "sites")

#summary(simpleRDA) 
# now 5.6%


# test it



# Test of all canonical axes
anova.model <- anova.cca(simpleRDA, by='axis', step=1000)  # cap1 is sign, cap2 not

# test the whole model
anova.model2 <- anova.cca(simpleRDA, step=1000) #is sign
RsquareAdj(simpleRDA)$adj.r.squared # and explains 1.7%


# test env paameters
anova.model3 <- anova(simpleRDA, step=1000, by = "term") #Temp + DOC sign, and K + O2 borderline


# new model
simpleRDA <- capscale(t(sp.dat) ~  Temp +  DOC + K + O2, data=env.dat ,
                      distance = "bray",
                      scaling = "sites")

#summary(simpleRDA) 
# now 1.4%






# vectors
ccavectors <- as.matrix(scores(simpleRDA, display = "bp", scaling = "sites")*40) %>% 
  t() %>% as.data.frame() %>% 
  rename("K**" = "K",
         "Temp." = "Temp",
         "O2*" = "O2") %>%
  t() %>% as.data.frame()

# site coordinates
site_data <- scores(simpleRDA, display = "sites") %>% 
  as.data.frame() %>% 
  cbind(., new_metadata$category) %>% 
  rename("cat" = "new_metadata$category")

# plotting
plot_cca <- 
  site_data %>% 
  ggplot( aes(x = CAP1, y = CAP2)) +
  geom_point(aes( color= cat), alpha = 0.2, size = 3) +
  geom_point(aes( color= cat), shape = 21, size = 3) +  
  geom_segment(data = ccavectors, aes(x = 0, y = 0, xend = CAP1, yend = CAP2), size = 1.2,
               arrow = arrow(length = unit(0.5, "cm"))) +
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  geom_text(data = ccavectors, aes(x = CAP1*1.5, y = CAP2*1.5, 
                                   label = rownames(ccavectors)),
            #nudge_x = 0.3, nudge_y = 0.3
            size=6 ) +
  theme_bw() +
  scale_fill_manual(values= pal3) +
  scale_color_manual(values=pal3) +
  labs(x = "CAP1 [1.6%]", y="CAP2 [0.7%]") +
  stat_ellipse(aes(color=cat), size = 1, alpha = 0.5) + 
  #stat_ellipse(aes(fill = cat), geom="polygon", level=0.95, alpha=0.09) +
  labs( color = "Temperature \ncategory", fill = "Temperature category", 
        title = "Bray distances of hellinger transformed samples (n = 191), 180 classes.") +
  
  theme(legend.background = element_blank(),
        legend.box.background = element_rect(colour = "gray")) 

plot_cca
