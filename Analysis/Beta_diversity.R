

# by temperature ####
# tax clean
tax_table %>% rownames_to_column("asv") -> tax_tab

# aggregate to genus level
asv_table %>% 
  rownames_to_column("asv") %>% 
  pivot_longer(cols = -asv, names_to = "samples", 
               values_to = "abundances") %>% 
  left_join(., tax_tab, by = "asv") %>% 
  dplyr::select("samples", "Genus", "abundances") %>% 
  group_by(samples,  Genus) %>% 
  summarise(sum(abundances)) %>% 
  ungroup() %>% 
  replace_na(list(Genus ='Unclassified' )) %>%
  pivot_wider(names_from = "samples", 
              values_from = "sum(abundances)") %>% 
  column_to_rownames("Genus") -> asv_aggregated



# do an ordination with a hellinger transformation 
sp.dist <-  vegdist(decostand( t(asv_aggregated),
                               method = "hellinger") )

# or without the transformation
# sp.dist <-  vegdist(t(asv_aggregated))
nmds <- metaMDS(sp.dist, k = 2, autotransform = FALSE)

# plot it 
nmds_plot <- as.data.frame(nmds$points) %>% 
  cbind(., as.factor(metadata$category)) %>% 
  dplyr::rename("cat" = "as.factor(metadata$category)") %>%
  filter(cat !=  "NA") %>% 
  ggplot(aes(x=MDS1,y=MDS2) ) +
  geom_point(aes( fill= cat), shape = 21, 
             size = 4, alpha = 0.6, stroke = 0.5) +  
  # coloring  
  scale_color_manual(values=pal3) +
  scale_fill_manual(values=pal3) +
  # add stress value
  annotate(geom="text",x=0.8, y=0.7, 
           label="stress =", color="black") +   
  annotate(geom="text", x=1.1, y=0.7, 
           label=round(nmds$stress,4), color="black") +
  # ellipse  
  stat_ellipse(aes(colour = cat), linewidth = 0.4) + 
  # visual parameters 
  theme(legend.background = element_blank(),
        legend.box.background = element_rect(colour = "gray")) +
  labs(fill = "Temperature \ncategory") +
  guides(color = "none") +
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) 

nmds_plot

## test it ####

# 1. test dispersion as an assumption 
# (variance test for multivariate statitics)
disp <- betadisper( sp.dist, metadata$category ) 

# p>0.05 is what we need - homogenous variances
anova(disp) 
# sign. differences in dispersion and we cant take sign, in centroids for granted




# 2. now test actual ordination
# test differences in group centroid with adonis 
# (anova version for multivariate statistics)
permanova <- adonis2(sp.dist ~ metadata$category, na.action = na.omit,
                     method = "bray" , permutations = 999)
permanova # not sign.






# by geographical position ####

# data with only the coordinates known
data_coo <- metadata %>% 
  dplyr::select("sample_season", "X","Y") %>% 
  na.omit() 

gg <- data_coo %>% 
  st_as_sf(., coords = c("X", "Y"), crs = 31253) %>% 
  st_set_crs(31253) %>% 
  st_transform(4326) %>% 
  st_coordinates() %>%
  as.data.frame() %>%
  distm(., fun = distHaversine) %>%
  as.dist(.) %>% as.vector()

aa <- asv_table[, colnames(asv_table) %in% data_coo$sample_season] %>%  
  t() %>% 
  vegdist(., method = "bray") %>%
  as.vector()


matr <- data.frame(gg, aa)

ggplot(matr, aes(y = gg, x = aa)) + 
  geom_point(size = 3, alpha = 0.9, color="black", 
             shape = 21, fill= "violet",size = 5 ) +
  geom_smooth(method = "lm", colour = "black", alpha = 0.2) +
  # add correlation info  
  ggpubr::stat_cor(method = "spearman",  
                   label.y.npc="top", 
                   label.x.npc = "left",
                   color="black", size=4) +
  # edit visualization  
  labs(x = "Community dissimilarity", 
       y = "Physical Separation (km)") + 
  theme( axis.text.x = element_text(face = "bold",
                                    colour = "black", size = 12), 
         axis.text.y = element_text(face = "bold", 
                                    size = 11, colour = "black"), 
         axis.title = element_text(face = "bold", 
                                   size = 14, colour = "black")) +
  # to turn y scale to kms
  scale_y_continuous(breaks = seq(0, 30000, by = 5000),
                     labels = function(x) x / 1000) 



# by aquifer ####

cbind(metadata, as.data.frame(nmds$points) ) %>% 
  dplyr::filter(! Geol_beb %in%  c("n.b.", "lokaler Aquifer",
                                   "Wiental", "Zubringer", "OF")) %>% 
  dplyr::select("Geol_beb") %>% 
  na.omit() -> data_aq


# first test assumption: equal dispersion
disp_aq <- betadisper( 
  vegdist(t(  asv_aggregated[,which(colnames(asv_aggregated) %in% rownames(data_aq))] ),
          method = "bray"), 
  data_aq$Geol_beb ) 

#p>0.05 is what we need - homogenous variances
anova(disp_aq) #no sign. differences in dispersion
#TukeyHSD(disp) # if there was sign, i would check with this which one are they


# now test actual ordination
permanova_aq <- adonis2(t( asv_aggregated[,which(colnames(asv_aggregated) %in% rownames(data_aq))] ) ~ 
                          data_aq$Geol_beb, 
                        method = "bray" , 
                        permutations = 999)
permanova # not sign
