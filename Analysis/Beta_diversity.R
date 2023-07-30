# temperature distribution

#| warning: false
#| output: false

# tax clean
new_taxonomy %>% rownames_to_column("asv") -> tax_tab


# aggregate to class
srs_asv_all %>% 
  rownames_to_column("asv") %>% 
  pivot_longer(cols = -asv, names_to = "samples", values_to = "abundances") %>% 
  left_join(., tax_tab, by = "asv") %>% 
  dplyr::select("samples", "Class", "abundances") %>% 
  group_by(samples,  Class) %>% 
  summarise(sum(abundances)) %>% 
  ungroup() %>% 
  dplyr::filter(Class != "") %>% 
  pivot_wider(names_from = "samples", values_from = "sum(abundances)") %>% 
  column_to_rownames("Class") -> asv_aggregated



# do an ordination
nmds <- metaMDS(t(asv_aggregated), k = 2, autotransform = FALSE)


# plot it 
nmds_plot <- as.data.frame(nmds$points) %>% 
  cbind(., as.factor(new_metadata$category)) %>% 
  dplyr::rename("cat" = "as.factor(new_metadata$category)") %>% 
  ggplot(aes(x=MDS1,y=MDS2) ) +
  geom_point(aes( color= cat), alpha = 0.2, size = 3) +
  geom_point(aes( color= cat), shape = 21, size = 3) +  
  scale_color_manual(values=pal3) +
  theme_bw() + 
  annotate(geom="text",x=1.4, y=1, label="stress =", color="black") +   
  annotate(geom="text", x=1.7, y=1, label=round(nmds$stress,4), color="black") +
  stat_ellipse(aes(colour = cat), linewidth = 0.3) + 
  theme( #aspect.ratio = 1,
    #legend.position = c( .98,  .02),
    #legend.justification = c("right", "bottom"),
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "gray")) +
  labs(color = "Temperature \ncategory") +
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) 

nmds_plot


# test ordination
# first test assumption: equal dispersion
disp <- betadisper( 
  vegdist(t(asv_aggregated), method = "bray"), 
  new_metadata$category ) 

#p>0.05 is what we need - homogenous variances
anova(disp) #no sign. differences in dispersion
#TukeyHSD(disp) # if there was sign, i would check with this which one are they

boxplot(disp, col=pal3) 


# now test actual ordination
permanova <- adonis2(t(asv_aggregated) ~ new_metadata$category, 
                     method = "bray" , permutations = 999)
permanova # 0.452



# if it was significant i would test which groups are significantly different
#pair_res <- pairwiseAdonis::pairwise.adonis2(t(asv_aggregated) ~ category, data = new_metadata)


# spatial distribution

gg <- new_metadata %>% st_as_sf(., coords = c("X", "Y"), crs = 31253) %>% 
  arrange(JMF.ID) %>%
  st_set_crs(31253) %>% 
  st_transform(4326) %>% 
  st_coordinates() %>%
  as.data.frame() %>%
  distm(., fun = distHaversine) %>%
  as.dist(.) %>% as.vector()

aa <- srs_asv_all %>% t() %>%
  vegdist(., method = "bray") %>%
  as.vector()


tt <-  new_metadata %>%
  dplyr::select("Temp") %>%
  dist(., method = "euclidean") %>%
  as.vector()


matr <- data.frame(gg,tt, aa)

ggplot(matr, aes(y = gg, x = aa)) + 
  geom_point(size = 3, alpha = 0.3, color="black", 
             shape = 21, fill= "blue",size =5 ) +
  geom_smooth(method = "lm", colour = "black", alpha = 0.2) +
  stat_cor(method = "spearman", color="black", label.y.npc="top", 
           label.x.npc = "left",size=4)+
  theme_bw() + 
  labs(x = "Community dissimilarity", y = "Physical Separation (km)") + 
  theme( axis.text.x = element_text(face = "bold",colour = "black", size = 12), 
         axis.text.y = element_text(face = "bold", size = 11, colour = "black"), 
         axis.title= element_text(face = "bold", size = 14, colour = "black"), 
         panel.background = element_blank(),
         legend.position = "none",
         panel.border = element_rect(fill = NA, colour = "black")) +
  scale_y_continuous(breaks = seq(0, 30000, by = 5000),
                     labels = function(x) x / 1000) 




# aquifer distribution
cbind(new_metadata, as.data.frame(nmds$points) ) %>% 
  dplyr::filter(! Geol_beb %in%  c("n.b.", "lokaler Aquifer",
                                   "Wiental", "Zubringer")) %>% 
  mutate(Geol_beb = case_when( grepl('DS', Geol_beb) ~ "Donauschotter", 
                               TRUE ~ Geol_beb)) -> data_aq


# first test assumption: equal dispersion
disp_aq <- betadisper( 
  vegdist(t(  asv_aggregated[,which(colnames(asv_aggregated) %in% rownames(data_aq))] ), 
          method = "bray"), 
  data_aq$Geol_beb ) 

#p>0.05 is what we need - homogenous variances
anova(disp_aq) #no sign. differences in dispersion
#TukeyHSD(disp) # if there was sign, i would check with this which one are they

boxplot(disp_aq) 


# now test actual ordination
permanova_aq <- adonis2(t( asv_aggregated[,which(colnames(asv_aggregated) %in% rownames(data_aq))] ) ~ 
                          data_aq$Geol_beb, 
                        method = "bray" , 
                        permutations = 999)
permanova # 0.477


