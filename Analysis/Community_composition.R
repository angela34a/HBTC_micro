# bar plot

# with microviz

#ps_all %>% tax_fix(unknowns = c("uncultured")) %>% 
#  comp_barplot("Class", n_taxa = 15, merge_other = TRUE, label = NULL) +
#  facet_wrap(vars(category), scales = "free", nrow = 2, ncol = 4) + 
#  coord_flip() +
#  scale_fill_manual(values=c25) + 
#  theme_minimal()




srs_asv_all %>%
  mutate(class = new_taxonomy$Class) %>%
  pivot_longer(cols = - class, values_to = "abundance", names_to = "sample") %>%
  left_join(.,new_metadata, by = c("sample" = "JMF.ID")) %>%  
  dplyr::select ("sample", "abundance", "class", "category") %>%
  group_by(category) %>%
  mutate (rel_abund = abundance / sum(abundance)) %>%
  ungroup() %>% 
  group_by(category, class) %>%
  summarise(rel_abund = sum(rel_abund)) %>%
  ungroup() %>%
  group_by(category, class) %>%
  mutate(class = case_when(rel_abund< 0.015~ "Diverse others", 
                           TRUE ~ class)) %>%
  ungroup() %>%
  group_by(category, class) %>%
  summarise(rel_abund = sum(rel_abund)) %>%
  ungroup() %>%
  #mutate(class = case_when( class == "" ~ "Unclassified", TRUE ~ class)) %>% 
  replace_na(list(class ='Unclassified' )) %>% 
  ggplot(aes(x = category, y = rel_abund, fill = class)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c25) +
  theme_classic() +
  theme(panel.grid = element_blank(), legend.position="right") + 
  labs(x = "",
       y = "Relative abundance", 
       fill = "Class") +
  guides(fill=guide_legend(ncol =1)) +
  scale_x_discrete(limits = c( "ref_cat", "<10", "10-12", "12-14", "14-16", "16-18",  "18-20",  "20<" )) 


# bubble plot


data <- srs_asv_all %>%
  mutate(class = new_taxonomy$Class) %>%
  pivot_longer(cols = - class, values_to = "abundance", names_to = "sample") %>%
  left_join(.,new_metadata, by = c("sample" = "JMF.ID")) %>%  
  dplyr::select ("sample", "abundance", "class", "category") %>%
  group_by(category) %>%
  mutate (rel_abund = abundance / sum(abundance)) %>%
  ungroup() %>% 
  group_by(category, class) %>%
  summarise(rel_abund = sum(rel_abund)) %>%
  ungroup() %>%
  group_by(category, class) %>%
  mutate(class = case_when(rel_abund< 0.015~ "Diverse others", 
                           TRUE ~ class)) %>%
  ungroup() %>%
  group_by(category, class) %>%
  summarise(rel_abund = sum(rel_abund)) %>%
  ungroup() %>%
  #mutate(class = case_when( class == "" ~ "Unclassified", TRUE ~ class))
  replace_na(list(class ='Unclassified' ))

data %>% as.data.frame() %>% 
  ggplot(aes(x = as.factor(category), y = as.factor(class) )) + 
  geom_point(aes(size = rel_abund, fill=class ), alpha = 0.75, shape = 21) + 
  scale_size_continuous(limits = c(0.001, 0.5), 
                        range = c(1,17), 
                        #breaks = c(0.1, 0.2, 0.5)
  ) + 
  labs( x= "", y = "", size = "Relative Abundance", fill = "")  + 
  theme(legend.key=element_blank(), 
        axis.text.x = element_text(colour = "black", size = 12, face = "bold", 
                                   angle = 90, vjust = 0.3, hjust = 1),
        axis.text.y = element_text(colour = "black", face = "bold", size = 11), 
        legend.text = element_text(size = 10, face ="bold", colour ="black"), 
        legend.title = element_text(size = 12, face = "bold"), 
        panel.background = element_blank(), panel.border = 
          element_rect(colour = "black", fill = NA, size = 1.2), 
        legend.position = "right") +  
  scale_fill_manual(values = c25, guide = FALSE) +   
  scale_y_discrete(limits = rev(levels(data$class))) +
  scale_x_discrete(limits = c( "ref_cat", "<10", "10-12", "12-14", "14-16", 
                               "16-18",  "18-20",  "20<" )) 



# mean + sd 
ps_for_sd <- tax_glom(ps_all, "Class") %>% 
  transform_sample_counts(., function(x) x/sum(x))

tb <- psmelt(ps_for_sd) %>%
  as_tibble

tb0 <- tb %>%
  group_by(Class) %>%
  dplyr::summarize( Mean = mean(Abundance), SD = sd(Abundance) ) %>%
  ungroup() %>% 
  mutate( Mean_per = .$Mean *100, sd_per = .$SD *100 )  %>% 
  dplyr::filter( Mean_per > 1 ) %>% 
  left_join(., 
            distinct( new_taxonomy[, c("Kingdom", "Phylum", "Class")], Class , 
                      .keep_all = TRUE ) ,
            by="Class") 

head(tb0[order(tb0$Mean_per), c("Class", "Mean_per", "sd_per", "Phylum")])
head(tb0[, c("Class", "Kingdom", "Phylum")] )



