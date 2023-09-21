
# ens ~ temp ####

p1_all_box <- spring_data_additional %>% 
  rownames_to_column("sample_season") %>% 
  
  left_join(., metadata[,c("category", "sample_season")], 
            by = "sample_season") %>% 
  
  ggplot( aes(x = category, y = ens, group = category,  fill = category)) +
  geom_jitter(width = 0.2, size = 3, shape=21, aes(fill = category) ) +
  geom_boxplot(alpha=0.5, outlier.shape = NA) +
  
  # add custom palette  
  scale_fill_manual(values = pal3) +
  # make  the figure more readable  
  theme(legend.position = "none",
        axis.title=element_text(size=14,face="bold"),
        axis.text.y = element_text(size=10,face="bold"),
        axis.text.x = element_text(angle = 90)) +
  labs(x = "Temperature category",
       y = "Shannon diversity index") +
scale_x_discrete(limits = name_vector)  



p2_just_subset <- spring_data_additional %>% 
  rownames_to_column("sample_season") %>% 
   
  left_join(., metadata[,c("category", "sample_season")], 
            by = "sample_season") %>% 

  dplyr::filter(category %in% c("10-12", "12-14",
                                "14-16", "16-18") ) %>% 
# plot
  ggplot( aes(x = category, y = ens, group = category,  fill = category)) +
  geom_jitter(width = 0.2, size = 3, shape=21, aes(fill = category) ) +
  geom_boxplot(alpha=0.5, outlier.shape = NA) +
    
  geom_smooth( method = "lm", aes(group = 1), se = F,
               color = "black") +


  # add custom palette  
  scale_fill_manual(values = c("#225ea8", "#1d91c0", "#7fcdbb", "#fdbb84")) +
  # make  the figure more readable  
  theme(legend.position = "none",
        axis.title=element_text(size=14,face="bold"),
        axis.text.y = element_text(size=10,face="bold"),
        axis.text.x = element_text(angle = 90)) +
  labs(x = "Temperature category",
       y = "Shannon diversity index") 


p1_all_box + p2_just_subset

# test 
spring_data_additional %>% 
  rownames_to_column("sample_season") %>% 
  left_join(., metadata[,c("category", "sample_season")], 
            by = "sample_season") %>% 
  filter(category != "NA") %>% 
  kruskal_test(ens ~ category)


spring_data_additional %>% 
  rownames_to_column("sample_season") %>% 
  left_join(., metadata[,c("category", "sample_season")], 
            by = "sample_season") %>% 
  filter(category != "NA") %>% 
  dunn_test(ens ~ category, 
            p.adjust.method = "hochberg") %>% 
  dplyr::filter(p.adj < 0.05)



# ATP ####
p_atp <- spring_data_additional %>% 
  rownames_to_column("sample_season") %>% 
  left_join(., metadata[,c("category", "sample_season")], 
            by = "sample_season") %>% 
  # filter out obvious outliers
  dplyr::filter(int_ATP <100) %>% 
  
  # plot
  ggplot( aes(x = category, y = int_ATP, fill = category)) +
  geom_jitter(width = 0.2, size = 3, shape=21, aes(fill = category) ) +
  geom_boxplot(alpha=0.5, outlier.shape = NA) +
  # add custom palette  
  scale_fill_manual(values = pal3) +
  # make  the figure more readable  
  theme(legend.position = "none",
        axis.title=element_text(size=14,face="bold"),
        axis.text.y = element_text(size=10,face="bold"),
        axis.text.x = element_text(angle = 90)) +
  labs(x = "Temperature category",
       y = "ATP [pM/L]") +
  scale_x_discrete(limits = name_vector, 
                   labels = c("ref_cat", "<10",   "10-12",
                              "12-14", "14-16", "16-18", 
                              "18-20",    ">20"  ))  +
  scale_y_log10()







# TCC ####
p_tcc <- spring_data_additional %>% 
  rownames_to_column("sample_season") %>% 
  left_join(., metadata[,c("category", "sample_season")], 
            by = "sample_season") %>% 

# filter out huge outliers
  dplyr::filter(TCC <200000) %>% 
    
  # plot
  ggplot( aes(x = category, y = TCC, fill = category)) +
  geom_jitter(width = 0.2, size = 3, shape=21, aes(fill = category) ) +
  geom_boxplot(alpha=0.5, outlier.shape = NA) +
  # add custom palette  
  scale_fill_manual(values = pal3) +
  # make  the figure more readable  
  theme(legend.position = "none",
        axis.title=element_text(size=14,face="bold"),
        axis.text.y = element_text(size=10,face="bold"),
        axis.text.x = element_text(angle = 90)) +
  labs(x = "Temperature category",
       y = "Total cell counts [cells/mL]") +
  scale_x_discrete(limits = name_vector, 
                   labels = c("ref_cat", "<10",   "10-12",
                              "12-14", "14-16", "16-18", 
                              "18-20",    ">20"  )) +
  scale_y_log10()


spring_data_additional %>% 
  rownames_to_column("sample_season") %>% 
  left_join(., metadata[,c("category", "sample_season")], 
            by = "sample_season") %>% 
  dplyr::filter(TCC <200000) %>% 
  dplyr::select("TCC", "category") %>% 
  group_by(category) %>% 
  summarise(TCC_mean = mean(TCC)) 


p_tcc + p_atp
