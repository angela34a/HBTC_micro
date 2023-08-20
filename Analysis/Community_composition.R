

# color palettes
c25 <- c("dodgerblue2", "#E31A1C", "green4" ,  "#6A3D9A",     "skyblue2", 
         "blue1", "gold1", "brown",  "#FB9A99", "steelblue4" ,  "palegreen2" , 
         "#CAB2D6", "#FDBF6F", "khaki2",  "maroon",  "gray70",  "orchid1",  
         "deeppink1",  "darkorange4",  "green1",  "yellow4"  ,    "yellow3",  
         "beige", "darkturquoise", "#FF7F00" )

# for easier uniform plotting of categories 
name_vector <- c( "ref_cat", "<10", "10-12", "12-14", "14-16", 
                  "16-18",  "18-20",  "20<" )



# bar plot ####

asv_table %>% 
  mutate_if(is.integer, as.numeric) %>% 
  mutate(class = tax_table$Class) %>%
  pivot_longer(cols = -class, values_to = "abundance", names_to = "sample") %>%
  left_join(.,metadata, by = c("sample" = "sample_season")) %>%  
  dplyr::select ("abundance", "class", "category") %>%
  # to remove those with no category
  dplyr::filter(category != "NA") %>% 
  # to make NA classes Unclassified   
  replace_na(list(class ='Unclassified' )) %>%
  mutate(category = as.factor(category),
         class = as.factor(class)) %>%  
  # to find relative abundance within each category  
  dplyr::group_by(category) %>%
  mutate (rel_abund = abundance / sum(abundance)) %>%
  ungroup() %>%
  # to sum up all the counts of one class from all the samples  
  dplyr::group_by(category, class) %>%
  summarise(rel_abund = sum(rel_abund)) %>%
  ungroup() %>%
  # those very rare categorize as Diverse others 
  dplyr::group_by(category, class) %>%
  mutate(class = case_when(rel_abund< 0.015~ "Diverse others", 
                           TRUE ~ class)) %>%
  ungroup() %>%
  # final regrouping after categorizing for Diverse others  
  dplyr::group_by(category, class) %>%
  summarise(rel_abund = sum(rel_abund)) %>%
  ungroup() %>%
  
  # plot
  ggplot(aes(x = category, y = rel_abund, fill = class)) + 
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values=c25) +
  theme(panel.grid = element_blank(), legend.position="right",
        legend.text = element_text(size = 6),      # Adjust the text size
        legend.title = element_text(size = 6),
        legend.key.size = unit(0.4, "cm"),
        axis.title=element_text(size=14,face="bold"),
        axis.text.y = element_text(size=10,face="bold"),
        axis.text.x = element_text(angle = 90, face = "bold")) + 
  labs(x = "Temperature categories",
       y = "Relative abundance", 
       fill = "Class") +
  guides(fill=guide_legend(ncol = 1)) +
  scale_x_discrete(limits = name_vector) 



# bubble plot ####
asv_table %>% 
  mutate_if(is.integer, as.numeric) %>% 
  mutate(class = tax_table$Class) %>%
  pivot_longer(cols = -class, values_to = "abundance", names_to = "sample") %>%
  left_join(.,metadata, by = c("sample" = "sample_season")) %>%  
  dplyr::select ("abundance", "class", "category") %>%
  # to remove those with no category
  dplyr::filter(category != "NA") %>% 
  # to make NA classes Unclassified   
  replace_na(list(class ='Unclassified' )) %>%
  mutate(category = as.factor(category),
         class = as.factor(class)) %>%  
  # to find relative abundance within each category  
  dplyr::group_by(category) %>%
  mutate (rel_abund = abundance / sum(abundance)) %>%
  ungroup() %>%
  # to sum up all the counts of one class from all the samples  
  dplyr::group_by(category, class) %>%
  summarise(rel_abund = sum(rel_abund)) %>%
  ungroup() %>%
  # those very rare categorize as Diverse others 
  dplyr::group_by(category, class) %>%
  mutate(class = case_when(rel_abund< 0.015~ "Diverse others", 
                           TRUE ~ class)) %>%
  ungroup() %>%
  # final regrouping after categorizing for Diverse others  
  dplyr::group_by(category, class) %>%
  summarise(rel_abund = sum(rel_abund)) %>%
  ungroup() -> data

# plot  
data %>% 
  ggplot(aes(x = as.factor(category), y = as.factor(class) )) + 
  geom_point(aes(size = rel_abund, fill=class ), 
             alpha = 0.8, shape = 21, stroke = 0.5) + 
  scale_size_continuous(limits = c(0.001, 0.4), range = c(1,7)) + 
  labs( x= "", y = "", size = "Relative Abundance", fill = "")  + 
  theme(legend.key=element_blank(), 
        axis.text.x = element_text(colour = "black", size = 7, face = "bold", 
                                   angle = 90, vjust = 0.3, hjust = 1),
        axis.text.y = element_text(colour = "black", face = "bold", size = 11), 
        legend.text = element_text(size = 7, face ="bold", colour ="black"), 
        legend.title = element_text(size = 7, face = "bold"), 
        legend.position = "right") +  
  scale_fill_manual(values = c25, guide = "none") +   
  scale_y_discrete(limits = rev(levels(data$class))) +
  scale_x_discrete(limits = name_vector) 


# mean +- sd ####
data %>% # as summarized in the above chunk 
  group_by(class) %>% 
  summarise( Mean = mean(rel_abund), SD = sd(rel_abund) ) %>% 
  ungroup() %>% 
  dplyr::mutate( Mean_perc = .$Mean *100, sd_perc = .$SD *100)  %>% 
  ungroup() %>% 
  arrange(desc(Mean_perc) ) %>% 
  dplyr::select(!c("Mean", "SD")) %>% 
  dplyr::filter(Mean_perc > 2) %>% 
  knitr::kable()



# methane-processing microorganisms ####

# 1. clean taxonomy
# subset the "Methylo-" and "Methano-" taxa at the order level
tax_table %>% rownames_to_column("asv") %>%   
  # filter the methane taxa
  dplyr::filter(grepl('Meth', Order)) %>% 
  dplyr::select("asv", "Order")  -> tax_meth


# 2. clean asv_table
# choose only the asvs from the selected orders
asv_table[which(rownames(asv_table) %in% tax_meth$asv ),] %>% 
  
  # only spring has CH4 measures, select spring samples
  rownames_to_column("asv") %>% 
  pivot_longer(cols = -asv, names_to = "samples", values_to = "abundances") %>%
  
  # filter only the spring samples
  dplyr::filter(grepl('spring', samples)) %>%   
  
  # connect to the order information  
  left_join(., tax_meth, by = "asv") %>% 
  dplyr::select("samples", "Order", "abundances") %>% 
  group_by(samples, Order) %>% 
  
  # sum up the abundance within each order (diff sp/taxa)  
  summarise(abund = sum(abundances)) %>% 
  ungroup() %>% 
  
  # remove the samples where abundance is 0
  dplyr::filter(abund != "0") %>% 
  
  # connect the asv data to the metadata (methane level)
  left_join(., spring_data_additional[,c("sample_season", "CH4")], 
            by = c("samples" = "sample_season") ) %>% 
  
  # remove the samples that do not have methane measures  
  dplyr::filter(CH4 != "NA") %>%
  
  # plot 
  ggplot(aes(x = CH4 , y = abund)) +
  geom_point(shape=21, size = 3, fill = "violet", alpha = 0.9, stroke = 0.5) + 
  geom_smooth(method = "lm", se= FALSE, color = "black") +
  facet_wrap(~ Order, scales = "free_y") + 
  stat_poly_eq(use_label(c( "adj.R2", "p")), 
               label.y = "top",
               size = 3) +
  labs(y="Abundances (absolute 16S counts)",
       x= "Methane levels")


# methane levels as standard deviation ####
asv_table[which(rownames(asv_table) %in% tax_meth$asv ),] %>% 
  rownames_to_column("asv") %>% 
  pivot_longer(cols = -asv, names_to = "samples", values_to = "abundances") %>%
  dplyr::filter(grepl('spring', samples)) %>%   
  left_join(., tax_meth, by = "asv") %>% 
  dplyr::select("samples", "Order", "abundances") %>% 
  group_by(samples, Order) %>% 
  summarise(abund = sum(abundances)) %>% 
  ungroup() %>% 
  dplyr::filter(abund != "0") %>% 
  
  # but connect it to the sd_CH4, not CH4 in hopes of clearer trends   
  left_join(., spring_data_additional[,c("sample_season", "Mean_CH4")], 
            by = c("samples" = "sample_season") ) %>% 
  dplyr::filter(Mean_CH4 != "NA") %>%
  
  # filter the selected orders   
  dplyr::filter(Order %in% c( "Methylococcales", 
                              "Methylomirabilales", 
                              "Methanosarciniales")) %>% 
  
  # plot 
  ggplot(aes(x = Mean_CH4 , y = abund)) +
  geom_point(shape=21, size = 3, fill = "violet", alpha = 0.9, stroke = 0.5) + 
  geom_smooth(method = "lm", se= FALSE, color = "black") +
  facet_wrap(~ Order, scales = "free_y") + 
  stat_poly_eq(use_label(c( "adj.R2", "p")), 
               label.y = "top", small.r = TRUE,
               size = 3) +
  labs(y="Abundances (absolute 16S counts)",
       x= "Mean methane levels")


# temperature related abundance - linear ####

# 1. clean taxonomy
tax_table %>% rownames_to_column("asv") %>%   
  dplyr::select("asv", "Order") %>% 
  # rename the unclassified taxa
  replace_na(list(Order ='Unclassified' )) -> tax_order


# 2. manipulate data
asv_table %>% 
  rownames_to_column("asv") %>% 
  pivot_longer(cols = -asv, names_to = "samples", values_to = "abundances") %>%
  
  # connect to the order information  
  left_join(., tax_order, by = "asv") %>% 
  dplyr::select("samples", "Order", "abundances") %>% 
  group_by(samples, Order) %>% 
  
  # sum up the abundance within each order (diff sp/taxa)  
  summarise(abund = sum(abundances)) %>% 
  ungroup() %>% 
  
  # remove the samples where abundance is 0
  dplyr::filter(abund != "0") %>% 
  
  # connect the asv data to the metadata (temperature)
  left_join(., metadata[,c("sample_season", "Temp")], 
            by = c("samples" = "sample_season") ) %>% 
  
  # remove the samples that do not have methane measures  
  dplyr::filter(Temp != "NA") %>% 
  
  # filter only the orders which are present in 40+ samples  
  group_by(Order) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  dplyr::filter(n>40)  -> data_for_temp
# this is now 157 orders  


# look at correlations btw order abundances and temperature   
data_for_temp %>% 
  group_by(Order) %>% 
  cor_test(Temp, abund, method = "spearman") %>% 
  ungroup() %>% 
  # choose only significant
  dplyr::filter(p<0.05) %>% 
  # remove unclassified  
  dplyr::filter(Order != "Unclassified") %>% 
  dplyr::select("Order")  -> corr_orders
#  this dataset now contains only the orders which exhibit sign corr.


data_for_temp [which(data_for_temp$Order %in% corr_orders$Order),
               c("Order", "abund",  "Temp")] %>% 
  # plot 
  ggplot(aes(x = Temp , y = abund)) +
  geom_point(shape=21, size = 3, fill = "violet", alpha = 0.9, stroke = 0.5) + 
  geom_smooth(method = "lm", se= FALSE, color = "black") +
  facet_wrap(~ Order, scales = "free_y") + 
  stat_poly_eq(use_label(c( "adj.R2", "p")), 
               label.y = "top", small.r = TRUE,
               size = 3)+
  labs(y="Abundances (absolute 16S counts)",
       x= "Temperature [°C]")


# temperature related abundance - category ####
data_for_temp %>% dplyr::select(!c("n", "samples")) %>% 
  mutate(temp_cat = case_when(Temp <13 ~ "cold",
                              Temp >15 ~ "hot", 
                              Temp >= 13 & Temp <= 15 ~ "mid")) -> data_for_group


# look at anova btw order abundances and temperature groups 
data_for_group %>% 
  group_by(Order) %>% 
  anova_test(abund ~ temp_cat) %>% 
  ungroup() %>% 
  as.data.frame() %>%    
  # choose only significant
  dplyr::filter(p<0.05) %>% 
  dplyr::select("Order")  -> corr_orders_group
#  this dataset now contains only the orders which exhibit sign diff.

data_for_group [which(data_for_group$Order %in% corr_orders_group$Order),
                c("Order", "abund",  "temp_cat")] %>% 
  # plot 
  ggplot(aes(x = temp_cat , y = abund)) +
  geom_jitter(width = 0.2, size = 3, shape=21, aes(fill = temp_cat) ) +
  geom_boxplot( alpha=0.5, outlier.shape = NA, aes(fill = temp_cat)) + 
  facet_wrap(~ Order, scales = "free_y") +
  scale_x_discrete(limits = c("cold", "mid", "hot")) +
  scale_fill_manual(values = c("lightblue", "#d7301f", "#fdbb84")) +
  theme(legend.position = "none") + 
  labs(y = "Abundances (absolute 16S counts)",
       x = "Temperature [°C]")

# test significance

data_for_group %>% 
  group_by(Order) %>% 
  tukey_hsd(abund ~ temp_cat) %>% 
  ungroup() %>% 
  as.data.frame() %>%    
  # choose only significant
  dplyr::filter(p.adj.signif != "ns")


