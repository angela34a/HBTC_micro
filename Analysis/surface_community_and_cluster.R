# surface ####

metadata_surf <- read.csv("C:/Users/Angela Cukusic/Desktop/DS_analysis/data/clean_data/master_data.csv") %>% 
  dplyr::select(-"X.1") %>% 
  dplyr::filter(Type == "SW")

metadata_surf %>% ncol() # it is 30 samples


# since looking into the surface communities came as an aftertought, 
# so these samples were removed from the asv_table
# i must go back to importing the main sheet

# find JMF ID just for the surface samples
main_sheet <- read.csv("C:/Users/Angela Cukusic/Desktop/DS_analysis/data/main_sheet.csv")
main_sheet$sample_season <- paste(main_sheet$Sample_ID, 
                                  main_sheet$Season, sep = "_")
main_sheet %>% 
  dplyr::filter(sample_season %in% metadata_surf$sample_season) %>% 
  dplyr::select("JMF_ID", "sample_season") -> JMF_data
  

# subset the asv table to just the surface ones
asv_table_surf <- read.delim("C:/Users/Angela Cukusic/Desktop/DS_analysis/data/DADA2_counts_as_matrix.tsv", row.names=1)
# rename so it matches main_sheet
names(asv_table_surf)<- sub("^JMF.2207.07.0","JMF-2207-07-0", names(asv_table_surf))
# remove unnecessary suffix
names(asv_table_surf) <- sub("A", "", names(asv_table_surf))


# total of 24 samples (includes the replicates!)
asv_table_surf %>% 
  rownames_to_column("ASV") %>%
  # pivot longer so you can summarize and add the sample_season identifier
  # which is the same for the replicates of the same well
  pivot_longer(cols= - ASV, names_to = "samples", 
               values_to = "abundance") %>%
  inner_join(., JMF_data, 
            by=c("samples"="JMF_ID") ) %>%
  dplyr::select("ASV", "abundance", "sample_season") %>%
  # apply function (summary) for each individual ASV per sample_season
  group_by(ASV, sample_season) %>%
  # find count sums of samples of the same well (same sample_season)
  summarise(abundance = sum(abundance)) %>% 
  ungroup() %>%  
  pivot_wider(names_from = "sample_season", 
              values_from = "abundance") %>% 
  column_to_rownames(var="ASV") -> surf_no_rep






# add raw tax table as well
asv_taxonomy <-
  read.csv("C:/Users/Angela Cukusic/Desktop/DS_analysis/data/DADA2_ASVs.rRNA_SSU.SILVA_classified.csv")
asv_taxonomy <- asv_taxonomy %>% dplyr::select("name", "lca_tax_slv") %>%
  separate_wider_delim(lca_tax_slv, delim = ";", 
                       # too few can be c("error", "debug", "align_start") and
                       # means there is  less taxa names (NAs/not classified) 
                       too_few = "align_start", 
                       # too many can be c("error", "debug", "drop") and 
                       # means there is more taxa names that levels (ssp.)
                       too_many = "drop",
                       names = c("Kingdom", "Phylum", "Class", "Order", 
                                 "Family", "Genus", "Species")) %>% 
  column_to_rownames("name")
# with the separate_wider_column the first NA in line end up just empty
## so make that an NA as well:
asv_taxonomy[asv_taxonomy == ''] <- NA


asv_table_surf %>% 
  mutate_if(is.integer, as.numeric) %>% 
  mutate(class = asv_taxonomy$Class) %>%
  pivot_longer(cols = -class, values_to = "abundance", names_to = "sample") %>%

  # to make NA classes Unclassified   
  replace_na(list(class ='Unclassified' )) %>%
  mutate(class = as.factor(class)) %>%  
  
  # to find relative abundance 
  mutate (rel_abund = abundance / sum(abundance)) %>%
  
  # to sum up all the counts of one class from all the samples  
  dplyr::group_by(class) %>%
  summarise(rel_abund = sum(rel_abund)) %>%
  ungroup() %>%
  
  # remove the ones not present in the surface samples
  dplyr::filter(rel_abund != 0) %>% 
  
  # remove the unclassified ones
  dplyr::filter(class != "Unclassified") %>% 
  
  # remove the less than 1% abundant
  dplyr::filter(rel_abund > 0.01) %>% 

  
  # plot
  ggplot(aes(x = "", y = rel_abund, fill = reorder(class, rel_abund))) + 
  geom_bar(stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  
  geom_text(aes(label = ifelse(rel_abund > 0.04, paste0(round(rel_abund *100), "%"), "") ), 
            position = position_stack(vjust = 0.5), 
            size = 5) +
  
  scale_fill_manual(values=c25) +
  theme(panel.grid = element_blank(), 
        legend.position="right") + 
  labs(fill = "Class") +
  guides(fill=guide_legend(ncol = 1)) 

######################################################
c24<- c("#E31A1C", #alphaproteo
        "white",# Aminicantia
        "white", #Bacili
        "green4"     , # bac25
        "#6A3D9A", #becteroidia
          "skyblue2", #brocadia
        "blue1", # camplyo
        "white", # cloacimonadia
        "white", # clostridia
        "gold1" , # cyano
        "#FB9A99",#diverse
        "steelblue4", #gamma
        "palegreen2" ,  #gracilli
          "#FDBF6F",#micrarchaea
          "maroon",#nano
        "gray70"     ,#nitro
         "orchid1", #omni
         "deeppink1", #percur
        "white", # thermodesulfovibrionia
          "darkorange4", #unclassified
         "green1",  #vampir
         "yellow4"#verruc
           )



surf_no_rep %>% 
  mutate_if(is.integer, as.numeric) %>% 
  mutate(class = asv_taxonomy$Class) %>%
  pivot_longer(cols = -class, values_to = "abundance", names_to = "sample") %>%
  
  # to make NA classes Unclassified   
  replace_na(list(class ='Unclassified' )) %>%
  mutate(class = as.factor(class)) %>%  
  
  # remove the ones not present in the surface samples
  dplyr::filter(abundance != 0) %>% 
  
  # to find relative abundance within each category  
  dplyr::group_by(sample) %>%
  mutate (rel_abund = abundance / sum(abundance)) %>%
  ungroup() %>%
  # to sum up all the counts of one class from all the samples  
  dplyr::group_by(sample, class) %>%
  summarise(rel_abund = sum(rel_abund)) %>%
  ungroup() %>%
  # those very rare categorize as Diverse others 
  dplyr::group_by(sample, class) %>%
  mutate(class = case_when(rel_abund< 0.015~ "Diverse others", 
                           TRUE ~ class)) %>%
  ungroup() %>%
  # final regrouping after categorizing for Diverse others  
  dplyr::group_by(sample, class) %>%
  summarise(rel_abund = sum(rel_abund)) %>%
  ungroup() %>%
  
  # plot
  ggplot(aes(x = sample, y = rel_abund, fill = #reorder(
               class#, rel_abund)
             )) + 
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values=c24) +
  theme(panel.grid = element_blank(), legend.position="right",
        legend.text = element_text(size = 10),      # Adjust the text size
        legend.title = element_text(size = 10),
        legend.key.size = unit(0.4, "cm"),
        axis.title = element_text(size=11,face="bold"),
        axis.text.y = element_text(size=10 ),
        axis.text.x = element_text(angle = 90, size = 10, face = "bold")) + 
  labs(x = NULL,
       y = "Relative abundance", 
       fill = "Class") +
  guides(fill=guide_legend(ncol = 1)) +
  scale_x_discrete(limits = c("Alte Donau_spring", "Hirsch See_spring",  
                              "22-154_surf_spring", "Wienfluss_spring", 
                              "Donaukanal_spring", "KB6_surf_spring",   
                              "LE-12 Donau_spring" ,  "ND7 Donau_spring"), 
                   labels = c("Alte Donau", "Hirsch See",  
                              "22-154", "Wienfluss", 
                              "Donaukanal", "KB6",   
                              "LE-12 Donau" ,  "ND7 Donau"))  -> plot_surface


plot_gw + plot_surface + plot_layout(guides = "collect")



# with plot_gw being:

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
  ggplot(aes(x = as.factor(category), y = as.factor(class) )) + 
  geom_point(aes(size = rel_abund, fill=class ), 
             alpha = 0.8, shape = 21, stroke = 0.5) + 
  scale_size_continuous(limits = c(0.001, 0.4), range = c(1,7)) + 
  labs( x= "", y = "", size = "Relative Abundance", fill = "")  + 
  theme(legend.key=element_blank(), 
        axis.text.x = element_text(colour = "black", size = 10, face = "bold", 
                                   angle = 90, vjust = 0.3, hjust = 1),
        axis.text.y = element_text(colour = "black", face = "bold", size = 10), 
        legend.text = element_text(size = 10, face ="bold", colour ="black"), 
        legend.title = element_text(size = 10), 
        legend.position = "right") +  
  scale_fill_manual(values = c25, guide = "none") +   
  scale_y_discrete(limits = rev(levels(data$class))) +
  scale_x_discrete(limits = name_vector, 
                   labels = c("ref_cat", "<10",   "10-12",
                              "12-14", "14-16", "16-18", 
                              "18-20",    ">20"  )) -> plot_gw



# cluster ####


