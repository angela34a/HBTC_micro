
# the DS dataset from EVA
Eva_master <- read_excel("C:/Users/Angela Cukusic/Desktop/Downloads/2022_Masterjoints.xlsx")
Eva_master$sample_season <- paste(Eva_master$Probennr, 
                                  "spring" , sep = "_")

# just spring DS data
main_sheet <- read.csv("C:/Users/Angela Cukusic/Desktop/Master thesis/data/main_sheet.csv")
main_sheet$sample_season <- paste(main_sheet$Sample.ID, 
                                  main_sheet$Season, sep = "_")
data_DS <- left_join(Eva_master, main_sheet, by = "sample_season") %>% 
  dplyr::filter(grepl('DS', Geol_beb)) 
data_DS <- ens2 %>% rownames_to_column("JMF.ID") %>% right_join(., data_DS, by ="JMF.ID")




add_data3 <- read_excel("C:/Users/Angela Cukusic/Desktop/Downloads/2022_01_31_Daten_Andela(1).xlsx")
data_DS <- add_data1[, c("Sample_ID", "Name_HBC")] %>% 
  full_join(., add_data3,  by = "Name_HBC") %>% 
  dplyr::select(!"Name_HBC") %>% left_join(data_DS , . , 
                                           by = c("Sample.ID" = "Sample_ID"),
                                           relationship = "many-to-many") 
data_DS["heatsources_UHI"] <- lapply(data_DS["heatsources_UHI"], as.factor) 
data_DS["heatsources_GWWP"] <- lapply(data_DS["heatsources_GWWP"], as.factor) 
data_DS["heatsources_FW"] <- lapply(data_DS["heatsources_FW"], as.factor) 
data_DS["heatsources_Versie"] <- lapply(data_DS["heatsources_Versie"], as.factor) 
data_DS["heatsources_count"] <- lapply(data_DS["heatsources_count"], as.factor) 
data_DS["OF_beb"] <- lapply(data_DS["OF_beb"], as.factor) 

#add it to new_metadata
new_metadata <- add_data1[, c("Sample_ID", "Name_HBC")] %>% 
  full_join(., add_data3,  by = "Name_HBC") %>% 
  dplyr::select(!"Name_HBC") %>% left_join(new_metadata , . , 
                                           by = c("Sample.ID.x" = "Sample_ID"),
                                           relationship = "many-to-many") 
new_metadata["heatsources_UHI"] <- lapply(new_metadata["heatsources_UHI"], as.factor) 
new_metadata["heatsources_GWWP"] <- lapply(new_metadata["heatsources_GWWP"], as.factor) 
new_metadata["heatsources_FW"] <- lapply(new_metadata["heatsources_FW"], as.factor) 
new_metadata["heatsources_Versie"] <- lapply(new_metadata["heatsources_Versie"], as.factor) 
new_metadata["heatsources_count"] <- lapply(new_metadata["heatsources_count"], as.factor) 



#just for spring


#combining eva master to shannon needs the HBT name to JMF
#of_beb

# add HBT_name to the Sample ID
new_metadata <- add_data1[, c("Sample_ID", "Name_HBC")] %>% 
  left_join(new_metadata, .,  by = c("Sample.ID.x" = "Sample_ID"))

full_data1  <-   ens2 %>% 
  rownames_to_column("JMF.ID") %>% 
  left_join(new_metadata,. , by = "JMF.ID") %>% 
  dplyr::select("Name_HBC"  , "Shannon")


Eva_master <- read_excel("C:/Users/Angela Cukusic/Desktop/Downloads/2022_Masterjoints.xlsx")



data <- full_data1 %>% 
  right_join(., Eva_master,  by = "Name_HBC") %>% 
  dplyr::filter(grepl('DS', Geol_beb)) 
data$OF_beb[data$OF_beb == 3] <- 0 #changing the 3s to 0
data["OF_beb"] <- lapply(data["OF_beb"], as.factor) 

split_labels <- str_wrap(c("no effect", "running water effect", "stagnant water effect"), width = 8)

p1 <- data %>% dplyr::filter(OF_beb != "NA") %>% 
  ggplot(aes(x=OF_beb, y=Shannon)) + geom_boxplot(fill = "gray", alpha=0.5) +
  theme_bw() + labs(x="Surface water impact", y= "Shannons diversity index") +
  scale_x_discrete(labels = split_labels)



shapiro.test (data_for_alpha2[which(data_for_alpha2$GWWP == "1"),"ens"] ) # norm
shapiro.test ( data_for_alpha2[which(data_for_alpha2$GWWP == "0"),"ens"]) # norm
bartlett.test(ens ~ GWWP, data= data_for_alpha2) # homo
summary( aov(ens ~ GWWP, data= data_for_alpha2) )




#wastewater
data["wastew_beb"] <- lapply(data["wastew_beb"], as.factor) 

p2 <- data %>% 
  #dplyr::filter(OF_beb != "NA") %>% 
  ggplot(aes(x=wastew_beb, y=Shannon)) + geom_boxplot(fill = "gray", alpha=0.5) +
  theme_bw() + labs(x="Waste water impact", y= "Shannons diversity index") +
  scale_x_discrete(labels = c("no effect", "effect present", "unclear"))

p1 + p2

#redox
data["Redox_beb"] <- lapply(data["Redox_beb"], as.factor) 

split_labels <- str_wrap(c("o2 > 2", "o2 < 2", "Mn>0.15 & Fe>0.15", "s>0.1"), width = 8)

p3 <- data %>% 
  dplyr::filter(Redox_beb != "NA") %>% 
  ggplot(aes(x=Redox_beb, y=Shannon)) + geom_boxplot(fill = "gray", alpha=0.5) +
  theme_bw() + labs(x="Redox cascade", y= "Shannons diversity index") +
  scale_x_discrete(labels = split_labels )



p1 + p2 +p3




data2 <- left_join(data, new_metadata[which(new_metadata$Season == "spring"),c("JMF.ID", "Name_HBC")], by = "Name_HBC")
unique_values  <-  unique(data2$JMF.ID)
data3 <- data2[!duplicated(data2$JMF.ID,  fromLast = TRUE), ] 
data3 <- data3[which(data3$JMF.ID != "NA"), ] 

data3[order(data3$JMF.ID), ]

rownames(data3) <- data3$JMF.ID
asv_ds_spring <- srs_asv_all[, which(colnames(srs_asv_all) %in% rownames(data3))]


#categorise by temperature groups
data3$Cat3 <- cut(data3$T_cali, 
                  breaks=c(0, 10, 12, 14, 16, 18, 20, 30), 
                  labels=c('<10', '10-12', '12-14', '14-16', 
                           '16-18', '18-20', '20<'))
# find reference samples (the one in Marchfeld)
reference <- new_metadata %>% 
  filter(sample_season %in% c("22-246_fall", "EM10_spring",  "EM62_spring", "22-52_fall", 
                              "22-52_spring", "22.7/2_spring", "22.7/2_fall"))

# distinguish reference samples from the 10-12C
data3 <- data3 %>%
  mutate( reference = case_when(JMF.ID %in% reference$JMF.ID ~ "referential", 
                                TRUE ~ "other")) 
data3$ref_cat <- paste(data3$Cat3, data3$reference, sep="_")
data3 <- data3 %>% 
  mutate( category = case_when(ref_cat == "<10_other" ~ "<10",
                               ref_cat == "10-12_other" ~ "10-12",
                               ref_cat == "12-14_other" ~ "12-14",
                               ref_cat == "14-16_other" ~ "14-16",
                               ref_cat == "16-18_other" ~ "16-18",
                               ref_cat == "18-20_other" ~ "18-20",
                               ref_cat == "20<_other" ~ "20<",
                               TRUE ~ "ref_cat")) %>% 
  dplyr::select(!c("Cat3" , "reference" ,    "ref_cat"))
#at this point 281 samples







ps_all <- phyloseq(otu_table(as.matrix(asv_ds_spring), taxa_are_rows=TRUE), 
                   tax_table(as.matrix(new_taxonomy) ) , 
                   sample_data(as.data.frame(data3)) )

ps_all <- prune_taxa(taxa_sums(ps_all) > 0, ps_all)


# ordination


# perform ordination
unconstrained_aitchison_pca <- ps_all %>% tax_fix() %>% 
  tax_filter(min_prevalence = 0.1, tax_level = "Genus") %>%
  tax_agg("Family") %>%
  tax_transform("clr") %>%
  ord_calc()
#ord_calc will automatically infer you want a "PCA" here
#specify explicitly with method = "PCA", or you can pick another method

# create plot
pca_plot <- unconstrained_aitchison_pca %>%
  ord_plot(
    plot_taxa = 1:6, colour = "bmi_group", size = 1.5,
    tax_vec_length = 0.325,
    tax_lab_style = tax_lab_style(max_angle = 90, aspect_ratio = 0.5),
    auto_caption = 8)

# customise plot
customised_plot <- pca_plot +
  stat_ellipse(aes(linetype = bmi_group, colour = bmi_group), linewidth = 0.3) + # linewidth not size, since ggplot 3.4.0
  scale_colour_brewer(palette = "Set1") +
  theme(legend.position = "bottom") +
  coord_fixed(ratio = 0.5, clip = "off") # makes rotated labels align correctly

# show plot
customised_plot







data_for_reg <- cbind(new_metadata) %>% dplyr::select(where(is.numeric)) %>% 
  scale(center = T, scale = T) %>% cbind(., richness, ens) %>% data.frame() %>% 
  dplyr::select(! c("DIC", "SO4", "Cl", "Na", "Ca", "Mg", "Lf", "richness",
                    "NO3", "NH3", "Fe", "X", "Y", "DOM_bix") )


nullModel = lm(ens ~ 1, data=data_for_reg) # model with the intercept only
fullModel = lm(ens ~ ., data=data_for_reg) # model with all  variables

#choose
step.model<-stepAIC(fullModel, # start with a model containing no variables
                    direction = 'both', # run forward selection
                    trace = 0) # dont show step-by-step process of model selection
summary(step.model) # only temp significant
RsquareAdj(step.model) # 0.04038098 -> 4 %
summary(lm(ens ~ Temp, data = data_for_reg)) 


### other models ####


# categorical variables
new_metadata %>% dplyr::select(!where(is.numeric)) 

# turn NAs from OF_beb into 0
new_metadata["OF_beb"] <- lapply(new_metadata["OF_beb"], as.character)
# Replace NA with 0
new_metadata[is.na(new_metadata)] <- 0 
# Change character columns back to factors
new_metadata["OF_beb"] <- lapply(new_metadata["OF_beb"], as.factor) 

# another thing needs fixing:
# add data about heat sources i forgot before
add_data3 <- read_excel("~/Desktop/DS_analysis/data/heat_sources.xlsx")
new_metadata <- add_data1[, c("Sample_ID", "Name_HBC")] %>% 
  full_join(., add_data3,  by = "Name_HBC") %>% 
  dplyr::select(!"Name_HBC") %>% left_join(new_metadata, . , 
                                           by = c("Sample_ID.x" = "Sample_ID") ) 
new_metadata["heatsources_UHI"] <- lapply(new_metadata["heatsources_UHI"], as.factor) 
new_metadata["heatsources_GWWP"] <- lapply(new_metadata["heatsources_GWWP"], as.factor) 
new_metadata["heatsources_FW"] <- lapply(new_metadata["heatsources_FW"], as.factor) 
new_metadata["heatsources_Versie"] <- lapply(new_metadata["heatsources_Versie"], as.factor) 
new_metadata["heatsources_count"] <- lapply(new_metadata["heatsources_count"], as.factor) 

boxplot(richness ~ new_metadata$Geol_beb)
boxplot(richness ~ new_metadata$heatsources_UHI) # according to how many heat sources impact well
summary(aov(richness ~ category*Land.use*season*Geol_beb*UHI*heatsources_UHI, data=new_metadata) ) 
# category:Geol_beb:heatsources_UHI interaction significant
#summary(aov(richness ~ category:Geol_beb:heatsources_UHI, data=new_metadata) )
#fit3 <- lm(richness ~ category:Geol_beb:heatsources_UHI, data=new_metadata) 
#summary(fit3 ) # not sign


# plot interactions
#cat_plot(fit3, pred = Geol_beb, modx = heatsources_UHI, geom = "line")



# the same but for ens
#boxplot(ens ~ new_metadata$Geol_beb)
#boxplot(ens ~ new_metadata$UHI)
#summary(aov(ens ~ category*Land.use*season*Geol_beb*UHI, data=new_metadata) ) 
# category + UHI + Land.use:UHI significant
#model_ens <- aov(ens ~ category + UHI + Land.use:UHI, data=new_metadata) 
#summary(model_ens)
# i should do a kruskal wallis?




