# Objective: adding needed categorizations
# a) temperature categories
# b) reference samples


library(vegan)

# temperature categories ####
master_data$temp_cat <- cut(master_data$Temp, 
                         breaks=c(0, 10, 12, 14, 16, 18, 20, 30), 
                         labels=c('<10', '10-12', '12-14', '14-16', 
                                  '16-18', '18-20', '20<'))

# reference samples ####
# the samples located in Marchfeld are a part of this category
reference <- master_data %>% 
  filter(sample_season %in% 
           c("22-246_fall", 
             "EM10_spring",  
             "EM62_spring", 
             "22-52_fall",  
             "22-52_spring", 
             "22.7/2_spring", 
             "22.7/2_fall")) 



# distinguish reference samples from the 10-12C samples
# designate which samples are referential
master_data <- master_data %>%
  mutate( reference = case_when(sample_season %in% 
                                reference$sample_season ~ "referential", 
                                TRUE ~ "other")) 

# make new column with both temp_cat info and reference info
master_data$ref_cat <- paste(master_data$temp_cat, 
                             master_data$reference, 
                              sep="_")

# make all the samples with "_referential" a reference sample ("ref_cat")
# regardless of temperature
master_data <- master_data %>% 
  mutate( category = case_when(ref_cat == "<10_other" ~ "<10",
                               ref_cat == "10-12_other" ~ "10-12",
                               ref_cat == "12-14_other" ~ "12-14",
                               ref_cat == "14-16_other" ~ "14-16",
                               ref_cat == "16-18_other" ~ "16-18",
                               ref_cat == "18-20_other" ~ "18-20",
                               ref_cat == "20<_other" ~ "20<",
                               grepl("referential", ref_cat)  ~ "ref_cat")) %>% 
  # remove these temporary categories from the master data
  dplyr::select(!c("temp_cat" , "reference" ,    "ref_cat"))


# sanity check since temperature is a point of focus in study
master_data$Temp %>% summary()
# certian measurements go above 700 -> make those NAs instead
master_data <- master_data %>% 
  mutate_at(vars(Temp), ~ ifelse(. > 40, NA, .)) 


# rarefaction ####
# now that the master_data is clean and categorised
# and contaminants are removed from asv and tax table
# we should remove the samples with low sequence read



# before rarefaction
# inspect the data to see the average read number
read_data <- asv_no_rep %>% 
  colSums() %>% # how many reads per sample (find sums per each column)
  as.data.frame() %>% 
  rename("reads" = ".") 

summary(read_data)
sd(read_data$reads)
# 6034 +- 4249

rm(read_data)



# rarefaction ####
# remove those that have less than 2000
asv_table_final <- asv_no_rep[, colSums(asv_no_rep) >= 2000] 
# subsample those with more than 2000 to this limit
asv_table_final <- rrarefy(t(asv_table_final), sample = 2000) %>% 
                   t() %>% as.data.frame()
# remove from master data >2000 samples
master_data <- master_data[which(master_data$sample_season %in% colnames(asv_table_final)  ),]
# 4 additional samples in the asv_table but not metadata
# these are the sequenced samples not connected to any well name
# remove
asv_table_final <- asv_table_final[, which(colnames(asv_table_final)  %in%  master_data$sample_season)]


# write out:
# a) final asv_table
write.csv(asv_table_final, 
          "C:/Users/Angela Cukusic/Desktop/DS_analysis/data/clean_data/asv_table_final.csv")
# b) final taxonomy table
write.csv(tax_no_cont, 
          "C:/Users/Angela Cukusic/Desktop/DS_analysis/data/clean_data/tax_no_cont.csv")
# c) final metadata
write.csv(master_data, 
          "C:/Users/Angela Cukusic/Desktop/DS_analysis/data/clean_data/master_data.csv")
# d) additional spring data
write.csv(spring_data_additional,
          "C:/Users/Angela Cukusic/Desktop/DS_analysis/data/clean_data/spring_data_additional.csv")

rm(reference, asv_no_rep, asv_table_final, tax_no_cont, master_data)
