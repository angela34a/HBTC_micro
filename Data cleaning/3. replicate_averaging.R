
#it should be easy to do this with phyloseq BUT it is not!
# ps_all <- merge_samples(ps_all, "sample_season", fun = mean)
# it eather a: changes all the factors to numeric and it is not so easy to revert
# the categories (i.e. each aquifer is now assigned a number)
# or b) it is straigh up just NAs



# this way is taxing and long, but for now I do not have a better alternative:

## a) metadata without replicates ------------------------
# remove the duplicate env data for replicates of the same sample:
# first see which wells have replicates 
mt_no_rep <- main_sheet %>%
  # main_sheet has JMF to sample name identifier
  filter(!grepl('Rep2|Rep3|Rep4', Sample.ID.with.replicates))
# remove all the names of replicates only keeping Rep1

# keep in metadata the well measurement only for the Rep1
new_metadata <- new_metadata[which(new_metadata$JMF.ID %in% mt_no_rep$JMF.ID),] 


## b) asv_table replicates ---------------------------
# now average Replicates by mean, and keep only the Rep1 JMFs to coincide with the metadata

# b.1
asv_ohne_rep <- asv_all_no_cont %>%
  rownames_to_column("ASV") %>%
  # pivot longer so you can summarize and add metadata (sample_season identifier)
  pivot_longer(cols= - ASV, names_to = "samples", values_to = "abundance") %>%
  left_join(., new_metadata, by=c("samples"="JMF.ID") ) %>%
  dplyr::select("ASV", "abundance", "sample_season") %>%
  # apply function (summary) for each individual ASV per sample_season
  # replicates of same well have the same sample_season 
  group_by(ASV, sample_season) %>%
  # find abund means of samples of the same well (same sample_season)
  summarise(abundance = mean(abundance)) %>% 
  ungroup()

# b.2
asv_all_no_rep <- main_sheet %>%  # keep just the Rep1
  filter(!grepl('Rep2|Rep3|Rep4', Sample.ID.with.replicates))%>%
  dplyr::select("sample_season","JMF.ID") %>%
  right_join(., asv_ohne_rep, by="sample_season") %>%
  dplyr::select(!"sample_season")

# order it from JMF-001 to JMF-410
asv_all_no_rep <- asv_all_no_rep[order(asv_all_no_rep$JMF.ID),] 
# easier to order it before transferring back to wide format than ordering 
## column names

# back to wide format
asv_table_final <- asv_all_no_rep %>%  
  pivot_wider(names_from = "JMF.ID", values_from = "abundance") %>%
  column_to_rownames(var="ASV")



# did any samples get lost 
new_metadata[which(!new_metadata$JMF.ID %in% colnames(asv_table_final)), ] 
asv_table_final <- asv_table_final[,which(colnames(asv_table_final) %in% rownames(new_metadata))] 

# remove to keep environment clearer
rm(mt_no_rep, main_sheet, asv_ohne_rep)



read_data <- asv_table_final %>% 
  colSums() %>% # how many reads per sample (find sums per each column)
  as.data.frame() %>% 
  rownames_to_column("JMF.sample.ID") %>% 
  rename("reads" = ".") 

summary(read_data)
sd(read_data$reads)
