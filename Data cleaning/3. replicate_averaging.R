# Objective: averaging the counts from the replicates of one well
# when a well had enough microbial load for more filters replicates were used


#it should be easy to do this with phyloseq BUT it is not!
# ps_all <- merge_samples(ps_all, "sample_season", fun = mean)
# it eather a: changes all the factors to numeric 
# and it is not so easy to revert the categories 
# (i.e. each aquifer is now assigned a number)
# or b) it is straigh up just NAs



# this way becomputationally long, but for now I do not have a more efficient alternative:


asv_no_rep <- asv_all_no_cont %>%
  rownames_to_column("ASV") %>%
  # pivot longer so you can summarize and add the sample_season identifier
  # which is the same for the replicates of the same well
  pivot_longer(cols= - ASV, names_to = "samples", 
               values_to = "abundance") %>%
  left_join(., main_sheet[,c("JMF_ID", "sample_season")], 
            by=c("samples"="JMF_ID") ) %>%
  dplyr::select("ASV", "abundance", "sample_season") %>%
  # apply function (summary) for each individual ASV per sample_season
  group_by(ASV, sample_season) %>%
  # find count means of samples of the same well (same sample_season)
  summarise(abundance = mean(abundance)) %>% 
  ungroup() %>%  
  pivot_wider(names_from = "sample_season", 
              values_from = "abundance") %>% 
  column_to_rownames(var="ASV")


rm(asv_all_no_cont)

# now that the replicates are removed from the count table
# and that colnames are the real sample names and not sequence codes
# we can integrate environmental data based on these colnames

master_data <- colnames(asv_no_rep) %>% as.data.frame() %>% 
               rename("sample_season" = ".") %>% 
               inner_join(., fall_data, spring_data, 
               by="sample_season")




# remove to keep environment clearer
rm(fall_data, spring_data, main_sheet)


# inspect the data to see if everything is fine
read_data <- asv_no_rep %>% 
  colSums() %>% # how many reads per sample (find sums per each column)
  as.data.frame() %>% 
  rename("reads" = ".") 

summary(read_data)
sd(read_data$reads)
# 5245  +- 3037

rm(read_data)

