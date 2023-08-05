# Objective: removing contaminants from the count data (asv_table)
library(decontam)

# 1. load asv_table ####
asv_table <- read.delim("C:/Users/Angela Cukusic/Desktop/DS_analysis/data/DADA2_counts_as_matrix.tsv", row.names=1)
# rename so it matches main_sheet
names(asv_table)<- sub("^JMF.2207.07.0","JMF-2207-07-0", names(asv_table))
# remove unnecessary suffix
names(asv_table) <- sub("A", "", names(asv_table))


# 2. load taxonomy ####
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



# 3. edit the main_sheet ####
# it  has info on blank batches 

# i have one blank which was not sequenced so i removed the whole batch 
## this is 12 samples - not optimal! 
## Take care when performing the lab work on the next project!
main_sheet <- main_sheet  %>% filter(batch_name !="Blank_26")

# remove the 12 samples from the asv_table as well
asv_table <- asv_table[ , which(colnames(asv_table) %in% main_sheet$JMF_ID)]  




# now see if all samples match from main_sheet to asv_table 
# and are all the main_sheet sequence IDs also samples from the asv_table
main_sheet[which(!  main_sheet$JMF_ID  %in% colnames(asv_table)), ]
# they dont match 
# JMF-2207-07-0282 was accidentally sequenced but not measured for env. data
# remove it
main_sheet <- main_sheet %>% filter(JMF_ID != "JMF-2207-07-0282") 





# 4. check order of samples ####
# before using decontam it is important that rows are ordered in 
# the same order as columns of asv_table!
# in our case they are because the JMF.ID goes up by number


# 5. decontam ####

##  STEP 5.1: make vector for blanks ####
vector_for_decontam <- print(main_sheet$Blank_Sample=="Blank") 
## STEP 5.2: identify  ####
contam_df <- isContaminant(t(asv_table), 
                           neg=vector_for_decontam, 
                           batch = main_sheet$batch_name)


# to see how many are contaminants
table(contam_df$contaminant) 
# identified 5763  as contaminants

# name the contaminants
contam_asvs <- row.names(contam_df[contam_df$contaminant == TRUE, ])
contam_asvs <- contam_asvs %>% as.data.frame() 
# find the taxonomy of the contaminats
cont_identified <- asv_taxonomy %>% 
                   rownames_to_column(".") %>% 
                   left_join (contam_asvs, ., by=".") 
table(cont_identified$Class) %>% 
  as.data.frame() %>%  
  arrange(desc(Freq)) %>% 
  head()
# the most frequent contamints are Nanoarchaea


## STEP 5.3: remove ####
# remove asvs which are contaminants from the asv_table
asv_all_no_cont <- asv_table[!row.names(asv_table) %in% contam_asvs$., ]
# removes blanks samples (JMFs) from the asv_table
asv_all_no_cont <- asv_all_no_cont[ , !vector_for_decontam]
# removes contaminants from taxonomy table
tax_no_cont <- asv_taxonomy[!row.names(asv_taxonomy) %in% contam_asvs$., ]


# 6. Blank removal ####
# Remove from the main_sheet sequence codes of blanks
main_sheet <- main_sheet %>% filter(Blank_Sample != "Blank")


rm(contam_df, cont_identified, contam_asvs, asv_taxonomy, asv_table, vector_for_decontam)

# we are now left with count data and taxonomy list without the contaminants
# in next script we average the replicates
# and only then have all three datasets clean and ready for analysis


