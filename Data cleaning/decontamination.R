# 2. load asv_table ####
asv_table <- read.delim("C:/Users/Angela Cukusic/Desktop/DS_analysis/data/DADA2_counts_as_matrix.tsv", row.names=1)
names(asv_table)<- sub("^JMF.2207.07.0","JMF-2207-07-0", names(asv_table))
names(asv_table) <- sub("A", "", names(asv_table))


# load taxonomy ####
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



# load the blank_sheet which has info on blank batches
blanks_df <- read.delim("C:/Users/Angela Cukusic/Desktop/DS_analysis/data/blanks.csv",
                        header=TRUE, sep = ",")


# i have one blank which was not sequenced so i removed the whole batch :(
## 12 samples - not optimal!
blanks_all <-  blanks_df %>% filter(batch_name !="Blank_26")
# remove the 12 samples from the asv_table as well
asv_all <- asv_table[ , which(colnames(asv_table) %in% blanks_all$JMF.sample.ID)]  



# see if all samples match from blank_df to asv_table - are all the JMFs 
## from the blank table also samples from the asv_table
blanks_all[which(!  blanks_all$JMF.sample.ID  %in% colnames(asv_all)), ]
# they dont match (JMF-2207-07-0282 was sequenced but not measured for env. data)
## remove it
blanks_all <- blanks_all %>% filter(JMF.sample.ID != "JMF-2207-07-0282") 


# (before using decontam it is important that rows are ordered in 
## the same order as columns of asv_table!)
## in our case they are because the JMF.ID goes up by number


# STEP 1: make vector for blanks 
vector_for_decontam <- print(blanks_all$sample_blank=="blank") 
# STEP 2: identify
contam_df <- isContaminant(t(asv_all), 
                           neg=vector_for_decontam, 
                           batch = blanks_all$batch_name)




# identified 5763  as contaminants
table(contam_df$contaminant) # to see how many are contaminants
# name the contaminants
contam_asvs <- row.names(contam_df[contam_df$contaminant == TRUE, ])
contam_asvs <- contam_asvs %>% as.data.frame() 
# find the taxonomy of the contaminats
cont_identified <- asv_taxonomy %>% rownames_to_column(".") %>% 
  left_join (contam_asvs, ., by=".") 
table(cont_identified$Class) %>% as.data.frame() %>%  arrange(desc(Freq)) %>% head()
# the most frequent contamints are Nanoarchaea


# Step 3: remove
# remove asvs which are contaminants from the asv_table
asv_all_no_cont <- asv_all[!row.names(asv_all) %in% contam_asvs, ]
# removes blanks samples (JMFs) from the asv_table
asv_all_no_cont <- asv_all_no_cont[ , !vector_for_decontam]
# removes contaminants from taxonomy table
new_taxonomy <- asv_taxonomy[!row.names(asv_taxonomy) %in% contam_asvs, ]


# Step 4: clean the rest 
# be sure that metadata has the same samples as the final asv table
rownames(metadata) <- metadata$JMF.ID
new_metadata <- metadata[which(rownames(metadata) %in% colnames(asv_all_no_cont) ),]
#and that all the samples in asv_table are in the metadata as well
asv_all_no_cont <- asv_all_no_cont[, which( colnames(asv_all_no_cont)  %in%  rownames(new_metadata))]


rm(blanks_all, blanks_df, contam_asvs, asv_taxonomy, asv_table)