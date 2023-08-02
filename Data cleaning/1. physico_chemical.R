# Objective: combining fragmented data into one metadata
# each campaign has its dataset, and each of them has 4 sheets


# 1. Loading in ####

# Spring campaign
# chemical info
spring_chem <- read_excel("C:/Users/Angela Cukusic/Desktop/DS_analysis/data/HBTC_mt_spring.xlsx")

# sampling info
spring_sampl <- read_excel("C:/Users/Angela Cukusic/Desktop/DS_analysis/data/HBTC_mt_spring.xlsx", 
                           sheet = "Sampling_info")

# ATP measurement
spring_ATP <-  read_excel("C:/Users/Angela Cukusic/Desktop/DS_analysis/data/HBTC_mt_spring.xlsx", 
                          sheet = "ATP")

# cell count measurement
spring_TCC <-  read_excel("C:/Users/Angela Cukusic/Desktop/DS_analysis/data/HBTC_mt_spring.xlsx", 
                          sheet = "TCC") 

# coordinates
spring_coord <- read_excel("C:/Users/Angela Cukusic/Desktop/DS_analysis/data/HBTC_mt_spring.xlsx", 
                            sheet = "Coordinates", col_types = c("text", "numeric", "numeric"))

# for spring there is also the heavy metal data and heatsources data
spring_hts <- read_excel("C:/Users/Angela Cukusic/Desktop/DS_analysis/data/HBTC_mt_spring.xlsx", 
                             sheet = "Heat_sources", col_types = c("text", "text", "text", 
                              "text", "text", "text", "text", "text", "text", "text",
                              "text", "text", "text", "text"))

spring_metals <- read_excel("C:/Users/Angela Cukusic/Desktop/DS_analysis/data/HBTC_mt_spring.xlsx",
                            sheet = "Heavy_metals")




# fall campaign
# chemical info
fall_chem <- read_excel("C:/Users/Angela Cukusic//Desktop/DS_analysis/data/HBTC_mt_autumn.xlsx", 
                        sheet = "Chemistry")

# sampling info
fall_sampl <- read_excel("C:/Users/Angela Cukusic/Desktop/DS_analysis/data/HBTC_mt_autumn.xlsx", 
                         sheet = "Sampling_info")

# ATP measurement
fall_ATP <-   read_excel("C:/Users/Angela Cukusic/Desktop/DS_analysis/data/HBTC_mt_autumn.xlsx",  
                         sheet = "ATP")

# cell count measurement
fall_TCC <-   read_excel("C:/Users/Angela Cukusic/Desktop/DS_analysis/data/HBTC_mt_autumn.xlsx", 
                         sheet = "TCC")


# coordinates
fall_coord <- read_excel("C:/Users/Angela Cukusic/Desktop/DS_analysis/data/HBTC_mt_autumn.xlsx", 
                           sheet = "Coordinates", col_types = c("text", "numeric", "numeric"))







# 2. combining sheets ####

spring_data <- full_join(spring_chem, spring_sampl, by="Sample_ID") %>% 
  full_join(.,spring_ATP, by="Sample_ID")  %>% 
  full_join(.,spring_TCC, by="Sample_ID") %>% 
  full_join(., spring_coord, by="Sample_ID") 

# to explore specific UHI impact
spring_data_additional <- spring_data %>% 
  full_join(., spring_hts, by="Sample_ID") %>% 
  full_join(., spring_metals, by="Sample_ID")
  

fall_data <- full_join(fall_chem, fall_sampl, by="Sample_ID") %>% 
  full_join(.,fall_ATP, by="Sample_ID")  %>% 
  full_join(.,fall_TCC, by="Sample_ID") %>% 
  full_join(., fall_coord, by="Sample_ID")  

rm(spring_chem, spring_sampl, spring_ATP, spring_TCC, spring_coord, spring_hts, spring_metals,
   fall_chem, fall_sampl, fall_ATP, fall_TCC, fall_coord)



# 3. adding identifier column ####
# unique identifier, to differentiate spring and fall wells with same name
spring_data$sample_season <- paste(spring_data$Sample_ID, "spring", sep = "_")
fall_data$sample_season <- paste(fall_data$Sample_ID, "fall", sep = "_")




# 4. connecting the sequence codes ####
# main sheet = sheet with sequence id and blank data

# should be done after decontamination and blank averaging, so to remove blanks from the main_sheet


# but for now just add the identifier column in the dataset

main_sheet <- read.csv("C:/Users/Angela Cukusic/Desktop/DS_analysis/data/main_sheet.csv")
main_sheet$sample_season <- paste(main_sheet$Sample_ID, 
                                  main_sheet$Season, sep = "_")


