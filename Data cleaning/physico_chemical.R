

# each campaign has its dataset, and each of them has 4 sheets
## combine them

#SPRING XLSX
spring_chem <- read_excel("C:/Users/Angela Cukusic/Desktop/DS_analysis/data/HBTC_mt_spring.xlsx")
spring_sampl <- read_excel("C:/Users/Angela Cukusic/Desktop/DS_analysis/data/HBTC_mt_spring.xlsx", 
                           sheet = "Sampling Info")
spring_ATP <-  read_excel("C:/Users/Angela Cukusic/Desktop/DS_analysis/data/HBTC_mt_spring.xlsx", 
                          sheet = "ATP")
spring_TCC <-  read_excel("C:/Users/Angela Cukusic/Desktop/DS_analysis/data/HBTC_mt_spring.xlsx", 
                          sheet = "TCC") 

#FALL XLSX
fall_chem <- read_excel("C:/Users/Angela Cukusic//Desktop/DS_analysis/data/HBTC_mt_autumn.xlsx", 
                        sheet = "Chemistry")
fall_sampl <- read_excel("C:/Users/Angela Cukusic/Desktop/DS_analysis/data/HBTC_mt_autumn.xlsx", 
                         sheet = "Sampling Info")
fall_ATP <-   read_excel("C:/Users/Angela Cukusic/Desktop/DS_analysis/data/HBTC_mt_autumn.xlsx",  
                         sheet = "ATP")
fall_TCC <-   read_excel("C:/Users/Angela Cukusic/Desktop/DS_analysis/data/HBTC_mt_autumn.xlsx", 
                         sheet = "TCC")

# add this column as a unique identifier, to differentiate spring and fall wells
# add to each sheet in order to combine them in one dataset
spring_chem$sample_season <- paste(spring_chem$`Sample ID`, "spring", sep = "_")
spring_sampl$sample_season <- paste(spring_sampl$`Sample ID`, "spring", sep = "_")
spring_ATP$sample_season <- paste(spring_ATP$`Sample ID`,  "spring", sep = "_")
spring_TCC$sample_season <- paste(spring_TCC$`Sample ID`,"spring", sep = "_")

fall_chem$sample_season <- paste(fall_chem$`Sample ID`,  "fall", sep = "_")
fall_sampl$sample_season <- paste(fall_sampl$`Sample ID`,  "fall", sep = "_")
fall_ATP$sample_season <- paste(fall_ATP$`Sample ID`, "fall", sep = "_")
fall_TCC$sample_season <- paste(fall_TCC$`Sample ID`, "fall", sep = "_")



### sheets

# 1. chemical info
spring_chem <- spring_chem %>% dplyr::select("Sample ID", "sample_season", "DIC (mg/L)", 
                                             "DOC (mg/L)", "PO4 (µg/L)",  "SO4 (mg/L)" , 
                                             "Cl (mg/L)" , "Na (mg/L)", "K (mg/L)", 
                                             "Ca (mg/L)" , "Mg (mg/L)"  )
fall_chem <- fall_chem %>%  dplyr::select("Sample ID", "sample_season", "DIC (mg/L)", 
                                          "DOC (mg/L)", "PO4 (µg/L)", "SO4 (mg/L)", 
                                          "Cl (mg/L)", "Na (mg/L)", "K (mg/L)", 
                                          "Ca (mg/L)", "Mg (mg/L)" )

# 2. sampling info                                      
spring_sampl <- spring_sampl %>%  dplyr::select("sample_season", "Land use", "Date", "Rohr D dm",  
                                                "GW Table [m]", "Well depth [m]", 
                                                "Watercolumn [m]", "Extraction depth [m]", 
                                                "T_3 (C)", "Lf_3 (uS)", "O2_3 (mg)",  "pH_3")
fall_sampl <- fall_sampl %>%  dplyr::select("sample_season", "Land use" , "Date","Rohr D dm", 
                                            "GW Table [m]", "Well depth [m]", 
                                            "Watercolumn [m]", "Extraction depth [m]", 
                                            "T_3 (C)", "Lf_3 (uS)", "O2_3 (mg)",  "pH_3")


# 3. ATP info
spring_ATP <- spring_ATP %>%  rename("tot ATP (pM)" = "tot ATP (pM)...3" , 
                                     "ext ATP" = "ext ATP...5", 
                                     "int ATP" = "int ATP...8") %>% 
  dplyr::select("sample_season", "tot ATP (pM)", 
                "ext ATP", "int ATP")
fall_ATP <- fall_ATP %>%   dplyr::select("sample_season", "tot ATP (pM)", "ext ATP", 
                                         "corr. int ATP" ) %>%
  rename("int ATP" = "corr. int ATP")

# 4. TCC info
spring_TCC <- spring_TCC %>%  dplyr::select("sample_season", "TCC (MW, Cells/ml)") %>% 
  rename( "TCC" = "TCC (MW, Cells/ml)")
fall_TCC <- fall_TCC %>%  dplyr::select("sample_season",  "TCC (MW, Cells/ml)") %>% 
  rename("TCC" = "TCC (MW, Cells/ml)")

## join chemical, sampling info, ATP and TCC 
df1<- inner_join(spring_chem, spring_sampl, by="sample_season") %>% 
  left_join(.,spring_ATP, by="sample_season")  %>% 
  left_join(.,spring_TCC, by="sample_season") 

df2 <- inner_join(fall_chem, fall_sampl, by="sample_season") %>% 
  left_join(.,fall_ATP, by="sample_season")  %>% 
  left_join(.,fall_TCC, by="sample_season")  



#main sheet = data sheet with sample names and their JMF.id
main_sheet <- read.csv("C:/Users/Angela Cukusic/Desktop/DS_analysis/data/main_sheet.csv")
main_sheet$sample_season <- paste(main_sheet$Sample.ID, 
                                  main_sheet$Season, sep = "_")


# join sample info with its JMF.id
df1.2<- inner_join(main_sheet, df1, 
                   by=c('sample_season'='sample_season')) #185/187
df2.2 <- inner_join(main_sheet, df2, 
                    by=c('sample_season'='sample_season')) #183/163 (the 20 are reps?)
df3<- full_join(df1.2, df2.2, by = c("JMF.ID", "Sample ID", "sample_season", "Date", 
                                     "DIC (mg/L)",  "DOC (mg/L)", 
                                     "PO4 (µg/L)", "SO4 (mg/L)", "Cl (mg/L)",
                                     "Na (mg/L)", "K (mg/L)", "Ca (mg/L)", "Mg (mg/L)",
                                     "Land use", "Rohr D dm", "GW Table [m]",
                                     "Well depth [m]", "Watercolumn [m]", 
                                     "Extraction depth [m]", "T_3 (C)", "Lf_3 (uS)", 
                                     "O2_3 (mg)", "pH_3", "tot ATP (pM)", "ext ATP", 
                                     "int ATP", "TCC") )

metadata <- df3 
# order it from JMF-001 to JMF-410
metadata<-metadata[order(metadata$JMF.ID),] 
rownames(metadata) <- metadata$JMF.ID


metadata <- metadata %>% rename("Sample_ID" = "Sample ID",
                                "DIC" = "DIC (mg/L)", 
                                "DOC" = "DOC (mg/L)",
                                "PO4" = "PO4 (µg/L)",
                                "SO4" ="SO4 (mg/L)", 
                                "Cl"=  "Cl (mg/L)",  
                                "Na" = "Na (mg/L)", 
                                "K" ="K (mg/L)", 
                                "Ca" = "Ca (mg/L)", 
                                "Mg" = "Mg (mg/L)",  
                                "Land_use" = "Land use", 
                                "Ex_depth" =  "Extraction depth [m]", 
                                "Depth" = "Well depth [m]",
                                "Temp" =  "T_3 (C)", 
                                "Lf"=  "Lf_3 (uS)", 
                                "O2"= "O2_3 (mg)", 
                                "pH"="pH_3", 
                                "totATP" = "tot ATP (pM)", 
                                "extATP"= "ext ATP", 
                                "intATP" = "int ATP" ) %>% 
  dplyr::select("JMF.ID",  "sample_season", "Sample_ID", "Date",  "Land_use", "DIC", "DOC", "PO4", "SO4", 
                "Cl" , "Na", "K",  "Ca", "Mg", "Depth", "Temp", "Lf",  
                "O2", "pH", "intATP", "TCC")




rm(df1, df2, fall_ATP, fall_chem, fall_sampl, fall_TCC, spring_ATP, 
   spring_chem, spring_sampl, spring_TCC)
rm( df1.2, df2.2)


# 1. heavy metal data in fall dataset
add_data1 <- read_excel("C:/Users/Angela Cukusic/Desktop/DS_analysis/data/fall_data.xlsx")
add_data1 <- add_data1 %>% dplyr::select("Sample ID", "Probennr","UHI",  
                                         "HBTC_Kam_all_ug Fe/l","Name_HBC",     
                                         "HBTC_Kam_all_NO3 (Nitrat)", 
                                         "HBTC_Kam_all_NH4 (Ammonium)",  
                                         "DOM_bix") %>% 
  rename("Sample_ID" = "Sample ID",   
         "Fe" = "HBTC_Kam_all_ug Fe/l",     
         "NO3" = "HBTC_Kam_all_NO3 (Nitrat)",   
         "NH3" = "HBTC_Kam_all_NH4 (Ammonium)")
# add season-specific identifier
add_data1$sample_season <- paste(add_data1$Probennr,  "fall", sep = "_")

# add to metadata
metadata <- metadata %>% left_join(., add_data1, by = "sample_season")


# 2. second dataset has aquifer categorization, land use, type, OF_beb, coordinates
add_data2 <- read.csv("C:/Users/Angela Cukusic/Desktop/DS_analysis/data/spring_data.csv", row.names=NULL, sep=";")
## but it does not have the same well identifier as metadata 
## first add column with "Sample ID" from add_data1

add_data2 <- add_data2 %>% dplyr::select("Probennr", "X", "Y",  # coordinates
                                         "Land.use", "Geol_beb", "Type", "OF_beb")
# look if they are all a factor
#OF_beb is not
add_data2$OF_beb <- as.factor(add_data2$OF_beb) 

# not adding season-specific identifier because these categorizations are per well
# add to metadata
metadata <- metadata %>% left_join(., add_data2, by = c("Sample_ID.x" = "Probennr"))


# we are left with data on surface water as well
# best bet to remove is to filter out samples with not aquifer type category
metadata <- metadata %>% dplyr::filter(Geol_beb != "" ) %>% 
  dplyr::filter(Geol_beb != "OF" ) 
#for some reason doesnt work with %in% c("", "OF") 


rm( add_data2)

