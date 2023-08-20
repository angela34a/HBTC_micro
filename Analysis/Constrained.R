
# pca  of env vars ####

df_alpha <- metadata %>%  
  column_to_rownames("sample_season") %>% 
  dplyr::select(where(is.numeric)) %>% 
  dplyr::select(-c("NH4", # many NAs
                   "X", "Y",     # coordinates
                   "Extraction_depth", "Watercolumn", "GW_Table")) %>% 
  na.omit() %>% 
  scale(center = T, scale = T) %>% 
  as.data.frame()
cor_matrix <- cor(df_alpha)
highly_correlated <- caret::findCorrelation(cor_matrix, cutoff = 0.9) 
# none
# df_alpha <- df_alpha[, - highly_correlated] 


# lets see if any env vars can be packed neatly in PCs
pca_env <- prcomp(df_alpha, scale = FALSE)
biplot(pca_env)


ggplot2::autoplot(pca_env, 
                  
                  data = 
                    (
                      df_alpha %>% 
                        rownames_to_column("sample_season") %>% 
                        left_join(., metadata[,c("category", "sample_season")], 
                                  by = "sample_season") 
                    ), 
                  
                  colour = 'category',
                  size=5, 
                  alpha = 0.2,
                  loadings = TRUE, 
                  loadings.colour = "black", 
                  loadings.label = TRUE, 
                  loadings.label.size = 4.3, 
                  loadings.label.colour = "black",
                  loadings.label.repel = TRUE) +
  
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  scale_color_manual(values = pal3)+
  theme(legend.background = element_blank(),
        legend.box.background = element_rect(colour = "gray"))  -> pca_plot


pca_plot

# summary of first three PCs
summary(pca_env)$importance %>% 
  as.data.frame() %>% 
  dplyr::select("PC1", "PC2", "PC3", "PC4")


# pca loadings ####
# Extract the loadings from the PCA result and convert to tidy format 
loadings_df <-   tidy(pca_env, matrix = "rotation")


# PC1
# Create the ggplot2 object for plotting the loadings
loadings_df %>% filter(PC == "1") %>% 
  #x=reorder(class,-amount,sum)
  ggplot( aes(x = reorder(column, -value), y = value, group = factor(column))) +
  geom_bar(stat = "identity", position = "dodge", 
           fill= "gray90", color = "gray20") +
  geom_text(aes(label = column), position = position_dodge(width = 0.9), 
            #vjust = "inward",  
            angle = 90 , hjust = "inward") +
  labs(x = "Variables of PC1", y = "Loadings") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        legend.position = "none")  -> plot_pca1

# PC2 
loadings_df %>% filter(PC == "2") %>% 
  #x=reorder(class,-amount,sum)
  ggplot( aes(x = reorder(column, -value), y = value, group = factor(column))) +
  geom_bar(stat = "identity", position = "dodge", 
           fill= "gray90", color = "gray20") +
  geom_text(aes(label = column), position = position_dodge(width = 0.9), 
            #vjust = "inward",  
            angle = 90 , hjust = "inward") +
  labs(x = "Variables of PC2", y = "Loadings") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        legend.position = "none") -> plot_pca2

# PC3
loadings_df %>% filter(PC == "3") %>% 
  #x=reorder(class,-amount,sum)
  ggplot( aes(x = reorder(column, -value), y = value, group = factor(column))) +
  geom_bar(stat = "identity", position = "dodge", 
           fill= "gray90", color = "gray20") +
  geom_text(aes(label = column), position = position_dodge(width = 0.9), 
            #vjust = "inward",  
            angle = 90 , hjust = "inward") +
  labs(x = "Variables of PC3", y = "Loadings") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        legend.position = "none") -> plot_pca3

plot_pca1 + plot_pca2 + plot_pca3


# constraints ####

# env clean
# df_alpha - already scaled and selected the important variables in other script

# aggregate to genus
# asv_aggregated - already done for nmds

# asv clean
# because some samples get removed because of NAs 
asv_aggregated [, which  ( colnames(asv_aggregated) %in% rownames(df_alpha)) ] -> sp.dat

# transform hellinger (or clr sometimes)
decostand(sp.dat, method = "hellinger") -> sp.dat
#clr(sp.dat) -> sp.dat

# 1. first model
simpleRDA <- capscale(t(sp.dat) ~  ., data = df_alpha ,
                      distance = "bray",
                      scaling = "sites")
# take only that are vif<10
env_vars_low_vif <- vif.cca(simpleRDA) %>% 
  as.data.frame() %>% 
  dplyr::rename("env" = ".") %>% 
  dplyr::filter(env < 10) 

# make new environmental data with only low vif variables
df_alpha[,which(colnames(df_alpha) %in%  rownames(env_vars_low_vif) )] -> env.dat


# 2. model without them
simpleRDA <- capscale(t(sp.dat) ~  ., 
                      data=  env.dat ,
                      distance = "bray",
                      scaling = "sites")


# test all variables and axes
# canonical axes
anova.model <- anova.cca(simpleRDA, by='axis', step=1000)  
# no axes sign

# the whole model
anova.model2 <- anova.cca(simpleRDA, step=1000) #is sign
RsquareAdj(simpleRDA)$adj.r.squared # and explains <1%


# env parameters
anova.model3 <- anova(simpleRDA, step=1000, by = "term") 
# none sign


