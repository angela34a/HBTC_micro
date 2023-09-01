spring_data_additional %>% 
  dplyr::filter( Temp > 10  &  Temp < 18 ) %>% 
  # plot
  ggplot(aes(Temp,ens)) + 
  geom_point(shape=21, size = 3, fill = "violet",
             alpha = 0.9, stroke = 0.6) + 
  geom_smooth(method = "lm", se= FALSE, color = "black") + 
  ggpmisc::stat_poly_eq(use_label(c("adj.R2", "p")), 
                        label.y = "top", size = 4,
                        small.p = TRUE) + 
  labs(y = "Shannon diversity index", x ="Temperature", 
       title = "Linear trend in diversity index") +
  theme(legend.position = "none",
        axis.title=element_text(size=14,face="bold"),
        axis.text.y = element_text(size=10,face="bold"),
        axis.text.x = element_text(angle = 90)) 
