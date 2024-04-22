


setwd("")


q_model<-read.csv("Q_model.csv",header=FALSE)


q_model_long <- pivot_longer(q_model, 
                             cols = everything(),  
                             names_to = "time",   
                             values_to = "valeur") 

q_model_long$time<-rep(1:100,30)
q_model_long$id<-sort(rep(1:30,100))

q_model_long$model<-"Q-model"

q_model_twin<-read.csv("Q_model_twin.csv",header=FALSE)

q_model_twin_long <- pivot_longer(q_model_twin, 
                                  cols = everything(),  # Sélectionne toutes les colonnes
                                  names_to = "time",   # Nom de la colonne pour les temps
                                  values_to = "valeur") # Nom de la colonne pour les valeurs

q_model_twin_long$time<-rep(1:100,30)
q_model_twin_long$id<-sort(rep(1:30,100))
q_model_twin_long$model<-"Q-model twin"

q_model_long<-bind_rows(q_model_long,q_model_twin_long)

color_palette <- viridis_pal()(30)


a<-ggplot(q_model_long, aes(x = time, y = valeur, group = as.factor(id), color = as.factor(id))) +
  geom_line() +
  facet_wrap(~model)+
  scale_color_manual(values = color_palette) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab("Publication") +
  ylab("Cumulative #citations")+
  theme(legend.position = "none",
        panel.spacing = unit(1, "lines"), 
        strip.text = element_text(color = "black", face = "bold"),
        strip.background = element_rect(fill = "seashell2"),
        aspect.ratio = 1.2,
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +  
  scale_y_continuous( breaks = seq(-0, 60, by = 10), limits = c(-0, 60),expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1, 10, by = 1), limits = c(1, 10), expand = c(0, 0))+
  theme(panel.spacing = unit(1, "lines")) 


#######################
# Contagious Poisson
#######################


cp_model<-read.csv("Contagious_Poisson.csv",header=FALSE)

cp_model_long <- pivot_longer(cp_model, 
                              cols = everything(),  
                              names_to = "time",   
                              values_to = "valeur") 

cp_model_long$time<-rep(1:10,30)
cp_model_long$id<-sort(rep(1:30,10))

cp_model_long$model<-"Contagious Poisson"

cp_model_twin<-read.csv("Contagious_Poisson_twin.csv",header=FALSE)


cp_model_twin_long <- pivot_longer(cp_model_twin, 
                                   cols = everything(),  
                                   names_to = "time",  
                                   values_to = "valeur") 

cp_model_twin_long$time<-rep(1:10,30)
cp_model_twin_long$id<-sort(rep(1:30,10))
cp_model_twin_long$model<-"Contagious Poisson twin"

cp_model_long<-bind_rows(cp_model_long,cp_model_twin_long)

color_palette <- viridis_pal()(30)

b<-ggplot(cp_model_long, aes(x = time, y = valeur, group = as.factor(id), color = as.factor(id))) +
  geom_line() +
  facet_wrap(~model)+
  scale_color_manual(values = color_palette) +
  theme_bw() +
  theme(legend.position = "none") + 
  xlab("Time") +
  ylab("Cumulative #papers")+
  theme(legend.position = "none",
        panel.spacing = unit(1, "lines"),  
        strip.text = element_text(color = "black", face = "bold"),
        strip.background = element_rect(fill = "seashell2"),
        aspect.ratio = 1.2,
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +  
  scale_y_continuous( breaks = seq(0, 100, by = 20), limits = c(0, 100),expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1, 10, by = 1), limits = c(1, 10), expand = c(0, 0))+
  theme(panel.spacing = unit(1, "lines")) 


#######################
# SD model 
#######################

set.seed(108431)
sd_model<-read.csv("SD_model.csv",header=FALSE)

sd_model_long <- pivot_longer(sd_model, 
                              cols = everything(), 
                              names_to = "time",   
                              values_to = "valeur") 

sd_model_long$time<-rep(1:100,30)
sd_model_long$id<-sort(rep(1:30,100))

ids<-sample(sd_model_long$id,size=10,replace=FALSE)

sd_model_long$model<-"Settled dispositions"

sd_model_long<-sd_model_long[which(sd_model_long$id%in%ids),]

sd_model_twin<-read.csv("SD_model_twin.csv",header=FALSE)


sd_model_twin_long <- pivot_longer(sd_model_twin, 
                                   cols = everything(),  
                                   names_to = "time",  
                                   values_to = "valeur") 

sd_model_twin_long$time<-rep(1:100,30)
sd_model_twin_long$id<-sort(rep(1:30,100))
sd_model_twin_long$model<-"Settled dispositions twin"

sd_model_twin_long<-sd_model_twin_long[which(sd_model_twin_long$id%in%ids),]


sd_model_long<-bind_rows(sd_model_long,sd_model_twin_long)

color_palette <- viridis_pal()(30)
color_palette <- color_palette[c(1,5,9,13,15,19,21,25,29)]

sd_model_long<-sd_model_long[which(sd_model_long$time<=10),]

c<-ggplot(sd_model_long, aes(x = time, y = valeur, group = as.factor(id), color = as.factor(id))) +
  geom_line() +
  facet_wrap(~model)+
  scale_color_manual(values = color_palette) +
  theme_bw() +
  theme(legend.position = "none") + 
  xlab("Time") +
  ylab("Belief")+
  theme(legend.position = "none",
        panel.spacing = unit(1, "lines"),  
        strip.text = element_text(color = "black", face = "bold"),
        strip.background = element_rect(fill = "seashell2"),
        aspect.ratio = 1.2,
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +  
  scale_y_continuous( breaks = seq(-5, 5, by = 1), limits = c(-5, 5),expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1, 10, by = 1), limits = c(1, 10), expand = c(0, 0)) +
  theme(panel.spacing = unit(1, "lines")) 


#######################
# Polya Urn model 
#######################
polya_model<-read.csv("Polya_urn.csv",header=FALSE)

polya_model_long <- pivot_longer(polya_model, 
                                 cols = everything(),  
                                 names_to = "time",   
                                 values_to = "valeur") 

polya_model_long$time<-rep(1:100,30)
polya_model_long$id<-sort(rep(1:30,100))

polya_model_long$model<-"Pólya urn"



polya_model_twin<-read.csv("Polya_urn_twin.csv",header=FALSE)
polya_model_twin_long <- pivot_longer(polya_model_twin, 
                                      cols = everything(),  
                                      names_to = "time",  
                                      values_to = "valeur") 

polya_model_twin_long$time<-rep(1:100,30)
polya_model_twin_long$id<-sort(rep(1:30,100))
polya_model_twin_long$model<-"Pólya urn twin"

polya_model_long<-bind_rows(polya_model_long,polya_model_twin_long)

color_palette <- viridis_pal()(30)


d<-ggplot(polya_model_long, aes(x = time, y = valeur, group = as.factor(id), color = as.factor(id))) +
  geom_line() +
  facet_wrap(~model) +
  scale_color_manual(values = color_palette) +
  theme_bw() +
  theme(legend.position = "none",
        panel.spacing = unit(1, "lines"),  
        strip.text = element_text(color = "black", face = "bold"),
        strip.background = element_rect(fill = "seashell2"),
        aspect.ratio = 1.2,
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +  
  xlab("Draw number") +
  ylab("Difference #black - #red") +
  scale_y_continuous(breaks = seq(-100, 100, by = 20), limits = c(-100, 100), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, 100, by = 25), limits = c(0, 100), expand = c(0, 0))



# Combine all graphs:
tot<-plot_grid(d, a, b, c, 
               labels=c("A", "B", "C","D"),  
               ncol = 2, nrow = 2)


ggsave("/figure1_all_models.jpeg",
       plot=tot, device = "jpeg",
       width = 24, height = 15, units = "cm")

