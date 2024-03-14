# Check if dplyr packages are installed and if not install and load them
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)
if (!requireNamespace("haven", quietly = TRUE)) {
  install.packages("haven")
}
library(haven)

##############################################
# Load the scientists data which contains one row per paper 
# and only the authorId retrived from A. Barabasi's website
# and the citations in the 10 years following publication date
# retrived with openAlex (https://docs.ropensci.org/openalexR/)
small<-read_dta("write_your_path/scientists.dta")

# Number of scientists:
length(unique(small$authorId))
# 22861

# Number papers in sequential order:
small<-small%>%
  group_by(authorId)%>%
  mutate(t=1,
         k=cumsum(t))

# log transform (add + 1 for papers with no citations in 10 years)
small<-small%>%
  mutate(logCit=log(cit10+1))

# compute q per researcher:
small<-small%>%
  group_by(authorId)%>%
  mutate(q=mean(logCit))

# compute p: deviation from author q
small$p<-small$q-small$logCit

# find c parameter
c<-var(small$p)/var(small$q)

# compute the elements needed to follow equation in appendix 
small<-small%>%
  group_by(authorId)%>%
  mutate(cumsumPrev=cumsum(logCit), # cumulative citation per author
         x_1=lag(cumsumPrev),       # same in t-1
         k_1=lag(k),                # paper number in t-1
         denominator=k_1+c,          
         division=x_1/denominator,  
         tot=logCit-division)       

# compute the variance per k 
res<-small%>%
  group_by(k)%>%
  summarise(v=var(tot,na.rm=TRUE))


# For all k
main_plot<-ggplot(res,aes(x=k,y=v))+
  geom_line()+
  geom_point(shape=4)+
  geom_smooth(se=FALSE,color="blue")+
  labs(
    x = "k",
    y = "Variance"
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.text = element_text(color = "white", face = "bold"),
    strip.background = element_rect(fill = "grey70")  
  )+
  annotate(
    "text",
    x = Inf, y = -Inf,   
    hjust = 1.2, vjust = -1,  
    label = paste0("c=",round(c,2)),
    color = "red", size = 5, fontface = "bold"  
  )


# For k<=50
sub_plot<-ggplot(res[which(res$k<=50),],aes(x=k,y=v))+
  geom_line()+
  geom_point(shape=4)+
  geom_smooth(se=FALSE,color="blue")+
  labs(
    x = "k",
    y = "Variance"
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 10, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.text = element_text(color = "white", face = "bold"),
    strip.background = element_rect(fill = "grey70")  
  )+
  annotate(
    "text",
    x = Inf, y = -Inf,   # Position in the bottom right corner
    hjust = 1.2, vjust = -1,  # To adjust horizontal and vertical justification
    label = paste0("c=",round(c,2)),
    color = "red", size = 5, fontface = "bold"  
  )

# Add the small graph to the big one and position it: 
g<-main_plot +
  coord_cartesian(xlim = c(1, 430), ylim = c(0, 6)) +
  annotation_custom(
    ggplotGrob(sub_plot),
    xmin = 0, xmax = 282,  
    ymin = 2.5, ymax = 6   
  )

g

# ggsave("write_your_path/variance_all_sample.jpeg",
#        plot=g, device = "jpeg",
#        width = 20, height = 15, units = "cm")
