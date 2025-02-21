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




#------------------------------------------------------------------------------------------
# Same graphs with only scientists who have 50 publications or more
small<-read_dta("scientists.dta")

small<-small%>%
  group_by(authorId)%>%
  mutate(t=1,
         paperNumber=cumsum(t),
         N=n())

small<-small%>%
  filter(N>49)

# Number of scientists:
length(unique(small$authorId))
# 2908

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



main_plot<-ggplot(res,aes(x=k,y=v))+
  geom_point(color="Orchid",alpha=0.8)+
  geom_smooth(color="red",se=FALSE)+
  labs(
    x = "n",
    # y = expression(bold(paste("Variance(", Z[n],")")))
    y = expression(paste("Variance(", Z[n],")"))
  ) +
  scale_y_continuous(limits=c(0.95,1.25), expand = c(0, 0))+
  scale_x_continuous(limits=c(0,50), expand = c(0, 0))+
  theme_bw() +
  theme(
    # axis.title = element_text(size = 14,face = "bold"),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    # panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )
main_plot


ggsave("images/figureS2.pdf",
       plot=main_plot, device = "pdf",
       width = 10, height = 8, 
       units = "cm",
       dpi = 1200,                                   
       useDingbats = FALSE)
#------------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------------
tot<-c()

for (i in 1:20){
  small<-small%>%
    group_by(authorId)%>%
    mutate(x_k_t=cumsum(logCit),
           lag_x_k_t=lag(x_k_t),
           full=logCit-(lag_x_k_t/(paperNumber+i)))
  
  small2<-small%>%
    group_by(paperNumber)%>%
    summarise(v=var(full))%>%
    filter(paperNumber<=50)
  
  small2$c<-i
  tot<-bind_rows(tot,small2)
  print(i)
}

tot$c_bis<-as.factor(paste0("c=", tot$c))
tot$c_bis <- factor(tot$c_bis, levels = c("c=1", "c=2", "c=3", "c=4", "c=5", "c=6", "c=7", "c=8",
                                          "c=9", "c=10", "c=11", "c=12", "c=13", "c=14", "c=15",
                                          "c=16", "c=17", "c=18", "c=19", "c=20"))

g<-ggplot(tot,aes(x=paperNumber,y=v)) +
  facet_wrap(~c_bis) +
  geom_point(color="Orchid",alpha=0.8)+
  geom_smooth(se=FALSE,color="red")+  scale_y_continuous(limits=c(1,1.25))+
  labs(
    x = "n",
    y = expression(paste("Variance(", Z[n],")"))
  )  +
  theme_bw() +
  theme(
    axis.title = element_text(size = 14),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.text = element_text(color = "black",face="bold"),
    strip.background = element_rect(fill = "seashell2")  
  )

g

ggsave("images/figureS3.pdf",
       plot=g, device = "pdf",
       width = 20, height = 15,
       units = "cm",
       dpi = 1200,                                   
       useDingbats = FALSE)

