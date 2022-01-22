d <- read.csv("inputs/SimulationDataWithShocks.csv")

library(ggplot2)
library(dplyr)

d <- 
  d %>% 
mutate(premium=nu/nd-1)

d_sum <- 
  d %>% 
  group_by(winner) %>% 
  summarise(across(c(nu:I0,premium),median))


ggplot(d)+
  geom_point(aes(x=nd,y=log(premium),col=winner),alpha=.1)+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  #scale_color_manual(values = c("#8C2730","#255059","#FFBA3D","#13091C"))
  scale_color_viridis_d(option="cividis")


ggplot(d)+
  geom_violin(aes(winner,Iu-Id)#,scale = "count"
              )+
  ylim(0,3)


ggplot(d)+
geom_density2d_filled(aes(I0,nd))
