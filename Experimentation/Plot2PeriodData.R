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


d <- 
  d %>% 
  select(c(nu:I0,winner))

library(tidyr)
d_sum <-
  d %>% 
  group_by(winner) %>% 
  summarise(across(.cols=everything(),range)) %>% 
  mutate(case=case_when(nu==max(nu)~"max",
                        T~"min")) %>% 
  pivot_longer(nu:I0) %>% 
  pivot_wider(names_from = case,values_from = value) %>% 
  rename(parameter=name)

d_long <- 
  d %>% 
  pivot_longer(nu:I0)

ggplot(d_long)+
  geom_violin(aes(x=winner,y=value,fill=winner))+
  facet_wrap(vars(name),scales = "free_y")+
  theme_bw()+
  ylab("Distribution of Parameters")+
  xlab("Strategy Chosen")


c <- 
  d %>% 
  mutate(cons_disc=(nu/nd)-1,
         alg_dis=(Iu/Id)-1) %>% 
  group_by(winner) %>% 
  summarise(CD=range(cons_disc),
            AD=range(alg_dis),
            tal=range(nd)
            )


x=runif(100000)
y=runif(100000,min=0,max=x)

a=runif(100000)
b=runif(100000)

keep=b<a
a=a[keep]
b=b[keep]
