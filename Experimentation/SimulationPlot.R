library(dplyr)
d30 <- 
  read.csv("../VFI_Data/SimulationData/sim_d30_A600.csv")
k20 <- 
  read.csv("../VFI_Data/SimulationData/sim_k20_A600.csv")
k10 <- 
  read.csv("../VFI_Data/SimulationData/sim_k10_A600.csv")
sd5 <- 
  read.csv("../VFI_Data/SimulationData/sim_sd5_A600.csv")


d30 <- 
  d30 %>% 
  mutate(sim="d30") %>% 
  rename(m=1, v =2, A =3, n =4, u=5, t=6, ids=7)
k20 <- 
  k20 %>% 
  mutate(sim="k20") %>% 
  rename(m=1, v =2, A =3, n =4, u=5, t=6, ids=7)
k10 <- 
  k10 %>% 
  mutate(sim="k10") %>% 
  rename(m=1, v =2, A =3, n =4, u=5, t=6, ids=7)
sd5 <- 
  sd5 %>% 
  mutate(sim="sd5") %>% 
  rename(m=1, v =2, A =3, n =4, u=5, t=6, ids=7)

d <- 
  d30 %>% 
  full_join(k20) %>% 
  full_join(k10) %>% 
  full_join(sd5) %>% 
  mutate(z=case_when(sim=="k10"~10/m,
                     T~20/m))

d <- 
  d %>% 
  #filter(ids<100) %>% 
  filter(t!=0 & t!=99)

d_roll <- 
  d %>% 
  select(sim,t,A,ids) %>% 
  group_by(sim,t) %>% 
  summarize(A=mean(A))

d <- 
  d %>% 
  filter(ids<100)



p_z <- 
  ggplot(d) +
  geom_line(aes(x=t,y=z,group=interaction(ids,sim),col=sim),alpha=.05, size=.4)+
  guides(colour = guide_legend(override.aes = list(alpha = 1,size=5)))+
  ylab("Quality")+
  xlab("Time")+
  scale_color_manual(values = c("#8C2730","#F28D9F","#255059","#FFBA3D"),labels=c("High Loyalty", "Low Talent", "Baseline", "High Volatility"),name=element_blank())+  theme(text = element_text(family = "Tahoma")) +
  theme(legend.title = element_text(hjust = .5)) +
  theme(
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  )

p_A <- 
  ggplot(d) +
  geom_line(aes(x=t,y=A,group=interaction(ids,sim),col=sim),alpha=.05, size=.4)+
  geom_line(data=d_roll,aes(x=t,y=A,col=sim),size=1)+
  scale_color_manual(values = c("#8C2730","#F28D9F","#255059","#FFBA3D"),labels=c("High Loyalty", "Low Talent", "Baseline", "High Volatility"),name=element_blank())+
  scale_y_continuous(position = "right")+
  ylab("Audience")+
  xlab("Time")+
  theme(text = element_text(family = "Tahoma")) +
  theme(legend.title = element_text(hjust = .5)) +
  theme(
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  )


ggpubr::ggarrange(p_z,p_A, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")


ggplot(d %>% filter(sim=="sd5")) +
  #xlim(c(0,4*10^-4))+
  geom_line(aes(x=t,y=A,group=interaction(ids,sim),col=sim),alpha=.05, size=.4)+
  geom_line(data=d_roll %>% filter(sim=="sd5"),aes(x=t,y=A,col=sim),size=1)+
  #scale_color_manual(values = c("#8C2730","#F28D9F","#255059","#FFBA3D"),labels=c("High Loyalty", "Low Talent", "Baseline", "High Volatility"),name=element_blank())+
  scale_y_continuous(position = "right")+
  ylab("Audience")+
  xlab("Time")+
  theme(text = element_text(family = "Tahoma")) +
  theme(legend.title = element_text(hjust = .5)) +
  theme(
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  )
