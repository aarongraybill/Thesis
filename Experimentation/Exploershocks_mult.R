d <- read.csv("/Volumes/GoogleDrive/My Drive/Thesis_Git/Thesis/Inputs/SimulationDataWithShocks_multiple_variance.csv")


d_sum <- 
  d %>% 
  group_by(var,winner) %>% 
  summarise(diff=mean(different),
            n_switches=sum(different))


ggplot(d_sum)+
  geom_line(aes(x=var,y=diff,col=winner))+
  scale_x_log10()+
  xlab("Variance In Simulation")+
  ylab("Proportion who switch from ex ante Strategy")

d <- 
  d %>% 
  mutate(talent=(nu-nd)/nd) %>% 
  mutate(u_max=pmax(u_high, u_low),
         u_min=pmin(u_high, u_low)) %>% 
  group_by(var) %>% 
  mutate(u_diff=u_max/abs(total_shock))

ggplot(d)+
  geom_point(aes(x=log(talent),y=u_max,col=as.factor(var)))


ggplot(d)+
  geom_point(aes(x=log(talent),y=log(u_diff)))+
  facet_wrap(vars(var))

  
  
  
ggplot(d_sum %>% filter(var!=0))+
  geom_area(aes(x=var,y=n_switches/9908,fill=winner))+
  scale_x_log10()+
  xlab("Variance In Simulation")+
  ylab("Fraction who switch from ex ante Strategy")







