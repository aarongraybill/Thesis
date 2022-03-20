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


  
  
  
ggplot(d_sum %>% filter(var!=0))+
  geom_area(aes(x=var,y=n_switches/9908,fill=winner))+
  scale_x_log10()+
  xlab("Variance In Simulation")+
  ylab("Fraction who switch from ex ante Strategy")







