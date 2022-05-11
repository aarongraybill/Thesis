d <- read.csv("inputs/SimulationDataWithShocks.csv")

library(dplyr)

table(d %>% select(winner,high_ex_ante))

library(ggplot2)
#skunk

ggplot(d)+
  geom_violin(aes(x=high_ex_ante,y=total_shock,col=different))

ggplot(d)+
  geom_violin(aes(x=winner,y=total_shock,col=different))

ggplot(d)+
  geom_violin(aes(x=winner,y=aud,col=different))


ggplot(d)+
  geom_hist()