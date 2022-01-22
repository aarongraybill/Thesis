d <- read.csv("inputs/SimulationDataWithShocks.csv")

library(dplyr)

table(d %>% select(winner,high_ex_ante))


d <- 
  d %>% 
  mutate(bad_boy=case_when(
    winner=="D" & high_ex_ante == "False"~TRUE,
    TRUE~FALSE
  ))
  

library(ggplot2)

ggplot(d)+
  geom_violin(aes(x=high_ex_ante,y=total_shock,col=different))

ggplot(d)+
  geom_violin(aes(x=winner,y=total_shock,col=different))


ggplot(d)+
  geom_hist()