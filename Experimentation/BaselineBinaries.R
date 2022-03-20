d <- read.csv("inputs/Binaries/SimulationDataWithShocks_Binary.csv")

d <- 
  d %>% 
  group_by(nu,nd,iA,beta,delta,Iu,Id,I0) %>% 
  mutate(numbo=n()) %>% 
  filter(winner=='') %>% 
  summarise(winner) %>%
  ungroup() %>% 
  filter(.,!duplicated(.))

# 0 and .625
a= 2
b=6/4*a
nd <- 10*rbeta(10000,a,6/4*a)
hist(nd)

nd <- runif(10000)

z <- 2*rbeta(10000,2,5)

nu <- (z+1)*nd
hist(nu)
