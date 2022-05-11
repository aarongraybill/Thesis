d <- read.csv("Inputs/TalentDist_75.csv")

library(ggplot2)  
ggplot(d)+
  geom_point(aes(x=talent,y=umax,color=winner),alpha=.1)


hist(d$nd)
hist(d$nu)
hist(d$talent)

# fix nd, run it three times for differrent I0
# wiggle talent dist to make sure it's not spurious
# percent of total population
# Compare dist of talent to dist of umax with and without dividing by sd



d <- read.csv("Inputs/TalentDist_lognormal.csv")

d$talent <- (d$nu-d$nd)/d$nd
d$umax <- pmax(d$uA,d$uB,d$uC,d$uC)

d$I0 <- as.factor(d$I0)


library(ggplot2)
library(dplyr)
ggplot(d %>% filter(talent<10))+
  geom_line(aes(x=talent,y=umax,linetype=I0,color=winner))


d_sum<- 
  d %>% 
  group_by(I0) %>% 
  mutate(rev_share=umax/sum(umax),
         talent_pct=ecdf(talent)(talent),
         rev_pct=ecdf(umax)(umax))


ggplot(d_sum)+
  geom_line(aes(x=talent,y=rev_share,color=I0))

#hiding all the lorenz business
{
lc_0 <- 
  d %>% 
  filter(I0==0) %>% 
  pull(umax) %>% 
  Lc()

lc_25 <- 
  d %>% 
  filter(I0==25) %>% 
  pull(umax) %>% 
  Lc()

lc_50 <- 
  d %>% 
  filter(I0==50) %>% 
  pull(umax) %>% 
  Lc()

lc_75 <- 
  d %>% 
  filter(I0==75) %>% 
  pull(umax) %>% 
  Lc()

lc_100 <- 
  d %>% 
  filter(I0==100) %>% 
  pull(umax) %>% 
  Lc()

# create data.frame from LC
p <- lc_0[1]
L <- lc_0[2]
lc_0_df <- data.frame(p,L,I0=0)

p <- lc_25[1]
L <- lc_25[2]
lc_25_df <- data.frame(p,L,I0=25)

p <- lc_50[1]
L <- lc_50[2]
lc_50_df <- data.frame(p,L,I0=50)

p <- lc_75[1]
L <- lc_75[2]
lc_75_df <- data.frame(p,L,I0=75)

p <- lc_100[1]
L <- lc_100[2]
lc_100_df <- data.frame(p,L,I0=100)

lc_talent <- 
  d %>% 
  pull(talent) %>% 
  Lc()

p <- lc_talent[1]
L <- lc_talent[2]
lc_talent_df <- data.frame(p,L,I0=100)

d_lc_plot <- 
  lc_0_df %>% 
  full_join(lc_25_df) %>% 
  full_join(lc_50_df) %>% 
  full_join(lc_75_df) %>% 
  full_join(lc_100_df) %>% 
  mutate(I0=as.factor(I0))
}

plot(density(d$talent))

ggplot(d_lc_plot)+
  geom_line(aes(x=p,y=L,color=I0))+
  #scale_x_continuous(name="Cumulative share of X", limits=c(0,1)) + 
  #scale_y_continuous(name="Cumulative share of Y", limits=c(0,1)) +
  geom_segment(aes(x=0,y=0,xend=1,yend=1),alpha=.4)+
  geom_line(data=lc_talent_df,aes(x=p,y=L),linetype=2)


# vary beta to see if the results to see if talent matters less with low beta
# talent on x cumulative revenue on y
  # for talent k, the y axis is share of all 
  # revenue coming from people with talent less than k
#introduce more noise to see if reduces importance of talent

# INTRODUCE F(N,EPSILON)=I AS NUMBER OF IMPRESSIONS
# Build the algorithm function





