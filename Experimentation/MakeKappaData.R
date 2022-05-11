library(dplyr)
setwd("/Volumes/GoogleDrive/My Drive/Thesis_Git")
### Read v values ----
kap_names <- list.files("Thesis/VFI_Data/kappa_tests/v_data",pattern = ".csv",full.names = TRUE)
kaps <- 
  lapply(kap_names, read.csv,header=F)

kaps <-
  lapply(kaps, function(x) {
    x <- as.matrix(x)
    x <- c(x)
    return(x)
  })

act_kap <- 
  substr(kap_names,39,nchar(kap_names)-6) %>% 
  as.numeric()

m_names <- list.files("Thesis/VFI_Data/kappa_tests/m_data",pattern = ".csv",full.names = TRUE)
m_kaps <- 
  lapply(m_names, read.csv,header=F)

m_kaps <-
  lapply(m_kaps, function(x) {
    x <- as.matrix(x)
    x <- c(x)
    return(x)
  })

act_m <- 
  substr(m_names,39,nchar(m_names)-6) %>% 
  as.numeric()

A_names <- list.files("Thesis/VFI_Data/kappa_tests/A_data",pattern = ".csv",full.names = TRUE)
A_kaps <- 
  lapply(A_names, read.csv,header=F)

A_kaps <-
  lapply(A_kaps, function(x) {
    x <- as.matrix(x)
    x <- c(x)
    return(x)
  })

act_A <- 
  substr(A_names,39,nchar(A_names)-6) %>% 
  as.numeric()

### Harmonize ----

nn = read.csv("Thesis/VFI_Data/kappa_tests/constants/nn.csv", header = F)
AA = read.csv("Thesis/VFI_Data/kappa_tests/constants/AA.csv", header = F)

nn <- as.matrix(nn)
nn <- c(nn)

AA <- as.matrix(AA)
AA <- c(AA)

for (i in 1:length(kaps)){
  kaps[[i]] <- 
    data.frame(v=kaps[[i]],
                  nn=nn,
                  AA=AA) %>% 
    mutate(k=act_kap[i])
  m_kaps[[i]] <- 
    data.frame(m=m_kaps[[i]],
               nn=nn,
               AA=AA) %>% 
    mutate(k=act_m[i])
  A_kaps[[i]] <-
    data.frame(A=A_kaps[[i]],
               nn=nn,
               AA=AA) %>%
    mutate(k=act_A[i])
}

k <-
  purrr::reduce(kaps,full_join)

m <- 
  purrr::reduce(m_kaps,full_join)

A <- 
  purrr::reduce(A_kaps,full_join)


k <- 
  full_join(k,m) %>% 
  full_join(A)

base_case_v=kaps[[which(act_kap==20)]]
base_case_m=m_kaps[[which(act_m==20)]]
base_case_A=A_kaps[[which(act_A==20)]]

base_case_v <- 
  base_case_v %>% 
  rename(v_baseline=v) %>% 
  select(-k)

base_case_m <- 
  base_case_m %>% 
  rename(m_baseline=m) %>% 
  select(-k)

base_case_A <- 
  base_case_A %>% 
  rename(A_baseline=A) %>% 
  select(-k)

k <- 
  full_join(k,base_case_v) %>%
  full_join(base_case_m) %>% 
  full_join(base_case_A) %>% 
  mutate(v_diff=(v-v_baseline)/(v_baseline)) %>% 
  mutate(m_diff=(m-m_baseline)/(m_baseline)) %>% 
  mutate(A_diff=(A-A_baseline)/(A_baseline)) %>% 
  mutate(k_diff=(k-20)/20)

write.csv(k,"Thesis/VFI_Data/kappa_tests/k_data.csv")



k_sum_nn <-
  k %>%
  group_by(k,AA) %>%
  summarize(v=median(v),
            m=median(m),
            A=median(A))

k_sum_AA <-
  k %>%
  group_by(k,nn) %>%
  summarize(v=median(v),
            m=median(m),
            A=median(A))


library(ggplot2)
ggplot(k_sum_nn %>% filter(AA!=min(AA)))+
  geom_line(aes(x=k,y=A,col=AA,group=AA),alpha=.6)


ggplot(k_sum_AA# %>% filter(AA!=min(AA))
       )+
  geom_line(aes(x=k,y=A,col=nn,group=nn),alpha=.6)



ggplot(k %>% 
         filter(k==50) %>% 
         filter(AA!=min(AA)),
       aes(AA, nn, fill= A),alpha=.1) + 
  geom_tile()+
  scale_fill_gradient(low = "#211030", high = "#FFBA3D")+
  #scale_fill_gradient(limits=c(0,1000))+
  theme_bw()



### SD 5 data ----

### Read v values ----
kap_names <- list.files("Thesis/VFI_Data/kappa_tests/v5_data",pattern = ".csv",full.names = TRUE)
kaps <- 
  lapply(kap_names, read.csv,header=F)

kaps <-
  lapply(kaps, function(x) {
    x <- as.matrix(x)
    x <- c(x)
    return(x)
  })

act_kap <- 
  substr(kap_names,40,nchar(kap_names)-6) %>% 
  as.numeric()

m_names <- list.files("Thesis/VFI_Data/kappa_tests/m5_data",pattern = ".csv",full.names = TRUE)
m_kaps <- 
  lapply(m_names, read.csv,header=F)

m_kaps <-
  lapply(m_kaps, function(x) {
    x <- as.matrix(x)
    x <- c(x)
    return(x)
  })

act_m <- 
  substr(m_names,40,nchar(m_names)-6) %>% 
  as.numeric()

A_names <- list.files("Thesis/VFI_Data/kappa_tests/A5_data",pattern = ".csv",full.names = TRUE)
A_kaps <- 
  lapply(A_names, read.csv,header=F)

A_kaps <-
  lapply(A_kaps, function(x) {
    x <- as.matrix(x)
    x <- c(x)
    return(x)
  })

act_A <- 
  substr(A_names,40,nchar(A_names)-6) %>% 
  as.numeric()

### Harmonize ----

nn = read.csv("Thesis/VFI_Data/kappa_tests/constants/nn.csv", header = F)
AA = read.csv("Thesis/VFI_Data/kappa_tests/constants/AA.csv", header = F)

nn <- as.matrix(nn)
nn <- c(nn)

AA <- as.matrix(AA)
AA <- c(AA)

for (i in 1:length(kaps)){
  kaps[[i]] <- 
    data.frame(v=kaps[[i]],
               nn=nn,
               AA=AA) %>% 
    mutate(k=act_kap[i])
  m_kaps[[i]] <- 
    data.frame(m=m_kaps[[i]],
               nn=nn,
               AA=AA) %>% 
    mutate(k=act_m[i])
  A_kaps[[i]] <-
    data.frame(A=A_kaps[[i]],
               nn=nn,
               AA=AA) %>%
    mutate(k=act_A[i])
}

k <-
  purrr::reduce(kaps,full_join)

m <- 
  purrr::reduce(m_kaps,full_join)

A <- 
  purrr::reduce(A_kaps,full_join)


k <- 
  full_join(k,m) %>% 
  full_join(A)

base_case_v=kaps[[which(act_kap==20)]]
base_case_m=m_kaps[[which(act_m==20)]]
base_case_A=A_kaps[[which(act_A==20)]]

base_case_v <- 
  base_case_v %>% 
  rename(v_baseline=v) %>% 
  select(-k)

base_case_m <- 
  base_case_m %>% 
  rename(m_baseline=m) %>% 
  select(-k)

base_case_A <- 
  base_case_A %>% 
  rename(A_baseline=A) %>% 
  select(-k)

k <- 
  full_join(k,base_case_v) %>%
  full_join(base_case_m) %>% 
  full_join(base_case_A) %>% 
  mutate(v_diff=(v-v_baseline)/(v_baseline)) %>% 
  mutate(m_diff=(m-m_baseline)/(m_baseline)) %>% 
  mutate(A_diff=(A-A_baseline)/(A_baseline)) %>% 
  mutate(k_diff=(k-20)/20)

write.csv(k,"Thesis/VFI_Data/kappa_tests/k_sd5_data.csv")

# make point 2 data----

### Read v values ----
kap_names <- list.files("Thesis/VFI_Data/kappa_tests/vp2_data",pattern = ".csv",full.names = TRUE)
kaps <- 
  lapply(kap_names, read.csv,header=F)

kaps <-
  lapply(kaps, function(x) {
    x <- as.matrix(x)
    x <- c(x)
    return(x)
  })

act_kap <- 
  substr(kap_names,41,nchar(kap_names)-6) %>% 
  as.numeric()

m_names <- list.files("Thesis/VFI_Data/kappa_tests/mp2_data",pattern = ".csv",full.names = TRUE)
m_kaps <- 
  lapply(m_names, read.csv,header=F)

m_kaps <-
  lapply(m_kaps, function(x) {
    x <- as.matrix(x)
    x <- c(x)
    return(x)
  })

act_m <- 
  substr(m_names,41,nchar(m_names)-6) %>% 
  as.numeric()

A_names <- list.files("Thesis/VFI_Data/kappa_tests/Ap2_data",pattern = ".csv",full.names = TRUE)
A_kaps <- 
  lapply(A_names, read.csv,header=F)

A_kaps <-
  lapply(A_kaps, function(x) {
    x <- as.matrix(x)
    x <- c(x)
    return(x)
  })

act_A <- 
  substr(A_names,41,nchar(A_names)-6) %>% 
  as.numeric()

### Harmonize ----

nn = read.csv("Thesis/VFI_Data/kappa_tests/constants/nn.csv", header = F)
AA = read.csv("Thesis/VFI_Data/kappa_tests/constants/AA.csv", header = F)

nn <- as.matrix(nn)
nn <- c(nn)

AA <- as.matrix(AA)
AA <- c(AA)

for (i in 1:length(kaps)){
  kaps[[i]] <- 
    data.frame(v=kaps[[i]],
               nn=nn,
               AA=AA) %>% 
    mutate(k=act_kap[i])
  m_kaps[[i]] <- 
    data.frame(m=m_kaps[[i]],
               nn=nn,
               AA=AA) %>% 
    mutate(k=act_m[i])
  A_kaps[[i]] <-
    data.frame(A=A_kaps[[i]],
               nn=nn,
               AA=AA) %>%
    mutate(k=act_A[i])
}

k <-
  purrr::reduce(kaps,full_join)

m <- 
  purrr::reduce(m_kaps,full_join)

A <- 
  purrr::reduce(A_kaps,full_join)


k <- 
  full_join(k,m) %>% 
  full_join(A)

base_case_v=kaps[[which(act_kap==20)]]
base_case_m=m_kaps[[which(act_m==20)]]
base_case_A=A_kaps[[which(act_A==20)]]

base_case_v <- 
  base_case_v %>% 
  rename(v_baseline=v) %>% 
  select(-k)

base_case_m <- 
  base_case_m %>% 
  rename(m_baseline=m) %>% 
  select(-k)

base_case_A <- 
  base_case_A %>% 
  rename(A_baseline=A) %>% 
  select(-k)

k <- 
  full_join(k,base_case_v) %>%
  full_join(base_case_m) %>% 
  full_join(base_case_A) %>% 
  mutate(v_diff=(v-v_baseline)/(v_baseline)) %>% 
  mutate(m_diff=(m-m_baseline)/(m_baseline)) %>% 
  mutate(A_diff=(A-A_baseline)/(A_baseline)) %>% 
  mutate(k_diff=(k-20)/20)

write.csv(k,"Thesis/VFI_Data/kappa_tests/k_sdp2_data.csv")



# Make plots----
k <-
  k %>% 
  mutate(sd=0.2)

k1 <- 
  read.csv("Thesis/VFI_Data/kappa_tests/k_data.csv") %>% 
  select(-X)

k1 <- k1 %>%  
  mutate(sd=1)

k5 <- 
  read.csv("Thesis/VFI_Data/kappa_tests/k_sd5_data.csv") %>% 
  select(-X)


k5 <- k5 %>%  
  mutate(sd=5)


k <- 
  full_join(k,k1) %>% 
  full_join(k5) %>%
  mutate(AA=round(AA,4)) %>% 
  mutate(nn=round(nn,4))

k_sum <-
  k %>%
  group_by(k,sd,AA) %>%
  summarize(v=median(v),
            m=median(m),
            A=median(A))

# ggplot(k_sum %>% filter(AA!=min(AA))
#        )+
#   geom_line(aes(x=k,y=A,col=sd,group=interaction(sd,AA)),alpha=.6)

k_sum <- 
  k %>%
  group_by(sd,AA,nn) %>% 
  arrange(k,.by_group = TRUE) %>%
  mutate(d1=(lead(m)-lag(m))/(lead(k)-lag(k))) %>% 
  mutate(d2=(lead(d1)-lag(d1))/(lead(k)-lag(k))) %>% 
  select(k,sd,AA,nn,v,d1,d2) 

k_base <- 
  k_sum %>% 
  filter(sd==1) %>% 
  rename(v_base=m,d1_base=d1,d2_base=d2) %>% 
  ungroup() %>% 
  select(-sd)

k_alts <- 
  k_sum %>% 
  filter(sd!=1)

k_sum <- 
  full_join(k_alts,k_base) %>% 
  group_by(sd,k) %>% 
  mutate(conc_diff=mean(d2-d2_base))

ggplot(k_sum)+
  geom_line(aes(x=k,y=d2_base,col=AA))

ggplot(k)+
  geom_point(aes(x=k,y=m,col=as.factor(sd)),alpha=.4)
  
  





