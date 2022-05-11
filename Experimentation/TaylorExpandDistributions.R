# Explore possible algorithm functions and shock distributions

# Cobb-Douglas ----
A <- function(n,e,g=.5){
  return(n^g*e^(1-g))
}

mu <- 0
sd <- 1
dist <- function(x){
  dlnorm(x,mu,sd)
}

gs <- seq(0,1,length.out=100)

moment <- function(g){
  piece <- function(x){
    (x^(1-g))*dist(x)
  }
  mom=integrate(piece,0,Inf)[[1]]
  return(mom)
}
moment <- Vectorize(moment)

plot(gs,moment(gs))

EA <- function(x,g){
  x^g*moment(g)
}





integrate(test,0,Inf)
  
