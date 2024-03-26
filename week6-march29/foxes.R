library(rethinking)

data(foxes)

library(ggplot2)
library(dplyr)


d <- foxes
d$C <- standardize(d$avgfood)
d$A <- standardize(d$area)
d$W <- standardize(d$weight)
d$G <- standardize(d$groupsize)

m.1 <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + bA * A,
    a ~ dnorm(0, 0.2),
    bA ~ dnorm(0, 0.2),
    sigma ~ dexp(1)
    ),
  data = d
)

set.seed(10)

prior <- extract.prior( m.1  )

mu <- link( m.1  , post=prior , data=list( A=c(-2,2) ) )

plot( NULL , xlim=c(-2,2) , ylim=c(-2,2) )

for ( i in 1:50 ) lines( c(-2,2) , mu[i,] , col=col.alpha("black",0.4) )

A_seq <- seq( from=-3 , to=3.2 , length.out=30 )

mu <- link( m.1 , data=list(A=A_seq) )

mu.mean <- apply( mu , 2, mean )

mu.PI <- apply( mu , 2 , PI )


plot( W ~ A , data=d , col=rangi2 )

lines( A_seq , mu.mean , lwd=2 )

shade( mu.PI , A_seq )

precis(m.1)


sims <- ((sim(m.1, n = 100))) |> t() |> as.data.frame() |>
  mutate(A = d$A) |>
  tidyr::pivot_longer(-c(A), names_to = "simn")

ggplot(d, aes(A, W))  + geom_point(color = "red") + geom_point(data = sims, aes(A, value), color = "blue", alpha = .1)

m.2 <- quap(
  
  alist(
    
    W ~ dnorm( mu , sigma ) ,
    
    mu <- a + bA*A + bC*C,
    
    a ~ dnorm( 0 , 0.2 ) ,
    
    bA ~ dnorm( 0 , 0.5 ) ,
    
    bC ~ dnorm( 0 , 0.5 ) ,
    
    sigma ~ dexp( 1 )
    
  ) , data=d )

precis(m.2) # C conditioned on A



plot( coeftab( m.2 , m.1) , pars=c("bA", "bC") )


m.3 <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + bC * C,
    a ~ dnorm(0, 0.2),
    bC ~ dnorm(0, 0.2),
    sigma ~ dexp(1)
  ),
  data = d
) # total causal effect of C

precis(m.3)

plot( coeftab( m.2 , m.1, m.3) , pars=c("bA", "bC") )

d2 <- d |> mutate(A = 0)

sims2 <- ((sim(m.1, n = 100, data = d2))) |> t() |> as.data.frame() |>
  mutate(A = d2$A, C = d2$C)  |>
  tidyr::pivot_longer(-c(A, C), names_to = "simn")

ggplot(d2, aes(C, W))  +
  geom_point() +
  geom_point(data = sims2, aes(C, value), color = "blue", alpha = .1)


m.4 <- quap(
    alist(
      W ~ dnorm(mu, sigma),
      
      mu <- a +  bC*C * bG*G,
      
      a ~ dnorm( 0 , 0.2 ) ,
      
      bC ~ dnorm( 0 , .5 ) ,
      
      bG ~ dnorm(0, .5),
      
      sigma ~ dexp(1)
    ),
    data = d
  ) 

precis(m.4)

plot( coeftab( m.2 , m.1, m.3, m.4) , pars=c("bA", "bC", "bG") )

