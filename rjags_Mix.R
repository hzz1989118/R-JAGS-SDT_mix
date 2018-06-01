library(R2jags)
library(rjags)
library(gtools)
library(coda)

############ Data Generation ###########
N <- 1000
J <- 10
K <- 2

set.seed(1108)

s_0 <- rdirichlet(N, c(1,1))
alpha_0 <- matrix(rbind(runif(J, 0.4, 1), runif(J, 0, 0.5)), K, J)

z_s <- matrix(0, N, J)
res <- matrix(0, N, J)
  
for (n in 1:N){
  for (j in 1:J){
    z_s[n,j] <- rbinom(1, 1, s_0[n,])
    P_j <- alpha_0[(z_s[n,j]+1), j]
    res[n,j] <- (runif(1) < P_j)*1 
  }
}

###### Save simluated data set used by JAGS later #####

dat_sim <- list("N" = N, "J" = J, 
                "K" = K, "y" = res)

############ JAGS Code (like BUGS) ########
cat("model
    {
    
    for (i in 1:N) {
    for (j in 1:J){
    z[i, j] ~ dbern(s[i,1]);
    y[i, j] ~ dbern(alpha[z[i, j]+1, j]);
    }
    }
    
    for (i in 1:N) {
    s[i, 1:K] ~ ddirch(rep(1, K));
    }
    
    for (k in 1:K) {
    for (j in 1:J){
    alpha[k, j] ~ dbeta(1, 1);
    }
    }
    
    }", 
    file="temp_jag_mix.bug")

######### Fit JAGS with 5000 initial adaptions #######

jags_fit <- jags.model(data = dat_sim,
                      n.chains = 1, n.adapt = 5000,
                      file = 'temp_jag_mix.bug')

#### Only focus on the parameter alpha #####
para <- c("alpha")

#### Update the JAGS and get 10,000 iteration #####
coda_sample <- coda.samples(jags_fit,
                            para,
                            n.iter=10000,
                            thin=5)

plot(coda_sample)

sm <- rbind(cbind(summary(window(coda_sample, 1001))[[2]][seq(1, 20, 2), ], alpha_0[2, ]),
            cbind(summary(window(coda_sample, 1001))[[2]][2*(1:10), ], alpha_0[1, ]))

colnames(sm)[6] <- "TrueValue"

round(sm, 3)


