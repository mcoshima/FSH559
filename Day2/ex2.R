setwd("~/Dropbox/Classes/Fall 2018/FSH559/Day2")
require(scales) #for plotting alpha argument
require(TMB)
dat <- read.table("ex2.dat", skip = 4, header = T)
head(dat)

#Make matrices of prey densities and consumption rates
pred = dat[,1]
n_row <- nrow(dat)

compile("predation.cpp")
dyn.load(dynlib("predation"))

parameters <- list(log_alpha1 = 1, 
                   log_alpha2 = 1,
                   log_alpha3 = 1, 
                   beta1 = 0,
                   beta2 = 0,
                   beta3 = 0,
                   gamma1 = 0,
                   gamma2 = 1,
                   gamma3 = 1,
                   lambda1 = 1,
                   lambda2 = 1,
                   lambda3 = 1)
rep <- list()
for(i in 1:4){
modnum = i
data <- list(pred = pred,
             prey_1 = dat[,2],
             prey_2 = dat[,3],
             prey_3 = dat[,4],
             consump_1 = dat[,5],
             consump_2 = dat[,6],
             consump_3 = dat[,7],
             Model_Num = modnum,
             n_row = n_row)

if(modnum == 1) map <- list(beta1 = factor(NA), beta2 = factor(NA), beta3 = factor(NA), lambda1 = factor(NA), lambda2 = factor(NA), lambda3 = factor(NA), gamma1 = factor(NA), gamma2 = factor(NA), gamma3 = factor(NA))
if(modnum == 2) map <- list(lambda1 = factor(NA), lambda2 = factor(NA), lambda3 = factor(NA), gamma1 = factor(NA), gamma2 = factor(NA), gamma3 = factor(NA))
if(modnum == 3) map <- list(lambda1 = factor(NA), lambda2 = factor(NA), lambda3 = factor(NA))
if(modnum == 4) map <- list(gamma1 = factor(NA), gamma2 = factor(NA), gamma3 = factor(NA))

model <- MakeADFun(data, parameters,DLL="predation",silent=T, map = map)
fit <- nlminb(model$par, model$fn, model$gr)
rep[[i]] <- sdreport(model)
}

## Make fit lines using parameter estimates
# Model 1: linear
m1p1 <- exp(as.numeric(unlist(rep[[1]])[1]))* seq(0,2,by = 0.1)
m1p2 <- exp(as.numeric(unlist(rep[[1]])[2]))* seq(0,2,by = 0.1)
m1p3 <- exp(as.numeric(unlist(rep[[1]])[3]))* seq(0,2,by = 0.1)
# Model 2: Holling Type 2
m2p1 <- exp(as.numeric(unlist(rep[[2]])[1]))* seq(0,2,by = 0.1)/(1+as.numeric(unlist(rep[[2]])[4])*seq(0,2,0.1))
m2p2 <- exp(as.numeric(unlist(rep[[2]])[2]))* seq(0,2,by = 0.1)/(1+as.numeric(unlist(rep[[2]])[5])*seq(0,2,0.1))
m2p3 <- exp(as.numeric(unlist(rep[[2]])[3]))* seq(0,2,by = 0.1)/(1+as.numeric(unlist(rep[[2]])[6])*seq(0,2,0.1))
# Model 3: Sigmoid
m3p1 <- (exp(as.numeric(unlist(rep[[3]])[1]))* seq(0.01,2,by = 0.1)*(seq(0.01,2,0.1)^as.numeric(unlist(rep[[3]])[7])-1))/(1+as.numeric(unlist(rep[[3]])[4])*seq(0.01,2,0.1)^as.numeric(unlist(rep[[3]])[7]))
m3p2 <- (exp(as.numeric(unlist(rep[[3]])[2]))* seq(0.01,2,by = 0.1)*(seq(0.01,2,0.1)^as.numeric(unlist(rep[[3]])[8])-1))/(1+as.numeric(unlist(rep[[3]])[5])*seq(0.01,2,0.1)^as.numeric(unlist(rep[[3]])[9]))
m3p3 <- (exp(as.numeric(unlist(rep[[3]])[3]))* seq(0.01,2,by = 0.1)*(seq(0.01,2,0.1)^as.numeric(unlist(rep[[3]])[9])-1))/(1+as.numeric(unlist(rep[[3]])[6])*seq(0.01,2,0.1)^as.numeric(unlist(rep[[3]])[9]))

# Model 4: Pre-emption
m4p1 <- (exp(as.numeric(unlist(rep[[4]])[1]))* seq(0,2,by = 0.1))/(1+as.numeric(unlist(rep[[3]])[4])*seq(0,2,by = 0.1)+1+as.numeric(unlist(rep[[4]])[7]))*seq(0,2,by=0.1)
m4p2 <- (exp(as.numeric(unlist(rep[[4]])[2]))* seq(0,2,by = 0.1))/(1+as.numeric(unlist(rep[[3]])[5])*seq(0,2,by = 0.1)+1+as.numeric(unlist(rep[[4]])[8]))*seq(0,2,by=0.1)
m4p3 <- (exp(as.numeric(unlist(rep[[4]])[3]))* seq(0,2,by = 0.1))/(1+as.numeric(unlist(rep[[3]])[6])*seq(0,2,by = 0.1)+1+as.numeric(unlist(rep[[4]])[9]))*seq(0,2,by=0.1)

## Plot consumption rate vs predator density by prey type for 4 models
par(mfrow = c(2,2))

plot(x = dat[,1], y = dat[,5], pch = 16, xlab = "Predator Density", ylab = "Consumption Rates", xlim = c(0,2), ylim = c(0,3.5), col = "grey50", main = "Linear")
points(x = dat[,1], y = dat[,6], pch = 16, col = alpha("mediumpurple2", 0.5))
points(x = dat[,1], y = dat[,7], pch = 16, col = alpha("sienna1", 0.5))
lines(x = seq(0,2,by = 0.1), y = m1p1, lwd = 2, col = "grey30")
lines(x = seq(0,2,by = 0.1), y = m1p2, lwd = 2, col = "mediumpurple2")
lines(x = seq(0,2,by = 0.1), y = m1p3, lwd = 2, col = "sienna1")

plot(x = dat[,1], y = dat[,5], pch = 16, xlab = "Predator Density", ylab = "Consumption Rates", xlim = c(0,2), ylim = c(0,3.5), col = "grey50", main = "Holling Type II")
points(x = dat[,1], y = dat[,6], pch = 16, col = alpha("mediumpurple2", 0.5))
points(x = dat[,1], y = dat[,7], pch = 16, col = alpha("sienna1", 0.5))
lines(x = seq(0,2,by = 0.1), y = m2p1, lwd = 2, col = "grey30")
lines(x = seq(0,2,by = 0.1), y = m2p2, lwd = 2, col = "mediumpurple2")
lines(x = seq(0,2,by = 0.1), y = m2p3, lwd = 2, col = "sienna1")

plot(x = dat[,1], y = dat[,5], pch = 16, xlab = "Predator Density", ylab = "Consumption Rates", xlim = c(0,2), ylim = c(-0.5,3.5), col = "grey50", main = "Sigmoid")
points(x = dat[,1], y = dat[,6], pch = 16, col = alpha("mediumpurple2", 0.5))
points(x = dat[,1], y = dat[,7], pch = 16, col = alpha("sienna1", 0.5))
lines(x = seq(0.01,2,by = 0.1), y = m3p1, lwd = 2, col = "grey30")
lines(x = seq(0.01,2,by = 0.1), y = m3p2, lwd = 2, col = "mediumpurple2")
lines(x = seq(0.01,2,by = 0.1), y = m3p3, lwd = 2, col = "sienna1")

plot(x = dat[,1], y = dat[,5], pch = 16, xlab = "Predator Density", ylab = "Consumption Rates", xlim = c(0,2), ylim = c(-0.5,3.5), col = "grey50", main = "Pre-emptive")
points(x = dat[,1], y = dat[,6], pch = 16, col = alpha("mediumpurple2", 0.5))
points(x = dat[,1], y = dat[,7], pch = 16, col = alpha("sienna1", 0.5))
lines(x = seq(0,2,by = 0.1), y = m4p1, lwd = 2, col = "grey30")
lines(x = seq(0,2,by = 0.1), y = m4p2, lwd = 2, col = "mediumpurple2")
lines(x = seq(0,2,by = 0.1), y = m4p3, lwd = 2, col = "sienna1")


