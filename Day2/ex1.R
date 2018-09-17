##Fitting growth curves example applications 1
setwd("~/Dropbox/Classes/Fall 2018/FSH559/Day2")

dat <- read.table("ex1.dat", skip = 2, header = T)
head(dat)
plot(x = dat$Age, y = dat$Length)

parameters <- list(log_linf = log(8), 
                   log_k = log(50),
                   log_Delta = log(50),
                   a0 = 0,
                   log_a50 = 50,
                   logSigma = 0)

require(TMB)
compile('growth.cpp') #turns .cpp file into a DLL (dynamic link library)
dyn.load(dynlib('growth'))

modnum = 1
data <- list(age = dat$Age, 
             length = dat$Length, 
             modnum = modnum)

if(data$modnum == 1) map <- list(log_Delta=factor(NA),log_a50=factor(NA))
if(data$modnum == 2) map <- list(log_k=factor(NA),log_a0=factor(NA))


vb_model <- MakeADFun(data, parameters,DLL="growth",silent=T, map = map)

vb_fit <- nlminb(vb_model$par, vb_model$fn, vb_model$gr)
vb_best <- vb_model$env$last.par.best;print(vb_best)
vb_rep <- sdreport(vb_model); vb_rep

data <- list(age = dat$Age, 
             length = dat$Length, 
             modnum = 2)

log_model <- MakeADFun(data, parameters,DLL="growth",silent=T)
log_fit <- nlminb(log_model$par, log_model$fn, log_model$gr)
log_rep <- sdreport(log_model); log_rep
log_best <- log_model$env$last.par.best;print(best)

###Predicting lengths
pred <- matrix(NA, nrow = 20, ncol = 3)
pred[,1] <- seq(1,20)
pred[,2] <- exp(vb_best[1])*(1 - exp(-exp(vb_best[2])*(pred[,1]-exp(vb_best[3]))))
pred[,3] <- exp(log_best[1])/(1 + exp(-log(19)*(pred[,1]-exp(log_best[3]))/exp(log_best[2])))

### Plot predicted lengths at ages 1 to 20
plot(x = dat$Age, y = dat$Length, pch = 16, xlab = "Age", ylab = "Length")
lines(x = pred[,1], y = pred[,2], col = "purple", lwd = 2)
lines(x = pred[,1], y = pred[,3], col = "green", lwd = 2)

#Calculating AIC and model weight
p <- 4
vb_aic <- -2*vb_fit$objective + 2*p; vb_aic
log_aic <- -2*log_fit$objective + 2*p; log_aic



