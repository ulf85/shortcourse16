##' Solution III
##'
##' Solution to exercise III
##'
##' @author Ulf Schepsmeier
##'
##' version 0.1 (2016-05-11)
##'

library(VineCopula)

data("daxreturns")

dat <- daxreturns[,1:6]

# fit an R-vine with only Gaussian copulas
RVMgauss <- RVineStructureSelect(data = dat, familyset=1, type="RVine")

summary(RVMgauss)


# fit an R-vine with arbitrary copula families
RVM <- RVineStructureSelect(data=dat)
RVM2 <- RVineStructureSelect(data=dat, cores=4) #parallel version

summary(RVM)

RVMcvine <- RVineStructureSelect(data=dat, type="CVine", cores=4)

summary(RVMcvine)


# Summary of the log-lik, AIC, BIC
tab <- data.frame(names=c("Gauss", "R-Vine", "C-Vine"),
                  loglik=c(RVMgauss$logLik, RVM$logLik, RVMcvine$logLik),
                  AIC=c(RVMgauss$AIC, RVM$AIC, RVMcvine$AIC),
                  BIC=c(RVMgauss$BIC, RVM$BIC, RVMcvine$BIC))
tab

# Vuong-test
RVineVuongTest(data=dat, RVM1=RVMgauss, RVM2=RVM)
# test statistic is negativ => second model is better
# p-value smaller than 0.05 => significant

RVineVuongTest(data=dat, RVM1=RVMgauss, RVM2=RVMcvine)
# C-vine > Gauss

RVineVuongTest(data=dat, RVM1=RVM, RVM2=RVMcvine)
# R-vine > C-vine


# Can we improve the model by MLE?
RVMmle <- RVineMLE(data=dat, RVM=RVM)
RVM$loglik
RVMmle$loglik
# Yes a little bit

RVMtrunc <- RVineStructureSelect(data=dat, trunclevel = 2, cores=4)
RVMtrunc$AIC
RVM$AIC
# Gap of 100 AIC points

RVMtrunc2 <- RVineStructureSelect(data=dat, trunclevel = 3, cores=4)
RVMtrunc2$AIC
RVM$AIC
# Gap of 30 AIC points => OK


