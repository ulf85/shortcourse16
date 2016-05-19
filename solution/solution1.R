##' Solution I
##'
##' Solution to exercise I
##'
##' @author Ulf Schepsmeier
##'
##' version 0.1 (2016-05-10)
##'

# load library
library(VineCopula)

# Load data
data(daxreturns)

# select columns
colnames(daxreturns)
select <- c(1,4,6,11)
dat <- daxreturns[,select]
dat <- as.copuladata(dat)

# easy version (all in one)
pairs(dat)


# Kendall's tau
cor(dat, method = "kendall")

BiCopMetaContour(u1=dat[,1], u2=dat[,2])
BiCopMetaContour(u1=dat[,1], u2=dat[,3])
BiCopMetaContour(u1=dat[,1], u2=dat[,4])

BiCopMetaContour(u1=dat[,2], u2=dat[,3])
BiCopMetaContour(u1=dat[,2], u2=dat[,4])
BiCopMetaContour(u1=dat[,3], u2=dat[,4])

# nicer with kdecopula
library(kdecopula)
fit <- kdecop(dat[,1:2])
contour(fit)

# create lambda-function plots
# From the pairs plot I suggest a Clayton copula
cop <- BiCop(family=3, par=BiCopTau2Par(family=3,
                                        tau = cor(dat[,1:2], method="kendall")[1,2]))
op <- par(mfrow = c(1, 3))
BiCopLambda(u1=dat[,1], u2=dat[,2])  # empirical lambda-function
BiCopLambda(cop)	# theoretical lambda-function
BiCopLambda(u1=dat[, 1], u2=dat[, 2], cop)	# both
par(op)

# suggestions for the pairs
# ALV-BMW: Clayton or survival Gumbel
# ALV-DBK: Student's t-copula
# ALV-MUV2: Student's t-copula
# BMW-DBK: t or a BB-copula (there may be asymmetric tails)
# BMW-MUV2: t or a BB-copula (there may be asymmetric tails)
# DBK-MUV2: t

# with AIC
BiCopSelect(u1=dat[,1], u2=dat[,2])
BiCopSelect(u1=dat[,1], u2=dat[,3])
BiCopSelect(u1=dat[,1], u2=dat[,4])
BiCopSelect(u1=dat[,2], u2=dat[,3])
BiCopSelect(u1=dat[,2], u2=dat[,4])
BiCopSelect(u1=dat[,3], u2=dat[,4])

# with BIC
BiCopSelect(u1=dat[,1], u2=dat[,2], selectioncrit="BIC")


# Goodness-of-fit tests
BiCopGofTest(u1=dat[,1], u2=dat[,2], family = 3, B=100) # bootstrapped p-value (bug)
BiCopGofTest(u1=dat[,1], u2=dat[,2], family = 3, B=0) # asymptotic p-value
# p-value smaller than 0.05

BiCopGofTest(u1=dat[,1], u2=dat[,2], family = 14) # better, p-value > 0.05

BiCopGofTest(u1=dat[,1], u2=dat[,2], family = 3, method = "kendall") # p-value = 0
BiCopGofTest(u1=dat[,1], u2=dat[,2], family = 14, method = "kendall") # p-value > 0.05


BiCopVuongClarke(u1=dat[,1], u2=dat[,2]) # error
# only familes with a positive dependence
BiCopVuongClarke(u1=dat[,1], u2=dat[,2], familyset = c(1:10, 13,14,16:20,104,204,114,214))
