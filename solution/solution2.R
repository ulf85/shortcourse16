##' Exercise II
##'
##' Solution to exercise II
##'
##' @author Ulf Schepsmeier
##'
##' version 0.1 (2016-05-11)
##'

library(VineCopula)

# Matrix for R-vine structure
Matrix <- matrix(c(6,0,0,0,0,0,
                   2,5,0,0,0,0,
                   3,2,4,0,0,0,
                   4,3,2,3,0,0,
                   1,4,3,2,2,0,
                   5,1,1,1,1,1),
                 nrow=6, ncol=6,
                 byrow = TRUE)

family <- matrix(1, nrow=6, ncol=6)
par <- matrix(0.5, nrow=6, ncol=6)
par2 <- matrix(0,nrow=6, ncol=6)

RVM <- RVineMatrix(Matrix=Matrix, family=family, par=par, par2=par2)

# validate the R-vine matrix
RVineMatrixCheck(Matrix)

# plot the R-vine tree structure
plot(RVM)

# with families
plot(RVM, edge.labels = "family")

# with Kendall's tau
plot(RVM, edge.labels = "tau")

# or RVineTreePlot(RVM, tree=1)

