# Module 4 — Solve LP Model in R (Weigelt Corporation)
# Author: Jaipreet Kaur

install.packages("lpSolve")  
library(lpSolve)

# Profits
profits <- c(420,360,300, 420,360,300, 420,360,300)

# Capacity
cap <- c(P1=750, P2=900, P3=450)

# Constraints
A <- list(); dir <- c(); rhs <- c()

# Plant capacities
A <- append(A, list(
  c(1,1,1, 0,0,0, 0,0,0),
  c(0,0,0, 1,1,1, 0,0,0),
  c(0,0,0, 0,0,0, 1,1,1)
))
dir <- c(dir, "<=", "<=", "<=")
rhs <- c(rhs, cap)

# Storage
A <- append(A, list(
  c(20,15,12, 0,0,0, 0,0,0),
  c(0,0,0, 20,15,12, 0,0,0),
  c(0,0,0, 0,0,0, 20,15,12)
))
dir <- c(dir, "<=", "<=", "<=")
rhs <- c(rhs, 13000, 12000, 5000)

# Demand upper bounds
A <- append(A, list(
  c(1,0,0, 1,0,0, 1,0,0),
  c(0,1,0, 0,1,0, 0,1,0),
  c(0,0,1, 0,0,1, 0,0,1)
))
dir <- c(dir, "<=", "<=", "<=")
rhs <- c(rhs, 900, 1200, 750)

# Equal percentage constraints
A <- append(A, list(
  900*c(1,1,1,0,0,0,0,0,0) - 750*c(0,0,0,1,1,1,0,0,0),
  450*c(1,1,1,0,0,0,0,0,0) - 750*c(0,0,0,0,0,0,1,1,1)
))
dir <- c(dir, "=", "=")
rhs <- c(rhs, 0, 0)

const.mat <- do.call(rbind, A)
const.dir <- dir
const.rhs <- rhs

sol <- lp(direction="max",
          objective.in=profits,
          const.mat=const.mat,
          const.dir=const.dir,
          const.rhs=const.rhs)

vars <- c("P1L","P1M","P1S","P2L","P2M","P2S","P3L","P3M","P3S")
plan <- setNames(sol$solution, vars)
profit_opt <- sol$objval

print(plan)
cat("Total Profit =", profit_opt, "\n")
