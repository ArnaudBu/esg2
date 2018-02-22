#### Test script for library esg2 ####
######################################

#---------- Installation of the package ----

# devtools is needed to install from a github repo

# devtools::install_github("arnaudbu/esg2")
library(esg2)

#---------- Creation of a zero coupon curve object ----

# Initialization

curve <- curvezc(method = "continuous",
             rates = c(-0.00316,-0.00269,-0.00203,-0.00122,-0.00022,
                       0.00092,0.00215,0.00342,0.00465,0.00581,
                       0.00684,0.00777,0.00861,0.00933,0.00989,
                       0.0103,0.01061,0.01092,0.01127,0.0117,
                       0.01222,0.01281,0.01345,0.01411,0.01478,
                       0.01546,0.01613,0.01679,0.01743,0.01806,
                       0.01867,0.01926,0.01983,0.02038,0.02092,
                       0.02143,0.02192,0.02239,0.02285,0.02329,
                       0.02371,0.02411,0.0245,0.02488,0.02524,
                       0.02558,0.02592,0.02624,0.02655,0.02685
                       )
             )

# Visualization

curve

print(curve)

plot(curve)

#---------- Creation of a Swaptions object ----

# Initialization

maturities = c(2,3,4,5,7,10,2,3,4,5,7,10,2,3,4,5,7,10,2,3,4,5,7,10,2,3,4,5,7,10,2,3,4,5,7,10,2,3,4,5,7,10,2,3,4,5,7,10,2,3,4,5,7,10,2,3,4,5,7,10)
tenors = c(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4,5,5,5,5,5,5,6,6,6,6,6,6,7,7,7,7,7,7,8,8,8,8,8,8,9,9,9,9,9,9,10,10,10,10,10,10)
vols = c(0.016735,0.009841,0.007156,0.005425,0.004593,0.004138,0.00885,0.006989,0.00564,0.004585,0.004194,0.003977,0.008197,0.006687,0.005471,0.004463,0.004139,0.003962,0.00775,0.006448,0.005312,0.004373,0.004102,0.003955,0.007566,0.006193,0.005223,0.004274,0.004056,0.003936,0.006943,0.005757,0.005051,0.004373,0.004178,0.004086,0.00636,0.00534,0.004901,0.004482,0.004308,0.004234,0.005794,0.004945,0.004761,0.004597,0.004442,0.004383,0.005246,0.004558,0.004629,0.004721,0.004588,0.004532,0.004704,0.004175,0.004503,0.00485,0.004734,0.004682)

swaptions <- swaptions("normal", curve, maturities, tenors, vols, 2)

# Visualization

swaptions

print(swaptions)

plot(swaptions)

#---------- Rate model ----

# Definition of the model

g2model <- g2(curve, horizon = 50, nsimul = 10000)

# Calibration of the model on swaptions prices (~ 1 min on i5)

g2model <- calibrate(g2model, swaptions, maxIter = 100)

# Generate correlated distributions

correl <- cbind(c(1,g2model@rho, 0.25),c(g2model@rho,1, 0), c(0.25, 0, 1))
W <- genW(correl, g2model@nsimul, g2model@horizon)

# Projection of the model

g2model <- project(g2model, Wx = W[,,1], Wy = W[,,2])

# Visualization

g2model

print(g2model)

plot(g2model)

# Deflator test

test_deflator(g2model)

# Get deflator table

def <- deflator(g2model)

# Get zero coupon table at time 10

zc10 <- zctable(g2model, 10)

#---------- Add an action model ----

# Definition

action <- bs(g2model,
             s0 = 100,
             vol = 0.2,
             div = 0.02,
             rho = -0.5,
             W = W[,,3])

# Visualization

action

print(action)

plot(action)

# Get the trajectories

trajAction <- traj(action)

# Martingal test

test_martingal(action)
