###########################
#### Utility Functions ####
###########################

# rm(list = ls())
# 
# setwd("C:/work/Forschung/DID/02_Data")

 # setwd("C:/work/Forschung/DID/06_HPC/01_Script")

library(foreach)
library(doParallel)

library(fect)
library(did)
library(staggered)
library(didimputation)
library(etwfe)
library(feisr)

library(ggplot2)
library(viridisLite)
library(extrafont)
loadfonts()




# Load functions
source("01_DiD-Smulation-Program.R")

# N cores
ncores <- parallel::detectCores(logical = FALSE)

print(ncores)



##################
#### Test RUN ####
##################




### Test 0 - normal TWFE with homogeneous treatment effect

test0 <- sim_did(
  seed = 1527, # set seed
  N = 300, # number of cross-sectional units
  T = 50, # Number of time periods
  R = 1000, # Number of simulation runs
  scale.ps = 5, # Scaling parameter for binomial
  b.b = c(1, 0, 0, -0), # Cross-sectional selection
  b.x = c(0, 0, 0, 0), # time-varying selection, emp correlation bet D and x
  b.trend = c(0.2), # Trend in Y
  treat.sd = 3, # SD for probability distribution of treatment timing (determines where treatment happens)
  b.nonparallel = c(0.00), # Extra trend for treated (vs. non-treated) # ADD Extra trend for early treated vs late?
  beta = 1, # Treatment effect (and its distribution)
  beta.t = rep(0.1, 50), # Temporal distribution of treatment effect
  beta.late = FALSE, # early vs late with heterogeneous ATT, factor for late (early will have 1/beta.late), 
  size.late = 0.5, # share of late treatment group in case of beta.late 
  crs = ncores, #Parallel cores
)

save(test0, file = "Hettreat_largeT_Sim0.RData")








### Test 1 - normal TWFE with heterogeneous treatment effect

test1 <- sim_did(
  seed = 1527, # set seed
  N = 300, # number of cross-sectional units
  T = 50, # Number of time periods
  R = 1000, # Number of simulation runs
  scale.ps = 5, # Scaling parameter for binomial
  b.b = c(1, 0, 0, -0), # Cross-sectional selection
  b.x = c(0, 0, 0, 0), # time-varying selection, emp correlation bet D and x
  b.trend = c(0.2), # Trend in Y
  treat.sd = 3, # SD for probability distribution of treatment timing (determines where treatment happens)
  b.nonparallel = c(0.00), # Extra trend for treated (vs. non-treated) # ADD Extra trend for early treated vs late?
  beta = 1, # Treatment effect (and its distribution)
  beta.t = c(0.15, 0.25, 0.3, 0.2, 0.1), # Temporal distribution of treatment effect
  beta.late = 0.5, # early vs late with heterogeneous ATT, factor for late (early will have 1/beta.late), 
  size.late = 0.5, # share of late treatment group in case of beta.late 
  crs = ncores, #Parallel cores
)

save(test1, file = "Hettreat_largeT_Sim1.RData")


# # Event plot
# test1.pl <- sim_plot2(test1, xlim = c(-3, 8), ylim = c(-0.1, 0.6))
# test1.pl
# 
# 
# # tr.pl <- ggplot(data = test1$tr_plot[[2]],
# #                 aes(x=t, y=Treatx)) +
# #   geom_count() +
# #   geom_smooth()
# # tr.pl
# 
# 
# png(filename = "../03_Output/Test1.png", 
#     width = 14, height = 8, units = "in", res = 300,
#     type = "cairo")
# test1.pl
# dev.off()
# 
# 
# # att plot
# test1.pl.att <- sim_plot_att2(test1, 
#                             use.rmse = FALSE,
#                             use.ci = FALSE, 
#                             violin = TRUE,
#                             xlim = c(-0.1, 0.4))
# test1.pl.att
# 
# 
# png(filename = "../03_Output/Test1_att.png", 
#     width = 8, height = 8, units = "in", res = 300,
#     type = "cairo")
# test1.pl.att
# dev.off()
# 






### Test8 - non-parallel trends with heterogeneous treatment effect + anticipation effect

test8 <- sim_did(
  seed = 1527, # set seed
  N = 300, # number of cross-sectional units
  T = 50, # Number of time periods
  R = 1000, # Number of simulation runs
  scale.ps = 5, # Scaling parameter for binomial
  b.b = c(1, 0, 0, -0), # Cross-sectional selection
  b.x = c(0, 0, 0, 0), # time-varying selection, emp correlation bet D and x
  b.trend = c(0.2), # Trend in Y
  treat.sd = 3, # SD for probability distribution of treatment timing (determines where treatment happens)
  b.nonparallel = c(0.00), # Extra trend for treated (vs. non-treated) # ADD Extra trend for early treated vs late?
  beta = 1, # Treatment effect (and its distribution)
  beta.t = c(0.15, 0.25, 0.3, 0.2, 0.1), # Temporal distribution of treatment effect
  beta.late = 0.5, # early vs late with heterogeneous ATT, factor for late (early will have 1/beta.late), 
  size.late = 0.5, # share of late treatment group in case of beta.late 
  crs = ncores, #Parallel cores
  anticipaton.t = c(0, 0, -0.05, -0.1)
)

save(test8, file = "Hettreat_largeT_Sim8.RData")

# test8.pl <- sim_plot2(test8, xlim = c(-3, 8), ylim = c(-0.1, 0.6))
# test8.pl
# 
# # tr.pl <- ggplot(data = test8$tr_plot[[2]],
# #                 aes(x=t, y=Treatx)) +
# #   geom_count() +
# #   geom_smooth()
# # tr.pl
# 
# png(filename = "../03_Output/Test8.png", 
#     width = 14, height = 8, units = "in", res = 300,
#     type = "cairo")
# test8.pl
# dev.off()
# 
# 
# # att plot
# test8.pl.att <- sim_plot_att2(test8,
#                              use.rmse = FALSE,
#                              use.ci = FALSE, 
#                              violin = TRUE,
#                              xlim = c(-0.1, 0.4))
# test8.pl.att
# 
# png(filename = "../03_Output/Test8_att.png", 
#     width = 8, height = 8, units = "in", res = 300,
#     type = "cairo")
# test8.pl.att
# dev.off()
# 








### Test 9 - normal TWFE with trend-breaking treatment effect

test9 <- sim_did(
  seed = 1527, # set seed
  N = 300, # number of cross-sectional units
  T = 50, # Number of time periods
  R = 1000, # Number of simulation runs
  scale.ps = 5, # Scaling parameter for binomial
  b.b = c(1, 0, 0, -0), # Cross-sectional selection
  b.x = c(0, 0, 0, 0), # time-varying selection, emp correlation bet D and x
  b.trend = c(0.2), # Trend in Y
  treat.sd = 3, # SD for probability distribution of treatment timing (determines where treatment happens)
  b.nonparallel = c(0.00), # Extra trend for treated (vs. non-treated) # ADD Extra trend for early treated vs late?
  beta = 1, # Treatment effect (and its distribution)
  beta.t = cumsum(c(0.05, rep(0.03214285, 50-1))), # Temporal distribution of treatment effect
  beta.late = 0.5, # early vs late with heterogeneous ATT, factor for late (early will have 1/beta.late), 
  size.late = 0.5, # share of late treatment group in case of beta.late 
  crs = ncores, #Parallel cores
)

save(test9, file = "Hettreat_largeT_Sim9.RData")


# # Event plot
# test9.pl <- sim_plot2(test9, xlim = c(-3, 8), ylim = c(-0.1, 0.6))
# test9.pl
# 
# 
# # tr.pl <- ggplot(data = test9$tr_plot[[2]],
# #                 aes(x=t, y=Treatx)) +
# #   geom_count() +
# #   geom_smooth()
# # tr.pl
# 
# 
# png(filename = "../03_Output/Test9.png", 
#     width = 14, height = 8, units = "in", res = 300,
#     type = "cairo")
# test9.pl
# dev.off()
# 
# 
# # att plot
# test9.pl.att <- sim_plot_att2(test9, 
#                              use.rmse = FALSE,
#                              use.ci = FALSE, 
#                              violin = TRUE,
#                              xlim = c(-0.1, 0.4))
# test9.pl.att
# 
# 
# png(filename = "../03_Output/Test9_att.png", 
#     width = 8, height = 8, units = "in", res = 300,
#     type = "cairo")
# test9.pl.att
# dev.off()
# 







### Test 9a - normal TWFE with trend-breaking treatment effect -- no group-specific treatment effect

test9a <- sim_did(
  seed = 1527, # set seed
  N = 300, # number of cross-sectional units
  T = 50, # Number of time periods
  R = 1000, # Number of simulation runs
  scale.ps = 5, # Scaling parameter for binomial
  b.b = c(1, 0, 0, -0), # Cross-sectional selection
  b.x = c(0, 0, 0, 0), # time-varying selection, emp correlation bet D and x
  b.trend = c(0.2), # Trend in Y
  treat.sd = 3, # SD for probability distribution of treatment timing (determines where treatment happens)
  b.nonparallel = c(0.00), # Extra trend for treated (vs. non-treated) # ADD Extra trend for early treated vs late?
  beta = 1, # Treatment effect (and its distribution)
  beta.t = cumsum(c(0.05, rep(0.03214285, 50-1))), # Temporal distribution of treatment effect
  beta.late = FALSE, # early vs late with heterogeneous ATT, factor for late (early will have 1/beta.late), 
  size.late = 0.5, # share of late treatment group in case of beta.late 
  crs = ncores, #Parallel cores
)

save(test9a, file = "Hettreat_largeT_Sim9a.RData")


# # Event plot
# test9.pl <- sim_plot2(test9, xlim = c(-3, 8), ylim = c(-0.1, 0.6))
# test9.pl
# 
# 
# # tr.pl <- ggplot(data = test9$tr_plot[[2]],
# #                 aes(x=t, y=Treatx)) +
# #   geom_count() +
# #   geom_smooth()
# # tr.pl
# 
# 
# png(filename = "../03_Output/Test9.png", 
#     width = 14, height = 8, units = "in", res = 300,
#     type = "cairo")
# test9.pl
# dev.off()
# 
# 
# # att plot
# test9.pl.att <- sim_plot_att2(test9, 
#                              use.rmse = FALSE,
#                              use.ci = FALSE, 
#                              violin = TRUE,
#                              xlim = c(-0.1, 0.4))
# test9.pl.att
# 
# 
# png(filename = "../03_Output/Test9_att.png", 
#     width = 8, height = 8, units = "in", res = 300,
#     type = "cairo")
# test9.pl.att
# dev.off()
# 









### Test10 - normal TWFE, homogenous treatmet and homgeneous treatment timing + time-varying selection

test10 <- sim_did(
  seed = 1527, # set seed
  N = 300, # number of cross-sectional units
  T = 50, # Number of time periods
  R = 1000, # Number of simulation runs
  scale.ps = 5, # Scaling parameter for binomial
  b.b = c(1, 0, 0, -0), # Cross-sectional selection
  b.x = c(0, 2, 0, 0), # time-varying selection, emp correlation bet D and x (N, trending cont, binary cumul N, scaled t)
  b.trend = c(0.2), # Trend in Y
  treat.sd = 3, # SD for probability distribution of treatment timing (determines where treatment happens)
  b.nonparallel = c(0.00), # Extra trend for treated (vs. non-treated) # ADD Extra trend for early treated vs late?
  beta = 1, # Treatment effect (and its distribution)
  beta.t = c(0.15, 0.25, 0.3, 0.2, 0.1), # Temporal distribution of treatment effect
  beta.late = 0.5, # early vs late with heterogeneous ATT, factor for late (early will have 1/beta.late), 
  size.late = 0.5, # share of late treatment group in case of beta.late 
  crs = ncores, #Parallel cores
)

save(test10, file = "Hettreat_largeT_Sim10.RData")

# test10.pl <- sim_plot2(test10, xlim = c(-3, 8), ylim = c(-0.1, 0.6))
# test10.pl
# 
# # tr.pl <- ggplot(data = test10.pl$tr_plot[[2]],
# #                 aes(x=t, y=Treatx)) +
# #   geom_count() +
# #   geom_smooth()
# # tr.pl
# 
# 
# png(filename = "../03_Output/Test10.png", 
#     width = 14, height = 8, units = "in", res = 300,
#     type = "cairo")
# test10.pl
# dev.off()
# 
# 
# # att plot
# test10.pl.att <- sim_plot_att2(test10,
#                               use.rmse = FALSE,
#                               use.ci = FALSE, 
#                               violin = TRUE,
#                               xlim = c(-0.1, 0.4))
# test10.pl.att
# 
# png(filename = "../03_Output/Test10_att.png", 
#     width = 8, height = 8, units = "in", res = 300,
#     type = "cairo")
# test10.pl.att
# dev.off()














### Test 12 - non-parallel trends (weaker) with heterogeneous treatment effect

test12 <- sim_did(
  seed = 1527, # set seed
  N = 300, # number of cross-sectional units
  T = 50, # Number of time periods
  R = 1000, # Number of simulation runs
  scale.ps = 5, # Scaling parameter for binomial
  b.b = c(1, 0, 0, -0), # Cross-sectional selection
  b.x = c(0, 0, 0, 0), # time-varying selection, emp correlation bet D and x
  b.trend = c(0.2), # Trend in Y
  treat.sd = 3, # SD for probability distribution of treatment timing (determines where treatment happens)
  b.nonparallel = c(0.025), # Extra trend for treated (vs. non-treated) # ADD Extra trend for early treated vs late?
  beta = 1, # Treatment effect (and its distribution)
  beta.t = c(0.15, 0.25, 0.3, 0.2, 0.1), # Temporal distribution of treatment effect
  beta.late = 0.5, # early vs late with heterogeneous ATT, factor for late (early will have 1/beta.late), 
  size.late = 0.5, # share of late treatment group in case of beta.late 
  crs = ncores, #Parallel cores
)

save(test12, file = "Hettreat_largeT_Sim12.RData")

# test12.pl <- sim_plot2(test12, xlim = c(-3, 8), ylim = c(-0.1, 0.6))
# test12.pl
# 
# # tr.pl <- ggplot(data = test12$tr_plot[[2]],
# #                 aes(x=t, y=Treatx)) +
# #   geom_count() +
# #   geom_smooth()
# # tr.pl
# 
# png(filename = "../03_Output/test12.png", 
#     width = 14, height = 8, units = "in", res = 300,
#     type = "cairo")
# test12.pl
# dev.off()
# 
# 
# # att plot
# test12.pl.att <- sim_plot_att2(test12,
#                              use.rmse = FALSE,
#                              use.ci = FALSE, 
#                              violin = TRUE,
#                              xlim = c(-0.1, 0.4))
# test12.pl.att
# 
# png(filename = "../03_Output/test12_att.png", 
#     width = 8, height = 8, units = "in", res = 300,
#     type = "cairo")
# test12.pl.att
# dev.off()
# 
# 
# 








### Test 13 - non-parallel trends (weaker) with heterogeneous treatment effect + anticipation

test13 <- sim_did(
  seed = 1527, # set seed
  N = 300, # number of cross-sectional units
  T = 50, # Number of time periods
  R = 1000, # Number of simulation runs
  scale.ps = 5, # Scaling parameter for binomial
  b.b = c(1, 0, 0, -0), # Cross-sectional selection
  b.x = c(0, 0, 0, 0), # time-varying selection, emp correlation bet D and x
  b.trend = c(0.2), # Trend in Y
  treat.sd = 3, # SD for probability distribution of treatment timing (determines where treatment happens)
  b.nonparallel = c(0.025), # Extra trend for treated (vs. non-treated) # ADD Extra trend for early treated vs late?
  beta = 1, # Treatment effect (and its distribution)
  beta.t = c(0.15, 0.25, 0.3, 0.2, 0.1), # Temporal distribution of treatment effect
  beta.late = 0.5, # early vs late with heterogeneous ATT, factor for late (early will have 1/beta.late), 
  size.late = 0.5, # share of late treatment group in case of beta.late 
  crs = ncores, #Parallel cores
  anticipaton.t = c(0, 0, -0.05, -0.1)
)

save(test13, file = "Hettreat_largeT_Sim13.RData")

# test13.pl <- sim_plot2(test13, xlim = c(-3, 8), ylim = c(-0.1, 0.6))
# test13.pl
# 
# # tr.pl <- ggplot(data = test13$tr_plot[[2]],
# #                 aes(x=t, y=Treatx)) +
# #   geom_count() +
# #   geom_smooth()
# # tr.pl
# 
# png(filename = "../03_Output/test13.png", 
#     width = 14, height = 8, units = "in", res = 300,
#     type = "cairo")
# test13.pl
# dev.off()
# 
# 
# # att plot
# test13.pl.att <- sim_plot_att2(test13,
#                              use.rmse = FALSE,
#                              use.ci = FALSE, 
#                              violin = TRUE,
#                              xlim = c(-0.1, 0.4))
# test13.pl.att
# 
# png(filename = "../03_Output/test13_att.png", 
#     width = 8, height = 8, units = "in", res = 300,
#     type = "cairo")
# test13.pl.att
# dev.off()
# 
# 
# 






### Test 14 - non-parallel trends (weaker) with heterogeneous treatment effect + anticipation + time varying selection

test14 <- sim_did(
  seed = 1527, # set seed
  N = 300, # number of cross-sectional units
  T = 50, # Number of time periods
  R = 1000, # Number of simulation runs
  scale.ps = 5, # Scaling parameter for binomial
  b.b = c(1, 0, 0, -0), # Cross-sectional selection
  b.x = c(0, 2, 0, 0), # time-varying selection, emp correlation bet D and x (N, trending cont, binary cumul N, scaled t)
  b.trend = c(0.2), # Trend in Y
  treat.sd = 3, # SD for probability distribution of treatment timing (determines where treatment happens)
  b.nonparallel = c(0.025), # Extra trend for treated (vs. non-treated) # ADD Extra trend for early treated vs late?
  beta = 1, # Treatment effect (and its distribution)
  beta.t = c(0.15, 0.25, 0.3, 0.2, 0.1), # Temporal distribution of treatment effect
  beta.late = 0.5, # early vs late with heterogeneous ATT, factor for late (early will have 1/beta.late), 
  size.late = 0.5, # share of late treatment group in case of beta.late 
  crs = ncores, #Parallel cores
  anticipaton.t = c(0, 0, -0.05, -0.1)
)

save(test14, file = "Hettreat_largeT_Sim14.RData")

# test14.pl <- sim_plot2(test14, xlim = c(-3, 8), ylim = c(-0.1, 0.6))
# test14.pl
# 
# # tr.pl <- ggplot(data = test14$tr_plot[[2]],
# #                 aes(x=t, y=Treatx)) +
# #   geom_count() +
# #   geom_smooth()
# # tr.pl
# 
# png(filename = "../03_Output/test14.png", 
#     width = 14, height = 8, units = "in", res = 300,
#     type = "cairo")
# test14.pl
# dev.off()
# 
# 
# # att plot
# test14.pl.att <- sim_plot_att2(test14,
#                              use.rmse = FALSE,
#                              use.ci = FALSE, 
#                              violin = TRUE,
#                              xlim = c(-0.1, 0.4))
# test14.pl.att
# 
# png(filename = "../03_Output/test14_att.png", 
#     width = 8, height = 8, units = "in", res = 300,
#     type = "cairo")
# test14.pl.att
# dev.off()
# 
# 
# 








