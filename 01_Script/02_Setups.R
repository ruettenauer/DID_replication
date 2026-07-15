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




### Test 0 - Trend-breaking, large T, two-time, early-treated
Time <- 30

sim0 <- sim_did(
  seed = 1527, # set seed
  N = 1000, # number of cross-sectional units
  T = Time, # Number of time periods
  R = 1000, # Number of simulation runs
  scale.ps = 5, # Scaling parameter for binomial
  b.b = c(1, 0, 0, -0), # Cross-sectional selection
  b.x = c(0, 0, 0, 0), # time-varying selection, emp correlation bet D and x
  b.trend = c(0.2), # Trend in Y
  treat.sd = "twotime", # SD for probability distribution of treatment timing (determines where treatment happens)
  b.nonparallel = c(0.00), # Extra trend for treated (vs. non-treated) # ADD Extra trend for early treated vs late?
  beta = 1, # Treatment effect (and its distribution)
  beta.t = cumsum(c(0.05, rep(0.03214285, Time-1))), # Temporal distribution of treatment effect
  beta.late = 0.5, # early vs late with heterogeneous ATT, factor for late (early will have 1/beta.late), 
  size.late = 0.5, # share of late treatment group in case of beta.late 
  crs = ncores, #Parallel cores
)

save(sim0, file = "Setup_Sim0.RData")




# # Event plot
# sim0.pl <- sim_plot2(sim0, xlim = c(-3, 8), ylim = c(-0.1, 0.6),
#                       models = c("Conventional TWFE",  
#                                  "Matrix Completion",    
#                                  "Callaway SantAnna",   
#                                  "Sun Abraham",    
#                                  "Borusyak et al",   
#                                  "Wooldridge ETWFE"))
# sim0.pl
# 
# # tr.pl <- ggplot(data = sim0$tr_plot[[2]],
# #                 aes(x=t, y=Treatx)) +
# #   geom_count() +
# #   geom_smooth()
# # tr.pl
# 
# 
# png(filename = "../03_Output/Setup_sim0.png",
#     width = 14, height = 8, units = "in", res = 300,
#     type = "cairo")
# sim0.pl
# dev.off()
# 
# 
# # att plot
# sim0.pl.att <- sim_plot_att2(sim0,
#                               use.rmse = FALSE,
#                               use.ci = FALSE,
#                               violin = TRUE,
#                               xlim = c(-0.1, 0.25),
#                               models = c("Conventional TWFE",  
#                                          "Matrix Completion",    
#                                          "Callaway SantAnna",   
#                                          "Sun Abraham",    
#                                          "Borusyak et al",   
#                                          "Wooldridge ETWFE"))
# sim0.pl.att
# 
# sim0.pl.att$data$estimate[sim0.pl.att$data$estimator =="twfe"]
# 
# png(filename = "../03_Output/Setup_sim0_att.png",
#     width = 8, height = 8, units = "in", res = 300,
#     type = "cairo")
# sim0.pl.att
# dev.off()







### Test 1 - Trend-breaking, two-time, early-treated
Time <- 15

sim1 <- sim_did(
  seed = 1527, # set seed
  N = 1000, # number of cross-sectional units
  T = Time, # Number of time periods
  R = 1000, # Number of simulation runs
  scale.ps = 5, # Scaling parameter for binomial
  b.b = c(1, 0, 0, -0), # Cross-sectional selection
  b.x = c(0, 0, 0, 0), # time-varying selection, emp correlation bet D and x
  b.trend = c(0.2), # Trend in Y
  treat.sd = "twotime", # SD for probability distribution of treatment timing (determines where treatment happens)
  b.nonparallel = c(0.00), # Extra trend for treated (vs. non-treated) # ADD Extra trend for early treated vs late?
  beta = 1, # Treatment effect (and its distribution)
  beta.t = cumsum(c(0.05, rep(0.03214285, Time-1))), # Temporal distribution of treatment effect
  beta.late = 0.5, # early vs late with heterogeneous ATT, factor for late (early will have 1/beta.late), 
  size.late = 0.5, # share of late treatment group in case of beta.late 
  crs = ncores, #Parallel cores
)

save(sim1, file = "Setup_sim1.RData")




# # Event plot
# sim1.pl <- sim_plot2(sim1, xlim = c(-3, 8), ylim = c(-0.1, 0.6),
#                       models = c("Conventional TWFE",  
#                                  "Matrix Completion",    
#                                  "Callaway SantAnna",   
#                                  "Sun Abraham",    
#                                  "Borusyak et al",   
#                                  "Wooldridge ETWFE"))
# sim1.pl
# 
# # tr.pl <- ggplot(data = sim1$tr_plot[[2]],
# #                 aes(x=t, y=Treatx)) +
# #   geom_count() +
# #   geom_smooth()
# # tr.pl
# 
# 
# png(filename = "../03_Output/Setup_sim1.png",
#     width = 14, height = 8, units = "in", res = 300,
#     type = "cairo")
# sim1.pl
# dev.off()
# 
# 
# # att plot
# sim1.pl.att <- sim_plot_att2(sim1,
#                               use.rmse = FALSE,
#                               use.ci = FALSE,
#                               violin = TRUE,
#                               xlim = c(-0.1, 0.25),
#                               models = c("Conventional TWFE",  
#                                          "Matrix Completion",    
#                                          "Callaway SantAnna",   
#                                          "Sun Abraham",    
#                                          "Borusyak et al",   
#                                          "Wooldridge ETWFE"))
# sim1.pl.att
# 
# sim1.pl.att$data$estimate[sim1.pl.att$data$estimator =="twfe"]
# 
# png(filename = "../03_Output/Setup_sim1_att.png",
#     width = 8, height = 8, units = "in", res = 300,
#     type = "cairo")
# sim1.pl.att
# dev.off()






### Test 2 - Trend-breaking, early-treated
Time <- 15

sim2 <- sim_did(
  seed = 1527, # set seed
  N = 1000, # number of cross-sectional units
  T = Time, # Number of time periods
  R = 1000, # Number of simulation runs
  scale.ps = 5, # Scaling parameter for binomial
  b.b = c(1, 0, 0, -0), # Cross-sectional selection
  b.x = c(0, 0, 0, 0), # time-varying selection, emp correlation bet D and x
  b.trend = c(0.2), # Trend in Y
  treat.sd = 3, # SD for probability distribution of treatment timing (determines where treatment happens)
  b.nonparallel = c(0.00), # Extra trend for treated (vs. non-treated) # ADD Extra trend for early treated vs late?
  beta = 1, # Treatment effect (and its distribution)
  beta.t = cumsum(c(0.05, rep(0.03214285, Time-1))), # Temporal distribution of treatment effect
  beta.late = 0.5, # early vs late with heterogeneous ATT, factor for late (early will have 1/beta.late), 
  size.late = 0.5, # share of late treatment group in case of beta.late 
  crs = ncores, #Parallel cores
)

save(sim2, file = "Setup_sim2.RData")




# # Event plot
# sim2.pl <- sim_plot2(sim2, xlim = c(-3, 8), ylim = c(-0.1, 0.6),
#                       models = c("Conventional TWFE",  
#                                  "Matrix Completion",    
#                                  "Callaway SantAnna",   
#                                  "Sun Abraham",    
#                                  "Borusyak et al",   
#                                  "Wooldridge ETWFE"))
# sim2.pl
# 
# # tr.pl <- ggplot(data = sim2$tr_plot[[2]],
# #                 aes(x=t, y=Treatx)) +
# #   geom_count() +
# #   geom_smooth()
# # tr.pl
# 
# 
# png(filename = "../03_Output/Setup_sim2.png",
#     width = 14, height = 8, units = "in", res = 300,
#     type = "cairo")
# sim2.pl
# dev.off()
# 
# 
# # att plot
# sim2.pl.att <- sim_plot_att2(sim2,
#                               use.rmse = FALSE,
#                               use.ci = FALSE,
#                               violin = TRUE,
#                               xlim = c(-0.1, 0.25),
#                               models = c("Conventional TWFE",  
#                                          "Matrix Completion",    
#                                          "Callaway SantAnna",   
#                                          "Sun Abraham",    
#                                          "Borusyak et al",   
#                                          "Wooldridge ETWFE"))
# sim2.pl.att
# 
# sim2.pl.att$data$estimate[sim2.pl.att$data$estimator =="twfe"]
# 
# png(filename = "../03_Output/Setup_sim2_att.png",
#     width = 8, height = 8, units = "in", res = 300,
#     type = "cairo")
# sim2.pl.att
# dev.off()






### Test 3 - Inverted u-shape, early-treated
Time <- 15

sim3 <- sim_did(
  seed = 1527, # set seed
  N = 1000, # number of cross-sectional units
  T = Time, # Number of time periods
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

save(sim3, file = "Setup_sim3.RData")




# # Event plot
# sim3.pl <- sim_plot2(sim3, xlim = c(-3, 8), ylim = c(-0.1, 0.6),
#                       models = c("Conventional TWFE",  
#                                  "Matrix Completion",    
#                                  "Callaway SantAnna",   
#                                  "Sun Abraham",    
#                                  "Borusyak et al",   
#                                  "Wooldridge ETWFE"))
# sim3.pl
# 
# # tr.pl <- ggplot(data = sim3$tr_plot[[2]],
# #                 aes(x=t, y=Treatx)) +
# #   geom_count() +
# #   geom_smooth()
# # tr.pl
# 
# 
# png(filename = "../03_Output/Setup_sim3.png",
#     width = 14, height = 8, units = "in", res = 300,
#     type = "cairo")
# sim3.pl
# dev.off()
# 
# 
# # att plot
# sim3.pl.att <- sim_plot_att2(sim3,
#                               use.rmse = FALSE,
#                               use.ci = FALSE,
#                               violin = TRUE,
#                               xlim = c(-0.1, 0.25),
#                               models = c("Conventional TWFE",  
#                                          "Matrix Completion",    
#                                          "Callaway SantAnna",   
#                                          "Sun Abraham",    
#                                          "Borusyak et al",   
#                                          "Wooldridge ETWFE"))
# sim3.pl.att
# 
# sim3.pl.att$data$estimate[sim3.pl.att$data$estimator =="twfe"]
# 
# png(filename = "../03_Output/Setup_sim3_att.png",
#     width = 8, height = 8, units = "in", res = 300,
#     type = "cairo")
# sim3.pl.att
# dev.off()




### Test 3 - Inverted u-shape, early-treated, fading groups


### Test 3 - Inverted u-shape, early-treated
Time <- 15

sim3 <- sim_did(
  seed = 1527, # set seed
  N = 1000, # number of cross-sectional units
  T = Time, # Number of time periods
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
  fading = TRUE
)

save(sim3, file = "Setup_sim3b.RData")






### Test 4 - Inverted u-shape, early-treated (smaller)
Time <- 15

sim4 <- sim_did(
  seed = 1527, # set seed
  N = 1000, # number of cross-sectional units
  T = Time, # Number of time periods
  R = 1000, # Number of simulation runs
  scale.ps = 5, # Scaling parameter for binomial
  b.b = c(1, 0, 0, -0), # Cross-sectional selection
  b.x = c(0, 0, 0, 0), # time-varying selection, emp correlation bet D and x
  b.trend = c(0.2), # Trend in Y
  treat.sd = 3, # SD for probability distribution of treatment timing (determines where treatment happens)
  b.nonparallel = c(0.00), # Extra trend for treated (vs. non-treated) # ADD Extra trend for early treated vs late?
  beta = 1, # Treatment effect (and its distribution)
  beta.t = c(0.15, 0.25, 0.3, 0.2, 0.1), # Temporal distribution of treatment effect
  beta.late = 0.75, # early vs late with heterogeneous ATT, factor for late (early will have 1/beta.late), 
  size.late = 0.5, # share of late treatment group in case of beta.late 
  crs = ncores, #Parallel cores
)

save(sim4, file = "Setup_sim4.RData")




# # Event plot
# sim4.pl <- sim_plot2(sim4, xlim = c(-3, 8), ylim = c(-0.1, 0.6),
#                       models = c("Conventional TWFE",  
#                                  "Matrix Completion",    
#                                  "Callaway SantAnna",   
#                                  "Sun Abraham",    
#                                  "Borusyak et al",   
#                                  "Wooldridge ETWFE"))
# sim4.pl
# 
# # tr.pl <- ggplot(data = sim4$tr_plot[[2]],
# #                 aes(x=t, y=Treatx)) +
# #   geom_count() +
# #   geom_smooth()
# # tr.pl
# 
# 
# png(filename = "../03_Output/Setup_sim4.png",
#     width = 14, height = 8, units = "in", res = 300,
#     type = "cairo")
# sim4.pl
# dev.off()
# 
# 
# # att plot
# sim4.pl.att <- sim_plot_att2(sim4,
#                               use.rmse = FALSE,
#                               use.ci = FALSE,
#                               violin = TRUE,
#                               xlim = c(-0.1, 0.25),
#                               models = c("Conventional TWFE",  
#                                          "Matrix Completion",    
#                                          "Callaway SantAnna",   
#                                          "Sun Abraham",    
#                                          "Borusyak et al",   
#                                          "Wooldridge ETWFE"))
# sim4.pl.att
# 
# sim4.pl.att$data$estimate[sim4.pl.att$data$estimator =="twfe"]
# 
# png(filename = "../03_Output/Setup_sim4_att.png",
#     width = 8, height = 8, units = "in", res = 300,
#     type = "cairo")
# sim4.pl.att
# dev.off()









### Test 4 - Inverted u-shape, early-treated (smaller), small T
Time <- 8

sim5 <- sim_did(
  seed = 1527, # set seed
  N = 1000, # number of cross-sectional units
  T = Time, # Number of time periods
  R = 1000, # Number of simulation runs
  scale.ps = 5, # Scaling parameter for binomial
  b.b = c(1, 0, 0, -0), # Cross-sectional selection
  b.x = c(0, 0, 0, 0), # time-varying selection, emp correlation bet D and x
  b.trend = c(0.2), # Trend in Y
  treat.sd = 3, # SD for probability distribution of treatment timing (determines where treatment happens)
  b.nonparallel = c(0.00), # Extra trend for treated (vs. non-treated) # ADD Extra trend for early treated vs late?
  beta = 1, # Treatment effect (and its distribution)
  beta.t = c(0.15, 0.25, 0.3, 0.2, 0.1), # Temporal distribution of treatment effect
  beta.late = 0.75, # early vs late with heterogeneous ATT, factor for late (early will have 1/beta.late), 
  size.late = 0.5, # share of late treatment group in case of beta.late 
  crs = ncores, #Parallel cores
)

save(sim5, file = "Setup_sim5.RData")




# # Event plot
# sim5.pl <- sim_plot2(sim5, xlim = c(-3, 8), ylim = c(-0.1, 0.6),
#                       models = c("Conventional TWFE",  
#                                  "Matrix Completion",    
#                                  "Callaway SantAnna",   
#                                  "Sun Abraham",    
#                                  "Borusyak et al",   
#                                  "Wooldridge ETWFE"))
# sim5.pl
# 
# # tr.pl <- ggplot(data = sim5$tr_plot[[2]],
# #                 aes(x=t, y=Treatx)) +
# #   geom_count() +
# #   geom_smooth()
# # tr.pl
# 
# 
# png(filename = "../03_Output/Setup_sim5.png",
#     width = 14, height = 8, units = "in", res = 300,
#     type = "cairo")
# sim5.pl
# dev.off()
# 
# 
# # att plot
# sim5.pl.att <- sim_plot_att2(sim5,
#                               use.rmse = FALSE,
#                               use.ci = FALSE,
#                               violin = TRUE,
#                               xlim = c(-0.1, 0.25),
#                               models = c("Conventional TWFE",  
#                                          "Matrix Completion",    
#                                          "Callaway SantAnna",   
#                                          "Sun Abraham",    
#                                          "Borusyak et al",   
#                                          "Wooldridge ETWFE"))
# sim5.pl.att
# 
# sim5.pl.att$data$estimate[sim5.pl.att$data$estimator =="twfe"]
# 
# png(filename = "../03_Output/Setup_sim5_att.png",
#     width = 8, height = 8, units = "in", res = 300,
#     type = "cairo")
# sim5.pl.att
# dev.off()







### Test 6 - fade-in + constant, early-treated
Time <- 15

sim6 <- sim_did(
  seed = 1527, # set seed
  N = 1000, # number of cross-sectional units
  T = Time, # Number of time periods
  R = 1000, # Number of simulation runs
  scale.ps = 5, # Scaling parameter for binomial
  b.b = c(1, 0, 0, -0), # Cross-sectional selection
  b.x = c(0, 0, 0, 0), # time-varying selection, emp correlation bet D and x
  b.trend = c(0.2), # Trend in Y
  treat.sd = 3, # SD for probability distribution of treatment timing (determines where treatment happens)
  b.nonparallel = c(0.00), # Extra trend for treated (vs. non-treated) # ADD Extra trend for early treated vs late?
  beta = 1, # Treatment effect (and its distribution)
  beta.t = c(cumsum(c(0.05, rep(0.03214285, 4))), rep(0.17857140, 10)), # Temporal distribution of treatment effect
  beta.late = 0.5, # early vs late with heterogeneous ATT, factor for late (early will have 1/beta.late), 
  size.late = 0.5, # share of late treatment group in case of beta.late 
  crs = ncores, #Parallel cores
)

save(sim6, file = "Setup_sim6.RData")




# # Event plot
# sim6.pl <- sim_plot2(sim6, xlim = c(-3, 8), ylim = c(-0.1, 0.6),
#                       models = c("Conventional TWFE",  
#                                  "Matrix Completion",    
#                                  "Callaway SantAnna",   
#                                  "Sun Abraham",    
#                                  "Borusyak et al",   
#                                  "Wooldridge ETWFE"))
# sim6.pl
# 
# # tr.pl <- ggplot(data = sim6$tr_plot[[2]],
# #                 aes(x=t, y=Treatx)) +
# #   geom_count() +
# #   geom_smooth()
# # tr.pl
# 
# 
# png(filename = "../03_Output/Setup_sim6.png",
#     width = 14, height = 8, units = "in", res = 300,
#     type = "cairo")
# sim6.pl
# dev.off()
# 
# 
# # att plot
# sim6.pl.att <- sim_plot_att2(sim6,
#                               use.rmse = FALSE,
#                               use.ci = FALSE,
#                               violin = TRUE,
#                               xlim = c(-0.1, 0.25),
#                               models = c("Conventional TWFE",  
#                                          "Matrix Completion",    
#                                          "Callaway SantAnna",   
#                                          "Sun Abraham",    
#                                          "Borusyak et al",   
#                                          "Wooldridge ETWFE"))
# sim6.pl.att
# 
# sim6.pl.att$data$estimate[sim6.pl.att$data$estimator =="twfe"]
# 
# png(filename = "../03_Output/Setup_sim6_att.png",
#     width = 8, height = 8, units = "in", res = 300,
#     type = "cairo")
# sim6.pl.att
# dev.off()









### Test 7 - fade-in + constant, early-treated (but fading between groups)
Time <- 15

sim7 <- sim_did(
  seed = 1527, # set seed
  N = 1000, # number of cross-sectional units
  T = Time, # Number of time periods
  R = 1000, # Number of simulation runs
  scale.ps = 5, # Scaling parameter for binomial
  b.b = c(1, 0, 0, -0), # Cross-sectional selection
  b.x = c(0, 0, 0, 0), # time-varying selection, emp correlation bet D and x
  b.trend = c(0.2), # Trend in Y
  treat.sd = 3, # SD for probability distribution of treatment timing (determines where treatment happens)
  b.nonparallel = c(0.00), # Extra trend for treated (vs. non-treated) # ADD Extra trend for early treated vs late?
  beta = 1, # Treatment effect (and its distribution)
  beta.t = c(cumsum(c(0.05, rep(0.03214285, 4))), rep(0.17857140, 10)), # Temporal distribution of treatment effect
  beta.late = 0.5, # early vs late with heterogeneous ATT, factor for late (early will have 1/beta.late), 
  size.late = 0.5, # share of late treatment group in case of beta.late 
  crs = ncores, #Parallel cores
  fading = TRUE # 
)

save(sim7, file = "Setup_sim7.RData")


# 
# 
# # Event plot
# sim7.pl <- sim_plot2(sim7, xlim = c(-3, 8), ylim = c(-0.1, 0.6),
#                       models = c("Conventional TWFE",
#                                  "Matrix Completion",
#                                  "Callaway SantAnna",
#                                  "Sun Abraham",
#                                  "Borusyak et al",
#                                  "Wooldridge ETWFE"))
# sim7.pl
# 
# # tr.pl <- ggplot(data = sim7$tr_plot[[2]],
# #                 aes(x=t, y=Treatx)) +
# #   geom_count() +
# #   geom_smooth()
# # tr.pl
# 
# 
# png(filename = "../03_Output/Setup_sim7.png",
#     width = 14, height = 8, units = "in", res = 300,
#     type = "cairo")
# sim7.pl
# dev.off()
# 
# 
# # att plot
# sim7.pl.att <- sim_plot_att2(sim7,
#                               use.rmse = FALSE,
#                               use.ci = FALSE,
#                               violin = TRUE,
#                               xlim = c(-0.1, 0.25),
#                               models = c("Conventional TWFE",
#                                          "Matrix Completion",
#                                          "Callaway SantAnna",
#                                          "Sun Abraham",
#                                          "Borusyak et al",
#                                          "Wooldridge ETWFE"))
# sim7.pl.att
# 
# sim7.pl.att$data$estimate[sim7.pl.att$data$estimator =="twfe"]
# 
# png(filename = "../03_Output/Setup_sim7_att.png",
#     width = 8, height = 8, units = "in", res = 300,
#     type = "cairo")
# sim7.pl.att
# dev.off()
# 

