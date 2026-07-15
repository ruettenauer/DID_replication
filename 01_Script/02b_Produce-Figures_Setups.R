###########################
#### Utility Functions ####
###########################

# rm(list = ls())
# 
# setwd("C:/work/Forschung/DID/02_Data")


setwd("C:/work/Forschung/DID/06_HPC/02_Data")


library(foreach)
library(doParallel)

library(fect)
library(did)
library(staggered)
library(didimputation)
library(etwfe)
library(feisr)

library(ggplot2)
library(grid)
library(gridExtra)
library(viridisLite)
library(extrafont)
loadfonts()
library(gridtext)
library(ggtext)



# Load functions
source("../01_Script/01_DiD-Smulation-Program.R")

# N cores
ncores <- parallel::detectCores(logical = FALSE)

print(ncores)






##################
#### Test RUN ####
##################




# ### Test 0 - Trend-breaking, large T, two-time, early-treated
# Time <- 30
# 
# sim0 <- sim_did(
#   seed = 1527, # set seed
#   N = 1000, # number of cross-sectional units
#   T = Time, # Number of time periods
#   R = 1000, # Number of simulation runs
#   scale.ps = 5, # Scaling parameter for binomial
#   b.b = c(1, 0, 0, -0), # Cross-sectional selection
#   b.x = c(0, 0, 0, 0), # time-varying selection, emp correlation bet D and x
#   b.trend = c(0.2), # Trend in Y
#   treat.sd = "twotime", # SD for probability distribution of treatment timing (determines where treatment happens)
#   b.nonparallel = c(0.00), # Extra trend for treated (vs. non-treated) # ADD Extra trend for early treated vs late?
#   beta = 1, # Treatment effect (and its distribution)
#   beta.t = cumsum(c(0.05, rep(0.03214285, Time-1))), # Temporal distribution of treatment effect
#   beta.late = 0.5, # early vs late with heterogeneous ATT, factor for late (early will have 1/beta.late), 
#   size.late = 0.5, # share of late treatment group in case of beta.late 
#   crs = ncores, #Parallel cores
# )
# 
# save(sim0, file = "Setup_Sim0.RData")



load("Setup_Sim0.RData")


# Event plot
sim0.pl <- sim_plot2(sim0, xlim = c(-3, 8), ylim = c(-0.1, 0.6),
                      models = c("Conventional TWFE",
                                 "Matrix Completion",
                                 "Callaway SantAnna",
                                 "Sun Abraham",
                                 "Borusyak et al",
                                 "Wooldridge ETWFE"))
sim0.pl

# tr.pl <- ggplot(data = sim0$tr_plot[[2]],
#                 aes(x=t, y=Treatx)) +
#   geom_count() +
#   geom_smooth()
# tr.pl


png(filename = "../03_Output/Setup_sim0.png",
    width = 14, height = 8, units = "in", res = 300,
    type = "cairo")
sim0.pl
dev.off()


# att plot
sim0.pl.att <- sim_plot_att2(sim0,
                              use.rmse = FALSE,
                              use.ci = FALSE,
                              violin = TRUE,
                              xlim = c(-0.1, 0.25),
                              models = c("Conventional TWFE",
                                         "Matrix Completion",
                                         "Callaway SantAnna",
                                         "Sun Abraham",
                                         "Borusyak et al",
                                         "Wooldridge ETWFE"))
sim0.pl.att

sim0.pl.att$data$estimate[sim0.pl.att$data$estimator =="twfe"]

png(filename = "../03_Output/Setup_sim0_att.png",
    width = 8, height = 8, units = "in", res = 300,
    type = "cairo")
sim0.pl.att
dev.off()







# ### Test 1 - Trend-breaking, two-time, early-treated
# Time <- 15
# 
# sim1 <- sim_did(
#   seed = 1527, # set seed
#   N = 1000, # number of cross-sectional units
#   T = Time, # Number of time periods
#   R = 1000, # Number of simulation runs
#   scale.ps = 5, # Scaling parameter for binomial
#   b.b = c(1, 0, 0, -0), # Cross-sectional selection
#   b.x = c(0, 0, 0, 0), # time-varying selection, emp correlation bet D and x
#   b.trend = c(0.2), # Trend in Y
#   treat.sd = "twotime", # SD for probability distribution of treatment timing (determines where treatment happens)
#   b.nonparallel = c(0.00), # Extra trend for treated (vs. non-treated) # ADD Extra trend for early treated vs late?
#   beta = 1, # Treatment effect (and its distribution)
#   beta.t = cumsum(c(0.05, rep(0.03214285, Time-1))), # Temporal distribution of treatment effect
#   beta.late = 0.5, # early vs late with heterogeneous ATT, factor for late (early will have 1/beta.late), 
#   size.late = 0.5, # share of late treatment group in case of beta.late 
#   crs = ncores, #Parallel cores
# )
# 
# save(sim1, file = "Setup_sim1.RData")


load("Setup_Sim1.RData")



# Event plot
sim1.pl <- sim_plot2(sim1, xlim = c(-3, 8), ylim = c(-0.1, 0.6),
                      models = c("Conventional TWFE",
                                 "Matrix Completion",
                                 "Callaway SantAnna",
                                 "Sun Abraham",
                                 "Borusyak et al",
                                 "Wooldridge ETWFE"))
sim1.pl

# tr.pl <- ggplot(data = sim1$tr_plot[[2]],
#                 aes(x=t, y=Treatx)) +
#   geom_count() +
#   geom_smooth()
# tr.pl


png(filename = "../03_Output/Setup_sim1.png",
    width = 14, height = 8, units = "in", res = 300,
    type = "cairo")
sim1.pl
dev.off()


# att plot
sim1.pl.att <- sim_plot_att2(sim1,
                              use.rmse = FALSE,
                              use.ci = FALSE,
                              violin = TRUE,
                              xlim = c(-0.1, 0.25),
                              models = c("Conventional TWFE",
                                         "Matrix Completion",
                                         "Callaway SantAnna",
                                         "Sun Abraham",
                                         "Borusyak et al",
                                         "Wooldridge ETWFE"))
sim1.pl.att

sim1.pl.att$data$estimate[sim1.pl.att$data$estimator =="twfe"]

png(filename = "../03_Output/Setup_sim1_att.png",
    width = 8, height = 8, units = "in", res = 300,
    type = "cairo")
sim1.pl.att
dev.off()






# ### Test 2 - Trend-breaking, early-treated
# Time <- 15
# 
# sim2 <- sim_did(
#   seed = 1527, # set seed
#   N = 1000, # number of cross-sectional units
#   T = Time, # Number of time periods
#   R = 1000, # Number of simulation runs
#   scale.ps = 5, # Scaling parameter for binomial
#   b.b = c(1, 0, 0, -0), # Cross-sectional selection
#   b.x = c(0, 0, 0, 0), # time-varying selection, emp correlation bet D and x
#   b.trend = c(0.2), # Trend in Y
#   treat.sd = 3, # SD for probability distribution of treatment timing (determines where treatment happens)
#   b.nonparallel = c(0.00), # Extra trend for treated (vs. non-treated) # ADD Extra trend for early treated vs late?
#   beta = 1, # Treatment effect (and its distribution)
#   beta.t = cumsum(c(0.05, rep(0.03214285, Time-1))), # Temporal distribution of treatment effect
#   beta.late = 0.5, # early vs late with heterogeneous ATT, factor for late (early will have 1/beta.late), 
#   size.late = 0.5, # share of late treatment group in case of beta.late 
#   crs = ncores, #Parallel cores
# )
# 
# save(sim2, file = "Setup_sim2.RData")


load("Setup_Sim2.RData")



# Event plot
sim2.pl <- sim_plot2(sim2, xlim = c(-3, 8), ylim = c(-0.1, 0.6),
                      models = c("Conventional TWFE",
                                 "Matrix Completion",
                                 "Callaway SantAnna",
                                 "Sun Abraham",
                                 "Borusyak et al",
                                 "Wooldridge ETWFE"))
sim2.pl

# tr.pl <- ggplot(data = sim2$tr_plot[[2]],
#                 aes(x=t, y=Treatx)) +
#   geom_count() +
#   geom_smooth()
# tr.pl


png(filename = "../03_Output/Setup_sim2.png",
    width = 14, height = 8, units = "in", res = 300,
    type = "cairo")
sim2.pl
dev.off()


# att plot
sim2.pl.att <- sim_plot_att2(sim2,
                              use.rmse = FALSE,
                              use.ci = FALSE,
                              violin = TRUE,
                              xlim = c(-0.1, 0.25),
                              models = c("Conventional TWFE",
                                         "Matrix Completion",
                                         "Callaway SantAnna",
                                         "Sun Abraham",
                                         "Borusyak et al",
                                         "Wooldridge ETWFE"))
sim2.pl.att

sim2.pl.att$data$estimate[sim2.pl.att$data$estimator =="twfe"]

png(filename = "../03_Output/Setup_sim2_att.png",
    width = 8, height = 8, units = "in", res = 300,
    type = "cairo")
sim2.pl.att
dev.off()






# ### Test 3 - Inverted u-shape, early-treated
# Time <- 15
# 
# sim3 <- sim_did(
#   seed = 1527, # set seed
#   N = 1000, # number of cross-sectional units
#   T = Time, # Number of time periods
#   R = 1000, # Number of simulation runs
#   scale.ps = 5, # Scaling parameter for binomial
#   b.b = c(1, 0, 0, -0), # Cross-sectional selection
#   b.x = c(0, 0, 0, 0), # time-varying selection, emp correlation bet D and x
#   b.trend = c(0.2), # Trend in Y
#   treat.sd = 3, # SD for probability distribution of treatment timing (determines where treatment happens)
#   b.nonparallel = c(0.00), # Extra trend for treated (vs. non-treated) # ADD Extra trend for early treated vs late?
#   beta = 1, # Treatment effect (and its distribution)
#   beta.t = c(0.15, 0.25, 0.3, 0.2, 0.1), # Temporal distribution of treatment effect
#   beta.late = 0.5, # early vs late with heterogeneous ATT, factor for late (early will have 1/beta.late), 
#   size.late = 0.5, # share of late treatment group in case of beta.late 
#   crs = ncores, #Parallel cores
# )
# 
# save(sim3, file = "Setup_sim3.RData")


load("Setup_Sim3.RData") 




# Event plot
sim3.pl <- sim_plot2(sim3, xlim = c(-3, 8), ylim = c(-0.1, 0.6),
                      models = c("Conventional TWFE",
                                 "Matrix Completion",
                                 "Callaway SantAnna",
                                 "Sun Abraham",
                                 "Borusyak et al",
                                 "Wooldridge ETWFE"))
sim3.pl

# tr.pl <- ggplot(data = sim3$tr_plot[[2]],
#                 aes(x=t, y=Treatx)) +
#   geom_count() +
#   geom_smooth()
# tr.pl


png(filename = "../03_Output/Setup_sim3.png",
    width = 14, height = 8, units = "in", res = 300,
    type = "cairo")
sim3.pl
dev.off()


# att plot
sim3.pl.att <- sim_plot_att2(sim3,
                              use.rmse = FALSE,
                              use.ci = FALSE,
                              violin = TRUE,
                              xlim = c(-0.1, 0.25),
                              models = c("Conventional TWFE",
                                         "Matrix Completion",
                                         "Callaway SantAnna",
                                         "Sun Abraham",
                                         "Borusyak et al",
                                         "Wooldridge ETWFE"))
sim3.pl.att

sim3.pl.att$data$estimate[sim3.pl.att$data$estimator =="twfe"]

png(filename = "../03_Output/Setup_sim3_att.png",
    width = 8, height = 8, units = "in", res = 300,
    type = "cairo")
sim3.pl.att
dev.off()








# ### Test 4 - Inverted u-shape, early-treated (smaller)
# Time <- 15
# 
# sim4 <- sim_did(
#   seed = 1527, # set seed
#   N = 1000, # number of cross-sectional units
#   T = Time, # Number of time periods
#   R = 1000, # Number of simulation runs
#   scale.ps = 5, # Scaling parameter for binomial
#   b.b = c(1, 0, 0, -0), # Cross-sectional selection
#   b.x = c(0, 0, 0, 0), # time-varying selection, emp correlation bet D and x
#   b.trend = c(0.2), # Trend in Y
#   treat.sd = 3, # SD for probability distribution of treatment timing (determines where treatment happens)
#   b.nonparallel = c(0.00), # Extra trend for treated (vs. non-treated) # ADD Extra trend for early treated vs late?
#   beta = 1, # Treatment effect (and its distribution)
#   beta.t = c(0.15, 0.25, 0.3, 0.2, 0.1), # Temporal distribution of treatment effect
#   beta.late = 0.75, # early vs late with heterogeneous ATT, factor for late (early will have 1/beta.late), 
#   size.late = 0.5, # share of late treatment group in case of beta.late 
#   crs = ncores, #Parallel cores
# )
# 
# save(sim4, file = "Setup_sim4.RData")



load("Setup_Sim4.RData")


# Event plot
sim4.pl <- sim_plot2(sim4, xlim = c(-3, 8), ylim = c(-0.1, 0.6),
                      models = c("Conventional TWFE",
                                 "Matrix Completion",
                                 "Callaway SantAnna",
                                 "Sun Abraham",
                                 "Borusyak et al",
                                 "Wooldridge ETWFE"))
sim4.pl

# tr.pl <- ggplot(data = sim4$tr_plot[[2]],
#                 aes(x=t, y=Treatx)) +
#   geom_count() +
#   geom_smooth()
# tr.pl


png(filename = "../03_Output/Setup_sim4.png",
    width = 14, height = 8, units = "in", res = 300,
    type = "cairo")
sim4.pl
dev.off()


# att plot
sim4.pl.att <- sim_plot_att2(sim4,
                              use.rmse = FALSE,
                              use.ci = FALSE,
                              violin = TRUE,
                              xlim = c(-0.1, 0.25),
                              models = c("Conventional TWFE",
                                         "Matrix Completion",
                                         "Callaway SantAnna",
                                         "Sun Abraham",
                                         "Borusyak et al",
                                         "Wooldridge ETWFE"))
sim4.pl.att

sim4.pl.att$data$estimate[sim4.pl.att$data$estimator =="twfe"]

png(filename = "../03_Output/Setup_sim4_att.png",
    width = 8, height = 8, units = "in", res = 300,
    type = "cairo")
sim4.pl.att
dev.off()









# ### Test 5 - Inverted u-shape, early-treated (smaller), small T
# Time <- 8
# 
# sim5 <- sim_did(
#   seed = 1527, # set seed
#   N = 1000, # number of cross-sectional units
#   T = Time, # Number of time periods
#   R = 1000, # Number of simulation runs
#   scale.ps = 5, # Scaling parameter for binomial
#   b.b = c(1, 0, 0, -0), # Cross-sectional selection
#   b.x = c(0, 0, 0, 0), # time-varying selection, emp correlation bet D and x
#   b.trend = c(0.2), # Trend in Y
#   treat.sd = 3, # SD for probability distribution of treatment timing (determines where treatment happens)
#   b.nonparallel = c(0.00), # Extra trend for treated (vs. non-treated) # ADD Extra trend for early treated vs late?
#   beta = 1, # Treatment effect (and its distribution)
#   beta.t = c(0.15, 0.25, 0.3, 0.2, 0.1), # Temporal distribution of treatment effect
#   beta.late = 0.75, # early vs late with heterogeneous ATT, factor for late (early will have 1/beta.late), 
#   size.late = 0.5, # share of late treatment group in case of beta.late 
#   crs = ncores, #Parallel cores
# )
# 
# save(sim5, file = "Setup_sim5.RData")


load("Setup_sim5.RData")



# Event plot
sim5.pl <- sim_plot2(sim5, xlim = c(-3, 8), ylim = c(-0.1, 0.6),
                      models = c("Conventional TWFE",
                                 "Matrix Completion",
                                 "Callaway SantAnna",
                                 "Sun Abraham",
                                 "Borusyak et al",
                                 "Wooldridge ETWFE"))
sim5.pl

# tr.pl <- ggplot(data = sim5$tr_plot[[2]],
#                 aes(x=t, y=Treatx)) +
#   geom_count() +
#   geom_smooth()
# tr.pl


png(filename = "../03_Output/Setup_sim5.png",
    width = 14, height = 8, units = "in", res = 300,
    type = "cairo")
sim5.pl
dev.off()


# att plot
sim5.pl.att <- sim_plot_att2(sim5,
                              use.rmse = FALSE,
                              use.ci = FALSE,
                              violin = TRUE,
                              xlim = c(-0.1, 0.25),
                              models = c("Conventional TWFE",
                                         "Matrix Completion",
                                         "Callaway SantAnna",
                                         "Sun Abraham",
                                         "Borusyak et al",
                                         "Wooldridge ETWFE"))
sim5.pl.att

sim5.pl.att$data$estimate[sim5.pl.att$data$estimator =="twfe"]

png(filename = "../03_Output/Setup_sim5_att.png",
    width = 8, height = 8, units = "in", res = 300,
    type = "cairo")
sim5.pl.att
dev.off()









# ### Test 6 - fade-in + constant, early-treated
# Time <- 15
# 
# sim6 <- sim_did(
#   seed = 1527, # set seed
#   N = 1000, # number of cross-sectional units
#   T = Time, # Number of time periods
#   R = 1000, # Number of simulation runs
#   scale.ps = 5, # Scaling parameter for binomial
#   b.b = c(1, 0, 0, -0), # Cross-sectional selection
#   b.x = c(0, 0, 0, 0), # time-varying selection, emp correlation bet D and x
#   b.trend = c(0.2), # Trend in Y
#   treat.sd = 3, # SD for probability distribution of treatment timing (determines where treatment happens)
#   b.nonparallel = c(0.00), # Extra trend for treated (vs. non-treated) # ADD Extra trend for early treated vs late?
#   beta = 1, # Treatment effect (and its distribution)
#   beta.t = c(cumsum(c(0.05, rep(0.03214285, 4))), rep(0.17857140, 10)), # Temporal distribution of treatment effect
#   beta.late = 0.5, # early vs late with heterogeneous ATT, factor for late (early will have 1/beta.late), 
#   size.late = 0.5, # share of late treatment group in case of beta.late 
#   crs = ncores, #Parallel cores
# )
# 
# save(sim6, file = "Setup_sim6.RData")


load("Setup_sim6.RData")


# Event plot
sim6.pl <- sim_plot2(sim6, xlim = c(-3, 8), ylim = c(-0.1, 0.6),
                      models = c("Conventional TWFE",
                                 "Matrix Completion",
                                 "Callaway SantAnna",
                                 "Sun Abraham",
                                 "Borusyak et al",
                                 "Wooldridge ETWFE"))
sim6.pl

# tr.pl <- ggplot(data = sim6$tr_plot[[2]],
#                 aes(x=t, y=Treatx)) +
#   geom_count() +
#   geom_smooth()
# tr.pl


png(filename = "../03_Output/Setup_sim6.png",
    width = 14, height = 8, units = "in", res = 300,
    type = "cairo")
sim6.pl
dev.off()


# att plot
sim6.pl.att <- sim_plot_att2(sim6,
                              use.rmse = FALSE,
                              use.ci = FALSE,
                              violin = TRUE,
                              xlim = c(-0.1, 0.25),
                              models = c("Conventional TWFE",
                                         "Matrix Completion",
                                         "Callaway SantAnna",
                                         "Sun Abraham",
                                         "Borusyak et al",
                                         "Wooldridge ETWFE"))
sim6.pl.att

sim6.pl.att$data$estimate[sim6.pl.att$data$estimator =="twfe"]

png(filename = "../03_Output/Setup_sim6_att.png",
    width = 8, height = 8, units = "in", res = 300,
    type = "cairo")
sim6.pl.att
dev.off()










# ### Test 7 - fade-in + constant, early-treated (but fading between groups)
# Time <- 15
# 
# sim7 <- sim_did(
#   seed = 1527, # set seed
#   N = 1000, # number of cross-sectional units
#   T = Time, # Number of time periods
#   R = 1000, # Number of simulation runs
#   scale.ps = 5, # Scaling parameter for binomial
#   b.b = c(1, 0, 0, -0), # Cross-sectional selection
#   b.x = c(0, 0, 0, 0), # time-varying selection, emp correlation bet D and x
#   b.trend = c(0.2), # Trend in Y
#   treat.sd = 3, # SD for probability distribution of treatment timing (determines where treatment happens)
#   b.nonparallel = c(0.00), # Extra trend for treated (vs. non-treated) # ADD Extra trend for early treated vs late?
#   beta = 1, # Treatment effect (and its distribution)
#   beta.t = c(cumsum(c(0.05, rep(0.03214285, 4))), rep(0.17857140, 10)), # Temporal distribution of treatment effect
#   beta.late = 0.5, # early vs late with heterogeneous ATT, factor for late (early will have 1/beta.late), 
#   size.late = 0.5, # share of late treatment group in case of beta.late 
#   crs = ncores, #Parallel cores
#   fading = TRUE # 
# )
# 
# save(sim7, file = "Setup_sim7.RData")

load("Setup_sim7.RData")




# Event plot
sim7.pl <- sim_plot2(sim7, xlim = c(-3, 8), ylim = c(-0.1, 0.6),
                      models = c("Conventional TWFE",
                                 "Matrix Completion",
                                 "Callaway SantAnna",
                                 "Sun Abraham",
                                 "Borusyak et al",
                                 "Wooldridge ETWFE"))
sim7.pl

# tr.pl <- ggplot(data = sim7$tr_plot[[2]],
#                 aes(x=t, y=Treatx)) +
#   geom_count() +
#   geom_smooth()
# tr.pl


png(filename = "../03_Output/Setup_sim7.png",
    width = 14, height = 8, units = "in", res = 300,
    type = "cairo")
sim7.pl
dev.off()


# att plot
sim7.pl.att <- sim_plot_att2(sim7,
                              use.rmse = FALSE,
                              use.ci = FALSE,
                              violin = TRUE,
                              xlim = c(-0.1, 0.25),
                              models = c("Conventional TWFE",
                                         "Matrix Completion",
                                         "Callaway SantAnna",
                                         "Sun Abraham",
                                         "Borusyak et al",
                                         "Wooldridge ETWFE"))
sim7.pl.att

sim7.pl.att$data$estimate[sim7.pl.att$data$estimator =="twfe"]

png(filename = "../03_Output/Setup_sim7_att.png",
    width = 8, height = 8, units = "in", res = 300,
    type = "cairo")
sim7.pl.att
dev.off()







############################
#### Overall plot setup ####
############################


# att.gg <- ggarrange(test0.pl.att + rremove("xlab"),
#                     test9.pl.att + rremove("xlab") + rremove("y.text"),
#                     test1.pl.att + rremove("xlab") + rremove("y.text"),
#                     test8.pl.att + rremove("xlab"),
#                     test2.pl.att + rremove("xlab") + rremove("y.text"),
#                     test10.pl.att + rremove("xlab") + rremove("y.text"),
#                     labels = c("1)", "2)", "3)", "4)", "5)", "6)"),
#                     ncol = 3, nrow = 3,
#                     bottom = text_grob(paste0("Mean Absolute Error ", "+- ", 1, " SD"),
#                                        family = "Times New Roman", size = 18, hjust = 0.5))

att.df <- data.frame(sim0.pl.att$data,               facet = "(1) Trend-breaking, large T,  \n two treatment timings") 
att.df <- rbind(att.df, data.frame(sim1.pl.att$data, facet = "(2) Trend-breaking, **small T**,  \n two treatment timings")) 
att.df <- rbind(att.df, data.frame(sim2.pl.att$data, facet = "(3) Trend-breaking, small T,  \n **distributed treatment timings**")) 
att.df <- rbind(att.df, data.frame(sim3.pl.att$data, facet = "(5) **Inverse U-shape**, small T,  \n distributed treatment timings")) 
att.df <- rbind(att.df, data.frame(sim6.pl.att$data, facet = "(4) **Fade-in**, small T,  \n distributed treatment timings")) 
# att.df <- rbind(att.df, data.frame(sim7.pl.att$data, facet = "(5) Fade-in, distributed timings,  \n **overlapping groups**, small T")) 


att.df$facet <- as.factor(att.df$facet)
att.df$facet <- factor(att.df$facet, levels = rev(levels(att.df$facet)))




dist.df <- data.frame(sim0.pl.att$layers[[1]]$data,                facet = "(1) Trend-breaking, large T,  \n two treatment timings") 
dist.df <- rbind(dist.df, data.frame(sim1.pl.att$layers[[1]]$data, facet = "(2) Trend-breaking, **small T**,  \n two treatment timings")) 
dist.df <- rbind(dist.df, data.frame(sim2.pl.att$layers[[1]]$data, facet = "(3) Trend-breaking, small T,  \n **distributed treatment timings**")) 
dist.df <- rbind(dist.df, data.frame(sim3.pl.att$layers[[1]]$data, facet = "(5) **Inverse U-shape**, small T,  \n distributed treatment timings")) 
dist.df <- rbind(dist.df, data.frame(sim6.pl.att$layers[[1]]$data, facet = "(4) **Fade-in**, small T,  \n distributed treatment timings")) 
# dist.df <- rbind(dist.df, data.frame(sim7.pl.att$layers[[1]]$data, facet = "(5) Fade-in, distributed timings,  \n **overlapping groups**, small T")) 


dist.df$facet <- as.factor(dist.df$facet)
dist.df$facet <- factor(dist.df$facet, levels = rev(levels(dist.df$facet)))


test <- ggplot(att.df, aes(x = facet, y = estimate)) +
  geom_violin(
    data = dist.df,
    mapping = aes(x = facet, y = dispersion, col = facet, alpha = facet),
    fill = "darkgoldenrod1",
    alpha = 0.4,
    trim = TRUE,
    scale = "area",
    lwd = 0.3,
    draw_quantiles = c(0.5)
  ) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "deeppink1", lwd = 1.1) +
  geom_vline(xintercept = 0.5, linetype = 1) +
  geom_pointrange(data = att.df, aes(x = facet, y = estimate,
                                     ymin = conf.low, ymax = conf.high,
                                     color = facet, fill = facet, apha = facet), 
                  shape = 21, size = 0.8, lwd = 1.05
  ) +
  coord_flip() +
  scale_color_viridis_d(option = "G", end = 0.80, begin = 0.2, guide = "none") +
  scale_fill_viridis_d(option = "B", end = 0.80, begin = 0.2, guide = "none", direction = -1) +
  scale_alpha_manual(values = rep(0, 6)) +
  theme(
    # Backgrounds
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    
    # Grid lines
    panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
    panel.grid.minor = element_line(color = "grey95", linewidth = 0.25),
    
    # Axes
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_markdown(family = "Times New Roman", size = 18, colour = "black"),
    axis.text.x = element_text(family = "Times New Roman", size = 18, colour = "black"),
    axis.title = element_text(family = "Times New Roman", size = 18, colour = "black"),
    strip.text = element_text(size = 18, face = "bold", hjust = 0)
  )

test



### Select TWFE model

att.df <- att.df[which(att.df$name == "Conventional TWFE"), ]
dist.df <- dist.df[which(dist.df$name == "Conventional TWFE"), ]



### Abolute Error Plot ###

# Parameters
use.rmse = FALSE
use.ci = FALSE
violin = TRUE
xlim = c(-0.25, 0.1)
sd.factor = 2

# Plot
zp <- ggplot(att.df, aes(x = facet, y = estimate)) 

if(violin == TRUE){
  zp <-
    zp + geom_violin(
      data = dist.df,
      mapping = aes(x = facet, y = dispersion, col = facet, alpha = facet),
      fill = "darkgoldenrod1",
      alpha = 0.4,
      trim = TRUE,
      scale = "area",
      lwd = 0.3,
      draw_quantiles = c(0.5)
    )
}
zp <- zp + geom_hline(yintercept = 0, linetype = "dashed", color = "deeppink1", lwd = 1.1) +
  geom_vline(xintercept = 0.5, linetype = 1) +
  geom_pointrange(data = att.df, aes(x = facet, y = estimate,
                                     ymin = conf.low, ymax = conf.high,
                                     color = facet, fill = facet, apha = facet), 
                  shape = 21, size = 0.8, lwd = 1.05
  ) +
  coord_flip() +
  scale_x_discrete()+
  scale_color_viridis_d(option = "G", end = 0.80, begin = 0.2, guide = "none") +
  scale_fill_viridis_d(option = "B", end = 0.80, begin = 0.2, guide = "none", direction = -1) +
  scale_alpha_manual(values = rep(0, 6)) +
  theme(
    # Backgrounds
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    
    # Grid lines
    panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
    panel.grid.minor = element_line(color = "grey95", linewidth = 0.25),
    
    # Axes
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_markdown(family = "Times New Roman", size = 18, colour = "black"),
    axis.text.x = element_text(family = "Times New Roman", size = 18, colour = "black"),
    axis.title = element_text(family = "Times New Roman", size = 18, colour = "black"),
    strip.text = element_text(size = 18, face = "bold", hjust = 0)
  ) +
  labs(x = element_blank(),
       y = paste0("Mean Absolute Error ", "+- ", sd.factor, " SD"))
if(use.rmse){
  zp <- zp + labs(x = element_blank(),
                  y = paste0("Root Mean Squared Error (RMSE) ", "+- ", sd.factor, " SD"))
}
if(use.ci){
  zp <- zp + labs(x = element_blank(),
                  y = paste0("Root Mean Squared Error (RMSE) ", "with 95% CI"))
}
if(!is.null(xlim)){
  zp <- zp + coord_flip(ylim = xlim)
}

zp


png(filename = "../03_Output/Setup_ATT_combined.png",
    width = 12, height = 7, units = "in", res = 300,
    type = "cairo")
zp
dev.off()









########################
#### Relative Error ####
########################



rel0 <- sim_plot_att2(sim0, 
                      use.rmse = FALSE,
                      use.ci = FALSE,
                      violin = FALSE,
                      relativ = TRUE,
                      xlim = c(-0.6, 0.1),
                      models = c("Conventional TWFE"))
rel0

rel1 <- sim_plot_att2(sim1, 
                      use.rmse = FALSE,
                      use.ci = FALSE,
                      violin = FALSE,
                      relativ = TRUE,
                      xlim = c(-0.6, 0.1),
                      models = c("Conventional TWFE"))


rel2 <- sim_plot_att2(sim2, 
                      use.rmse = FALSE,
                      use.ci = FALSE,
                      violin = FALSE,
                      relativ = TRUE,
                      xlim = c(-0.6, 0.1),
                      models = c("Conventional TWFE"))


rel3 <- sim_plot_att2(sim3, 
                      use.rmse = FALSE,
                      use.ci = FALSE,
                      violin = FALSE,
                      relativ = TRUE,
                      xlim = c(-0.6, 0.1),
                      models = c("Conventional TWFE"))


rel6 <- sim_plot_att2(sim6, 
                      use.rmse = FALSE,
                      use.ci = FALSE,
                      violin = FALSE,
                      relativ = TRUE,
                      xlim = c(-0.6, 0.1),
                      models = c("Conventional TWFE"))



rel7 <- sim_plot_att2(sim6, 
                      use.rmse = FALSE,
                      use.ci = FALSE,
                      violin = FALSE,
                      relativ = TRUE,
                      xlim = c(-0.6, 0.1),
                      models = c("Conventional TWFE"))



### Combine

rel.df <- data.frame(rel0$data,               facet = "(1) Trend-breaking, large T,  \n two treatment timings") 
rel.df <- rbind(rel.df, data.frame(rel1$data, facet = "(2) Trend-breaking, **small T**,  \n two treatment timings")) 
rel.df <- rbind(rel.df, data.frame(rel2$data, facet = "(3) Trend-breaking, small T,  \n **distributed treatment timings**")) 
rel.df <- rbind(rel.df, data.frame(rel3$data, facet = "(5) **Inverse U-shape**, small T,  \n distributed treatment timings")) 
rel.df <- rbind(rel.df, data.frame(rel6$data, facet = "(4) **Fade-in**, small T,  \n distributed treatment timings")) 
# rel.df <- rbind(rel.df, data.frame(rel7$data, facet = "(5) Fade-in, distributed timings,  \n **overlapping groups**, small T")) 

rel.df$facet <- as.factor(rel.df$facet)
rel.df$facet <- factor(rel.df$facet, levels = rev(levels(rel.df$facet)))


### Relative Error Plot ###

# Parameters
use.rmse = TRUE
use.ci = FALSE
violin = FALSE
xlim = c(-0.6, 0.3)
sd.factor = 2

# Plot
zp2 <- ggplot(rel.df, aes(x = facet, y = estimate)) 
zp2 <- zp2 + geom_hline(yintercept = 0, linetype = "dashed", color = "deeppink1", lwd = 1.1) +
  geom_vline(xintercept = 0.5, linetype = 1) +
  geom_pointrange(data = rel.df, aes(x = facet, y = estimate,
                                     ymin = conf.low, ymax = conf.high,
                                     color = facet, fill = facet), 
                  shape = 21, size = 0.8, lwd = 1.05
  ) +
  coord_flip() +
  scale_x_discrete()+
  scale_color_viridis_d(option = "G", end = 0.80, begin = 0.2, guide = "none") +
  scale_fill_viridis_d(option = "B", end = 0.80, begin = 0.2, guide = "none", direction = -1) +
  theme(
    # Backgrounds
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    
    # Grid lines
    panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
    panel.grid.minor = element_line(color = "grey95", linewidth = 0.25),
    
    # Axes
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_markdown(family = "Times New Roman", size = 18, colour = "black"),
    axis.text.x = element_text(family = "Times New Roman", size = 18, colour = "black"),
    axis.title = element_text(family = "Times New Roman", size = 18, colour = "black"),
    strip.text = element_text(size = 18, face = "bold", hjust = 0)
  ) +
  labs(x = element_blank(),
       y = paste0("Mean Absolute Error ", "+- ", sd.factor, " SD"))
zp2 <- zp2 + labs(x = element_blank(),
                y = paste0("Mean Relative Error "))

zp2


png(filename = "../03_Output/Setup_ATT_combined_relative.png",
    width = 12, height = 8, units = "in", res = 300,
    type = "cairo")
zp2
dev.off()






grid <- grid.arrange(
  grobs = list(zp, 
               zp2 + theme(axis.text.y = element_blank())
               ),
  layout_matrix = rbind(c(1,1,1,1,1, 2,2))
)


png(filename = "../03_Output/Setup_ATT.png",
    width = 14, height = 7, units = "in", res = 300,
    type = "cairo")
grid.draw(grid)
dev.off()






#### --- Null plot --- ####


# Parameters
use.rmse = FALSE
use.ci = FALSE
violin = TRUE
xlim = c(-0.25, 0.1)
sd.factor = 2

# Plot 1
dist2.df <- dist.df
dist2.df$dispersion[which(dist2.df$facet != "(1) Trend-breaking, large T,  \n two treatment timings")] <- NA

if(violin == TRUE){
  zp <-
    zp + geom_violin(
      data = dist2.df,
      mapping = aes(x = facet, y = dispersion, col = facet),
      fill = "darkgoldenrod1",
      color = viridis(6, option = "G", end = 0.80, begin = 0.2)[6],
      alpha = 0.4,
      trim = TRUE,
      scale = "area",
      lwd = 0.3,
      draw_quantiles = c(0.5)
    )
}
zp <- zp + geom_hline(yintercept = 0, linetype = "dashed", color = "deeppink1", lwd = 1.1) +
  geom_vline(xintercept = 0.5, linetype = 1) +
  geom_pointrange(data = att.df, aes(x = facet, y = estimate,
                                     ymin = conf.low, ymax = conf.high,
                                     color = facet, fill = facet, alpha = facet), 
                  shape = 21, size = 0.8, lwd = 1.05, 
                  color = viridis(6, option = "G", end = 0.80, begin = 0.2)[6],
                  fill = viridis(6, option = "B", end = 0.80, begin = 0.2,  direction = -1)[6]
  ) +
  coord_flip() +
  scale_x_discrete()+
  scale_color_viridis_d(option = "G", end = 0.80, begin = 0.2, guide = "none") +
  scale_fill_viridis_d(option = "B", end = 0.80, begin = 0.2, guide = "none", direction = -1) +
  scale_alpha_manual(values = c(rep(0, 4), 1), guide = FALSE) +
  theme(
    # Backgrounds
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    
    # Grid lines
    panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
    panel.grid.minor = element_line(color = "grey95", linewidth = 0.25),
    
    # Axes
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_markdown(family = "Times New Roman", size = 18, colour = "black"),
    axis.text.x = element_text(family = "Times New Roman", size = 18, colour = "black"),
    axis.title = element_text(family = "Times New Roman", size = 18, colour = "black"),
    strip.text = element_text(size = 18, face = "bold", hjust = 0)
  ) +
  labs(x = element_blank(),
       y = paste0("Mean Absolute Error ", "+- ", sd.factor, " SD"))
if(use.rmse){
  zp <- zp + labs(x = element_blank(),
                  y = paste0("Root Mean Squared Error (RMSE) ", "+- ", sd.factor, " SD"))
}
if(use.ci){
  zp <- zp + labs(x = element_blank(),
                  y = paste0("Root Mean Squared Error (RMSE) ", "with 95% CI"))
}
if(!is.null(xlim)){
  zp <- zp + coord_flip(ylim = xlim)
}

zp


# Plot 2

# Parameters
use.rmse = TRUE
use.ci = FALSE
violin = FALSE
xlim = c(-0.6, 0.3)
sd.factor = 2

zp2 <- ggplot(rel.df, aes(x = facet, y = estimate)) 
zp2 <- zp2 + geom_hline(yintercept = 0, linetype = "dashed", color = "deeppink1", lwd = 1.1) +
  geom_vline(xintercept = 0.5, linetype = 1) +
  geom_pointrange(data = rel.df, aes(x = facet, y = estimate,
                                     ymin = conf.low, ymax = conf.high,
                                     color = facet, fill = facet, alpha = facet), 
                  shape = 21, size = 1, lwd = 1.05
  ) +
  coord_flip() +
  scale_x_discrete()+
  scale_color_viridis_d(option = "G", end = 0.80, begin = 0.2, guide = "none") +
  scale_fill_viridis_d(option = "B", end = 0.80, begin = 0.2, guide = "none", direction = -1) +
  scale_alpha_manual(values = c(rep(0, 4), 1), guide = FALSE) + 
  theme(
    # Backgrounds
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    
    # Grid lines
    panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
    panel.grid.minor = element_line(color = "grey95", linewidth = 0.25),
    
    # Axes
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_markdown(family = "Times New Roman", size = 18, colour = "black"),
    axis.text.x = element_text(family = "Times New Roman", size = 18, colour = "black"),
    axis.title = element_text(family = "Times New Roman", size = 18, colour = "black"),
    strip.text = element_text(size = 18, face = "bold", hjust = 0)
  ) +
  labs(x = element_blank(),
       y = paste0("Mean Absolute Error ", "+- ", sd.factor, " SD"))
zp2 <- zp2 + labs(x = element_blank(),
                  y = paste0("Mean Relative Error "))

zp2






grid <- grid.arrange(
  grobs = list(zp , 
               zp2 + theme(axis.text.y = element_blank())
  ),
  layout_matrix = rbind(c(1,1,1,1,1, 2,2))
)


png(filename = "../03_Output/Setup_ATT_0.png",
    width = 14, height = 7, units = "in", res = 300,
    type = "cairo")
grid.draw(grid)
dev.off()


ragg::agg_png("../03_Output/Setup_ATT_0.png",
              width = 14, height = 7, units = "in", res = 300,)
grid.draw(grid)
dev.off()




