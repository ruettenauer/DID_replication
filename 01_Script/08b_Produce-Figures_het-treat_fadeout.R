###########################
#### Utility Functions ####
###########################

rm(list = ls())

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
library(ggpubr)
library(grid)
library(gridExtra)
library(viridisLite)
library(extrafont)
loadfonts()



# Load functions
source("../01_Script/01_DiD-Smulation-Program.R")

# N cores
ncores <- parallel::detectCores(logical = FALSE)

print(ncores)



##################
#### Test RUN ####
##################




### Test 0 - normal TWFE with homogeneous treatment effect



load("Hettreat_Sim0.RData")


# Event plot
test0.pl <- sim_plot2(test0, xlim = c(-3, 8), ylim = c(-0.1, 0.6),
                      models = c("Conventional TWFE",  
                                 "Matrix Completion",    
                                 "Callaway SantAnna",   
                                 "Sun Abraham",    
                                 "Borusyak et al",   
                                 "Wooldridge ETWFE"))
test0.pl

# tr.pl <- ggplot(data = test0$tr_plot[[2]],
#                 aes(x=t, y=Treatx)) +
#   geom_count() +
#   geom_smooth()
# tr.pl


png(filename = "../03_Output/Hettreat_fadeout_Test0.png",
    width = 14, height = 8, units = "in", res = 300,
    type = "cairo")
test0.pl
dev.off()


# att plot
test0.pl.att <- sim_plot_att2(test0,
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
test0.pl.att


png(filename = "../03_Output/Hettreat_fadeout_0_att.png",
    width = 8, height = 8, units = "in", res = 300,
    type = "cairo")
test0.pl.att
dev.off()








### Test 1 - normal TWFE with heterogeneous treatment effect



load("Hettreat_fadeout_Sim1.RData")


# Event plot
test1.pl <- sim_plot2(test1, xlim = c(-3, 8), ylim = c(-0.1, 0.6),
                      models = c("Conventional TWFE",  
                                 "Matrix Completion",    
                                 "Callaway SantAnna",   
                                 "Sun Abraham",    
                                 "Borusyak et al",   
                                 "Wooldridge ETWFE"))
test1.pl


# tr.pl <- ggplot(data = test1$tr_plot[[2]],
#                 aes(x=t, y=Treatx)) +
#   geom_count() +
#   geom_smooth()
# tr.pl


png(filename = "../03_Output/Hettreat_fadeout_1.png",
    width = 14, height = 8, units = "in", res = 300,
    type = "cairo")
test1.pl
dev.off()


# att plot
test1.pl.att <- sim_plot_att2(test1,
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
test1.pl.att


png(filename = "../03_Output/Hettreat_fadeout_1_att.png",
    width = 8, height = 8, units = "in", res = 300,
    type = "cairo")
test1.pl.att
dev.off()




### Produce Plot for FEIS


test1.pl.feis <- sim_plot2(test1, xlim = c(-3, 8), ylim = c(-0.1, 0.6),
                           models = c("FEIS"))
test1.pl.feis

# att plot
test1.pl.att.feis <- sim_plot_att2(test1,
                                   use.rmse = FALSE,
                                   use.ci = FALSE,
                                   violin = TRUE,
                                   xlim = c(-0.1, 0.25),
                                   models = c("Conventional TWFE",
                                              "Callaway SantAnna",
                                              "FEIS"))
test1.pl.att.feis









### Test8 - non-parallel trends with heterogeneous treatment effect + anticipation effect



load("Hettreat_fadeout_Sim8.RData")

test8.pl <- sim_plot2(test8, xlim = c(-3, 8), ylim = c(-0.1, 0.6),
                      models = c("Conventional TWFE",  
                                 "Matrix Completion",    
                                 "Callaway SantAnna",   
                                 "Sun Abraham",    
                                 "Borusyak et al",   
                                 "Wooldridge ETWFE"))
test8.pl

# tr.pl <- ggplot(data = test8$tr_plot[[2]],
#                 aes(x=t, y=Treatx)) +
#   geom_count() +
#   geom_smooth()
# tr.pl

png(filename = "../03_Output/Hettreat_fadeout_8.png",
    width = 14, height = 8, units = "in", res = 300,
    type = "cairo")
test8.pl
dev.off()


# att plot
test8.pl.att <- sim_plot_att2(test8,
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
test8.pl.att

png(filename = "../03_Output/Hettreat_fadeout_8_att.png",
    width = 8, height = 8, units = "in", res = 300,
    type = "cairo")
test8.pl.att
dev.off()







### Test 9 - normal TWFE with trend-breaking treatment effect ### Take the normal one 


load("Hettreat_fadeout_Sim9.RData")

# Event plot
test9.pl <- sim_plot2(test9, xlim = c(-3, 8), ylim = c(-0.1, 0.6),
                      models = c("Conventional TWFE",  
                                 "Matrix Completion",    
                                 "Callaway SantAnna",   
                                 "Sun Abraham",    
                                 "Borusyak et al",   
                                 "Wooldridge ETWFE"))
test9.pl


# tr.pl <- ggplot(data = test9$tr_plot[[2]],
#                 aes(x=t, y=Treatx)) +
#   geom_count() +
#   geom_smooth()
# tr.pl


png(filename = "../03_Output/Hettreat_fadeout_9.png",
    width = 14, height = 8, units = "in", res = 300,
    type = "cairo")
test9.pl
dev.off()


# att plot
test9.pl.att <- sim_plot_att2(test9,
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
test9.pl.att


png(filename = "../03_Output/Hettreat_fadeout_9_att.png",
    width = 8, height = 8, units = "in", res = 300,
    type = "cairo")
test9.pl.att
dev.off()







### Test10 - normal TWFE, heterogeous treatmet and homgeneous treatment timing + time-varying selection



load("Hettreat_fadeout_Sim10.RData")

test10.pl <- sim_plot2(test10, xlim = c(-3, 8), ylim = c(-0.1, 0.6),
                       models = c("Conventional TWFE",  
                                  "Matrix Completion",    
                                  "Callaway SantAnna",   
                                  "Sun Abraham",    
                                  "Borusyak et al",   
                                  "Wooldridge ETWFE"))
test10.pl

# tr.pl <- ggplot(data = test10.pl$tr_plot[[2]],
#                 aes(x=t, y=Treatx)) +
#   geom_count() +
#   geom_smooth()
# tr.pl


png(filename = "../03_Output/Hettreat_fadeout_10.png",
    width = 14, height = 8, units = "in", res = 300,
    type = "cairo")
test10.pl
dev.off()


# att plot
test10.pl.att <- sim_plot_att2(test10,
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
test10.pl.att

png(filename = "../03_Output/Hettreat_fadeout_10_att.png",
    width = 8, height = 8, units = "in", res = 300,
    type = "cairo")
test10.pl.att
dev.off()












### Test 12 - non-parallel trends (weaker) with heterogeneous treatment effect



load("Hettreat_fadeout_Sim12.RData")

test12.pl <- sim_plot2(test12, xlim = c(-3, 8), ylim = c(-0.1, 0.6),
                       models = c("Conventional TWFE",  
                                  "Matrix Completion",    
                                  "Callaway SantAnna",   
                                  "Sun Abraham",    
                                  "Borusyak et al",   
                                  "Wooldridge ETWFE"))
test12.pl

# tr.pl <- ggplot(data = test12$tr_plot[[2]],
#                 aes(x=t, y=Treatx)) +
#   geom_count() +
#   geom_smooth()
# tr.pl

png(filename = "../03_Output/Hettreat_fadeout_12.png",
    width = 14, height = 8, units = "in", res = 300,
    type = "cairo")
test12.pl
dev.off()


# att plot
test12.pl.att <- sim_plot_att2(test12,
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
test12.pl.att

png(filename = "../03_Output/Hettreat_fadeout_12_att.png",
    width = 8, height = 8, units = "in", res = 300,
    type = "cairo")
test12.pl.att
dev.off()




### Produce Plot for FEIS


test12.pl.feis <- sim_plot2(test12, xlim = c(-3, 8), ylim = c(-0.1, 0.6),
                            models = c("FEIS"))
test12.pl.feis

# att plot
test12.pl.att.feis <- sim_plot_att2(test12,
                                    use.rmse = FALSE,
                                    use.ci = FALSE,
                                    violin = TRUE,
                                    xlim = c(-0.1, 0.25),
                                    models = c("Conventional TWFE",
                                               "Callaway SantAnna",
                                               "FEIS"))
test12.pl.att.feis




# Combine 
test1.pl.att.feis <- test1.pl.att.feis + ggtitle("4) Fade-out") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0))
test1.pl.att.feis

test12.pl.att.feis <- test12.pl.att.feis + ggtitle("6) Non-parallel trends") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0))
test12.pl.att.feis


zp <- grid.arrange(test1.pl.att.feis, 
                   test12.pl.att.feis + rremove("y.text"),
                   test1.pl.feis, test12.pl.feis,
                   ncol = 2, nrow = 2, 
                   layout_matrix = rbind(c(1,1,1,1,1,1, 2,2,2,2), 
                                         c(NA,NA, 3,3,3,3, 4,4,4,4)))
zp


png(filename = "../03_Output/Hettreat_fadeout_FEIS_combined.png",
    width = 10, height = 7, units = "in", res = 300,
    type = "cairo")
grid.draw(zp)
dev.off()






### Test 13 - non-parallel trends (weaker) with heterogeneous treatment effect + anticipation



load("Hettreat_fadeout_Sim13.RData")

test13.pl <- sim_plot2(test13, xlim = c(-3, 8), ylim = c(-0.1, 0.6),
                       models = c("Conventional TWFE",  
                                  "Matrix Completion",    
                                  "Callaway SantAnna",   
                                  "Sun Abraham",    
                                  "Borusyak et al",   
                                  "Wooldridge ETWFE"))
test13.pl

# tr.pl <- ggplot(data = test13$tr_plot[[2]],
#                 aes(x=t, y=Treatx)) +
#   geom_count() +
#   geom_smooth()
# tr.pl

png(filename = "../03_Output/Hettreat_fadeout_13.png",
    width = 14, height = 8, units = "in", res = 300,
    type = "cairo")
test13.pl
dev.off()


# att plot
test13.pl.att <- sim_plot_att2(test13,
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
test13.pl.att

png(filename = "../03_Output/Hettreat_fadeout_13_att.png",
    width = 8, height = 8, units = "in", res = 300,
    type = "cairo")
test13.pl.att
dev.off()









### Test 9a - normal TWFE with trend-breaking treatment effect ### without heterogeneous group effects
# Take the old one from "03_Run-Simulation-set_hpc_4"

load("Hettreat_fadeout_Sim9a.RData")

# Event plot
test9a.pl <- sim_plot2(test9a, xlim = c(-3, 8), ylim = c(-0.1, 0.6),
                      models = c("Conventional TWFE",  
                                 "Matrix Completion",    
                                 "Callaway SantAnna",   
                                 "Sun Abraham",    
                                 "Borusyak et al",   
                                 "Wooldridge ETWFE"))
test9a.pl


# tr.pl <- ggplot(data = test9$tr_plot[[2]],
#                 aes(x=t, y=Treatx)) +
#   geom_count() +
#   geom_smooth()
# tr.pl


png(filename = "../03_Output/Hettreat_fadeout_9a.png",
    width = 14, height = 8, units = "in", res = 300,
    type = "cairo")
test9a.pl
dev.off()


# att plot
test9a.pl.att <- sim_plot_att2(test9a,
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
test9a.pl.att


png(filename = "../03_Output/Hettreat_fadeout_9a_att.png",
    width = 8, height = 8, units = "in", res = 300,
    type = "cairo")
test9a.pl.att
dev.off()












#######################
#### Combine plots ####
#######################


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

att.df <- data.frame(test0.pl.att$data, facet = "1) Homogeneous") 
att.df <- rbind(att.df, data.frame(test9a.pl.att$data, facet = "2) Trend-breaking")) 
att.df <- rbind(att.df, data.frame(test9.pl.att$data, facet = "3) Group-specific")) 
att.df <- rbind(att.df, data.frame(test1.pl.att$data, facet = "4) Fade-out")) 
att.df <- rbind(att.df, data.frame(test8.pl.att$data, facet = "5) Anticipation")) 
att.df <- rbind(att.df, data.frame(test12.pl.att$data, facet = "6) Non-parallel trends")) 


dist.df <- data.frame(test0.pl.att$layers[[1]]$data, facet = "1) Homogeneous") 
dist.df <- rbind(dist.df, data.frame(test9a.pl.att$layers[[1]]$data, facet = "2) Trend-breaking")) 
dist.df <- rbind(dist.df, data.frame(test9.pl.att$layers[[1]]$data, facet = "3) Group-specific")) 
dist.df <- rbind(dist.df, data.frame(test1.pl.att$layers[[1]]$data, facet = "4) Fade-out")) 
dist.df <- rbind(dist.df, data.frame(test8.pl.att$layers[[1]]$data, facet = "5) Anticipation")) 
dist.df <- rbind(dist.df, data.frame(test12.pl.att$layers[[1]]$data, facet = "6) Non-parallel trends")) 



# Parameters
use.rmse = FALSE
use.ci = FALSE
violin = TRUE
xlim = c(-0.1, 0.21)
sd.factor = 2

# Plot
zp <- ggplot(att.df, aes(x = name, y = estimate)) +
  facet_wrap(~facet, nrow = 2)
if(violin == TRUE){
  zp <-
    zp + geom_violin(
      data = dist.df,
      mapping = aes(x = name, y = dispersion, col = name),
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
  geom_pointrange(data = att.df, aes(x = name, y = estimate,
                                     ymin = conf.low, ymax = conf.high,
                                     color = name, fill = name), 
                  shape = 21, size = 0.6, lwd = 1.05
  ) +
  coord_flip() +
  scale_x_discrete()+
  scale_color_viridis_d(option = "G", end = 0.80, begin = 0.2, guide = "none") +
  scale_fill_viridis_d(option = "B", end = 0.80, begin = 0.2, guide = "none", direction = -1) +
  theme_minimal() + theme( # panel.grid.minor = element_blank(),
    text = element_text(family = "Times New Roman", size = 18),
    axis.text = element_text(colour = "black"),
    strip.text = element_text(size = 18, face = "bold", hjust = 0)) +
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




png(filename = "../03_Output/Hettreat_fadeout_ATT_combined.png",
    width = 12, height = 8, units = "in", res = 300,
    type = "cairo")
zp
dev.off()

