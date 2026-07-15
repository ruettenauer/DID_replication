###############
#### Setup ####
###############
  library(foreach)
  library(doParallel)
  
  library(fect)
  library(did)
  library(staggered)
  library(didimputation)
  library(etwfe)
  library(feisr)
  library(haven)
  
  library(ggplot2)
  library(viridisLite)
  library(extrafont)
  
  library(tidyverse)
  library(labelled)
  library(dplyr)
library(grid)
  library(gridExtra)
# loadfonts()

cl <- makeCluster(6)
registerDoParallel(cl)


### Help function for dopar output
comb<-function(x){
  lapply(seq_along(x[[1]]), function(i) 
    do.call(Map, c(f = rbind, lapply(x, `[[`, i))))
}

comb2<-function(x){
  lapply(seq_along(x[[1]]), function(i) 
    do.call(rbind, lapply(x, `[[`, i)))
}


### RMSE
rmse <- function(sim = NULL, true = NULL) {
  se <- (sim - true) ^ 2
  mse <- mean(se)
  rmse <- sqrt(mse)
  return(rmse)
}

rse <- function(sim = NULL, true = NULL) {
  se <- (sim - true) ^ 2
  rse <- sqrt(se)
  return(rse)
}



#####################
#### WD and data ####
#####################

setwd("C:/work/Forschung/DID/09_Case-Study")


### Load
### Subset of vars
vars <- c(
  "pidp", 
  "wave", 
  "hidp", 
  "year", 
  "sex", 
  "age_dv", 
  "mastat_dv", 
  "hiqual_dv", 
  "jbstat", 
  "sf1", 
  "sclfsat1", 
  "sclfsat2", 
  "sclfsato", 
  "health", 
  "scghq1_dv", 
  "fimnlabgrs_dv", 
  "fimngrs_dv", 
  "bornuk_dv", 
  "evermar_dv", 
  "anychild_dv", 
  "ethn_dv", 
  "hhsize", 
  "nkids_dv", 
  "ieqmoecd_dv", 
  "tenure_dv", 
  "fihhmngrs_dv",
  "lprnt", 
  "lnprnt", 
  "ladopt", 
  "lnadopt",
  "nnewborn"
)


ukhls.df <- read_dta("all_ukhls_11_stata.dta",
                     col_select = all_of(vars))


### Mutate vars
ukhls.df <- ukhls.df %>% 
  mutate(
    female = as.numeric(sex) - 1,
    age = as.double(age_dv),
    hhincome = as.double(fihhmngrs_dv) / as.double(ieqmoecd_dv) ,
    lab_income = as.double(fimnlabgrs_dv),
    ukborn = to_factor(bornuk_dv),
    hhsize = as.double(hhsize),
    maritalstat = to_factor(mastat_dv),
    hhsize = as.double(hhsize),
    educ = to_factor(hiqual_dv),
    lifesat = as.double(sclfsato),
    healthsat = as.double(sclfsat1),
    nkids = as.double(nkids_dv),
    health_issue = 2 - as.double(health),
    owner = ifelse(tenure_dv <= 2, 1, 0),
    unemployed = ifelse(jbstat == 3, 1, 0),
    retired = ifelse(jbstat == 4, 1, 0),
    lprnt = as.double(lprnt), 
    lnprnt = as.double(lnprnt), 
    ladopt = as.double(ladopt), 
    lnadopt = as.double(lnadopt),
    nnewborn = as.double(nnewborn)
  )

ukhls.df <- ukhls.df %>% 
  select(
    pidp,
    wave,
    year,
    age,
    female,
    hhincome,
    lab_income,
    hhsize,
    lifesat,
    maritalstat,
    healthsat,
    nkids,
    health_issue,
    owner,
    unemployed,
    retired,
    lprnt, 
    lnprnt, 
    ladopt, 
    lnadopt,
    nnewborn
  )


### Omit negative labour income
ukhls.df <- ukhls.df %>% 
  mutate(
    lab_income = ifelse(lab_income >= 0, lab_income, NA)
  )


### Marital status

ukhls.df <- ukhls.df %>% 
  mutate(
    married = ifelse(maritalstat %in% c("Married",
                                        "In a registered same-sex civil partnership",
                                        "Separated but legally married",
                                        "Separated from civil partner"),
                     1, 0),
    divorced = ifelse(maritalstat %in% c("Divorced",
                                         "Widowed",
                                         "A former civil partner",
                                         "A surviving civil partner"),
                      1, 0),
    cohab = ifelse(maritalstat %in% c("Living as couple"),
                   1, 0),
    )



ukhls.df <- as.data.frame(ukhls.df)
ukhls.df$pidp <- as.numeric(ukhls.df$pidp)
ukhls.df$wave <- as.numeric(ukhls.df$wave)




###--------------------------------------###
### Contruct fertility history from data ###
###--------------------------------------###

# Use lprnt lnprnt ladopt lnadopt (first wave variables) to determine the number
# of children in the entrance wave

# Then use father (+nchild): fathered a child since last interview
# The use nnewborn: number of new children since last wave (only if interviewed last wave)

# anychild: time constant identifier if people ever had child


# This will basically kick BHPS



### Order the data
ukhls.df <- ukhls.df[order(ukhls.df$pidp, ukhls.df$wave),]

# person year number 
ukhls.df$pynr <- ave(ukhls.df$pidp,
                     ukhls.df$pidp,
                     FUN = function(x) seq(1, length(x)))


### Calculate new kids (if newborn is missing use change in nkids)
ukhls.df$ch_nkids_ds <- ave(ukhls.df$nkids,
                            ukhls.df$pidp,
                            FUN = function(x) x - dplyr::lag(x))
# omit negative changes 
ukhls.df$ch_nkids_ds[ukhls.df$ch_nkids_ds < 0] <- 0

# New kids
ukhls.df$new_kids <- ukhls.df$nnewborn
oo <- which(is.na(ukhls.df$new_kids))
ukhls.df$new_kids[oo] <- ukhls.df$ch_nkids_ds[oo]



### Starting conditions first wave (biological + adopted children)


# Number kids at beginning
ukhls.df$number_kids <- ukhls.df$lnprnt
# set to zero if none
oo <- which(ukhls.df$lprnt == 2)
ukhls.df$number_kids[oo] <- 0


# Number adopted kids at beginning
ukhls.df$adop_number_kids <- ukhls.df$lnadopt
# set to zero if none
oo <- which(ukhls.df$ladopt == 2)
ukhls.df$adop_number_kids[oo] <- 0


oo <- which(!is.na(ukhls.df$adop_number_kids))
ukhls.df$number_kids[oo] <-
  ukhls.df$number_kids[oo] + ukhls.df$adop_number_kids[oo]

# Tab
table(ukhls.df$number_kids)
table(ukhls.df$adop_number_kids)


# Distribute across observations
ukhls.df$number_kids <- ave(ukhls.df$number_kids,
                            ukhls.df$pidp,
                            FUN = function(x) min(x, na.rm = TRUE))
ukhls.df$number_kids[is.infinite(ukhls.df$number_kids)] <- NA


### Cumulative new kids

# Set first observation to zero
ukhls.df$new_kids[ukhls.df$pynr == 1] <- 0

# Set observation to zero when first number asked
ukhls.df$new_kids[!is.na(ukhls.df$lnprnt)] <- 0
ukhls.df$new_kids[!is.na(ukhls.df$lnadopt)] <- 0

# Sum
ukhls.df <- ukhls.df[order(ukhls.df$pidp, ukhls.df$wave),]
ukhls.df$new_kids <- ave(ukhls.df$new_kids,
                         ukhls.df$pidp,
                         FUN = function(x) cumsum(x))

# Combine
ukhls.df$number_kids <- ukhls.df$number_kids + ukhls.df$new_kids

### Get those to zero that never have a kid
oo <- which(ukhls.df$anychild_dv == 0)
ukhls.df$number_kids[oo] <- 0


# View(ukhls.df[, c("pidp", "wave", "sex", "age_dv",  "nkids", 
#                              "number_kids", "lnprnt", "lprnt", "lnadopt", "new_kids", "nnewborn")])


### If Children at all
ukhls.df$has_child <- ifelse(ukhls.df$number_kids > 0, 1, 0)


table(ukhls.df$number_kids)
table(ukhls.df$has_child)


### Ever kids
ukhls.df$echild <- ave(ukhls.df$has_child,
                       by = ukhls.df$pidp,
                       FUN = function(x) max(x, na.rm = TRUE))



### Generate parity groups
ukhls.df$number_kids_cat <- ifelse(ukhls.df$number_kids >= 2,
                                   2,
                                   ukhls.df$number_kids)
ukhls.df$number_kids_cat <- factor(ukhls.df$number_kids_cat,
                                   levels = c(0, 1, 2),
                                   labels = c("No children",
                                              "One single child",
                                              "Two or more children"))
table(ukhls.df$number_kids_cat )







##########################
#### Sample selection ####
##########################

fm <- as.formula(lab_income ~ has_child 
                 + married + divorced + cohab + health_issue + 
                   owner + unemployed)

### Only below 65
ukhls.df <- ukhls.df[which(ukhls.df$age <= 65), ]

### Drop always kids
ukhls.df$pynr <- ave(ukhls.df$pidp,
                     by = ukhls.df$pidp,
                     FUN = function(x) c(1:length(x)))

oo <- which(ukhls.df$pynr == 1)
ukhls.df$achild <- NA
ukhls.df$achild[oo] <- ukhls.df$has_child[oo]
ukhls.df$achild <- ave(ukhls.df$achild,
                         by = ukhls.df$pidp,
                         FUN = function(x) mean(x, na.rm = TRUE))
table(ukhls.df$achild)

ukhls.df <- ukhls.df[which(ukhls.df$achild == 0), ]


### Listwise deletion
oo <- complete.cases(ukhls.df[,all.vars(fm)])
ukhls.df <- ukhls.df[oo, ]


### At least two years
ukhls.df$pynn <- ave(ukhls.df$pidp,
                     by = ukhls.df$pidp,
                     FUN = function(x) length(x))
table(ukhls.df$pynn)

ukhls.df <- ukhls.df[which(ukhls.df$pynn >= 2), ]


### Check staggered
ukhls.df$mdiff <- ave(ukhls.df$has_child,
                      ukhls.df$pidp,
                      FUN = function(x) x - lag(x))
oo <- which(is.na(ukhls.df$mdiff))
ukhls.df$mdiff[oo] <- 0
table(ukhls.df$mdiff)


### Only women
ukhls.df <- ukhls.df[which(ukhls.df$female == 1), ]


###########################
#### Define event time ####
###########################
ukhls.df <- ukhls.df[order(ukhls.df$pidp, ukhls.df$wave),]

# Count since treatment
ukhls.df$Treat_count <- ave(ukhls.df$has_child,
                            ukhls.df$pidp,
                            FUN = function(x) cumsum(x))

# Anticipation
ukhls.df$Ant_count <- ave(ukhls.df$has_child,
                          ukhls.df$pidp,
                          FUN = function(x) -rev(cumsum(rev(ifelse(x == 0, 1, 0)))))
ukhls.df$Ant_count[which(ukhls.df$Ant_count < -4)] <- 0


# First treatment instance
ukhls.df$Treat_first <- ifelse(ukhls.df$Treat_count == 1, ukhls.df$wave, 0)
ukhls.df$Treat_first <- ave(ukhls.df$Treat_first, ukhls.df$pidp, FUN = max)




table(ukhls.df$time_to_treatment)


##########################
#### Model comparison ####
##########################
ukhls.df <- ukhls.df[order(ukhls.df$pidp, ukhls.df$wave),]
anticipation <- 0

ukhls.df$Treat_first_ant <- ukhls.df$Treat_first - anticipation
ukhls.df$Treat_first_ant[which(ukhls.df$Treat_first_ant == - anticipation)] <- 0 

ukhls.df$has_child_ant <- ave(ukhls.df$has_child,
                              ukhls.df$pidp,
                              FUN = function(x) dplyr::lead(x))



### FE OLS ###

### Prep
# Create event time indicator
ukhls.df$time_to_treatment <- ukhls.df$wave - ukhls.df$Treat_first

# t = -1 and never treated as control
control <- c(-(anticipation + 1), min(ukhls.df$time_to_treatment))
ukhls.df$time_to_treatment <-
  ifelse(ukhls.df$time_to_treatment %in% control | ukhls.df$Treat_first == 0,
         -9999,
         ukhls.df$time_to_treatment)
ukhls.df$time_to_treatment <-
  relevel(as.factor(ukhls.df$time_to_treatment), "-9999")



### Model TWFE
out.fect1 <- feols(lab_income ~ time_to_treatment 
                    + married + divorced + cohab + health_issue + 
                     owner + unemployed | pidp + wave, 
                   data = ukhls.df)

summary(out.fect1)

es.fect1 <- out.fect1$coeftable[grep("time_to_treatment", 
                                     rownames(out.fect1$coeftable)), ]
es.fect1$time <- gsub("time_to_treatment", "", rownames(es.fect1))
es.fect1 <- es.fect1[, c("time", "Estimate", "Std. Error")]

out.fect1.simple <- feols(lab_income ~ has_child 
                    + married + divorced + cohab + health_issue + 
                     owner + unemployed | pidp + wave, 
                   data = ukhls.df)

summary(out.fect1.simple)


### Some descriptives

table(ukhls.df$time_to_treatment)
table(ukhls.df$time_to_treatment, ukhls.df$married)


#--- Model 4 SantAnna
out.did <- att_gt(
  yname = "lab_income",
  gname = "Treat_first",
  idname = "pidp",
  tname = "wave",
  xformla = ~ married + divorced + cohab + health_issue + owner + unemployed,
  control_group = "notyettreated",
  anticipation = anticipation,
  data = ukhls.df,
  allow_unbalanced_panel = TRUE,
  faster_mode = FALSE,
  est_method = "reg"
)
es.did <- aggte(out.did, type = "dynamic", na.rm = TRUE)
summary(es.did)
es.did.simple <- aggte(out.did, type = "simple", na.rm = TRUE)


#--- Model 5 Sun Abraham (with fixest)
out.sa <-
  feols(lab_income ~ sunab(Treat_first, wave, ref.p = -c(1 + anticipation)) + 
          married + divorced + cohab + health_issue + 
          owner + unemployed | pidp + wave, 
        data = ukhls.df)
summary(out.sa)

es.sa <- data.frame(summary(out.sa)$coeftable)
es.sa <- es.sa[which(grepl("wave::", rownames(es.sa))), ]
es.sa$time <- gsub("wave::", "", rownames(es.sa))

out.sa.simple <- summary(out.sa, agg = "ATT")



#--- Model 6 Borusyak
out.bo <- did_imputation(
  data = ukhls.df,
  yname = "lab_income",
  gname = "Treat_first",
  tname =  "wave",
  idname = "pidp",
  first_stage = ~ married + divorced + cohab + health_issue + 
    owner + unemployed | pidp + wave,
  horizon = TRUE,
  pretrends = c(-10:-2) ## setting true --> add zero period below
)
out.bo


out.bo.simple <- did_imputation(
  data = ukhls.df,
  yname = "lab_income",
  gname = "Treat_first",
  tname =  "wave",
  idname = "pidp",
  first_stage = ~ married + divorced + cohab + health_issue + 
    owner + unemployed | pidp + wave
)
out.bo.simple


#--- Model 7 Wooldridge
out.etwfe <- etwfe(
  fml  = lab_income ~ married + divorced + cohab + health_issue + 
    owner + unemployed,
  tvar = "wave",
  gvar = "Treat_first",
  data = ukhls.df
)

es.etwfe <- emfx(out.etwfe,
                 type = "event",
                 vcov = TRUE,
                 post_only = FALSE,
                 window = 8,
                 compress = TRUE
                )


es.etwfe
# save(es.etwfe, file = "ETWFE_est.RData")

es.etwfe.simple <- emfx(out.etwfe,
                        type = "simple",
                        vcov = TRUE,
                        post_only = FALSE)
# save(es.etwfe.simple, file = "ETWFE_est_simple.RData")



#--- Model 2 IFE
out.fect2 <-
  fect(
    lab_income ~ has_child,
    X = married + divorced + cohab + health_issue + 
      owner + unemployed,
    data = ukhls.df,
    na.rm = TRUE,
    index = c("pidp", "wave"),
    method = "ife",
    force = "two-way",
    CV = FALSE,
    r = 1,
    min.T0 = 1,
    se = TRUE,
    parallel = TRUE, 
    cores = 8
  )
plot(out.fect2)


#--- Model 3 Matrix Completion (lambda.norm* = 0.421696503428582)
out.fect3 <-
  fect(
    lab_income ~ has_child,
    X = hhincome + married + divorced + cohab + health_issue + 
      owner + unemployed,
    data = ukhls.df,
    na.rm = TRUE,
    index = c("pidp", "wave"),
    method = "mc",
    force = "two-way",
    CV = TRUE,
    nlambda = 10,
    k = 3,
    min.T0 = 1,
    se = TRUE,
    parallel = TRUE, 
    cores = 8
  )

plot(out.fect3)

save(out.fect3, file = "MC_est2.RData")



##############
### Export ###
##############
T = 10
res.fect1 <- data.frame(time = c(as.numeric(es.fect1$time), -c(1 + anticipation)), # Add zero / -1 reference period
                  est = c(es.fect1$Estimate, 0),
                  se = c(es.fect1$`Std. Error`, NA),
                  name = "twfe")
res.fect2 <- data.frame(time = out.fect2$time - (1 + anticipation),
                  est = out.fect2$att,
                  se = sqrt(diag(out.fect2$att.vcov)),
                  name = "ife")
res.fect3 <- data.frame(time = out.fect3$time - (1 + anticipation),
                  est = out.fect3$att,
                  se = sqrt(diag(out.fect3$att.vcov)),
                  name = "mc")
res.did <- data.frame(time = es.did$egt,
                est = es.did$att.egt,
                se = es.did$se.egt,
                name = "did")
res.sa <- data.frame(time = c(as.numeric(es.sa$time), -c(1 + anticipation)), # Add zero / -1 reference period
               est = c(es.sa$Estimate, 0),
               se = c(es.sa$Std..Error, NA),
               name = "sa")
out.bo <- out.bo[which(as.numeric(out.bo$term) %in% c(-T:T)),]
res.bo <- data.frame(time = c(as.numeric(out.bo$term), -c(1 + anticipation)),
               est = c(out.bo$estimate, 0),
               se = c(out.bo$std.error, NA),
               name = "bo")

res.etwfe <- data.frame(time = as.numeric(es.etwfe$event) - anticipation,
                  est = es.etwfe$estimate,
                  se = c(es.etwfe$std.error),
                  name = "etwfe")

# combine
res.df <- bind_rows(res.fect1, 
                    res.fect2, 
                    res.fect3, 
                    res.did, 
                    res.sa, 
                    res.bo, 
                    res.etwfe) %>%
  mutate(
    time = as.numeric(time),
    est  = as.numeric(est),
    se   = as.numeric(se)
  )


### Extract summary measure for ATT
sum.df <- data.frame(matrix(ncol = 3, nrow = 7))
colnames(sum.df) <- c("estimator", "att", "se") 
sum.df$estimator <- c("twfe",
                      "ife",   
                      "mc",    
                      "did",   
                      "sa",    
                      "bo",    
                      "etwfe"
)


sum.df[sum.df$estimator == "twfe", c(2:3)] <- c(out.fect1.simple$coeftable["has_child", 1],
                                                out.fect1.simple$coeftable["has_child", 2])
sum.df[sum.df$estimator == "ife", 2:3] <- c(out.fect2$att.avg, 
                                            out.fect2$est.avg[2])
sum.df[sum.df$estimator == "mc", 2:3] <- c(out.fect3$att.avg, 
                                           out.fect3$est.avg[2])
sum.df[sum.df$estimator == "did", 2:3] <- c(es.did.simple$overall.att,
                                          es.did.simple$overall.se)
sum.df[sum.df$estimator == "sa", 2:3] <- c(out.sa.simple$coeftable["ATT", 1],
                                           out.sa.simple$coeftable["ATT", 2])
sum.df[sum.df$estimator == "etwfe", 2:3] <- c(es.etwfe.simple$estimate,
                                              es.etwfe.simple$std.error)
sum.df[sum.df$estimator == "bo", 2:3] <- c(out.bo.simple$estimate,
                                           out.bo.simple$std.error)



### Save
save(res.df, file = "Case_study_event_2.RData")
save(sum.df, file = "Case_study_simple_2.RData")






###########################
### Plot Event time ATT ###
###########################

models = c("Conventional TWFE",  
           "Matrix Completion",    
           "Callaway SantAnna",   
           "Sun Abraham",    
           "Borusyak et al",   
           "Wooldridge ETWFE")

# Name models
res.df$name <- factor(res.df$name, levels = c("real",  
                                              "ols",   
                                              "twfe",
                                              "feis",
                                              "ife",   
                                              "mc",    
                                              "did",   
                                              "sa",    
                                              "bo", 
                                              "etwfe",
                                              "did2"),
                      labels = c("real",  
                                 "OLS",   
                                 "Conventional TWFE",  
                                 "FEIS",
                                 "Interactive FE",   
                                 "Matrix Completion",    
                                 "Callaway SantAnna",   
                                 "Sun Abraham",    
                                 "Borusyak et al",   
                                 "Wooldridge ETWFE",
                                 "Callaway SantAnna NYT"))


# Subset
mod_es <- res.df[which(res.df$name %in% models), ]

# Push time scale to be consistent
mod_es$time <- mod_es$time + 1

# Uncertainty estimates +- 1 sd
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier
mod_es$conf.low <- mod_es$est - interval2 * mod_es$se
mod_es$conf.high <- mod_es$est + interval2 * mod_es$se


# Restrict window
t <- 6
mod_es <- mod_es[which(mod_es$time <= 9 & mod_es$time >= -4),]


# Plot
zp <- ggplot(mod_es, aes(x = time, y = est)) +
  facet_wrap(vars(name), ncol = max(1, round(length(models)/2))) + 
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0.5, linetype = "dashed") +
  geom_pointrange(data = mod_es, aes(x = time, y = est,
                                     ymin = conf.low, ymax = conf.high,
                                     color = name), 
                  #col = "darkcyan"
  ) +
  scale_color_viridis_d(option = "G", end = 0.80, begin = 0.2, guide = "none") +
  theme_minimal() + theme(panel.grid.minor = element_blank(),
                          text = element_text(family = "Times New Roman", size = 18),
                          axis.text = element_text(colour = "black")) +
  labs(x = "Event time (Treatment = 1)", 
       y = paste0("Estimated Effect on Y ", "with ", "95%", " CIs")) 
  #coord_cartesian(ylim = ylim)
zp


png(filename = "Case_Femaleincome_att.png",
    width = 16, height = 9, units = "in", res = 300,
    type = "cairo")
zp
dev.off()




#######################
### Plot Single ATT ###
#######################


models = c(
  "Conventional TWFE",  
  "Matrix Completion",    
  "Callaway SantAnna",   
  "Sun Abraham",    
  "Borusyak et al",   
  "Wooldridge ETWFE"
)


  # Name models
  sum.df$name <- factor(
    sum.df$estimator,
    levels = rev(c(
      "real",
      "ols",
      "twfe",
      "feis",
      "ife",
      "mc",
      "did",
      "sa",
      "bo",
      "etwfe",
      "did2"
    )),
    labels = rev(c(
      "real",
      "OLS",
      "Conventional TWFE",
      "FEIS",
      "Interactive FE",
      "Matrix Completion",
      "Callaway SantAnna",
      "Sun Abraham",
      "Borusyak et al",
      "Wooldridge ETWFE",
      "Callaway SantAnna NYT"
    ))
  )

# Subset
mod_es <- sum.df[which(sum.df$name %in% models), ]

# Uncertainty estimates +- 1 sd
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier
mod_es$conf.low <- mod_es$att - interval2 * mod_es$se
mod_es$conf.high <- mod_es$att + interval2 * mod_es$se

 zp2 <- ggplot(mod_es, aes(x = name, y = att)) +
   geom_hline(yintercept = 0, linetype = "dashed", color = "deeppink1", lwd = 1.1) +
    geom_vline(xintercept = 0.5, linetype = 1) +
    geom_pointrange(data = mod_es, aes(x = name, y = att,
                                       ymin = conf.low, ymax = conf.high,
                                       color = name, fill = name), 
                    shape = 21, size = 0.8, lwd = 1.05
    ) +
    coord_flip() +
    scale_x_discrete()+
    scale_color_viridis_d(option = "G", end = 0.80, begin = 0.2, guide = "none") +
    scale_fill_viridis_d(option = "B", end = 0.80, begin = 0.2, guide = "none", direction = -1) +
    theme_minimal() + theme( # panel.grid.minor = element_blank(),
      text = element_text(family = "Times New Roman", size = 18),
      axis.text = element_text(colour = "black")) +
    labs(x = element_blank(),
         y = paste0("Average ATT ", "with ", "95%", " CIs"))
zp2


png(filename = "Case_Femaleincome_avg.png",
    width = 9, height = 9, units = "in", res = 300,
    type = "cairo")
zp2
dev.off()




### Combine plots

zp3 <- grid.arrange(zp2, 
                   zp,
                   layout_matrix = rbind(c(1, 1, 2, 2, 2, 2),
                                         c(1, 1, 2, 2, 2, 2)))
zp3

png(filename = "Case_Femaleincome_comb.png",
    width = 12, height = 7, units = "in", res = 300,
    type = "cairo")
grid.draw(zp3)
dev.off()
