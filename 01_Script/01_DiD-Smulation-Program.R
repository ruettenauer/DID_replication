###########################
#### Utility Functions ####
###########################

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
# loadfonts()


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

#################################
#### DID Simulation Function ####
#################################

sim_did <- function(
    seed = 3283755, # set seed
    N = 2000, # number of cross-sectional units
    T = 15, # Number of time periods
    R = 30, # Number of simulation runs
    scale.ps = 5, # Scaling parameter for binomial
    b.b = c(1, 0.2, 0, 0), # Cross-sectional selection, 4 coefs
    b.x = c(-0.3, 0.1, 0, 0), # time-varying selection, emp correlation bet D and x
    b.trend = c(0.3), # Trend in Y
    # st_tr = 2, # first period in which treatment can occur
    treat.sd = 2, # SD for probability distribution of treatment timing (determines where treatment happens)
    b.nonparallel = c(0), # Extra trend for treated (vs. non-treated) # ADD Extra trend for early treated vs late?
    beta = 1, # Treatment effect (and its distribution)
    beta.t = c(0.15, 0.25, 0.3, 0.2, 0.1), # Temporal distribution of treatment effect
    anticipaton.t = c(0, 0, 0, 0), # Anticipation effect before treatment (t-4, t-3, t-2, t-1)
    beta.late = FALSE, # early vs late with heterogeneous ATT, factor for late (early will have 1/beta.late), 
    size.late = 0.5, # share of late treatment group in case of beta.late 
    crs = NA, #Parallel cores
    fading = FALSE, ... # fading effect between treatment groups
){
  
  # Save parameters
  parameters <- list(seed = seed,
                  N = N, 
                  T = T, 
                  R = R, 
                  scale.ps = scale.ps,
                  b.b = b.b,
                  b.x = b.x,
                  b.trend = b.trend,
                  treat.sd = treat.sd,
                  b.nonparallel = b.nonparallel,
                  beta = beta,
                  beta.t = beta.t,
                  anticipaton.t = anticipaton.t,
                  beta.late = beta.late,
                  size.late = size.late)
  
  # Set seed
  set.seed(seed)
  maxint <- 2147483647
  seeds <- runif(R, min = -maxint, max = maxint)
  
  
  # Determine other params
  NT <- N * T

  ### Start individual run
  
  ### Register Cores
  if(is.na(crs)){
    crs <- detectCores(all.tests = FALSE, logical = TRUE)
    print(crs)
  }
  cl <- makeCluster(crs)
  registerDoParallel(cl)
  
  res <- foreach(i = 1:R) %dopar% {
    
    library(fect)
    library(did)
    library(staggered)
    library(didimputation)
    library(etwfe)
    library(fixest)
    library(feisr)
    # library(faux)
    library(ggplot2)
    
    # Set seed
    set.seed(seeds[i])
    
    # ids
    id <- rep(1:N, each = T)
    t <- rep(c(1:T), times = N)
    
    # Generate time constant B
    B1 <- stats::rnorm(N)
    B2 <- stats::rnorm(N)
    B3 <- stats::rnorm(N)
    B4 <- stats::rnorm(N)
    B <- cbind(B1, B2, B3, B4)
    
    # Generate pscore and Treatment Status
    b.b.Times.x <- B %*% b.b
    pr <- 1/(1 + exp(- scale.ps * b.b.Times.x))
    Treat <- stats::rbinom(N, size = 1, prob = pr)
    
    # # Test
    # mod <- glm(Treat ~ B, family = "binomial")
    # summary(mod)
    
    # Extend
    df <- data.frame(id, t, apply(data.frame(B, pr, Treat), 2, 
                                  FUN = function(x) rep(x, each = T)))
    
    
    
    #### Version 3 ####
    
    ### Generate time varying X
    
    # Define X1 as completely random
    X1 <- stats::rnorm(NT)
    
    # Trending variable
    MT <- cumsum(rnorm(n = T, mean = seq(length.out = T, by = 0.1), sd = 1))
    X2 <- as.numeric(scale(rnorm(n = NT, mean = MT, sd = 1)))
    
    # Binary X, normal distributed treatment timing
    xt <- c(1:T)
    m <- round(mean(xt)) # mean
    
    if(treat.sd == "twotime"){
      pt <- dnorm(xt, mean = m, sd = 2) # probability distribution
    }else{
      pt <- dnorm(xt, mean = m, sd = treat.sd) # probability distribution
    }
    
    # Sample 
    treatgroup <- sample(xt, size = N, replace = TRUE, prob = pt)
    treatgroup <- rep(treatgroup, each = T)
    X3 <- ifelse(df$t >= treatgroup, 1, 0)

    # Define X4 as time trend
    X4 <- as.numeric(scale(t))
    
    # merge
    X <- cbind(X1, X2, X3, X4)
    df <- cbind(df, X)
    
    
    ### Fill function
    fill <- function(x, t){
      res <- rep(0, t)
      res[x] <- 1
      return(res)
    }
    
    
    ### Treatment timing
    if(treat.sd == "twotime"){
      
      # Randomly select one out of two time points
      t1 <- unname(round(quantile(c(1:T), probs = 0.25)))
      t2 <- unname(round(quantile(c(1:T), probs = 0.75)))
      rand <- sample(c(t1, t2), N, replace = TRUE)
      
      Treatx <- by(rand, 
                   unique(df$id),
                   FUN = function(z) fill(z, T),
                 simplify = FALSE)
      
    }else{
      
      # Without any covariates, just take the normal distribution
      if(all(b.x == 0)){
        
        # Normal dist
        pt <- dnorm(xt, mean = m, sd = treat.sd)
        
        # Sample, but omit first time period 
        treatgroup <- sample(xt[-1], size = N, replace = TRUE, prob = pt[-1])
        
        Treatx <- by(treatgroup, 
                     unique(df$id),
                     FUN = function(z) fill(z, T),
                     simplify = FALSE)
      
        
        ### Otherwise include covariates in cumulative distribution
      }else{
        
        # Treatment probability
        xt <- c(1:T)
        m <- round(mean(xt)) # mean
        
        # Introduce a "constant" from cumulative normal
        pt <- rep(cumsum(dnorm(xt, mean = m, sd = treat.sd)), N)
        
        # Add covariates
        pr <- as.numeric(-0.5 + pt + X %*% b.x)
        
        # treatment as inverse logit
        prt <- 1/(1 + exp(- scale.ps * pr))
        Treatx <- by(prt, 
                     df$id,
                     FUN = function(z) stats::rbinom(T, 
                                                     size = 1, 
                                                     prob = z))
        
      }
      
      
      

    }
    

    
    
    # Check if some have no treatment timing, and replace with random 
    sum_Tx <- sapply(Treatx, sum)
    if(any(sum_Tx == 0)){
      oo <- which(sum_Tx == 0)
      for(i in oo){
        rn <- sample(c(1:T), size = 1, replace = FALSE)
        Treatx[[i]][rn] <- 1
      }
    }
    
    
    
    # Make perfectly staggered design by taking D=1 after first treatment occur
    Treatx <- ave(unlist(Treatx),
                  df$id,
                  FUN = function(x) lapply(cumsum(x),
                                           function(z) min(z, 1)))
    
    # Build time-varying treatment
    df$Treatx <- df$Treat * unlist(Treatx)
    
    # Make sure everyone in Treat has a treatment timing
    df$Treatx_sum <- ave(df$Treatx,
                         df$id,
                         FUN = function(x) max(x))
    if(!all(df$Treatx_sum == df$Treat)){
      warning("Some in treatment group have no treatment timing.")
    }
    
    # Count since treatment
    df$Treat_count <- ave(df$Treatx,
                          df$id,
                          FUN = function(x) cumsum(x))
    
    # Anticipation
    df$Ant_count <- ave(df$Treatx,
                          df$id,
                          FUN = function(x) -rev(cumsum(rev(ifelse(x == 0, 1, 0)))))
    df$Ant_count[which(df$Ant_count < -4)] <- 0
    
    
    # First treatment instance
    df$Treat_first <- ifelse(df$Treat_count == 1, df$t, 0)
    df$Treat_first <- ave(df$Treat_first, df$id, FUN = max)
    
    # # Plot the treatment distribution
    # tr.pl <- ggplot(data = data.frame(t = df$t, Treatx = df$Treatx),
    #                 aes(x=t, y=Treatx)) +
    #   geom_count() +
    #   geom_smooth()
    # tr.pl
  
    
    # Matrix for time varying treatment effect
    beta.timevarying <- beta * beta.t
    for(j in 1:length(beta.t)){
      X <- ifelse(df$Treat_count == j, 1, 0)
      if(j == 1){
        Treatmat <- X
      }else{
        Treatmat <- cbind(Treatmat, X)
      }
    }
    
    # Matrix for time varying anticipation effect
    for(j in -length(anticipaton.t):-1){
      X <- ifelse(df$Ant_count == j, 1, 0)
      if(j == -length(anticipaton.t)){
        Antmat <- X
      }else{
        Antmat <- cbind(Antmat, X)
      }
    }
    
    
    # ATT by early vs late with b_l = 0.5*b_e, but keep mean
    if(treat.sd == "twotime"){
      
      if(beta.late != FALSE){
        beta.timevarying_e <- 2 * beta.timevarying / (beta.late + 1)
        beta.timevarying_l <- beta.late * beta.timevarying_e
        beta.timevarying_l <- beta.timevarying_l - beta.timevarying_e # use diff for interaction
        flag_late <- 1
        
        # define early and late groups according to numbers above
        df$late <- ifelse(df$Treat_first >= t2, 1, 0)
        df$late[is.na(df$late)] <- 0
        
        
      }else{
        beta.late <- 0
        flag_late <- 0
        size.late <- 0
        beta.timevarying_e <- beta.timevarying
        beta.timevarying_l <- rep(0, length(beta.timevarying))
        
        df$late <- 0
      }
      
      
    }else{
      
      if(beta.late != FALSE){
        beta.timevarying_e <- 2 * beta.timevarying / (beta.late + 1)
        beta.timevarying_l <- beta.late * beta.timevarying_e
        beta.timevarying_l <- beta.timevarying_l - beta.timevarying_e # use diff for interaction
        flag_late <- 1
        
        # define early and late groups according to size 
        # using groups as freq, leads to underrepresentation of late obs (ok or not??)
        qunt.late <- quantile(df$Treat_first[df$Treat_first>0], probs = size.late)
        df$late <- ifelse(df$Treat_first > qunt.late, 1, 0)
        df$late[is.na(df$late)] <- 0
        
        
        # Fading between treatment groups
        if(fading == TRUE){
          
          mint <- min(df$Treat_first[df$Treat_first != 0])
          maxt <- max(df$Treat_first[df$Treat_first != 0])
          diff <- maxt - mint
          df$late <- ifelse(df$Treat_first != 0, df$Treat_first - mint, 0) / (maxt - mint)
        }
        
          
          
        
        
      }else{
        beta.late <- 0
        flag_late <- 0
        size.late <- 0
        beta.timevarying_e <- beta.timevarying
        beta.timevarying_l <- rep(0, length(beta.timevarying))
        
        df$late <- 0
      }
      
    }
    


    
    # Define the outcome model
    e <- rnorm(NT, 0, sd = 0.2)
    Y <- df$B1 + df$B2 + df$B3 + df$B4 +
       Treatmat %*% beta.timevarying_e +
      df$late * Treatmat %*% beta.timevarying_l +
      Antmat %*% anticipaton.t +
      b.trend * df$t + 
      b.nonparallel * df$Treat * df$t +
      as.matrix(df[, c("X1", "X2", "X3", "X4")]) %*% b.x +
      e
    Y <- as.numeric(Y)
    df <- cbind(df, Y)
    
    
    # Define the counterfactual
    Treatmat0 <- Treatmat
    Treatmat0[Treatmat0 == 1] <- 0
    Y0 <- df$B1 + df$B2 + df$B3 + df$B4 +
      Treatmat0 %*% beta.timevarying_e +
      df$late * Treatmat0 %*% beta.timevarying_l +
      Antmat %*% anticipaton.t +
      b.trend * df$t + 
      b.nonparallel * df$Treat * df$t +
      as.matrix(df[, c("X1", "X2", "X3", "X4")]) %*% b.x +
      e
    Y0 <- as.numeric(Y0)
    df <- cbind(df, Y0)
    
    # # Plot outcome
    # tmp <- df
    # tmp$Treat <- as.factor(tmp$Treat)
    # zp <- ggplot(data = tmp, aes(x = t, y = Y0, color = Treat, group = id)) +
    #   geom_line(alpha = 0.2) +
    #   geom_smooth(aes(group = Treat), linewidth = 2.5, color = "yellow") +
    #   geom_smooth(aes(group = Treat, color = Treat), se = FALSE) +
    #   geom_smooth(aes(y = Y, group = Treat, color = Treat), se = FALSE) +
    #   theme_bw()
    # zp

    
    # individual treatment effect
    df$ite <- as.numeric(Treatmat %*% beta.timevarying_e +
      df$late * Treatmat %*% beta.timevarying_l)
    
    
    # Define treatment indicator
    df$DT <- ave(rowSums(Treatmat),
                 df$id,
                 FUN = function(x) cumsum(x))
    df$DT[which(df$Treat_count > ncol(Treatmat))] <- 0
    df$DT <- as.factor(df$DT)
    
    # Define anticipation indicator
    df$Ant <- ave(rowSums(Antmat),
                 df$id,
                 FUN = function(x) -rev(cumsum(rev(x))))
    df$Ant[which(abs(df$Ant_count) > ncol(Antmat))] <- 0
    df$Ant <- as.factor(df$Ant)

    
    
    #######################
    ### Estimate models ###
    #######################
    
    #--- Model following DGP
    if(flag_late == 1){
      real.mod <- lfe::felm(Y ~ DT + DT*late +
                              Ant +
                              B1 + B2 + B3 + B4 +
                              t + 
                              Treat * t + 
                              X1 + X2 + X3 + X4,
                            data = df)
      real.mod.simple <- lm(Y ~ Treatx + Treatx*late +
                              Ant +
                              B1 + B2 + B3 + B4 +
                              t + 
                              Treat * t + 
                              X1 + X2 + X3 + X4,
                            data = df)
      
    }else{
      real.mod <- lfe::felm(Y ~ DT + 
                              Ant +
                              B1 + B2 + B3 + B4 +
                              t + 
                              Treat * t + 
                              X1 + X2 + X3 + X4,
                            data = df)
      real.mod.simple <- lm(Y ~ Treatx + 
                              Ant +
                              B1 + B2 + B3 + B4 +
                              t + 
                              Treat * t + 
                              X1 + X2 + X3 + X4,
                            data = df)
    }

    
    #--- Model 0 OLS
    out.fect0 <-
      fect(
        Y ~ Treatx,
        data = df,
        index = c("id", "t"),
        method = "fe",
        force = "none"
      )
    out.fect0.simple <- lm(Y ~ Treatx, data = df)
    # plot(out.fect0)
    
    
    #--- Model 1 TWFE
    # out.fect1 <-
    #   fect(
    #     Y ~ Treatx,
    #     data = df,
    #     index = c("id", "t"),
    #     method = "fe",
    #     force = "two-way"
    #   )
    
    # Create event time indicator
    df$time_to_treatment <- df$t - df$Treat_first
    
    # t = -1 and never treated as control
    control <- c(-1, min(df$time_to_treatment))
    df$time_to_treatment <-
      ifelse(df$time_to_treatment %in% control | df$Treat == 0,
             -9999,
             df$time_to_treatment)
    df$time_to_treatment <-
      relevel(as.factor(df$time_to_treatment), "-9999")
    
    
    out.fect1 <- feols(Y ~ time_to_treatment | id + t, df)
    
    es.fect1 <- out.fect1$coeftable[grep("time_to_treatment", 
                                        rownames(out.fect1$coeftable)), ]
    es.fect1$time <- gsub("time_to_treatment", "", rownames(es.fect1))
    es.fect1 <- es.fect1[, c("time", "Estimate")]
    
    
    out.fect1.simple <-
      feols(Y ~ Treatx | id + t, df)
    
    
    #--- Model 2 IFE
    out.fect2 <-
      fect(
        Y ~ Treatx,
        data = df,
        index = c("id", "t"),
        method = "ife",
        force = "two-way",
        CV = TRUE,
        r = c(0:5),
        k = 10,
        min.T0 = 1
        # parallel = TRUE
      )
    # plot(out.fect2)
    
    
    #--- Model 3 Matrix Completion
    out.fect3 <-
      fect(
        Y ~ Treatx,
        data = df,
        index = c("id", "t"),
        method = "mc",
        force = "two-way",
        CV = TRUE,
        nlambda = 10,
        k = 10,
        min.T0 = 1
        # parallel = TRUE
      )
    
    
    #--- Model 4 SantAnna
    out.did <- att_gt(
      yname = "Y",
      gname = "Treat_first",
      idname = "id",
      tname = "t",
      # xformla = ~ X1 + X2 + X3,
      control_group = "nevertreated",
      data = df,
      est_method = "dr"
    )
    es.did <- aggte(out.did, type = "dynamic", na.rm = TRUE)
    # ggdid(es.did)
    
    
    #--- Model 5 Sun Abraham (with fixest)
    out.sa <-
      feols(Y ~ sunab(Treat_first, t) | id + t, df)
    es.sa <- data.frame(summary(out.sa)$coeftable)
    es.sa <- es.sa[which(grepl("t::", rownames(es.sa))), ]
    es.sa$time <- gsub("t::", "", rownames(es.sa))
    
    out.sa.simple <- summary(out.sa, agg = "ATT")
    
    
    #--- Model 6 Borusyak
    out.bo <- did_imputation(
      data = df,
      yname = "Y",
      gname = "Treat_first",
      tname =  "t",
      idname = "id",
      # first_stage = ~  0 | id + t,
      horizon = TRUE,
      pretrends = TRUE ## setting true --> add zero period below
    )
    
    
    #--- Model 7 Wooldridge
    out.etwfe <- etwfe(
      fml  = Y ~ 0,
      tvar = "t",
      gvar = "Treat_first",
      data = df
    )
    es.etwfe <- emfx(out.etwfe,
                     type = "event",
                     vcov = FALSE,
                     post_only = FALSE)
    es.etwfe.simple <- emfx(out.etwfe,
                            type = "simple",
                            vcov = FALSE,
                            post_only = FALSE)
    
    #--- Model 7 SantAnna - Control: not yet treated
    out.did2 <- att_gt(
      yname = "Y",
      gname = "Treat_first",
      idname = "id",
      tname = "t",
      # xformla = ~ X1 + X2 + X3,
      control_group = "notyettreated",
      data = df,
      est_method = "dr"
    )
    es.did2 <- aggte(out.did2, type = "dynamic", na.rm = TRUE)
    
    
    #--- Model 8 FEIS
    
    # Create event time indicator
    df$time_to_treatment <- df$t - df$Treat_first
    
    # t = -1 and never treated as control
    control <- c(-1, min(df$time_to_treatment))
    df$time_to_treatment <-
      ifelse(df$time_to_treatment %in% control | df$Treat == 0,
             -9999,
             df$time_to_treatment)
    df$time_to_treatment <-
      relevel(as.factor(df$time_to_treatment), "-9999")
    
    
    # Est model
    out.feis <-
      feis(
        formula  = Y ~ time_to_treatment | t,
        id = "id",
        data = df
      )
    es.feis <- data.frame(summary(out.feis)$coefficients)
    es.feis <-
      es.feis[grep("time_to_treatment", rownames(es.feis)), ]
    es.feis$time <- gsub("time_to_treatment", "", rownames(es.feis))

    out.feis.simple <-
      feis(formula  = Y ~ Treatx | t,
           id = "id",
           data = df)
    
    
    ##############
    ### Export ###
    ##############
    
    # combine with time = 1 as first treatment period
    prop <- table(df$late[df$Treatx == 1]) / sum(df$Treatx)
    if(flag_late == 0){
      prop <- c(1, 0)
    }
    dk <- max(as.numeric(levels(df$DT)))
    st <- which(rownames(real.mod$coefficients) == "DT1")
    stl <- which(rownames(real.mod$coefficients) == "DT1:late")
    if(flag_late){
      tmp <- summary(real.mod)$coefficients[stl:(stl - 1 + dk), 1]
      tmp[which(is.na(tmp))] <- 0
      es.real <- cbind(time = c(1:dk),
                       est.real = summary(real.mod)$coefficients[st:(st - 1 + dk), 1] +
                         prop[2] * tmp
      )
    }else{
      es.real <- cbind(time = c(1:dk),
                       est.real = summary(real.mod)$coefficients[st:(st - 1 + dk), 1] 
      )
    }

    es.fect0 <- cbind(time = out.fect0$time,
                      est.ols = out.fect0$att)
    # es.fect1 <- cbind(time = out.fect1$time,
    #                   est.twfe = out.fect1$att)
    es.fect1 <- cbind(time = c(as.numeric(es.fect1$time) + 1, 0), # Add zero / -1 reference period
                      est.twfe = c(es.fect1$Estimate, 0))
    es.fect2 <- cbind(time = out.fect2$time,
                      est.ife = out.fect2$att)
    es.fect3 <- cbind(time = out.fect3$time,
                      est.mc = out.fect3$att)
    es.did <- cbind(time = es.did$egt + 1,
                    est.did = es.did$att.egt)
    es.sa <- cbind(time = c(as.numeric(es.sa$time) + 1, 0), # Add zero / -1 reference period
                   est.sa = c(es.sa$Estimate, 0))
    out.bo <- out.bo[which(out.bo$term %in% c(-T:T)),]
    
    # Sometimes Bo includes -1 as pre-trend
    if(-1 %in% out.bo$term){
      es.bo <- cbind(time = c(as.numeric(out.bo$term) + 1),
                     est.bo = c(out.bo$estimate))
    }else{
      es.bo <- cbind(time = c(as.numeric(out.bo$term) + 1, 0),
                     est.bo = c(out.bo$estimate, 0))
    }
    
    es.etwfe <- cbind(time = as.numeric(es.etwfe$event) + 1,
                      est.etwfe = es.etwfe$estimate)
    es.did2 <- cbind(time = es.did2$egt + 1,
                    est.did2 = es.did2$att.egt)
    es.feis <- cbind(time = c(as.numeric(es.feis$time) + 1, 0), # Add zero / -1 reference period
                     est.feis = c(es.feis$Estimate, 0))
    
    # combine
    res.df <- merge(data.frame(time = c(-T:T)), es.real, by = "time", all.x = TRUE)
    res.df <- merge(res.df, es.fect0, by = "time", all.x = TRUE )
    res.df <- merge(res.df, es.fect1, by = "time", all.x = TRUE )
    res.df <- merge(res.df, es.fect2, by = "time", all.x = TRUE )
    res.df <- merge(res.df, es.fect3, by = "time", all.x = TRUE )
    res.df <- merge(res.df, es.did, by = "time", all.x = TRUE )
    res.df <- merge(res.df, es.sa, by = "time", all.x = TRUE )
    res.df <- merge(res.df, es.bo, by = "time", all.x = TRUE )
    res.df <- merge(res.df, es.etwfe, by = "time", all.x = TRUE )
    res.df <- merge(res.df, es.did2, by = "time", all.x = TRUE )
    res.df <- merge(res.df, es.feis, by = "time", all.x = TRUE )
    
    ### Extract summary measure for ATT
    sum.df <- data.frame(matrix(ncol = 3, nrow = 11))
    colnames(sum.df) <- c("estimator", "att1", "att2") 
    sum.df$estimator <- c("real", "ols",   
                          "twfe",
                          "feis",
                          "ife",   
                          "mc",    
                          "did",   
                          "sa",    
                          "bo",    
                          "etwfe",
                          "did2"
                          )
    
    # Use available summary measures
    if(flag_late){
      sum.df[sum.df$estimator == "real", 2] <- real.mod.simple$coefficients["Treatx"] +
        prop[2] * real.mod.simple$coefficients["Treatx:late"]
    }else{
      sum.df[sum.df$estimator == "real", 2] <- real.mod.simple$coefficients["Treatx"] 
    }
    
    
    sum.df[sum.df$estimator == "ols", 2] <- out.fect0.simple$coefficients["Treatx"]
    
    sum.df[sum.df$estimator == "twfe", 2] <- out.fect1.simple$coeftable["Treatx", 1]
    # what is att.avg from fect?
    #sum.df[sum.df$estimator == "twfe", 2:3] <- c(out.fect1$att.avg, out.fect1$att.avg.unit) 
    
    sum.df[sum.df$estimator == "ife", 2:3] <- c(out.fect2$att.avg, out.fect2$att.avg.unit)
    sum.df[sum.df$estimator == "mc", 2:3] <- c(out.fect3$att.avg, out.fect3$att.avg.unit)
    sum.df[sum.df$estimator == "did", 2] <- aggte(out.did, type = "simple", na.rm = TRUE)$overall.att
    sum.df[sum.df$estimator == "etwfe", 2] <- es.etwfe.simple$estimate
    sum.df[sum.df$estimator == "did2", 2] <- aggte(out.did2, type = "simple", na.rm = TRUE)$overall.att
    sum.df[sum.df$estimator == "feis", 2] <- out.feis.simple$coefficients["Treatx"]
    sum.df[sum.df$estimator == "sa", 2] <- out.sa.simple$coeftable["ATT", 1]
    
    # otherwise calculate using all time periods weighted by their empirical occurrence
    df$D_var <- ave(df$Treatx, 
                    df$Treat_first, 
                    FUN = function(x) var(x))
    freq1 <- table(df$Treat_count[df$D_var > 0])[-1] # obs per period after treatment
    freq2 <- table(df$Treat_first[df$t == 1 & df$D_var > 0])[-1]  # obs per treatment group
    
    # Borusyak
    pos.att <- es.bo[es.bo[,1] >= 1, 2]
    pos.att <- pos.att[which(!is.na(pos.att))]
    bo.att <- weighted.mean(pos.att, freq1[1:length(pos.att)])
    sum.df[sum.df$estimator == "bo", 2] <- bo.att
    
    
    
    
    
    
    ### true ATT ###
    
    ### ATT in the data: average Y - Y0 for all treatment periods
    oo <- which(df$Treatx == 1)
    df_att <- df$Y[oo] - df$Y0[oo]
    df_att_time <- aggregate(df_att,
                             by = list(Treat_count = df$Treat_count[oo]),
                             FUN = function(x) mean(x, na.rm = TRUE))
    df_att <- mean(df_att)
    
    
    ### ATT as average of time-specific treatment effects
    
    # Frequencies and variations
    tr_timing <- table(df$Treat_count)[-1]
    tr_groups <- table(df$Treat_first)[-1]
    tr_var <- as.numeric(by(df$Treatx, df$Treat_first, var))[-1]
    natt <- length(beta.timevarying)
    
    if(flag_late == 1){
      # Group specific treatment effects
      b_e <- beta.timevarying_e
      b_l <- beta.timevarying_e * beta.late
      
      # Fill zeros
      b_e_f <- c(b_e, rep(0, T - natt))
      b_l_f <- c(b_l, rep(0, T - natt))
      
      # Treatment timings by group
      tr_timing_e <- table(df$Treat_count[df$Treatx == 1 & df$late == 0])
      tr_timing_l <- table(df$Treat_count[df$Treatx == 1 & df$late == 1])
      if(length(length(tr_timing_e) < T)){
        tr_timing_e <- c(tr_timing_e, rep(0, T - length(tr_timing_e)))
      }
      if(length(length(tr_timing_l) < T)){
        tr_timing_l <- c(tr_timing_l, rep(0, T - length(tr_timing_l)))
      }
      
      # Weight by occurence
      watt <- weighted.mean(c(b_e_f, b_l_f), w = c(tr_timing_e, tr_timing_l))
    }else{
      
      # Fill zeros
      b_f <- c(beta.timevarying, rep(0, T - natt))
      if(length(length(tr_timing) < T)){
        tr_timing <- c(tr_timing, rep(0, T - length(tr_timing)))
      }
      
      # Weight by occurence
      watt <- weighted.mean(b_f, w = tr_timing)
    }
    
    
    ### ATT as unweighed average of all g,t-specific ATT
    if(flag_late == 1){
      b_e_f <- c(b_e, rep(0, T - natt))
      b_l_f <- c(b_l, rep(0, T - natt))
      att.av <- mean(c(b_e_f, b_l_f))
    }else{
      b_f <- c(beta.timevarying, rep(0, T - natt))
      att.av <- mean(b_f)
    }
    
    
    
    
    
    ### Print progress
    if(i == 1){
      cat(paste("Simulations completed (by 10):"," "))
    } 
    if(i %% 50 == 0){
      cat(paste(" ", i," "))
    }else if(i %% 10 == 0){
      cat(paste("+"))
    }
    if(i == R){
      cat("\n")
    }
    table(df$Treat_first)
    
    
    ### Export results
    res <- list(
      estimate.df = res.df,
      tr_timing.df = table(factor(df$Treat_first, levels = c(0:T))),
      sum.df = sum.df,
      att.av = att.av,
      itt.av = df_att,
      vwtt.av = watt,
      itt = df_att_time
    )
    
    return(res)
    
  }
  
  ### Stop Cluster
  
  stopCluster(cl)
  
  ### Reshape results
  
  if(beta.late == TRUE){
    beta.timevarying_e <- 2 * (beta.t * beta) / (beta.late + 1)
    beta.timevarying_l <- beta.late * beta.timevarying_e
  }else{
    beta.timevarying_e <- (beta.t * beta) 
    beta.timevarying_l <- (beta.t * beta) 
  }
  
  att <- (1 - size.late) * beta.timevarying_e + 
    size.late * beta.timevarying_l
  
  res <- comb2(res)
  estimate.df <- res[[1]]
  estimate.df <- cbind(R = rep(1:R, each = T*2 + 1),
                       estimate.df)
  
  tr_timing.df <- res[[2]]
  sum.df <- res[[3]]
  att.av <- res[[4]]
  itt.av <- res[[5]]
  vwtt.av <- res[[6]]
  itt <- res[[7]]
  
  ### Create output element
  
  result <- list(
    parameters    = parameters,
    estimate.df   = estimate.df,
    tr_timing     = tr_timing.df,
    att = att,
    att.av = att.av,
    itt.av = itt.av,
    itt = itt,
    vwtt.av = vwtt.av,
    sum.df = sum.df
  )
  
  class(result) <- c("sim_did")
  
  return(result)
  
}







#######################
#### Plot function ####
#######################

sim_plot <- function(object = NULL,
                     xlim = c(-5, 8),
                     ylim = c(NA, NA),
                     sd.factor = 1, 
                     rect_width = 0.01,
                     ...){
  parameters <- object$parameters
  estimate.df <- object$estimate.df
  beta <- parameters[["beta"]]
  beta.t <- parameters[["beta.t"]]
  anticipaton.t <- parameters[["anticipaton.t"]]
  att <- object$att
  
  # Mean and sd of simulation values
  ave.df <- aggregate(estimate.df[, 3:ncol(estimate.df)],
                      by = list(time = estimate.df$time),
                      FUN = function(x) mean(x, na.rm = TRUE))
  
  sd.df <- aggregate(estimate.df[, 3:ncol(estimate.df)],
                     by = list(time = estimate.df$time),
                     FUN = function(x) sd(x, na.rm = TRUE))
  
  ave.df <- ave.df[which(ave.df$time %in% c(xlim[1]:xlim[2])), ]
  sd.df <- sd.df[which(sd.df$time %in% c(xlim[1]:xlim[2])), ]
  
  # True coefficients
  coef <- data.frame(time = c(xlim[1]:xlim[2]),
                est = 0)
  coef$est[which(coef$time %in% c(1:length(beta.t)))] <- att[1:length(which(coef$time %in% c(1:length(beta.t))))]
  coef$est[which(coef$time %in% c(-3:0))] <- anticipaton.t
  
  coef$xmin <- coef$time - 0.45
  coef$xmax <- coef$time + 0.45
  coef$ymin <- coef$est - rect_width
  coef$ymax <- coef$est + rect_width
  
  # Reshape the effects
  ave.df <- tidyr::pivot_longer(ave.df,
                                cols = starts_with("est"),
                                names_prefix = "est.",
                                values_to = "estimate")
  sd.df <- tidyr::pivot_longer(sd.df,
                               cols = starts_with("est"),
                               names_prefix = "est.",
                               values_to = "sd")
  
  mod_es <- merge(ave.df, sd.df, by = c("time", "name"))
  
  # Name models
  mod_es$name <- factor(mod_es$name, levels = c("real",  
                                                "ols",   
                                                "twfe",
                                                "feis",
                                                "ife",   
                                                "mc",    
                                                "did",   
                                                "sa",    
                                                "bo",    
                                                "did2"),
                        labels = c("real",  
                                   "OLS",   
                                   "Conventional TWFE",  
                                   "FEIS",
                                   "Interactive FE",   
                                   "Matrix Completion",    
                                   "Callaway SantAnna DiD",   
                                   "Sun Abraham",    
                                   "Borusyak et al",    
                                   "Callaway SantAnna NYT"))
  
  
  # Subset
  mod_es <- mod_es[which(mod_es$name != "OLS"), ]
  mod_es <- mod_es[which(mod_es$name != "real"), ]
  
  # Uncertainty estimates +- 1 sd
  mod_es$conf.low <- mod_es$estimate - sd.factor * mod_es$sd
  mod_es$conf.high <- mod_es$estimate + sd.factor * mod_es$sd
  
  # Merge real
  mod_es <- merge(mod_es, coef, by = "time", all.x = TRUE)
  
  
  # Plot
  zp <- ggplot(mod_es, aes(x = time, y = estimate)) +
    facet_wrap(vars(name), ncol = 5) + 
    geom_rect(mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = "deeppink1", color = "deeppink1", alpha = 0.2) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0.5, linetype = "dashed") +
    geom_pointrange(data = mod_es, aes(x = time, y = estimate,
                                       ymin = conf.low, ymax = conf.high,
                        color = name), 
                    #col = "darkcyan"
                      ) +
    scale_color_viridis_d(option = "G", end = 0.80, begin = 0.2, guide = "none") +
    theme_minimal() + theme(panel.grid.minor = element_blank(),
                            text = element_text(family = "Times New Roman", size = 18),
                            axis.text = element_text(colour = "black")) +
    labs(x = "Event time (Treatment = 1)", 
         y = paste0("Estimated Effect on Y ", "+- ", sd.factor, " SD")) +
    coord_cartesian(ylim = ylim)
  zp
  
  return(zp)
  
}









###################################
#### Plot function average ATT ####
###################################


sim_plot_att <- function(object = NULL,
                     xlim = NULL,
                     sd.factor = 1, 
                     use.rmse = TRUE,
                     use.ci = TRUE,
                     violin = TRUE,
                     ...){
  parameters <- object$parameters
  estimate.df <- object$sum.df
  beta <- parameters[["beta"]]
  beta.t <- parameters[["beta.t"]]
  
  # ATTs: in the following use itt/vwtt (empirically weighted g,t-specific ATT)
  att <- object$itt.av
  vwtt <- object$vwtt.av
  av.att <- mean(att)
  av.vwtt <- mean(vwtt)
  # diff_vwtt_att <- av.vwtt - av.att
  
  
  # Difference using ATT weighted by empirical occurence
  N_e <- length(unique(estimate.df$estimator))
  estimate.df$diff <- estimate.df$att1 - rep(att, each = N_e)
  
  
  # Mean and sd of simulation values
  ave.df <- aggregate(estimate.df[, 2:ncol(estimate.df)],
                      by = list(estimator = estimate.df$estimator),
                      FUN = function(x) mean(x, na.rm = TRUE))
  names(ave.df)[-1] <- paste0("est_", names(ave.df)[-1])
  
  sd.df <- aggregate(estimate.df[, 2:ncol(estimate.df)],
                     by = list(estimator = estimate.df$estimator),
                     FUN = function(x) sd(x, na.rm = TRUE))
  names(sd.df)[-1] <- paste0("sd_", names(sd.df)[-1])
  
  rmse.df <- aggregate(estimate.df[, "diff"],
                       by = list(estimator = estimate.df$estimator),
                       FUN = function(x) sqrt(mean(x ^ 2)))
  names(rmse.df)[-1] <- paste0("rmse_", names(rmse.df)[-1])
  
  # distribution
  dist.df <- estimate.df
  # dist.df$rse <- rse(dist.df$att1, true = av.vwtt)
  dist.df$rse <- sqrt(dist.df$diff ^ 2)

  # Merge
  mod_es <- merge(ave.df, sd.df, by = "estimator")
  mod_es <- merge(mod_es, rmse.df, by = "estimator")
  
  
  # Name models
  mod_es$name <- factor(
    mod_es$estimator,
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
      "Callaway SantAnna DiD",
      "Sun Abraham",
      "Borusyak et al",
      "Wooldridge ETWFE",
      "Callaway SantAnna NYT"
    ))
  )
  
  dist.df$name <- factor(
    dist.df$estimator,
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
      "Callaway SantAnna DiD",
      "Sun Abraham",
      "Borusyak et al",
      "Wooldridge ETWFE",
      "Callaway SantAnna NYT"
    ))
  )
  
  # rect
  coef <- mod_es
  ln <- which(names(coef) == "name")
  for(i in 2:(ln-1)){
    coef[, i] <- coef[which(coef$name == "real"), i]
  }
  
  # Subset
  omit <- c("OLS", "real")
  mod_es <- mod_es[which(!mod_es$name %in% omit), ]
  coef <- coef[which(!coef$name %in% omit), ]
  dist.df <- dist.df[which(!dist.df$name %in% omit), ]
  
  coef$xmin <- as.numeric(coef$name) - 0.35
  coef$xmax <- as.numeric(coef$name) + 0.35
  
  # Uncertainty estimates +- 1 sd
  if(use.rmse == TRUE){
    mod_es$estimate <- mod_es$rmse_x
    
    coef$estimate <- coef$rmse_x
    dist.df$dispersion <- dist.df$rse
  }else{
    mod_es$estimate <- (mod_es$est_diff) / 1
    
    coef$estimate <- (coef$est_diff) / 1
    dist.df$dispersion <- dist.df$diff
  }
  
  if(use.ci == TRUE){
    interval  <-  -qnorm((1-0.95)/2)  # 95% multiplier
    mod_es$conf.low <- mod_es$estimate - interval * mod_es$sd_att1
    mod_es$conf.high <- mod_es$estimate + interval * mod_es$sd_att1
    
    coef$ymin <- coef$estimate - interval * coef$sd_att1
    coef$ymax <- coef$estimate + interval * coef$sd_att1
  }else{
    mod_es$conf.low <- mod_es$estimate - sd.factor * mod_es$sd_att1
    mod_es$conf.high <- mod_es$estimate + sd.factor * mod_es$sd_att1
    
    coef$ymin <- coef$estimate - sd.factor * coef$sd_att1
    coef$ymax <- coef$estimate + sd.factor * coef$sd_att1
  }

  
  
  # Plot
  zp <- ggplot(mod_es, aes(x = name, y = estimate)) +
    geom_rect(data = coef, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = "deeppink1", color = "deeppink1", alpha = 0.2)
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
  # if(include.vwtt == TRUE){
  #   zp <- zp + geom_hline(yintercept = diff_vwtt_att, linetype = "dotdash", color = "grey40", lwd = 1)
  # }
  zp <- zp + geom_hline(yintercept = 0, linetype = "dashed", color = "deeppink4", lwd = 1.1) +
    geom_vline(xintercept = 0.5, linetype = 1) +
    geom_pointrange(data = mod_es, aes(x = name, y = estimate,
                                       ymin = conf.low, ymax = conf.high,
                                       color = name, fill = name), 
                    shape = 21, size = 1, lwd = 1.05
    ) +
    coord_flip() +
    scale_x_discrete()+
    scale_color_viridis_d(option = "G", end = 0.80, begin = 0.2, guide = "none") +
    scale_fill_viridis_d(option = "B", end = 0.80, begin = 0.2, guide = "none", direction = -1) +
    theme_minimal() + theme( # panel.grid.minor = element_blank(),
                            text = element_text(family = "Times New Roman", size = 18),
                            axis.text = element_text(colour = "black")) +
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
  
  return(zp)
  
}










############# ---------- Combine some estimators -------------- ############# 




#######################
#### Plot function ####
#######################

sim_plot2 <- function(object = NULL,
                     xlim = c(-5, 8),
                     ylim = c(NA, NA),
                     sd.factor = 2, 
                     rect_width = 0.01,
                     models = c("Conventional TWFE",  
                                "FEIS",
                                "Matrix Completion",    
                                "Callaway SantAnna",   
                                "Sun Abraham",    
                                "Borusyak et al",   
                                "Wooldridge ETWFE",
                                "Callaway SantAnna NYT"),
                     ...){
  parameters <- object$parameters
  estimate.df <- object$estimate.df
  beta <- parameters[["beta"]]
  beta.t <- parameters[["beta.t"]]
  anticipaton.t <- parameters[["anticipaton.t"]]
  att <- object$att
  
  # Ave itt
  itt <- object$itt
  itt <- aggregate(itt$x,
                   list(itt$Treat_count),
                   mean)[, 2]
  
  # Mean and sd of simulation values
  ave.df <- aggregate(estimate.df[, 3:ncol(estimate.df)],
                      by = list(time = estimate.df$time),
                      FUN = function(x) mean(x, na.rm = TRUE))
  
  sd.df <- aggregate(estimate.df[, 3:ncol(estimate.df)],
                     by = list(time = estimate.df$time),
                     FUN = function(x) sd(x, na.rm = TRUE))
  
  ave.df <- ave.df[which(ave.df$time %in% c(xlim[1]:xlim[2])), ]
  sd.df <- sd.df[which(sd.df$time %in% c(xlim[1]:xlim[2])), ]
  
  # True coefficients
  coef <- data.frame(time = c(xlim[1]:xlim[2]),
                     est = 0)
  coef$est[which(coef$time %in% c(1:length(beta.t)))] <- itt[1:length(which(coef$time %in% c(1:length(beta.t))))]
  coef$est[which(coef$time %in% c(-3:0))] <- anticipaton.t
  
  coef$xmin <- coef$time - 0.45
  coef$xmax <- coef$time + 0.45
  coef$ymin <- coef$est - rect_width
  coef$ymax <- coef$est + rect_width
  
  # Reshape the effects
  ave.df <- tidyr::pivot_longer(ave.df,
                                cols = starts_with("est"),
                                names_prefix = "est.",
                                values_to = "estimate")
  sd.df <- tidyr::pivot_longer(sd.df,
                               cols = starts_with("est"),
                               names_prefix = "est.",
                               values_to = "sd")
  
  mod_es <- merge(ave.df, sd.df, by = c("time", "name"))
  
  # Name models
  mod_es$name <- factor(mod_es$name, levels = c("real",  
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
  mod_es <- mod_es[which(mod_es$name %in% models), ]
  
  # Uncertainty estimates +- 1 sd
  mod_es$conf.low <- mod_es$estimate - sd.factor * mod_es$sd
  mod_es$conf.high <- mod_es$estimate + sd.factor * mod_es$sd
  
  # Merge real
  mod_es <- merge(mod_es, coef, by = "time", all.x = TRUE)
  
  
  # Plot
  zp <- ggplot(mod_es, aes(x = time, y = estimate)) +
    facet_wrap(vars(name), ncol = max(1, round(length(models)/2))) + 
    geom_rect(mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = "deeppink1", color = "deeppink1", alpha = 0.2) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0.5, linetype = "dashed") +
    geom_pointrange(data = mod_es, aes(x = time, y = estimate,
                                       ymin = conf.low, ymax = conf.high,
                                       color = name), 
                    #col = "darkcyan"
    ) +
    scale_color_viridis_d(option = "G", end = 0.80, begin = 0.2, guide = "none") +
    theme_minimal() + theme(panel.grid.minor = element_blank(),
                            text = element_text(family = "Times New Roman", size = 18),
                            axis.text = element_text(colour = "black")) +
    labs(x = "Event time (Treatment = 1)", 
         y = paste0("Estimated Effect on Y ", "+- ", sd.factor, " SD")) +
    coord_cartesian(ylim = ylim)
  zp
  
  return(zp)
  
}









###################################
#### Plot function average ATT ####
###################################


sim_plot_att2 <- function(object = NULL,
                          xlim = NULL,
                          sd.factor = 2, 
                          use.rmse = TRUE,
                          use.ci = TRUE,
                          violin = TRUE,
                          relativ = FALSE,
                          models = c("Conventional TWFE",  
                                     "FEIS",
                                     "Matrix Completion",    
                                     "Callaway SantAnna",   
                                     "Sun Abraham",    
                                     "Borusyak et al",   
                                     "Wooldridge ETWFE",
                                     "Callaway SantAnna NYT"),
                          ...){
  parameters <- object$parameters
  estimate.df <- object$sum.df
  beta <- parameters[["beta"]]
  beta.t <- parameters[["beta.t"]]
  
  # ATTs: in the following use itt/vwtt (empirically weighted g,t-specific ATT)
  att <- object$itt.av
  vwtt <- object$vwtt.av
  av.att <- mean(att)
  av.vwtt <- mean(vwtt)
  # diff_vwtt_att <- av.vwtt - av.att
  
  
  # Difference using ATT weighted by empirical occurence
  N_e <- length(unique(estimate.df$estimator))
  estimate.df$diff <- estimate.df$att1 - rep(att, each = N_e)
  
  if(relativ == TRUE){
    estimate.df$diff <- estimate.df$diff / av.att
  }
  
  # Mean and sd of simulation values
  ave.df <- aggregate(estimate.df[, 2:ncol(estimate.df)],
                      by = list(estimator = estimate.df$estimator),
                      FUN = function(x) mean(x, na.rm = TRUE))
  names(ave.df)[-1] <- paste0("est_", names(ave.df)[-1])
  
  sd.df <- aggregate(estimate.df[, 2:ncol(estimate.df)],
                     by = list(estimator = estimate.df$estimator),
                     FUN = function(x) sd(x, na.rm = TRUE))
  names(sd.df)[-1] <- paste0("sd_", names(sd.df)[-1])
  
  rmse.df <- aggregate(estimate.df[, "diff"],
                       by = list(estimator = estimate.df$estimator),
                       FUN = function(x) sqrt(mean(x ^ 2)))
  names(rmse.df)[-1] <- paste0("rmse_", names(rmse.df)[-1])
  
  # distribution
  dist.df <- estimate.df
  # dist.df$rse <- rse(dist.df$att1, true = av.vwtt)
  dist.df$rse <- sqrt(dist.df$diff ^ 2)
  
  # Merge
  mod_es <- merge(ave.df, sd.df, by = "estimator")
  mod_es <- merge(mod_es, rmse.df, by = "estimator")
  
  
  # Name models
  mod_es$name <- factor(
    mod_es$estimator,
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
  
  dist.df$name <- factor(
    dist.df$estimator,
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
  
  # # rect
  # coef <- mod_es
  # ln <- which(names(coef) == "name")
  # for(i in 2:(ln-1)){
  #   coef[, i] <- coef[which(coef$name == "real"), i]
  # }
  
  # Subset
  omit <- c("OLS", "real", "Interactive FE")
  mod_es <- mod_es[which(mod_es$name %in% models), ]
  # coef <- coef[which(coef$name %in% models), ]
  dist.df <- dist.df[which(dist.df$name %in% models), ]
  
  mod_es$name <- droplevels(mod_es$name)
  # coef$name <- droplevels(coef$name)
  dist.df$name <- droplevels(dist.df$name)
  
  # coef$xmin <- as.numeric(coef$name) - 0.35
  # coef$xmax <- as.numeric(coef$name) + 0.35
  
  # Uncertainty estimates +- 1 sd
  if(use.rmse == TRUE){
    mod_es$estimate <- mod_es$rmse_x
    
    # coef$estimate <- coef$rmse_x
    dist.df$dispersion <- dist.df$rse
  }else{
    mod_es$estimate <- (mod_es$est_diff) / 1
    
    # coef$estimate <- (coef$est_diff) / 1
    dist.df$dispersion <- dist.df$diff
  }
  
  if(use.ci == TRUE){
    interval  <-  -qnorm((1-0.95)/2)  # 95% multiplier
    mod_es$conf.low <- mod_es$estimate - interval * mod_es$sd_att1
    mod_es$conf.high <- mod_es$estimate + interval * mod_es$sd_att1
    
    # coef$ymin <- coef$estimate - interval * coef$sd_att1
    # coef$ymax <- coef$estimate + interval * coef$sd_att1
  }else{
    mod_es$conf.low <- mod_es$estimate - sd.factor * mod_es$sd_att1
    mod_es$conf.high <- mod_es$estimate + sd.factor * mod_es$sd_att1
    
    # coef$ymin <- coef$estimate - sd.factor * coef$sd_att1
    # coef$ymax <- coef$estimate + sd.factor * coef$sd_att1
  }
  
  
  
  # Plot
  zp <- ggplot(mod_es, aes(x = name, y = estimate)) 
  # zp <- zp + geom_rect(data = coef, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
  #           fill = "deeppink1", color = "deeppink1", alpha = 0.2)
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
  # if(include.vwtt == TRUE){
  #   zp <- zp + geom_hline(yintercept = diff_vwtt_att, linetype = "dotdash", color = "grey40", lwd = 1)
  # }
  zp <- zp + geom_hline(yintercept = 0, linetype = "dashed", color = "deeppink1", lwd = 1.1) +
    geom_vline(xintercept = 0.5, linetype = 1) +
    geom_pointrange(data = mod_es, aes(x = name, y = estimate,
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
  
  return(zp)
  
}



