################################################################################## #
## Power-Analyses                                                               ####
################################################################################## #

library(simsem)
library(simr)
library(pwr)
library(haven)
library(tidyverse)
library(lavaan)
library(effsize)


## Studie 1.1 (Konstruktvalidierung) ##################################################################

  # └ estimations on BilWiss data ####
    
    ## how high can we expect the loadings to be?
      # because we do not have pilot data, we get approximate estimates
      # from BilWiss data and its scale on beliefs of theory-practice relation
      BilWiss <- read_sav("data/BilWiss_Laengsschnitt_MZP1234_SUF_1809-24a.sav") #we can't share this data as is is scientific usefile from another study
      
      # we use the second measurement point because there is data available from both cohorts
      cfa_loadings_model <- "lv =~ tp01_3 + tp02r_3 + tp03_3 + tp04_3 + tp05_3 + tp06_3"
      
      cfa_loadings <- cfa(model = cfa_loadings_model, data = BilWiss, std.lv = TRUE)
      
      # estimating the score of loadings based on mean loading size
      # result is 0.654
      mean(standardizedSolution(cfa_loadings)$est.std[1],
           standardizedSolution(cfa_loadings)$est.std[2], 
           standardizedSolution(cfa_loadings)$est.std[3], 
           standardizedSolution(cfa_loadings)$est.std[4], 
           standardizedSolution(cfa_loadings)$est.std[5], 
           standardizedSolution(cfa_loadings)$est.std[6]
           ) 
      
      # estimating residual variance based on mean variance
      # result is 0.573
      mean(standardizedSolution(cfa_loadings)$est.std[7],
           standardizedSolution(cfa_loadings)$est.std[8], 
           standardizedSolution(cfa_loadings)$est.std[9], 
           standardizedSolution(cfa_loadings)$est.std[10], 
           standardizedSolution(cfa_loadings)$est.std[11], 
           standardizedSolution(cfa_loadings)$est.std[12]
           )
      
      
    ## how high can we expect the change to be over time (effect of time/ semester)
      # because we do not have pilot data, we get approximate estimates
      # from BilWiss data and its scale on beliefs of theory-practice relation
      # the only feasible comparison is cohort 1 and their change from T2 (April) to T3 (December)
      
      BilWiss_d <- BilWiss %>%
        mutate(tp2 = rowMeans(.[c("tp01_2", "tp02r_2", "tp03_2", "tp04_2", "tp05_2", "tp06_2")], na.rm = T),
               tp3 = rowMeans(.[c("tp01_3", "tp02r_3", "tp03_3", "tp04_3", "tp05_3", "tp06_3")], na.rm = T)) %>%
        filter(kohorte == 1 & !is.na(tp2) & !is.na(tp3))
      
      # computing efect size for paired t-Test with pooled SDs
        cohen.d(BilWiss_d$tp2, BilWiss_d$tp3, pooled = T, paired = T)   # result is Cohen's d= 0.245295
        
        # Conversion according to Rosenthal, R. (1994). Parametric measures of effect size. In H. Cooper & L. V. Hedges (Eds.), The Handbook of Research Synthesis. New York, NY: Sage. pp. 239.
        alpha <- (243 * 2)^2 / (243 * 243)
        0.245295 / sqrt(0.245295^2 + alpha)  # result is r= 0.1217353
    
    
## simulation of the cfa
  # └ CFA model 1 ####
  # with
    #   - latent factors: 7 (one for each model)
    #   - 2nd oder factors: NO

    popModel_7F <- "
          # LATENT VARIABLES
              f1 =~ 0.654*i1_1 + 0.654*i1_2 + 0.654*i1_3 + 0.654*i1_4 + 0.654*i1_5
              f2 =~ 0.654*i2_1 + 0.654*i2_2 + 0.654*i2_3 + 0.654*i2_4 + 0.654*i2_5
              f3 =~ 0.654*i3_1 + 0.654*i3_2 + 0.654*i3_3 + 0.654*i3_4 + 0.654*i3_5
              f4 =~ 0.654*i4_1 + 0.654*i4_2 + 0.654*i4_3 + 0.654*i4_4 + 0.654*i4_5
              f5 =~ 0.654*i5_1 + 0.654*i5_2 + 0.654*i5_3 + 0.654*i5_4 + 0.654*i5_5
              f6 =~ 0.654*i6_1 + 0.654*i6_2 + 0.654*i6_3 + 0.654*i6_4 + 0.654*i6_5
              f7 =~ 0.654*i7_1 + 0.654*i7_2 + 0.654*i7_3 + 0.654*i7_4 + 0.654*i7_5

          # VARIANCES
              f1 ~~ 1*f1
              f2 ~~ 1*f2
              f3 ~~ 1*f3
              f4 ~~ 1*f4
              f5 ~~ 1*f5
              f6 ~~ 1*f6
              f7 ~~ 1*f7

              i1_1 ~~ 0.573*i1_1
              i1_2 ~~ 0.573*i1_2
              i1_3 ~~ 0.573*i1_3
              i1_4 ~~ 0.573*i1_4
              i1_5 ~~ 0.573*i1_5

              i2_1 ~~ 0.573*i2_1
              i2_2 ~~ 0.573*i2_2
              i2_3 ~~ 0.573*i2_3
              i2_4 ~~ 0.573*i2_4
              i2_5 ~~ 0.573*i2_5

              i3_1 ~~ 0.573*i3_1
              i3_2 ~~ 0.573*i3_2
              i3_3 ~~ 0.573*i3_3
              i3_4 ~~ 0.573*i3_4
              i3_5 ~~ 0.573*i3_5

              i4_1 ~~ 0.573*i4_1
              i4_2 ~~ 0.573*i4_2
              i4_3 ~~ 0.573*i4_3
              i4_4 ~~ 0.573*i4_4
              i4_5 ~~ 0.573*i4_5

              i5_1 ~~ 0.573*i5_1
              i5_2 ~~ 0.573*i5_2
              i5_3 ~~ 0.573*i5_3
              i5_4 ~~ 0.573*i5_4
              i5_5 ~~ 0.573*i5_5

              i6_1 ~~ 0.573*i6_1
              i6_2 ~~ 0.573*i6_2
              i6_3 ~~ 0.573*i6_3
              i6_4 ~~ 0.573*i6_4
              i6_5 ~~ 0.573*i6_5

              i7_1 ~~ 0.573*i7_1
              i7_2 ~~ 0.573*i7_2
              i7_3 ~~ 0.573*i7_3
              i7_4 ~~ 0.573*i7_4
              i7_5 ~~ 0.573*i7_5

          # FACTOR CORRELATIONS
              f1 ~~ 0.6*f2;
              f1 ~~ 0.4*f3;
              f1 ~~ 0.2*f4;
              f1 ~~ 0.0*f5;
              f1 ~~ -0.2*f6;
              f1 ~~ -0.4*f7;
              
              f2 ~~ 0.6*f3;
              f2 ~~ 0.4*f4;
              f2 ~~ 0.2*f5;
              f2 ~~ 0*f6;
              f2 ~~ -0.2*f7;
              
              f3 ~~ 0.6*f4;
              f3 ~~ 0.4*f5;
              f3 ~~ 0.2*f6;
              f3 ~~ 0*f7;
              
              f4 ~~ 0.6*f5;
              f4 ~~ 0.4*f6;
              f4 ~~ 0.2*f7;
              
              f5 ~~ 0.6*f6;
              f5 ~~ 0.4*f7;
              
              f6 ~~ 0.6*f7;
    "
    
    analyzeModel_7F <- "
          # LATENT VARIABLES
              f1 =~ i1_1 + i1_2 + i1_3 + i1_4 + i1_5
              f2 =~ i2_1 + i2_2 + i2_3 + i2_4 + i2_5
              f3 =~ i3_1 + i3_2 + i3_3 + i3_4 + i3_5
              f4 =~ i4_1 + i4_2 + i4_3 + i4_4 + i4_5
              f5 =~ i5_1 + i5_2 + i5_3 + i5_4 + i5_5
              f6 =~ i6_1 + i6_2 + i6_3 + i6_4 + i6_5
              f7 =~ i7_1 + i7_2 + i7_3 + i7_4 + i7_5
    "
    
    Output_7F <- sim(nRep = 500, 
                       model = analyzeModel_7F, 
                       n = 940, 
                       generate = popModel_7F, 
                       lavaanfun = "cfa", 
                       std.lv = T, 
                       seed = 123)
    
    summary(Output_7F)
    getCutoff(Output_7F, 0.05)
    plotCutoff(Output_7F, 0.05)


  # └ CFA model 2 ####
  # with
    #   - latent factors: 7 (one for each model)
    #   - 2nd oder factors: YES

    popModel_7F_2nd <- "
          # LATENT VARIABLES
              f1 =~ 0.654*i1_1 + 0.654*i1_2 + 0.654*i1_3 + 0.654*i1_4 + 0.654*i1_5
              f2 =~ 0.654*i2_1 + 0.654*i2_2 + 0.654*i2_3 + 0.654*i2_4 + 0.654*i2_5
              f3 =~ 0.654*i3_1 + 0.654*i3_2 + 0.654*i3_3 + 0.654*i3_4 + 0.654*i3_5
              f4 =~ 0.654*i4_1 + 0.654*i4_2 + 0.654*i4_3 + 0.654*i4_4 + 0.654*i4_5
              f5 =~ 0.654*i5_1 + 0.654*i5_2 + 0.654*i5_3 + 0.654*i5_4 + 0.654*i5_5
              f6 =~ 0.654*i6_1 + 0.654*i6_2 + 0.654*i6_3 + 0.654*i6_4 + 0.654*i6_5
              f7 =~ 0.654*i7_1 + 0.654*i7_2 + 0.654*i7_3 + 0.654*i7_4 + 0.654*i7_5
              
              g1 =~ 0.654*f1 + 0.654*f2
              g2 =~ 0.654*f3 + 0.654*f4 + 0.654*f5 + 0.654*f6

          # VARIANCES
              f1 ~~ 1*f1
              f2 ~~ 1*f2
              f3 ~~ 1*f3
              f4 ~~ 1*f4
              f5 ~~ 1*f5
              f6 ~~ 1*f6
              f7 ~~ 1*f7
              g1 ~~ 1*g1
              g2 ~~ 1*g2

              i1_1 ~~ 0.573*i1_1
              i1_2 ~~ 0.573*i1_2
              i1_3 ~~ 0.573*i1_3
              i1_4 ~~ 0.573*i1_4
              i1_5 ~~ 0.573*i1_5

              i2_1 ~~ 0.573*i2_1
              i2_2 ~~ 0.573*i2_2
              i2_3 ~~ 0.573*i2_3
              i2_4 ~~ 0.573*i2_4
              i2_5 ~~ 0.573*i2_5

              i3_1 ~~ 0.573*i3_1
              i3_2 ~~ 0.573*i3_2
              i3_3 ~~ 0.573*i3_3
              i3_4 ~~ 0.573*i3_4
              i3_5 ~~ 0.573*i3_5

              i4_1 ~~ 0.573*i4_1
              i4_2 ~~ 0.573*i4_2
              i4_3 ~~ 0.573*i4_3
              i4_4 ~~ 0.573*i4_4
              i4_5 ~~ 0.573*i4_5

              i5_1 ~~ 0.573*i5_1
              i5_2 ~~ 0.573*i5_2
              i5_3 ~~ 0.573*i5_3
              i5_4 ~~ 0.573*i5_4
              i5_5 ~~ 0.573*i5_5

              i6_1 ~~ 0.573*i6_1
              i6_2 ~~ 0.573*i6_2
              i6_3 ~~ 0.573*i6_3
              i6_4 ~~ 0.573*i6_4
              i6_5 ~~ 0.573*i6_5

              i7_1 ~~ 0.573*i7_1
              i7_2 ~~ 0.573*i7_2
              i7_3 ~~ 0.573*i7_3
              i7_4 ~~ 0.573*i7_4
              i7_5 ~~ 0.573*i7_5

          # FACTOR CORRELATIONS
              g1 ~~ 0.2*g2
              g1 ~~ -0.2*f7
              g2 ~~ 0*f7
    "
    
    analyzeModel_7F_2nd <- "
          # LATENT VARIABLES
              f1 =~ i1_1 + i1_2 + i1_3 + i1_4 + i1_5
              f2 =~ i2_1 + i2_2 + i2_3 + i2_4 + i2_5
              f3 =~ i3_1 + i3_2 + i3_3 + i3_4 + i3_5
              f4 =~ i4_1 + i4_2 + i4_3 + i4_4 + i4_5
              f5 =~ i5_1 + i5_2 + i5_3 + i5_4 + i5_5
              f6 =~ i6_1 + i6_2 + i6_3 + i6_4 + i6_5
              f7 =~ i7_1 + i7_2 + i7_3 + i7_4 + i7_5
              
          # 2nd ORDER VARIABLES
              g1 =~ f1 + f2
              g2 =~ f3 + f4 + f5 + f6
    "
    
    Output_7F_2nd <- sim(nRep = 500, 
                       model = analyzeModel_7F_2nd, 
                       n = 940, 
                       generate = popModel_7F_2nd, 
                       lavaanfun = "cfa", 
                       std.lv = T, 
                       seed = 123)
    
    summary(Output_7F_2nd)
    getCutoff(Output_7F_2nd, 0.05)
    plotCutoff(Output_7F_2nd, 0.05)


    

## Studie 1.2 (Überzeugungen zw. Fachsemestern (1)) ##################################################################
  # Querschnitt SoSe 2020
    
  # └ SEM model 1 ####
    # with
    #   - latent factors: 7 (one for each model)
    #   - 2nd oder factors: NO
    
    popModel_7F_reg <- "
          # LATENT VARIABLES
              f1 =~ 0.654*i1_1 + 0.654*i1_2 + 0.654*i1_3 + 0.654*i1_4 + 0.654*i1_5
              f2 =~ 0.654*i2_1 + 0.654*i2_2 + 0.654*i2_3 + 0.654*i2_4 + 0.654*i2_5
              f3 =~ 0.654*i3_1 + 0.654*i3_2 + 0.654*i3_3 + 0.654*i3_4 + 0.654*i3_5
              f4 =~ 0.654*i4_1 + 0.654*i4_2 + 0.654*i4_3 + 0.654*i4_4 + 0.654*i4_5
              f5 =~ 0.654*i5_1 + 0.654*i5_2 + 0.654*i5_3 + 0.654*i5_4 + 0.654*i5_5
              f6 =~ 0.654*i6_1 + 0.654*i6_2 + 0.654*i6_3 + 0.654*i6_4 + 0.654*i6_5
              f7 =~ 0.654*i7_1 + 0.654*i7_2 + 0.654*i7_3 + 0.654*i7_4 + 0.654*i7_5

          # VARIANCES
              f1 ~~ 1*f1
              f2 ~~ 1*f2
              f3 ~~ 1*f3
              f4 ~~ 1*f4
              f5 ~~ 1*f5
              f6 ~~ 1*f6
              f7 ~~ 1*f7

              i1_1 ~~ 0.573*i1_1
              i1_2 ~~ 0.573*i1_2
              i1_3 ~~ 0.573*i1_3
              i1_4 ~~ 0.573*i1_4
              i1_5 ~~ 0.573*i1_5

              i2_1 ~~ 0.573*i2_1
              i2_2 ~~ 0.573*i2_2
              i2_3 ~~ 0.573*i2_3
              i2_4 ~~ 0.573*i2_4
              i2_5 ~~ 0.573*i2_5

              i3_1 ~~ 0.573*i3_1
              i3_2 ~~ 0.573*i3_2
              i3_3 ~~ 0.573*i3_3
              i3_4 ~~ 0.573*i3_4
              i3_5 ~~ 0.573*i3_5

              i4_1 ~~ 0.573*i4_1
              i4_2 ~~ 0.573*i4_2
              i4_3 ~~ 0.573*i4_3
              i4_4 ~~ 0.573*i4_4
              i4_5 ~~ 0.573*i4_5

              i5_1 ~~ 0.573*i5_1
              i5_2 ~~ 0.573*i5_2
              i5_3 ~~ 0.573*i5_3
              i5_4 ~~ 0.573*i5_4
              i5_5 ~~ 0.573*i5_5

              i6_1 ~~ 0.573*i6_1
              i6_2 ~~ 0.573*i6_2
              i6_3 ~~ 0.573*i6_3
              i6_4 ~~ 0.573*i6_4
              i6_5 ~~ 0.573*i6_5

              i7_1 ~~ 0.573*i7_1
              i7_2 ~~ 0.573*i7_2
              i7_3 ~~ 0.573*i7_3
              i7_4 ~~ 0.573*i7_4
              i7_5 ~~ 0.573*i7_5

          # FACTOR CORRELATIONS
              f1 ~~ 0.6*f2;
              f1 ~~ 0.4*f3;
              f1 ~~ 0.2*f4;
              f1 ~~ 0.0*f5;
              f1 ~~ -0.2*f6;
              f1 ~~ -0.4*f7;
              
              f2 ~~ 0.6*f3;
              f2 ~~ 0.4*f4;
              f2 ~~ 0.2*f5;
              f2 ~~ 0*f6;
              f2 ~~ -0.2*f7;
              
              f3 ~~ 0.6*f4;
              f3 ~~ 0.4*f5;
              f3 ~~ 0.2*f6;
              f3 ~~ 0*f7;
              
              f4 ~~ 0.6*f5;
              f4 ~~ 0.4*f6;
              f4 ~~ 0.2*f7;
              
              f5 ~~ 0.6*f6;
              f5 ~~ 0.4*f7;
              
              f6 ~~ 0.6*f7;
              
          # REGRESSION PATHS
              f1 ~ 0.1217353*semester
              f2 ~ 0.1217353*semester
              f3 ~ 0.1217353*semester
              f4 ~ 0.1217353*semester
              f5 ~ 0.1217353*semester
              f6 ~ 0.1217353*semester
              f7 ~ 0.1217353*semester
    "
    
    analyzeModel_7F_reg <- "
          # LATENT VARIABLES
              f1 =~ i1_1 + i1_2 + i1_3 + i1_4 + i1_5
              f2 =~ i2_1 + i2_2 + i2_3 + i2_4 + i2_5
              f3 =~ i3_1 + i3_2 + i3_3 + i3_4 + i3_5
              f4 =~ i4_1 + i4_2 + i4_3 + i4_4 + i4_5
              f5 =~ i5_1 + i5_2 + i5_3 + i5_4 + i5_5
              f6 =~ i6_1 + i6_2 + i6_3 + i6_4 + i6_5
              f7 =~ i7_1 + i7_2 + i7_3 + i7_4 + i7_5
              
          # REGRESSION PATHS
              f1 ~ semester
              f2 ~ semester
              f3 ~ semester
              f4 ~ semester
              f5 ~ semester
              f6 ~ semester
              f7 ~ semester
    "

    # making a loop to identify sample size for 80% power with effect from Bilwiss
    Output_7F_reg <- data.frame()
    
    for(participants in seq(from = 150, to = 950, by = 20)) {
      
      Output_tmp <- sim(nRep = 500, 
                        model = analyzeModel_7F_reg, 
                        n = participants, 
                        generate = popModel_7F_reg, 
                        lavaanfun = "sem", 
                        std.lv = T, 
                        seed = 123)
      
      Output_7F_reg[(participants-130)/20, "samplesize"] <- participants
      Output_7F_reg[(participants-130)/20, "f1"] <- summaryParam(Output_tmp)[36, 4] # power semester on f1
      Output_7F_reg[(participants-130)/20, "f2"] <- summaryParam(Output_tmp)[37, 4] # power semester on f2
      Output_7F_reg[(participants-130)/20, "f3"] <- summaryParam(Output_tmp)[38, 4] # power semester on f3
      Output_7F_reg[(participants-130)/20, "f4"] <- summaryParam(Output_tmp)[39, 4] # power semester on f4
      Output_7F_reg[(participants-130)/20, "f5"] <- summaryParam(Output_tmp)[40, 4] # power semester on f5
      Output_7F_reg[(participants-130)/20, "f6"] <- summaryParam(Output_tmp)[41, 4] # power semester on f6
      Output_7F_reg[(participants-130)/20, "f7"] <- summaryParam(Output_tmp)[42, 4] # power semester on f7
      Output_7F_reg[(participants-130)/20, "rmsea"] <- getCutoff(Output_tmp, 0.05)[4]
      Output_7F_reg[(participants-130)/20, "cfi"] <- getCutoff(Output_tmp, 0.05)[5]
      Output_7F_reg[(participants-130)/20, "tli"] <- getCutoff(Output_tmp, 0.05)[6]
      Output_7F_reg[(participants-130)/20, "srmr"] <- getCutoff(Output_tmp, 0.05)[7]
    }
    
    View(Output_7F_reg)  


    
    
  # └ SEM model 2 ####
    # with
    #   - latent factors: 7 (one for each model)
    #   - 2nd oder factors: YES
    
    popModel_7F_2nd_reg <- "
          # LATENT VARIABLES
              f1 =~ 0.654*i1_1 + 0.654*i1_2 + 0.654*i1_3 + 0.654*i1_4 + 0.654*i1_5
              f2 =~ 0.654*i2_1 + 0.654*i2_2 + 0.654*i2_3 + 0.654*i2_4 + 0.654*i2_5
              f3 =~ 0.654*i3_1 + 0.654*i3_2 + 0.654*i3_3 + 0.654*i3_4 + 0.654*i3_5
              f4 =~ 0.654*i4_1 + 0.654*i4_2 + 0.654*i4_3 + 0.654*i4_4 + 0.654*i4_5
              f5 =~ 0.654*i5_1 + 0.654*i5_2 + 0.654*i5_3 + 0.654*i5_4 + 0.654*i5_5
              f6 =~ 0.654*i6_1 + 0.654*i6_2 + 0.654*i6_3 + 0.654*i6_4 + 0.654*i6_5
              f7 =~ 0.654*i7_1 + 0.654*i7_2 + 0.654*i7_3 + 0.654*i7_4 + 0.654*i7_5
              
              g1 =~ 0.654*f1 + 0.654*f2
              g2 =~ 0.654*f3 + 0.654*f4 + 0.654*f5 + 0.654*f6

          # VARIANCES
              f1 ~~ 1*f1
              f2 ~~ 1*f2
              f3 ~~ 1*f3
              f4 ~~ 1*f4
              f5 ~~ 1*f5
              f6 ~~ 1*f6
              f7 ~~ 1*f7
              g1 ~~ 1*g1
              g2 ~~ 1*g2

              i1_1 ~~ 0.573*i1_1
              i1_2 ~~ 0.573*i1_2
              i1_3 ~~ 0.573*i1_3
              i1_4 ~~ 0.573*i1_4
              i1_5 ~~ 0.573*i1_5

              i2_1 ~~ 0.573*i2_1
              i2_2 ~~ 0.573*i2_2
              i2_3 ~~ 0.573*i2_3
              i2_4 ~~ 0.573*i2_4
              i2_5 ~~ 0.573*i2_5

              i3_1 ~~ 0.573*i3_1
              i3_2 ~~ 0.573*i3_2
              i3_3 ~~ 0.573*i3_3
              i3_4 ~~ 0.573*i3_4
              i3_5 ~~ 0.573*i3_5

              i4_1 ~~ 0.573*i4_1
              i4_2 ~~ 0.573*i4_2
              i4_3 ~~ 0.573*i4_3
              i4_4 ~~ 0.573*i4_4
              i4_5 ~~ 0.573*i4_5

              i5_1 ~~ 0.573*i5_1
              i5_2 ~~ 0.573*i5_2
              i5_3 ~~ 0.573*i5_3
              i5_4 ~~ 0.573*i5_4
              i5_5 ~~ 0.573*i5_5

              i6_1 ~~ 0.573*i6_1
              i6_2 ~~ 0.573*i6_2
              i6_3 ~~ 0.573*i6_3
              i6_4 ~~ 0.573*i6_4
              i6_5 ~~ 0.573*i6_5

              i7_1 ~~ 0.573*i7_1
              i7_2 ~~ 0.573*i7_2
              i7_3 ~~ 0.573*i7_3
              i7_4 ~~ 0.573*i7_4
              i7_5 ~~ 0.573*i7_5

          # FACTOR CORRELATIONS
              g1 ~~ 0.2*g2
              g1 ~~ -0.2*f7
              g2 ~~ 0*f7
              
          # REGRESSIONS
              g1 ~ 0.1217353*semester
              g2 ~ 0.1217353*semester
              f7 ~ 0.1217353*semester
    "
    
    analyzeModel_7F_2nd_reg <- "
          # LATENT VARIABLES
              f1 =~ i1_1 + i1_2 + i1_3 + i1_4 + i1_5
              f2 =~ i2_1 + i2_2 + i2_3 + i2_4 + i2_5
              f3 =~ i3_1 + i3_2 + i3_3 + i3_4 + i3_5
              f4 =~ i4_1 + i4_2 + i4_3 + i4_4 + i4_5
              f5 =~ i5_1 + i5_2 + i5_3 + i5_4 + i5_5
              f6 =~ i6_1 + i6_2 + i6_3 + i6_4 + i6_5
              f7 =~ i7_1 + i7_2 + i7_3 + i7_4 + i7_5
              
          # 2nd ORDER VARIABLES
              g1 =~ f1 + f2
              g2 =~ f3 + f4 + f5 + f6
              
          # REGRESSIONS
              g1 ~ semester
              g2 ~ semester
              f7 ~ semester
    "
    
    Output_7F_2nd_reg <- sim(nRep = 500, 
                         model = analyzeModel_7F_2nd_reg, 
                         n = 940, 
                         generate = popModel_7F_2nd_reg, 
                         lavaanfun = "sem", 
                         std.lv = T, 
                         seed = 123)
    
    summary(Output_7F_2nd_reg)
    getCutoff(Output_7F_2nd_reg, 0.05)
    plotCutoff(Output_7F_2nd_reg, 0.05)

    
    
    
    
    
    
    


# how many participants do we need for a power of .80?
pwr.anova.test(k = 4,
               # n = ?,
               f = .1226, # derived from BilWiss Cohens d of 0.245295
               sig.level = .05,
               power = .8
)


# at what effect size do we get a power of 80% ?
pwr.anova.test(k = 4,
               n = 235, # 940 subjects evenly distributed over 4 groups
               # f = ?,
               sig.level = .05,
               power = .8
)

# at what effect size do we get a power of 95% ?
pwr.anova.test(k = 4,
               n = 235, # 940 subjects evenly distributed over 4 groups
               # f = .1,
               sig.level = .05,
               power = .95
)

## Studie 1.2 (Überzeugungen zw. Fachsemestern (2)) ##################################################################
  # Querschnitt über alle Jahrgänge, Berücksichtigung der Panelstruktur


  # └ estimations on BilWiss data ####
    # estimating
    #        - random intercept variance
    #        - residual standard deviation
  
    # reshaping data to fit the lmer function
    BilWiss_1.2 <- BilWiss %>%
      mutate(tp_2 = rowMeans(data.frame(tp01_2, tp02r_2, tp03_2, tp04_2, tp05_2, tp06_2), na.rm = T),         # computing scales
             tp_3 = rowMeans(data.frame(tp01_3, tp02r_3, tp03_3, tp04_3, tp05_3, tp06_3), na.rm = T),
             tp_4 = rowMeans(data.frame(tp01_4, tp02r_4, tp03_4, tp04_4, tp05_4, tp06_4), na.rm = T)) %>%
      select(idref, kohorte, tp_2, tp_3, tp_4) %>%                                                            # getting rid of all unused variables
      gather("variable", "value", 3:5) %>%                                                                    # transforming into long dataset
      mutate(mzp = case_when(                                                                                 # introducing variable for measurement time
                          variable == "tp_2" ~ 2,
                          variable == "tp_3" ~ 3,
                          variable == "tp_4" ~ 4
                          ),
             variable = case_when(                                                                            # aligning variable names so that it will be one column after reshaping
                          variable == "tp_2" | variable == "tp_3" | variable == "tp_4" ~ "tp"
                          )
             ) %>%
      spread(key = variable, value = value)                                                                   # wide dataset with tp as one DV
  
    # linear mixed effects model (random intercept)  
    BilWiss_1.2_fit <- lmer(tp ~ mzp + (1 | kohorte), data = BilWiss_1.2)
    
    # random intercept variance (is actually only based on two groups, but it's the best we've got)
      # it is close to zero, so we switch to 0.01 instead of 0.000648
    VarCorr(BilWiss_1.2_fit)[[1]][1]
    
    # residual standard deviation
    attr(VarCorr(BilWiss_1.2_fit), "sc")




  # └ Model 1 [semester as groups] ####
    # there is no pilot data available, simulations have to be built from scratch
    # example taken from https://cran.r-project.org/web/packages/simr/vignettes/fromscratch.html
    
    # parameters
    x1 <- c(rep(0, times = 1050), rep(1, times = 1050), rep(0, times = 1900))   # 1050 in 2nd semester (reference category), 1050 in 4th semester
    x2 <- c(rep(0, times = 2100), rep(1, times = 700), rep(0, times = 1200))    # 700 in 5th bachelor semester
    x3 <- c(rep(0, times = 2800), rep(1, times = 240), rep(0, times = 960))     # 240 in 1st master semester
    x4 <- c(rep(0, times = 3040), rep(1, times = 360), rep(0, times = 600))     # 360 in 2nd master semester
    x5 <- c(rep(0, times = 3400), rep(1, times = 240), rep(0, times = 360))     # 240 in 3rd master semester
    x6 <- c(rep(0, times = 3640), rep(1, times = 360))                          # 360 in 4th master semester
    
    g <- c(rep(letters[1:3], times = 350),    # seven panels (=letters) from design (see table 3 in grant proposal)
           rep(letters[2:4], times = 350), 
           rep(letters[3:4], times = 350), 
           rep(letters[4:5], times = 120), 
           rep(letters[4:6], times = 120), 
           rep(letters[5:6], times = 120), 
           rep(letters[5:7], times = 120)
    )
    
    mydata2 <- data.frame(x1, x2, x3, x4, x5, x6, g) 
    
    b1 <- c(2, 0.10, 0.10, 0.10,
            0.10, 0.10, 0.10)       # fixed intercept and slopes, assuming small effect
    b2 <- c(2, 0.30, 0.30, 0.30,
            0.30, 0.30, 0.30)       # fixed intercept and slopes, assuming medium effects
    b3 <- c(2, 0.1217353, 0.1217353, 0.1217353,
            0.1217353, 0.1217353, 0.1217353) # fixed intercept and slopes, assuming effect of BilWiss
    v <- 0.01                       # random intercept variance (derived from BilWiss data)
    s <- 0.527                      # residual standard deviation (derived from BilWiss data)
    
    
    # build a model
    model1 <- makeLmer(y ~ x1 + x2 + x3 + x4 + x5 + x6 + (1|g),
                       fixef=b1,
                       VarCorr=v,
                       sigma=s,
                       data=mydata2
    )
    print(model1)
    
    model2 <- makeLmer(y ~ x1 + x2 + x3 + x4 + x5 + x6 + (1|g),
                       fixef=b2,
                       VarCorr=v,
                       sigma=s,
                       data=mydata2
    )
    print(model2)
    
    model3 <- makeLmer(y ~ x1 + x2 + x3 + x4 + x5 + x6 + (1|g),
                       fixef=b3,
                       VarCorr=v,
                       sigma=s,
                       data=mydata2
    )
    print(model3)
    
    
    # start simulation
    powerSim(model1, nsim=500, seed = 123)
    # powerCurve(model1)
    powerSim(model2, nsim=500, seed = 123)
    # PC2 <- powerCurve(model2, along = "g", progress = F)
    powerSim(model3, nsim=500, seed = 123)
    
  # └ Model 2 [semester as interval scaled] ####
    
    # parameters
    predictor <- c(rep(2, times = 1050),     # 1050 in 2nd bachelor semester (reference category)   
                   rep(4, times = 1050),     # 1050 in 4th bachelor semester
                   rep(5, times = 700),      # 700 in 5th bachelor semester
                   rep(6, times = 240),      # 240 in 1st master semester
                   rep(7, times = 360),      # 360 in 2nd master semester
                   rep(8, times = 240),      # 240 in 3rd master semester
                   rep(9, times = 360)       # 360 in 4th master semester
                   )
    
    g <- c(rep(letters[1:3], times = 350),    # seven panels (=letters) from design (see table 3 in grant proposal)
           rep(letters[2:4], times = 350), 
           rep(letters[3:4], times = 350), 
           rep(letters[4:5], times = 120), 
           rep(letters[4:6], times = 120), 
           rep(letters[5:6], times = 120), 
           rep(letters[5:7], times = 120)
    )
    
    mydata3 <- data.frame(predictor, g) 
    
    b1 <- c(2, 0.10)       # fixed intercept and slopes, assuming small effect
    b2 <- c(2, 0.30)       # fixed intercept and slopes, assuming medium effects
    b3 <- c(2, 0.1217353)     # fixed intercept and slopes, assuming effects from BilWiss
    v <- 0.01              # random intercept variance (derived from BilWiss data) 0.000648
    s <- 0.527             # residual standard deviation (derived from BilWiss data)
    
    
    # build a model
    model4 <- makeLmer(y ~ predictor + (1|g),
                       fixef=b1,
                       VarCorr=v,
                       sigma=s,
                       data=mydata3
    )
    print(model4)
    
    model5 <- makeLmer(y ~ predictor + (1|g),
                       fixef=b2,
                       VarCorr=v,
                       sigma=s,
                       data=mydata3
    )
    print(model5)
    
    model6 <- makeLmer(y ~ predictor + (1|g),
                       fixef=b3,
                       VarCorr=v,
                       sigma=s,
                       data=mydata3
    )
    print(model6)
    
    
    # start simulation
    powerSim(model4, nsim=500, seed = 123)
    # powerCurve(model4)
    powerSim(model5, nsim=500, seed = 123)
    pc5 <- powerCurve(model5)
    powerSim(model6, nsim=500, seed = 123)
    pc6 <- powerCurve(model6)
    

## Studie 2 ####################################################################
# example taken from https://cran.r-project.org/web/packages/simr/vignettes/fromscratch.html
# example computed for master panel with 4 measurements

# parameters
x1 <- c(rep(0, times = 120), rep(1, times = 120), rep(0, times = 240))     # 120 measurements in T1 (reference category), 120 measurements in T2
x2 <- c(rep(0, times = 240), rep(1, times = 120), rep(0, times = 120))     # 120 measurements in T3
x3 <- c(rep(0, times = 360), rep(1, times = 120))                          # 120 measurements in T4

g <- c(rep(1:120, times = 4))          # clustered within person, n=120 students

mydata3 <- data.frame(x1, x2, x3, g)

b1 <- c(2, .30, .30, .30)       # fixed intercept and slope, assuming meadium effect
b2 <- c(2, .10, .10, .10)       # fixed intercept and slopes, assuming small effects
b3 <- c(2, .1217, .1217, .1217) # fixed intercept and slopes, assuming effects from BilWiss
v <- 0.01              # random intercept variance
s <- 0.527                # residual standard deviation


# build a model
model7 <- makeLmer(y ~ x1 + x2 + x3 + (1|g),
                   fixef=b1,
                   VarCorr=v,
                   sigma=s,
                   data=mydata3
)
print(model7)

model8 <- makeLmer(y ~ x1 + x2 + x3 + (1|g),
                   fixef=b2,
                   VarCorr=v,
                   sigma=s,
                   data=mydata3
)
print(model8)

model9 <- makeLmer(y ~ x1 + x2 + x3 + (1|g),
                   fixef=b3,
                   VarCorr=v,
                   sigma=s,
                   data=mydata3
)
print(model9)


# start simulation
powerSim(model7, nsim=500, seed = 123)
powerSim(model8, nsim=500, seed = 123)
powerSim(model9, nsim=500, seed = 123)



# Plotting Power for different effect sizes
PC9 <- powerCurve(model9, along = "g", progress = F)
plot(PC9) + title(main = "Poweranalysis of RQ2 based on BilWiss data")

# extracting n at which there is 80% Power
PC9$nlevels[9]


## Studie 3 ####################################################################
# how many participants do we need for a power of .80?
pwr.f2.test(u = 4,    # p-1 (predictors -1): treatment, 2*beliefs, 2*interactions
            f2 = .15,  #f²= {.02 (small); .15 (medium); .35 (large)} (Cohen, 1988)
            # v = 345,  # n-p
            sig.level = .05,
            power = .80
)


# how many participants do we need for a power of .95?
pwr.f2.test(u = 4,    # p-1 (predictors -1): treatment, 2*beliefs, 2*interactions
            f2 = .15,  #f²= {.02 (small); .15 (medium); .35 (large)} (Cohen, 1988)
            # v = 345,  # n-p
            sig.level = .05,
            power = .95
)



# Generate plot for power calculations
ptab <- cbind(NULL, NULL)       

for (i in seq(0,.25, by = .001)){
  pwrt1 <- pwr.f2.test(u = 4,               # p-1 (predictors -1): treatment, belief, interaction
                       f2 = i,              #f²= {.02 (small); .15 (medium); .35 (large)} (Cohen, 1988)
                       v = 345,             # n-p
                       sig.level = .05
                       # power = 
  )
  
  ptab <- rbind(ptab, cbind(pwrt1$power, pwrt1$f2))
}

ptab <- data.frame(Power = ptab[,1], effectSizef2 = ptab[,2])

ggplot(ptab, aes(x = effectSizef2, y = Power)) +
  geom_line(size=1, col = "#9b0029") + 
  theme_bw() + 
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12)) +
  labs(title = "Power acheived with different effect sizes",
       subtitle = "horizontal line at 80% power, vertical lines at small (f²=.02) and medium (f²=.15) effect",
       # caption = "", 
       x = "effect size f²", 
       y = "acheived Power") +
  scale_x_continuous(expand = c(0.02, 0)
  ) +
  geom_vline(xintercept = .02, linetype = 2, alpha = .6) +
  geom_vline(xintercept = .15, linetype = 2, alpha = .6) +
  geom_hline(yintercept = 0.80, linetype = 2, alpha = .6)
