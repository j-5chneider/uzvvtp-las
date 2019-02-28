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
      BilWiss <- read_sav("../data/BilWiss_Laengsschnitt_MZP1234_SUF_1809-24a.sav") #we can't share this data as is is scientific usefile from another study
      
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


    

## Studie 1.2 (Überzeugungen zw. Fachsemestern [SoSe20]) ##################################################################
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
    
    # the script takes quite long, so here are some rows of the resulting table
      #> samplesize   f1     f2     f3     f4     f5     f6     f7     rmsea      cfi       tli       srmr
      #> 670          0.806  0.806  0.808  0.792  0.806  0.784  0.782  0.01403042 0.9896873 0.9885414 0.03034742
      #> 690          0.802  0.784  0.778  0.796  0.788  0.818  0.822  0.01316585 0.9907838 0.9897598 0.02995081
      #> 710          0.790  0.828  0.802  0.842  0.842  0.838  0.852  0.01278678 0.9912922 0.9903247 0.02913166
      #> 730          0.828  0.818  0.838  0.798  0.820  0.820  0.800  0.01208556 0.9921801 0.9913112 0.02876481
      #> 750          0.856  0.862  0.862  0.840  0.838  0.842  0.852  0.01250722 0.9916610 0.9907345 0.02847058
      #> 770          0.858  0.850  0.844  0.854  0.862  0.836  0.844  0.01242964 0.9919912 0.9911013 0.02827026

    
    

    
    
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

    
    


## Studie 1.2 (Überzeugungen zw. Fachsemestern [alle Semester]) ##################################################################
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
    
    # making a loop to check out power for different sample sizes
    power_6 <- data.frame()
    for (participants in seq(from = 0.1, to = 1, by = 0.1)) {
        # parameters
        predictor <- c(rep(2, times = 1050*participants),     # 1050 in 2nd bachelor semester (reference category)   
                       rep(4, times = 1050*participants),     # 1050 in 4th bachelor semester
                       rep(5, times = 700*participants),      # 700 in 5th bachelor semester
                       rep(6, times = 240*participants),      # 240 in 1st master semester
                       rep(7, times = 360*participants),      # 360 in 2nd master semester
                       rep(8, times = 240*participants),      # 240 in 3rd master semester
                       rep(9, times = 360*participants)       # 360 in 4th master semester
        )
        
        g <- c(rep(letters[1:3], times = 350*participants),    # seven panels (=letters) from design (see table 3 in grant proposal)
               rep(letters[2:4], times = 350*participants), 
               rep(letters[3:4], times = 350*participants), 
               rep(letters[4:5], times = 120*participants), 
               rep(letters[4:6], times = 120*participants), 
               rep(letters[5:6], times = 120*participants), 
               rep(letters[5:7], times = 120*participants)
        )
        
        mydata3 <- data.frame(predictor, g) 
        
        b3 <- c(2, 0.1217353)     # fixed intercept and slopes, assuming effects from BilWiss
        v <- 0.01              # random intercept variance (derived from BilWiss data) 0.000648
        s <- 0.527             # residual standard deviation (derived from BilWiss data)
        
        model6 <- makeLmer(y ~ predictor + (1|g),
                           fixef=b3,
                           VarCorr=v,
                           sigma=s,
                           data=mydata3
        )
        
        tmp <- powerSim(model6, nsim=500, seed = 123)
        
        power_6[participants*10, "n"] <- participants*10
        power_6[participants*10, "power_mean"] <- summary(tmp)$mean
        power_6[participants*10, "power_lower"] <- summary(tmp)$lower
        power_6[participants*10, "power_upper"] <- summary(tmp)$upper
    }
    
    View(power_6)
    
    
  # └ Model 2.1 [semester as interval scaled, smaller sample] ####
    

    # making a loop to check out power for different sample sizes
    power_6.1 <- data.frame()
    for (participants in 0:10) {
      # parameters
      predictor <- c(rep(2, times = 9+(9*participants)),     # 27 in 2nd bachelor semester (reference category)   
                     rep(4, times = 9+(9*participants)),     # 27 in 4th bachelor semester
                     rep(5, times = 6+(6*participants)),     # 18 in 5th bachelor semester
                     rep(6, times = 2+(2*participants)),      # 6 in 1st master semester
                     rep(7, times = 3+(3*participants)),      # 9 in 2nd master semester
                     rep(8, times = 2+(2*participants)),      # 6 in 3rd master semester
                     rep(9, times = 3+(3*participants))       # 9 in 4th master semester
      )
      
      g <- c(rep(letters[1:3], times = 3+(3*participants)),    # seven panels (=letters) from design (see table 3 in grant proposal)
             rep(letters[2:4], times = 3+(3*participants)), 
             rep(letters[3:4], times = 3+(3*participants)), 
             rep(letters[4:5], times = 1+participants), 
             rep(letters[4:6], times = 1+participants), 
             rep(letters[5:6], times = 1+participants), 
             rep(letters[5:7], times = 1+participants)
      )
      
      mydata3.1 <- data.frame(predictor, g) 
      
      b3 <- c(2, 0.1217353)     # fixed intercept and slopes, assuming effects from BilWiss
      v <- 0.01              # random intercept variance (derived from BilWiss data) 0.000648
      s <- 0.527             # residual standard deviation (derived from BilWiss data)
      
      model6 <- makeLmer(y ~ predictor + (1|g),
                         fixef=b3,
                         VarCorr=v,
                         sigma=s,
                         data=mydata3.1
      )
      
      tmp <- powerSim(model6, nsim=500, seed = 123)
      
      power_6.1[participants+1, "n"] <- 15+(participants*15)
      power_6.1[participants+1, "power_mean"] <- summary(tmp)$mean
      power_6.1[participants+1, "power_lower"] <- summary(tmp)$lower
      power_6.1[participants+1, "power_upper"] <- summary(tmp)$upper
    }
    
    View(power_6.1)
    # results do take quite a while, here are some rows 
    #> n   power_mean power_lower power_upper
    #> 15	 0.680      0.6371369	  0.7207188
    #> 30	 0.932      0.9062704	  0.9524518
    #> 45	 0.968      0.9485532	  0.9816008
    #> 60	 0.992      0.9796444	  0.9978161
    #> 75	 0.998      0.9889075	  0.9999494
    

## Studie 2 [semester as categorical] ####################################################################
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
b3 <- c(2, 0.1217353, 0.1217353, 0.1217353) # fixed intercept and slopes, assuming effects from BilWiss
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

## Studie 2 [semester as interval scaled] ####################################################################
# └ latent change model ####
  popModel_7F_ch <- "
            # LATENT VARIABLES
                f1.1 =~ 0.654*i1_1_1 + 0.654*i1_1_2 + 0.654*i1_1_3 + 0.654*i1_1_4 + 0.654*i1_1_5
                f2.1 =~ 0.654*i2_1_1 + 0.654*i2_1_2 + 0.654*i2_1_3 + 0.654*i2_1_4 + 0.654*i2_1_5
                f3.1 =~ 0.654*i3_1_1 + 0.654*i3_1_2 + 0.654*i3_1_3 + 0.654*i3_1_4 + 0.654*i3_1_5
                f4.1 =~ 0.654*i4_1_1 + 0.654*i4_1_2 + 0.654*i4_1_3 + 0.654*i4_1_4 + 0.654*i4_1_5
                f5.1 =~ 0.654*i5_1_1 + 0.654*i5_1_2 + 0.654*i5_1_3 + 0.654*i5_1_4 + 0.654*i5_1_5
                f6.1 =~ 0.654*i6_1_1 + 0.654*i6_1_2 + 0.654*i6_1_3 + 0.654*i6_1_4 + 0.654*i6_1_5
                f7.1 =~ 0.654*i7_1_1 + 0.654*i7_1_2 + 0.654*i7_1_3 + 0.654*i7_1_4 + 0.654*i7_1_5
                
                f1.2 =~ 0.654*i1_2_1 + 0.654*i1_2_2 + 0.654*i1_2_3 + 0.654*i1_2_4 + 0.654*i1_2_5
                f2.2 =~ 0.654*i2_2_1 + 0.654*i2_2_2 + 0.654*i2_2_3 + 0.654*i2_2_4 + 0.654*i2_2_5
                f3.2 =~ 0.654*i3_2_1 + 0.654*i3_2_2 + 0.654*i3_2_3 + 0.654*i3_2_4 + 0.654*i3_2_5
                f4.2 =~ 0.654*i4_2_1 + 0.654*i4_2_2 + 0.654*i4_2_3 + 0.654*i4_2_4 + 0.654*i4_2_5
                f5.2 =~ 0.654*i5_2_1 + 0.654*i5_2_2 + 0.654*i5_2_3 + 0.654*i5_2_4 + 0.654*i5_2_5
                f6.2 =~ 0.654*i6_2_1 + 0.654*i6_2_2 + 0.654*i6_2_3 + 0.654*i6_2_4 + 0.654*i6_2_5
                f7.2 =~ 0.654*i7_2_1 + 0.654*i7_2_2 + 0.654*i7_2_3 + 0.654*i7_2_4 + 0.654*i7_2_5
                
                f1.3 =~ 0.654*i1_3_1 + 0.654*i1_3_2 + 0.654*i1_3_3 + 0.654*i1_3_4 + 0.654*i1_3_5
                f2.3 =~ 0.654*i2_3_1 + 0.654*i2_3_2 + 0.654*i2_3_3 + 0.654*i2_3_4 + 0.654*i2_3_5
                f3.3 =~ 0.654*i3_3_1 + 0.654*i3_3_2 + 0.654*i3_3_3 + 0.654*i3_3_4 + 0.654*i3_3_5
                f4.3 =~ 0.654*i4_3_1 + 0.654*i4_3_2 + 0.654*i4_3_3 + 0.654*i4_3_4 + 0.654*i4_3_5
                f5.3 =~ 0.654*i5_3_1 + 0.654*i5_3_2 + 0.654*i5_3_3 + 0.654*i5_3_4 + 0.654*i5_3_5
                f6.3 =~ 0.654*i6_3_1 + 0.654*i6_3_2 + 0.654*i6_3_3 + 0.654*i6_3_4 + 0.654*i6_3_5
                f7.3 =~ 0.654*i7_3_1 + 0.654*i7_3_2 + 0.654*i7_3_3 + 0.654*i7_3_4 + 0.654*i7_3_5
                
                change1.1 =~ 1*f1.2 + 1*f1.3
                change1.2 =~ 1*f1.3
                
                change2.1 =~ 1*f2.2 + 1*f2.3
                change2.2 =~ 1*f2.3
                
                change3.1 =~ 1*f3.2 + 1*f3.3
                change3.2 =~ 1*f3.3
                
                change4.1 =~ 1*f4.2 + 1*f4.3
                change4.2 =~ 1*f4.3
                
                change5.1 =~ 1*f5.2 + 1*f5.3
                change5.2 =~ 1*f5.3
                
                change6.1 =~ 1*f6.2 + 1*f6.3
                change6.2 =~ 1*f6.3
                
                change7.1 =~ 1*f7.2 + 1*f7.3
                change7.2 =~ 1*f7.3
  
            # VARIANCES
                f1.1 ~~ 1*f1.1
                f2.1 ~~ 1*f2.1
                f3.1 ~~ 1*f3.1
                f4.1 ~~ 1*f4.1
                f5.1 ~~ 1*f5.1
                f6.1 ~~ 1*f6.1
                f7.1 ~~ 1*f7.1
                
                # f1.2 ~~ 0*f1.2
                # f2.2 ~~ 0*f2.2
                # f3.2 ~~ 0*f3.2
                # f4.2 ~~ 0*f4.2
                # f5.2 ~~ 0*f5.2
                # f6.2 ~~ 0*f6.2
                # f7.2 ~~ 0*f7.2
                # 
                # f1.3 ~~ 0*f1.3
                # f2.3 ~~ 0*f2.3
                # f3.3 ~~ 0*f3.3
                # f4.3 ~~ 0*f4.3
                # f5.3 ~~ 0*f5.3
                # f6.3 ~~ 0*f6.3
                # f7.3 ~~ 0*f7.3
                
                change1.1 ~~ 0.01*change1.1
                change1.2 ~~ 0.01*change1.2
                change2.1 ~~ 0.01*change2.1
                change2.2 ~~ 0.01*change2.2
                change3.1 ~~ 0.01*change3.1
                change3.2 ~~ 0.01*change3.2
                change4.1 ~~ 0.01*change4.1
                change4.2 ~~ 0.01*change4.2
                change5.1 ~~ 0.01*change5.1
                change5.2 ~~ 0.01*change5.2
                change6.1 ~~ 0.01*change6.1
                change6.2 ~~ 0.01*change6.2
                change7.1 ~~ 0.01*change7.1
                change7.2 ~~ 0.01*change7.2
  
                i1_1_1 ~~ 0.573*i1_1_1   # T1
                i1_1_2 ~~ 0.573*i1_1_2
                i1_1_3 ~~ 0.573*i1_1_3
                i1_1_4 ~~ 0.573*i1_1_4
                i1_1_5 ~~ 0.573*i1_1_5
  
                i2_1_1 ~~ 0.573*i2_1_1
                i2_1_2 ~~ 0.573*i2_1_2
                i2_1_3 ~~ 0.573*i2_1_3
                i2_1_4 ~~ 0.573*i2_1_4
                i2_1_5 ~~ 0.573*i2_1_5
  
                i3_1_1 ~~ 0.573*i3_1_1
                i3_1_2 ~~ 0.573*i3_1_2
                i3_1_3 ~~ 0.573*i3_1_3
                i3_1_4 ~~ 0.573*i3_1_4
                i3_1_5 ~~ 0.573*i3_1_5
  
                i4_1_1 ~~ 0.573*i4_1_1
                i4_1_2 ~~ 0.573*i4_1_2
                i4_1_3 ~~ 0.573*i4_1_3
                i4_1_4 ~~ 0.573*i4_1_4
                i4_1_5 ~~ 0.573*i4_1_5
  
                i5_1_1 ~~ 0.573*i5_1_1
                i5_1_2 ~~ 0.573*i5_1_2
                i5_1_3 ~~ 0.573*i5_1_3
                i5_1_4 ~~ 0.573*i5_1_4
                i5_1_5 ~~ 0.573*i5_1_5
  
                i6_1_1 ~~ 0.573*i6_1_1
                i6_1_2 ~~ 0.573*i6_1_2
                i6_1_3 ~~ 0.573*i6_1_3
                i6_1_4 ~~ 0.573*i6_1_4
                i6_1_5 ~~ 0.573*i6_1_5
  
                i7_1_1 ~~ 0.573*i7_1_1
                i7_1_2 ~~ 0.573*i7_1_2
                i7_1_3 ~~ 0.573*i7_1_3
                i7_1_4 ~~ 0.573*i7_1_4
                i7_1_5 ~~ 0.573*i7_1_5
                
                i1_2_1 ~~ 0.573*i1_2_1   # T2
                i1_2_2 ~~ 0.573*i1_2_2
                i1_2_3 ~~ 0.573*i1_2_3
                i1_2_4 ~~ 0.573*i1_2_4
                i1_2_5 ~~ 0.573*i1_2_5
  
                i2_2_1 ~~ 0.573*i2_2_1
                i2_2_2 ~~ 0.573*i2_2_2
                i2_2_3 ~~ 0.573*i2_2_3
                i2_2_4 ~~ 0.573*i2_2_4
                i2_2_5 ~~ 0.573*i2_2_5
  
                i3_2_1 ~~ 0.573*i3_2_1
                i3_2_2 ~~ 0.573*i3_2_2
                i3_2_3 ~~ 0.573*i3_2_3
                i3_2_4 ~~ 0.573*i3_2_4
                i3_2_5 ~~ 0.573*i3_2_5
  
                i4_2_1 ~~ 0.573*i4_2_1
                i4_2_2 ~~ 0.573*i4_2_2
                i4_2_3 ~~ 0.573*i4_2_3
                i4_2_4 ~~ 0.573*i4_2_4
                i4_2_5 ~~ 0.573*i4_2_5
  
                i5_2_1 ~~ 0.573*i5_2_1
                i5_2_2 ~~ 0.573*i5_2_2
                i5_2_3 ~~ 0.573*i5_2_3
                i5_2_4 ~~ 0.573*i5_2_4
                i5_2_5 ~~ 0.573*i5_2_5
  
                i6_2_1 ~~ 0.573*i6_2_1
                i6_2_2 ~~ 0.573*i6_2_2
                i6_2_3 ~~ 0.573*i6_2_3
                i6_2_4 ~~ 0.573*i6_2_4
                i6_2_5 ~~ 0.573*i6_2_5
  
                i7_2_1 ~~ 0.573*i7_2_1
                i7_2_2 ~~ 0.573*i7_2_2
                i7_2_3 ~~ 0.573*i7_2_3
                i7_2_4 ~~ 0.573*i7_2_4
                i7_2_5 ~~ 0.573*i7_2_5
                
                i1_3_1 ~~ 0.573*i1_3_1   # T3
                i1_3_2 ~~ 0.573*i1_3_2
                i1_3_3 ~~ 0.573*i1_3_3
                i1_3_4 ~~ 0.573*i1_3_4
                i1_3_5 ~~ 0.573*i1_3_5
  
                i2_3_1 ~~ 0.573*i2_3_1
                i2_3_2 ~~ 0.573*i2_3_2
                i2_3_3 ~~ 0.573*i2_3_3
                i2_3_4 ~~ 0.573*i2_3_4
                i2_3_5 ~~ 0.573*i2_3_5
  
                i3_3_1 ~~ 0.573*i3_3_1
                i3_3_2 ~~ 0.573*i3_3_2
                i3_3_3 ~~ 0.573*i3_3_3
                i3_3_4 ~~ 0.573*i3_3_4
                i3_3_5 ~~ 0.573*i3_3_5
  
                i4_3_1 ~~ 0.573*i4_3_1
                i4_3_2 ~~ 0.573*i4_3_2
                i4_3_3 ~~ 0.573*i4_3_3
                i4_3_4 ~~ 0.573*i4_3_4
                i4_3_5 ~~ 0.573*i4_3_5
  
                i5_3_1 ~~ 0.573*i5_3_1
                i5_3_2 ~~ 0.573*i5_3_2
                i5_3_3 ~~ 0.573*i5_3_3
                i5_3_4 ~~ 0.573*i5_3_4
                i5_3_5 ~~ 0.573*i5_3_5
  
                i6_3_1 ~~ 0.573*i6_3_1
                i6_3_2 ~~ 0.573*i6_3_2
                i6_3_3 ~~ 0.573*i6_3_3
                i6_3_4 ~~ 0.573*i6_3_4
                i6_3_5 ~~ 0.573*i6_3_5
  
                i7_3_1 ~~ 0.573*i7_3_1
                i7_3_2 ~~ 0.573*i7_3_2
                i7_3_3 ~~ 0.573*i7_3_3
                i7_3_4 ~~ 0.573*i7_3_4
                i7_3_5 ~~ 0.573*i7_3_5
  
            # FACTOR CORRELATIONS
                f1.1 ~~ 0.6*f2.1   # factors T1
                f1.1 ~~ 0.4*f3.1
                f1.1 ~~ 0.2*f4.1
                f1.1 ~~ 0.0*f5.1
                f1.1 ~~ -0.2*f6.1
                f1.1 ~~ -0.4*f7.1
                f2.1 ~~ 0.6*f3.1
                f2.1 ~~ 0.4*f4.1
                f2.1 ~~ 0.2*f5.1
                f2.1 ~~ 0*f6.1
                f2.1 ~~ -0.2*f7.1
                f3.1 ~~ 0.6*f4.1
                f3.1 ~~ 0.4*f5.1
                f3.1 ~~ 0.2*f6.1
                f3.1 ~~ 0*f7.1
                f4.1 ~~ 0.6*f5.1
                f4.1 ~~ 0.4*f6.1
                f4.1 ~~ 0.2*f7.1
                f5.1 ~~ 0.6*f6.1
                f5.1 ~~ 0.4*f7.1
                f6.1 ~~ 0.6*f7.1

                f1.1 ~~ 0.1*change1.1   # change1
                f1.1 ~~ 0.1*change1.2
                f2.1 ~~ 0*change1.1
                f2.1 ~~ 0*change1.2
                f3.1 ~~ 0*change1.1
                f3.1 ~~ 0*change1.2
                f4.1 ~~ 0*change1.1
                f4.1 ~~ 0*change1.2
                f5.1 ~~ 0*change1.1
                f5.1 ~~ 0*change1.2
                f6.1 ~~ 0*change1.1
                f6.1 ~~ 0*change1.2
                f7.1 ~~ 0*change1.1
                f7.1 ~~ 0*change1.2

                f1.1 ~~ 0*change2.1   # change2
                f1.1 ~~ 0*change2.2
                f2.1 ~~ 0.1*change2.1
                f2.1 ~~ 0.1*change2.2
                f3.1 ~~ 0*change2.1
                f3.1 ~~ 0*change2.2
                f4.1 ~~ 0*change2.1
                f4.1 ~~ 0*change2.2
                f5.1 ~~ 0*change2.1
                f5.1 ~~ 0*change2.2
                f6.1 ~~ 0*change2.1
                f6.1 ~~ 0*change2.2
                f7.1 ~~ 0*change2.1
                f7.1 ~~ 0*change2.2

                f1.1 ~~ 0*change3.1   # change3
                f1.1 ~~ 0*change3.2
                f2.1 ~~ 0*change3.1
                f2.1 ~~ 0*change3.2
                f3.1 ~~ 0.1*change3.1
                f3.1 ~~ 0.1*change3.2
                f4.1 ~~ 0*change3.1
                f4.1 ~~ 0*change3.2
                f5.1 ~~ 0*change3.1
                f5.1 ~~ 0*change3.2
                f6.1 ~~ 0*change3.1
                f6.1 ~~ 0*change3.2
                f7.1 ~~ 0*change3.1
                f7.1 ~~ 0*change3.2

                f1.1 ~~ 0*change4.1   # change4
                f1.1 ~~ 0*change4.2
                f2.1 ~~ 0*change4.1
                f2.1 ~~ 0*change4.2
                f3.1 ~~ 0*change4.1
                f3.1 ~~ 0*change4.2
                f4.1 ~~ 0.1*change4.1
                f4.1 ~~ 0.1*change4.2
                f5.1 ~~ 0*change4.1
                f5.1 ~~ 0*change4.2
                f6.1 ~~ 0*change4.1
                f6.1 ~~ 0*change4.2
                f7.1 ~~ 0*change4.1
                f7.1 ~~ 0*change4.2

                f1.1 ~~ 0*change1.1   # change5
                f1.1 ~~ 0*change1.2
                f2.1 ~~ 0*change1.1
                f2.1 ~~ 0*change1.2
                f3.1 ~~ 0*change1.1
                f3.1 ~~ 0*change1.2
                f4.1 ~~ 0*change1.1
                f4.1 ~~ 0*change1.2
                f5.1 ~~ 0.1*change1.1
                f5.1 ~~ 0.1*change1.2
                f6.1 ~~ 0*change1.1
                f6.1 ~~ 0*change1.2
                f7.1 ~~ 0*change1.1
                f7.1 ~~ 0*change1.2

                f1.1 ~~ 0*change1.1   # change6
                f1.1 ~~ 0*change1.2
                f2.1 ~~ 0*change1.1
                f2.1 ~~ 0*change1.2
                f3.1 ~~ 0*change1.1
                f3.1 ~~ 0*change1.2
                f4.1 ~~ 0*change1.1
                f4.1 ~~ 0*change1.2
                f5.1 ~~ 0*change1.1
                f5.1 ~~ 0*change1.2
                f6.1 ~~ 0.1*change1.1
                f6.1 ~~ 0.1*change1.2
                f7.1 ~~ 0*change1.1
                f7.1 ~~ 0*change1.2

                f1.1 ~~ 0*change1.1   # change7
                f1.1 ~~ 0*change1.2
                f2.1 ~~ 0*change1.1
                f2.1 ~~ 0*change1.2
                f3.1 ~~ 0*change1.1
                f3.1 ~~ 0*change1.2
                f4.1 ~~ 0*change1.1
                f4.1 ~~ 0*change1.2
                f5.1 ~~ 0*change1.1
                f5.1 ~~ 0*change1.2
                f6.1 ~~ 0*change1.1
                f6.1 ~~ 0*change1.2
                f7.1 ~~ 0.1*change1.1
                f7.1 ~~ 0.1*change1.2
                
                # f1.2 ~~ 0.6*f2.2   # factors T2 (don't correlate b/c endogenous variabels)
                # f1.2 ~~ 0.4*f3.2
                # f1.2 ~~ 0.2*f4.2
                # f1.2 ~~ 0.0*f5.2
                # f1.2 ~~ -0.2*f6.2
                # f1.2 ~~ -0.4*f7.2
                # f2.2 ~~ 0.6*f3.2
                # f2.2 ~~ 0.4*f4.2
                # f2.2 ~~ 0.2*f5.2
                # f2.2 ~~ 0*f6.2
                # f2.2 ~~ -0.2*f7.2
                # f3.2 ~~ 0.6*f4.2
                # f3.2 ~~ 0.4*f5.2
                # f3.2 ~~ 0.2*f6.2
                # f3.2 ~~ 0*f7.2
                # f4.2 ~~ 0.6*f5.2
                # f4.2 ~~ 0.4*f6.2
                # f4.2 ~~ 0.2*f7.2
                # f5.2 ~~ 0.6*f6.2
                # f5.2 ~~ 0.4*f7.2
                # f6.2 ~~ 0.6*f7.2
                # 
                # f1.3 ~~ 0.6*f2.3    # factors T3 (don't correlate b/c endogenous variabels)
                # f1.3 ~~ 0.4*f3.3
                # f1.3 ~~ 0.2*f4.3
                # f1.3 ~~ 0.0*f5.3
                # f1.3 ~~ -0.2*f6.3
                # f1.3 ~~ -0.4*f7.3
                # f2.3 ~~ 0.6*f3.3
                # f2.3 ~~ 0.4*f4.3
                # f2.3 ~~ 0.2*f5.3
                # f2.3 ~~ 0*f6.3
                # f2.3 ~~ -0.2*f7.3
                # f3.3 ~~ 0.6*f4.3
                # f3.3 ~~ 0.4*f5.3
                # f3.3 ~~ 0.2*f6.3
                # f3.3 ~~ 0*f7.3
                # f4.3 ~~ 0.6*f5.3
                # f4.3 ~~ 0.4*f6.3
                # f4.3 ~~ 0.2*f7.3
                # f5.3 ~~ 0.6*f6.3
                # f5.3 ~~ 0.4*f7.3
                # f6.3 ~~ 0.6*f7.3
                
            # MEANS / INTERCEPTS
                change1.1 ~ 0.1217353*1
                change1.2 ~ 0.1217353*1
                change2.1 ~ 0.1217353*1
                change2.2 ~ 0.1217353*1
                change3.1 ~ 0.1217353*1
                change3.2 ~ 0.1217353*1
                change4.1 ~ 0.1217353*1
                change4.2 ~ 0.1217353*1
                change5.1 ~ 0.1217353*1
                change5.2 ~ 0.1217353*1
                change6.1 ~ 0.1217353*1
                change6.2 ~ 0.1217353*1
                change7.1 ~ 0.1217353*1
                change7.2 ~ 0.1217353*1
              
            # REGRESSION PATHS
                f1.2 ~ 1*f1.1
                f1.3 ~ 1*f1.1
                f1.3 ~ 1*f1.2
                
                f2.2 ~ 1*f2.1
                f2.3 ~ 1*f2.1
                f2.3 ~ 1*f2.2
                
                f3.2 ~ 1*f3.1
                f3.3 ~ 1*f3.1
                f3.3 ~ 1*f3.2
                
                f4.2 ~ 1*f4.1
                f4.3 ~ 1*f4.1
                f4.3 ~ 1*f4.2
                
                f5.2 ~ 1*f5.1
                f5.3 ~ 1*f5.1
                f5.3 ~ 1*f5.2
                
                f6.2 ~ 1*f6.1
                f6.3 ~ 1*f6.1
                f6.3 ~ 1*f6.2
                
                f7.2 ~ 1*f7.1
                f7.3 ~ 1*f7.1
                f7.3 ~ 1*f7.2
      "
  
  analyzeModel_7F_ch <- "
            # LATENT VARIABLES
                f1.1 =~ lambda*i1_1_1 + lambda*i1_1_2 + lambda*i1_1_3 + lambda*i1_1_4 + lambda*i1_1_5
                f2.1 =~ lambda*i2_1_1 + lambda*i2_1_2 + lambda*i2_1_3 + lambda*i2_1_4 + lambda*i2_1_5
                f3.1 =~ lambda*i3_1_1 + lambda*i3_1_2 + lambda*i3_1_3 + lambda*i3_1_4 + lambda*i3_1_5
                f4.1 =~ lambda*i4_1_1 + lambda*i4_1_2 + lambda*i4_1_3 + lambda*i4_1_4 + lambda*i4_1_5
                f5.1 =~ lambda*i5_1_1 + lambda*i5_1_2 + lambda*i5_1_3 + lambda*i5_1_4 + lambda*i5_1_5
                f6.1 =~ lambda*i6_1_1 + lambda*i6_1_2 + lambda*i6_1_3 + lambda*i6_1_4 + lambda*i6_1_5
                f7.1 =~ lambda*i7_1_1 + lambda*i7_1_2 + lambda*i7_1_3 + lambda*i7_1_4 + lambda*i7_1_5
                
                f1.2 =~ lambda*i1_2_1 + lambda*i1_2_2 + lambda*i1_2_3 + lambda*i1_2_4 + lambda*i1_2_5
                f2.2 =~ lambda*i2_2_1 + lambda*i2_2_2 + lambda*i2_2_3 + lambda*i2_2_4 + lambda*i2_2_5
                f3.2 =~ lambda*i3_2_1 + lambda*i3_2_2 + lambda*i3_2_3 + lambda*i3_2_4 + lambda*i3_2_5
                f4.2 =~ lambda*i4_2_1 + lambda*i4_2_2 + lambda*i4_2_3 + lambda*i4_2_4 + lambda*i4_2_5
                f5.2 =~ lambda*i5_2_1 + lambda*i5_2_2 + lambda*i5_2_3 + lambda*i5_2_4 + lambda*i5_2_5
                f6.2 =~ lambda*i6_2_1 + lambda*i6_2_2 + lambda*i6_2_3 + lambda*i6_2_4 + lambda*i6_2_5
                f7.2 =~ lambda*i7_2_1 + lambda*i7_2_2 + lambda*i7_2_3 + lambda*i7_2_4 + lambda*i7_2_5
                
                f1.3 =~ lambda*i1_3_1 + lambda*i1_3_2 + lambda*i1_3_3 + lambda*i1_3_4 + lambda*i1_3_5
                f2.3 =~ lambda*i2_3_1 + lambda*i2_3_2 + lambda*i2_3_3 + lambda*i2_3_4 + lambda*i2_3_5
                f3.3 =~ lambda*i3_3_1 + lambda*i3_3_2 + lambda*i3_3_3 + lambda*i3_3_4 + lambda*i3_3_5
                f4.3 =~ lambda*i4_3_1 + lambda*i4_3_2 + lambda*i4_3_3 + lambda*i4_3_4 + lambda*i4_3_5
                f5.3 =~ lambda*i5_3_1 + lambda*i5_3_2 + lambda*i5_3_3 + lambda*i5_3_4 + lambda*i5_3_5
                f6.3 =~ lambda*i6_3_1 + lambda*i6_3_2 + lambda*i6_3_3 + lambda*i6_3_4 + lambda*i6_3_5
                f7.3 =~ lambda*i7_3_1 + lambda*i7_3_2 + lambda*i7_3_3 + lambda*i7_3_4 + lambda*i7_3_5
                
                change1.1 =~ 1*f1.2 + 1*f1.3
                change1.2 =~ 1*f1.3
                
                change2.1 =~ 1*f2.2 + 1*f2.3
                change2.2 =~ 1*f2.3
                
                change3.1 =~ 1*f3.2 + 1*f3.3
                change3.2 =~ 1*f3.3
                
                change4.1 =~ 1*f4.2 + 1*f4.3
                change4.2 =~ 1*f4.3
                
                change5.1 =~ 1*f5.2 + 1*f5.3
                change5.2 =~ 1*f5.3
                
                change6.1 =~ 1*f6.2 + 1*f6.3
                change6.2 =~ 1*f6.3
                
                change7.1 =~ 1*f7.2 + 1*f7.3
                change7.2 =~ 1*f7.3
                
            # VARIANCES
                f1.2 ~~ 0*f1.2
                f2.2 ~~ 0*f2.2
                f3.2 ~~ 0*f3.2
                f4.2 ~~ 0*f4.2
                f5.2 ~~ 0*f5.2
                f6.2 ~~ 0*f6.2
                f7.2 ~~ 0*f7.2
                
                f1.3 ~~ 0*f1.3
                f2.3 ~~ 0*f2.3
                f3.3 ~~ 0*f3.3
                f4.3 ~~ 0*f4.3
                f5.3 ~~ 0*f5.3
                f6.3 ~~ 0*f6.3
                f7.3 ~~ 0*f7.3
                
            # REGRESSION PATHS
                f1.2 ~ 1*f1.1
                f1.3 ~ 1*f1.1

                f2.2 ~ 1*f2.1
                f2.3 ~ 1*f2.1

                f3.2 ~ 1*f3.1
                f3.3 ~ 1*f3.1

                f4.2 ~ 1*f4.1
                f4.3 ~ 1*f4.1

                f5.2 ~ 1*f5.1
                f5.3 ~ 1*f5.1

                f6.2 ~ 1*f6.1
                f6.3 ~ 1*f6.1

                f7.2 ~ 1*f7.1
                f7.3 ~ 1*f7.1
      "
  
  # making a loop to identify sample size for 80% power with effect from Bilwiss
  Output_7F_ch <- data.frame()
  
  for(participants in seq(from = 440, to = 600, by = 20)) {
    
    Output_tmp <- sim(nRep = 500, 
                      model = analyzeModel_7F_ch, 
                      n = participants, 
                      generate = popModel_7F_ch, 
                      lavaanfun = "growth",
                      std.lv = T, 
                      seed = 123)
    
    Output_7F_ch[(participants-420)/20, "samplesize"] <- participants
    Output_7F_ch[(participants-420)/20, "change1.1"] <- summaryParam(Output_tmp)[442, 4] # power of mean of change var
    Output_7F_ch[(participants-420)/20, "change1.2"] <- summaryParam(Output_tmp)[443, 4] # power of mean of change var
    Output_7F_ch[(participants-420)/20, "change2.1"] <- summaryParam(Output_tmp)[444, 4] # power of mean of change var
    Output_7F_ch[(participants-420)/20, "change2.2"] <- summaryParam(Output_tmp)[445, 4] # power of mean of change var
    Output_7F_ch[(participants-420)/20, "change3.1"] <- summaryParam(Output_tmp)[446, 4] # power of mean of change var
    Output_7F_ch[(participants-420)/20, "change3.2"] <- summaryParam(Output_tmp)[447, 4] # power of mean of change var
    Output_7F_ch[(participants-420)/20, "change4.1"] <- summaryParam(Output_tmp)[448, 4] # power of mean of change var
    Output_7F_ch[(participants-420)/20, "change4.2"] <- summaryParam(Output_tmp)[449, 4] # power of mean of change var
    Output_7F_ch[(participants-420)/20, "change5.1"] <- summaryParam(Output_tmp)[450, 4] # power of mean of change var
    Output_7F_ch[(participants-420)/20, "change5.2"] <- summaryParam(Output_tmp)[451, 4] # power of mean of change var
    Output_7F_ch[(participants-420)/20, "change6.1"] <- summaryParam(Output_tmp)[452, 4] # power of mean of change var
    Output_7F_ch[(participants-420)/20, "change6.2"] <- summaryParam(Output_tmp)[453, 4] # power of mean of change var
    Output_7F_ch[(participants-420)/20, "change7.1"] <- summaryParam(Output_tmp)[454, 4] # power of mean of change var
    Output_7F_ch[(participants-420)/20, "change7.2"] <- summaryParam(Output_tmp)[455, 4] # power of mean of change var
    Output_7F_ch[(participants-420)/20, "rmsea"] <- getCutoff(Output_tmp, 0.05)[4]
    Output_7F_ch[(participants-420)/20, "cfi"] <- getCutoff(Output_tmp, 0.05)[5]
    Output_7F_ch[(participants-420)/20, "tli"] <- getCutoff(Output_tmp, 0.05)[6]
    Output_7F_ch[(participants-420)/20, "srmr"] <- getCutoff(Output_tmp, 0.05)[7]
  }
  
  View(Output_7F_ch)

  
# └ latent (neighbour) change model (on one theory practice model) ####
  popModel_7F_ch1 <- "
  # LATENT VARIABLES
    f1.1 =~ 0.654*i1_1_1 + 0.654*i1_1_2 + 0.654*i1_1_3 + 0.654*i1_1_4 + 0.654*i1_1_5
    f1.2 =~ 0.654*i1_2_1 + 0.654*i1_2_2 + 0.654*i1_2_3 + 0.654*i1_2_4 + 0.654*i1_2_5
    f1.3 =~ 0.654*i1_3_1 + 0.654*i1_3_2 + 0.654*i1_3_3 + 0.654*i1_3_4 + 0.654*i1_3_5
    
    change1.1 =~ 1*f1.2 + 1*f1.3
    change1.2 =~ 1*f1.3
  

  # VARIANCES
    f1.1 ~~ 1*f1.1
    # f1.2 ~~ 0*f1.2
    # f1.3 ~~ 0*f1.3

    change1.1 ~~ 0.2*change1.1
    change1.2 ~~ 0.2*change1.2

    i1_1_1 ~~ 1*i1_1_1   # T1
    i1_1_2 ~~ 1*i1_1_2
    i1_1_3 ~~ 1*i1_1_3
    i1_1_4 ~~ 1*i1_1_4
    i1_1_5 ~~ 1*i1_1_5
    
    i1_2_1 ~~ 1*i1_2_1   # T2
    i1_2_2 ~~ 1*i1_2_2
    i1_2_3 ~~ 1*i1_2_3
    i1_2_4 ~~ 1*i1_2_4
    i1_2_5 ~~ 1*i1_2_5
    
    i1_3_1 ~~ 1*i1_3_1   # T3
    i1_3_2 ~~ 1*i1_3_2
    i1_3_3 ~~ 1*i1_3_3
    i1_3_4 ~~ 1*i1_3_4
    i1_3_5 ~~ 1*i1_3_5
    

  # FACTOR CORRELATIONS
    f1.1 ~~ 0.1*change1.1
    f1.1 ~~ 0.1*change1.2
    f1.2 ~~ 0.1*change1.2
    change1.1 ~~ -0.1*change1.2   # DOES THIS MAKE SENSE?


  # MEANS / INTERCEPTS
    change1.1 ~ 0.1217353*1
    change1.2 ~ 0.1217353*1

    # i1_1_1 ~ 0*1
    # i1_1_2 ~ 0*1
    # i1_1_3 ~ 0*1
    # i1_1_4 ~ 0*1
    # i1_1_5 ~ 0*1
    # i1_2_1 ~ 0*1
    # i1_2_2 ~ 0*1
    # i1_2_3 ~ 0*1
    # i1_2_4 ~ 0*1
    # i1_2_5 ~ 0*1
    # i1_3_1 ~ 0*1
    # i1_3_2 ~ 0*1
    # i1_3_3 ~ 0*1
    # i1_3_4 ~ 0*1
    # i1_3_5 ~ 0*1

  
  # REGRESSION PATHS
    f1.2 ~ 1*f1.1
    f1.3 ~ 1*f1.1
  "
  
  analyzeModel_7F_ch1 <- "
    # LATENT VARIABLES
      f1.1 =~ lambda*i1_1_1 + lambda*i1_1_2 + lambda*i1_1_3 + lambda*i1_1_4 + lambda*i1_1_5
      f1.2 =~ lambda*i1_2_1 + lambda*i1_2_2 + lambda*i1_2_3 + lambda*i1_2_4 + lambda*i1_2_5
      f1.3 =~ lambda*i1_3_1 + lambda*i1_3_2 + lambda*i1_3_3 + lambda*i1_3_4 + lambda*i1_3_5
    
      change1.1 =~ 1*f1.2 + 1*f1.3
      change1.2 =~ 1*f1.3
    
    # MEANS / INTERCEPTS
        # i1_1_1 ~ xi*1
        # i1_1_2 ~ xi*1
        # i1_1_3 ~ xi*1
        # i1_1_4 ~ xi*1
        # i1_1_5 ~ xi*1
        # i1_2_1 ~ xi*1
        # i1_2_2 ~ xi*1
        # i1_2_3 ~ xi*1
        # i1_2_4 ~ xi*1
        # i1_2_5 ~ xi*1
        # i1_3_1 ~ xi*1
        # i1_3_2 ~ xi*1
        # i1_3_3 ~ xi*1
        # i1_3_4 ~ xi*1
        # i1_3_5 ~ xi*1

  # FACTOR CORRELATIONS
        change1.2 ~~ f1.2
        change1.1 ~~ change1.2
    
    # VARIANCES
      # f1.1 ~~ 1*f1.1
      f1.2 ~~ 0*f1.2
      f1.3 ~~ 0*f1.3
    
    # REGRESSION PATHS
      f1.2 ~ 1*f1.1
      f1.3 ~ 1*f1.1
    "
  
  # making a loop to identify sample size for 80% power with effect from Bilwiss
  Output_7F_ch1 <- data.frame()
  
  for(participants in seq(from = 440, to = 540, by = 20)) {
      
      Output_tmp <- sim(nRep = 500, 
                        model = analyzeModel_7F_ch1, 
                        n = participants, 
                        generate = popModel_7F_ch1, 
                        lavaanfun = "growth", 
                        std.lv = T, 
                        seed = 123)
      
      Output_7F_ch1[(participants-420)/20, "samplesize"] <- participants
      Output_7F_ch1[(participants-420)/20, "change1.1"] <- summaryParam(Output_tmp)[442, 4] # power of mean of change var
      Output_7F_ch1[(participants-420)/20, "change1.2"] <- summaryParam(Output_tmp)[443, 4] # power of mean of change var
      Output_7F_ch1[(participants-420)/20, "change2.1"] <- summaryParam(Output_tmp)[444, 4] # power of mean of change var
      Output_7F_ch1[(participants-420)/20, "change2.2"] <- summaryParam(Output_tmp)[445, 4] # power of mean of change var
      Output_7F_ch1[(participants-420)/20, "change3.1"] <- summaryParam(Output_tmp)[446, 4] # power of mean of change var
      Output_7F_ch1[(participants-420)/20, "change3.2"] <- summaryParam(Output_tmp)[447, 4] # power of mean of change var
      Output_7F_ch1[(participants-420)/20, "change4.1"] <- summaryParam(Output_tmp)[448, 4] # power of mean of change var
      Output_7F_ch1[(participants-420)/20, "change4.2"] <- summaryParam(Output_tmp)[449, 4] # power of mean of change var
      Output_7F_ch1[(participants-420)/20, "change5.1"] <- summaryParam(Output_tmp)[450, 4] # power of mean of change var
      Output_7F_ch1[(participants-420)/20, "change5.2"] <- summaryParam(Output_tmp)[451, 4] # power of mean of change var
      Output_7F_ch1[(participants-420)/20, "change6.1"] <- summaryParam(Output_tmp)[452, 4] # power of mean of change var
      Output_7F_ch1[(participants-420)/20, "change6.2"] <- summaryParam(Output_tmp)[453, 4] # power of mean of change var
      Output_7F_ch1[(participants-420)/20, "change7.1"] <- summaryParam(Output_tmp)[454, 4] # power of mean of change var
      Output_7F_ch1[(participants-420)/20, "change7.2"] <- summaryParam(Output_tmp)[455, 4] # power of mean of change var
      Output_7F_ch1[(participants-420)/20, "rmsea"] <- getCutoff(Output_tmp, 0.05)[4]
      Output_7F_ch1[(participants-420)/20, "cfi"] <- getCutoff(Output_tmp, 0.05)[5]
      Output_7F_ch1[(participants-420)/20, "tli"] <- getCutoff(Output_tmp, 0.05)[6]
      Output_7F_ch1[(participants-420)/20, "srmr"] <- getCutoff(Output_tmp, 0.05)[7]
  }
  
  View(Output_7F_ch1)


## └ mixed effects model (varying intercept) ####
  # example taken from https://cran.r-project.org/web/packages/simr/vignettes/fromscratch.html
  # example computed for master panel with 4 measurements
  
  
  # making a loop to check out power for different sample sizes
  power_9 <- data.frame()
  for (participants in 0:6) {
    # parameters
    semester <- c(rep(1, times = 24 + participants), # students in their 1st,2nd,3rd or 4th semester
                  rep(2, times = 24 + participants), 
                  rep(3, times = 24 + participants), 
                  rep(4, times = 24 + participants)) 
    
    g <- c(rep(1:(24 + participants), times = 4))          # clustered within person, 4 measurement times
    
    mydata3 <- data.frame(semester, g)
    
    b <- c(2, 0.1217353) # fixed intercept and slopes, assuming effects from BilWiss
    v <- 0.01              # random intercept variance
    s <- 0.527                # residual standard deviation
    
    model9 <- makeLmer(y ~ semester + (1|g),
                       fixef=b,
                       VarCorr=v,
                       sigma=s,
                       data=mydata3
    )
  
    # start simulation
    tmp <- powerSim(model9, nsim=500, seed = 123)
    
    # saving results in data.frame
    power_9[participants+1, "n"] <- 24+(participants*4)
    power_9[participants+1, "power_mean"] <- summary(tmp)$mean
    power_9[participants+1, "power_lower"] <- summary(tmp)$lower
    power_9[participants+1, "power_upper"] <- summary(tmp)$upper
  }
  
  View(power_9)
  # results do take quite a while, here are some rows 
  #> n  power_mean  power_lower  power_upper
  #> 24	0.648	      0.6043586	   0.6898795
  #> 28	0.712	      0.6701349	   0.7513373
  #> 32	0.760	      0.7200905	   0.7968034
  #> 36	0.732	      0.6908790	   0.7703527
  #> 40	0.778	      0.7389832	   0.8136927
  #> 44	0.798	      0.7600903	   0.8323421
  #> 48	0.818	      0.7813315	   0.8508561
  
  
  
  # Plotting Power for different effect sizes
  PC9 <- powerCurve(model9, along = "g", progress = F)
  plot(PC9) + title(main = "Poweranalysis of RQ2 based on BilWiss data")



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
