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
      BilWiss <- read_sav("../../3_Instrumente/BilWiss/BilWiss MZP 1-4/BilWiss_Laengsschnitt_MZP1234_SUF_1809-24a.sav") #we can't share this data as is is scientific usefile from another study
      
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
    
    for(participants in seq(from = 600, to = 780, by = 10)) {
      
      Output_tmp <- sim(nRep = 1000, 
                        model = analyzeModel_7F_reg, 
                        n = participants, 
                        generate = popModel_7F_reg, 
                        lavaanfun = "sem", 
                        std.lv = T, 
                        seed = 321)
      
      Output_7F_reg[(participants-590)/10, "samplesize"] <- participants
      Output_7F_reg[(participants-590)/10, "f1"] <- summaryParam(Output_tmp)[36, 4] # power semester on f1
      Output_7F_reg[(participants-590)/10, "f2"] <- summaryParam(Output_tmp)[37, 4] # power semester on f2
      Output_7F_reg[(participants-590)/10, "f3"] <- summaryParam(Output_tmp)[38, 4] # power semester on f3
      Output_7F_reg[(participants-590)/10, "f4"] <- summaryParam(Output_tmp)[39, 4] # power semester on f4
      Output_7F_reg[(participants-590)/10, "f5"] <- summaryParam(Output_tmp)[40, 4] # power semester on f5
      Output_7F_reg[(participants-590)/10, "f6"] <- summaryParam(Output_tmp)[41, 4] # power semester on f6
      Output_7F_reg[(participants-590)/10, "f7"] <- summaryParam(Output_tmp)[42, 4] # power semester on f7
      Output_7F_reg[(participants-590)/10, "rmsea"] <- getCutoff(Output_tmp, 0.05)[4]
      Output_7F_reg[(participants-590)/10, "cfi"] <- getCutoff(Output_tmp, 0.05)[5]
      Output_7F_reg[(participants-590)/10, "tli"] <- getCutoff(Output_tmp, 0.05)[6]
      Output_7F_reg[(participants-590)/10, "srmr"] <- getCutoff(Output_tmp, 0.05)[7]
    }
    
    View(Output_7F_reg)
    
    # the script takes quite long, so here are some rows of the resulting table
      #> samplesize   f1     f2     f3     f4     f5     f6     f7     rmsea      cfi       tli       srmr
      #> 670          0.794  0.797  0.790  0.788  0.809  0.774  0.782  0.01376676 0.9899855 0.9888728 0.03050905
      #> 680          0.805  0.809  0.791  0.811  0.809  0.805  0.801  0.01370217 0.9903191 0.9892434 0.03010329
      #> 690          0.824  0.818  0.836  0.827  0.814  0.805  0.786  0.01329076 0.9906169 0.9895743 0.02980208
      #> 700          0.801  0.827  0.827  0.806  0.811  0.802  0.822  0.01304758 0.9909914 0.9899905 0.02966691
      #> 710          0.814  0.811  0.827  0.821  0.824  0.826  0.816  0.01304505 0.9909458 0.9899398 0.02938304
      #> 720          0.837  0.839  0.822  0.829  0.787  0.818  0.845  0.01293031 0.9910091 0.9900101 0.02920495
      #> 730          0.836  0.839  0.825  0.836  0.838  0.834  0.844  0.01349428 0.9902298 0.9891442 0.02922476



## Studie 2 [semester as interval scaled] ####################################################################

# └ latent (neighbour) change model (on one theory practice model) ####
  ## estimations of correlations between change-factors and state1 based on Bilwiss data
    model <- "
      state1 =~ lamb1*tp01_2 + lamb2*tp02r_2 + lamb3*tp03_2 + lamb4*tp04_2 + lamb5*tp05_2 + lamb6*tp06_2
      state2 =~ lamb1*tp01_3 + lamb2*tp02r_3 + lamb3*tp03_3 + lamb4*tp04_3 + lamb5*tp05_3 + lamb6*tp06_3
      state3 =~ lamb1*tp01_4 + lamb2*tp02r_4 + lamb3*tp03_4 + lamb4*tp04_4 + lamb5*tp05_4 + lamb6*tp06_4
      
      diff2_1 =~ 1*state2 + 1*state3
      diff3_2 =~ 1*state3
      
      state2 ~ 1*state1
      state3 ~ 1*state1
      
      state2 ~~ 0*state3
      state2 ~~ 0*diff3_2
      state3 ~~ 0*diff2_1
      diff2_1 ~~ state1
      diff3_2 ~~ state1
      diff3_2 ~~ diff2_1
      
      state2 ~~ 0*state2
      state3 ~~ 0*state3
      "
    
    fit <- growth(model = model, data = BilWiss)
    
    # as I cannot share BilWiss data, here are the results on the correlations
      #>  lhs     op  rhs      est.std
      #>  
      #>  
      #>  
  
  
  
  ## simulating and estimating latent-change model (factor loadings derived from BilWiss)
    pop_lc <- "
        state1 =~ 1*d11 + 1*d21 + 1*d31 + 1*d41 + 1*d51    # derived from BilWis-data
        state2 =~ 1*d12 + 1*d22 + 1*d32 + 1*d42 + 1*d52
        state3 =~ 1*d13 + 1*d23 + 1*d33 + 1*d43 + 1*d53
        
        diff2_1 =~ 0*d11
        diff3_2 =~ 0*d11
        
        state2 ~ 1*state1 + 1*diff2_1
        state2 ~~ 0*state2
        
        state3 ~ 1*state1 + 1*diff2_1 + 1*diff3_2
        state3 ~~ 0*state3
        
        state2 ~~ 0*state3
        state2 ~~ 0*diff3_2
        
        state3 ~~ 0*diff2_1
        
        state1 ~~ -0.036*diff2_1
        state1 ~~ -0.031*diff3_2
        
        diff2_1 ~~ -0.037*diff3_2
        
        d11 ~ 0*1
        d21 ~ 0*1
        d31 ~ 0*1
        d41 ~ 0*1
        d51 ~ 0*1
        d12 ~ 0*1
        d22 ~ 0*1
        d32 ~ 0*1
        d42 ~ 0*1
        d52 ~ 0*1
        d13 ~ 0*1
        d23 ~ 0*1
        d33 ~ 0*1
        d43 ~ 0*1
        d53 ~ 0*1
        
        state1 ~ 2.8*1
        state2 ~ -0.019*1
        state3 ~ -0.063*1
        diff2_1 ~ -0.082*1
        diff3_2 ~ -0.063*1
        
        d11 ~~ 0.381*d11
        d21 ~~ 0.381*d21
        d31 ~~ 0.381*d31
        d41 ~~ 0.381*d41
        d51 ~~ 0.381*d51
        d12 ~~ 0.381*d12
        d22 ~~ 0.381*d22
        d32 ~~ 0.381*d32
        d42 ~~ 0.381*d42
        d52 ~~ 0.381*d52
        d13 ~~ 0.381*d13
        d23 ~~ 0.381*d23
        d33 ~~ 0.381*d33
        d43 ~~ 0.381*d43
        d53 ~~ 0.381*d53
        
        state1 ~~ 0.205*state1
        diff2_1 ~~ 0.087*diff2_1
        diff3_2 ~~ 0.119*diff3_2
        "
    ana_lc <- "
        state1 =~ lamb1*d11 + lamb2*d21 + lamb3*d31 + lamb4*d41 + lamb5*d51
        state2 =~ lamb1*d12 + lamb2*d22 + lamb3*d32 + lamb4*d42 + lamb5*d52
        state3 =~ lamb1*d13 + lamb2*d23 + lamb3*d33 + lamb4*d43 + lamb5*d53
        
        diff2_1 =~ 0*d11
        diff3_2 =~ 0*d11
        
        state2 ~ 1*state1 + 1*diff2_1
        state2 ~~ 0*state2
        
        state3 ~ 1*state1 + 1*diff2_1 + 1*diff3_2
        state3 ~~ 0*state3
        
        state2 ~~ 0*state3
        state2 ~~ 0*diff3_2
        state3 ~~ 0*diff2_1
        diff2_1 ~~ state1
        diff3_2 ~~ state1
        diff3_2 ~~ diff2_1

        d11 ~ 0*1
        d21 ~ x2*1
        d31 ~ x3*1
        d41 ~ x4*1
        d51 ~ x5*1
        d12 ~ 0*1
        d22 ~ x2*1
        d32 ~ x3*1
        d42 ~ x4*1
        d52 ~ x5*1
        d13 ~ 0*1
        d23 ~ x2*1
        d33 ~ x3*1
        d43 ~ x4*1
        d53 ~ x5*1
        " 
    
      
      
      # making a loop to identify sample size for 80% power with effect from Bilwiss
      Output_lc <- data.frame()
      
      for(participants in seq(from = 65, to = 100, by = 5)) {
          
          Out_tmp <- sim(nRep = 500,
                         model = ana_lc,
                         n = participants,
                         generate = pop_lc,
                         lavaanfun = "growth",
                         seed = 123)
          
          Output_lc[(participants-60)/5, "samplesize"] <- participants
          Output_lc[(participants-60)/5, "change1.1"] <- summaryParam(Out_tmp)[49, 4] # power of mean of change var
          Output_lc[(participants-60)/5, "change1.2"] <- summaryParam(Out_tmp)[50, 4] # power of mean of change var
          Output_lc[(participants-60)/5, "rmsea"] <- getCutoff(Out_tmp, 0.05)[4]
          Output_lc[(participants-60)/5, "cfi"] <- getCutoff(Out_tmp, 0.05)[5]
          Output_lc[(participants-60)/5, "tli"] <- getCutoff(Out_tmp, 0.05)[6]
          Output_lc[(participants-60)/5, "srmr"] <- getCutoff(Out_tmp, 0.05)[7]
      }
      
      View(Output_lc)
      
      #> samplesize change1.1 change1.2   rmsea        cfi         tli         srmr
      #> 50         0.606     0.780       0.09856440   0.8334725   0.8302390   0.12693923
      #> 55         0.646     0.834       0.09022738   0.8547837   0.8519639   0.12301040
      #> 60         0.656     0.860       0.08533429   0.8736513   0.8711979   0.11709535
      #> 65         0.714     0.896       0.08079458   0.8835989   0.8813387   0.11079500
      #> 70         0.752     0.904       0.07585843   0.8987260   0.8967596   0.10989735
      #> 75         0.782     0.928       0.07310551   0.9071454   0.9053424   0.10408118
      #> 80         0.810     0.944       0.07089856   0.9115083   0.9097900   0.09990589
      #> 85         0.838     0.952       0.06846975   0.9175925   0.9159924   0.09779472
  



## Studie 3 ####################################################################
# how many participants do we need for a power of .80?
pwr.f2.test(u = 4,    # p-1 (predictors -1): treatment, 2*beliefs (Transformation & Relationierung), 2*interactions
            f2 = .085,  #f²= {.02 (small); .15 (medium); .35 (large)} (Cohen, 1988); we assume an effect between amall and medium
            # v = 345,  # n-p
            sig.level = .05,
            power = .80
)
 
  
# how many participants do we need for a power of .95?
pwr.f2.test(u = 4,    # p-1 (predictors -1): treatment, 2*beliefs, 2*interactions
            f2 = .085,  #f²= {.02 (small); .15 (medium); .35 (large)} (Cohen, 1988)
            # v = 345,  # n-p
            sig.level = .05,
            power = .95
)



# Generate plot for power calculations
ptab <- cbind(NULL, NULL)       

for (i in seq(0,.25, by = .001)){
  pwrt1 <- pwr.f2.test(u = 4,               # p-1 (predictors -1): treatment, belief, interaction
                       f2 = i,              #f²= {.02 (small); .15 (medium); .35 (large)} (Cohen, 1988)
                       v = 345,             # n-p (the sample with which we plan)
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
  labs(title = "Power acheived with different effect sizes (and our expected 350 participants)",
       subtitle = "horizontal line at 80% power, vertical lines at small (f²=.02) and medium (f²=.15) effect",
       # caption = "", 
       x = "effect size f²", 
       y = "acheived Power") +
  scale_x_continuous(expand = c(0.02, 0)
  ) +
  geom_vline(xintercept = .02, linetype = 2, alpha = .6) +
  geom_vline(xintercept = .15, linetype = 2, alpha = .6) +
  geom_hline(yintercept = 0.80, linetype = 2, alpha = .6)
