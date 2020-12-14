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
library(pander)


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
    
    # Output_7F <- sim(nRep = 500, 
    #                    model = analyzeModel_7F, 
    #                    n = 940, 
    #                    generate = popModel_7F, 
    #                    lavaanfun = "cfa", 
    #                    std.lv = T, 
    #                    seed = 123)
    # 
    # summary(Output_7F)
    # getCutoff(Output_7F, 0.05)
    # plotCutoff(Output_7F, 0.05)
        
        # making a loop to identify sample size for 80% power with medium efect
        Output2_loop <- data.frame()
        
        for(participants in seq(from = 50, to = 250, by = 10)) {
          
          Output_tmp <- sim(nRep = 500, 
                            model = analyzeModel_7F, 
                            n = participants, 
                            generate = popModel_7F, 
                            lavaanfun = "cfa", 
                            std.lv = T, 
                            seed = 123)
          
          Output2_loop[(participants-40)/10, "samplesize"] <- participants
          Output2_loop[(participants-40)/10, "f1_1"] <- summaryParam(Output_tmp)[1, 10]
          Output2_loop[(participants-40)/10, "f1_2"] <- summaryParam(Output_tmp)[2, 10]
          Output2_loop[(participants-40)/10, "f1_3"] <- summaryParam(Output_tmp)[3, 10]
          Output2_loop[(participants-40)/10, "f1_4"] <- summaryParam(Output_tmp)[4, 10]
          Output2_loop[(participants-40)/10, "f1_5"] <- summaryParam(Output_tmp)[5, 10]
          Output2_loop[(participants-40)/10, "f2_1"] <- summaryParam(Output_tmp)[6, 10]
          Output2_loop[(participants-40)/10, "f2_2"] <- summaryParam(Output_tmp)[7, 10]
          Output2_loop[(participants-40)/10, "f2_3"] <- summaryParam(Output_tmp)[8, 10]
          Output2_loop[(participants-40)/10, "f2_4"] <- summaryParam(Output_tmp)[9, 10]
          Output2_loop[(participants-40)/10, "f2_5"] <- summaryParam(Output_tmp)[10, 10]
          Output2_loop[(participants-40)/10, "f3_1"] <- summaryParam(Output_tmp)[11, 10]
          Output2_loop[(participants-40)/10, "f3_2"] <- summaryParam(Output_tmp)[12, 10]
          Output2_loop[(participants-40)/10, "f3_3"] <- summaryParam(Output_tmp)[13, 10]
          Output2_loop[(participants-40)/10, "f3_4"] <- summaryParam(Output_tmp)[14, 10]
          Output2_loop[(participants-40)/10, "f3_5"] <- summaryParam(Output_tmp)[15, 10]
          Output2_loop[(participants-40)/10, "f4_1"] <- summaryParam(Output_tmp)[16, 10]
          Output2_loop[(participants-40)/10, "f4_2"] <- summaryParam(Output_tmp)[17, 10]
          Output2_loop[(participants-40)/10, "f4_3"] <- summaryParam(Output_tmp)[18, 10]
          Output2_loop[(participants-40)/10, "f4_4"] <- summaryParam(Output_tmp)[19, 10]
          Output2_loop[(participants-40)/10, "f4_5"] <- summaryParam(Output_tmp)[20, 10]
          Output2_loop[(participants-40)/10, "f5_1"] <- summaryParam(Output_tmp)[21, 10]
          Output2_loop[(participants-40)/10, "f5_2"] <- summaryParam(Output_tmp)[22, 10]
          Output2_loop[(participants-40)/10, "f5_3"] <- summaryParam(Output_tmp)[23, 10]
          Output2_loop[(participants-40)/10, "f5_4"] <- summaryParam(Output_tmp)[24, 10]
          Output2_loop[(participants-40)/10, "f5_5"] <- summaryParam(Output_tmp)[25, 10]
          Output2_loop[(participants-40)/10, "f6_1"] <- summaryParam(Output_tmp)[26, 10]
          Output2_loop[(participants-40)/10, "f6_2"] <- summaryParam(Output_tmp)[27, 10]
          Output2_loop[(participants-40)/10, "f6_3"] <- summaryParam(Output_tmp)[28, 10]
          Output2_loop[(participants-40)/10, "f6_4"] <- summaryParam(Output_tmp)[29, 10]
          Output2_loop[(participants-40)/10, "f6_5"] <- summaryParam(Output_tmp)[30, 10]
          Output2_loop[(participants-40)/10, "f7_1"] <- summaryParam(Output_tmp)[31, 10]
          Output2_loop[(participants-40)/10, "f7_2"] <- summaryParam(Output_tmp)[32, 10]
          Output2_loop[(participants-40)/10, "f7_3"] <- summaryParam(Output_tmp)[33, 10]
          Output2_loop[(participants-40)/10, "f7_4"] <- summaryParam(Output_tmp)[34, 10]
          Output2_loop[(participants-40)/10, "f7_5"] <- summaryParam(Output_tmp)[35, 10]
          Output2_loop[(participants-40)/10, "chi2"] <- getCutoff(Output_tmp, 0.05)[1]
          Output2_loop[(participants-40)/10, "aic"] <- getCutoff(Output_tmp, 0.05)[2]
          Output2_loop[(participants-40)/10, "bic"] <- getCutoff(Output_tmp, 0.05)[3]
          Output2_loop[(participants-40)/10, "rmsea"] <- getCutoff(Output_tmp, 0.05)[4]
          Output2_loop[(participants-40)/10, "cfi"] <- getCutoff(Output_tmp, 0.05)[5]
          Output2_loop[(participants-40)/10, "tli"] <- getCutoff(Output_tmp, 0.05)[6]
          Output2_loop[(participants-40)/10, "srmr"] <- getCutoff(Output_tmp, 0.05)[7]
        }
        
        ggplot(Output2_loop, aes(x=samplesize, y=rmsea)) + geom_point() + geom_hline(yintercept=0.05, color="#ff0000")
        ggplot(Output2_loop, aes(x=samplesize, y=cfi)) + geom_point() + geom_hline(yintercept=0.95, color="#ff0000")
        ggplot(Output2_loop, aes(x=samplesize, y=tli)) + geom_point() + geom_hline(yintercept=0.95, color="#ff0000")
        ggplot(Output2_loop, aes(x=samplesize, y=srmr)) + geom_point() + geom_hline(yintercept=0.11, color="#ff0000")
        
        Output2_loop <- Output2_loop %>%
          mutate(coverage = rowMeans(data.frame(f1_1, f1_2, f1_3, f1_4, f1_5, f2_1, f2_2, f2_3, f2_4, f2_5, f3_1, f3_2, f3_3, f3_4, f3_5, f4_1, f4_2, f4_3, f4_4, f4_5, f5_1, f5_2, f5_3, f5_4, f5_5, f6_1, f6_2, f6_3, f6_4, f6_5, f7_1, f7_2, f7_3, f7_4, f7_5)))
        
        ggplot(Output2_loop, aes(x=samplesize, y=coverage)) + 
          geom_point() + 
          geom_smooth(se = F, method = "lm", formula = y ~ log(x)) + 
          scale_y_continuous(limits = c(.9,1))
        
        pander(Output2_loop[c(1, 37:44)])