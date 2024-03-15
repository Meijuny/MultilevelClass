##Read data in:
load("D:/Sociology Master/KU Leuven/Multilevel Analysis/MultilevelClass/Data/RData/Exercise10/distress.Rdata")

##Q3: Declare the data frame to be a panel data frame, using the pdata.frame object from the plm package.
distress_panel<-pdata.frame(distress, index = c("id","wave"), drop.index = TRUE)

##Now we want to estimate a number of models. 
##The dependent variables is psydis; 
##the independent variables are socsel, divorce, widow1, cohab1, ager, sexr, educr.

##a. Start with a FE model, using the plm package. 
distress_FE<-plm(formula = psydis~socsel+divorce+widow1+cohab1+ager+sexr+educr,
                 data = distress_panel,
                 model = "within")
summary(distress_FE)

##Does the model suggest a negative effect of divorce on psychological distress?
##The effect of divorce on distress is positive

##b. Estimate a RE model, using the plm package. 
distress_RE<-plm(formula = psydis~socsel+divorce+widow1+cohab1+ager+sexr+educr,
                 data = distress_panel,
                 model = "random")
summary(distress_RE)

##What effect is suggested by the RE model? 
##How does it compare to the estimate from the FE model?
#
##Socsel (disposition for stress) is a time-constant variable --> only appears in RE model
#
#divorce has positive effect on both FE and RE model --> stronger effect in RE model 
##(In addition to going through divorce at a certain time point within an individual, 
##the individuals not divorced are also compared with individuals who are divorced)
#
#widow has positive effect on both FE and RE model --> stronger effect in RE model
#(same reason as divorce)
#
#Cohabitation has negative effect on both FE and RE model --> stronger effect in RE model
#(same reason as divorce)
#
#age has positive effect on both FE and RE model --> WEAKER effect in RE model --> why??????
#
#sex is a time-constant variable --> only appears in RE model
#
#education has negative effect on both FE and RE model --> stronger effect in RE model
#(same reason above)

##c Perform a Breusch-Pagan test on your RE model. 
##What is the results of the test and what does it mean?

#Breush_Pagan test for the RE model: 
plmtest(distress_RE, type="bp")
##p<0.05, must consider the hierarchical data structure

##d. Perform a Hausman test to compare your FE and your RE model. Which should be preferred?
phtest(distress_FE, distress_RE)
#p<0.05, RE model has biased results, prefer FE model

##Q5: Now we want to estimate within-between models.
##a. Which variable can be decomposed into within and between effects, which cannot?
#
##socsel and sexr cannot be decomposed
##divorce, widow1, cohab1, ager, educr can be decomposed

##b. Estimate a within-between model and construct the variables yourself.
distress<-distress %>%
        group_by(id) %>%
        mutate(divorce_mean=mean(divorce, na.rm=TRUE),
               widow1_mean=mean(widow1, na.rm=TRUE),
               cohab1_mean=mean(cohab1, na.rm=TRUE),
               ager_mean=mean(ager,na.rm=TRUE),
               educr_mean=mean(educr, na.rm=TRUE))

distress<-distress %>%
        mutate(divoce_dm=divorce-divorce_mean,
               widow1_dm=widow1-widow1_mean,
               cohab1_dm=cohab1-cohab1_mean,
               ager_dm=ager-ager_mean,
               educr_dm=educr-educr_mean)

##c. Then, estimate the same model with the panelr package and use the results to check your former model.
##make distress into panel data again:
distress_panel<-pdata.frame(distress, index = c("id","wave"), drop.index = TRUE)

##Estimate the model:
distress_WBmodel<-plm(formula = psydis~socsel+sexr+divorce_mean+divoce_dm+widow1_mean+widow1_dm+
                              cohab1_mean+cohab1_dm+ager_mean+ager_dm+educr_mean+educr_dm,
                      data = distress_panel,
                      model = "random")
summary(distress_WBmodel)

##Q6: Finally, estimate a model using the Mundlak specification (using the panelr package). 
##Use the model output to answer the following question:
##Mundlak plm package:
distress_Mundlak_plm<-plm(formula = psydis~socsel+sexr+divorce+divorce_mean+widow1+widow1_mean
                          +cohab1+cohab1_mean+ager+ager_mean+educr+educr_mean,
                          data = distress_panel,
                          model = "random")
summary(distress_Mundlak_plm)

##WB panelr package approach:
##first declare the dataset as panel data in panelr:
distress_panelr_data<-panel_data(distress, id=id, wave = wave)

##Estimate the WB model:
distress_WB_panelr<-wbm(formula = psydis~divorce+widow1+cohab1+ager+educr | 
                                socsel+sexr,
                        data = distress_panelr_data)

summary(distress_WB_panelr)

##a. Are the within and between effects of divorce identical?
##No

##b. Does that change our opinion on the preferred model?
##We should use RE to decompose the within and between effect