##Read the data in:
load("D:/Sociology Master/KU Leuven/Multilevel Analysis/MultilevelClass/Data/RData/Exercise7/sns.Rdata")

View(sns)

##Q2:Estimate a null model with attain as the dependent variable (M0) 
##and random intercepts at the school-level (a strictly hierarchical two-level model).
School_M0<-lmer(formula = attain~1+(1|schid),
                data = sns)
summary(School_M0)


##a. How much variance is there between schools?
0.09501/(0.09501+0.93048)
##9.26%

##Q3: Add all student-level variables to account for the composition of students in schools: 
##dadocc, dadunemp, daded, momed, male.
School_M1<-lmer(formula = attain~1+dadocc+dadunemp+daded+momed+male+(1|schid),
                data = sns)
summary(School_M1)


##a. How much of the variance between schools has been explained by the level-1 variables?
(0.09501-0.03794)/0.09501
#60.07%

##b. Extract the REs of schools and make a caterpillar plot to get a ranking of schools.
u0j_total<-ranef(School_M1, condVar=TRUE)
dotplot(u0j_total)


##Q4: Now, estimate a cross-classified null model, where schools and neighborhoods are higher-level clusters.
School_Neigh_M0<-lmer(formula = attain~1+(1|schid)+(1|neighid),
                      data = sns)
summary(School_Neigh_M0)

##a. What proportions of variance are located at the school level and at the neighborhood-level?
##School level:
0.08115/(0.14051+0.08115+0.79571)
##7.98% 

##Neighbourhood level:
0.14051/(0.14051+0.08115+0.79571)
##13.81%

##b. Compare the proportion to the null model with just school effects. 
##Did we overestimate school-effects when we ignored neighborhoods?
##Yes, the school level variation decrease from 9.26% to 7.98% 

##Q5: Add all student-level predictors to the model.
School_Neigh_M1<-lmer(formula = attain~1+dadocc+dadunemp+daded+momed+male+(1|schid)+(1|neighid),
                data = sns)
summary(School_Neigh_M1)

##a. How much of the variance in schools and how much of the variance in neighborhoods is explained 
##by level-1 variables.
##School-level:
(0.08115-0.03627)/0.08115
##55.3%

##Neighbourhood level:
(0.14051-0.06153)/0.14051
##56.21%

##b. Extract the REs for schools and neighborhoods. Create a caterpillar plot. Did the ranking change?
uj_vk_total<-ranef(School_Neigh_M1, condVar=TRUE)
dotplot(uj_vk_total, scales=list(x=list(relation='free')))

uj_school<-dotplot(uj_vk_total)[["schid"]]
vk_neighbourhood<-dotplot(uj_vk_total)[["neighid"]]
grid.arrange(uj_school, vk_neighbourhood, nrow=1)

##c. For fast students: After finishing 6. 
##you may come back here: Create a scatter plot of the school-level REs 
##from the strictly hierarchical two-level model and from the cross-classified model. 
##This plot shows us how strong the correlation between rankings in the two models is.



##Q6: Add the neighborhood-level variable deprivation (deprive) to your model.
School_Neigh_M2<-lmer(formula = attain~1+dadocc+dadunemp+daded+momed+male+deprive+
                              (1|schid)+(1|neighid),
                      data = sns)
summary(School_Neigh_M2)

##a. Does the variable have a significant effect?
##Yes

##b. Does it explain variation between neighborhoods only or also variation between schools? 
##How much variation is explained at the higher-levels?
##Neighbourhood: 
(0.06153-0.02565)/0.06153
##58.31%

##School level:
(0.03627-0.02109)/0.03627
##41.85%


##Whether the effect of father unemployment status becomes stronger in the deprived neighbourhood
School_Neigh_M3_RanEf<-lmer(formula = attain~1+dadocc+dadunemp+daded+momed+male+deprive+
                              (1|schid)+(1+dadunemp|neighid),
                      data = sns)
summary(School_Neigh_M3_RanEf)

School_Neigh_M3_RanEf_Inter<-lmer(formula = attain~1+dadocc+dadunemp+daded+momed+male+deprive+
                                    dadunemp:deprive+
                                    (1+dadunemp|schid)+(1+dadunemp|neighid),
                            data = sns)
summary(School_Neigh_M3_RanEf_Inter)
