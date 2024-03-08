##Exercise 6.1
##Read ESS and OECD data in:
ESS8<-read.dta("./Data/Stata/Exercise3/ESS8.dta")

OECD<-read.csv("./Data/Excel/Exercise3/OECDdata.csv", sep=";")

##Merge the data:
OECD<-OECD %>%
        mutate(inflow_foreig_perc=(inflow_foreign/population)*100)
OECD_ESS<-merge(x=ESS8, y=OECD, by.x = "Country", by.y="cntry", all=FALSE)

##Variables need to be standardized: 
##ais, lrscale, agea, gdppc, unemp_rate, infl_for_perc.

##a. An empty model with anti-immigrant sentiments (ais) as the dependent variable 
##and countries as level-2 clusters [M0].
ais_M0<-lmer(formula = scale(ais)~1+(1|Country),
             data = OECD_ESS)
summary(ais_M0)

##b. A model in which you add all individual-level variables: 
##res_mig (migration background), res_educ (educational level), 
##lrscale (left-right position), gndr (gender), agea (age) [M1].
ais_m1<-lmer(formula = scale(ais)~1+res_mig+res_educ+scale(lrscale)+gndr+scale(agea)+
                     (1|Country),
             data = OECD_ESS)
summary(ais_m1)


##Include a random slope of left-right self-placement into your model [M2].
##a. Is there variation in the effect of the left-right position? 
##How strong is it compared to the average effect?
ais_m2<-lmer(formula = scale(ais)~1+res_mig+res_educ+scale(lrscale)+gndr+scale(agea)+
                     (1+scale(lrscale)|Country),
             data = OECD_ESS)
summary(ais_m2)
options(scipen = 999)


##b. Did the inclusion of a random slope reduce the intercept variance?
##No

##c. What is the correlation/covariance between intercepts and slopes? What kind of ‘pattern’ of the regression lines does this imply?
VarCorr(ais_m2)$Country
##-0.01355
##Fanning in pattern

##d. Produce a caterpillar plot of the random effects (check the slides for the R code).
u_total<-ranef(ais_m2, condVar=TRUE)
##Catepillar plot:
dotplot(u_total,scale=(list(x=list(relation='free'))))


##e. Produce a scatter plot with the random intercepts at the x-axis and the random slopes at the y-axis (see slides for code). 
##Is there any observable pattern? Does it correspond to what we saw in the variance-covariance matrix?
u_total_df<-as.data.frame(u_total)

u0j<-u_total_df %>%
        filter(term=="(Intercept)")

u1j<-u_total_df %>%
        filter(term=="scale(lrscale)")

u0j$lr_ranef<-u1j$condval

colnames(u0j)[4]<-"ranIn"

ggplot(data = u0j, aes(x=ranIn, y=lr_ranef))+
        geom_point(aes(color=grp),shape=19)+
        geom_hline(yintercept = 0)+
        geom_vline(xintercept = 0)+
        xlab("intercept")+ylab("slope")


##Include the inflow of foreigners into your model [M3].
ais_M3<-lmer(formula = scale(ais)~1+res_mig+res_educ+scale(lrscale)+gndr+scale(agea)+
                     scale(inflow_foreig_perc)+(1+scale(lrscale)|Country),
             data = OECD_ESS)
summary(ais_M3)


## a. Does the inflow have a significant effect?
##Yes

## b. Check the estimated variance-covariance matrix. Do you notice any changes?
VarCorr(ais_m2)$Country
VarCorr(ais_M3)$Country

##The covariance between u1j - scale(lrscale) and u0j - intercept become positive
##changing from -0.01355052 to 0.006676447


## c. How much of the unexplained country-level variance from M2 is explained by M3?
(0.14719-0.10867)/0.14719
##26.17%

###Include a cross-level interaction between the inflow of foreigners and the left-right position [M4].
ais_M4<-lmer(formula = scale(ais)~1+res_mig+res_educ+scale(lrscale)+gndr+scale(agea)+
                     scale(lrscale):scale(inflow_foreig_perc)+
                     scale(inflow_foreig_perc)+(1+scale(lrscale)|Country),
             data = OECD_ESS)
summary(ais_M4)

VarCorr(ais_M4)$Country

##a. Is there a significant interaction effect between the left-right position and the inflow of foreigners?
##Yes

##b. How much of the variation in the slope from Model M3 is explained by the interaction in Model M4.
(0.02298-0.0165)/0.02298
##28.20%

##c. Produce a conditional effect plot of the marginal effect of left-right (see slides for code) to visualize the interaction effect.
interplot(m=ais_M4, var1 = "scale(lrscale)", var2 = "scale(inflow_foreig_perc)", hist=TRUE)

##Test
OECD_ESS$scale_lrs<-scale(OECD_ESS$lrscale)
OECD_ESS$scale_inflow<-scale(OECD_ESS$inflow_foreig_perc)

ais_M_test<-lmer(formula = scale(ais)~1+res_mig+res_educ+scale_lrs+gndr+scale(agea)+
                         scale_lrs:scale_inflow+
                         scale_inflow+(1+scale_lrs|Country),
             data = OECD_ESS)
summary(ais_M_test)

interplot(ais_M_test, "scale_lrs","scale_inflow",hist=TRUE)+
        geom_hline(yintercept = 0)+
        xlab("inflow foreigners(% of population)")+
        ylab("marginal effect of left-right position")+
        labs(caption = "dataset:ESS8")

screenreg(list(ais_M0, ais_m1, ais_m2, ais_M3, ais_M4), custom.model.names = c("M0", "M1", "M2", "M3","M4"),
          custom.coef.names = c("Intercept", "migrant background (Ref.=No)", 
                                "Upper medium (Ref.=high)", "lower medium (Ref.=high)", "low (Ref.=high)",
                                "Left-right",  
                                "Female",
                                "Age", "Inflow_immigrants",
                                "leftRight*InflowImmi"))

##produce a plot showing the fitted lines from the random slope model (M2).
OECD_ESS$M2Prediction<-predict(ais_m2)
        
ggplot(data = OECD_ESS, aes(scale(lrscale),M2Prediction))+
        geom_smooth(se=FALSE, method="lm", linewidth=2, color="black")+
        geom_smooth(aes(group=Country, color=Country), method="lm", alpha=0.4, linewidth=0.7, se=FALSE)+
        xlab("left right")+ylab("anti-immigrant sentiments")

#####-------------------------------------------------------------------------------------------------
###Exercise 6.2:
##Read the data in: 
load("./Data/RData/Exercise4/ssls.Rdata")


##Estimate a null model with score as the dependent variable (M0).
score_M0<-lmer(formula = score~1+(1|schoolid),
               data = ssls)
summary(score_M0)


##Estimate a model with pupils’ social class (sclass) and sex (female) as independent variables (M1).
score_M1<-lmer(formula = score~sclass+female+(1|schoolid),
               data = ssls)
summary(score_M1)

##Estimate a model where the effect of sex (female) varies between different schools (M2).
score_M2<-lmer(formula = score~sclass+female+(1+female|schoolid),
               data = ssls)
summary(score_M2)

VarCorr(score_M2)$schoolid

##a. How much variance is there between schools in the effect of sex?
##0.4509

##b. Does the inclusion of a random slope explain variation between school intercepts (compared to M1)? 
###What does that mean (hint: think about the coding of female)?
(34.81-32.9452)/34.81
##5.36% is explained by including a random slope of female
##It means that the average score of female and males vary???

##c. Produce a caterpillar plot showing the variation in intercepts and slopes.
u_total2<-ranef(score_M2,condVar=TRUE)
dotplot(u_total2, scales=list(x=list(relation='free')))

##d. Produce a scatter plot of the random effects, with slopes on the y-axis and intercepts on the x-axis.
##Does this plot indicate a clear pattern that corresponds to the correlation from the variance-covariance matrix of the model?
U_total2_df<-as.data.frame(u_total2)
u0j_2<-U_total2_df %>%
        filter(term=="(Intercept)")

u1j_2<-U_total2_df %>%
        filter(term=="femalefemale")

u0j_2$female_ranef<-u1j_2$condval

colnames(u0j_2)

ggplot(data = u0j_2, aes(x=condval,y=female_ranef))+
        geom_point()+
        geom_hline(yintercept = 0)+
        geom_vline(xintercept = 0)+
        xlab("intercept")+ylab("slope")


##e. What does the correlation between intercepts and slopes suggest? 
##Does it make sense to speak about ‘fanning’ and ‘fanning out’? How else should this plot be interpreted?
##The correlation is 0.4714613
##fanning out patter, the score are homogenous among boys and polarize more among females


##Estimate a model including the school-level variable 
##school type (schtype)and its interaction with sex (M3).
score_M3<-lmer(formula = score~sclass+female+schtype+
                       female:schtype+
                       (1+female|schoolid),
               data = ssls)
summary(score_M3)

VarCorr(score_M3)$schoolid

##a. Is the interaction significant?
##No

##b. Independent of significance, what does the interaction effect suggest? 
##Are differences between boys and girls larger in independent or in state schools?
##The differences between boys and girls are larger in independent schools


##c. How much of the variation in the slope of sex is explained by school type?
(0.4509-0.4409)/0.4509
##2.22%

##Produce a summary table
screenreg(list(score_M0, score_M1, score_M2, score_M3), custom.model.names = c("M0", "M1", "M2", "M3"),
          custom.coef.names = c("Intercept", "Intermediate class (Ref.=prof.)", 
                                "Working class (Ref.=prof.)",  
                                "Undefined class (Ref.=prof.)",
                                "Female (Ref.=male)", "Independent school (Ref.=state)",
                                "Female X Independ. School"))
