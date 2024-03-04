###Exercise 4.1
##Read ESS and OECD data in:
ESS8<-read.dta("./Data/Stata/Exercise3/ESS8.dta")

OECD<-read.csv("./Data/Excel/Exercise3/OECDdata.csv", sep=";")

##Merge the data:
OECD<-OECD %>%
        mutate(inflow_foreig_perc=(inflow_foreign/population)*100)
OECD_ESS<-merge(x=ESS8, y=OECD, by.x = "Country", by.y="cntry", all=FALSE)

##Empty model
ais_M0<-lmer(formula = scale(ais)~1+(1|Country),
             data = OECD_ESS)
summary(ais_M0)

options(scipen = 999)

##a. How much variation is due to between-country differences? Calculate a statistic that answers this question.
0.1762/(0.1762+0.8552)
###=17.08%

##b. Create a caterpillar plot from the empty model. 
##Is the ranking in line with your expectations? Are there any countries that stick out?
u0j<-ranef(ais_M0)
dotplot(u0j)
##Island is very NOT anti-immigrant, Slovakia is very anti-immigrant followed by Hungary and Czech

##M1: with all indivdidual variables
ais_M1<-lmer(formula = scale(ais)~1+res_mig+res_educ+scale(lrscale)+gndr+scale(agea)+
                     (1|Country),
             data = OECD_ESS)
summary(ais_M1)


##How much variation at level-1 is explained by your model? 
##Calculate the reduction of unexplained variance in comparison to the null model.
(0.8552-0.7725)/0.8552
##9.67%

##c. How much variation between countries (i.e. at level-2) is explained by the individual-level variables? 
##Calculate the reduction of unexplained variance. 
##Is there between-country variation left to be explained by country-level variables?
(0.1762-0.1441)/0.1762
##18.22%

##M2: model with all individual + inflow of immigrants
ais_M2<-lmer(formula = scale(ais)~1+res_mig+res_educ+scale(lrscale)+gndr+scale(agea)
             +scale(inflow_foreig_perc)+(1|Country),
             data = OECD_ESS)
summary(ais_M2)

##Does immigration inflow have a significant effect? What direction has the effect?
##negative significant, the more immigrant inflow, the less anti-immigrants


##How much variation at the country-level is explained by this variable? 
##This is, calculate the reduction of unexplained variance compared to the last model.
(0.1441-0.1054)/0.1441
###26.86%

##M3: individual + inflow of immigrants + unemploy + GDP per capita
ais_M3<-lmer(formula = scale(ais)~1+res_mig+res_educ+scale(lrscale)+gndr+scale(agea)
             +scale(inflow_foreig_perc)+scale(unemp_rate)+scale(gdppc)
             +(1|Country),
             data = OECD_ESS)
summary(ais_M3)


##a. Do the control variables have significant effects?
##No

##b. Does the effect of immigration inflow change? Is it still significant?
##Not significant

##c. Calculate the explained variance between countries in comparison to the null model.
(0.1762-0.1057)/0.1762
##40%
##We can explain more variance even though the effects are not significant????

##d. Calculate the explained variance at the country-level in comparison to the last model. 
##Do GDP/c and unemployment rates explain anything?
(0.1054-0.1057)/0.1054
##No

##Put everything together
library(textreg)
texreg(list(ais_M0, ais_M1, ais_M2, ais_M3), custom.model.names = c("M0", "M1", "M2","M3"),
       caption.above=TRUE, single.row=TRUE, custom.coef.names = c("Intercept",
                                                                  "Migration Background (Ref.=no)", "Upper Medium Educ. (Ref.=high)",
                                                                  "Lower Medium Educ. (Ref.=high)", "Low Educ. (Ref.=high)", "Left-Right",
                                                                  "Female (Ref.=Men)", "Age", "Inflow Immigrants","Unemploy Rate","GDP/c"),
       caption="Example: Multilevel Models of Anti-immigrant sentiments")



##create a plot that shows the estimated parallel lines from some of the models you estimated (maybe the null model and the final model).
OECD_ESS$PredM0<-predict(ais_M0)
ggplot(data = OECD_ESS, aes(x=agea, y=PredM0, color=Country, group=Country))+
        geom_line()

OECD_ESS$PredM1<-predict(ais_M1)
ggplot(data = OECD_ESS, aes(x=scale(lrscale), y=PredM1, color=Country, group=Country))+
        geom_smooth(se=FALSE, method="lm")

####-------------------------------------------------------------------------------------------------
##Exercise 4.2:
load("D:/Sociology Master/KU Leuven/Multilevel Analysis/MultilevelClass/Data/RData/Exercise4/ssls.Rdata")

View(ssls)

##Estimate a null model with score as the dependent variable.
##a. How many students and how many schools are in the estimation sample?
score_M0<-lmer(formula = score~1+(1|schoolid),
               data = ssls)
summary(score_M0)

##b. How much variance is there between schools? Calculate the ICC/ VPC.
(61.17)/(61.17+258.36)
##19.14%

##c. Create a caterpillar plot from the null model.
Score_u0j<-ranef(score_M0)
dotplot(Score_u0j)

##Estimate a model with pupils’ social class (sclass) and sex (female) as independent variables.
score_M1<-lmer(formula = score~1+sclass+female+(1|schoolid),
               data = ssls)
summary(score_M1)

##a. What are the effects of these variables? Are they in line with your expectations?
##Compared to professional class, the score are lower in the rest of the classes
##Female is better in scores than boys

##b. How much variation at the pupil- and the school-level is explained by your model?
##Pupil level:
(258.36-231.46)/258.36
##10.41% on pupil level is explained

##school level
(61.17-34.81)/61.17
##43.09% on country level is explained

##c. How much room is there left for school-level effects?
34.81/61.17
##56.9% is left

##Estimate a model including the three school-level variables 
##school type, urban-rural and denomination (schtype, schurban, schdenom).
score_M2<-lmer(formula = score~1+sclass+female
               +schtype+schurban+schdenom+
               +(1|schoolid),
               data = ssls)
summary(score_M2)

##a. What are the effects of these variables? Which variable has the strongest effect?
##How to compare without standardize it?

##b. How much variation between schools is explained by the model?
(61.17-30.66)/61.17
##48.88% is explained compared to the null model

##c. How much did the introduction of school-level variables reduce the unexplained variance from the former model?
(34.81-30.66)/34.81
##11.92% is explained compared to the previous model




#####-------------------------------------------------------------------------------------------
M_Class<-lmer(formula = ais~1+lrscale+(1+lrscale|Country),
              data = ESS8)
summary(M_Class)

ESS8_newdat<-ESS8 %>%
        dplyr::select(lrscale, Country) %>%
        arrange(Country)

newdat<-expand.grid(lrscale=seq(min(ESS8$lrscale),max(ESS8$lrscale), by=0.1),
                    Country=unique(ESS8$Country))

ESS8$Mpred1<-predict(M_Class)
ESS8$AverageLine<-predict(M_Class, re.form=NA)

ggplot(ESS8, aes(x=lrscale, y=ais)) + 
        geom_line(data = ESS8_newdat, aes(x=lrscale, y=Mpred1, group=Country, color=Country), method="lm")+
        geom_line(data = ESS8_newdat, aes(x=lrscale, y=AverageLine), color="black", linewidth=1.25, method="lm")

ggplot(data = ESS8, aes(x=lrscale, y=Mpred1))+
        geom_smooth(se=FALSE, method="lm", linewidth=2, color="black")+
        stat_smooth(aes(group=Country, color=Country), geom="line", linewidth=1, alpha=0.4)

u<-ranef(M_Class, condVar=TRUE)
u_df<-as.data.frame(u)
u0j<-u_df %>%
        filter(term=="(Intercept)")
u1j<-u_df %>%
        filter(term=="lrscale")
u0j$lr_ranef<-u1j$condval

##To plot the relationship between intercept and slope
ggplot(data = u0j, aes(x=condval, y=lr_ranef))+
        geom_point(aes(color=grp),shape=19)+
        geom_hline(yintercept = 0)+
        geom_vline(xintercept = 0)+
        xlab("intercept")+ylab("slope")

##catepillar plot
dotplot(u, scales=list(x=list(relation='free')))

VarCorr(M_Class)$Country

x<-seq(from=-3,3,by=0.1)
y<-0.19014-2*0.01783*x+0.03118*x^2
plot(x,y,type = "l",
     xlab = "lrscale",
     ylab="variance between countries")


###Interaction term:
M1<-lmer(formula = ais~1+scale(lrscale)+inflow_foreig_perc+
                 (1+lrscale|Country),
         data = OECD_ESS)
summary(M1)

M2<-lmer(formula = ais~1+scale(lrscale)+inflow_foreig_perc+
                 lrscale:inflow_foreig_perc+(1+lrscale|Country),
         data = OECD_ESS)
summary(M2)


interplot(m=M2, var1="lrscale", var2="inflow_foreig_perc")
