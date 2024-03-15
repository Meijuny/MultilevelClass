##Load the data in:
load("D:/Sociology Master/KU Leuven/Multilevel Analysis/MultilevelClass/Data/RData/Exercise8/bdhs.Rdata")

##Q2Estimate a null model with antemed as the dependent variable (M0) 
##and random intercepts at the comunity-level. Use the function glmer with the logit link.
Health_M0<-glmer(formula = antemed~1+(1|comm),
                 data = bdhs,
                 family=binomial(link = "logit"))
summary(Health_M0)


##a. What is the overall probability of having received antenatal care?
exp(0.14809)/(1+exp(0.14809))
##53.7% probability to have receveid care

##b. How much variance is there between communities? Calculate the VPC.
1.464/(1.464+pi^2/3)
##30.8%

##c. Calculate the probabilities of having received antenatal care 
##for a community one standard deviation below the average 
##and compare it to the probability in a community one standard deviation above the average.
exp(0.14809+1.21)/(1+exp(0.14809+1.21))
##In a community with care one sd more than average, the probability for a woman to
##receive care is 79.5%

##one commmynity below
exp(0.14809-1.21)/(1+exp(0.14809-1.21))
##25.69%

## 79.5-25.69=53.81% difference

##Q3: Add the individual-level variables: magec, meduc, islam, wealth.
Health_M1<-glmer(formula = antemed~1+magec+meduc+islam+wealth+(1|comm),
                 data = bdhs,
                 family=binomial(link = "logit"))
summary(Health_M1)

##a. Which effects are significant? Are they in the expected direction?
##education
##wealth

##b. We want to know how much variation between communities is still unexplained. 
##Since it is difficult to use the VPC, we calculate the same probabilities as in 2a. and c. 
##Calculate the probabilities for a woman with zero on all predictor variables (just using the constant); 
##note, however, that the differences you get between these probabilities will 
##depend on the values you choose for the covariates.

##For woman with the average age with none education and not beliving in Islam and with average wealth
exp(-0.365290)/(1+exp(-0.365290))
##40.97%

##For women in a community one sd above:
exp(-0.365290+0.9262)/(1+exp(-0.365290+0.9262))
##63.67%

##For women in a community one sd below:
exp(-0.365290-0.9262)/(1+exp(-0.365290-0.9262))
##21.56%

##Now around 42.11% difference (decrease from 53.81%)

##c. Are there still strong community effects or did our model with individual-level variables account for them?
##Account for some

##Q4:Now, estimate a model which includes the community-level variable urban.
Health_M2<-glmer(formula = antemed~1+magec+meduc+islam+wealth+urban+(1|comm),
                 data = bdhs,
                 family=binomial(link = "logit"))
summary(Health_M2)

##a. Is the effect significant? How large is it compared to other coefficients?
#Yes, large, but a bit smaller than secondary education

##b. Calculate the probabilities for an average community, a community one standard deviation above, 
##and a community one standard deviation below the average.
##i. First, just use the constant, as in 3b. 
##What is the difference between the two communities below and above the average?

##For woman with the average age with none education and not beliving in Islam and with average wealth
exp(-0.688904)/(1+exp(-0.688904))
##33.43%

##For women in a community one sd above:
exp(-0.688904+0.8218)/(1+exp(-0.688904+0.8218))
##53.32%

##For women in a community one sd below:
exp(-0.688904-0.8218)/(1+exp(-0.688904-0.8218))
##18.08%

##What is the difference between the two communities below and above the average?
53.32-18.08
##35.24% (decrease further from 42.11%)

##ii. Second, calculate these probabilities with the value 0.314 for the variable urban 
##(0.314 is the mean of the dummy urban). 
##What is the difference between the two communities below and above the average?
exp(-0.688904+1.037129*0.314)/(1+exp(-0.688904+1.037129*0.314))

##Community one sd above:
exp(-0.688904+1.037129*0.314+0.8218)/(1+exp(-0.688904+1.037129*0.314+0.8218))
##61.27%

##Community one sd below:
exp(-0.688904+1.037129*0.314-0.8218)/(1+exp(-0.688904+1.037129*0.314-0.8218))
##23.42%

##Difference: 
61.27-23.42
##37.84% (VS. 42.11%)

##iii. Which one of the two calculations is comparable to the former model from 3.
##Th

##c. Are there still large community difference in the unexplained part of the model?
##Yes!


library(panelr)