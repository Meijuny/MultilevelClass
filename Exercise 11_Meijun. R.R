##Read the data in:
load("D:/Sociology Master/KU Leuven/Multilevel Analysis/MultilevelClass/Data/RData/Exercise11/garmitvoc.Rdata")

##Q3: Create two new variables:
##a. a variable t, which measures years but starts with the value 0, and
##b. a variable tsq, which is t squared.
garmitvoc$t<-as.numeric(sub(1961, 0, garmitvoc$year))
garmitvoc$t<-as.numeric(sub(1962, 1, garmitvoc$t))
garmitvoc$t<-as.numeric(sub(1963, 2, garmitvoc$t))
garmitvoc$t<-as.numeric(sub(1964, 3, garmitvoc$t))
garmitvoc$t<-as.numeric(sub(1965, 4, garmitvoc$t))
garmitvoc$t<-as.numeric(sub(1966, 5, garmitvoc$t))
garmitvoc$t<-as.numeric(sub(1967, 6, garmitvoc$t))
garmitvoc$t<-as.numeric(sub(1968, 7, garmitvoc$t))
garmitvoc$t<-as.numeric(sub(1969, 8, garmitvoc$t))
garmitvoc$t<-as.numeric(sub(1970, 9, garmitvoc$t))
garmitvoc$t<-as.numeric(sub(1971, 10, garmitvoc$t))
garmitvoc$t<-as.numeric(sub(1972, 11, garmitvoc$t))
garmitvoc$t<-as.numeric(sub(1973, 12, garmitvoc$t))
garmitvoc$t<-as.numeric(sub(1974, 13, garmitvoc$t))
garmitvoc$t<-as.numeric(sub(1975, 14, garmitvoc$t))
garmitvoc$t<-as.numeric(sub(1976, 15, garmitvoc$t))
garmitvoc$t<-as.numeric(sub(1977, 16, garmitvoc$t))
garmitvoc$t<-as.numeric(sub(1978, 17, garmitvoc$t))
garmitvoc$t<-as.numeric(sub(1979, 18, garmitvoc$t))
garmitvoc$t<-as.numeric(sub(1980, 19, garmitvoc$t))
garmitvoc$t<-as.numeric(sub(1981, 20, garmitvoc$t))
garmitvoc$t<-as.numeric(sub(1982, 21, garmitvoc$t))
garmitvoc$t<-as.numeric(sub(1983, 22, garmitvoc$t))
garmitvoc$t<-as.numeric(sub(1984, 23, garmitvoc$t))
garmitvoc$t<-as.numeric(sub(1985, 24, garmitvoc$t))
garmitvoc$t<-as.numeric(sub(1986, 25, garmitvoc$t))
garmitvoc$t<-as.numeric(sub(1987, 26, garmitvoc$t))
garmitvoc$t<-as.numeric(sub(1988, 27, garmitvoc$t))
garmitvoc$t<-as.numeric(sub(1989, 28, garmitvoc$t))
garmitvoc$t<-as.numeric(sub(1990, 29, garmitvoc$t))
garmitvoc$t<-as.numeric(sub(1991, 30, garmitvoc$t))
garmitvoc$t<-as.numeric(sub(1992, 31, garmitvoc$t))
garmitvoc$t<-as.numeric(sub(1993, 32, garmitvoc$t))

garmitvoc<-garmitvoc %>%
        mutate(tsq=t^2)

##Q4: Estimate a linear growth model with spend as the dependent variable, using t as your explanatory variable.
spend_gcm<-lmer(formula = spend~1+t+(1+t|country),
                data = garmitvoc)
summary(spend_gcm)

##a. Is there a significant growth of public spending?
#Yes, as the time pass, the spending increases

##b. By how much does spending on average increase each year?
#0.7139% of GDP more each year

##Q5: Estimate a quadratic growth curve model, adding the variable tsq. 
##Hint: The model does not converge very well when we include the quadratic component of time also in the random part. So, we just add it to the fixed part and assume that the quadratic component is the same across all countries.
spend_gcm_tsq<-lmer(formula = spend~1+t+tsq+(1+t|country),
                    data = garmitvoc)
summary(spend_gcm_tsq)

##a. Is the quadratic component significant? Let’s keep it anyway.
##No
##b. Create a plot showing the growth curves of each country and the average growth curve. 
##[You may want to have a look at the slides and my visualization of pigs’ growth curves].
garmitvoc$prediction<-predict(spend_gcm_tsq)
garmitvoc$aprediction<-predict(spend_gcm_tsq, re.form=NA)

ggplot(data = garmitvoc, aes(x=t, y=spend))+
        geom_line(data=garmitvoc, aes(x=t, y=prediction, group=country, color=country))+
        geom_line(data = garmitvoc, aes(x=t, y=aprediction), linewidth=2, color="black")
        

##Add the variable voc to your model. 
##Specify the model so that it tests whether there is a difference in the linear component of the growth curves (only interact t with voc, but not tsq).
##a. Is there a significant difference between liberal and coordinated market economies in their growth curves?
spend_gcm_voc<-lmer(formula = spend~1+t+tsq+voc+t:voc+
                            (1+t|country),
                    data = garmitvoc)

summary(spend_gcm_voc)

##b. Which economies start higher and which show stronger increases in spending over time?
##the coordinated is 5.769816 higher and show faster increases over time

##c. Create a plot to visualize the growth curves of each country but color them according to the value of voc.
garmitvoc$prediction2<-predict(spend_gcm_voc)
garmitvoc$aprediction2<-predict(spend_gcm_voc, re.form=NA)

ggplot(data=garmitvoc, aes(x=t, y=spend))+
        geom_line(data = garmitvoc, aes(x=t, y=prediction2, group=country, color=voc))+
        xlab("Year(1961=0)")+ylab("Public Spending(% of GDP)")+
        labs(caption = "Dataset:garmit")
