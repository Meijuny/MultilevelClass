##Read the data in:
load("D:/Sociology Master/KU Leuven/Multilevel Analysis/MultilevelClass/Data/RData/Exercise3/spend.Rdata")
load("D:/Sociology Master/KU Leuven/Multilevel Analysis/MultilevelClass/Data/RData/Exercise3/explanatory.Rdata")

##Q3:Combine the two data sets with the appropriate procedure.
Spend_Explan<-merge(spend, explanatory, 
                    by.x = c("country","year"),
                    by.y = c("country","year"))

##a. How many variables and how many observations does the data have? Is this what we would expect?
dim(Spend_Explan)
##495 obs, and 10 columns (8 vars.)

##Transform the data from long to wide format, using the package panelr and the command widen_panel.
##First declare it as a panel data:
Spend_Explan_long<-panel_data(Spend_Explan, id=country, wave=year)

##To make it wide:
Spend_Explan_wide<-widen_panel(Spend_Explan_long)

##a. How many variables and how many observations does the data have? Is this what we expect?

##33 years with 8 variables: 8*33=264
##plus the country identifier: 265
dim(Spend_Explan_wide)


##b. Create a new variable that gives the difference in spending from 1961 to 1991. What is the average increase over this time period?
##Subset the data to have the spend of 1961 and 1991
spend_1961_1991<-Spend_Explan_wide %>%
        dplyr::select(country, spend_1961, spend_1991)

spend_1961_1991<-spend_1961_1991 %>%
        mutate(SpendIncrease=spend_1991-spend_1961)

##Average increase:
mean(spend_1961_1991$SpendIncrease, na.rm=TRUE)
##19.57

##c. Create a correlation matrix of spend for all measures between 1961 and 1971. 
##How strong are correlations between consecutive measures of spending? 
##Are the correlations about equal in all consecutive years?

##Subset the data for spend between 1961 and 1971:
spend_1961_1971<-Spend_Explan_wide %>%
        dplyr::select(spend_1961, spend_1962, spend_1963, spend_1964, spend_1965,
                      spend_1966, spend_1967, spend_1968, spend_1969, spend_1970,
                      spend_1971)

##Create the Variance_cov matrix:
spend_1961_1971_cov<-cov(spend_1961_1971, use="pairwise.complete.obs")

##Turn into a correlation mattrix:
spend_1961_1971_cor<-cov2cor(spend_1961_1971_cov)

##plot the correlation matrix:
corrplot::corrplot(spend_1961_1971_cor,
                   is.corr = TRUE,
                   method="square",
                   type="lower",
                   addCoef.col="lightblue")

##Q5: Transform the data back to long format, using the command long_panel.
spend_explan_long2<-long_panel(Spend_Explan_wide, prefix="_",
                               begin = 1961, end=1993,
                               label_location = "end")

##a. Estimate an empty random intercept model, with spending as the dependent variable, 
##and calculate the ICC. What is the average correlation within countries?
spend_M0<-lmer(formula = spend~1+(1|country),
               data = spend_explan_long2)
summary(spend_M0)

##ICC
52.66/(52.66+63.51)
##45.33%

##NOT UNDERSTAND! What is the average correlation within countries?

##Q6: Taking a naive approach, we start with a standard OLS model. 
##Public spending is the dependent variable; the remaining seven variables are independent variables. 
##Cluster the standard errors using the command lm.cluster from the miceadds package.
library(miceadds)
Spend_OLS<-lm.cluster(formula = spend~lowwage+trade+unem+growthpc+depratio+left+cdem,
                      cluster = "country",
                      data = spend_explan_long2)
summary(Spend_OLS)

##a. What does the model suggest? 
##Is globalization (measured by lowwage and trade) increasing or decreasing public spending?
##low wage decreasing and trade increasing

##Q7:Estimate a Fixed Effects model using the LSDV approach.
spend_LSDV<-lm(spend~lowwage+trade+unem+growthpc+depratio+left+cdem+factor(country),
               data = spend_explan_long2)
summary(spend_LSDV)


##Estimate a Fixed Effects model using the plm package. Compare both models. 
##Did they yield the same results?
##declare it as a panel data:
spend_explan_panel<-pdata.frame(spend_explan_long2,
                                index = c("country","wave"),
                                drop.index = TRUE)

spend_fe<-plm(spend~lowwage+trade+unem+growthpc+depratio+left+cdem,
              data = spend_explan_panel,
              model = "within")

summary(spend_fe)


##a. What do the results suggest? 
##Are the results in line with the OLS estimates or does the FE model tell a different story?

##Finally, reproduce the model by applying the FE transformation to the variables and then estimating an OLS model.

##a. Check that you have reproduced the coefficients from the former FE models.