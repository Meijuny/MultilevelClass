###Exercise 3.1: 
###Read the data in:
load(file="./Data/RData/Exercise3/spend_aus.Rdata")
load(file="./Data/RData/Exercise3/spend_dk.Rdata")
load(file="./Data/RData/Exercise3/spend_fr.Rdata")
load(file="./Data/RData/Exercise3/spend_ger.Rdata")
load(file="./Data/RData/Exercise3/spend_us.Rdata")
View(spend_aus)

##combine everything in one data frame:
spend_5countries<-rbind(spend_aus, spend_dk,
                        spend_fr, spend_ger, spend_us)
table(spend_5countries$year)

##Use ggplot to make the graphs:
ggplot(data = spend_5countries, aes(x=year, y=spend, group=country, color=country))+
        geom_line()+
        geom_point()+labs(title = "Public Spending, 1961-1993")+
        xlab("Year")+ylab("public spending (% of GDP)")+
        coord_cartesian(ylim = c(20,70),clip = "off")+
        annotate("text", x=1990, y=12, 
                 label="source: Data from Garret & Mitchell(2001)",size=2.5)


###-----------------------------------------------------------------------------------------------------
###Exercise 3.2:
###Read the data in:
load("./Data/RData/Exercise3/spend.Rdata")
load("./Data/RData/Exercise3/explanatory.Rdata")


View(spend)
table(spend$country)
View(explanatory)
table(explanatory$country)

##Make a new variable as primary key to merge in each dataset
spend$CountryYear<-paste(spend$country, spend$year,
                         sep = "-")
explanatory$CountryYear<-paste(explanatory$country, explanatory$year,
                               sep="-")

##Merge the two datasets:
ContryPoolDataTry<-merge(x=spend, y=explanatory,
                         by.x = c("country","year"),
                         by.y = c("country","year"))

CountryPoolData<-merge(x=spend, y=explanatory,
                       by.x = "CountryYear", by.y = "CountryYear",
                       all=TRUE)

CountryPoolData<-CountryPoolData %>%
        dplyr::select(-c("country.y","year.y"))

colnames(CountryPoolData)[2]<-"country"
colnames(CountryPoolData)[3]<-"year"

##Fit linear regression model:
Globa_Spending<-lm(formula = spend~1+lowwage+trade+unem+growthpc+depratio+left+cdem,
                   data = CountryPoolData)
summary(Globa_Spending)                          

##Trying with a multilevel model: Country <- Year
Global_Spending_MLM<-lmer(formula = spend~1+lowwage+trade+unem+growthpc+depratio+left+cdem+
                                  (1+lowwage+trade|country),
                          data = CountryPoolData)
summary(Global_Spending_MLM)
##low wage random effects: 
0.276/(0.525/(15)^(1/2))


###-----------------------------------------------------------------------------------------------------
###Exercise 3.3:
##Read the Stata data in:
ESS8<-read.dta("./Data/Stata/Exercise3/ESS8.dta")

##Check the data to answer the following questions:
##a. How many countries and how many individuals are in the dataset? How many variables are in the dataset?
dim(ESS8) ##9345 individuals and 8 variables
table(ESS8$Country) ##28 countries in total, but only 21 has valid observations

##b. What is the structure implied by the survey design?
##Hierarchichal: Country<-individuals

##c. What is the identifier we need in order to combine the data with the OECD data?
##Country

##Read the OECD data in:
OECD<-read.csv("./Data/Excel/Exercise3/OECDdata.csv", sep=";")

##a. How many countries are in the dataset? Are these identical to the countries in the ESS data? 
##How many variables are in the data?
table(OECD$cntry)  ##24, not identical to the ESS data
dim(OECD) ##6 variables


##b. Is there a hierarchical structure in this data? 
##No

##c. What is the identifier we need in order to combine it with the ESS data? 
##Do we need to rename it before we can bring together the two datasets?
##Identifier: country
##Not necessarily

##Q3: Construct a new variable “inflow of immigrants per 100 inhabitants” (inflow_foreig_perc).
OECD<-OECD %>%
        mutate(inflow_foreig_perc=(inflow_foreign/population)*100)

##Q4: Combine the two datasets using the appropriate procedure. 
##Include only countries that are included in the ESS data into the combined dataset.
OECD_ESS<-merge(x=ESS8, y=OECD, by.x = "Country", by.y="cntry", all=FALSE)

ols_antiImmi<-lm(formula = ais~lrscale+unemp_rate, data = OECD_ESS)
summary(ols_antiImmi)
ols_antiImmi_cluster<-lm.cluster(data = OECD_ESS,
                                 formula = ais~lrscale+unemp_rate,
                                 cluster = "Country")
summary(ols_antiImmi_cluster)


##Q5: Try to answer the following questions:
##a. How many countries, observations and variables does the combined data have? Is this what you expected?
table(OECD_ESS$Country) ##28 countries in total, but only 21 has valid observations
dim(OECD_ESS) ##9354 observations, 14 variables


##b. What is the structure of the combined working dataset?
##Country<-Individuals
        
##c. Look at the variables. At which levels are they measured?
##ESS variables - individual level: "ais", "res_mig", "res_educ", "lrscale", "gndr","agea"
##OECD variables - country level:  "inflow_foreign", "population", "gdppc", "Unem-rate","inflow_foreign_perc"

##Q6: Try to make a scatter plot (with a regression line) at the aggregate level of countries. 
##It should have mean anti-immigrant attitudes at the Y-axis and immigration inflow at the X-axis.

##First calculate the mean of anti-immigrant attitudes by country:
antiImmi_Country<-ESS8 %>%
        dplyr::group_by(Country) %>%
        dplyr::summarise(antiImmiMean=mean(ais,na.rm=TRUE))

antiImmi_Country_OECD<-merge(x=antiImmi_Country, y=OECD, 
                             by.x = "Country",
                             by.y="cntry",
                             all=F)

ggplot(data = antiImmi_Country_OECD, aes(x=inflow_foreig_perc, y=antiImmiMean))+
        geom_point()+
        geom_smooth(se=TRUE, method="lm")+
        xlab("Inflow of Immigrants(% of population)")+ylab("Anti-Immigrant Sentiment")


####--------------------------------------------------------------------------------------------
dim(OECD_ESS)

M0<-lmer(formula = ais~1+(1|Country),
         data = OECD_ESS)
summary(M0)

##ICC=0.1955/(0.1955+0.9492)=0.1707871

u0j<-ranef(M0)
dotplot(u0j)

