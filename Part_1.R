# ------------------------------------------------------------
# Part 1 - Mastering using R Programming
# Kamal Mishra
# Last Updated - Dec/2015
# ------------------------------------------------------------


# 1. Univariate analysis ----

# Get "Prestige" dataset from "car" package
install.packages("car",repo = "http://cran.rstudio.com")
library(car)
library(carData)
data(Prestige)
head(Prestige)

# It's always better to use na.rm=TRUE in case there are any NA's in the dataset
mean(Prestige$income)
mean(Prestige$income,na.rm = T)

# You can get mean, median, sd, variance, quantile etc all of these one single command using "summary"
# summary of dataset with all statistical parameters
summary(Prestige)

# Another library from which you can use "describe" and get similar information
library(Hmisc)
describe(Prestige)

# Another package example - here we can get coefficient of var and CI at 95% and so on...
install.packages("pastecs")
pastecs::stat.desc(Prestige)

# Checkpoint - How to get 95% of "Price" in Cars93 dataset
#              and how to get coeff. of var. for that
library(MASS)
data("Cars93")
head(Cars93)
pastecs::stat.desc(Cars93$Price) # You can get coef.var from here as well directly, else do sd / mean computation
sd(Cars93$Price) / mean(Cars93$Price)
quantile(Cars93$Price,probs = 0.95) # This will give you 95 percentile...

# 2. Bivariate analysis - Correlation, Chi-square, ANOVA ----

# Case 1: Continuous vs Continuous
# Correlation values: -1 to +1

# high correlation shows there is a strong relationship between income and education
cor(Prestige$income, Prestige$education) 

plot(x=Prestige$education, y=Prestige$income,
     main="Income vs Education - in Prestige dataset",
     ylim=c(0,10000))
abline(lm(income ~ education, data = Prestige))

# cor.test function demonstrates statistical significance
# Here since p-value is much less than 0.05 (or 5%), we can reject null hypothesis
# i.e. we can conclude that true correlation is not equal to zero
# therefore the relationship is statistically significant
cor.test(Prestige$income, Prestige$education)

# Case 2: Continuous vs Categorical
# Here Null Hypothesis: All population means are equal
#      Alternate Hypothesis: At least one population mean is different from the rest
# F-ratio = (Mean between groups sum of squares)/(Mean within groups sum of squares)
# Great F-ratio -> stronger relationship between x and y

boxplot(income ~ type, data = Prestige,
        main = "Income vs Occupation Type",
        ylab = "Income")
anovamod <- aov(income ~ type, data = Prestige)
summary(anovamod)

# higher F-ratio will have a lower p-value -> will also indicate stronger relationship
# Mean Sq (type) / Mean Sq (residuals) = F-value


# Case 3: Categorical vs Categorical
# Then use chi-square test
# Higher the chi-square char. -> the stronger the relationship

Prestige$income_cat <- dplyr::ntile(Prestige$income, 4)
table(Prestige$income_cat,Prestige$type)
head(Prestige)
chisq.test(y=Prestige$income_cat,Prestige$type)




# 3. Detecting Outliers ----

# Those lie 1.5 times quantile - important to exclude outliers for fitting the model effectively
# hence relevant and important

# Let's take the example of Income
varIncome <- Prestige$income
# get 3 times IQR
iqr <- IQR(varIncome) 
varIncome[varIncome > (3 * iqr)]
# outside 1.5 times the 75% quantile
third_quantile <- quantile(varIncome,0.75)
varIncome[varIncome > (1.5 * third_quantile)]

# capping - cap the outliers, replace values with respective percentile value itself
varIncome[varIncome > quantile(varIncome, 0.95)] <- quantile(varIncome, 0.95)
varIncome[varIncome < quantile(varIncome, 0.05)] <- quantile(varIncome, 0.05)


# 4. Missing values treatment ----
# Two types of missing values
# a) Missing completely at random
# b) Missing not at random

# - If large no of observations, then consider deleting obs. with missing values
# - If prticular variable is the major contributor of missing values in dataset, then consider removing the variable
# you can impute missing value of continuous variable with mean/median
# you can impute missing value of categorical variable with mode


# Ends
# ------------------------------------------------------------
