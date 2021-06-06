# Creation of the linear models and testing of assumptions (model diagnostics).

# Load required packages
library(lmtest)
library(readxl)
library(kableExtra)
library(car)
library(nortest)
library(MASS)
library(broom)
library(stargazer)

# Path to dataLemon file - insert your own
# exportdirxls <- "D:\\MPI_LEMON\\EEG_MPILMBB_LEMON\\EEG_Statistics\\DataLemon.xlsx" # 1 - 44 Hz
exportdirxls <- "D:\\MPI_LEMON\\EEG_MPILMBB_LEMON\\DataLemon.xlsx" # Old ones from git

# Import data
Data <- data.frame(read_excel(exportdirxls, 1, col_names = TRUE))
row.names(Data) <- Data[,1]
Data <- Data[,-1]

Data[,19] <- factor(Data[,19]) 
Data[,20] <- factor(Data[,20])
Data[,21] <- factor(Data[,21])

# Create separate datasets for each model, without missing (n=3) values
Data <- na.omit(Data)
N = nrow(Data)
dataF2F1 <- Data[,-c(2:4)]
dataF4F3 <- Data[,-c(1,3,4)]
dataF6F5 <- Data[,-c(1,2,4)]
dataF8F7 <- Data[,-c(1:3)]
# Create four OLS models
full.modelF2F1 <- lm(FAA.F2F1 ~ ., data = dataF2F1)
full.modelF4F3 <- lm(FAA.F4F3 ~ ., data = dataF4F3)
full.modelF6F5 <- lm(FAA.F6F5 ~ ., data = dataF6F5)
full.modelF8F7 <- lm(FAA.F8F7 ~ ., data = dataF8F7)


## **Checking of Assumptions for Ordinary Least Squares (OLS) Regression**

#-----------------------------(F2-F1)-------------------------------------------

# The **diagnostic plots** show residuals in four different ways
# (http://www.sthda.com/english/articles/39-regression-model-diagnostics/161-linear-regression-assumptions-and-diagnostics-in-r-essentials/)):
# 1. Residuals vs Fitted. The predicted Y-values ($\hat{y}$) are on the X-axis, and the residuals (*e*) are
# on the Y-axis. Used to check the linear relationship assumptions. A horizontal line, without distinct
# patterns is an indication for a linear relationship.
# 2. Normal Q-Q. The ordered observed standardised residuals are on the Y-axis, and the ordered theoretical
# residuals (expected residuals if they are truly normally distributed) are on the X-axis. Used to examine
# whether the residuals are normally distributed. It’s good if residuals points follow the straight dashed
# line.
# 3. Scale-Location (or Spread-Location). Used to check the homogeneity of variance of the residuals
# (homoscedasticity). Horizontal line with equally spread points is a good indication of homoscedasticity.
# 4. Residuals vs Leverage. Used to identify influential cases, that is extreme values that might influence
# the regression results when included or excluded from the analysis. This plot will be described further
# in the next sections.
# The four plots show the top 3 most extreme data points labeled with the row numbers of the data in the
# data set. They might be potentially problematic.

par(mfrow = c(2,2))
plot(full.modelF2F1, las = 1)
par(mfrow=c(1,1))

### 1. Unusual Observations
# Unusual observations may disproportionately influence the outcomes of the models.
# OLS regression is not very robust against these types of observations.
# In  the  absence  of  outliers  and  with  the  fulfillment  of  the  assumptions  of  zero mean,
# constant variance, and uncorrelated errors the OLS provides the Best Linear Unbiased Estimators (BLUE)
# of the regression parameters. But any anomalous point can disproportionately pull the line and distort
# the predictions. Detection of outlying observations is a very  essential  part  of  good  regression 
# analysis. I will not remove outliers unless they are obvious errors (e.g., 44 on a 1 - 5 scale).

# An influential observation is one which either individually or together with several other observations
# has a demonstrably larger impact on the calculated values of various estimates (e.g., coefficient,
# standard errors, t-values) than to the case for most of the other observation (Belsley et al., 1980).

#### 1.1 Outliers
# An outlier is an observation with a large (absolute) residual. Outliers can be identified by examining
# the standardized residual, which is the residual divided by its estimated standard error. Standardized
# residuals can be interpreted as the number of standard errors away from the regression line. Observations
# whose standardized residuals are greater than 3 in absolute value are possible outliers
# (James et al. 2014). It would be expected that only 0.2% of observations would fall into this category.
d <- cooks.distance(full.modelF2F1) # COOK'S DISTANCE
r <- stdres(full.modelF2F1) # STANDARDIZED RESIDUALS
rabs <- abs(r) # absolute stand. resid.
a <- cbind(dataF2F1[,-c(1:19)], d, r, rabs)
asorted <- a[order(-rabs), ]
absresids <- asorted[1:10, ]
kable(absresids, "html") %>% kable_styling("striped") %>% scroll_box(width = "100%")
# The table shows the 10 highest absolute values of the standardized residuals (rabs). Three such residuals
# are larger than 3, subject 261, 246, and 191. On of these subjects, sub-261, is identified as an outlier
# by an outlier test below.

# TEST FOR OUTLIERS. Bonferroni outlier test
outlierTest(full.modelF2F1) # sub-010261


#### 1.2 Influential Observations
# Cook's Distance (d). Cut-off points of *4/n*. 
# Cook's distance refers to how far, on average, predicted y-values will move if the observation in
# question is dropped from the data set. It is a measure that combines the information of leverage and the
# residual of the observation. 14 observations have values of Cook's D above 4/N = 0.019.
kable(a[d > 4/N, ], "html") %>% kable_styling("striped") %>% scroll_box(width = "100%")

#### 1.3 High leverage observations
# Possible high leverage observations are shown in the **Residuals vs Leverage plot**:
plot(full.modelF2F1, 5)
# A data point has high leverage if it has extreme predictor x values. This can be detected by examining
# the leverage statistic, or the hat-values, which are the diagonal elements of the hat-matrix and which
# describe the leverage each point has on its fitted values. A value of this statistic above 2(p + 1)/n
# indicates an observation with high leverage (P. Bruce and Bruce 2017); where, p is the number of
# predictors and n is the number of observations. Three observations are identified as having high
# leverage (.hat > 2(p+1)/n)

# Create table of model metrics and identify observations with high leverage
model.metrics.F2F1 <- data.frame(augment(full.modelF2F1))
row.names(model.metrics.F2F1) <- model.metrics.F2F1[,1]
model.metrics.F2F1 <- model.metrics.F2F1[, -c(1:19)]
p <- ncol(dataF2F1)
n <- nrow(dataF2F1)
hlt.F2F1 <- 2 * (p + 1) / n # high-leverage threshold
model.metrics.F2F1[model.metrics.F2F1$.hat > hlt.F2F1,]


### 2. Residuals are Normaly Distributed

# The distribution of the residuals can be visualized with a histogram. It appears slightly skewed.
resid.modelF2F1 <- residuals(full.modelF2F1)
hist(resid.modelF2F1)

# The **Q-Q plot** of residuals can be used to inspect the normality assumption. Some observations fall
# outside of the reference line.
plot(full.modelF2F1, 2)

# Due to the large sample size, a normality test should be able to provide more information. The
# *Shaprio-Wilks test* indicate that residuals are not normally distributed. The *Anderson-Darling* test,
# which is generally considered better than the Shaprio-Wilks, does not reject the null hypothesis of
# normally distributed residuals for the model, but with p = 0.052.
shapiro.test(resid.modelF2F1)
ad.test(resid.modelF2F1)

# The central limit theorem will make the coefficients asymptotically normal, even if the errors are not.
# However, the speed of this asymptotic convergence greatly depends on how non-normal the distribution
# of the errors is. The histogram and the Q-Q plot both indicate that there is no large deviation from the
# assumption of normality, rather a small amount of outliers that appear to be causing some problems.
# Non-severe departures from normality yield valid (asymptotic) inference for relatively large sample
# sizes, which is one reason why this assumption is generally considered less problematic than the other
# assumptions. See e.g., https://doi.org/10.1146/annurev.publhealth.23.100901.140546.

### 3. Homoscedasticity
# This assumption states that the variance of error terms are similar across the values of the independent
# variables. A **scale-location** plot of standardized residuals versus predicted values can show whether
# points are equally distributed across all values of the independent variables.
# The red line is fairly horizontal and there is no clear pattern, which is what we want.
plot(full.modelF2F1, 3)

#### 3.1 Studentized Breusch-Pagan (BP) test for heteroscedasticity.
# The Breush-Pagan test can be used to test for heteroscedasticity. It tests whether the variance of the
# errors from a regression is dependent on the values of the independent variables. In that case,
# heteroscedasticity is present. Similar to the scale-location plot, It does not indicate heteroscedasticity. 
bp <- bptest(full.modelF2F1, studentize = TRUE) # looks good
bp

### 4. Linear Relationship Between the Dependent Variable (FAA) and the Independent Variables

#### 4.1 Residuals vs fitted plot
# There should be no pattern in the **residual plot**. Ideally, the red line should be approximately flat
# and the points scattered completely random. This line indicates a slight negative slope, but no clear
# non-linear pattern is visible.
plot(full.modelF2F1, 1)

### 5. No Multicollinearity
# The correlation matrices showed low correlations between the independent variables. Variance Inflation
# Factor (VIF) test shows the same, no collinearity (< 2.5 is very low).
# VARIANCE INFLATION FACTOR (VIF)
car::vif(full.modelF2F1)

# For model F2-F1, the biggest problem is the possible violation of the assumption of normality of
# residuals, although this was only indicated by one of the two tests. This is the least severe assumption
# to violate, as the linear model will still be the BLUE (if other assumptions hold), but inference will
# be incorrect due to biased standard errors. As mentioned above, the central limit theorem will make the
# coefficients asymptotically normal (https://doi.org/10.1146/annurev.publhealth.23.100901.140546).

#----------------------------------(F4-F3)--------------------------------------

par(mfrow = c(2,2))
plot(full.modelF4F3, las = 1)

### 1. Unusual Observations
d <- cooks.distance(full.modelF4F3) # COOK'S DISTANCE
r <- stdres(full.modelF4F3) # STANDARDIZED RESIDUALS
rabs <- abs(r) # absolute stand. resid.
a <- cbind(dataF4F3[,-c(1:19)], d, r, rabs)
asorted <- a[order(-rabs), ]
absresids <- asorted[1:10, ]
kable(absresids, "html") %>% kable_styling("striped") %>% scroll_box(width = "100%")

# The table shows the 10 highest absolute values of the standardized residuals rabs. Only one such
# residual is larger than 3, which is the one for subject 166. The same subject is identified as an
# outlier by an outlier test below.
outlierTest(full.modelF4F3)

#### 1.2 Influential Observations
# Cook's Distance. Cut-off points of *4/n*
kable(a[d > 4/N, ], "html") %>% kable_styling("striped") %>% scroll_box(width = "100%")
# 15 observations have values of Cook's D above 4/N = 0.019.

#### 1.3 High leverage observations
plot(full.modelF4F3, 5)

# Create table of model metrics and identify observations with high leverage
# Three observations are identified as having high leverage (.hat > 2(p+1)/n)
model.metrics.F4F3 <- data.frame(augment(full.modelF4F3))
row.names(model.metrics.F4F3) <- model.metrics.F4F3[,1]
model.metrics.F4F3 <- model.metrics.F4F3[, -c(1:19)]
p <- ncol(dataF4F3)
n <- nrow(dataF4F3)
hlt.F4F3 <- 2 * (p + 1) / n # high-leverage threshold
model.metrics.F4F3[model.metrics.F4F3$.hat > hlt.F4F3,]

### 2. Residuals are Normaly Distributed
resid.modelF4F3 <- residuals(full.modelF4F3)
hist(resid.modelF4F3) # looks good

# The **Q-Q plot** of residuals can be used to inspect the normality assumption. Some observations fall
# outside of the reference line.
plot(full.modelF4F3, 2)

# Due to the large sample size, a normality test should be able to provide more information. Neither the
# *Shaprio-Wilks* nor the *Anderson-Darling* test reject the null hypothesis of normally distributed
# residuals for the model.
shapiro.test(resid.modelF4F3)
ad.test(resid.modelF4F3)

### 3. Homoscedasticity
# The plot below shows that the variances of the residuals are not constant (heteroscedasticity). The red
# line, rather than being horizontal, has a slight inverse U-shaped form, indicating that the variance of
# the residuals are not constant.
plot(full.modelF4F3, 3)

#### 3.1 Studentized Breusch-Pagan test for heteroscedasticity.
# The Breush-Pagan test and can be used to test for heteroscedasticity. It does not indicate
# heteroscedasticity. However, these tests are not necessarily better than inspecting the plots, so there
# is cause for some concern regarding this assumption.
bp <- bptest(full.modelF4F3, studentize = TRUE)
bp # looks good

### 4. Linear Relationship Between the Dependent Variable (FAA) and the Independent Variables
#### 4.1 Residuals vs fitted plot
# This line indicates a slight negative slope, but no clear non-linear pattern is visible.
plot(full.modelF4F3, 1)

### 5. No Multicollinearity
# VARIANCE INFLATION FACTOR (VIF)
car::vif(full.modelF4F3) # Looks good

# The biggest issue, despite the BP test, is the assumption of homoscedasticity. Violating this assumption 
# can result in p-values for hypothesis tests and confidence intervals not performing as they should,
# because they use the estimated variance. Luckily, the PB test did not indicate heteroscedasticity. 

#-----------------------------(F6-F5)-------------------------------------------

par(mfrow = c(2,2))
plot(full.modelF6F5, las = 1)
par(mfrow=c(1,1))

### 1. Unusual Observations
#### 1.1 Outliers
d <- cooks.distance(full.modelF6F5) # COOK'S DISTANCE
r <- stdres(full.modelF6F5) # STANDARDIZED RESIDUALS
rabs <- abs(r) # absolute stand. resid.
a <- cbind(dataF6F5[,-c(1:19)], d, r, rabs)
asorted <- a[order(-rabs), ]
absresids <- asorted[1:10, ]
kable(absresids, "html") %>% kable_styling("striped") %>% scroll_box(width = "100%")
# The table shows the 10 highest absolute values of the standardized residuals (rabs). Two such residuals
# are larger than 3, subjects 91 and 83. On of these subjects, sub-91, is identified as an outlier by an
# outlier test below.
outlierTest(full.modelF6F5)

#### 1.2 Influential Observations
# Cook's Distance. Cut-off points of *4/n*
kable(a[d > 4/N, ], "html") %>% kable_styling("striped") %>% scroll_box(width = "100%")
# 14 observations have values of Cook's D above 4/N = 0.019.

#### 1.3 High leverage observations
plot(full.modelF6F5, 5)

# Create table of model metrics and identify observations with high leverage
model.metrics.F6F5 <- data.frame(augment(full.modelF6F5))
row.names(model.metrics.F6F5) <- model.metrics.F6F5[,1]
model.metrics.F6F5 <- model.metrics.F6F5[, -c(1:19)]
p <- ncol(dataF6F5)
n <- nrow(dataF6F5)
hlt.F6F5<- 2 * (p + 1) / n # high-leverage threshold
model.metrics.F6F5[model.metrics.F6F5$.hat > hlt.F6F5,]
# Three observations are identified as having high leverage (.hat > 2(p+1)/n)

### 2. Residuals are Normaly Distributed
resid.modelF6F5 <- residuals(full.modelF6F5)
hist(resid.modelF6F5) # appears relatively normal.

# Q-Q plot
plot(full.modelF6F5, 2) # Some observations fall outside of the reference line.

# Due to the large sample size, a normality test should be able to provide more information. Neither the
# *Shaprio-Wilks* nor the *Anderson-Darling* test reject the null hypothesis of normally distributed
# residuals for the model.

shapiro.test(resid.modelF6F5)
ad.test(resid.modelF6F5)

### 3. Homoscedasticity
# The red line is not flat, but rather appears to be slanting upwards
plot(full.modelF6F5, 3)

#### 3.1 Studentized Breusch-Pagan test for heteroscedasticity.
# BP does not indicate heteroscedasticity. However, these tests are not necessarily better than inspecting
# the plots, so there is cause for some concern regarding this assumption.

bp <- bptest(full.modelF6F5, studentize = TRUE) # looks good
bp

### 4. Linear Relationship Between the Dependent Variable (FAA) and the Independent Variables
# This line indicates a slight negative slope towards the middle, but there is no clear non-linear pattern
plot(full.modelF6F5, 1)

### 5. No Multicollinearity
# VARIANCE INFLATION FACTOR (VIF)
car::vif(full.modelF6F5) # looks good

# Similar to the previous model, the biggest issue, despite the BP test, is the assumption of
# homoscedasticity. Violating this assumption can result in p-values for hypothesis tests and confidence
# intervals not performing as they should, because they use the estimated variance. Luckily, the PB test
# did not indicate heteroscedasticity. 

#-----------------------------(F8-F7)-------------------------------------------

par(mfrow = c(2,2))
plot(full.modelF8F7, las = 1)
par(mfrow=c(1,1))

### 1. Unusual Observations
#### 1.1 Outliers
d <- cooks.distance(full.modelF8F7) # COOK'S DISTANCE
r <- stdres(full.modelF8F7) # STANDARDIZED RESIDUALS
rabs <- abs(r) # absolute stand. resid.
a <- cbind(dataF8F7[,-c(1:19)], d, r, rabs)
asorted <- a[order(-rabs), ]
absresids <- asorted[1:10, ]
kable(absresids, "html") %>% kable_styling("striped") %>% scroll_box(width = "100%")
# The table shows the 10 highest absolute values of the standardized residuals (rabs). One residual is
# larger than 3, subject 247. The same subject is identified as an outlier by an outlier test below.
outlierTest(full.modelF8F7)

#### 1.2 Influential Observations
# Cook's Distance. Cut-off points of *4/n*
kable(a[d > 4/N, ], "html") %>% kable_styling("striped") %>% scroll_box(width = "100%")
# 18 observations have values of Cook's D above 4/N = 0.019.

#### 1.3 High leverage observations
plot(full.modelF8F7, 5)
# Three observations are identified as having high leverage (.hat > 2(p+1)/n)

# Create table of model metrics and identify observations with high leverage
model.metrics.F8F7 <- data.frame(augment(full.modelF8F7))
row.names(model.metrics.F8F7) <- model.metrics.F8F7[,1]
model.metrics.F8F7 <- model.metrics.F8F7[, -c(1:19)]
p <- ncol(dataF8F7)
n <- nrow(dataF8F7)
hlt.F8F7<- 2 * (p + 1) / n # high-leverage threshold
model.metrics.F8F7[model.metrics.F8F7$.hat > hlt.F8F7,]

### 2. Residuals are Normaly Distributed
# The distribution of the residuals can be visualized with a histogram. It appears relatively normal,
# with one, or a few, outliers on the right tail.
resid.modelF8F7 <- residuals(full.modelF8F7)
hist(resid.modelF8F7)

# Q-Q plot. Some observations fall outside of the reference line.
plot(full.modelF8F7, 2)

# Due to the large sample size, a normality test should be able to provide more information. Neither the
# *Shaprio-Wilks* nor the *Anderson-Darling* test reject the null hypothesis of normally distributed
# residuals for the model.
shapiro.test(resid.modelF8F7)
ad.test(resid.modelF8F7)

### 3. Homoscedasticity
# Scale-location plot. The red line appears to be slanting first downwards and then upwards.
plot(full.modelF8F7, 3)

#### 3.1 Studentized Breusch-Pagan test for heteroscedasticity.
# The Breush-Pagan test and can be used to test for heteroscedasticity. It does not indicate
# heteroscedasticity. However, these tests are not necessarily better than inspecting the plots, so there
# is a slight cause for concern regarding this assumption.
bp <- bptest(full.modelF8F7, studentize = TRUE) # looks good
bp

### 4. Linear Relationship Between the Dependent Variable (FAA) and the Independent Variables
# Residuals vs fitted plot. This line indicates a negative slope for larger fitted values.
plot(full.modelF8F7, 1)

### 5. No Multicollinearity
# VARIANCE INFLATION FACTOR (VIF)
car::vif(full.modelF8F7) # Looks good

# Similar to the previous model, the biggest issue, despite the BP test, is the assumption of
# homoscedasticity. Violating this assumption can result in p-values for hypothesis tests and confidence
# intervals not performing as they should, because they use the estimated variance. Luckily, the PB test
# did not indicate heteroscedasticity. 

# Overall, there are no major departures from the assumptions. We also have a fairly large sample (N>200),
# and N is large enough, the sampling distribution will be approximately normal because of the Central
# Limit Theorem (CLT). Furthermore, I wish to stay within the linear model framework, as opposed to e.g.,
# transforming certain variables, as this is both the most common practice in behavioural sciences and
# provide easily interpretable results. This is important since the aim of this analysis is to explain the
# relationship between FAA and various personality variables, as opposed to predict prediction
# (see e.g., doi.org/10.1214/10-STS330).

# At the end, under EXTRA, I also estimate standard errors of the coefficients and confidence intervals
# empirically with non-parametric bootstrap, as a way to deal with possible bias in these measures. This
# relaxes the assumptions of residuals being a certain distribution (i.e., normal). See e.g., Pek et al.,
# 2018 and https://doi.org/10.1016/j.brat.2017.05.013 for more information. I do this simply for the
# purpose of comparison. If these bootstrapped estimates deviate notably from the once derived by the OLS
# models above, there could be some problems with my models. These estimates are not reported in the thesis. 

#------------------------------------Linear Models inference--------------------

# Let's first take a look at the four models. 
stargazer(full.modelF2F1, full.modelF4F3, full.modelF6F5, full.modelF8F7,
          title="Linear Models",
          dep.var.caption = "Frontal Alpha Asymmetry",
          notes.label = "Significance levels",
          type="text")
# Several independent variables appear significant in the different models. However, I have not yet
# corrected for multiple comparisons, as we just conducted 20 t-tests for every model. FDR is chosen to
# not be as conservative as for example Bonferroni.

# FDR adjust p-values
# Model 1: F2-F1
pvals.F2F1 <- coef(summary(full.modelF2F1))[,4 ]# storing p-values
adj.P <- p.adjust(pvals.F2F1, method = "fdr") # adjusted p-values
sumF2F1 <- cbind(coef(summary(full.modelF2F1)), adj.P) # add to summary
stargazer(sumF2F1,
          title="Linear Model 1: FAA F2-F1",
          dep.var.caption = "Frontal Alpha Asymmetry",
          notes.label = "Significance levels",
          type="text") # Visualize model F2-F1


# Model 2: F4-F3
pvals.F4F3 <- coef(summary(full.modelF4F3))[,4 ]# storing p-values
adj.P <- p.adjust(pvals.F4F3, method = "fdr") # adjusted p-values
sumF4F3 <- cbind(coef(summary(full.modelF4F3)), adj.P) # add to summary
stargazer(sumF4F3,
          title="Linear Model 1: FAA F4-F3",
          dep.var.caption = "Frontal Alpha Asymmetry",
          notes.label = "Significance levels",
          type="text") # Visualize model F4-F3


# Model 3: F6-F5
pvals.F6F5 <- coef(summary(full.modelF6F5))[,4 ]# storing p-values
adj.P <- p.adjust(pvals.F6F5, method = "fdr") # adjusted p-values
sumF6F5 <- cbind(coef(summary(full.modelF6F5)), adj.P) # add to summary
stargazer(sumF6F5,
          title="Linear Model 1: FAA F6-F5",
          dep.var.caption = "Frontal Alpha Asymmetry",
          notes.label = "Significance levels",
          type="text") # Visualize model F6-F5


# Model 4: F8-F7
pvals.F8F7 <- coef(summary(full.modelF8F7))[,4 ]# storing p-values
adj.P <- p.adjust(pvals.F8F7, method = "fdr") # adjusted p-values
sumF8F7 <- cbind(coef(summary(full.modelF8F7)), adj.P) # add to summary
stargazer(sumF8F7,
          title="Linear Model 1: FAA F8-F7",
          dep.var.caption = "Frontal Alpha Asymmetry",
          notes.label = "Significance levels",
          type="text") # Visualize model F8-F7

#-------------------------------EXTRA-------------------------------------------
# Bootstrap.
library(boot)
# Bootstrapping estimates (std. errors (SE) and confidence intervals (CI)) F2-F1

boot_reg <- function (formula,data,i){
    d <- dataF2F1[i,]
    fit <- lm(FAA.F2F1 ~ ., data = dataF2F1[i,]) # resample rows
    return(coef(fit))
}
set.seed(321) # for reproducibility
bs1 <- boot(data=dataF2F1, statistic=boot_reg, R=5000, formula = FAA.F2F1 ~ .) # bootstrap linear model
b.ciF2F1 <- data.frame(matrix(0,20,2)) # matrix of 0's for storing CIs
for(i in 1:20){ # save all CIs into matrix
    b.ciF2F1[i,1:2] <- boot.ci(bs1, index = i, type = "perc")$percent[,4:5]
}

# Merge the bootstrapped std. errors of the beta coefficients with those estimated with the OLS model.
# Also merging the bootstrapped CIs and regular CIs. Round to two decimals.
se.mergeF2F1 <- round(data.frame(cbind(coef(summary(full.modelF2F1))[,2],
                             summary(bs1)[,4]), confint(full.modelF2F1),b.ciF2F1),2)
colnames(se.mergeF2F1) <- c("Std. Error", "Std. Error Boot", "2.5 %", "97.5 %", "2.5 % Boot", "97.5 % Boot")

# Overall, the bootstrapped estimates (SE and CI) look very similar to those used in the linear model. 

#-------------------------------------------------------------------------------
# Bootstrapping estimates (std. errors (SE) and confidence intervals (CI)) F4-F3

boot_reg <- function (formula,data,i){
    d <- dataF4F3[i,]
    fit <- lm(FAA.F4F3 ~ ., data = dataF4F3[i,]) # resample rows
    return(coef(fit))
}
set.seed(321) # for reproducibility
bs2 <- boot(data=dataF4F3, statistic=boot_reg, R=5000, formula = FAA.F4F3 ~ .) # bootstrap linear model
b.ciF4F3 <- data.frame(matrix(0,20,2)) # matrix of 0's for storing CIs
for(i in 1:20){ # save all CIs into matrix
    b.ciF4F3[i,1:2] <- boot.ci(bs2, index = i, type = "perc")$percent[,4:5]
}

# Merge the bootstrapped std. errors of the beta coefficients with those estimated with the OLS model.
# Also merging the bootstrapped CIs and regular CIs. Round to 2 decimals.
se.mergeF4F3 <- round(data.frame(cbind(coef(summary(full.modelF4F3))[,2],
                                   summary(bs2)[,4]), confint(full.modelF4F3),b.ciF4F3),2)
colnames(se.mergeF4F3) <- c("Std. Error", "Std. Error Boot", "2.5 %", "97.5 %", "2.5 % Boot", "97.5 % Boot")

# Overall, the bootstrapped estimates (SE and CI) look very similar to those used in the linear model. 


#-------------------------------------------------------------------------------
# Bootstrapping estimates (std. errors (SE) and confidence intervals (CI)) F6-F5

boot_reg <- function (formula,data,i){
    d <- dataF6F5[i,]
    fit <- lm(FAA.F6F5 ~ ., data = dataF6F5[i,]) # resample rows
    return(coef(fit))
}
set.seed(321) # for reproducibility
bs3 <- boot(data=dataF6F5, statistic=boot_reg, R=5000, formula = FAA.F6F5 ~ .) # bootstrap linear model
b.ciF6F5 <- data.frame(matrix(0,20,2)) # matrix of 0's for storing CIs
for(i in 1:20){ # save all CIs into matrix
    b.ciF6F5[i,1:2] <- boot.ci(bs3, index = i, type = "perc")$percent[,4:5]
}

# Merge the bootstrapped std. errors of the beta coefficients with those estimated with the OLS model.
# Also merging the bootstrapped CIs and regular CIs. Round to 2 decimals.
se.mergeF6F5 <- round(data.frame(cbind(coef(summary(full.modelF6F5))[,2],
                                       summary(bs3)[,4]), confint(full.modelF6F5),b.ciF6F5),2)
colnames(se.mergeF6F5) <- c("Std. Error", "Std. Error Boot", "2.5 %", "97.5 %", "2.5 % Boot", "97.5 % Boot")

# Overall, the bootstrapped estimates (SE and CI) look very similar to those used in the linear model. 


#-------------------------------------------------------------------------------
# Bootstrapping estimates (std. errors (SE) and confidence intervals (CI)) F8-F7

boot_reg <- function (formula,data,i){
    d <- dataF8F7[i,]
    fit <- lm(FAA.F8F7 ~ ., data = dataF8F7[i,]) # resample rows
    return(coef(fit))
}
set.seed(321) # for reproducibility
bs4 <- boot(data=dataF8F7, statistic=boot_reg, R=5000, formula = FAA.F8F7 ~ .) # bootstrap linear model
b.ciF8F7 <- data.frame(matrix(0,20,2)) # matrix of 0's for storing CIs
for(i in 1:20){ # save all CIs into matrix
    b.ciF8F7[i,1:2] <- boot.ci(bs4, index = i, type = "perc")$percent[,4:5]
}

# Merge the bootstrapped std. errors of the beta coefficients with those estimated with the OLS model.
# Also merging the bootstrapped CIs and regular CIs. Round to 2 decimals.
se.mergeF8F7 <- round(data.frame(cbind(coef(summary(full.modelF8F7))[,2],
                                       summary(bs3)[,4]), confint(full.modelF6F5),b.ciF6F5),2)
colnames(se.mergeF8F7) <- c("Std. Error", "Std. Error Boot", "2.5 %", "97.5 %", "2.5 % Boot", "97.5 % Boot")

# Overall, the bootstrapped estimates (SE and CI) look very similar to those used in the linear model, 
# although there is an important difference between the bootstrapped CIs and the real CIs when it comes to
# SKID.Diagnosespast for this model. Unfortunately, I have not had time to learn how to incorporate
# multiple testing into the bootstrapped estimates, so I can not say for sure whether this effect would
# have survived multiple testing.



### References:
# Belsley, D.A., Kuh. E and Welsch, R.E., Regression Diagnostics: Identifying Influential Data and Sources
#   of Collinearity, Wiley, New York, (1980).
# Bruce, Peter, and Andrew Bruce. 2017. Practical Statistics for Data Scientists. O’Reilly Media.
# James, Gareth, Daniela Witten, Trevor Hastie, and Robert Tibshirani. 2014. An Introduction to Statistical
#   Learning: With Applications in R. Springer Publishing Company, Incorporated.
# Pek et al., 2018: https://doi.org/10.3389/fpsyg.2018.02104

# END OF SCRIPT
