# EXPLORATORY DATA ANALYSIS. WHAT DO I HAVE? TESTING ASSUMPTIONS ETC

# LOAD REQUIRED PACKAGES
library(readxl)
library(ggpubr)
library(tidyverse)
library(caret)
library(leaps)
library(MASS)
library(foreign)
library(Hmisc)

# PATH TO DATA FILES
exportdirxls <- "D:\\MPI_LEMON\\EEG_MPILMBB_LEMON\\EEG_Statistics\\Data.xlsx"
uppsdir <- "D:/MPI_LEMON/Behavioural_Data_MPILMBB_LEMON/Emotion_and_Personality_Test_Battery_LEMON/UPPS.csv"
eegsubsdir <- "D:/MPI_LEMON/EEG_MPILMBB_LEMON/EEG_Statistics/Diagnostics.csv"

# IMPORT DATA
Data <- data.frame(read_excel(exportdirxls, 1, col_names = TRUE))
eegsubs <- Data$ID
Data <- Data[,1:11]
N = nrow(Data)

# -------------UPPS REMOVE LATER ONCE MATLAB IS DONE

upps <- read_csv(uppsdir)
upps <- upps[order(upps$ID),] # SORT IN ORDER OF SUBJECT ID
differ <- setdiff(upps$ID, eegsubs) # 17 SUBJECTS IDENTIFIED (3 REMOVED DURING PRE-PROCESSING)
setdiff(eegsubs, upps$ID) # NULL (0)
upps <- upps[- which(upps$ID %in% differ), ]
setdiff(upps$ID, eegsubs) # DIFFERENCE SHOULD NOW BE NULL
setdiff(eegsubs, upps$ID) # STILL NULL, AS IT SHOULD BE
upps <- upps[,-1]
data1 <- Data[,1:9]
data2 <- Data[,10:11]
Data <- cbind(data1, upps)
Data <- cbind(Data, data2)
colnames(Data)[10] <- "UPPS.Urgency"
colnames(Data)[11] <- "UPPS.Lack.Premed"
colnames(Data)[12] <- "UPPS.Lack.Persev"
colnames(Data)[13] <- "UPPS.Sens.Seek"
rm(data1, data2, eegsubs, differ, upps)

#---------------------------------
# RECODING OF GENDER AND AGE TO INDICATOR/DUMMY (0/1) VARIABLES
# THE INTERCEPT WILL BE THE ESTIMATED Y-VALUE FOR THE REFERENCE GROUP (AGE AND GENDER EQUAL TO 0)
for (i in 1:N) {
    if(Data[i,14] == 2){
        Data[i,14] <- 0
    }
}

for(j in 1:N) {
    if(Data[j,15] == "20-25"){
        Data[j, 15] <- 1
    } else if(Data[j,15] == "25-30"){
        Data[j,15] <- 1
    } else if(Data[j,15] == "30-35"){
        Data[j,15] <- 1
    } else {
        Data[j,15] <- 0
    }
}

Data[,14] <- factor(Data[,14]) # FOR GENDER, 1 = FEMALE, 0 = MALE
Data[,15] <- factor(Data[,15]) # FOR AGE, 1 = YOUNG, 0 = OLD
str(Data)

#---------------------------------
# CREATE SEPARATE DATASETS FOR EACH MODEL (EACH DEPENDENT VARIABLE)
dataF2F1 <- Data[,-c(7:9)]
dataF4F3 <- Data[,-c(6, 8, 9)]
dataF6F5 <- Data[,-c(6,7, 9)]
dataF8F7 <- Data[,-c(6:8)]
#---------------------------------

# RUN FOUR OLS REGRESSION MODELS, ONE FOR EACH ELECTRODE PAIR
summary(full.modelF2F1 <- lm(FAA.F2F1 ~ BAS.Drive+BAS.Fun+BAS.Reward+BIS+UPPS.Urgency+UPPS.Lack.Premed+UPPS.Lack.Persev+UPPS.Sens.Seek+Gender+Age, data = dataF2F1))
summary(full.modelF4F3 <- lm(FAA.F4F3 ~ BAS.Drive+BAS.Fun+BAS.Reward+BIS+UPPS.Urgency+UPPS.Lack.Premed+UPPS.Lack.Persev+UPPS.Sens.Seek+Gender+Age, data = dataF4F3))
summary(full.modelF6F5 <- lm(FAA.F6F5 ~ BAS.Drive+BAS.Fun+BAS.Reward+BIS+UPPS.Urgency+UPPS.Lack.Premed+UPPS.Lack.Persev+UPPS.Sens.Seek+Gender+Age, data = dataF6F5))
summary(full.modelF8F7 <- lm(FAA.F8F7 ~ BAS.Drive+BAS.Fun+BAS.Reward+BIS+UPPS.Urgency+UPPS.Lack.Premed+UPPS.Lack.Persev+UPPS.Sens.Seek+Gender+Age, data = dataF8F7))

#---------------------------------
## DESCRIPTIVE STATISTICS AND CHECKING OF ASSUMPTIONS
summary(Data)

#1: The relationship between the IVs and the DV is linear

# SCATTER PLOTS - NOTHING OF INTEREST ATM

# FOR EACH MODEL, CREATE SCATTER PLOT MATRICES WITH PEARSON CORRELATIONS IN LOWER PANELS
# AND LINEAR REGRESSION LINES FITTED. FAA ~ BIS/BAS AND FAA ~ UPPS SEPARATELY, DUE TO LACK OF SPACE
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    text(0.5, 0.5, txt, cex = 1.1, font = 4)
}

reg <- function(x, y, col) abline(lm(y~x), col=col) # WHAT IS X AND Y GO BACK

panel.lm =  function (x, y, col = par("col"), bg = NA, pch = par("pch"), 
                      cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...)  {
    points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) reg(x[ok], y[ok], col.smooth)
}

# MODEL 1: F2-F1
pairs(dataF2F1[2:6], panel = panel.lm,
      cex = 1.5, pch = 19, col = adjustcolor(4, .4), cex.labels = 2, 
      font.labels = 2, lower.panel = panel.cor)

pairs(dataF2F1[2:6], panel = panel.lm,
      cex = 1.5, pch = 19, col = adjustcolor(4, .4), cex.labels = 1, 
      font.labels = 2, lower.panel = panel.cor)

# MODEL 2: F4-F3
pairs(dataF4F3[2:6], panel = panel.lm,
      cex = 1.5, pch = 19, col = adjustcolor(4, .4), cex.labels = 2, 
      font.labels = 2, lower.panel = panel.cor)

pairs(dataF4F3[6:10], panel = panel.lm,
      cex = 1.5, pch = 19, col = adjustcolor(4, .4), cex.labels = 1, 
      font.labels = 2, lower.panel = panel.cor)

# MODEL 3: F6-F5
pairs(dataF6F5[2:6], panel = panel.lm,
      cex = 1.5, pch = 19, col = adjustcolor(4, .4), cex.labels = 2, 
      font.labels = 2, lower.panel = panel.cor)

pairs(dataF6F5[6:10], panel = panel.lm,
      cex = 1.5, pch = 19, col = adjustcolor(4, .4), cex.labels = 1, 
      font.labels = 2, lower.panel = panel.cor)

# MODEL 4: F8-F7
pairs(dataF8F7[2:6], panel = panel.lm,
      cex = 1.5, pch = 19, col = adjustcolor(4, .4), cex.labels = 2, 
      font.labels = 2, lower.panel = panel.cor)

pairs(dataF8F7[6:10], panel = panel.lm,
      cex = 1.5, pch = 19, col = adjustcolor(4, .4), cex.labels = 1, 
      font.labels = 2, lower.panel = panel.cor)


# PLOTS OF PEARSON CORRELATIONS (.r) AND THEIR P-VALUES (.p)
corrplot <- rcorr(as.matrix(Data[,-c(1, 14,15)]), type = "pearson")
View(corrplot.r <- round(corrplot[["r"]], digits = 3)) # cor() is identical
View(corrplot.p <- round(corrplot[["P"]], digits = 3))

# MULTICOLLINEARITY
# PLOT OF PEARSON CORRELATIONS FOR THE NUMERIC INDEPENDENT VARIABLES
corrplotIV <- rcorr(as.matrix(Data[,-c(1, 6:9, 14,15)]), type = "pearson")
corrplotIV.r <- round(corrplot[["r"]], digits = 3) # cor() is identical
View(corrplotIV.r)


# VARIANCE INFLATION FACTOR (VIF) - ADD FULL.MODEL LATER (NO UPPS ATM) - LOOKS GOOD
car::vif(full.modelF2F1 <- lm(FAA.F2F1 ~ BAS.Drive+BAS.Fun+BAS.Reward+BIS+Gender+Age, data = dataF2F1))
car::vif(full.modelF4F3 <- lm(FAA.F4F3 ~ BAS.Drive+BAS.Fun+BAS.Reward+BIS+Gender+Age, data = dataF4F3))
car::vif(full.modelF6F5 <- lm(FAA.F6F5 ~ BAS.Drive+BAS.Fun+BAS.Reward+BIS+Gender+Age, data = dataF6F5))
car::vif(full.modelF8F7 <- lm(FAA.F8F7 ~ BAS.Drive+BAS.Fun+BAS.Reward+BIS+Gender+Age, data = dataF8F7))

#------------------------------------------------------------------

# RESIDUALS AND OUTLIERS
par(mfrow = c(2,2))
plot(full.modelF2F1, las = 1) # obs. 144, 153, 157 are possibly problematic
plot(full.modelF4F3, las = 1) # obs. 167, 174, 207 are possibly problematic
plot(full.modelF6F5, las = 1) # obs. 69, 77, 174 are possibly problematic
plot(full.modelF8F7, las = 1) # obs. 78, 145, 204 are possibly problematic

# OUTLIERS - https://towardsdatascience.com/how-to-detect-unusual-observations-on-your-regression-model-with-r-de0eaa38bc5b
outlierTest(full.modelF2F1) # OBS 157
outlierTest(full.modelF4F3) # OBS 167
outlierTest(full.modelF6F5) # OBS 77
outlierTest(full.modelF8F7) # OBS 78

# COMPUTE HIGH-LEVERAGE OBSERVATIONS FOR MODEL 1 (F2-F1)
highleverage <- function(full.modelF2F1) {
    p <- length(coefficients(full.modelF2F1))
    n <- length(fitted(full.modelF2F1))
    ratio <-p/n
    plot(hatvalues(full.modelF2F1), main="Index Plot of Ratio")
    abline(h=c(2,3)*ratio, col="red", lty=2)
    identify(1:n, hatvalues(full.modelF2F1), names(hatvalues(full.modelF2F1)))
}

highleverage(full.modelF2F1)

# RESIDUALS
resid.modelF2F1 <- residuals(full.modelF2F1)
resid.modelF4F3 <- residuals(full.modelF4F3)
resid.modelF6F5 <- residuals(full.modelF6F5)
resid.modelF8F7 <- residuals(full.modelF8F7)
hist(resid.modelF2F1)
hist(resid.modelF4F3)
hist(resid.modelF6F5)
hist(resid.modelF8F7)
shapiro.test(resid.modelF2F1) # NOT NORMAL
shapiro.test(resid.modelF4F3) # NOT NORMAL
shapiro.test(resid.modelF6F5)
shapiro.test(resid.modelF8F7)

# INFLUENTIAL VARIABLES (RR) - https://stats.idre.ucla.edu/r/dae/robust-regression/

# COOK'S DISTANCES. CUT-OFF POINT OF 4/N, WHERE N = 211
d1 <- cooks.distance(full.modelF2F1)
d2 <- cooks.distance(full.modelF4F3)
d3 <- cooks.distance(full.modelF6F5)
d4 <- cooks.distance(full.modelF8F7)
r1 <- stdres(full.modelF2F1) # STANDARDIZED RESIDUALS
r2 <- stdres(full.modelF4F3)
r3 <- stdres(full.modelF6F5)
r4 <- stdres(full.modelF8F7)
a1 <- cbind(dataF2F1, d1, r1)
a2 <- cbind(dataF4F3, d2, r2)
a3 <- cbind(dataF6F5, d3, r3)
a4 <- cbind(dataF8F7, d4, r4)
dcutoff1 <- a1[d1 > 4/N, ]
dcutoff2 <- a2[d1 > 4/N, ]
dcutoff3 <- a3[d1 > 4/N, ]
dcutoff4 <- a4[d1 > 4/N, ]
# SAME 10 SUBJECTS IN ALL MODELS

# CALCULATE ABSOLUTE VALUE OF RESIDUALS. VIEW 20 OBS. WITH HIGHEST RESIDUALS
rabs1 <- abs(r1)
a1 <- cbind(dataF2F1, d1, r1, rabs1)
a1sorted <- a1[order(-rabs1), ]
absresids1 <- a1sorted[1:20, ]
View(absresids1)

rabs2 <- abs(r2)
a2 <- cbind(dataF4F3, d2, r2, rabs2)
a2sorted <- a2[order(-rabs2), ]
absresids2 <- a2sorted[1:20, ]
View(absresids2)

rabs3 <- abs(r3)
a3 <- cbind(dataF6F5, d3, r3, rabs3)
a3sorted <- a3[order(-rabs3), ]
absresids3 <- a3sorted[1:20, ]
View(absresids3)

rabs4 <- abs(r4)
a4 <- cbind(dataF8F7, d4, r4, rabs4)
a4sorted <- a4[order(-rabs4), ]
absresids4 <- a4sorted[1:20, ]
View(absresids4)
