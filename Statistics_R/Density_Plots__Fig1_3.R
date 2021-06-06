# Distributions (density plots) of the scores on motivational and personality variables for young and old adults.

library(readxl) # for importing xls
library(tidyverse) # Data manipulation
library(ggpubr) # Data visualization (including ggplot2)

# Load data
# Path to dataLemon file
exportdirxls <- "D:\\MPI_LEMON\\EEG_MPILMBB_LEMON\\EEG_Statistics\\DataLemon.xlsx"

# Import data
Data <- data.frame(read_excel(exportdirxls, 1, col_names = TRUE))
row.names(Data) <- Data[,1]
Data <- Data[,-1]
Data <- na.omit(Data) # Remove missing values
Data[,19] <- factor(Data[,19]) 
Data[,20] <- factor(Data[,20])
Data[,21] <- factor(Data[,21])

# Create density plots.
p1=Data%>%ggplot(aes(x=BAS.Drive,fill=Age))+geom_rug(aes(color=Age))+geom_density(alpha=0.5)+theme_bw()+geom_vline(xintercept = c(4,16),linetype="dashed",color=c("black","black"))+theme(legend.position = "none",axis.title.x = element_blank(),axis.title.y = element_blank())+ggtitle("BAS Drive")+theme(plot.title = element_text(hjust = 0.5))
p2=Data%>%ggplot(aes(x=BAS.Fun,fill=Age))+geom_rug(aes(color=Age))+geom_density(alpha=0.5)+theme_bw()+geom_vline(xintercept = c(4,16),linetype="dashed",color=c("black","black"))+theme(legend.position = "none",axis.title.x = element_blank(),axis.title.y = element_blank())+ggtitle("BAS Fun Seeking")+theme(plot.title = element_text(hjust = 0.5))
p3=Data%>%ggplot(aes(x=BAS.Reward,fill=Age))+geom_rug(aes(color=Age))+geom_density(alpha=0.5)+theme_bw()+geom_vline(xintercept = c(5,20),linetype="dashed",color=c("black","black"))+theme(legend.position = "none",axis.title.x = element_blank(),axis.title.y = element_blank())+ggtitle("BAS Reward Responsiveness")+theme(plot.title = element_text(hjust = 0.5))
p4=Data%>%ggplot(aes(x=BIS,fill=Age))+geom_rug(aes(color=Age))+geom_density(alpha=0.5)+theme_bw()+geom_vline(xintercept = c(7,28),linetype="dashed",color=c("black","black"))+theme(legend.position = "none",axis.title.x = element_blank(),axis.title.y = element_blank())+ggtitle("BIS")+theme(plot.title = element_text(hjust = 0.5)) # MEAN
p5=Data%>%ggplot(aes(x=NEOFFI.Neuroticism,fill=Age))+geom_rug(aes(color=Age))+geom_density(alpha=0.5)+theme_bw()+geom_vline(xintercept = c(0,4),linetype="dashed",color=c("black","black"))+theme(legend.position = "none",axis.title.x = element_blank(),axis.title.y = element_blank())+ggtitle("Neuroticism")+theme(plot.title = element_text(hjust = 0.5))
p6=Data%>%ggplot(aes(x=NEOFFI.Extraversion,fill=Age))+geom_rug(aes(color=Age))+geom_density(alpha=0.5)+theme_bw()+geom_vline(xintercept = c(0,4),linetype="dashed",color=c("black","black"))+theme(legend.position = "none",axis.title.x = element_blank(),axis.title.y = element_blank())+ggtitle("Extraversion")+theme(plot.title = element_text(hjust = 0.5)) # MEAN
p7=Data%>%ggplot(aes(x=NEOFFI.OpennessForExperiences,fill=Age))+geom_rug(aes(color=Age))+geom_density(alpha=0.5)+theme_bw()+geom_vline(xintercept = c(0,4),linetype="dashed",color=c("black","black"))+theme(legend.position = "none",axis.title.x = element_blank(),axis.title.y = element_blank())+ggtitle("Openness for Experience")+theme(plot.title = element_text(hjust = 0.5))
p8=Data%>%ggplot(aes(x=NEOFFI.Agreeableness,fill=Age))+geom_rug(aes(color=Age))+geom_density(alpha=0.5)+theme_bw()+geom_vline(xintercept = c(0,4),linetype="dashed",color=c("black","black"))+theme(legend.position = "none",axis.title.x = element_blank(),axis.title.y = element_blank())+ggtitle("Agreeableness")+theme(plot.title = element_text(hjust = 0.5)) # MEAN
p9=Data%>%ggplot(aes(x=NEOFFI.Conscientiousness,fill=Age))+geom_rug(aes(color=Age))+geom_density(alpha=0.5)+theme_bw()+geom_vline(xintercept = c(0,4),linetype="dashed",color=c("black","black"))+theme(legend.position = "none",axis.title.x = element_blank(),axis.title.y = element_blank())+ggtitle("Conscientiousness")+theme(plot.title = element_text(hjust = 0.5))
p10=Data%>%ggplot(aes(x=STAI.TRAIT.ANXIETY,fill=Age))+geom_rug(aes(color=Age))+geom_density(alpha=0.5)+theme_bw()+geom_vline(xintercept = c(20,80),linetype="dashed",color=c("black","black"))+theme(legend.position = "none",axis.title.x = element_blank(),axis.title.y = element_blank())+ggtitle("Trait Anxiety")+theme(plot.title = element_text(hjust = 0.5))
p11=Data%>%ggplot(aes(x=UPPS.urgency,fill=Age))+geom_rug(aes(color=Age))+geom_density(alpha=0.5)+theme_bw()+geom_vline(xintercept = c(12,48),linetype="dashed",color=c("black","black"))+theme(legend.position = "none",axis.title.x = element_blank(),axis.title.y = element_blank())+ggtitle("Urgency")+theme(plot.title = element_text(hjust = 0.5)) # MEAN
p12=Data%>%ggplot(aes(x=UPPS.lack.premeditation,fill=Age))+geom_rug(aes(color=Age))+geom_density(alpha=0.5)+theme_bw()+geom_vline(xintercept = c(11,44),linetype="dashed",color=c("black","black"))+theme(legend.position = "none",axis.title.x = element_blank(),axis.title.y = element_blank())+ggtitle("Premeditation")+theme(plot.title = element_text(hjust = 0.5)) # MEAN
p13=Data%>%ggplot(aes(x=UPPS.lack.perseverance,fill=Age))+geom_rug(aes(color=Age))+geom_density(alpha=0.5)+theme_bw()+geom_vline(xintercept = c(10,40),linetype="dashed",color=c("black","black"))+theme(legend.position = "none",axis.title.x = element_blank(),axis.title.y = element_blank())+ggtitle("Perseverance")+theme(plot.title = element_text(hjust = 0.5)) # MEAN
p14=Data%>%ggplot(aes(x=UPPS.sens.seek,fill=Age))+geom_rug(aes(color=Age))+geom_density(alpha=0.5)+theme_bw()+geom_vline(xintercept = c(12,48),linetype="dashed",color=c("black","black"))+theme(legend.position = "none",axis.title.x = element_blank(),axis.title.y = element_blank())+ggtitle("Sensation Seeking")+theme(plot.title = element_text(hjust = 0.5))
p15=Data%>%ggplot(aes(x=Hamilton.Scale,fill=Age))+geom_rug(aes(color=Age))+geom_density(alpha=0.5)+theme_bw()+geom_vline(xintercept = c(0,17),linetype="dashed",color=c("black","black"))+theme(legend.position = "none",axis.title.x = element_blank(),axis.title.y = element_blank())+ggtitle("HAM-D")+theme(plot.title = element_text(hjust = 0.5))

# Create figure
plot <- ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,
                   ncol = 3, nrow = 5,
                   common.legend = TRUE, legend = "bottom") # +theme(panel.border = element_rect(colour = "black", fill=NA, size=2))


# Table 1: Descriptive Statistics. Used as a template only.
tablesum <- stargazer(Data, type = "text",
                      digits = 2,
                      summary.stat = c("n","mean","sd","min","median","max")) # Use type = "html" to save html file




#-----------PART NOT USED FOR THESIS-----------------

# One variable at a time. Normality tests and tests for difference between age groups.
# BAS.Drive
psych::describeBy(Data$BAS.Drive, Data$Age) # descriptive stats
Data$BAS.Drive%>%split(Data$Age)%>%map(~shapiro.test(.)) # test normality - both non-normal

# modified t-test for independent trimmed means (Yuen, 1974) with a bootstrap.
# tr = 0.2 (20 % each side) as recommended by (Wilcox, 2017). 0% trim is equivalent to Welch's t-test
yuenbt(BAS.Drive~Age, data=Data, tr = 0.2, nboot = 5000)
t.test(BAS.Drive~Age, data=Data) # Welch's t-test for comparison
# No significant difference in trimmed means between age groups on the BAS-DR scale,
# Mdiff = 0.17 [-0.57, 0.90], Yt = 0.44, p = 0.65, NS.
#----------
# BAS.Fun
psych::describeBy(Data$BAS.Fun, Data$Age) # descriptive stats
Data$BAS.Fun%>%split(Data$Age)%>%map(~shapiro.test(.)) # test normality - both non-normal

# modified t-test for independent trimmed means (Yuen, 1974) with a bootstrap.
# tr = 0.2 (20 % each side) as recommended by (Wilcox, 2017). 0% trim is equivalent to Welch's t-test
yuenbt(BAS.Fun~Age, data=Data, tr = 0.2, nboot = 5000)
# Found a significant difference in trimmed means between age groups on the BAS-FS scale,
# Mdiff = -0.85 [-1.39, -0.31], Yt = -3.06, p = 0.004.
#----------
# BAS.Reward
psych::describeBy(Data$BAS.Reward, Data$Age) # descriptive stats
Data$BAS.Reward%>%split(Data$Age)%>%map(~shapiro.test(.)) # test normality - both non-normal

# modified t-test for independent trimmed means (Yuen, 1974) with a bootstrap.
# tr = 0.2 (20 % each side) as recommended by (Wilcox, 2017). 0% trim is equivalent to Welch's t-test
yuenbt(BAS.Reward~Age, data=Data, tr = 0.2, nboot = 5000)
# No significant difference in trimmed means between age groups on the BAS-RR scale,
# Mdiff = -0.37 [-1.08, 0.34], Yt = -1.02, p = 0.3, NS.
#----------
# BIS
psych::describeBy(Data$BIS, Data$Age) # descriptive stats
Data$BIS%>%split(Data$Age)%>%map(~shapiro.test(.)) # test normality - both normal
library(rstatix) # Values above Q3 + 3xIQR or below Q1 - 3xIQR are outliers. For parametric t-test.
identify_outliers(as.data.frame(Data$BIS)) # 0 found. Using Welch's t-test instead of Yuen's test.

# modified t-test for independent trimmed means (Yuen, 1974) with a bootstrap.
# tr = 0.2 (20 % each side) as recommended by (Wilcox, 2017). 0% trim is equivalent to Welch's t-test
t.test(BIS~Age, data=Data) # Welch's t-test is appropriate here.
# No significant difference in trimmed means between age groups on the BAS-RR scale,
# Mdiff = -1.35 [-2.18, -0.51], df = 157.63, t = -3.18, p = 0.002.
#----------
# NEOFFI.Neuroticism
psych::describeBy(Data$NEOFFI.Neuroticism, Data$Age) # descriptive stats
Data$NEOFFI.Neuroticism%>%split(Data$Age)%>%map(~shapiro.test(.)) # test normality - only young normal

# modified t-test for independent trimmed means (Yuen, 1974) with a bootstrap.
# tr = 0.2 (20 % each side) as recommended by (Wilcox, 2017). 0% trim is equivalent to Welch's t-test
yuenbt(NEOFFI.Neuroticism~Age, data=Data, tr = 0.2, nboot = 5000)
# No significant difference in trimmed means between age groups on the BAS-RR scale,
# Mdiff = -0.23 [-0.40, -0.06], Yt = -2.65, p = 0.007.
#----------
# NEOFFI.Extraversion
psych::describeBy(Data$NEOFFI.Extraversion, Data$Age) # descriptive stats
Data$NEOFFI.Extraversion%>%split(Data$Age)%>%map(~shapiro.test(.)) # test normality - both normal
identify_outliers(as.data.frame(Data$NEOFFI.Extraversion)) # 0 found. Using Welch's t-test instead of Yuen's test.

# modified t-test for independent trimmed means (Yuen, 1974) with a bootstrap.
# tr = 0.2 (20 % each side) as recommended by (Wilcox, 2017). 0% trim is equivalent to Welch's t-test
t.test(NEOFFI.Extraversion~Age, data=Data)
# No significant difference in trimmed means between age groups on the BAS-RR scale,
# Mdiff = -0.06 [-0.21, 0.09], t = -0.82, df = 148.52, p = 0.411, NS.
#----------
# NEOFFI.OpennessForExperiences
psych::describeBy(Data$NEOFFI.OpennessForExperiences, Data$Age) # descriptive stats
Data$NEOFFI.OpennessForExperiences%>%split(Data$Age)%>%map(~shapiro.test(.)) # test normality - one non-normal

# modified t-test for independent trimmed means (Yuen, 1974) with a bootstrap.
# tr = 0.2 (20 % each side) as recommended by (Wilcox, 2017). 0% trim is equivalent to Welch's t-test
yuenbt(NEOFFI.OpennessForExperiences~Age, data=Data, tr = 0.2, nboot = 5000)
# No significant difference in trimmed means between age groups on the BAS-DR scale,
# Mdiff = -0.26 [-0.42, -0.09], Yt = -3.06, p = 0.003.
#----------
# NEOFFI.Agreeableness
psych::describeBy(Data$NEOFFI.Agreeableness, Data$Age) # descriptive stats
Data$NEOFFI.Agreeableness%>%split(Data$Age)%>%map(~shapiro.test(.)) # test normality - both normal
identify_outliers(as.data.frame(Data$NEOFFI.Agreeableness)) # 0 found. Using Welch's t-test instead of Yuen's test.

# modified t-test for independent trimmed means (Yuen, 1974) with a bootstrap.
# tr = 0.2 (20 % each side) as recommended by (Wilcox, 2017). 0% trim is equivalent to Welch's t-test
t.test(NEOFFI.Agreeableness~Age, data=Data)
# No significant difference in trimmed means between age groups on the BAS-DR scale,
# Mdiff = 0.02 [-0.10, 0.15], t = 0.36, df = 142.2, p = 0.722, NS.
#----------
# NEOFFI.Conscientiousness
psych::describeBy(Data$NEOFFI.Conscientiousness, Data$Age) # descriptive stats
Data$NEOFFI.Conscientiousness%>%split(Data$Age)%>%map(~shapiro.test(.)) # test normality - one normal

# modified t-test for independent trimmed means (Yuen, 1974) with a bootstrap.
# tr = 0.2 (20 % each side) as recommended by (Wilcox, 2017). 0% trim is equivalent to Welch's t-test
yuenbt(NEOFFI.Conscientiousness~Age, data=Data, tr = 0.2, nboot = 5000)
# No significant difference in trimmed means between age groups on the BAS-DR scale,
# Mdiff = -0.26 [0.31, 0.61], Yt = 5.93, p = 0.000.
#----------
# STAI.TRAIT.ANXIETY
psych::describeBy(Data$STAI.TRAIT.ANXIETY, Data$Age) # descriptive stats
Data$STAI.TRAIT.ANXIETY%>%split(Data$Age)%>%map(~shapiro.test(.)) # test normality - both non-normal

# modified t-test for independent trimmed means (Yuen, 1974) with a bootstrap.
# tr = 0.2 (20 % each side) as recommended by (Wilcox, 2017). 0% trim is equivalent to Welch's t-test
yuenbt(STAI.TRAIT.ANXIETY~Age, data=Data, tr = 0.2, nboot = 5000)
# No significant difference in trimmed means between age groups on the BAS-DR scale,
# Mdiff = -5.40 [-7.95, -2.85], Yt = -4.08, p = 0.000.
#----------
# UPPS.urgency
psych::describeBy(Data$UPPS.urgency, Data$Age) # descriptive stats
Data$UPPS.urgency%>%split(Data$Age)%>%map(~shapiro.test(.)) # test normality - both normal
identify_outliers(as.data.frame(Data$UPPS.urgency)) # 0 found. Using Welch's t-test instead of Yuen's test.

# Welch (1951) t-test
# tr = 0.2 (20 % each side) as recommended by (Wilcox, 2017). 0% trim is equivalent to Welch's t-test
t.test(UPPS.urgency~Age, data=Data)
# No significant difference in trimmed means between age groups on the BAS-DR scale,
# Mdiff = -1.49 [-2.81, -0.11], t = -2.23, p = 0.027.
#----------
# UPPS.lack.premeditation
psych::describeBy(Data$UPPS.lack.premeditation, Data$Age) # descriptive stats
Data$UPPS.lack.premeditation%>%split(Data$Age)%>%map(~shapiro.test(.)) # test normality - both normal
identify_outliers(as.data.frame(Data$UPPS.lack.premeditation)) # 0 extreme. Using Welch's t-test instead of Yuen's test.

# Welch (1951) t-test
t.test(UPPS.lack.premeditation~Age, data=Data)
# No significant difference in trimmed means between age groups on the BAS-DR scale,
# Mdiff = -1.33 [-2.46, -0.20], t = -2.34, p = 0.021.
#----------
# UPPS.lack.perseverance
psych::describeBy(Data$UPPS.lack.perseverance, Data$Age) # descriptive stats
Data$UPPS.lack.perseverance%>%split(Data$Age)%>%map(~shapiro.test(.)) # test normality - both normal
identify_outliers(as.data.frame(Data$UPPS.lack.perseverance)) # 0 extreme. Using Welch's t-test instead of Yuen's test.

# Welch (1951) t-test
t.test(UPPS.lack.perseverance~Age, data=Data)
# No significant difference in trimmed means between age groups on the BAS-DR scale,
# Mdiff = -2.55 [-3.76, -1.34], t = -4.15, df = 171.93 p < 0.001.
#----------
# UPPS.sens.seek
psych::describeBy(Data$UPPS.sens.seek, Data$Age) # descriptive stats
Data$UPPS.sens.seek%>%split(Data$Age)%>%map(~shapiro.test(.)) # test normality - one normal

yuenbt(UPPS.sens.seek~Age, data=Data, tr = 0.2, nboot = 5000)
# No significant difference in trimmed means between age groups on the BAS-DR scale,
# Mdiff = -7.22 [-9.21, -5.22], Ty = -6.96, p < 0.001.
#----------
# Hamilton.Scale 
psych::describeBy(Data$Hamilton.Scale , Data$Age) # descriptive stats
Data$Hamilton.Scale %>%split(Data$Age)%>%map(~shapiro.test(.)) # test normality - both non-normal

yuenbt(Hamilton.Scale~Age, data=Data, tr = 0.2, nboot = 5000)
# No significant difference in trimmed means between age groups on the BAS-DR scale,
# Mdiff = -0.46 [-1.22, 0.29], Ty = -1.21, p = 0.22, NS.

# END OF SCRIPT
