# Correlation between FAA scores and motivational and personality traits. Use interactively. 

# Load required packages
library(readxl)
library(ggpubr)
library(kableExtra)
library(Hmisc)
library(RVAideMemoire)
# library(reshape2)
library(rstatix)

# Path to dataLemon file - insert your own
# exportdirxls <- "D:\\MPI_LEMON\\EEG_MPILMBB_LEMON\\EEG_Statistics\\DataLemon.xlsx"
exportdirxls <- "D:\\MPI_LEMON\\EEG_MPILMBB_LEMON\\DataLemon.xlsx"

# Import data
Data <- data.frame(read_excel(exportdirxls, 1, col_names = TRUE))
row.names(Data) <- Data[,1]
Data <- Data[,-1]
attach(Data)

Data[,19] <- factor(Data[,19]) 
Data[,20] <- factor(Data[,20])
Data[,21] <- factor(Data[,21])

#### Testing normality of single variables
shapiro.test(FAA.F2F1) # Not normal (F2-F1)
shapiro.test(FAA.F4F3) # Not normal (F4-F3)
shapiro.test(FAA.F6F5) # Not normal (F6-F5)
shapiro.test(FAA.F8F7) # Normal (F8-F7)
shapiro.test(BAS.Drive) # Not normal (BAS.Drive)
shapiro.test(BAS.Fun) # Not normal (BAS.Fun)
shapiro.test(BAS.Reward) # Not normal (BAS.Reward)
shapiro.test(BIS) # Normal (BIS)
shapiro.test(NEOFFI.Neuroticism) # Not normal (NEOFFI.Neuroticism)
shapiro.test(NEOFFI.Extraversion) # Normal (NEOFFI.Extraversion)
shapiro.test(NEOFFI.OpennessForExperiences) # Not normal (NEOFFI.OpennessForExperiences)
shapiro.test(NEOFFI.Agreeableness) # Normal (NEOFFI.Agreeableness)
shapiro.test(NEOFFI.Conscientiousness) # Not normal (NEOFFI.Conscientiousness)
shapiro.test(STAI.TRAIT.ANXIETY) # Not normal (STAI.TRAIT.ANXIETY)
shapiro.test(UPPS.urgency) # Normal (UPPS.Urgency)
shapiro.test(UPPS.lack.premeditation) # Not normal (UPPS.Lack.Premeditation)
shapiro.test(UPPS.lack.perseverance) # Not normal (UPPS.Lack.Perseverance)
shapiro.test(UPPS.sens.seek) # Not normal (UPPS.Sens.Seek)
shapiro.test(Hamilton.Scale) # Not normal (Hamilton.Scale)
# Mostly non-normal variables - Spearman's Rho


##### Scatter plots (Spearman's rho) with CIs of the different frontal alpha asymmetry (FAA) scores
ggscatter(Data, x = "FAA.F2F1", y = c("FAA.F4F3","FAA.F6F5","FAA.F8F7"), combine = TRUE, 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "FAA (F2-F1)")

ggscatter(Data, x = "FAA.F4F3", y = c("FAA.F6F5","FAA.F8F7"), combine = TRUE, 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "FAA (F4-F3)")

ggscatter(Data, x = "FAA.F8F7", y = "FAA.F6F5", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "FAA (F8-F7)", ylab = "FAA (F6-F5)")


#### Matrices of Pearson and Spearman correlations

##### Pearson - for comparison
cor.data <- rcorr(as.matrix(Data[, - c(19:21)], type = "pearson"))
cor.data$r <- round(cor.data$r, digits = 3)


##### Spearman
cor.data.s <- rcorr(as.matrix(Data[, - c(19:21)], type = "spearman"))
cor.data.s$r <- round(cor.data.s$r, digits = 3)
kable(cor.data.s$r, "html") %>% kable_styling("striped") %>% scroll_box(width = "100%")

##### Construct matrix of FDR corrected p-values for Spearman's rho
p.vals <- data.frame(cor.data.s$P) # matrix of p-values
long.form <- p.vals %>% cor_gather() # convert to long and remove NA

long.form$cor <- p.adjust(long.form$cor, method = "fdr") # adjust p-values
long.spread <- long.form %>% cor_spread() # convert back

p.vals.adj <- rbind(long.spread[19,], long.spread[1:18,]) # cleanup
p.vals.adj<-data.frame(p.vals.adj[,-1])
row.names(p.vals.adj) <- colnames(p.vals.adj)
p.vals.adj <- round(p.vals.adj, 3)
kable(p.vals.adj, "html") %>% kable_styling("striped") %>% scroll_box(width = "100%")

#### Compute confidence intervals of all Spearman's rank correlation coefficients by bootstraping (B=5000)
set.seed(123)
cont.data <- Data[, - c(19:21)] # continuous variables
conf.ints <- data.frame(matrix(0,ncol(cont.data),ncol(cont.data)))

for(i in 1:ncol(cont.data)){
    for(j in 1:ncol(cont.data)){
        temp <- spearman.ci(cont.data[,j], cont.data[,i], nrep=5000, conf.level=0.95)
        conf.ints[[j,i]] <- data.frame(round(temp$conf.int,3))
    }
}
colnames(conf.ints) <- colnames(cor.s.p.adj)
row.names(conf.ints) <- row.names(cor.s.p.adj)
View(conf.ints)

### Useful Links:
# [Correlations in R](http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r)  
# [Importance of the Normality Assumption](https://doi.org/10.1146/annurev.publhealth.23.100901.140546)  
# [P-values vs Confidence Intervals](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2689604/)  

# END OF SCRIPT
