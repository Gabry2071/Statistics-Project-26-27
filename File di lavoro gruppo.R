# Summarizing data and descriptive analysis
# For each of the 3 biomarkers: mean, median, quantiles, variance, standard deviation,range,interqaurtile range, histogram (dati numerici continui)
# questi calcoli vanno fatti per diagnosi

##TROVATE I DATI GIA' NORMALIZZATI NELLE RISPETTIVE COLONNE FILE EXCEL
#La normalizzazione dei dati dei biomarcatori urinari per l'adenocarcinoma duttale pancreatico (PDAC) rispetto alla creatinina è un passaggio fondamentale per correggere la naturale variabilità della concentrazione delle urine tra i pazienti e nei diversi momenti della giornata. [1, 2]
#Senza questo processo, i test di screening o diagnosi precoce basati su pannelli urinari (come il noto pannello a tre biomarcatori LYVE1, REG1B e TFF1) produrrebbero falsi positivi o falsi negativi dovuti semplicemente a quanto il paziente ha bevuto. [1, 2])

options(scipen = 999)
#Ragazzi mi raccomando una volta che scrivete,usatae funzioni o quelloche volete COMMENTATE!!! il codice che avete fatto
#anche ragionamenti e step per arrivare alla funzione usata o altro (so che non ha senso per ora, ma è buona prassi)


#### Importing the dataset ####
###Download il file R dalla repository e inseriscilo nella tua cartella di lavoro
###In R, vai su SESSION > SET WORKING DIRECTORY > CHOOSE DIRECTORY > SELEZIONA LA TUA CARTELLA DI LAVORO 
###The following code should work anche per voi, basta che settate la vostra working directory come sopra :)

#install.packages("readxl")
#install.packages("dplyr")
library(dplyr)
library(readxl)

dataset = read_xlsx("dataset_stats.xlsx")[1:8] 
#this is the complete raw dataset

##important to state we are dealing with NUMBERS
dataset$LYVE1 <- as.numeric(dataset$LYVE1)
dataset$creatinine <- as.numeric(dataset$creatinine)
dataset$REG1B <- as.numeric(dataset$REG1B)
dataset$TFF1 <- as.numeric(dataset$TFF1)


##Creating columns for normalised values
dataset$LYVE1_Norm <- dataset$LYVE1 / dataset$creatinine
dataset$REG1B_Norm <- dataset$REG1B / dataset$creatinine
dataset$TFF1_Norm <- dataset$TFF1 / dataset$creatinine

#check if there are NA values in the dataset (should be handled carefully)
anyNA(dataset) #FALSE, (luckily) there are no NAs in the dataset


### EXPLORATIVE ANALYSIS ###

####LYVE1 (CHECKED)####
# Description an analysis of: LYVE1

LYVE1_1 = dataset$LYVE1_Norm[dataset$diagnosis == 1]
LYVE1_2 = dataset$LYVE1_Norm[dataset$diagnosis == 2]
LYVE1_3 = dataset$LYVE1_Norm[dataset$diagnosis == 3]

summary(LYVE1_1)
summary(LYVE1_2)
summary(LYVE1_3)

# MEAN (CHECKED)
LYVE1_m1 = mean(LYVE1_1) # 1.303221 mean diagnosis 1
LYVE1_m2 = mean(LYVE1_2) # 3.211063 mean diagnosis 2
LYVE1_m3 = mean(LYVE1_3) # 9.468053 mean diagnosis 3

#MEDIAN (CHECKED)
LYVE1_med1 = median(LYVE1_1) # 0.227452 median diagnosis 1
LYVE1_med2 = median(LYVE1_2) # 1.516199 median diagnosis 2
LYVE1_med3 = median(LYVE1_3) # 6.931753 median diagnosis 3

#QUANTILES (CHECKED)
LYVE1_q1 = quantile(LYVE1_1) # quantiles diagnosis 1
LYVE1_q1
LYVE1_q2 = quantile(LYVE1_2) # quantiles diagnosis 2
LYVE1_q2
LYVE1_q3 = quantile(LYVE1_3) # quantiles diagnosis 3
LYVE1_q3

#VARIANCE (CHECKED)
LYVE1_var1 = var(LYVE1_1) # 4.227097
LYVE1_var2 = var(LYVE1_2) # 32.36012
LYVE1_var3 = var(LYVE1_3) # 78.44902

#STANDARD DEVIATION (CHECKED)
LYVE1_sd1 = sd(LYVE1_1) # 2.05599 sd diagnosis 1
LYVE1_sd2 = sd(LYVE1_2) # 5.688595 sd diagnosis 2
LYVE1_sd3 = sd(LYVE1_3) # 8.857145 sd diagnosis 3

#INTERQUANTILE RANGE (CHECKED)
LYVE1_iqr1 = IQR(LYVE1_1) # 1.7334 iqr diagnosis 1
LYVE1_iqr2 = IQR(LYVE1_2) # 3.592694 iqr diagnosis 2
LYVE1_iqr3 = IQR(LYVE1_3) # 9.481684 iqr diagnosis 3

#HISTOGRAMS (CHECKED)
#library(MASS)
#truehist(LYVE1_1, nbins = "FD", xlab = 'LYVE1 expression', main = 'LYVE1 CONTROL') 
#truehist(LYVE1_2, nbins = "FD", xlab = 'LYVE1 expression', main = 'LYVE1 CONTROL')
#truehist(LYVE1_3, nbins = "FD", xlab = 'LYVE1 expression', main = 'LYVE1 CONTROL')

par(mfrow = c(1,3))
hist(LYVE1_1, xlab = 'LYVE1 expression', main = 'LYVE1 CONTROL')
hist(LYVE1_2, xlab = 'LYVE1 expression', main = 'LYVE1 BENIGN')
hist(LYVE1_3, xlab = 'LYVE1 expression', main = 'LYVE1 MALIGNANT')
dev.off()


#### REG1B (CHECKED)####
# Description an analysis of: REG1B
reg1b_1 = dataset$REG1B_Norm[dataset$diagnosis == 1] #reg1b normalised values for DIAGNOSIS 1
reg1b_2 = dataset$REG1B_Norm[dataset$diagnosis == 2] #reg1b normalised values for DIAGNOSIS 2
reg1b_3 = dataset$REG1B_Norm[dataset$diagnosis == 3] #reg1b normalised values for DIAGNOSIS 3

summary (reg1b_1)
summary (reg1b_2)
summary (reg1b_3)

# MEAN (CHECKED)
reg1b_m1 = mean (reg1b_1) #58.49999 mean DIAGNOSIS 1
reg1b_m2 = mean (reg1b_2) #106.6098 mean DIAGNOSIS 2
reg1b_m3 = mean (reg1b_3) #346.6593 mean DIAGNOSIS 3

#MEDIAN (CHECKED)
reg1b_med1 = median (reg1b_1) #28.89873 median DIAGNOSIS 1
reg1b_med2 = median (reg1b_2) #34.59517 median DIAGNOSIS 2
reg1b_med3 = median (reg1b_3) #201.3776 median DIAGNOSIS 3

#QUANTILES
reg1b_q1 = quantile(reg1b_1) #quantiles DIAGNOSIS 1
reg1b_q1
reg1b_q2 = quantile(reg1b_2) #quantiles DIAGNOSIS 2
reg1b_q2
reg1b_q3 = quantile (reg1b_3) #quantiles DIAGNOSIS 3
reg1b_q3

#VARIANCE (CHECKED)
reg1b_var1 = var(reg1b_1) #7849.712 variance DIAGNOSIS 1
reg1b_var2 = var(reg1b_2) #54634.88 variance DIAGNOSIS 2
reg1b_var3 = var(reg1b_3) #230406.5 variance DIAGNOSIS 3

#STANDARD DEVIATION (CHECKED)
reg1b_sd1 = sqrt(reg1b_var1) #notice: it is THE SAME as sd(reg1b_1) #88.5986 SD DIAGNOSIS 1
reg1b_sd2 = sqrt(reg1b_var2) #233.741 SD DIAGNOSIS 2
reg1b_sd3 = sqrt(reg1b_var3) #480.0067 SD DIAGNOSIS 3

#INTERQUANTILE RANGE (CHECKED)
reg1b_iqr1 = IQR(reg1b_1)#56.01297 IQR DIAGNOSIS 1
reg1b_iqr2 = IQR(reg1b_2) #79.95126 IQR DIAGNOSIS 2
reg1b_iqr3 = IQR(reg1b_3) #370.2075 IQR DIAGNOSIS 3

#HISTOGRAMS
library(MASS)
truehist(reg1b_1, nbins = "FD") #FD sta per Freedman-Diaconis, una regola per scegliere automaticamente la larghezza delle barre (bin width) di un istogramma.
truehist(reg1b_2, nbins = "FD")
truehist(reg1b_3, nbins = "FD")

par(mfrow = c(1,3))
hist(reg1b_1, xlab = 'REG1B', main = 'REG1B CONTROL')
hist(reg1b_2, xlab = 'REG1B', main = 'REG1B BENIGN')
hist(reg1b_3, xlab = 'REG1B', main = 'REG1B MALIGNANT')
dev.off()


#### TFF1 (CHECKED) ####
# Description an analysis of: TFF1
TFF1_1 = dataset$TFF1_Norm[dataset$diagnosis == 1]
TFF1_2 = dataset$TFF1_Norm[dataset$diagnosis == 2]
TFF1_3 = dataset$TFF1_Norm[dataset$diagnosis == 3]

summary(TFF1_1)
summary(TFF1_2)
summary(TFF1_3)

# MEAN 
TFF1_m1 = mean(TFF1_1) # 171.48 mean diagnosis 1
TFF1_m2 = mean(TFF1_2) # 575.388 mean diagnosis 2
TFF1_m3 = mean(TFF1_3) # 1620.886 mean diagnosis 3

#MEDIAN
TFF1_med1 = median(TFF1_1) # 102.145 median diagnosis 1
TFF1_med2 = median(TFF1_2) # 312.34 median diagnosis 2
TFF1_med3 = median(TFF1_3) # 1001.937 median diagnosis 3

#QUANTILES
TFF1_q1 = quantile(TFF1_1) # quantiles diagnosis 1
TFF1_q1
TFF1_q2 = quantile(TFF1_2) # quantiles diagnosis 2
TFF1_q2
TFF1_q3 = quantile(TFF1_3) # quantiles diagnosis 3
TFF1_q3

#VARIANCE
TFF1_var1 = var(TFF1_1) # 51444.451
TFF1_var2 = var(TFF1_2) # 680287.305
TFF1_var3 = var(TFF1_3) # 5027436.525

#STANDARD DEVIATION 
TFF1_sd1 = sd(TFF1_1) # 226.813 sd diagnosis 1
TFF1_sd2 = sd(TFF1_2) # 824.795 sd diagnosis 2
TFF1_sd3 = sd(TFF1_3) # 2242.194 sd diagnosis 3

#INTERQUANTILE RANGE
TFF1_iqr1 = IQR(TFF1_1) # 222.1916 iqr diagnosis 1
TFF1_iqr2 = IQR(TFF1_2) # 545.581 iqr diagnosis 2
TFF1_iqr3 = IQR(TFF1_3) # 1710.734 iqr diagnosis 3

#HISTOGRAMS
library(MASS)
truehist(TFF1_1, nbins = "FD") 
truehist(TFF1_2, nbins = "FD")
truehist(TFF1_3, nbins = "FD")

par(mfrow = c(1,3))
hist(TFF1_1, xlab = 'TFF1', main = 'TFF1 CONTROL')
hist(TFF1_2, xlab = 'TFF1', main = 'TFF1 BENIGN')
hist(TFF1_3, xlab = 'TFF1', main = 'TFF1 MALIGNANT')
dev.off()

#### SCATTERPLOTS, VARIANCE AND COVARIANCE PAIRWISE ####
#CONTROL
par(mfrow = c(1,3))
# LYVE1 vs REG1B
plot(LYVE1_1, reg1b_1,
     xlab = "LYVE1", ylab = "REG1B",
     main = "CONTROL")

cor(LYVE1_1, reg1b_1) # -0.01548522
cov(LYVE1_1, reg1b_1) # -2.820755
# LYVE1 vs TFF1
plot(LYVE1_1, TFF1_1,
     xlab = "LYVE1", ylab = "TFF1",
     main = "CONTROL")

cor(LYVE1_1, TFF1_1) # 0.4644655
cov(LYVE1_1, TFF1_1) # 216.5927
# REG1B vs TFF1
plot(reg1b_1, TFF1_1,
     xlab = "REG1B", ylab = "TFF1",
     main = "CONTROL")

cor(reg1b_1, TFF1_1) # 0.1355866
cov(reg1b_1, TFF1_1) # 2724.664

#################### BENIGN

par(mfrow = c(1,3))
# LYVE1 vs REG1B
plot(LYVE1_2, reg1b_2,
     xlab = "LYVE1", ylab = "REG1B",
     main = "BENIGN")

cor(LYVE1_2, reg1b_2) # 0.2452998
cov(LYVE1_2, reg1b_2) # 326.1649
# LYVE1 vs TFF1
plot(LYVE1_2, TFF1_2,
     xlab = "LYVE1", ylab = "TFF1",
     main = "BENIGN")

cor(LYVE1_2, TFF1_2) # 0.4292406
cov(LYVE1_2, TFF1_2) # 2013.965
# REG1B vs TFF1
plot(reg1b_2, TFF1_2,
     xlab = "REG1B", ylab = "TFF1",
     main = "BENIGN")

cor(reg1b_2, TFF1_2) # 0.4392715
cov(reg1b_2, TFF1_2) # 84686.51

############### MALIGNANT

par(mfrow = c(1,3))
# LYVE1 vs REG1B
plot(LYVE1_3, reg1b_3,
     xlab = "LYVE1", ylab = "REG1B",
     main = "MALIGNANT")

cor(LYVE1_3, reg1b_3) # 0.42723
cov(LYVE1_3, reg1b_3) # 1816.364
# LYVE1 vs TFF1
plot(LYVE1_3, TFF1_3,
     xlab = "LYVE1", ylab = "TFF1",
     main = "MALIGNANT")

cor(LYVE1_3, TFF1_3) # 0.3842481
cov(LYVE1_3, TFF1_3) # 7630.953
# REG1B vs TFF1
plot(reg1b_3, TFF1_3,
     xlab = "REG1B", ylab = "TFF1",
     main = "MALIGNANT")

cor(reg1b_3, TFF1_3) # 0.7202756
cov(reg1b_3, TFF1_3) # 775209.9


#### INFERENTIAL STATS ####
dataset$diagnosis <- as.factor(dataset$diagnosis)


########################
#la prossima come idea per matrice di scatterplot con regressione lineare
#la metto tutta come commento per ora (file esempio caricato come )

# pairs(dataset[dataset$diagnosis == 1,
#               c("LYVE1_Norm", "REG1B_Norm", "TFF1_Norm")],
#       panel = function(x, y, ...) {
#         points(x, y)
#         
#         # Calcolo della regressione lineare
#         modello = lm(y ~ x)
#         
#         # Disegno della retta
#         abline(modello, col = "red")
#         
#         # Intercetta e pendenza
#         a = coef(modello)[1]
#         b = coef(modello)[2]
#         
#         # Equazione con 6 cifre significative
#         legend("topright",
#                legend = sprintf("y = %.6g %+.6g x", a, b),
#                bty = "o", cex = 0.9)
#       },
#       main = "CONTROL")

#####################

