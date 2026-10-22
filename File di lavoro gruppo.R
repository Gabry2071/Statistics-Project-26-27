# Summarizing data and descriptive analysis
# For each of the 3 biomarkers: mean, median, quantiles, variance, standard deviation,range,interqaurtile range, histogram (dati numerici continui)
# questi calcoli vanno fatti per diagnosi
#Histogram da definire... 

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

dataset = read_xlsx("Debernardi et al 2020 data.xlsx", sheet = 'Dataset scremato')[1:8]
#this is the complete raw dataset (filtering from excel does not hold when you import the file)

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


#### EXPLORATIVE ANALYSIS ####

#### LYVE1 ####
# Description an analysis of: LYVE1

# MEAN 

#MEDIAN

#QUANTILES

#VARIANCE

#STANDARD DEVIATION 

#RANGE

#INTERQUANTILE RANGE



#### REG1B ####
# Description an analysis of: REG1B
reg1b_1 = dataset$REG1B_Norm[dataset$diagnosis == 1] #reg1b normalised values for DIAGNOSIS 1
reg1b_2 = dataset$REG1B_Norm[dataset$diagnosis == 2] #reg1b normalised values for DIAGNOSIS 2
reg1b_3 = dataset$REG1B_Norm[dataset$diagnosis == 3] #reg1b normalised values for DIAGNOSIS 3

summary (reg1b_1)
summary (reg1b_2)
summary (reg1b_3)

# MEAN 
reg1b_m1 = mean (reg1b_1) #58.20542 mean DIAGNOSIS 1
reg1b_m2 = mean (reg1b_2) #106.6098 mean DIAGNOSIS 2
reg1b_m3 = mean (reg1b_3) #347.027 mean DIAGNOSIS 3

#MEDIAN
reg1b_med1 = median (reg1b_1) #28.82848 median DIAGNOSIS 1
reg1b_med2 = median (reg1b_2) #34.59517 median DIAGNOSIS 2
reg1b_med3 = median (reg1b_3) #199.3209 median DIAGNOSIS 3

#QUANTILES
reg1b_q1 = quantile(reg1b_1) #quantiles DIAGNOSIS 1
reg1b_q1
reg1b_q2 = quantile(reg1b_2) #quantiles DIAGNOSIS 2
reg1b_q2
reg1b_q3 = quantile (reg1b_3) #quantiles DIAGNOSIS 3
reg1b_q3

#VARIANCE
reg1b_var1 = var(reg1b_1) #7822.783 variance DIAGNOSIS 1
reg1b_var2 = var(reg1b_2) #54634.88 variance DIAGNOSIS 2
reg1b_var3 = var(reg1b_3) #231549 variance DIAGNOSIS 3

#STANDARD DEVIATION 
reg1b_sd1 = sqrt(reg1b_var1) #notice: it is THE SAME as sd(reg1b_1) #88.4465 SD DIAGNOSIS 1
reg1b_sd2 = sqrt(reg1b_var2) #233.741 SD DIAGNOSIS 2
reg1b_sd3 = sqrt(reg1b_var3) #481.1954 SD DIAGNOSIS 3

#INTERQUANTILE RANGE
reg1b_iqr1 = IQR(reg1b_1) #55.63396 IQR DIAGNOSIS 1
reg1b_iqr2 = IQR(reg1b_2) #79.95126 IQR DIAGNOSIS 2
reg1b_iqr3 = IQR(reg1b_3) #372.642 IQR DIAGNOSIS 3

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


#### TFF1 ####
# Description an analysis of: TFF1

# MEAN 

#MEDIAN

#QUANTILES

#VARIANCE

#STANDARD DEVIATION 

#RANGE

#INTERQUANTILE RANGE

#### SCATTERPLOTS, VARIANCE AND COVARIANCE PAIRWISE ####


#SCATTERPLOT (PAIRWISE) #confronto nello stesso gruppo due marcatori diversi
#stessa cosa per Correlazione e Covarianza
