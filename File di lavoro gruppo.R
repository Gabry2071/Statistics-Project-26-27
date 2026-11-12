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
dataset$diagnosis <- as.factor(dataset$diagnosis)


##Creating columns for normalized values (and log transformed)
dataset$LYVE1_Norm <- log(dataset$LYVE1 / dataset$creatinine)
dataset$REG1B_Norm <- log(dataset$REG1B / dataset$creatinine)
dataset$TFF1_Norm <- log(dataset$TFF1 / dataset$creatinine)

#check if there are NA values in the dataset (should be handled carefully)
anyNA(dataset) #FALSE, (luckily) there are no NAs in the dataset


#### EXPLORATIVE ANALYSIS ####

####LYVE1 ####
# Description an analysis of: LYVE1

LYVE1_1 = dataset$LYVE1_Norm[dataset$diagnosis == 1]
LYVE1_2 = dataset$LYVE1_Norm[dataset$diagnosis == 2]
LYVE1_3 = dataset$LYVE1_Norm[dataset$diagnosis == 3]

summary(LYVE1_1)
summary(LYVE1_2)
summary(LYVE1_3)

# MEAN 
LYVE1_m1 = mean(LYVE1_1) 
LYVE1_m2 = mean(LYVE1_2) 
LYVE1_m3 = mean(LYVE1_3) 

#MEDIAN 
LYVE1_med1 = median(LYVE1_1) 
LYVE1_med2 = median(LYVE1_2) 
LYVE1_med3 = median(LYVE1_3) 

#QUANTILES 
LYVE1_q1 = quantile(LYVE1_1) # quantiles diagnosis 1
LYVE1_q1
LYVE1_q2 = quantile(LYVE1_2) # quantiles diagnosis 2
LYVE1_q2
LYVE1_q3 = quantile(LYVE1_3) # quantiles diagnosis 3
LYVE1_q3

#VARIANCE 
LYVE1_var1 = var(LYVE1_1) # 4.227097
LYVE1_var2 = var(LYVE1_2) # 32.36012
LYVE1_var3 = var(LYVE1_3) # 78.44902

#STANDARD DEVIATION 
LYVE1_sd1 = sd(LYVE1_1) # 2.05599 sd diagnosis 1
LYVE1_sd2 = sd(LYVE1_2) # 5.688595 sd diagnosis 2
LYVE1_sd3 = sd(LYVE1_3) # 8.857145 sd diagnosis 3

#INTERQUANTILE RANGE 
LYVE1_iqr1 = IQR(LYVE1_1) # 1.7334 iqr diagnosis 1
LYVE1_iqr2 = IQR(LYVE1_2) # 3.592694 iqr diagnosis 2
LYVE1_iqr3 = IQR(LYVE1_3) # 9.481684 iqr diagnosis 3

#HISTOGRAMS 
#library(MASS)
#truehist(LYVE1_1, nbins = "FD", xlab = 'LYVE1 expression', main = 'LYVE1 CONTROL') 
#truehist(LYVE1_2, nbins = "FD", xlab = 'LYVE1 expression', main = 'LYVE1 CONTROL')
#truehist(LYVE1_3, nbins = "FD", xlab = 'LYVE1 expression', main = 'LYVE1 CONTROL')

par(mfrow = c(1,3))
hist(LYVE1_1, xlab = 'LYVE1 expression', main = 'LYVE1 CONTROL', col ='#50C878')
hist(LYVE1_2, xlab = 'LYVE1 expression', main = 'LYVE1 BENIGN', col = '#B284BE')
hist(LYVE1_3, xlab = 'LYVE1 expression', main = 'LYVE1 MALIGNANT', col ='#FE0000')
dev.off()

#### REG1B ####
# Description an analysis of: REG1B
reg1b_1 = dataset$REG1B_Norm[dataset$diagnosis == 1] #reg1b normalised values for DIAGNOSIS 1
reg1b_2 = dataset$REG1B_Norm[dataset$diagnosis == 2] #reg1b normalised values for DIAGNOSIS 2
reg1b_3 = dataset$REG1B_Norm[dataset$diagnosis == 3] #reg1b normalised values for DIAGNOSIS 3

summary (reg1b_1)
summary (reg1b_2)
summary (reg1b_3)

# MEAN 
reg1b_m1 = mean (reg1b_1) #58.49999 mean DIAGNOSIS 1
reg1b_m2 = mean (reg1b_2) #106.6098 mean DIAGNOSIS 2
reg1b_m3 = mean (reg1b_3) #346.6593 mean DIAGNOSIS 3

#MEDIAN 
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

#VARIANCE 
reg1b_var1 = var(reg1b_1) 
reg1b_var2 = var(reg1b_2) 
reg1b_var3 = var(reg1b_3) 

#STANDARD DEVIATION 
reg1b_sd1 = sqrt(reg1b_var1) 
reg1b_sd2 = sqrt(reg1b_var2) 
reg1b_sd3 = sqrt(reg1b_var3) 

#INTERQUANTILE RANGE 
reg1b_iqr1 = IQR(reg1b_1)#56.01297 IQR DIAGNOSIS 1
reg1b_iqr2 = IQR(reg1b_2) #79.95126 IQR DIAGNOSIS 2
reg1b_iqr3 = IQR(reg1b_3) #370.2075 IQR DIAGNOSIS 3

#HISTOGRAMS
#library(MASS)
#truehist(reg1b_1, nbins = "FD") #FD sta per Freedman-Diaconis, una regola per scegliere automaticamente la larghezza delle barre (bin width) di un istogramma.
#truehist(reg1b_2, nbins = "FD")
#truehist(reg1b_3, nbins = "FD")

par(mfrow = c(1,3))
hist(reg1b_1, xlab = 'REG1B expression', main = 'REG1B CONTROL', col ='#50C878')
hist(reg1b_2, xlab = 'REG1B expression', main = 'REG1B BENIGN', col = '#B284BE')
hist(reg1b_3, xlab = 'REG1B expression', main = 'REG1B MALIGNANT', col ='#FE0000')
dev.off()



#### TFF1  ####
# Description an analysis of: TFF1
TFF1_1 = dataset$TFF1_Norm[dataset$diagnosis == 1]
TFF1_2 = dataset$TFF1_Norm[dataset$diagnosis == 2]
TFF1_3 = dataset$TFF1_Norm[dataset$diagnosis == 3]

summary(TFF1_1)
summary(TFF1_2)
summary(TFF1_3)

# MEAN 
TFF1_m1 = mean(TFF1_1) 
TFF1_m2 = mean(TFF1_2) 
TFF1_m3 = mean(TFF1_3) 

#MEDIAN
TFF1_med1 = median(TFF1_1) 
TFF1_med2 = median(TFF1_2) 
TFF1_med3 = median(TFF1_3) 

#QUANTILES
TFF1_q1 = quantile(TFF1_1) # quantiles diagnosis 1
TFF1_q1
TFF1_q2 = quantile(TFF1_2) # quantiles diagnosis 2
TFF1_q2
TFF1_q3 = quantile(TFF1_3) # quantiles diagnosis 3
TFF1_q3

#VARIANCE
TFF1_var1 = var(TFF1_1) 
TFF1_var2 = var(TFF1_2) 
TFF1_var3 = var(TFF1_3) 

#STANDARD DEVIATION 
TFF1_sd1 = sd(TFF1_1) 
TFF1_sd2 = sd(TFF1_2) 
TFF1_sd3 = sd(TFF1_3) 

#INTERQUANTILE RANGE
TFF1_iqr1 = IQR(TFF1_1) 
TFF1_iqr2 = IQR(TFF1_2) 
TFF1_iqr3 = IQR(TFF1_3) 

#HISTOGRAMS
#library(MASS)
#truehist(TFF1_1, nbins = "FD") 
#truehist(TFF1_2, nbins = "FD")
#truehist(TFF1_3, nbins = "FD")

par(mfrow = c(1,3))
hist(TFF1_1, xlab = 'TFF1 expression', main = 'TFF1 CONTROL', col ='#50C878')
hist(TFF1_2, xlab = 'TFF1 expression', main = 'TFF1 BENIGN', col = '#B284BE')
hist(TFF1_3, xlab = 'TFF1 expression', main = 'TFF1 MALIGNANT', col ='#FE0000')
dev.off()


#### SCATTERPLOTS, VARIANCE AND COVARIANCE PAIRWISE ####
### CONTROL
# LYVE1 vs REG1B

cor1 = cor(LYVE1_1, reg1b_1) 
cov(LYVE1_1, reg1b_1) 

plot(LYVE1_1, reg1b_1, 
     col = 'steelblue', pch = 19, 
     xlab = "LYVE1", ylab = "REG1B", main = "Control")

text(x = par("usr")[2],  
     y = par("usr")[3],  
     labels = paste("Cor:", round(cor1, 3)),
     adj = c(1, -0.5),   
     cex = 1.2,
     col = "blue")


# LYVE1 vs TFF1
plot(LYVE1_1, TFF1_1, col = '#FF1DCE', pch = 19,
     xlab = "LYVE1", ylab = "TFF1",
     main = "CONTROL")

cor(LYVE1_1, TFF1_1)
cov(LYVE1_1, TFF1_1) 

text(x = 0,  
     y = 0,  
     labels = paste("Cor:", round(cor(LYVE1_1, TFF1_1), 3)),
     adj = c(0, 0.5),   
     cex = 1.2,
     col = "blue")


# REG1B vs TFF1
plot(reg1b_1, TFF1_1, col = '#FF5800', pch=19,
     xlab = "REG1B", ylab = "TFF1",
     main = "CONTROL")

cor(reg1b_1, TFF1_1)
cov(reg1b_1, TFF1_1)

text(x = 0,  
     y = 0,  
     labels = paste("Cor:", round(cor(reg1b_1, TFF1_1), 3)),
     adj = c(3, 0.5),   
     cex = 1.2,
     col = "blue")


#################### BENIGN
# LYVE1 vs REG1B
plot(LYVE1_2, reg1b_2, col ='steelblue', pch=19,
     xlab = "LYVE1", ylab = "REG1B",
     main = "BENIGN")

cor(LYVE1_2, reg1b_2) 
cov(LYVE1_2, reg1b_2) 

text(x = 0,  
     y = 0,  
     labels = paste("Cor:", round(cor(LYVE1_2, reg1b_2), 3)),
     adj = c(0, 1),   
     cex = 1.2,
     col = "blue")

# LYVE1 vs TFF1
plot(LYVE1_2, TFF1_2, col = '#FF1DCE', pch = 19,
     xlab = "LYVE1", ylab = "TFF1",
     main = "BENIGN")

cor(LYVE1_2, TFF1_2)
cov(LYVE1_2, TFF1_2)

text(x = 0,  
     y = 0,  
     labels = paste("Cor:", round(cor(LYVE1_2, TFF1_2), 3)),
     adj = c(0, 1),   
     cex = 1.2,
     col = "blue")

# REG1B vs TFF1
plot(reg1b_2, TFF1_2, col = '#FF5800', pch=19,
     xlab = "REG1B", ylab = "TFF1",
     main = "BENIGN")

cor(reg1b_2, TFF1_2)
cov(reg1b_2, TFF1_2)

text(x = 0,  
     y = 0,  
     labels = paste("Cor:", round(cor(reg1b_2, TFF1_2), 3)),
     adj = c(0, 1),   
     cex = 1.2,
     col = "blue")


############### MALIGNANT

par(mfrow = c(1,3))

# LYVE1 vs REG1B
plot(LYVE1_3, reg1b_3, col ='steelblue', pch=19,
     xlab = "LYVE1", ylab = "REG1B",
     main = "MALIGNANT")

cor(LYVE1_3, reg1b_3)
cov(LYVE1_3, reg1b_3)

legend("topleft",
       legend = paste("Cor:", round(cor(LYVE1_3, reg1b_3), 3)),
       bty = "n",
       cex = 1.2,
       text.col = "blue")

# LYVE1 vs TFF1
plot(LYVE1_3, TFF1_3, col = '#FF1DCE', pch = 19,
     xlab = "LYVE1", ylab = "TFF1",
     main = "MALIGNANT")

cor(LYVE1_3, TFF1_3)
cov(LYVE1_3, TFF1_3)

legend("topleft",
       legend = paste("Cor:", round(cor(LYVE1_3, TFF1_3), 3)),
       bty = "n",
       cex = 1.2,
       text.col = "blue")

# REG1B vs TFF1
plot(reg1b_3, TFF1_3, col = '#FF5800', pch=19,
     xlab = "REG1B", ylab = "TFF1",
     main = "MALIGNANT")

cor(reg1b_3, TFF1_3)
cov(reg1b_3, TFF1_3)

legend("topleft",
       legend = paste("Cor:", round(cor(reg1b_3, TFF1_3), 3)),
       bty = "n",
       cex = 1.2,
       text.col = "blue")

###  BOXPLOTS ###

boxplot(LYVE1_Norm ~ diagnosis, data=dataset, col = c('#50C878', '#B284BE', '#FE0000'), ylab= 'LYVE1 expression')
legend("bottomright", legend = c("Control", "Benign", "Malignant"), fill = c('#50C878', '#B284BE', '#FE0000'), bty='n')

boxplot(REG1B_Norm ~ diagnosis, data=dataset, col = c('#50C878', '#B284BE', '#FE0000'), ylab= 'REG1B expression')
legend("bottomright", legend = c("Control", "Benign", "Malignant"), fill = c('#50C878', '#B284BE', '#FE0000'), bty='n')

boxplot(TFF1_Norm ~ diagnosis, data=dataset, col = c('#50C878', '#B284BE', '#FE0000'), ylab= 'TFF1 expression')
legend("bottomright", legend = c("Control", "Benign", "Malignant"), fill = c('#50C878', '#B284BE', '#FE0000'), bty='n')

dev.off()

#### INFERENTIAL STATS ####



############   LINEAR REGRESSION   ############
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



########## NULL HYPOTHESIS ##########
# We used the t-test, a statistic test used to visualize if there is a significant difference between the mean of two groups

# T-TEST FOR LYVE1
# No Cancer vs Benign
t.test(LYVE1_1, LYVE1_2)

# No Cancer vs PDAC
t.test(LYVE1_1, LYVE1_3)

# Benign vs PDAC
t.test(LYVE1_2, LYVE1_3)


# T-TEST FOR REG1B
# No Cancer vs Benign
t.test(reg1b_1, reg1b_2)

# No Cancer vs PDAC
t.test(reg1b_1, reg1b_3)

# Benign vs PDAC
t.test(reg1b_2, reg1b_3)


# T-TEST FOR TFF1
# No Cancer vs Benign
t.test(TFF1_1, TFF1_2)

# No Cancer vs PDAC
t.test(TFF1_1, TFF1_3)

# Benign vs PDAC
t.test(TFF1_2, TFF1_3)


##### P-VALUE CORRECTION #####
# Correggere i p-value. Se utilizzi sempre 0.05 come soglia, aumenta la probabilità di ottenere falsi positivi.

# Collect all 9 p-values
p_values <- c(
     
  # LYVE1
  t.test(LYVE1_1, LYVE1_2)$p.value,
  t.test(LYVE1_1, LYVE1_3)$p.value,
  t.test(LYVE1_2, LYVE1_3)$p.value,
     
  # REG1B
  t.test(reg1b_1, reg1b_2)$p.value,
  t.test(reg1b_1, reg1b_3)$p.value,
  t.test(reg1b_2, reg1b_3)$p.value,
  
  # TFF1
  t.test(TFF1_1, TFF1_2)$p.value,
  t.test(TFF1_1, TFF1_3)$p.value,
  t.test(TFF1_2, TFF1_3)$p.value
)

# Apply Holm correction to correct p-value. 
# Holm correction is used to correct the p-value when more statistic test are made at once because doing 9 tests, without correction, increases the possibility of a false positive
# Holm correction orders p-value from smallest to biggest and applies a threshold progressively less restrictive

p_adjusted <- p.adjust(p_values, method = "holm")

# Display results
results <- data.frame(
  Biomarker = rep(c("LYVE1", "REG1B", "TFF1"), each = 3),
  Comparison = rep(c(
    "No Cancer vs Benign",
    "No Cancer vs PDAC",
    "Benign vs PDAC"
  ), 3),
  Original_p = p_values,
  Adjusted_p = p_adjusted
)

print(results)
