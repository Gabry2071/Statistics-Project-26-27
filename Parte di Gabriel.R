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
#install.packages(c("dplyr", "tidyr", "ggplot2"))
#install.packages("BiocManager")
#BiocManager::install("EnhancedVolcano")
#install.packages("ggplot")
#install.packages("ggsignif")

library(dplyr)
library(tidyr)
library(ggplot2)
library(EnhancedVolcano)
library(ggsignif)
dataset = read.csv("Debernardi et al 2020 data.csv") 
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


### PARTE SU FOLDGCHANGE CON GRAFICO E HEATMAP ###
#### Calcolo Fold Change log2FC per LYVE1 #####
LYVE1_1_log2 = log2((dataset$LYVE1 / dataset$creatinine))[dataset$diagnosis == 1]
LYVE1_2_log2 = log2((dataset$LYVE1 / dataset$creatinine))[dataset$diagnosis == 2]  
LYVE1_3_log2 = log2((dataset$LYVE1 / dataset$creatinine))[dataset$diagnosis == 3]

FC_LYVE1_3vs1 = mean(LYVE1_3_log2)-mean(LYVE1_1_log2)
FC_LYVE1_2vs1 = mean(LYVE1_2_log2)-mean(LYVE1_1_log2)
FC_LYVE1_3vs2 = mean(LYVE1_3_log2)-mean(LYVE1_2_log2)


#### Calcolo Fold Change log2FC per REG1B ####
REG1B_1_log2 = log2(dataset$REG1B / dataset$creatinine)[dataset$diagnosis == 1]
REG1B_2_log2 = log2(dataset$REG1B / dataset$creatinine)[dataset$diagnosis == 2]  
REG1B_3_log2 = log2(dataset$REG1B / dataset$creatinine)[dataset$diagnosis == 3]

FC_REG1B_3vs1 = mean(REG1B_3_log2)-mean(REG1B_1_log2)
FC_REG1B_2vs1 = mean(REG1B_2_log2)-mean(REG1B_1_log2)
FC_REG1B_3vs2 = mean(REG1B_3_log2)-mean(REG1B_2_log2)

#### Calcolo Fold Change log2FC per TFF1 ####
TFF1_1_log2 = log2(dataset$TFF1 / dataset$creatinine)[dataset$diagnosis == 1]
TFF1_2_log2 = log2(dataset$TFF1 / dataset$creatinine)[dataset$diagnosis == 2]  
TFF1_3_log2 = log2(dataset$TFF1 / dataset$creatinine)[dataset$diagnosis == 3]

FC_TFF1_3vs1 = mean(TFF1_3_log2)-mean(TFF1_1_log2)
FC_TFF1_2vs1 = mean(TFF1_2_log2)-mean(TFF1_1_log2)
FC_TFF1_3vs2 = mean(TFF1_3_log2)-mean(TFF1_2_log2)

#### BOXPLOT con FC####
dati_box <- data.frame(
  Biomarker = c(
    rep("LYVE1", length(LYVE1_1_log2) + length(LYVE1_2_log2) + length(LYVE1_3_log2)),
    rep("REG1B", length(REG1B_1_log2) + length(REG1B_2_log2) + length(REG1B_3_log2)),
    rep("TFF1",  length(TFF1_1_log2)  + length(TFF1_2_log2)  + length(TFF1_3_log2))
  ),
  Group = factor(
    c(
      rep("1: Control", length(LYVE1_1_log2)), rep("2: Benign", length(LYVE1_2_log2)), rep("3: PDAC", length(LYVE1_3_log2)),
      rep("1: Control", length(REG1B_1_log2)), rep("2: Benign", length(REG1B_2_log2)), rep("3: PDAC", length(REG1B_3_log2)),
      rep("1: Control", length(TFF1_1_log2)),  rep("2: Benign", length(TFF1_2_log2)),  rep("3: PDAC", length(TFF1_3_log2))
    ),
    levels = c("1: Control", "2: Benign", "3: PDAC")
  ),
  Log2_Value = c(
    LYVE1_1_log2, LYVE1_2_log2, LYVE1_3_log2,
    REG1B_1_log2, REG1B_2_log2, REG1B_3_log2,
    TFF1_1_log2,  TFF1_2_log2,  TFF1_3_log2
  )
)

#Create the data frame for significance brackets and Log2FC annotations
etichette_fc <- data.frame(
  Biomarker = rep(c("LYVE1", "REG1B", "TFF1"), each = 3),
  start = rep(c("1: Control", "2: Benign", "1: Control"), times = 3),
  end   = rep(c("2: Benign",  "3: PDAC",   "3: PDAC"),    times = 3),
  # Vertical height of the brackets above the maximum value in each panel
  y_pos = c(
    max(c(LYVE1_1_log2, LYVE1_2_log2, LYVE1_3_log2)) + c(0.8, 2.2, 3.8),
    max(c(REG1B_1_log2, REG1B_2_log2, REG1B_3_log2)) + c(0.8, 2.2, 3.8),
    max(c(TFF1_1_log2,  TFF1_2_log2,  TFF1_3_log2))  + c(0.8, 2.2, 3.8)
  ),
  # Text displayed above each bracket (rounded to 2 decimal places)
  label = c(
    paste0("log2FC = ", round(FC_LYVE1_2vs1, 2)),
    paste0("log2FC = ", round(FC_LYVE1_3vs2, 2)),
    paste0("log2FC = ", round(FC_LYVE1_3vs1, 2)),
    
    paste0("log2FC = ", round(FC_REG1B_2vs1, 2)),
    paste0("log2FC = ", round(FC_REG1B_3vs2, 2)),
    paste0("log2FC = ", round(FC_REG1B_3vs1, 2)),
    
    paste0("log2FC = ", round(FC_TFF1_2vs1, 2)),
    paste0("log2FC = ", round(FC_TFF1_3vs2, 2)),
    paste0("log2FC = ", round(FC_TFF1_3vs1, 2))
  )
)

# 3. Plot the Boxplots with Log2FC annotations
ggplot(dati_box, aes(x = Group, y = Log2_Value, fill = Group)) +
  geom_boxplot(width = 0.55, alpha = 0.85, outlier.shape = 21, outlier.size = 1.5) +
  # White diamond indicating the MEAN (used to compute Log2FC)
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white", color = "black") +
  # Brackets with Log2FC values
  geom_signif(
    data = etichette_fc,
    aes(xmin = start, xmax = end, annotations = label, y_position = y_pos),
    manual = TRUE,
    textsize = 3.2,
    tip_length = 0.02,
    inherit.aes = FALSE
  ) +
  facet_wrap(~ Biomarker, scales = "free_y") +
  # Expand upper Y-axis margin so the top bracket is not clipped
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  scale_fill_manual(values = c("#50C878", "#B284BE", "#FE0000")) +
  labs(
    title = "Urinary Biomarker Comparison Across Diagnostic Groups",
    subtitle = "Horizontal line = Median | White diamond = Mean | Brackets = Log2 Fold Change",
    x = "",
    y = "Log2 (Biomarker / Creatinine)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 13),
    axis.text.x = element_text(angle = 20, hjust = 1, face = "bold")
  )






#### HEATMAP ####
#Heatmaps  biomarker divisi per le tre diagnosi Control,Benign,Malignant conisderando anche una cor fra età,sesso e diagnosi
# ho rimesso qui le library da chiamare solo per sicurezza nell'esecuzione del codice (si posson commentare out)
#library(dplyr)
#library(ggplot2)
sex_num <- ifelse(as.character(dataset$sex) %in% c("M", "Male", "1"), 1, 0) #trasformo sex in numerico 0/1
#mo creo i tre gruppi con tute le loro robette

df_Control <- data.frame(  
  Pannello="1:Control",
  LYVE1=LYVE1_1,
  REG1B=reg1b_1,
  TFF1=TFF1_1,
  Age=dataset$age[dataset$diagnosis == 1],
  Sex=sex_num[dataset$diagnosis == 1]
)


df_Benign <- data.frame(  
  Pannello="2:Benign",
  LYVE1=LYVE1_2,
  REG1B=reg1b_2,
  TFF1=TFF1_2,
  Age=dataset$age[dataset$diagnosis == 2],
  Sex=sex_num[dataset$diagnosis == 2]
)


df_Malignant <- data.frame(  
  Pannello="3:Malignant",
  LYVE1=LYVE1_3,
  REG1B=reg1b_3,
  TFF1=TFF1_3,
  Age=dataset$age[dataset$diagnosis == 3],
  Sex=sex_num[dataset$diagnosis == 3]
)


df_diagnosi<-bind_rows(df_Control, df_Benign, df_Malignant)
df_overall<-df_diagnosi %>% mutate(Pannello = "Overall (All)")

df_tutti <- bind_rows(df_overall, df_diagnosi) %>%
  mutate(Pannello = factor(Pannello, levels = c("Overall (All)", "1:Control", "2:Benign", "3:Malignant")))

# Pearson correlation (su Age,Sex, 3 Biomarkers) per tutti e quattro i pannelli
variables <- c("LYVE1", "REG1B", "TFF1", "Age", "Sex")
risultati_pearson <- list()


for (pan in levels(df_tutti$Pannello)) {
  sub_df <- df_tutti %>% filter(Pannello == pan) %>% select(all_of(variables))
  
  for (v1 in variables) {
    for (v2 in variables) {
      test <- cor.test(sub_df[[v1]], sub_df[[v2]], method = "pearson") # p-value calcolato in questo caso impostando H0:la r(pearson correlation) fra le due variabili sia uguale a 0
      # quindi H1 è r!=0
      r_val <- as.numeric(test$estimate)
      p_val <- test$p.value
      
      sig <- ifelse(v1 == v2, "",
                    ifelse(p_val < 0.001, "***",
                           ifelse(p_val < 0.01,  "**",
                                  ifelse(p_val < 0.05,  "*", ""))))
      
      risultati_pearson[[length(risultati_pearson) + 1]] <- data.frame(
        Pannello = pan,
        Var1 = factor(v1, levels = variables),
        Var2 = factor(v2, levels = rev(variables)),
        r = r_val,
        p = p_val,
        Label = ifelse(v1 == v2, "1.00", paste0(sprintf("%.2f", r_val), sig))
      )
    }
  }
}

tabella_pearson <- bind_rows(risultati_pearson)

# 5. Disegna le 4 Heatmap affiancate
ggplot(tabella_pearson, aes(x = Var1, y = Var2, fill = r)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = Label), size = 3.2, fontface = "bold", color = "black") +
  facet_wrap(~ Pannello, nrow = 1) +
  scale_fill_gradient2(
    low = "#2166AC", mid = "white", high = "#B2182B",
    midpoint = 0, limits = c(-1, 1),
    name = "Pearson r"
  ) +
  coord_fixed() +
  labs(
    title = "Pearson Correlation Matrix: Overall vs Diagnostic Groups",
    subtitle = "Biomarkers, Age, and Sex (0 = Female, 1 = Male) | * p < 0.05 , ** p < 0.01, *** p < 0.001",
    #i valori del p-value per Biomarker vs Biomarker tendono ad essere p<0.05 se abbiamo una correlazione diretta fra i due nello stesso gruppo di pazienti (entrmabi up o down)
    #se fosse maggiore di 0.05 allora sono uno indipendente dall'altro
    #Biomarker vs Sex, dato che abbiamo 0/1 abbiamo due t-test (maschi e femmine) se p<0.05 abbiamo una differenza statistica dell'espressione del biomarker fra maschi e femmine
    # se r>0 più alto negli uomini, se r<0 più alto nelle donne, se p>0.05 allora non abbiamo differenze significative
    #Biomarker vs Age, se p<0.05 allora i livelli cambiano in maniera significativa in base all'età, mentre se p>0.05 l'età non influenza in maniera significativa
    #Sex vs Age abbiamo p>=0.05 e r vicino a 0 allora uomini e donne hannostessa età media(campione bilanciato)
    #p<0.05 sbilanciamento dell'età nei due sessi se r>0 gli uomini sono mediamente più anziani, se r<0 donne sono mediamente più anziane 
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    panel.grid = element_blank()
  )

# Nel pannello Overall (All) hai tutti i pazienti messi insieme (N grande), quindi il test ha molta potenza statistica e anche un 
# r piccolo (es. 0.20) risulta significativo (***).
# Nei singoli gruppi (Control, Benign, Malignant), avendo meno pazienti lo stesso identico valore di r=0.20 potrebbe non avere asterischi
# gli asterischi sono la significatività del dato rapportato alla r e al numeor di pazienti










### GRAFICI CHE VOLENDO ABBIAMO MA DOBBIAMO ANCORA DISCUTERE COSA FARNE ###
####VULCAN PLOT####
# dati_volcano <- data.frame(
#   Biomarker = rep(c("LYVE1", "REG1B", "TFF1"), times = 3),
#   Confronto = rep(c("3 vs 1 (PDAC vs Ctrl)", "2 vs 1 (Benigno vs Ctrl)", "3 vs 2 (PDAC vs Benigno)"), each = 3),
#   Etichetta = c(
#     "LYVE1 (3 vs 1)", "REG1B (3 vs 1)", "TFF1 (3 vs 1)",
#     "LYVE1 (2 vs 1)", "REG1B (2 vs 1)", "TFF1 (2 vs 1)",
#     "LYVE1 (3 vs 2)", "REG1B (3 vs 2)", "TFF1 (3 vs 2)"
#   ),
#   Log2FC = c(
#     FC_LYVE1_3vs1, FC_REG1B_3vs1, FC_TFF1_3vs1,
#     FC_LYVE1_2vs1, FC_REG1B_2vs1, FC_TFF1_2vs1,
#     FC_LYVE1_3vs2, FC_REG1B_3vs2, FC_TFF1_3vs2
#   ),
#   pvalue = c(
#     t.test(LYVE1_3_log2, LYVE1_1_log2)$p.value,
#     t.test(REG1B_3_log2, REG1B_1_log2)$p.value,
#     t.test(TFF1_3_log2,  TFF1_1_log2)$p.value,
#     
#     t.test(LYVE1_2_log2, LYVE1_1_log2)$p.value,
#     t.test(REG1B_2_log2, REG1B_1_log2)$p.value,
#     t.test(TFF1_2_log2,  TFF1_1_log2)$p.value,
#     
#     t.test(LYVE1_3_log2, LYVE1_2_log2)$p.value,
#     t.test(REG1B_3_log2, REG1B_2_log2)$p.value,
#     t.test(TFF1_3_log2,  TFF1_2_log2)$p.value
#   )
# )
# 
# 
# EnhancedVolcano(
#   dati_volcano,
#   lab = dati_volcano$Etichetta,   # Etichette (es. "LYVE1 (3 vs 1)")
#   x = "Log2FC",                   # Nome colonna asse X
#   y = "pvalue",                   # Nome colonna asse Y (fa lui -log10 in automatico!)
#   
#   # Soglie di significatività (puoi modificarle a piacere)
#   pCutoff = 0.05,                 # Soglia p-value (linea orizzontale)
#   FCcutoff = 1.0,                 # Soglia |Log2FC| (linee verticali)
#   
#   # Estetica dei punti e delle etichette
#   pointSize = 4.5,
#   labSize = 4.0,
#   drawConnectors = TRUE,          # Disegna le linee che collegano il punto all'etichetta
#   widthConnectors = 0.5,
#   max.overlaps = Inf,             # Forza la visualizzazione di tutte e 9 le etichette
#   
#   # Titoli e legende
#   title = "Volcano Plot - Biomarker Urinari",
#   subtitle = "Confronti tra Control (1), Benign (2) e Malignant (3)",
#   caption = "Soglie: |Log2FC| > 1 e p-value < 0.05",
#   legendPosition = "bottom",
#   legendLabSize = 10,
#   legendIconSize = 4.0
# )


##### VIOLIN PLOT#####
# # 1. Crea la tabella (data.frame) unendo i tuoi 9 vettori log2
# dati_violin <- data.frame(
#   Biomarker = c(
#     rep("LYVE1", length(LYVE1_1_log2) + length(LYVE1_2_log2) + length(LYVE1_3_log2)),
#     rep("REG1B", length(REG1B_1_log2) + length(REG1B_2_log2) + length(REG1B_3_log2)),
#     rep("TFF1",  length(TFF1_1_log2)  + length(TFF1_2_log2)  + length(TFF1_3_log2))
#   ),
#   Gruppo = factor(
#     c(
#       rep("1: Control", length(LYVE1_1_log2)), rep("2: Benign", length(LYVE1_2_log2)), rep("3: PDAC", length(LYVE1_3_log2)),
#       rep("1: Control", length(REG1B_1_log2)), rep("2: Benign", length(REG1B_2_log2)), rep("3: PDAC", length(REG1B_3_log2)),
#       rep("1: Control", length(TFF1_1_log2)),  rep("2: Benign", length(TFF1_2_log2)),  rep("3: PDAC", length(TFF1_3_log2))
#     ),
#     levels = c("1: Control", "2: Benign", "3: Malignant")
#   ),
#   Valore_Log2 = c(
#     LYVE1_1_log2, LYVE1_2_log2, LYVE1_3_log2,
#     REG1B_1_log2, REG1B_2_log2, REG1B_3_log2,
#     TFF1_1_log2,  TFF1_2_log2,  TFF1_3_log2
#   )
# )
# 
# # 2. Disegna il Violin Plot con Boxplot interno e punti dei pazienti
# ggplot(dati_violin, aes(x = Gruppo, y = Valore_Log2, fill = Gruppo)) +
#   geom_violin(trim = FALSE, alpha = 0.6, color = NA) +
#   geom_boxplot(width = 0.15, fill = "white", outlier.shape = NA, alpha = 0.9) +
#   geom_jitter(width = 0.12, size = 1, alpha = 0.35, color = "black") +
#   facet_wrap(~ Biomarker, scales = "free_y") +
#   scale_fill_manual(values = c("#4DBBD5", "#F39B7F", "#E64B35")) +
#   labs(
#     title = "Biomarkers distribution in the 3 groups",
#     x = "",
#     y = "LOG2FC"
#   ) +
#   theme_minimal(base_size = 12) +
#   theme(
#     legend.position = "none",
#     strip.text = element_text(face = "bold", size = 13),
#     axis.text.x = element_text(angle = 20, hjust = 1, face = "bold")
#   )





