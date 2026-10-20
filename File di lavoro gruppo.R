# Summarizing data and descriptive analysis
# For each of the 3 biomarkers: mean, median, quantiles, variance, standard deviation,range,interqaurtile range, histogram (dati numerici continui)
# questi calcoli vanno fatti per diagnosi
#Histogram da definire... 

##TROVATE I DATI GIA' NORMALIZZATI NELLE RISPETTIVE COLONNE FILE EXCEL
#La normalizzazione dei dati dei biomarcatori urinari per l'adenocarcinoma duttale pancreatico (PDAC) rispetto alla creatinina è un passaggio fondamentale per correggere la naturale variabilità della concentrazione delle urine tra i pazienti e nei diversi momenti della giornata. [1, 2]
#Senza questo processo, i test di screening o diagnosi precoce basati su pannelli urinari (come il noto pannello a tre biomarcatori LYVE1, REG1B e TFF1) produrrebbero falsi positivi o falsi negativi dovuti semplicemente a quanto il paziente ha bevuto. [1, 2])


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

dataset= read_xlsx("Debernardi et al 2020 data.xlsx", sheet = 'Dataset scremato')[1:8]
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

# MEAN 


#MEDIAN

#QUANTILES

#VARIANCE

#STANDARD DEVIATION 

#RANGE

#INTERQUANTILE RANGE



#### TFF1 ####
# Description an analysis of: TFF1

# MEAN 

#MEDIAN

#QUANTILES

#VARIANCE

#STANDARD DEVIATION 

#RANGE

#INTERQUANTILE RANGE


