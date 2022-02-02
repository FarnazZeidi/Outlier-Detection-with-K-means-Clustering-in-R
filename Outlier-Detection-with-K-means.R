
# Outlier-Detection-with-K-means
# By: Farnaz ZEIDI
# Date: 2020


####################################################
##Importing Libraries
####################################################
library(VIM)
library(class)
library(caret)
library(cluster)

####################################################
##Loading The Data
####################################################
#https://www.kaggle.com/uciml/pima-indians-diabetes-database
#Pima Indians Diabetes Database
DataSet <- read.csv( "diabetes.csv",header = TRUE, sep = ",")
View(DataSet)
summary(DataSet)


##########################################################################################
################################### Data Preprocessing ################################### 
##########################################################################################
#sutun isimlerini degistirme
colnames(DataSet) <- c("GS", "Glikoz", "KanBasinci","TDKK","Insulin", "BMI", "DSF", "Yas", "Sonuc")


#Changing the last column to factor
DataSet$Sonuc <- as.factor(DataSet$Sonuc)
levels(DataSet$Sonuc) <- c("Saglam", "Hasta")


#"The levels of a factor are re-ordered so that the level specified by ref is first and the others are moved down.
DataSet$Sonuc <- relevel(DataSet$Sonuc, ref = "Hasta")
table(DataSet$Sonuc)

####################################################
##k-Nearest Neighbour Imputation (Filling missing values)
####################################################

# install.packages("VIM")
library(VIM)
DataSet<-kNN(data=DataSet,variable=c(2,3,4,6), k=17)
View(DataSet)
summary(DataSet)

#Removing extra columns 
DataSet<-DataSet[,1:9]


####################################################
##Normalization
####################################################

#install.packages("clusterSim")
library(clusterSim)
library(cluster)

DataSet[,1:8] <- data.Normalization(DataSet [,1:8],type="n4",normalization="column")
summary(DataSet)


####################################################
##K-Means: Outlier Detection
####################################################

#Kadhm et al who study on the "PIMA Indian Type-2 diabetes database", proposed 10 clusters as the best cluster number for removing the outliers.
#Therefore, 10 clusters have been used to remove outliers with the K-means method.
k<-10
set.seed(4)
k_DataSet <- kmeans(DataSet[,1:8], k)

DataSet$KMeanCValue <- k_DataSet$cluster
table(DataSet$KMeanCValue, DataSet$Sonuc)


##Removing outliers(188 lines)

DataSet<-DataSet[which((DataSet$Sonuc=="Saglam" & DataSet$KMeanCValue==1) | (DataSet$KMeanCValue!=1) ),]
DataSet<-DataSet[which((DataSet$Sonuc=="Hasta" & DataSet$KMeanCValue==2) | (DataSet$KMeanCValue!=2) ),]
DataSet<-DataSet[which((DataSet$Sonuc=="Saglam" & DataSet$KMeanCValue==3) | (DataSet$KMeanCValue!=3) ),]
DataSet<-DataSet[which((DataSet$Sonuc=="Saglam" & DataSet$KMeanCValue==4) | (DataSet$KMeanCValue!=4) ),]
DataSet<-DataSet[which((DataSet$Sonuc=="Hasta" & DataSet$KMeanCValue==5) | (DataSet$KMeanCValue!=5) ),]
DataSet<-DataSet[which((DataSet$Sonuc=="Saglam" & DataSet$KMeanCValue==6) | (DataSet$KMeanCValue!=6) ),]
DataSet<-DataSet[which((DataSet$Sonuc=="Saglam" & DataSet$KMeanCValue==7) | (DataSet$KMeanCValue!=7) ),]
DataSet<-DataSet[which((DataSet$Sonuc=="Hasta" & DataSet$KMeanCValue==8) | (DataSet$KMeanCValue!=8) ),]
DataSet<-DataSet[which((DataSet$Sonuc=="Saglam" & DataSet$KMeanCValue==9) | (DataSet$KMeanCValue!=9) ),]
DataSet<-DataSet[which((DataSet$Sonuc=="Hasta" & DataSet$KMeanCValue==10) | (DataSet$KMeanCValue!=10) ),]

table(DataSet$KMeanCValue, DataSet$Sonuc)



# References
#Kadhm, M. S., Ghindawi, I. W., & Mhawi, D. E. (2018). An accurate diabetes prediction system based on K-means clustering and proposed classification approach. International Journal of Applied Engineering Research, 13(6), 4038-4041.
#Campos, G. O., Zimek, A., Sander, J., Campello, R. J., Micenková, B., Schubert, E., et al. (2016). On the evaluation of unsupervised outlier detection: measures, datasets, and an empirical study. Data Mining and Knowledge Discovery, 30(4), 891-927.
