#D206 Data cleaning task

#set the workspace to a specific directory
setwd('C:/Users/laksh/Desktop/WGU-MS Data analytics')

#Import raw data set to a new dataframe
medinfo<-read.csv('C:/Users/laksh/Desktop/WGU-MS Data analytics/medical_raw_data.csv')
 
#install required packages and load libraries
install.packages("dplyr")       #for data manipulation tasks
install.packages("ggplot2")     #for creating and customizing visualizations
install.packages("visdat")      # visualization of missing data
install.packages("plyr")        # split-apply-combine pattern for ordinal encoding
install.packages("factoextra")  #for PCA
install.packages("naniar")      #for working with missing data
#Load libraries from installed packages
library(dplyr)
library(ggplot2)
library(visdat)
library(plyr)
library(factoextra) 

#Inspect the newly created  data frame
head(medinfo,10)       #to view the first few rows of a data frame or matrix
str(medinfo)          #compactly display the internal structure of an R object

#check the datatypes of the variables in data set
glimpse(medinfo)

#removing first column which is not in data dictionary
medinfo <- medinfo[,-1]

#renaming survey variables in a meaningful way
colnames(medinfo)[colnames(medinfo)=="Item1"]<-"Timely_admission"
colnames(medinfo)[colnames(medinfo)=="Item2"]<-"Timely_treatment"
colnames(medinfo)[colnames(medinfo)=="Item3"]<-"Timely_visits"
colnames(medinfo)[colnames(medinfo)=="Item4"]<-"Reliability"
colnames(medinfo)[colnames(medinfo)=="Item5"]<-"Treatment_hours"
colnames(medinfo)[colnames(medinfo)=="Item6"]<-"Options"
colnames(medinfo)[colnames(medinfo)=="Item7"]<-"Courteous_staff"
colnames(medinfo)[colnames(medinfo)=="Item8"]<-"Active_listening"
#------------------------------------------------------------------
#Detection of duplicates in the data frame
duplicated(medinfo)

#Count of all duplicates exists in the data frame
sum(duplicated(medinfo))
#------------------------------------------------------------------
#missing values in the data frame / Treating with univariate distribution
#vis_miss from naniar package
vis_miss(medinfo)

#count of all missing values in the data frame
colSums((is.na(medinfo)))


#Histogram of Age variable to inspect the missingness
ggplot(medinfo,aes(x=Age)) + geom_histogram(color="white",fill="gold")+ ggtitle("Histogram of patient Age") + theme(plot.title = element_text(hjust = 0.5)) + xlab("Age") + ylab("count") + geom_histogram(color="white",fill="gold")+ ggtitle("Histogram of patient Age") + theme(plot.title = element_text(hjust = 0.5)) + xlab("Age") + ylab("count")

#summary of Age attribute before imputaion
summary(medinfo$Age)

#treating the missing values by imputing the mean value  since histogram shows nearly uniform distribution
mean_age<-mean(medinfo$Age,na.rm = TRUE)
mean_age<-round(mean_age,digits = 0)  #to round numerical values
medinfo$Age[is.na(medinfo$Age)]<- mean_age

#summary of Age variable after imputaion
summary(medinfo$Age)


#Histogram of Age variable after imputation
ggplot(medinfo,aes(x=Age)) + geom_histogram(color="white",fill="gold")+ ggtitle("Histogram of patient Age") + theme(plot.title = element_text(hjust = 0.5)) + xlab("Age") + ylab("count") 

#To confirm distribution
boxplot(medinfo$Age,col = "bisque")
#---------------------------------------------------------------------
#Histogram of 'Income' variable to inspect the missingness
ggplot(medinfo,aes(x=medinfo$Income)) + geom_histogram(color="white",fill="gold")+ xlab("Income") + ylab("count") + geom_histogram(color="white",fill="gold")

#summary of Income attribute before imputation
summary(medinfo$Income)

#treating the missing values by imputing the median value  since histogram shows a skewed distribution
medinfo$Income[is.na(medinfo$Income)]<-median(medinfo$Income,na.rm = TRUE)

#summary of Age variable after imputation
summary(medinfo$Income)


#Histogram of 'Income' variable after imputation
ggplot(medinfo,aes(x=medinfo$Income)) + geom_histogram(color="white",fill="gold")+ ggtitle("Histogram of Income")+xlab("Income")

#To confirm distribution
boxplot(medinfo$Income,col = "bisque")
#---------------------------------------------------------------------------
#Histogram of 'Children' variable to inspect the missingness
ggplot(medinfo,aes(x=Children)) + geom_histogram(color="white",fill="gold") +xlab("Number of children patient has") + ylab("count") 

#summary of Income attribute before imputation
summary(medinfo$Children)

#treating the missing values by imputing the mean value  since histogram shows a skewed distribution
medinfo$Children[is.na(medinfo$Children)]<-median(medinfo$Children,na.rm = TRUE)

#number of missingness
length(which(is.na((medinfo$Children))))

#summary of Age variable after imputation
summary(medinfo$Children)
#------------------------------------------------------------
#Bar chart of 'soft_drink' variable to inspect the missingness
ggplot(medinfo,aes(x=Soft_drink)) + geom_bar(color="white",fill="gold") 


#treating the missing values by imputing the mod value
mode_funct <- function(x) {
  col_tbl <- table(x)
  names(col_tbl)[which(col_tbl==max(col_tbl))]
}
medinfo$Soft_drink <- ifelse(is.na(medinfo$Soft_drink),mode_funct(medinfo$Soft_drink),medinfo$Soft_drink)
length(which(is.na((medinfo$Soft_drink))))

#summary of 'soft_drink' variable after imputation
summary(medinfo$Soft_drink)


#Bar chart of 'soft_drink' variable after imputation
ggplot(medinfo,aes(x=Soft_drink)) + geom_bar(color="white",fill="gold") 
#--------------------------------------------------------------------------
#summary of 'Overweight' 
summary(medinfo$Overweight)

#Treating null values with imputing mod 
medinfo$Overweight <- ifelse(is.na(medinfo$Overweight),mode_funct(medinfo$Overweight),medinfo$Overweight)
length(which(is.na((medinfo$Overweight))))


#summary of 'Overweight' variable after imputation
summary(medinfo$Overweight)


#Bar chart of 'Overweight' variable after imputation
ggplot(medinfo,aes(x=Overweight)) + geom_bar(color="white",fill="gold") 
#----------------------------------------------------------------------------

#summary of 'Anxiety' 
summary(medinfo$Anxiety)

#Treating null values by imputing mod since it is  yes/no answers
medinfo$Anxiety <- ifelse(is.na(medinfo$Anxiety),mode_funct(medinfo$Anxiety),medinfo$Anxiety)

#inspect number of missingness
length(which(is.na((medinfo$Anxiety))))

#summary of 'Anxiety' variable after imputation
summary(medinfo$Anxiety)


#------------------------------------------------------------------------
#Histogram of Initial days spent in hospital
ggplot(medinfo,aes(x=Initial_days)) + geom_histogram(color="white",fill="gold") + theme(plot.title = element_text(hjust = 0.5))  + xlab("Initial_days patient stayed in hospital ") + ylab(" Count")

#Box plot To confirm distribution
boxplot(medinfo$Initial_days)

#summary of Initial days 
summary(medinfo$Initial_days)

#Treating missingness with mean value since histogram shows uniform distribution
medinfo$Initial_days[is.na(medinfo$Initial_days)] <- mean(medinfo$Initial_days,na.rm = TRUE)


#inspect number of missingness
length(which(is.na((medinfo$Initial_days))))
#-------------------------------------------------------------------------------

#count of all missing values in the data frame
colSums((is.na(medinfo)))
#---------------------------------------------------------
#To check Quantitative variables for outliers
glimpse(medinfo)

#Detecting outliers in Income variable
 medinfo$Income_z <- scale(x=  medinfo$Income)

#Display the first 10 values
head(  medinfo$Income_z,10)

#visualize the Z-score of Income variable
ggplot(  medinfo,aes(x=Income_z))+geom_histogram(bins=20,color="black",fill="gold")+xlab("Histogram of Income Z-score")

#Boxplot of Income variable
ggplot(  medinfo,aes(x=Income))+geom_boxplot(bins=20,color="black",fill="gold")+xlab("Boxplot of Income ")

#From the boxplot it is clear that all values above 100000 are outliers
#--------------------------------------------------------------------------

#Boxplot of Age to detect outliers
ggplot(  medinfo,aes(x=Age))+geom_boxplot(bins=30,color="black",fill="gold")+xlab("Boxplot of Age ")
#There are no outliers present in the age variable
#-------------------------------------------------------------------
#Boxplot of VitD_levels to detect outliers
ggplot(  medinfo,aes(x=VitD_levels))+geom_boxplot(bins=20,color="black",fill="gold")+xlab("Boxplot of Vitamin D level ")
#Vitamin D levels could be a sign of some kind of health issues, altering these values might cause issues
#----------------------------------------------------------------------
#Boxplot of Doc_visits to detect outliers
ggplot(  medinfo,aes(x=Doc_visits))+geom_boxplot(color="black",fill="gold")+xlab("Boxplot of Doc_visits")
#No outliers exists in boxplot 
#--------------------------------------------------------------------------
#Boxplot of VitD_supp to detect outliers
ggplot(  medinfo,aes(x=VitD_supp))+geom_boxplot(bins=30,color="black",fill="gold")+xlab("Boxplot of VitD_supp ")
#Vitamin D supplements intake can also be due to some health issues, altering may cause error in patient record
#-------------------------------------------------------------------------------------
#Boxplot of Initial_days to detect outliers
ggplot(  medinfo,aes(x=Initial_days))+geom_boxplot(bins=30,color="black",fill="gold")+xlab("Boxplot of Initial_days ")
#There are no outliers present in this variable
#--------------------------------------------------------------------
#Boxplot of TotalCharge to detect outliers
ggplot(medinfo,aes(x=TotalCharge))+geom_boxplot(bins=30,color="black",fill="gold")+xlab("Boxplot of Total_charges ")

#values near or above 15000 are outliers

#--------------------------------------------------------------------------------
#Boxplot of Additional_charges to detect outliers
ggplot( medinfo,aes(x=Additional_charges)) + geom_boxplot(bins=30,color="black",fill="gold")+xlab("Boxplot of Additional_charges ")
#values close to 30000 are outliers
 
 #impute extreme outliers from data frame with Null values
 medinfo$Additional_charges[medinfo$Additional_charges > 25000] <- NA
 
 
 #total number of null values 
 length(which(is.na((medinfo$Additional_charges))))
 
 #impute with median values to NULL
 medinfo$Additional_charges[is.na(medinfo$Additional_charges)] <- median( medinfo$Additional_charges,na.rm = TRUE)
 
 #Boxplot of Additional_charges to detect outliers after imputation
 ggplot(medinfo,aes(x=Additional_charges))+geom_boxplot(bins=30,color="black",fill="gold")+xlab("Boxplot of Additional_charges after trating outliers ")

 #Treating outliers in a clinical data wouldn't be a good practice
 
 #------------------------------------------------------------------------------------
 
 
 #To find number of unique values 
  unique(medinfo$Education)
  unique(medinfo$Complication_risk)
 
 #Assign numeric value using 'ordinal encoding'
 edu_num <- revalue(x=medinfo$Education,replace=c("Doctorate Degree"=12,"Master's Degree"=11,"Professional School Degree"=10,"Bachelor's Degree"=9,"Associate's Degree"=8,"Some College, 1 or More Years, No Degree"=7,"Some College, Less than 1 Year"=6,"Regular High School Diploma"=5,"GED or Alternative Credential"=4,"9th Grade to 12th Grade, No Diploma"=3,"Nursery School to 8th Grade"=2, "No Schooling Completed"=1))
 cr_num <-  revalue(x=medinfo$Complication_risk,replace=c("Low"=1,"Medium"=2,"High"=3))
 
 
 #convert variable type to numeric and assign it to newly created variable in dataframe
 medinfo$education_numeric <- as.numeric(edu_num)
 medinfo$complicationrisk_numeric <- as.numeric(cr_num)
 head(medinfo ,10)
 
 
 #Inspect the structure of data frame
  str(medinfo)
 #--------------------------------------------------------
  
  #To find number of unique values 
  unique(medinfo$ReAdmis)
  unique(medinfo$HighBlood)
  unique(medinfo$Soft_drink)
  unique(medinfo$Stroke)
  unique(medinfo$Arthritis)
  unique(medinfo$Diabetes)
  unique(medinfo$Hyperlipidemia)
  unique(medinfo$BackPain)
  unique(medinfo$Allergic_rhinitis)
  unique(medinfo$Reflux_esophagitis)
  unique(medinfo$Asthma)
  
  #Assign numeric value using 'label encoding' for categorical variables
  ReAdmis_num <- revalue(x=medinfo$ReAdmis,replace=c("Yes"=1,"No"=0))
  HB_num <- revalue(x=medinfo$HighBlood,replace=c("Yes"=1,"No"=0))
  softd_num <- revalue(x=medinfo$Soft_drink,replace=c("Yes"=1,"No"=0))
  stroke_num <- revalue(x=medinfo$Stroke,replace=c("Yes"=1,"No"=0))
  Arthritis_num <- revalue(x=medinfo$Arthritis,replace=c("Yes"=1,"No"=0))
  Diabetes_num <- revalue(x=medinfo$Diabetes,replace=c("Yes"=1,"No"=0))
  Hyperlipidemia_num <- revalue(x=medinfo$Hyperlipidemia,replace=c("Yes"=1,"No"=0))
  BackPain_num <- revalue(x=medinfo$BackPain,replace=c("Yes"=1,"No"=0))
  Allergic_rhinitis_num <- revalue(x=medinfo$Allergic_rhinitis,replace=c("Yes"=1,"No"=0))
  Reflux_esophagitis_num <- revalue(x=medinfo$Reflux_esophagitis,replace=c("Yes"=1,"No"=0))
  Asthma_num <- revalue(x=medinfo$Asthma,replace=c("Yes"=1,"No"=0))
  
  #convert type to numeric and assign it to newly created variables in dataframe
  medinfo$ReAdmis_numeric <- as.numeric(ReAdmis_num)
  medinfo$HighBlood_numeric <- as.numeric(HB_num)
  medinfo$softdrink_numeric <- as.numeric(softd_num)
  medinfo$stroke_numeric <- as.numeric(stroke_num)
  medinfo$Arthritis_numeric <- as.numeric(Arthritis_num)
  medinfo$Diabetes_numeric <- as.numeric(Diabetes_num)
  medinfo$Hyperlipidemia_numeric <- as.numeric(Hyperlipidemia_num)
  medinfo$BackPain_numeric <- as.numeric(BackPain_num)
  medinfo$Allergic_rhinitis_numeric <- as.numeric(Allergic_rhinitis_num)
  medinfo$Reflux_esophagitis_numeric <- as.numeric(Reflux_esophagitis_num)
  medinfo$Asthma_numeric <- as.numeric(Asthma_num)
  
 
  #Inspect the structure of data frame
  str(medinfo)
  
  #Exporting clean data file
  write.csv(medinfo,file = "Medical_data_cleaned.csv",row.names = FALSE,sep = ",")
  #------------------------------------------------------------
  #Applying Unsupervised learning technique PCA
   str(medinfo) #To select quantitative continuous/num features for PCA
  
  #discarded categorical variables and other discrete variables
    pcdf <- data.frame(medinfo$Lat,medinfo$Lng,medinfo$Income,medinfo$Age,medinfo$VitD_levels,medinfo$Initial_days,medinfo$TotalCharge,medinfo$Additional_charges)
 
  #Normalize data and apply PCA,center parameter indicates if the data should be centered to zero before analysis
   pcdf.pca <- prcomp(pcdf,center = TRUE,scale = TRUE)
   
   #closer look to loadings, which identifies the variables that contributes most to PCs
  pcdf.pca$rotation
 
  #To find structure of the data and the relationships between the variables.
  summary(pcdf.pca)
  
  #selecting PCs with scree plot
  fviz_eig(pcdf.pca,choice="eigenvalue",addlabels = TRUE)
  
  #Display the exact  eigenvalues
  eigenvalues <- pcdf.pca$sdev^2  #shows the squared standard deviations or eigenvalues
  

  