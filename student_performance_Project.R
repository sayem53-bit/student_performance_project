library(GGally)
library(ACSWR)
install.packages("olsrr")
devtools::install_github("rsquaredacademy/olsrr")
library(olsrr)
library(tidyr)
library(prettydoc) #template
library(readr) #for reading in the data
library(dplyr) #exploratory data analysis
library(ggplot2)#visualizations
library(tidyverse)#tidying up the data and exploratory data analysis
library(gsheet)#Reading from google sheets
library(readxl) #Reading in Excel Files
library(knitr) #Kable function for printing tables
library(DT) #Printing dataframes concisely
library(caret) #Dummy Variables for Correlation Matrix
library(gridExtra) #Multiple plots 
library(rpart) #Decision Tree
library(factoextra)

install.packages("GGally")
library(GGally)
install.packages("tibble")
library(tibble)
install.packages("dplyr")   # Only once
library(tibble)
library(dplyr)


temp <- tempfile()
download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/00356/student.zip",temp, mode="wb")
unzip(temp, "student-mat.csv")
data <- read.table("student-mat.csv",sep= ";", header= T)
unlink(temp)
(data <- as_tibble(data))
head(data)
#Data Cleaning
#The data is already cleaned and in a tidy format and does not require much cleaning for the data analysis. 
#The majority of the cleaning is done for selecting the necessary variables.

#R.version.string
#If it says R 4.1+, use |>
 # If older, you must use %>% and load dplyr.
remove.packages("rlang")
install.packages("rlang")

data <- data %>%
  as_tibble()%>%
  select(sex, age, address,Pstatus, Medu, Fedu, Mjob, Fjob,studytime,traveltime,failures,higher,internet, 
         goout, Dalc,Walc,health, absences,G1,G2,G3)
DT::datatable(data)


glimpse(data)

packageVersion("rlang")

library(tidyverse)
find.package("rlang")

data <- data |>
  tibble::as_tibble() |>
  dplyr::select(sex, age, address, Pstatus, Medu, Fedu,
                Mjob, Fjob, studytime, traveltime,
                failures, higher, internet,
                goout, Dalc, Walc, health,
                absences, G1, G2, G3)




#Simple Exploratory Data Analysis
#Number of Male vs Female Students who failed in the final exam

(data2<-data%>%
    mutate(pass=ifelse(G3>=10,1,0), fail= ifelse(G3<10,1,0))%>%
    filter(sex=="F"|sex=="M")%>%
    group_by(sex)%>%
    summarise(Pass=sum(pass), 
              Fail=sum(fail)))
data2%>%
  ggplot(aes(x=sex,y=Fail))+
  geom_bar(stat="identity")

#We find that the number of female students who have failed is higher than the number of male students who have failed.


#View(data)
#summary(data)

boxplot(data)

## DATA cleaning: Bad data leads to bad model
## Clean the data
data<-drop_na(data)
## Attach the data

##Missing data: There is no missing data in the dataset.

sum(is.na(data))

## Attach the data
attach(data)
plot(data)


res.pca <- prcomp(data <- data %>%
                    as_tibble()%>%
                    select( age, Medu, Fedu,studytime,traveltime,failures, 
                           goout, Dalc,Walc,health, absences,G1,G2,G3), scale = TRUE)



fviz_eig(res.pca)


#################################


#you get a scree plot with three pieces of information:
  
#  Eigenvalues (bar height)

#Percentage of variance explained by each PC

#Cumulative variance explained (often a line)

#Each bar = one principal component.


######################---

fviz_pca_biplot(res.pca, repel = TRUE)

#What a PCA biplot shows
#  Points → individual students

#Arrows → variables (age, G1, G2, failures, goout, etc.)

#Axes:

# X-axis = PC1 (Academic Performance & Engagement)

#Y-axis = PC2 (Lifestyle & Social Behavior)

#Interpreting students along PC1 (left ↔ right)
#Right side of the plot (high PC1)

#Students on the right tend to have:
  
 # Higher G1, G2, G3

#More studytime

#Fewer failures

#Fewer absences
#Interpretation:
  #These are academically strong and engaged students.

#Left side of the plot (low PC1)

#Students on the left tend to have:
  
 # More failures

#More absences

#Lower grades

# Interpretation:
#  These are students experiencing academic difficulty or disengagement.

#Interpreting students along PC2 (down ↔ up)
#Upper part of the plot (high PC2)

#Students higher up tend to align with:
  
 # Higher goout

#Higher Dalc / Walc

#More social activity

# Interpretation:
 # Students with a more active social lifestyle.

#Lower part of the plot (low PC2)

#Students lower down tend to align with:
  
 # Better health

#Lower alcohol consumption

#More routine-oriented behavior

# Interpretation:
 # Students with a more restrained or health-focused lifestyle.

############################################################

#What counts as an outlier in a PCA biplot?
  
 # A student is considered an outlier if they are:
  
#1. Far from the origin (0,0) on PC1 and/or PC2

#2.Isolated from other points

#3.Strongly aligned with one or two variable arrows

#Distance from the center means the student contributes disproportionately to variation.



#############################################################


pca_data <- data %>%
  select(age, Medu, Fedu, studytime, traveltime, failures,
         goout, Dalc, Walc, health, absences, G1, G2, G3) %>%
  as.data.frame()
pca_res <- prcomp(pca_data, scale. = TRUE)

fviz_pca_var(
  pca_res,
  col.var = "contrib",  # Color by contribution
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE
)


pca_data <- data %>%
  select(age, Medu, Fedu, studytime, traveltime, failures,
         goout, Dalc, Walc, health, absences, G1, G2) %>%
  as.data.frame()
pca_res <- prcomp(pca_data, scale. = TRUE)

fviz_pca_var(
  pca_res,
  col.var = "contrib",  # Color by contribution
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE
)




pca_data <- data %>%
  select(age, Medu, Fedu, studytime, traveltime, failures,
         goout, Dalc, Walc, health, absences, G1) %>%
  as.data.frame()
pca_res <- prcomp(pca_data, scale. = TRUE)

fviz_pca_var(
  pca_res,
  col.var = "contrib",  # Color by contribution
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE
)

# Angle between arrows

#Small angle → strong positive correlation

#180° apart → strong negative correlation

#~90° → no correlation

#Example:
  
 # G1, G2, G3 tightly grouped → very strong correlation

#failures pointing opposite grades → negative relationship

#🔹 Arrow length

#Longer = more important to PCA

#Short arrows near origin = weak contributors




get_pca_var(pca_res)$contrib

pca_res$rotation[, 1:2]
#pca_res$rotation

#A principal component is driven by variables that have:
  
#  Large absolute loadings (far from 0)

#High contribution (%)

#Long arrows in the PCA variable plot

#Similar directions (positive correlation) or opposite directions (negative correlation)

#PC1 explains the most variance, PC2 the second most, and they are independent axes.
#|loading| > 0.30 = meaningful

#|loading| > 0.50 = strong driver
#
#Likely strong drivers of PC1:
  
#  G1, G2, G3

#failures

#studytime

#absences

#Interpreting PC2 in your dataset

#PC2 usually captures behavioral or lifestyle variation that is not explained by grades.

#Likely strong drivers of PC2:
  
#  goout

#Dalc

#Walc

#health

#traveltime

#PC1 represents overall academic performance, driven primarily by grades (G1, G2, G3), failures, study time, and absences.

#PC2 reflects lifestyle and behavioral factors, dominated by social activity (goout), alcohol consumption (Dalc, Walc), health, and travel time, largely independent of academic outcomes.


#Summary Interpretation

#In summary, the PCA decomposes student characteristics into two dominant dimensions:
  
# PC1: Academic Performance and Engagement

#PC2: Lifestyle and Social Behavior

#Together, these components provide a concise and interpretable structure for understanding variation in student outcomes, highlighting the separation between scholastic achievement and behavioral factors within the dataset.


#######################

pca_res <- prcomp(
  data %>% 
    select(age, Medu, Fedu, studytime, traveltime, failures,
           goout, Dalc, Walc, health, absences, G1, G2, G3),
  scale. = TRUE
)

biplot(pca_res, scale = 0)

fviz_pca_biplot(res.pca, repel = TRUE)

#biplot(G3~., scale = 0)

modelfull=lm(G3~., data=data)  #G3= Final Grade
summary(modelfull)

install.packages("lm.beta")
library(lm.beta)

std_model <- lm.beta(modelfull)
summary(std_model)


coefs <- as.data.frame(std_model$standardized.coefficients)
coefs$variable <- rownames(coefs)
colnames(coefs)[1] <- "beta"

coefs <- coefs[coefs$variable != "(Intercept)", ]

library(ggplot2)

ggplot(coefs, aes(x = reorder(variable, beta), y = beta)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Variable Impact on Final Grade (Standardized)",
       x = "Predictor",
       y = "Standardized Coefficient")



modelfull
ggpairs(data)
# Model
#G3=-0.258872+0.381902*sexM+0.216828*age-0.112499*addressU+0.237226*PstatusT-0.103654* Medu+0.134246*Fedu+0.202026*Mjobhealth-0.202026*Mjobother+0.008868*Mjobservices-0.082790*Mjobteacher-0.944147*Fjobhealth-0.853139*Fjobother-0.966557*Fjobservices+0.122806 *Fjobteacher+0.210159*studytime+0.197775 *traveltime-0.233737*failures +0.357509 *higheryes-0.159236*internetyes +0.020357*goout-0.073707*Dalc-0.126525*Walc*0.009846*health+absences*0.002224+0.588469*G2+0.122729*G3        


set.seed(123)
n <- nrow(data)
train_index <- sample(1:n, 0.7*n)
train <- data[train_index, ]
test  <- data[-train_index, ]
full_train <- lm(G3 ~ ., data = train)
reduced_train <- lm(G3 ~ G2 + G1 + absences + age, data = train)
pred_full <- predict(full_train, test)
pred_reduced <- predict(reduced_train, test)
#Compute accuracy metrics RMSE
rmse_full <- sqrt(mean((test$G3 - pred_full)^2))
rmse_reduced <- sqrt(mean((test$G3 - pred_reduced)^2))

rmse_full
rmse_reduced

mae_full <- mean(abs(test$G3 - pred_full))
mae_reduced <- mean(abs(test$G3 - pred_reduced))





modelint=lm(G3~1, data=data)
modelint

stepforward=step(modelint,direction = "forward", scope = formula(modelfull))
stepforward
summary(stepforward)
#AIC=514.33
#G3 ~  G2 + G1 + absence + Age 


stepback=step(modelfull,direction = "backward")
stepback
summary(stepback)

#Step:  AIC=514.3
#G3 ~ age + failures + Walc + absences + G1 + G2
# backward model is better than forward model due to lower AIC 

stepbothfull=step(modelfull,direction = "both")
stepbothfull
summary(stepbothfull)
#Step:  AIC=514.3
#G3 ~  G2+G1+Walc+failures+age+absences 

# backward and both model has same AIC. The best model is G3 ~  G2+G1+Walc+failures+age+absences 




modelFinal=lm(G3~age + failures + Walc + absences + G1 + G2, data=data)
summary(modelFinal)
modelFinal

AIC(modelFinal,stepbothfull,stepback)


modelFinalwithoutG2=lm(G3~age + failures + Walc + absences + G1 , data=data)
summary(modelFinalwithoutG2)
modelFinalwithoutG2

anova(modelFinal, modelfull)

---------
 
  AIC(modelfull,stepforward,stepback,stepbothfull)

AIC(stepforward)


   
  AIC(stepforward,stepback,stepbothfull)  
  
anova(modelfull,stepforward,stepback,stepbothfull)

AIC(modelfull,stepforward,stepback,stepbothfull)

anova(stepforward,stepback,stepbothfull)


abline(stepforward, col="blue")

anova(stepforward,stepback,stepbothfull,modelFinalwithoutG2)
#There is no significant difference between the final model including only Age, Ed, Ex0, U2, W, X and the full model. Therefore, I choose the simpler model



coef(modelFinal)

plot(modelFinal)

#Test1:constant variance of residuals : Breusch–Pagan test (most common formal test)
library(lmtest)
bptest(modelFinal)

#bptest(modelFinal, ~ fitted(modelFinal) + I(fitted(modelFinal)^2))


shapiro.test(residuals(modelFinal))

AIC(modelfull) 
#1666.252


AIC(modelFinal) 
#1637.258

BIC(modelfull) 
#1777.661

BIC(modelFinal) 
#1669.089


anova(modelFinal, modelfull)
# anova TEST INDICATE THAT There is no significant difference between the final 
#model including only Age, Ed, Ex0, U2, W, X and the full model. Therefore, I choose the simpler model

residuals(modelFinal)
## Residuals : Difference between an observed value and the value predicted by a regression model.

boxplot(residuals(modelFinal))


#Boxplot is a quick sanity check on your regression errors. It doesn’t test one assumption by itself, but it gives fast clues about center, spread, symmetry, and outliers in the residuals
#the residuals appear to be normally distributed

#Good boxplot

#Residuals are centered near zero with roughly symmetric spread and no extreme outliers.

#Problematic boxplot

#Residuals show skewness and several extreme values, indicating departures from normality and potential influential observations.




#You are plotting the distribution of residuals (observed − fitted values).

#Ideally, residuals should:
  
 # Be centered around 0

#Be roughly symmetric

#Have no extreme outliers





#Check whether a statistical model (usually linear regression) satisfies its required assumptions so the results are valid.

# test 1 : Linearity
plot(modelFinal, which = 1)  ## Residuals vs Fitted
#Good:Random scatter around 0

library(lmtest)
raintest(modelFinal)



#Test 2. Normality of residuals
plot(modelFinal, which = 2)
shapiro.test(residuals(modelFinal))  


#Interpretation:
# p > 0.05 → normality acceptable

#p ≤ 0.05 → deviation from normality
# We fail to reject the null hypothesis in the shapiro test meaning that their is no evidence 
# showing the the residuals are not normally distributed.  

## There is one outlier.
#qqnorm(residuals(modelFinal))
#qqline(residuals(modelFinal))
#shapiro.test(residuals(modelFinal))

#Test 3 :Constant variance of residuals (Homoscedasticity)
plot(modelFinal, which = 3)   # Scale–Location
library(lmtest)
bptest(modelFinal)   

#p > 0.05 → constant variance

#p ≤ 0.05 → heteroscedasticity

# Test 4: Independence of errors

library(lmtest)
dwtest(modelFinal)  

#Interpretation:
# DW ≈ 2 → independence

#DW < 2 → positive autocorrelation

#DW > 2 → negative autocorrelation




#Test 5: Multicollinearity (important extra assumption)
library(car)
vif(modelFinal)
#interpretation:
  
#Interpretation:
  
#VIF < 5 → acceptable

#VIF > 10 → serious multicollinearity

# numbers greater than 5 or 10 you should be suspicious. Close to one is best.



#Test 6: Influential observations / outliers
plot(modelFinal, which = 4)

#Rules of thumb:
  
 # Cook’s D > 1 → influential

#Investigate, don’t auto-delete

###############################Testing done



par(mfrow = c(2, 2))
plot(modelFinal)


#drop W due to it have a variance inflation factor of 8.7.
modelvif=lm(R ~ Age + Ed + Ex0 + U2+X)
summary(modelvif)

vif(modelvif)
#The variance inflation factor is less than 5 for all independent variables. 
  




















d1=read.table("student-mat.csv",sep=";",header=TRUE)
d2=read.table("C:/Users/sasoy/Desktop/FIU/IDC-Capstone Course/student/student-por.csv",sep=";",header=TRUE)

d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
print(nrow(d3)) # 382 students
d4=d3
View(d4)

##Steps 1. Collect 2. Clean 3.Analyze 4.Report
## understand the structure of data
# Look at data
## Visualize data
## Clean the data 

class(d4)
str(d4)

## DATA cleaning: Bad data leads to bad model
## Clean the data
d4<-drop_na(d4)
## Attach the data
attach(d4)
plot(d4)




boxplot(d4)
##, data = d4, xlab = "Number of Cylinders",
##        ylab = "Miles Per Gallon", main = "Mileage Data")



modelforward <- lm( d4 ~ ., data = d4,direction="forward")

summary(modelforward)

#stepwise search for best regression model
# Train the model
# test model
# test accouracy
# predict student performance based on parameters 





#Decision Tree Analysis


g2a<-data %>% 
  group_by(address)%>%
  ggplot(aes(x=factor(Dalc), y= G3))+
  geom_jitter(alpha=0.6)+
  scale_x_discrete("WorkDay Alcohol")+
  scale_y_continuous("Grade")+
  facet_grid(~address)
g2b<-data %>% 
  group_by(address)%>%
  ggplot(aes(x=factor(Walc), y= G3))+
  geom_jitter(alpha=0.6)+
  scale_x_discrete("Weekday Alcohol")+
  scale_y_continuous("Grade")+
  facet_grid(~address)
grid.arrange(g2a,g2b,ncol=2)


#Relationship between access to internet and the performance of the students.

data%>%
  group_by(internet)%>%
  ggplot(aes(x=G3, fill=internet))+
  geom_density( alpha=0.5)
#We find that the presence of internet is actually detrimental to the average grades of the student.


#The current performance Vs future goals of the student based on gender of the student
data%>%
  ggplot(aes(x=higher, y=G3))+
  geom_boxplot()+
  facet_grid(~sex)
#We find that on an average, students who are planning on a higher education perform better than those who do not. We also find that Male students outperform female students when they plan to go for higher education and viceversa if not.
