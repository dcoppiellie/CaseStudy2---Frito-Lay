library(mlbench)
library(caret)
library(mlr)
library(tidyverse)
library(ggthemes)
library(gplots)
library(randomForest)
library(skimr)
library(corrplot) 
library(tidyverse)
library(cowplot)
library(GGally)
library(class)
library(e1071)
######################################
##  Read in Files
######################################

Frito <- read.csv("C:\\Users\\Owner\\Desktop\\SMU DataScience\\DS6306\\Unit 14 and 15 Case Study 2\\CaseStudy2-data.csv",
                  strip.white = TRUE,
                  header = TRUE)

# Always important to look at the basic structure first
str(Frito) 
summary(Frito)

length(Frito)
skim(Frito)
########Removing columns (Employee Count and Standard hours) with a single value from data######
##Eliminated variables "EmployeeCount and StandardHours because they only contained a single value
##Eliminated variable "EmployeeNumber" "ID" and "Over18" as being irrelevant to any potential attrition analysis
fl <- Frito %>% select(Age, Attrition, BusinessTravel,	DailyRate,	Department,
                       DistanceFromHome,	Education,	EducationField, EnvironmentSatisfaction,
                       Gender,	HourlyRate,	JobInvolvement,	JobLevel,	JobRole,	JobSatisfaction,
                       MaritalStatus,	MonthlyIncome,	MonthlyRate, NumCompaniesWorked,
                       OverTime,	PercentSalaryHike,	PerformanceRating,	RelationshipSatisfaction,
                       StockOptionLevel,	TotalWorkingYears,	TrainingTimesLastYear,	WorkLifeBalance,
                       YearsAtCompany,	YearsInCurrentRole,	YearsSinceLastPromotion,	YearsWithCurrManager)

####Numeric:Numeric Variable elimination###########
fl %>% keep(is.numeric) %>% cor %>% corrplot("upper", addCoef.col = "black", number.digits = 2,
                                                            number.cex = 0.5, method="square",
                                                            order="hclust",
                                                            title="Variable Corr Heatmap",
                                                            tl.srt=45, tl.cex = 0.8)


correlator  <-  function(fl){
  Frito %>%
    keep(is.numeric) %>%
    tidyr::drop_na() %>%
    cor %>%
    corrplot("upper", addCoef.col = "white", number.digits = 2,
             number.cex = 0.5, method="square",
             order="hclust", title="Variable Corr Heatmap",
             tl.srt=45, tl.cex = 0.8)
  }

#######Eliminate variables with correlation with each other###########
##I decided to use a 75% correlation threshold to eliminate similar variables that convey like information.
##With this, the assumed strongest indicator was chosen.
##For example, the correlation between PercentSalaryHike and PerformanceRating show strong correlation.
##PercentSalaryHike was selected as being more indicative of attrition as people are more likely to stay in a job where their salary increases.
##Likewise, a person is less likely to leave a job as a high performer, except for a potential increase in pay at another job.
##The PercentSalaryHike variable should represent this.  This same logic applies to JobLevel against MonthlyIncome, and MonthlyIncome against TotalWorkingYears, and TotalWorkingYears against JobLevel.
##YearsAtCompany, and YearsinCurrentRole and YearsWithCurrManager also share a correlation.
##YearsAtCompany was selected because the role of the manager and job responsibilities will be included in the time at the company.
##RelationshipSatisfaction and MaritalStatus seem redudant, so relationship satisfaction was removed.
##This same logic applies to OverTime and WorkLifeBalance, and WorkLifeBalance seems more inclusive.
##Additonally, DailyRate, HourlyRate, and MonthlyRate would seem to be included in the MonthlyIncome calculations, and are therefore redundant with TotalWorkingYears.
fl2 <- Frito %>% select(Age, Attrition, BusinessTravel,	Department,
                       DistanceFromHome,	Education,	EducationField, EnvironmentSatisfaction,
                       Gender,	JobInvolvement,	MonthlyIncome, JobRole,	JobSatisfaction,
                       MaritalStatus, NumCompaniesWorked,
                       PercentSalaryHike,
                       StockOptionLevel,	TrainingTimesLastYear,	WorkLifeBalance,
                       YearsAtCompany,	YearsSinceLastPromotion)

skim(fl2)

#############Numeric:Catergorical variable elimination#####################
##Convert categorical variables to factors###
fl2$Attrition <- as.factor(fl2$Attrition)
fl2$BusinessTravel <- as.factor(fl2$BusinessTravel)
fl2$Department <- as.factor(fl2$Department)
fl2$Education <- as.factor(fl2$Education)
fl2$EducationField <- as.factor(fl2$EducationField)
fl2$EnvironmentSatisfaction <- as.factor(fl2$EnvironmentSatisfaction)
fl2$Gender <- as.factor(fl2$Gender)
fl2$JobInvolvement <- as.factor(fl2$JobInvolvement)
fl2$JobRole <- as.factor(fl2$JobRole)
fl2$JobSatisfaction <- as.factor(fl2$JobSatisfaction)
fl2$MaritalStatus <- as.factor(fl2$MaritalStatus)
fl2$WorkLifeBalance <- as.factor(fl2$WorkLifeBalance)

skim(fl2)

# x : y
# numeric : categorical

fl2$rvar <- rnorm(nrow(fl2))

length(unique(fl2$JobRole))
# [1] 2

ggplot(data = fl2) + geom_density(aes_string(x = "rvar", fill = "Attrition"), alpha = 0.5)

# step 1, save target variable name
target <- "Attrition"
# step 2, save explanator variable names
numvars <- fl2 %>% keep(is.numeric) %>% colnames


numplot <- function(df, explan, resp) {
  ggplot(data = df) + geom_density(aes_string(x = explan, fill = resp), alpha = 0.5)
}

numplot(fl2, explan = "YearsAtCompany", resp = "Attrition")

plotlist <- lapply(numvars, function(x) numplot(fl2, x, "Attrition"))

plot_grid(plotlist = plotlist)

#########Based on above analysis, more variables may be elminated.
##For example, Age, PercentSalaryHike, NumCompaniesWorked, YearsSinceLastPromotion, TrainingTimesLastYear, YearsAtCompany

fl3 <- Frito %>% select(Attrition, BusinessTravel,	Department,
                        DistanceFromHome,	Education,	EducationField, EnvironmentSatisfaction,
                        Gender,	JobInvolvement,	MonthlyIncome, JobRole,	JobSatisfaction,
                        MaritalStatus,
                        StockOptionLevel,	WorkLifeBalance)

skim(fl3)
########Categorical:Categorical variable elmination##############
ggplot(data = fl3) + geom_bar(aes(x = WorkLifeBalance, fill = Attrition), position = "fill", alpha = 0.9) + coord_flip()


ones <- rep(1, nrow(fl3))
zeroes <- rep(0, nrow(fl3))
onezeroes <- c(ones, zeroes)

fl3$rcat <- sample(onezeroes, nrow(fl3))


ggplot(data = fl3) + geom_bar(aes(x = rcat, fill = Attrition), position = "fill", alpha = 0.9) + coord_flip()

# step 1: Name target variable:

target <- "Attrition"

# step 2: name explanatory vars

expls <- fl3 %>% keep(is.factor) %>% colnames


catplot <- function(df, x,y){
  ggplot(data = df, aes_string(x = x, fill = y)) + 
    geom_bar(position = "fill", alpha = 0.9) + 
    coord_flip()
}


plotlist2 <- lapply(expls, function(x) catplot(fl3, x, target))
plot_grid(plotlist = plotlist2)
########################Last Cut#########################
##At this point, an assumption will be made that EnvironmentSatisfacation and JobSatisfaction are redundant, and JobStatisfaction will be kept.
##Same assumption applies to BusinessTravel, WorkLifeBalance and JobSatisfacation, and JobSatisfaction will be kept.
##Another assumption that was applied was that Education, and JobRole can be accounted for in JobLevel.
fl4 <- Frito %>% select(Attrition, DistanceFromHome, JobInvolvement, 
                        MonthlyIncome, JobSatisfaction, MaritalStatus,
                        StockOptionLevel)
 
skim(fl4)


fl4 %>% ggpairs(aes(color= Attrition))
#######################Final Cut#####################
###From ggpairs plot, JobInvolvement, JobSatisfaction, and MaritalStatus appear to have the same or similar means, and can be eliminated.
flfinal <- Frito %>% select(Attrition, DistanceFromHome, MonthlyIncome, StockOptionLevel)

skim(flfinal)
#####KNN Model######################
flfinal$Attrition <- as.factor(flfinal$Attrition)
flfinal$StockOptionLevel <- as.numeric(flfinal$StockOptionLevel)

# Spit Data set into a training Data set and a testing dataset. @ 75/25
sp = 0.75  

for (seed in 1:100)
{
  set.seed(seed)
  TrainingRows = sample(1:dim(flfinal)[1],round(sp * dim(flfinal)[1])) # Calculate Training Rows
  fl_train = flfinal[TrainingRows,]  # Split into 2 seperate data frames. Include Training Rows
  fl_test = flfinal[-TrainingRows,]  # Exclude Training Rows (Testing Rows)
  classifications = knn(fl_train[,c(2,3,4)], fl_test[,c(2,3,4)],
                        fl_train$Attrition, k=7, prob = TRUE)
  table(fl_test$Attrition, classifications)
  cm = confusionMatrix(table(fl_test$Attrition, classifications))
  AccHolder[seed] = cm$overall[1]
  SensHolder[seed] = cm$byClass[1]
  SpecHolder[seed] = cm$byClass[2]
}

cm
###Attrition prediction#####
attrition <- read.csv("C:\\Users\\Owner\\Desktop\\SMU DataScience\\DS6306\\Unit 14 and 15 Case Study 2\\CaseStudy2CompSet No Attrition.csv", 
                      strip.white = TRUE,
                      header = TRUE)

attrition$Attrition <- knn(fl_train[,c(2,3,4)], attrition[,c("DistanceFromHome", "MonthlyIncome", "StockOptionLevel")], fl_train$Attrition, k=7, prob = TRUE)

write.csv(attrition, "C:\\Users\\Owner\\Desktop\\SMU DataScience\\DS6306\\Unit 14 and 15 Case Study 2\\Case2PredictionsCoppiellie Attrition.csv")
####Naive-Bayes Model######
# Find Average etc. of 100 train / test splits with model
AccHolder = numeric(100)
SensHolder = numeric(100)
SpecHolder = numeric(100)

for (seed in 1:100)
{
  set.seed(seed)
  trainIndices = sample(seq(1:length(flfinal$Attrition)),round(.7*length(flfinal$Attrition)))
  trainfl = flfinal[trainIndices,]
  testfl = flfinal[-trainIndices,]
  model = naiveBayes(trainfl[,c("DistanceFromHome", "MonthlyIncome","StockOptionLevel")],factor(trainfl$Attrition, labels = c("No", "Yes")))
  CM = confusionMatrix(table(factor(testfl$Attrition, labels = c("No", "Yes")),predict(model,testfl[,c("DistanceFromHome", "MonthlyIncome","StockOptionLevel")])))
  AccHolder[seed] = CM$overall[1]
  SensHolder[seed] = CM$byClass[1]
  SpecHolder[seed] = CM$byClass[2]
}

CM
#########################Linear Regression###################################
flfinal2 <- Frito %>% select(Attrition, DistanceFromHome, MonthlyIncome, StockOptionLevel, JobLevel, MaritalStatus)
skim(flfinal2)
########### Linear Regression Model#########################
trainIndices = sample(seq(1:length(flfinal2$MonthlyIncome)),round(.7*length(flfinal2$MonthlyIncome)))
trainfl2 = flfinal2[trainIndices,]
testfl2 = flfinal2[-trainIndices,]
fit2 <- lm(MonthlyIncome~DistanceFromHome + StockOptionLevel + Attrition + JobLevel, data=flfinal2)
monthinc <- predict(fit2, testfl2)

summary(fit2)
confint(fit2)
########Salary Predictions#############
nsalary <- read.csv("C:\\Users\\Owner\\Desktop\\SMU DataScience\\DS6306\\Unit 14 and 15 Case Study 2\\CaseStudy2CompSet No Salary.csv", 
                   strip.white = TRUE,
                   header = TRUE)

str(nsalary) # I see car information: 2 factor columns, 3 num columns, and 4 int columns
summary(nsalary)

nsalary$salary <- predict(fit2, salary)
write.csv(nsalary, "C:\\Users\\Owner\\Desktop\\SMU DataScience\\DS6306\\Unit 14 and 15 Case Study 2\\Case2PredictionsCoppiellie Salary.csv")
