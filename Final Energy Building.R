energy = read.csv("C:/Users/Nandita/OneDrive - McGill University/Sem 4/MGSC 401/FINAL/energydata.csv")
attach(energy)
View(energy)
names(energy)

install.packages("caret")
install.packages("dplyr")
library(tree)
library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)
library(ggplot2)
library(GGally)
library(readr)
library(ggfortify)
library(caret)
library(stargazer)
library(psych)
library(dplyr)
library(car)
library(stargazer)

energy$orientation = as.factor(energy$orientation)

##Random Forest
myforestheating = randomForest(heating_load~relative_compactness+surface_area+wall_area+roof_area+overall_height+orientation+glazing_area+glazing_area_distribution, ntree=500, data = energy, importance = TRUE, na.action = na.omit)
myforestheating

myforestcooling = randomForest(cooling_load~relative_compactness+surface_area+wall_area+roof_area+overall_height+orientation+glazing_area+glazing_area_distribution, ntree=500, data = energy, importance = TRUE, na.action = na.omit)
myforestcooling

##var implot 
importance(myforestheating)
varImpPlot(myforestheating, main = "Variable Importance for Heating Load")

importance(myforestcooling)
varImpPlot(myforestcooling, main = "Variable Importance for Cooling Load")

##random forest
myforestheating = randomForest(heating_load~relative_compactness+surface_area+wall_area+roof_area+overall_height+orientation+glazing_area+glazing_area_distribution, ntree=500, data = energy, importance = TRUE, na.action = na.omit, do.trace = 50)
myforestheating

myforestcooling = randomForest(cooling_load~relative_compactness+surface_area+wall_area+roof_area+overall_height+orientation+glazing_area+glazing_area_distribution, ntree=500, data = energy, importance = TRUE, na.action = na.omit, do.trace = 50)
myforestcooling

##Removing heating: orientation, surface area
##removing cooling: orientation
myforestheating1 = randomForest(heating_load~relative_compactness+wall_area+roof_area+overall_height+glazing_area+glazing_area_distribution, ntree=500, data = energy, importance = TRUE, na.action = na.omit)
myforestheating1 ##var explained goes up after removing orientation

myforestcooling1 = randomForest(cooling_load~relative_compactness+wall_area+roof_area+overall_height+glazing_area+glazing_area_distribution, ntree=500, data = energy, importance = TRUE, na.action = na.omit)
myforestcooling1 ##Var explained goes down after removing orientation


##var implot 
importance(myforestheating1)
varImpPlot(myforestheating1, main = "Variable Importance for Heating Load")

importance(myforestcooling1)
varImpPlot(myforestcooling1, main = "Variable Importance for Cooling Load")


##boosting
set.seed(1)
boostedheating = gbm(heating_load~relative_compactness+surface_area+wall_area+roof_area+overall_height+orientation+glazing_area+glazing_area_distribution, data = energy, distribution = "gaussian", n.trees = 10000, interaction.depth = 4)
summary(boostedheating, main = "Boosting for Heating Load")

boostedcooling = gbm(cooling_load~relative_compactness+surface_area+wall_area+roof_area+overall_height+orientation+glazing_area+glazing_area_distribution, data = energy, distribution = "gaussian", n.trees = 10000, interaction.depth = 4)
summary(boostedcooling, main = "Boosting for Cooling Load")


##forest after picking best predictors
myforestheatingbest = randomForest(heating_load~relative_compactness+roof_area, ntree=500, data = energy, importance = TRUE, na.action = na.omit)
myforestheatingbest

myforestcoolingbest = randomForest(cooling_load~relative_compactness+surface_area+wall_area+roof_area+overall_height+orientation+glazing_area+glazing_area_distribution, ntree=500, data = energy, importance = TRUE, na.action = na.omit)
myforestcoolingbest


##obtaining MSE
predicted_heating = predict(boostedheating, data = energy, n.trees = 10000)
mean((predicted_heating-heating_load)^2) ##0.03264163

predicted_cooling = predict(boostedcooling, data = energy, n.trees = 10000)
mean((predicted_cooling-cooling_load)^2) ##0.1404597

##Doing PCA

#buildinglabel = energy[,c(9,10)]
buildingvars = energy[,c(1:10)]

pcabuilding = prcomp(buildingvars, scale = TRUE)
pcabuilding

energy$orientation =  as.factor(energy$orientation)
energy$glazing_area_distribution =  as.factor(energy$glazing_area_distribution)
autoplot(pcabuilding, data = buildingvars, loadings = TRUE, loadings.label = TRUE)



mregheating = lm(heating_load~relative_compactness+surface_area+wall_area+roof_area+overall_height+orientation+glazing_area+glazing_area_distribution)
mregheating

mregcooling = lm(cooling_load~relative_compactness+surface_area+wall_area+roof_area+overall_height+orientation+glazing_area+glazing_area_distribution)
mregcooling

####################################################################################################################################################################

energy$factorientation =  as.factor(energy$orientation)
energy$factorglazing_area_distribution =  as.factor(energy$glazing_area_distribution)


#############################
##### 1. Descriptive ########
#############################

##Heating load
boxplot(heating_load, main = "Heating Load Boxplot") ##put pics
energy$log_heating = log10(energy$heating_load)
summary(heating_load)
hist(heating_load)
shapiro.test(heating_load)

##Cooling Load
boxplot(cooling_load, main = "Cooling Load Boxplot") ##put pics
energy$log_cooling = log10(energy$cooling_load)
summary(cooling_load)
hist(cooling_load)
shapiro.test(cooling_load)

##Relative Compactness
hist(relative_compactness) ##notnormal
shapiro.test(relative_compactness) ## p value < 5%, not normal
energy$log_relative_compactness = log10(energy$relative_compactness)
boxplot(relative_compactness, main = "Relative Compactness Boxplot")

reg1 = lm(heating_load~relative_compactness)
summary(reg1)

reg11 = lm(cooling_load~relative_compactness)
summary(reg11)

summary(relative_compactness)
 
##surface area
hist(surface_area) ##notnormal
shapiro.test(surface_area) ## p value < 5%, not normal
energy$log_surface_area = log10(energy$surface_area)
boxplot(surface_area, main = "Surface Area Boxplot")

reg2 = lm(heating_load~surface_area)
summary(reg2)

reg22 = lm(cooling_load~surface_area)
summary(reg22)
summary(surface_area)

##Wall area
hist(wall_area) ##notnormal
shapiro.test(wall_area) ## p value < 5%, not normal
energy$log_wall_area = log10(energy$wall_area)
boxplot(wall_area, main = "Wall Area Boxplot")

reg3 = lm(heating_load~wall_area)
summary(reg3)

reg33 = lm(cooling_load~wall_area)
summary(reg33)
summary(wall_area)

##Roof area
hist(roof_area) ##notnormal
shapiro.test(roof_area) ## p value < 5%, not normal
energy$log_roof_area = log10(energy$roof_area)
boxplot(roof_area, main = "Roof Area Boxplot")

reg4 = lm(heating_load~roof_area)
summary(reg4)

reg44 = lm(cooling_load~roof_area)
summary(reg44)
summary(roof_area)


## Overall Height
hist(overall_height) ##notnormal
shapiro.test(overall_height) ## p value < 5%, not normal
energy$log_overall_height = log10(energy$overall_height)
boxplot(overall_height, main = "Overall Height Boxplot")

reg5 = lm(heating_load~overall_height)
summary(reg5)

reg55 = lm(cooling_load~overall_height)
summary(reg55)

summary(overall_height)

##Orientation
##can't do histogram on categorical
orientation_num = as.numeric(energy$orientation)
boxplot(orientation, main = "Orientation Boxplot")
hist(orientation)

reg6 = lm(heating_load~orientation)
summary(reg6)

reg66 = lm(cooling_load~orientation)
summary(reg66)
shapiro.test(orientation) 
summary(orientation)


##Glazing Area
hist(glazing_area) ##notnormal
shapiro.test(glazing_area) ## p value < 5%, not normal
energy$log_glazing_area = log10(energy$glazing_area)
boxplot(glazing_area, main = "Glazing Area Boxplot")
energy$glazing_area_grouped = case_when(energy$glazing_area == 0~ "None",energy$glazing_area == 0.1  ~ "Low",energy$glazing_area == 0.25 ~ "Medium",energy$glazing_area == 0.4  ~ "High")

reg7 = lm(heating_load~glazing_area)
summary(reg7)

reg77 = lm(cooling_load~glazing_area)
summary(reg77)
summary(glazing_area)


# Convert to factor and optionally set order
energy$glazing_area_grouped <- factor(energy$glazing_area_grouped,levels = c("None", "Low", "Medium", "High"))


##Glazing area distribution
##can't do histogram on categorical
glazing_area_distribution_num = as.numeric(energy$glazing_area_distribution)
boxplot(glazing_area_distribution, main = "Glazing Area Distribution Boxplot")
hist(glazing_area_distribution)
shapiro.test(glazing_area_distribution)

reg8 = lm(heating_load~glazing_area_distribution)
summary(reg8)

reg88 = lm(cooling_load~glazing_area_distribution)
summary(reg88)
summary(glazing_area_distribution)

##Corell
quantvars = energy[,c(1,2,3,4,5,7)]
corr_matrix = cor(quantvars)
round(corr_matrix, 2)

##regression
mregheating1 = lm(heating_load~relative_compactness+surface_area+wall_area+roof_area+overall_height+glazing_area+orientation+glazing_area_distribution)
summary(mregheating1)
residualPlot(mregheating1)
residualPlots(mregheating1)
ncvTest(mregheating1)


mregcooling1 = lm(cooling_load~relative_compactness+surface_area+wall_area+roof_area+overall_height+glazing_area+orientation+glazing_area_distribution)
summary(mregcooling1)
residualPlot(mregcooling1)
residualPlots(mregcooling1)
ncvTest(mregcooling1)

##log backward elimination
mregheatingbacklog = lm(log_heating~log_wall_area+log_surface_area+log_roof_area+log_relative_compactness+glazing_area_grouped+log_overall_height+factorientation+factorglazing_area_distribution, data = energy)
summary(mregheatingbacklog)
residualPlots(mregheatingbacklog)

backwardheatlog = step(mregheatingbacklog, direction = "backward") ##had to change glazing area because I was getting -INF for 0 values in glazing area, changed into categorical
summary(backwardheatlog)



mregcoolingbacklog = lm(log_cooling~log_wall_area+log_surface_area+log_roof_area+log_relative_compactness+glazing_area_grouped+log_overall_height+factorientation+factorglazing_area_distribution, data = energy)
summary(mregcoolingbacklog)
residualPlots(mregcoolingbacklog)

backwardcoolog = step(mregcoolingbacklog, direction = "backward")
summary(backwardcoolog)

##Normal stepwise

backwardheat = step(mregheating, direction = "backward") ##had to change glazing area because I was getting -INF for 0 values in glazing area, changed into categorical
summary(backwardheat)

backwardcool = step(mregcooling, direction = "backward")
summary(backwardcool)



###random forest with log keeping all variables
myforestheatinglog = randomForest(log_heating~log_wall_area+log_surface_area+log_roof_area+log_relative_compactness+glazing_area_grouped+log_overall_height+factorientation+factorglazing_area_distribution, ntree=500, data = energy, importance = TRUE, na.action = na.omit)
myforestheatinglog

myforestcoolinglog = randomForest(log_cooling~log_wall_area+log_surface_area+log_roof_area+log_relative_compactness+glazing_area_grouped+log_overall_height+factorientation+factorglazing_area_distribution, ntree=500, data = energy, importance = TRUE, na.action = na.omit)
myforestcoolinglog

##importance logs
importance(myforestheatinglog)
varImpPlot(myforestheatinglog, main = "Variable Importance for Heating Load Log")

importance(myforestcoolinglog)
varImpPlot(myforestcoolinglog, main = "Variable Importance for Cooling Load Log")

##Boosting
set.seed(1)
boostedheatinglog = gbm(log_heating~log_wall_area+log_surface_area+log_roof_area+log_relative_compactness+glazing_area_grouped+log_overall_height+factorientation+factorglazing_area_distribution, data = energy, distribution = "gaussian", n.trees = 10000, interaction.depth = 4)
summary(boostedheatinglog, main = "Boosting for Heating Load Log")

boostedcoolinglog = gbm(log_cooling~log_wall_area+log_surface_area+log_roof_area+log_relative_compactness+glazing_area_grouped+log_overall_height+factorientation+factorglazing_area_distribution, data = energy, distribution = "gaussian", n.trees = 10000, interaction.depth = 4)
summary(boostedcoolinglog, main = "Boosting for Cooling Load Log")

##MSE logged
predicted_heating_log = predict(boostedheatinglog, data = energy, n.trees = 10000)
mean((predicted_heating_log-energy$log_heating)^2) 

predicted_cooling_log = predict(boostedcoolinglog, data = energy, n.trees = 10000)
mean((predicted_cooling_log-energy$log_cooling)^2) 


##Making forest better
myforestheatinglog2 = randomForest(log_heating~log_surface_area+log_wall_area+log_roof_area+log_relative_compactness+glazing_area_grouped+log_overall_height, ntree=500, data = energy, importance = TRUE, na.action = na.omit)
myforestheatinglog2 ##kept wall area because it increase %var explained by 5%

myforestcoolinglog2 = randomForest(log_cooling~log_surface_area+log_relative_compactness+glazing_area_grouped+log_wall_area, ntree=500, data = energy, importance = TRUE, na.action = na.omit)
myforestcoolinglog2 ##kept wall area increase % var by 4%


mregfinalheat = lm(log_heating~log_surface_area+log_wall_area+log_roof_area+log_relative_compactness+glazing_area_grouped+log_overall_height, data = energy)
summary(mregfinalheat)

stargazer(mregfinalheat, type = "html")
stargazer(mregfinalcool, type = "html")

mregfinalcool = lm(log_cooling~log_surface_area+log_relative_compactness+glazing_area_grouped+log_wall_area, data = energy)
summary(mregfinalcool)

##surface1, wall1, roof, relative1, glazinggroup1, height

regsurfaceheat = lm(energy$log_heating~energy$log_surface_area)
summary(regsurfaceheat)
regsurfacecool = lm(energy$log_cooling~energy$log_surface_area)
summary(regsurfacecool)

regwallheat = lm(energy$log_heating~energy$log_wall_area)
summary(regwallheat)
regwallcool = lm(energy$log_cooling~energy$log_wall_area)
summary(regwallcool)

regroofheat = lm(energy$log_heating~energy$log_roof_area)
summary(regroofheat)

regcompheat = lm(energy$log_heating~energy$log_relative_compactness)
summary(regcompheat)
regcompcool = lm(energy$log_cooling~energy$log_relative_compactness)
summary(regcompcool)

regroupheat = lm(energy$log_heating~energy$glazing_area_grouped)
summary(regroupheat)
regroupcool = lm(energy$log_cooling~energy$glazing_area_grouped)
summary(regroupcool)

regheightheat = lm(energy$log_heating~energy$log_overall_height)
summary(regheightheat)








