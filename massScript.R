#=====================================================================================#

# This needs a lot of work. Start by making a function of the model.
# Also, make sure to move all library packages up to the top of the page.
# Make sure to add comments throughout so it's undertandable later.

#=====================================================================================#
#### Mass Movement Poster

library(devtools)
library(gmodels)
library(openxlsx)
library(ggbiplot)
library(class)
library(caret)
library(e1071)
library(glmnet)
library(kernlab)
library(stringr)

setwd("/Users/connordavis/Desktop/mass_test")
# complete_data <- read.csv("attribution_data_new.csv", na.strings=c("","NA"), header=T)
# complete_data <- complete_data[,-62]


# setwd("~/Downloads/")
complete_data <- read.csv("mass_data.csv", header=T)
complete_data <- mutate(complete_data,
                        rising_passing =  rising_passing/total,
                        downward_passing = downward_passing/total,	
                        upper_neighbor = upper_neighbor/total,
                        lower_neighbor = lower_neighbor/total,
                        double_neighbor_ul = double_neighbor_ul/total,	
                        double_neighbor_lu = double_neighbor_lu/total,
                        upper_echappee = upper_echappee/total,	
                        lower_echappee = lower_echappee/total,	
                        ascending_short_nota_cambiata = ascending_short_nota_cambiata/total,	
                        descending_short_nota_cambiata = descending_short_nota_cambiata/total,	
                        ascending_long_nota_cambiata = ascending_long_nota_cambiata/total,	
                        descending_long_nota_cambiata = descending_long_nota_cambiata/total,	
                        rising_anticipation = rising_anticipation/total,	
                        descending_anticipation = descending_anticipation/total,	
                        reverse_ascending_nota_cambiata = reverse_ascending_nota_cambiata/total,	
                        reverse_descending_nota_cambiata = reverse_descending_nota_cambiata/total,	
                        reverse_upper_echappee = reverse_upper_echappee/total,
                        reverse_lower_echappee = reverse_lower_echappee/total,	
                        ternary_suspension = ternary_suspension/total,	
                        binary_suspension = binary_suspension/total,	
                        ternary_suspension_agent = ternary_suspension_agent/total,	
                        binary_suspension_agent = binary_suspension_agent/total,	
                        fake_suspension_step_up = fake_suspension_step_up/total,	
                        fake_suspension_step_down = fake_suspension_step_down/total,	
                        resolution_against_suspension_dissonance = resolution_against_suspension_dissonance/total,	
                        suspension_repeated_note = suspension_repeated_note/total,	
                        suspension_missing_agent_approached_step_up = suspension_missing_agent_approached_step_up/total,	
                        suspension_missing_agent_approached_step_down = suspension_missing_agent_approached_step_down/total,	
                        dissonant_third_quarter_rising_pt = dissonant_third_quarter_rising_pt/total,	
                        dissonant_third_quarter_falling_pt = dissonant_third_quarter_falling_pt/total,	
                        dissonant_third_quarter_un = dissonant_third_quarter_un/total,	
                        dissonant_third_quarter_ln = dissonant_third_quarter_ln/total,	
                        appoggiature_approached_from_below = appoggiature_approached_from_below/total,	
                        appoggiature_approached_from_above = appoggiature_approached_from_above/total,	
                        ascending_accented_pt = ascending_accented_pt/total,	
                        descending_accented_pt = descending_accented_pt/total,	
                        accented_upper_neighbor = accented_upper_neighbor/total,	
                        descending_lower_neighbor = descending_lower_neighbor/total,	
                        chanson_idiom = chanson_idiom/total,	
                        unclassified_2_or7 = unclassified_2_or7/total,	
                        unclassified_4 = unclassified_4/total)

complete_data$Composer <- gsub("^.*:", "", complete_data$Composer)
complete_data$Composer <- as.factor(complete_data$Composer)




####### COMPLETE DATA PREDICT MVMNT MODEL

#####Cleaning
cd.lar <- complete_data[complete_data$Composer == "la Rue_Pierre de",-12]
cd.pal <- complete_data[complete_data$Composer == "Palestrina_Giovanni Perluigi da",-12]
cd.ock <- complete_data[complete_data$Composer == "Johannes Ockeghem",-12]
cd.ort <- complete_data[complete_data$Composer == "de Orto_Marbrianus",-12]
cd.duf <- complete_data[complete_data$Composer == "Du Fay_Guillaume",-12]

cd.jos <- complete_data[complete_data$Composer == 'Josquin des Prez',-12]
cd.jos_secure <- cd.jos[cd.jos$Attribution.Level <= 3 ,]

cd <- rBind(cd.lar, cd.pal, cd.ock, cd.ort, cd.duf, cd.jos_secure)

cd <- cd[,-1]          
cd <- cd[-(1:56)]
cd <- cd[-42]

cd_loadings <- cd
cd_kmeans <- cbind(cd$mvmnt
                   , cd_loadings)
cd_kmeans <- as.data.frame(cd_kmeans)
cd.data <- cd_kmeans
table(cd.data[,1])

table(cd.data$PC1)

cd.inTrain <- createDataPartition(y = cd$mvmnt,
                                  p = .7,
                                  list = FALSE)
str(cd.inTrain)

# Make your train test baesed on above indexing
cd.training <- cd[cd.inTrain,]
cd.testing <- cd[-cd.inTrain,]

# Reproduce! 
set.seed(107)

## Set up Control, this one does repeated cross validation and picks best one on given number of repeats
# class probs says you want probablity returned and the last argument says you are only dealing with one thing 

## With what you want set, you now just have to set the model here
cd[is.na(cd)] <- 0

ctrl <- trainControl(method = "repeatedcv",
                     repeats = 4,
                     classProbs = TRUE,
                     ###binary
                     # summaryFunction = twoClassSummary,
                     verboseIter = TRUE)

# jos.training$Composer <- as.factor(jos.training$Composer)

####### COMPLETE DATA MODEL
cd.plsFit <- train(mvmnt ~ .,
                   data = cd.training,
                   method = "glmnet",
                   tuneLength = 10,
                   trControl = ctrl,
                   metric = "ROC",
                   #sample = "up",
                   preProc = c("center","scale"))

plot(cd.plsFit) # Pretty, pretty good 

cd.plsClasses <- predict(cd.plsFit, newdata = cd.testing)

confusionMatrix(cd.plsClasses, cd.testing$mvmnt,
                positive = NULL, 
                dnn = c("Prediction", "Reference"), 
                prevalence = NULL)

cd.importance <- varImp(object=cd.plsFit)
plot(cd.importance)


################################## EXPERIMENT

####PAL

pal.test.plsClasses <- predict(cd.plsFit, newdata = pal.testing)

confusionMatrix(test.plsClasses, pal.testing$mvmnt,
                positive = NULL, 
                dnn = c("Prediction", "Reference"), 
                prevalence = NULL)
####DUF

duf.test.plsClasses <- predict(cd.plsFit, newdata = duf.testing)

confusionMatrix(duf.test.plsClasses, duf.testing$mvmnt,
                positive = NULL, 
                dnn = c("Prediction", "Reference"), 
                prevalence = NULL)

####DeOrto

ort.test.plsClasses <- predict(cd.plsFit, newdata = ort.testing)

confusionMatrix(ort.test.plsClasses, ort.testing$mvmnt,
                positive = NULL, 
                dnn = c("Prediction", "Reference"), 
                prevalence = NULL)


####Josquin

jos.test.plsClasses <- predict(cd.plsFit, newdata = jos.testing)

confusionMatrix(jos.test.plsClasses, jos.testing$mvmnt,
                positive = NULL, 
                dnn = c("Prediction", "Reference"), 
                prevalence = NULL)

####Ockeghem

ock.test.plsClasses <- predict(cd.plsFit, newdata = ock.testing)

confusionMatrix(ock.test.plsClasses, ock.testing$mvmnt,
                positive = NULL, 
                dnn = c("Prediction", "Reference"), 
                prevalence = NULL)

####LaRue

lar.test.plsClasses <- predict(cd.plsFit, newdata = lar.testing)

confusionMatrix(lar.test.plsClasses, lar.testing$mvmnt,
                positive = NULL, 
                dnn = c("Prediction", "Reference"), 
                prevalence = NULL)


####### COMPLETE DATA PREDICT COMPOSER MODEL

#####Cleaning
comp.lar <- complete_data[complete_data$Composer == "la Rue_Pierre de",-12]
comp.pal <- complete_data[complete_data$Composer == "Palestrina_Giovanni Perluigi da",-12]
comp.ock <- complete_data[complete_data$Composer == "Johannes Ockeghem",-12]
comp.ort <- complete_data[complete_data$Composer == "de Orto_Marbrianus",-12]
comp.duf <- complete_data[complete_data$Composer == "Du Fay_Guillaume",-12]

comp.jos <- complete_data[complete_data$Composer == 'Josquin des Prez',-12]
comp.jos_secure <- comp.jos[comp.jos$Attribution.Level <= 3 ,]

comp <- rBind(comp.lar, comp.pal, comp.ock, comp.ort, comp.duf, comp.jos_secure)

comp <- comp[,-1]          
comp <- comp[-(2:57)]
comp <- comp[-43]


comp <- read.csv("comp.csv", header = TRUE) 
comp <- comp[,-1]
comp <- comp[,-43]

comp_loadings <- comp
comp_kmeans <- cbind(comp$Composer
                     , comp_loadings)
comp_kmeans <- as.data.frame(comp)
comp.data <- comp_kmeans
table(comp.data[,1])

table(comp.data$PC1)

comp.inTrain <- createDataPartition(y = comp$Composer,
                                    p = .6,
                                    list = FALSE)
str(comp.inTrain)

# Make your train test baesed on above indexing
comp.training <- comp[comp.inTrain,]
comp.testing <- comp[-comp.inTrain,]

# Reproduce! 
set.seed(107)

## Set up Control, this one does repeated cross validation and picks best one on given number of repeats
# class probs says you want probablity returned and the last argument says you are only dealing with one thing 

## With what you want set, you now just have to set the model here
comp[is.na(comp)] <- 0

ctrl <- trainControl(method = "repeatedcv",
                     repeats = 4,
                     classProbs = TRUE,
                     ###binary
                     # summaryFunction = twoClassSummary,
                     verboseIter = TRUE)

# jos.training$Composer <- as.factor(jos.training$Composer)

####### COMPLETE DATA COMPOSER MODEL
comp.plsFit <- train(Composer ~.,
                     data = comp.training,
                     method = "glmnet",
                     tuneLength = 10,
                     trControl = ctrl,
                     metric = "ROC",
                     #sample = "up",
                     preProc = c("center","scale"))

plot(comp.plsFit) # Pretty, pretty good 

comp.plsClasses <- predict(comp.plsFit, newdata = comp.testing)

confusionMatrix(comp.plsClasses, comp.testing$Composer,
                positive = NULL, 
                dnn = c("Prediction", "Reference"), 
                prevalence = NULL)

comp.importance <- varImp(object=comp.plsFit)
plot(comp.importance)




########## COMPLETE DATA BY MOVEMENT NO PAL


#####Cleaning
cd.lar1 <- complete_data[complete_data$Composer == "la Rue_Pierre de",-12]
cd.pal1 <- complete_data[complete_data$Composer == "Palestrina_Giovanni Perluigi da",-12]
cd.ock1 <- complete_data[complete_data$Composer == "Johannes Ockeghem",-12]
cd.ort1 <- complete_data[complete_data$Composer == "de Orto_Marbrianus",-12]
cd.duf1 <- complete_data[complete_data$Composer == "Du Fay_Guillaume",-12]

cd.jos1 <- complete_data[complete_data$Composer == 'Josquin des Prez',-12]
cd.jos_secure1 <- cd.jos1[cd.jos1$Attribution.Level <= 3 ,]

cd.np <- rBind(cd.lar, cd.ock, cd.ort, cd.duf, cd.jos_secure)

cd.np <- cd.np[,-1]          
cd.np <- cd.np[-(1:56)]
cd.np <- cd.np[-42]

cd.np_loadings <- cd.np
cd.np_kmeans <- cbind(cd.np$mvmnt
                      , cd.np_loadings)
cd.np_kmeans <- as.data.frame(cd.np_kmeans)
cd.np.data <- cd.np_kmeans
table(cd.np.data[,1])

table(cd.np.data$PC1)

cd.np.inTrain <- createDataPartition(y = cd.np$mvmnt,
                                     p = .7,
                                     list = FALSE)
str(cd.np.inTrain)

# Make your train test baesed on above indexing
cd.np.training <- cd.np[cd.np.inTrain,]
cd.np.testing <- cd.np[-cd.np.inTrain,]

# Reproduce! 
set.seed(107)

## Set up Control, this one does repeated cross validation and picks best one on given number of repeats
# class probs says you want probablity returned and the last argument says you are only dealing with one thing 

## With what you want set, you now just have to set the model here
cd.np[is.na(cd.np)] <- 0

ctrl <- trainControl(method = "repeatedcv",
                     repeats = 4,
                     classProbs = TRUE,
                     ###binary
                     # summaryFunction = twoClassSummary,
                     verboseIter = TRUE)

# jos.training$Composer <- as.factor(jos.training$Composer)

####### COMPLETE DATA MODEL NO PAL!!!
cd.np.plsFit <- train(mvmnt ~ .,
                      data = cd.np.training,
                      method = "glmnet",
                      tuneLength = 10,
                      trControl = ctrl,
                      metric = "ROC",
                      #sample = "up",
                      preProc = c("center","scale"))

plot(cd.np.plsFit) # Pretty, pretty good 

cd.np.plsClasses <- predict(cd.np.plsFit, newdata = cd.np.testing)

confusionMatrix(cd.np.plsClasses, cd.np.testing$mvmnt,
                positive = NULL, 
                dnn = c("Prediction", "Reference"), 
                prevalence = NULL)

cd.np.importance <- varImp(object=cd.np.plsFit)
plot(cd.np.importance)

####MORE STUFF WITH NO PALESTRINA


#### LA RUE NOPAL
lar.cd.np.plsClasses <- predict(cd.np.plsFit, newdata = lar.testing)

confusionMatrix(lar.cd.np.plsClasses, lar.testing$mvmnt,
                positive = NULL, 
                dnn = c("Prediction", "Reference"), 
                prevalence = NULL)


### OCK NO PAL

ock.cd.np.plsClasses <- predict(cd.np.plsFit, newdata = ock.testing)

confusionMatrix(ock.cd.np.plsClasses, ock.testing$mvmnt,
                positive = NULL, 
                dnn = c("Prediction", "Reference"), 
                prevalence = NULL)


### JOS NO PAL

jos.cd.np.plsClasses <- predict(cd.np.plsFit, newdata = jos.testing)

confusionMatrix(jos.cd.np.plsClasses, jos.testing$mvmnt,
                positive = NULL, 
                dnn = c("Prediction", "Reference"), 
                prevalence = NULL)



### ORT NO PAL

ort.cd.np.plsClasses <- predict(cd.np.plsFit, newdata = ort.testing)

confusionMatrix(ort.cd.np.plsClasses, ort.testing$mvmnt,
                positive = NULL, 
                dnn = c("Prediction", "Reference"), 
                prevalence = NULL)






####### COMPLETE DATA PREDICT COMPOSER MODEL NO PAL

#####Cleaning
comp.lar.1 <- complete_data[complete_data$Composer == "la Rue_Pierre de",-12]
comp.ock.1 <- complete_data[complete_data$Composer == "Johannes Ockeghem",-12]
comp.ort.1 <- complete_data[complete_data$Composer == "de Orto_Marbrianus",-12]
comp.duf.1 <- complete_data[complete_data$Composer == "Du Fay_Guillaume",-12]

comp.jos <- complete_data[complete_data$Composer == 'Josquin des Prez',-12]
comp.jos_secure.1 <- comp.jos[comp.jos$Attribution.Level <= 3 ,]

comp.1 <- rBind(comp.lar.1, comp.ock.1, comp.ort.1, comp.duf.1, comp.jos_secure.1)

comp.1 <- read.csv("comp_no_pal.csv")
comp.1 <- comp.1[,-(1:2)] 

### FOR PCA WITH NO PAL
comp.3 <- comp.1

comp.1 <- comp.1[-(2:57)]
comp.1 <- comp.1[-(43:44)]

####FOR PCA's WITH NO PAL

comp2 <- comp.1
comp3 <- comp.3

comp1_loadings <- comp.1
comp1_kmeans <- cbind(comp.1$Composer
                      , comp1_loadings)
comp1_kmeans <- as.data.frame(comp.1)
comp1.data <- comp1_kmeans
table(comp1.data[,1])

table(comp1.data$PC1)

comp1.inTrain <- createDataPartition(y = comp.1$Composer,
                                     p = .6,
                                     list = FALSE)
str(comp1.inTrain)

# Make your train test baesed on above indexing
comp1.training <- comp.1[comp1.inTrain,]
comp1.testing <- comp.1[-comp1.inTrain,]

# Reproduce! 
set.seed(107)

## Set up Control, this one does repeated cross validation and picks best one on given number of repeats
# class probs says you want probablity returned and the last argument says you are only dealing with one thing 

## With what you want set, you now just have to set the model here
comp[is.na(comp)] <- 0

ctrl <- trainControl(method = "repeatedcv",
                     repeats = 4,
                     classProbs = TRUE,
                     ###binary
                     # summaryFunction = twoClassSummary,
                     verboseIter = TRUE)

# jos.training$Composer <- as.factor(jos.training$Composer)

####### COMPLETE DATA COMPOSER NO PAL MODEL
comp1.plsFit <- train(Composer ~.,
                      data = comp1.training,
                      method = "glmnet",
                      tuneLength = 10,
                      trControl = ctrl,
                      metric = "ROC",
                      #sample = "up",
                      preProc = c("center","scale"))

plot(comp1.plsFit) # Pretty, pretty good 

comp1.plsClasses <- predict(comp1.plsFit, newdata = comp1.testing)

confusionMatrix(comp1.plsClasses, comp1.testing$Composer,
                positive = NULL, 
                dnn = c("Prediction", "Reference"), 
                prevalence = NULL)

comp1.importance <- varImp(object=comp1.plsFit)
plot(comp1.importance)



###subset the data by analysis level

# Josquin attribution level 1 and palestrina

josquin <- complete_data[complete_data$Composer == 'Josquin des Prez',-12]

josquin_secure <- josquin[josquin$Attribution.Level <= 3 ,]
josquin_secure <- josquin_secure[,-100]
josquin_secure <- josquin_secure[,58:99]

# josquin_secure$Composer <- as.character(josquin_secure$Composer)
# josquin_less_secure <- josquin[ josquin$Attribution.Level >= 3,]


####Other composers
# bach <- complete_data[complete_data$Composer == "Bach_Johann Sebastian",-12]
# larue <- complete_data[complete_data$Composer == "la Rue_Pierre de",-12]
# palestrina <- complete_data[complete_data$Composer == "Palestrina_Giovanni Perluigi da",-12]
# orto <- complete_data[complete_data$Composer == "de Orto_Marbrianus",-12]
# # 
# josquin_bach <- rbind(josquin_secure, bach)
# josquin_palestrina <- rbind(josquin_secure, palestrina)
# josquin_larue <- rbind(josquin_secure, larue)


#####Cleaning

josq_loadings <- josquin_secure
josq_kmeans <- cbind(josquin_secure$mvmnt
                     , josq_loadings)
josq_kmeans <- as.data.frame(josq_kmeans)
data <- josq_kmeans
table(data[,1])

table(data$PC1)

jos.inTrain <- createDataPartition(y = josquin_secure$mvmnt,
                                   p = .7,
                                   list = FALSE)
str(jos.inTrain)

# Make your train test baesed on above indexing
jos.training <- josquin_secure[jos.inTrain,]
jos.testing <- josquin_secure[-jos.inTrain,]

# Reproduce! 
set.seed(107)

## Set up Control, this one does repeated cross validation and picks best one on given number of repeats
# class probs says you want probablity returned and the last argument says you are only dealing with one thing 

## With what you want set, you now just have to set the model here
josquin_secure[is.na(josquin_secure)] <- 0

ctrl <- trainControl(method = "repeatedcv",
                     repeats = 4,
                     classProbs = TRUE,
                     ###binary
                     # summaryFunction = twoClassSummary,
                     verboseIter = TRUE)

# jos.training$Composer <- as.factor(jos.training$Composer)

#######JOSQUIN MODEL CD
jos.plsFit <- train(mvmnt ~ .,
                    data = jos.training,
                    method = "glmnet",
                    tuneLength = 10,
                    trControl = ctrl,
                    metric = "ROC",
                    #sample = "up",
                    preProc = c("center","scale"))

plot(jos.plsFit) # Pretty, pretty good 

jos.plsClasses <- predict(jos.plsFit, newdata = jos.testing)

confusionMatrix(jos.plsClasses, jos.testing$mvmnt,
                positive = NULL, 
                dnn = c("Prediction", "Reference"), 
                prevalence = NULL)

jos.importance <- varImp(object=jos.plsFit)
plot(jos.importance)









# ####### DUFAY MODEL CD
# dufay <- complete_data[complete_data$Composer == "Du Fay_Guillaume",-12]
# dufay <- dufay[,58:100]
# dufay <- dufay[,-42]
# 
# #####Cleaning
# 
# duf_loadings <- dufay
# #duf_kmeans <- cbind(dufay$mvmnt
#                      #, duf_loadings)
# duf_kmeans <- dufay
# duf_kmeans <- as.data.frame(duf_kmeans)
# duf.data <- duf_kmeans
# table(duf.data[,42])
# 
# table(duf.data$PC1)
# 
# duf.inTrain <- createDataPartition(y = dufay$mvmnt,
#                                    p = .7,
#                                    list = FALSE)
# str(duf.inTrain)
# str(duf.inTrain)
# # Make your train test baesed on above indexing
# duf.training <- dufay[duf.inTrain,]
# duf.testing <- dufay[-duf.inTrain,]
# 
# # Reproduce! 
# set.seed(107)
# 
# ## Set up Control, this one does repeated cross validation and picks best one on given number of repeats
# # class probs says you want probablity returned and the last argument says you are only dealing with one thing 
# 
# ## With what you want set, you now just have to set the model here
# dufay[is.na(dufay)] <- 0
# 
# duf.ctrl <- trainControl(method = "repeatedcv",
#                      repeats = 4,
#                      classProbs = TRUE,
#                      ###binary
#                      # summaryFunction = twoClassSummary,
#                      verboseIter = TRUE)
# 
# # duf.training$Composer <- as.factor(duf.training$Composer)
# 
# ####### DUFAY MODEL CD
# duf.plsFit <- train(mvmnt ~ .,
#                     data = duf.training,
#                     method = "glmnet",
#                     tuneLength = 10,
#                     trControl = ctrl,
#                     metric = "ROC",
#                     #sample = "up",
#                     preProc = c("center","scale"))
# 
# plot(duf.plsFit) # Pretty, pretty good 
# 
# ##### ERRORS HERE #####
# 
# duf.plsClasses <- predict(duf.plsFit, newdata = duf.testing)
# 
# confusionMatrix(duf.plsClasses, duf.testing$mvmnt,
#                 positive = NULL, 
#                 dnn = c("Prediction", "Reference"), 
#                 prevalence = NULL)
# 
# duf.importance <- varImp(object=duf.plsFit)
# plot(duf.importance)


#############################  DUFAY CORRECTED MODEL

dufay <- complete_data[complete_data$Composer == "Du Fay_Guillaume",-12]
dufay <- dufay[,58:100]
dufay <- dufay[,-42]


#####Cleaning

duf_loadings <- dufay
duf_kmeans <- cbind(dufay$mvmnt
                    , duf_loadings)
duf_kmeans <- as.data.frame(duf_kmeans)
duf.data <- duf_kmeans
table(duf.data[,1])

table(duf.data$PC1)

duf.inTrain <- createDataPartition(y = dufay$mvmnt,
                                   p = .7,
                                   list = FALSE)
str(duf.inTrain)
str(jos.inTrain)
# Make your train test baesed on above indexing
duf.training <- dufay[duf.inTrain,]
duf.testing <- dufay[-duf.inTrain,]

# Reproduce! 
set.seed(107)

## Set up Control, this one does repeated cross validation and picks best one on given number of repeats
# class probs says you want probablity returned and the last argument says you are only dealing with one thing 

## With what you want set, you now just have to set the model here
dufay[is.na(dufay)] <- 0

duf.ctrl <- trainControl(method = "repeatedcv",
                         repeats = 4,
                         classProbs = TRUE,
                         ###binary
                         # summaryFunction = twoClassSummary,
                         verboseIter = TRUE)

# duf.training$Composer <- as.factor(duf.training$Composer)

####### DUFAY MODEL CD
duf.plsFit <- train(mvmnt ~ .,
                    data = duf.training,
                    method = "glmnet",
                    tuneLength = 10,
                    trControl = ctrl,
                    metric = "ROC",
                    #sample = "up",
                    preProc = c("center","scale"))

plot(duf.plsFit) # Pretty, pretty good 

duf.plsClasses <- predict(duf.plsFit, newdata = duf.testing)

confusionMatrix(duf.plsClasses, duf.testing$mvmnt,
                positive = NULL, 
                dnn = c("Prediction", "Reference"), 
                prevalence = NULL)

duf.importance <- varImp(object=duf.plsFit)
plot(duf.importance)




####### OCKEGHEM MODEL CD
ockeghem <- complete_data[complete_data$Composer == "Johannes Ockeghem",-12]
ockeghem <- ockeghem[,58:100]
ockeghem <- ockeghem[,-42]

#####Cleaning

ock_loadings <- ockeghem
ock_kmeans <- cbind(ockeghem$mvmnt
                    , ock_loadings)
ock_kmeans <- as.data.frame(ock_kmeans)
ock.data <- ock_kmeans
table(ock.data[,1])

table(ock.data$PC1)

ock.inTrain <- createDataPartition(y = ockeghem$mvmnt,
                                   p = .7,
                                   list = FALSE)
str(ock.inTrain)
str(jos.inTrain)
# Make your train test baesed on above indexing
ock.training <- ockeghem[ock.inTrain,]
ock.testing <- ockeghem[-ock.inTrain,]

# Reproduce! 
set.seed(107)

## Set up Control, this one does repeated cross validation and picks best one on given number of repeats
# class probs says you want probablity returned and the last argument says you are only dealing with one thing 

## With what you want set, you now just have to set the model here
ockeghem[is.na(ockeghem)] <- 0

ock.ctrl <- trainControl(method = "repeatedcv",
                         repeats = 4,
                         classProbs = TRUE,
                         ###binary
                         # summaryFunction = twoClassSummary,
                         verboseIter = TRUE)

# duf.training$Composer <- as.factor(duf.training$Composer)

####### OCKEGHEM MODEL CD
ock.plsFit <- train(mvmnt ~ .,
                    data = ock.training,
                    method = "glmnet",
                    tuneLength = 10,
                    trControl = ctrl,
                    metric = "ROC",
                    #sample = "up",
                    preProc = c("center","scale"))

plot(ock.plsFit) # Pretty, pretty good 

ock.plsClasses <- predict(ock.plsFit, newdata = ock.testing)

confusionMatrix(ock.plsClasses, ock.testing$mvmnt,
                positive = NULL, 
                dnn = c("Prediction", "Reference"), 
                prevalence = NULL)

ock.importance <- varImp(object=ock.plsFit)
plot(ock.importance)







#   La Rue Model

larue <- complete_data[complete_data$Composer == "la Rue_Pierre de",-12]
larue <- larue[,58:100]
larue <- larue[,-42]

#####Cleaning

lar_loadings <- larue
lar_kmeans <- cbind(larue$mvmnt
                    , lar_loadings)
lar_kmeans <- as.data.frame(lar_kmeans)
lar.data <- lar_kmeans
table(lar.data[,1])

table(lar.data$PC1)

lar.inTrain <- createDataPartition(y = larue$mvmnt,
                                   p = .7,
                                   list = FALSE)
str(lar.inTrain)

# Make your train test baesed on above indexing
lar.training <- larue[lar.inTrain,]
lar.testing <- larue[-lar.inTrain,]

# Reproduce! 
set.seed(107)

## Set up Control, this one does repeated cross validation and picks best one on given number of repeats
# class probs says you want probablity returned and the last argument says you are only dealing with one thing 

## With what you want set, you now just have to set the model here
larue[is.na(larue)] <- 0

lar.ctrl <- trainControl(method = "repeatedcv",
                         repeats = 4,
                         classProbs = TRUE,
                         ###binary
                         # summaryFunction = twoClassSummary,
                         verboseIter = TRUE)

####### LARUE MODEL CD
lar.plsFit <- train(mvmnt ~ .,
                    data = lar.training,
                    method = "glmnet",
                    tuneLength = 10,
                    trControl = ctrl,
                    metric = "ROC",
                    #sample = "up",
                    preProc = c("center","scale"))

plot(lar.plsFit) # Pretty, pretty good 

lar.plsClasses <- predict(lar.plsFit, newdata = lar.testing)

confusionMatrix(lar.plsClasses, lar.testing$mvmnt,
                positive = NULL, 
                dnn = c("Prediction", "Reference"), 
                prevalence = NULL)

lar.importance <- varImp(object=lar.plsFit)
plot(lar.importance)





########### Palestrina 


#####Cleaning

palestrina <- complete_data[complete_data$Composer == "Palestrina_Giovanni Perluigi da",-12]
palestrina <- palestrina[,58:100]
palestrina <- palestrina[,-42]

pal_loadings <- palestrina
pal_kmeans <- cbind(palestrina$mvmnt
                    , pal_loadings)
pal_kmeans <- as.data.frame(pal_kmeans)
pal.data <- pal_kmeans
table(pal.data[,1])

table(pal.data$PC1)

pal.inTrain <- createDataPartition(y = palestrina$mvmnt,
                                   p = .7,
                                   list = FALSE)
str(pal.inTrain)

# Make your train test baesed on above indexing
pal.training <- palestrina[pal.inTrain,]
pal.testing <- palestrina[-pal.inTrain,]

# Reproduce! 
set.seed(107)

## Set up Control, this one does repeated cross validation and picks best one on given number of repeats
# class probs says you want probablity returned and the last argument says you are only dealing with one thing 

## With what you want set, you now just have to set the model here
palestrina[is.na(palestrina)] <- 0

pal.ctrl <- trainControl(method = "repeatedcv",
                         repeats = 4,
                         classProbs = TRUE,
                         ###binary
                         # summaryFunction = twoClassSummary,
                         verboseIter = TRUE)

# jos.training$Composer <- as.factor(jos.training$Composer)

#######PAL MODEL CD
pal.plsFit <- train(mvmnt ~ .,
                    data = jos.training,
                    method = "glmnet",
                    tuneLength = 10,
                    trControl = ctrl,
                    metric = "ROC",
                    #sample = "up",
                    preProc = c("center","scale"))

plot(pal.plsFit) # Pretty good 

pal.plsClasses <- predict(pal.plsFit, newdata = pal.testing)

confusionMatrix(pal.plsClasses, pal.testing$mvmnt, ####not so hot?
                positive = NULL, 
                dnn = c("Prediction", "Reference"), 
                prevalence = NULL)

pal.importance <- varImp(object=pal.plsFit)
plot(pal.importance)






########Orto 

### CLEANING

orto <- complete_data[complete_data$Composer == "de Orto_Marbrianus",-12]
orto <- orto[,58:100]
orto <- orto[,-42]

ort_loadings <- orto
ort_kmeans <- cbind(orto$mvmnt
                    , ort_loadings)
ort_kmeans <- as.data.frame(ort_kmeans)
ort.data <- ort_kmeans
table(ort.data[,1])

table(ort.data$PC1)

ort.inTrain <- createDataPartition(y = orto$mvmnt,
                                   p = .7,
                                   list = FALSE)
str(ort.inTrain)

# Make your train test baesed on above indexing
ort.training <- orto[ort.inTrain,]
ort.testing <- orto[-ort.inTrain,]

# Reproduce! 
set.seed(107)

## Set up Control, this one does repeated cross validation and picks best one on given number of repeats
# class probs says you want probablity returned and the last argument says you are only dealing with one thing 

## With what you want set, you now just have to set the model here
orto[is.na(orto)] <- 0

ctrl <- trainControl(method = "repeatedcv",
                     repeats = 4,
                     classProbs = TRUE,
                     ###binary
                     # summaryFunction = twoClassSummary,
                     verboseIter = TRUE)

# jos.training$Composer <- as.factor(jos.training$Composer)

#######JOSQUIN MODEL CD
ort.plsFit <- train(mvmnt ~ .,
                    data = ort.training,
                    method = "glmnet",
                    tuneLength = 10,
                    trControl = ctrl,
                    metric = "ROC",
                    #sample = "up",
                    preProc = c("center","scale"))

plot(ort.plsFit) # Pretty, pretty good 

ort.plsClasses <- predict(ort.plsFit, newdata = ort.testing)

confusionMatrix(ort.plsClasses, ort.testing$mvmnt,
                positive = NULL, 
                dnn = c("Prediction", "Reference"), 
                prevalence = NULL)

ort.importance <- varImp(object=ort.plsFit)
plot(ort.importance)





########################################### PCA AND BI-PLOTS 

### COMPOSER PCA###

# log transform (AKA the devil)

log.comp <- log1p(comp[2:42])
comp.comp <- comp[, 1]

# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
comp.pca <- prcomp(log.comp,center = TRUE, scale = TRUE) 

# print method
print(comp.pca)

# plot method
plot(comp.pca, type = "l")

# summary method
summary(comp.pca)

# Predict PCs
predict(comp.pca, 
        newdata=tail(log.df, 2))

#### PCA COMP BI-PLOT

library(devtools)
install_github("ggbiplot", "vqv")

library(ggbiplot)
comp.biplot <- ggbiplot(comp.pca, choices = 2:1, obs.scale = 1, var.scale = 1, 
                        groups = comp.comp, ellipse = TRUE, 
                        circle = TRUE, var.axes = FALSE)
comp.biplot <- comp.biplot + scale_color_discrete(name = '')
comp.biplot <- comp.biplot + theme(legend.direction = 'horizontal', 
                                   legend.position = 'top')

comp.biplot <- comp.biplot + ylim(-15, 15)
print(comp.biplot)

#### MOVEMENT PCA

### Movement PCA###

# log transform (AKA the devil)

log.cd <- log1p(cd[2:42])
cd.comp <- cd[, 43]

# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
cd.pca <- prcomp(log.cd,center = TRUE, scale = TRUE) 

# print method
print(cd.pca)

# plot method
plot(cd.pca, type = "l")

# summary method
summary(cd.pca)

# Predict PCs
predict(cd.pca, 
        newdata=tail(log.df, 2))

#### PCA MOVEMENT NO PAL BI-PLOT

library(devtools)
install_github("ggbiplot", "vqv")

library(ggbiplot)
cd.biplot <- ggbiplot(cd.pca, choices = 2:1, obs.scale = 1, var.scale = 1, 
                      groups = cd.comp, ellipse = TRUE, 
                      circle = TRUE, var.axes = FALSE)
cd.biplot <- cd.biplot + scale_color_discrete(name = '')
cd.biplot <- cd.biplot + theme(legend.direction = 'horizontal', 
                               legend.position = 'top')

cd.biplot <- cd.biplot + ylim(-15, 15)
print(cd.biplot)





### MVMNT NO PAL PCA###

## CLEANING

comp3 <- comp3[,-(1:2)]
comp3 <- comp3[,56:98]

# log transform (AKA the devil)
log.comp3 <- log1p(comp3[1:41])
comp3.comp <- comp3[, 43]

# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
comp3.pca <- prcomp(log.comp3,center = TRUE, scale = TRUE) 

# print method
print(comp3.pca)

# plot method
plot(comp3.pca, type = "l")

# summary method
summary(comp3.pca)

# Predict PCs
predict(comp3.pca, 
        newdata=tail(log.df, 2))

#### PCA MVMNT NO PAL BI-PLOT

library(devtools)
install_github("ggbiplot", "vqv")

library(ggbiplot)
comp3.biplot <- ggbiplot(comp3.pca, choices = 2:1, obs.scale = 1, var.scale = 1, 
                         groups = comp3.comp, ellipse = TRUE, 
                         circle = TRUE, var.axes = FALSE)
comp3.biplot <- comp3.biplot + scale_color_discrete(name = '')
comp3.biplot <- comp3.biplot + theme(legend.direction = 'horizontal', 
                                     legend.position = 'top')

comp3.biplot <- comp3.biplot + ylim(-10, 10)
print(comp3.biplot)

# 
# 
# ### Compare Composers
# composers <- comparison[,1]
# composers <- as.matrix(composers)
# composersLoads <- pieces.pca$x[,1:27]
# composersLoads <- as.data.frame(composersLoads)
# toUse <- cbind(composers,composersLoads)
# 
# 
# ind <- sample(2, nrow(pieces.pca$x), replace=TRUE, prob=c(0.2,0.8))
# trainData <- toUse[ind==1,]
# testData <- toUse[ind==2,]
# tempTest <- testData[,-1]
# #data_train_labels <- trainData[,1]
# #data_test_labels <- testData[,1]
# 
# my_classifier <- ksvm(composers ~ ., data=trainData, kernel="vanilladot")
# my_prediction <- predict(my_classifier, tempTest)
# my_prediction
# head(my_prediction)
# 
# ###What's up with this?
# table(my_prediction, testData$composer)
# 
# agreement <- my_prediction == testData$composer
# 
# table(agreement)
# prop.table(table(agreement))
# 
# my_classifier_rbf <- ksvm(composers ~ ., data = trainData, kernel = "rbfdot")
# composer_predictions_rbf <- predict(my_classifier_rbf, testData)
# agreement_rbf <- composer_predictions_rbf == testData$composers
# table(agreement_rbf)
# prop.table(table(agreement_rbf))
# 
# 
# CrossTable(x =testData$composers, y = composer_predictions_rbf,
#            prop.chisq=FALSE)
# 
