##############################----------------------------------###############
### PREDICTIVE MAINTENANCE ###  Big Data for Smart Manufacturing
##############################__________________________________###############


#################
### LIBRARIES ###
#################

#install.packages('dplyr') # for pipeline operations (%>%)
#install.packages('zoo') # for rollapply
#install.packages('data.table') # more rapid for rolling operations 
#install.packages('gbm') # gradient boosting tree
#install.packages('ggplot2')

library(dplyr)
library(zoo)
library(data.table)
library(gbm)
library(ggplot2)


########################
### DATA ACQUISITION ###
########################

# 100 machines (machineID); model type and years in service as metadata for each machine.
# 4 components per machine.
# failures identified with the component where it occurs (comp1, comp2, ...).
# sensors in machines measures voltage, rotation, pressure, vibration.
# 5 kind of errors can happen: they do not constitute failure (machine still operational).


# time-series data of the 4 measurement during 2015 for all 100 machines:
# system('wget https://azuremlsampleexperiments.blob.core.windows.net/datasets/PdM_telemetry.csv')
telemetry <- read.csv('PdM_telemetry.csv')

# error logs, rounded to the closest hour, during 2015:
# system('wget https://azuremlsampleexperiments.blob.core.windows.net/datasets/PdM_errors.csv')
errors <- read.csv('PdM_errors.csv')

# maintenance record is generated if a component is replaced (during scheduled inspection or breakdown); it containts 2014 and 2015 records:
# system('wget https://azuremlsampleexperiments.blob.core.windows.net/datasets/PdM_maint.csv')
maint <- read.csv('PdM_maint.csv')

# machines metadata:
# system('wget https://azuremlsampleexperiments.blob.core.windows.net/datasets/PdM_failures.csv')
machines <- read.csv('PdM_machines.csv')

# component replacement due only to failures; only 2015 records:
# system('wget https://azuremlsampleexperiments.blob.core.windows.net/datasets/PdM_machines.csv ')
failures <- read.csv('PdM_failures.csv')

# some failures are not contained in maintenance:
all(failures$datetime %in% maint$datetime)







###########################
### DATA PRE-PROCESSING ###
###########################

# datetime field (chr) has to be transformed in calendar dates type:
telemetry$datetime <- as.POSIXct(telemetry$datetime,
                                format='%Y-%m-%d %H:%M:%S',
                                tz='UTC')

errors$datetime <- as.POSIXct(errors$datetime,
                                 format='%Y-%m-%d %H:%M:%S',
                                 tz='UTC')

maint$datetime <- as.POSIXct(maint$datetime,
                              format='%Y-%m-%d %H:%M:%S',
                              tz='UTC')

failures$datetime <- as.POSIXct(failures$datetime,
                             format='%Y-%m-%d %H:%M:%S',
                             tz='UTC')

# errorID (chr) in factor:
errors$errorID <- as.factor(errors$errorID)

# comp (chr) in factor:
maint$comp <- as.factor(maint$comp)

# failure (chr) in factor:
failures$failure <- as.factor(failures$failure)

# model (chr) in factor:
machines$model <- as.factor(machines$model)








##########################
### DATA VISUALIZATION ###
##########################

ggplot(data = telemetry %>% filter(machineID==1, datetime>as.POSIXct('2015-01-01'),
       datetime<as.POSIXct('2015-02-01')),
       aes(x=datetime, y=volt)) + geom_line(color='red') +
         labs(x='timestamps year 2015', y='volts')

ggplot(data = errors, aes(x=errorID)) + geom_bar(fill='orange', stat='count') +
  labs(title="Number of errors by Type", x='error types')

ggplot(data = errors %>% filter(machineID==1),
       aes(x=errorID)) + geom_bar(fill='orange', stat='count') +
  labs(title='Number of error by type for machine 1', x='error types')

ggplot(data = maint, 
       aes(x=comp)) + geom_bar(fill='red', stat='count') +
  labs(title='Number of components replaced by type', x='component types')

ggplot(data = maint %>% filter(machineID==1),
       aes(x=comp)) + geom_bar(fill='red', stat='count') +
  labs(title='Number of components replaced by type for machine 1', x='component types')

ggplot(data = maint,
       aes(x=machineID)) + geom_bar(aes(fill=comp), stat='count') +
  labs(title='Number of components replaced by type for each machine', x='machineID')

ggplot(data = machines,
       aes(x=age)) + geom_bar(fill='red', stat='count') +
  labs(title='Number of machines of a certain age', x='age')

ggplot(data = failures,
       aes(x=failure)) + geom_bar(fill='orange', stat='count') +
  labs(title='Number of failures of a certain type', x='failure type')

ggplot(data = failures,
       aes(x=machineID)) + geom_bar(aes(fill=failure), stat='count') +
  labs(title='Number of failure of a certain type for each machine', x='machineID')








###########################
### FEATURE ENGINEERING ###
###########################

# telemetry lag features
# for volt, rotate, pressure, vibration with mean and std every 3 hours (i.e. by 3).
# two lag windows (short and long term history): 3 h; 24 h.
# align=right -> the function is applied to the right border of the lag windows
# missing values filled with NA

telemetrymean <- telemetry %>%
  arrange(machineID, datetime) %>%
  group_by(machineID) %>%
  mutate(voltmean = rollapply(volt, width=3, FUN=mean, align='right', fill=NA, by=3),
         rotatemean = rollapply(rotate, width=3, FUN=mean, align='right', fill=NA, by=3),
         pressuremean = rollapply(pressure, width=3, FUN=mean, align='right', fill=NA, by=3),
         vibrationmean = rollapply(vibration, width=3, FUN=mean, align='right', fill=NA, by=3)) %>%
  select(datetime, machineID, voltmean, rotatemean, pressuremean, vibrationmean) %>%
  filter(!is.na(voltmean)) %>%
  ungroup()

telemetrysd <- telemetry %>%
  arrange(machineID, datetime) %>%
  group_by(machineID) %>%
  mutate(voltsd = rollapply(volt, width=3, FUN=sd, align='right', fill=NA, by=3),
         rotatesd = rollapply(rotate, width=3, FUN=sd, align='right', fill=NA, by=3),
         pressuresd = rollapply(pressure, width=3, FUN=sd, align='right', fill=NA, by=3),
         vibrationsd = rollapply(vibration, width=3, FUN=sd, align='right', fill=NA, by=3)) %>%
  select(datetime, machineID, voltsd, rotatesd, pressuresd, vibrationsd) %>%
  filter(!is.na(voltsd)) %>%
  ungroup()

telemetrymean_24hrs <- telemetry %>%
  arrange(machineID, datetime) %>%
  group_by(machineID) %>%
  mutate(voltmean_24hrs = rollapply(volt, width=24, FUN=mean, align='right', fill=NA, by=3),
         rotatemean_24hrs = rollapply(rotate, width=24, FUN=mean, align='right', fill=NA, by=3),
         pressuremean_24hrs = rollapply(pressure, width=24, FUN=mean, align='right', fill=NA, by=3),
         vibrationmean_24hrs = rollapply(vibration, width=24, FUN=mean, align='right', fill=NA, by=3)) %>%
  select(datetime, machineID, voltmean_24hrs, rotatemean_24hrs, pressuremean_24hrs, vibrationmean_24hrs) %>%
  filter(!is.na(voltmean_24hrs)) %>%
  ungroup()

telemetrysd_24hrs <- telemetry %>%
  arrange(machineID, datetime) %>%
  group_by(machineID) %>%
  mutate(voltsd_24hrs = rollapply(volt, width=24, FUN=sd, align='right', fill=NA, by=3),
         rotatesd_24hrs = rollapply(rotate, width=24, FUN=sd, align='right', fill=NA, by=3),
         pressuresd_24hrs = rollapply(pressure, width=24, FUN=sd, align='right', fill=NA, by=3),
         vibrationsd_24hrs = rollapply(vibration, width=24, FUN=sd, align='right', fill=NA, by=3)) %>%
  select(datetime, machineID, voltsd_24hrs, rotatesd_24hrs, pressuresd_24hrs, vibrationsd_24hrs) %>%
  filter(!is.na(voltsd_24hrs)) %>%
  ungroup()

# merge
telemetryfeat <- data.frame(telemetrymean, telemetrysd[,-c(1:2)])
telemetryfeat_24hrs <- data.frame(telemetrymean_24hrs, telemetrysd_24hrs[,-c(1:2)])

# left join of telemetryfeat and telemetryfeat_24hrs by datetime, machineID
# all rows of telemetryfeat; for all values of datetime not present in telemetryfeat_24hrs, NA is put
# all rows with NA are discharged

telemetryfeat_final <- telemetryfeat %>%
  left_join(telemetryfeat_24hrs, by=c('datetime', 'machineID')) %>%
  filter(!is.na(voltmean_24hrs))

# errors lag features
# errorID is categorical, not possible aggregation through average
# the aggregation will be done by sum, with 24 hours windows (long term history), skipping by 3

# preliminary step 1: dummy variable creation
errorcount <- errors %>%
  mutate(error1 = as.integer(errorID=='error1'),
         error2 = as.integer(errorID=='error2'),
         error3 = as.integer(errorID=='error3'),
         error4 = as.integer(errorID=='error4'),
         error5 = as.integer(errorID=='error5'))

# preliminary step 2: sum of error# after grouping both by machine and by date
errorcount_final <- errorcount %>%
  group_by(machineID,datetime) %>%
  summarize(error1sum = sum(error1),
            error2sum = sum(error2),
            error3sum = sum(error3),
            error4sum = sum(error4),
            error5sum = sum(error5)) %>%
  ungroup()

# less entries in errors w.r.t. telemetry
# preliminary step 3: left join
errorfeat <- telemetry %>% select(datetime, machineID) %>%
  left_join(errorcount_final, by=c('datetime', 'machineID'))

# all NA must be 0s
errorfeat[is.na(errorfeat)] <- 0

# lag feature step:
# count the number of errors of different types in the last 24 hrs, every 3
errorfeat_final <- errorfeat %>%
  arrange(machineID, datetime) %>%
  group_by(machineID) %>%
  mutate(error1count=rollapply(error1sum, width=24, FUN=sum, align='right', fill=NA, by=3),
         error2count=rollapply(error2sum, width=24, FUN=sum, align='right', fill=NA, by=3),
         error3count=rollapply(error3sum, width=24, FUN=sum, align='right', fill=NA, by=3),
         error4count=rollapply(error4sum, width=24, FUN=sum, align='right', fill=NA, by=3),
         error5count=rollapply(error5sum, width=24, FUN=sum, align='right', fill=NA, by=3)) %>%
  select(datetime, machineID, error1count, error2count, error3count, error4count, error5count) %>%
  filter(!is.na(error1count)) %>%
  ungroup()

# maintenance
# computing time since a component is last replaced (for all 4)
# preliminary step 1: dummy for each substitution (comp#)
comprep <- maint %>%
  select(datetime, machineID, comp) %>%
  mutate(comp1 = as.integer(comp=='comp1'),
         comp2 = as.integer(comp=='comp2'),
         comp3 = as.integer(comp=='comp3'),
         comp4 = as.integer(comp=='comp4')) %>%
  select(-comp)

# data.table fastest package for data maipulation
# setkey order data.table using cols as keys, ascending by default
# preliminary step 2: separate different component type replacements into different tables
# lastrepcomp = datetime
comprep<-as.data.table(comprep)
setkey(comprep,machineID, datetime)
comp1rep <- comprep[comp1==1, list(machineID, datetime, lastrepcomp1=datetime)]
comp2rep <- comprep[comp2==1, list(machineID, datetime, lastrepcomp2=datetime)]
comp3rep <- comprep[comp3==1, list(machineID, datetime, lastrepcomp3=datetime)]
comp4rep <- comprep[comp4==1, list(machineID, datetime, lastrepcomp4=datetime)]

# preliminary step 3: rolling joins; it associate each element of a table A to the most successive elements
# of another table B, but antecedent w.r.t the next element of A (according to a date);
# if the key is multi-column, the roll column should be the last keyed column
compdate <- as.data.table(telemetryfeat_final[,c(1:2)])
setkey(compdate,machineID, datetime)

comp1feat<-comp1rep[compdate[,list(machineID, datetime)], roll=TRUE]
comp1feat$sincelastcomp1<-as.numeric(difftime(comp1feat$datetime, comp1feat$lastrepcomp1, units="days"))

comp2feat<-comp2rep[compdate[,list(machineID, datetime)], roll=TRUE]
comp2feat$sincelastcomp2<-as.numeric(difftime(comp2feat$datetime, comp2feat$lastrepcomp2, units="days"))

comp3feat<-comp3rep[compdate[,list(machineID, datetime)], roll=TRUE]
comp3feat$sincelastcomp3<-as.numeric(difftime(comp3feat$datetime, comp3feat$lastrepcomp3, units="days"))

comp4feat<-comp4rep[compdate[,list(machineID, datetime)], roll=TRUE]
comp4feat$sincelastcomp4<-as.numeric(difftime(comp4feat$datetime, comp4feat$lastrepcomp4, units="days"))

compfeat_final <- data.frame(
  compdate, 
  comp1feat[,list(sincelastcomp1)],
  comp2feat[,list(sincelastcomp2)],
  comp3feat[,list(sincelastcomp3)],
  comp4feat[,list(sincelastcomp4)]
)

#final table; merge
finalfeat <- data.frame(telemetryfeat_final, errorfeat_final[,-c(1:2)])
finalfeat <- finalfeat %>%
  left_join(compfeat_final, by=c('datetime', 'machineID')) %>%
  left_join(machines, by=c('machineID'))

str(finalfeat)







#####################
### DATA LABELING ###
#####################


# labeling is done by taking a 24 hrs time window prior to the failure of a component
# and labeling the feature records that fall into that window
# as 'comp# (about to fail)', while the others are 'none'

# left join between final feat and failures by machineID (resulting in two datetime columns)
# datetime difference between thw two
# the prediction horizon is 24 hrs: other differences discharged (they are in finalfeat)

labeled <- left_join(finalfeat, failures, by=c('machineID')) %>%
  mutate(datediff=difftime(datetime.y, datetime.x, units='hours')) %>%
  filter(datediff<=24, datediff>=0) 

labeledfeatures <- left_join(finalfeat,
                             labeled %>% select(datetime.x, machineID, failure),
                             by=c('datetime'='datetime.x', 'machineID')) %>%
  arrange(machineID, datetime)

# adding level 'none'
levels(labeledfeatures$failure) <- c(levels(labeledfeatures$failure), 'none')
labeledfeatures$failure[is.na(labeledfeatures$failure)] <- 'none'






################
### MODELING ###
################

# time dependent splitting: training on older records and validating/testing using newer records
# removing boundary records to avoid sharing time windows between training and validation/test sets
# 24 hours data prior to timepoint split is ignored
# three choices of splitting: 8/4 mounth; 9/3 mounth; 10/2 mounth

#trainingdata1 <- labeledfeatures[labeledfeatures$datetime < "2015-07-31 01:00:00",]
#testingdata1 <-labeledfeatures[labeledfeatures$datetime > "2015-08-01 01:00:00",]

trainingdata2 <- labeledfeatures[labeledfeatures$datetime < "2015-08-31 01:00:00",]
testingdata2 <-labeledfeatures[labeledfeatures$datetime > "2015-09-01 01:00:00",]

#trainingdata3 <- labeledfeatures[labeledfeatures$datetime < "2015-09-30 01:00:00",]
#testingdata3 <-labeledfeatures[labeledfeatures$datetime > "2015-10-01 01:00:00",]

# generalized boosted regression model
trainformula <- as.formula(paste(
  'failure',
  paste(names(labeledfeatures)[c(3:29)], collapse=' + '),
  sep=' ~ '
  ))

set.seed(1234)
model <- gbm(formula=trainformula,
             data=trainingdata2,
             distribution='multinomial',
             n.trees=50, 
             interaction.depth=5,
             shrinkage=0.1)

summary(model)
pred_model <- as.data.frame(
  predict(model, testingdata2, n.trees=50, type = 'response'))

names(pred_model) <- gsub('.50', '', names(pred_model))

pred_model$failurePredicted <- as.factor(
  colnames(pred_model)[max.col(pred_model)]
)

#prediction <- testingdata2 %>% 
#  mutate(failurePredicted = select(pred_model, failurePredicted))

#head(prediction)
#prediction_1 <- filter(prediction, failure!='none')

#View(filter(prediction_1, failurePredicted=='none'))

#prediction1<-testingdata1 %>%
#  mutate(failurePredicted=as.factor(pred_gbm1$failure))

Evaluate<-function(actual=NULL, predicted=NULL, cm=NULL){
  if(is.null(cm)) {
    actual = actual[!is.na(actual)]
    predicted = predicted[!is.na(predicted)]
    f = factor(union(unique(actual), unique(predicted)))
    actual = factor(actual, levels = levels(f))
    predicted = factor(predicted, levels = levels(f))
    cm = as.matrix(table(Actual=actual, Predicted=predicted))
  }
  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the classes
  q = colsums / n # distribution of instances over the predicted classes
  #accuracy
  accuracy = sum(diag) / n
  #per class
  recall = diag / rowsums
  precision = diag / colsums
  f1 = 2 * precision * recall / (precision + recall)
  #macro
  macroPrecision = mean(precision)
  macroRecall = mean(recall)
  macroF1 = mean(f1)
  #1-vs-all matrix
  oneVsAll = lapply(1 : nc,
                    function(i){
                      v = c(cm[i,i],
                            rowsums[i] - cm[i,i],
                            colsums[i] - cm[i,i],
                            n-rowsums[i] - colsums[i] + cm[i,i]);
                      return(matrix(v, nrow = 2, byrow = T))})
  s = matrix(0, nrow=2, ncol=2)
  for(i in 1:nc){s=s+oneVsAll[[i]]}
  #avg accuracy
  avgAccuracy = sum(diag(s))/sum(s)
  #micro
  microPrf = (diag(s) / apply(s,1, sum))[1];
  #majority class
  mcIndex = which(rowsums==max(rowsums))[1] # majority-class index
  mcAccuracy = as.numeric(p[mcIndex])
  mcRecall = 0*p; mcRecall[mcIndex] = 1
  mcPrecision = 0*p; mcPrecision[mcIndex] = p[mcIndex]
  mcF1 = 0*p; mcF1[mcIndex] = 2 * mcPrecision[mcIndex] / (mcPrecision[mcIndex] + 1)
  #random accuracy
  expAccuracy = sum(p*q)
  #kappa
  kappa = (accuracy - expAccuracy) / (1 - expAccuracy)
  #random guess
  rgAccuracy = 1 / nc
  rgPrecision = p
  rgRecall = 0*p + 1 / nc
  rgF1 = 2 * p / (nc * p + 1)
  #rnd weighted
  rwgAccurcy = sum(p^2)
  rwgPrecision = p
  rwgRecall = p
  rwgF1 = p
  classNames = names(diag)
  if(is.null(classNames)) classNames = paste("C",(1:nc),sep="")
  return(list(
    ConfusionMatrix = cm,
    Metrics = data.frame(
      Class = classNames,
      Accuracy = accuracy,
      Precision = precision,
      Recall = recall,
      F1 = f1,
      MacroAvgPrecision = macroPrecision,
      MacroAvgRecall = macroRecall,
      MacroAvgF1 = macroF1,
      AvgAccuracy = avgAccuracy,
      MicroAvgPrecision = microPrf,
      MicroAvgRecall = microPrf,
      MicroAvgF1 = microPrf,
      MajorityClassAccuracy = mcAccuracy,
      MajorityClassPrecision = mcPrecision,
      MajorityClassRecall = mcRecall,
      MajorityClassF1 = mcF1,
      Kappa = kappa,
      RandomGuessAccuracy = rgAccuracy,
      RandomGuessPrecision = rgPrecision,
      RandomGuessRecall = rgRecall,
      RandomGuessF1 = rgF1,
      RandomWeightedGuessAccurcy = rwgAccurcy,
      RandomWeightedGuessPrecision = rwgPrecision,
      RandomWeightedGuessRecall= rwgRecall,
      RandomWeightedGuessWeightedF1 = rwgF1)))
}

eval <- Evaluate(actual=testingdata2$failure,predicted=pred_model$failurePredicted)
eval$ConfusionMatrix
t(eval$Metrics)

rownames <- c("comp1","comp2","comp3","comp4","none")
data.frame(failure = rownames, Recall = eval$Metrics$Recall)





