library(ggpattern)
library(tidyverse)
library(ggplot2)
library(caret)
library(caretEnsemble)
library(psych)
library(Amelia)
library(GGally)
library(rpart)
library(randomForest)
library(e1071)
library(ISLR)
library(class)
library(groupdata2)


### Importing my Data
crime = read.csv("crime.csv")
crime_original = read.csv("crime.csv")
sort(table(crime$Category))
crime = replace(crime, crime=='', NA)
crime = na.omit(crime)
crime$Category[crime$Descript=="DOMESTIC VIOLENCE"] = "DOMESTIC VIOLENCE"
crime = subset(crime, Category=="LARCENY/THEFT" | Category=="NON-CRIMINAL" | Category=="ASSAULT" |
                 Category=="VANDALISM" | Category=="VEHICLE THEFT" | Category=="WARRANTS" | 
                 Category=="BURGLARY" | Category=="SUSPICIOUS OCC" | Category=="MISSING PERSON" | 
                 Category=="DRUG/NARCOTIC" | Category=="ROBBERY" | Category=="FRAUD" | 
                 Category=="DOMESTIC VIOLENCE" | Category=="TRESPASS" | Category=="WEAPON LAWS")
categories = c("LARCENY/THEFT", 'NON-CRIMINAL', 'ASSAULT', 'VANDALISM', 'VEHCILE THEFT', 
               'WARRANTS', 'BURGLRARY', 'SUSPICIOUS OCC', 'MISSING PERSON', 'DRUG/NARCOTIC', 
               'ROBBERY', 'FRAUD', 'DOMESTIC VIOLENCE', 'TRESPASS', 'WEAPON LAWS')
crime$PdDistrict = as.factor(crime$PdDistrict)
crime$DayOfWeek = as.factor(crime$DayOfWeek)
crime = crime[ , !names(crime) %in% 
    c("Resolution","PdId", "Location")]
crime$Date = as.Date(crime$Date, "%m/%d/%Y")
crime$Hour = substring(crime$Time,1,2)
crime$Hour = as.numeric(crime$Hour)
crime$violent = 0
crime$nonviolent = 0
crime$violent[crime$Category=="ASSAULT"] = 1
crime$violent[crime$Category=="BURGLARY"] = 1
crime$violent[crime$Category=="ROBBERY"] = 1
crime$violent[crime$Category=="DOMESTIC VIOLENCE"] = 1
crime$violent[crime$Category=="WEAPON LAWS"] = 1
crime$nonviolent[crime$violent==0] = 1
crime$violent = as.factor(crime$violent)
crime$nonviolent = as.factor(crime$nonviolent)
crime$Hour = as.integer(crime$Hour)
violent_crimes = c("ASSAULT", "BURGLARY", "ROBBERY", "DOMESTIC VIOLENCE", "WEAPON LAWS")

### Exploratory Data Analysis
variable_data = setNames(stack(sapply(crime, class))[2:1], c('variable', 'class'))
descriptions = c('Identification number for each record', 'Category of crime', 'Description of incident', 
                 'Day of the week on which the crime occurred', 'Date on which the crime occurred', 
                 'Time at which the crime occurred', 'Police district in which the crime occurred', 
                 'Address at which the crime occurred',  'Longitude coordinate for location of incident', 
                 'Latitude coordinate for location of incident', 'Hour of the day at which the crime occurred (0 = hour directly after midnight)', 
                 '1 = violent', '1 = nonviolent')
variable_data$description = descriptions
gt::gt(variable_data)
cat_counts = list()
for (category in unique(crime$Category)) {
  count = sum(crime$Category == category)
  cat_counts[[category]] = count
}
cat_totals = data.frame(Category = names(cat_counts), Total = unlist(cat_counts))
ggplot(cat_totals, aes(x=Category, y=Total, fill=Category)) +
  geom_bar(stat="identity", color="black", linewidth=0.5) +
  labs(x="Category", y="Total", title="Total Incidents by Category") +
  theme(legend.position="top", legend.text=element_text(size=6), legend.title=element_text(size=10),
        axis.text.x=element_blank()) +
  scale_fill_discrete(name="Category")
cat_totals$Percent = (cat_totals$Total/sum(cat_totals$Total))*100
ggplot(cat_totals, aes(x="", y=Total, fill=Category)) +
  geom_bar(stat="identity", width=1, color="black", linewidth=0.5) +
  coord_polar("y", start=0) +
  theme_void() +
  scale_fill_discrete(name="Category", labels=paste(cat_totals$Category, " (", 
                                                    round(cat_totals$Percent), "%)", sep="")) +
  labs(title="Total Incidents by Category") +
  theme(legend.position="right")
day_counts = list()
for (day in unique(crime$DayOfWeek)) {
  count = sum(crime$DayOfWeek == day)
  day_counts[[day]] = count
}
day_totals = data.frame(DayOfWeek = names(day_counts), Total = unlist(day_counts))
ggplot(day_totals, aes(x=DayOfWeek, y=Total, fill=DayOfWeek)) +
  geom_bar(stat="identity", color="black", linewidth=0.5) +
  labs(x="Day of the Week", y="Total", title="Total Incidents by Day of the Week") +
  theme(legend.position="top", legend.text=element_text(size=8), legend.title=element_text(size=10),
        axis.text.x=element_blank()) +
  scale_fill_discrete(name="Day of the Week") +
  geom_text(aes(label=Total), position=position_stack(vjust=0.5), size=3)
hourly_incidents = crime %>% group_by(Hour) %>% summarise(n = n())
ggplot(hourly_incidents, aes(x = Hour, y = n, fill = n)) +
  geom_bar(stat = "identity", color = "black", linewidth = 0.5) +
  labs(x = "Hour of The Day", y = "Number of Incidents", title = "Total Incidents by Hour of Day") +
  scale_fill_gradient(low = "yellow", high = "red")
crime$AM = 0
crime$PM = 0
crime$AM[crime$Hour >= 0 & crime$Hour < 12] = 1
crime$PM[crime$Hour > 11] = 1
crime$AM = as.factor(crime$AM)
crime$PM = as.factor(crime$PM)
crime_counts = aggregate(cbind(AM, PM) ~ Category, data = crime, FUN = sum)
crime_counts_long = reshape2::melt(crime_counts, id.vars = "Category", variable.name = "time_of_day", 
                                   value.name = "count")
ggplot(crime_counts_long, aes(x = time_of_day, y = count, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge", color="black", linewidth=0.5) +
  labs(x = "Time of Day", y = "Count", fill = "Category of Crime") +
  ggtitle("Counts of Crimes by Category and Time of Day") +
  scale_fill_discrete(name = "Category of Crime") 
pd_counts = list()
for (pd in unique(crime$PdDistrict)) {
  count = sum(crime$PdDistrict == pd)
  pd_counts[[pd]] = count
}
pd_totals = data.frame(PdDistrict = names(pd_counts), Total = unlist(pd_counts))
ggplot(pd_totals, aes(x=PdDistrict, y=Total, fill=PdDistrict)) +
  geom_bar(stat="identity", color="black", linewidth=0.5) +
  labs(x="SF Police District", y="Total", title="Total Incidents by SF Police District") +
  theme(legend.position="top", legend.text=element_text(size=6), legend.title=element_text(size=10),
        axis.text.x=element_blank()) +
  scale_fill_discrete(name="Police District") +
  geom_text(aes(label=Total), position=position_stack(vjust=0.5), size=3)
violent = crime[crime$violent==1,]
nonviolent = crime[crime$violent==0,]
table(crime$violent) / nrow(crime)
violent_pd_totals = aggregate(violent$IncidntNum, by=list(violent$PdDistrict), FUN=length)
colnames(violent_pd_totals) = c("PdDistrict", "ViolentTotal")
nonviolent_pd_totals = aggregate(nonviolent$IncidntNum, by=list(nonviolent$PdDistrict), FUN=length)
colnames(nonviolent_pd_totals) = c("PdDistrict", "NonviolentTotal")
pd_totals = merge(violent_pd_totals, nonviolent_pd_totals, by="PdDistrict")
pd_totals$Total = pd_totals$ViolentTotal + pd_totals$NonviolentTotal
ggplot(pd_totals, aes(x = PdDistrict, y = round(ViolentTotal/Total,2), fill = PdDistrict)) +
  geom_bar(aes(y = round(ViolentTotal/Total,2)), stat = "identity", position = "dodge", color="black", linewidth=0.5) +
  labs(x = "SF Police District", y = "Count", fill = "Police District") +
  theme(legend.position="top", legend.text=element_text(size=6), legend.title=element_text(size=10),
        axis.text.x=element_blank()) +
  ggtitle("Counts of Violent Crimes by Police District") +
  scale_fill_discrete(name = "Police District") +
  geom_text(aes(label=round(ViolentTotal/Total,2)), position=position_stack(vjust=0.5), size=3) +
  geom_hline(yintercept=sum(pd_totals$ViolentTotal)/sum(pd_totals$Total), linetype="dashed")
ggplot(pd_totals, aes(x = PdDistrict, y = round(NonviolentTotal/Total,2), fill = PdDistrict)) +
  geom_bar(aes(y = round(NonviolentTotal/Total,2)), stat = "identity", position = "dodge", color="black", linewidth=0.5) +
  labs(x = "SF Police District", y = "Count", fill = "Police District") +
  theme(legend.position="top", legend.text=element_text(size=6), legend.title=element_text(size=10),
        axis.text.x=element_blank()) +
  ggtitle("Counts of Non-Violent Crimes by Police District") +
  scale_fill_discrete(name = "Police District") +
  geom_text(aes(label=round(NonviolentTotal/Total,2)), position=position_stack(vjust=0.5), size=3) +
  geom_hline(yintercept=sum(pd_totals$NonviolentTotal)/sum(pd_totals$Total), linetype="dashed")

### Analysis: Statistical Modelling
indxTrain = createDataPartition(y = crime$violent,p = 0.8,list = FALSE)
training = crime[indxTrain,]
testing = crime[-indxTrain,]
predictors = c('DayOfWeek','Hour','Date','X','Y','PdDistrict')
prop.table(table(training$violent)) * 100
prop.table(table(testing$violent)) * 100

set.seed(500)
x = training[, predictors]
x$violent = training$violent
dt_model = rpart(violent~., data = x, method="class")
dt_predictions = predict(dt_model, newdata = testing, type="class")
dt_confmat = confusionMatrix(dt_predictions, testing$violent)
dt_confmat

predictors = c('DayOfWeek','Hour','Date','X','Y')
training$Date = as.numeric(training$Date)
testing$Date = as.numeric(testing$Date)
x = training[, predictors]
y = training$violent
set.seed(500)
nb_model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))
nb_predictions = predict(nb_model, newdata = testing )
nb_confmat = confusionMatrix(nb_predictions, testing$violent)
nb_confmat

set.seed(500)
rf_model = randomForest(formula = violent~DayOfWeek+Hour+Date+X+Y+PdDistrict, data=training)
rf_predictions = predict(rf_model, newdata = testing)
rf_confmat = confusionMatrix(rf_predictions, testing$violent)
rf_confmat

### Analysis: Balanced Datasets
training_downsample = downsample(training, cat_col = "violent")
table(training$violent) / nrow(training)
table(training_downsample$violent) / nrow(training_downsample)
testing_downsample = downsample(testing, cat_col = "violent")
table(testing$violent) / nrow(testing)
table(testing_downsample$violent) / nrow(testing_downsample)

set.seed(500)
x = training_downsample[, predictors]
x$violent = training_downsample$violent
dt_model = rpart(violent~., data = x, method="class")
dt_predictions = predict(dt_model, newdata = testing_downsample, type="class")
dt_confmat = confusionMatrix(dt_predictions, testing_downsample$violent)
dt_confmat

set.seed(500)
predictors = c('DayOfWeek','Hour','Date','X','Y')
training$Date = as.numeric(training$Date)
testing$Date = as.numeric(testing$Date)
x = training_downsample[, predictors]
y = training_downsample$violent
set.seed(500)
nb_model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))
nb_predictions = predict(nb_model, newdata = testing_downsample)
nb_confmat = confusionMatrix(nb_predictions, testing_downsample$violent)
nb_confmat

set.seed(500)
rf_model = randomForest(formula = violent~DayOfWeek+Hour+Date+X+Y+PdDistrict, data=training_downsample)
rf_predictions = predict(rf_model, newdata = testing_downsample)
rf_confmat = confusionMatrix(rf_predictions, testing_downsample$violent)
rf_confmat