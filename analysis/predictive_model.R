## Derrick Stuckey
## Pegged Software Assessment
## 07/16/2016

## Part 3 ##

# install.packages("pROC")
library(pROC)

# pull in data from exploration, use constructed app_attrition_data dataframe
source("exploration.R")

# construct target variables
app_attrition_data$attritted_6mo <- app_attrition_data$tenure_length < 180
app_attrition_data$tenured_1y <- app_attrition_data$tenure_length >= 365

# set 6 month attrition flag to NA if tenure < 6 months and still employed
app_attrition_data$attritted_6mo[!app_attrition_data$attritted_6mo & 
                                 app_attrition_data$currently_employed] <- NA
# same for 1 year tenure flag
app_attrition_data$tenured_1y[!app_attrition_data$tenured_1y & 
                                app_attrition_data$currently_employed] <- NA

app_attrition_data$tenured_6mo_to_1y <- !app_attrition_data$attritted_6mo & !app_attrition_data$tenured_1y

# keep only records with application data info
app_attrition_data <- app_attrition_data[!is.na(app_attrition_data$app_client),]

# see what we're left with
summary(app_attrition_data$attritted_6mo)
summary(app_attrition_data$tenured_1y)
summary(app_attrition_data$tenured_6mo_to_1y)
dim(app_attrition_data)

# convert all question answers to factors
for (i in 1:25) {
  print(i)
  statement <- paste("app_attrition_data$answer",i,
                     " <- as.factor(app_attrition_data$answer",i,")",sep="")
  print(statement)
  eval(parse(text=statement))
}
head(app_attrition_data$answer1)
head(app_attrition_data$answer5)
head(app_attrition_data$answer25)

# convert character strings to factors
app_attrition_data$hired_client <- as.factor(app_attrition_data$hired_client)
app_attrition_data$hire_job_category <- as.factor(app_attrition_data$hire_job_category)

# clean up other predictors
app_attrition_data$device_type[is.na(app_attrition_data$device_type)] <- "Unknown"
app_attrition_data$device_type <- as.factor(app_attrition_data$device_type)
app_attrition_data$log_total_time <- as.numeric(app_attrition_data$log_total_time)

# split data into training and test
set.seed(11235)
test_idx <- sample(nrow(app_attrition_data),nrow(app_attrition_data)*0.2,replace = FALSE)
test_data <- app_attrition_data[test_idx,]
train_data <- app_attrition_data[-test_idx,]

# examine relationships of individual answers to tenure
for (i in 1:25) {
  print(i)
  statement <- paste("chisq.test(train_data_6mo$answer",i,
                     ",train_data_6mo$attritted_6mo)",sep="")
  print(statement)
  print(eval(parse(text=statement)))
  statement2 <- paste("boxplot(train_data$tenure_length ~ train_data$answer",i,", outline=FALSE,main='answer",i,"')",sep="")
  eval(parse(text=statement2))
}

# construct apparently useful features (too many to include all answers for small training set)
app_attrition_data$answer25_4 <- app_attrition_data$answer25==4
app_attrition_data$answer23_4 <- app_attrition_data$answer23==4
app_attrition_data$answer18_4 <- app_attrition_data$answer18==4
app_attrition_data$answer16_4 <- app_attrition_data$answer16==4
app_attrition_data$answer12_4 <- app_attrition_data$answer12==4
app_attrition_data$answer9_4 <- app_attrition_data$answer9==4

# add these features to the training and test data sets
test_data <- app_attrition_data[test_idx,]
train_data <- app_attrition_data[-test_idx,]

names(train_data)

# set up data to build a model for predicting 6-month attrition
train_data_6mo <- train_data[!is.na(train_data$attritted_6mo),]
train_data_6mo <- subset(train_data_6mo, select=-c(user_id ,app_client ,tenure_length ,tenured_1y ,currently_employed,device,tenure_length_adj,tenured_6mo_to_1y))
names(train_data_6mo)

dim(train_data_6mo)
dim(na.omit(train_data_6mo))
# can safely drop NAs and only lose 3 records
train_data_6mo <- na.omit(train_data_6mo)

# build the logistic model and trim features using stepwise regression
attritted_6mo_model <- glm(attritted_6mo ~ log_total_time + device_type + exp(log_total_time) +
                                      answer9_4 + answer12_4 + answer16_4 + answer18_4 + answer23_4 + answer25_4,
                                 data=train_data_6mo)
summary(attritted_6mo_model)
# step(attritted_6mo_model)
# summary(attritted_6mo_model)

# test the predictions
test_data_6mo <- test_data[!is.na(test_data$attritted_6mo),]
test_data_6mo <- subset(test_data_6mo, select=-c(user_id ,app_client ,tenure_length ,tenured_1y ,currently_employed,device,tenure_length_adj,tenured_6mo_to_1y))

dim(test_data_6mo)
test_data_6mo <- na.omit(test_data_6mo)
dim(test_data_6mo)

test_data_6mo$predicted_attritted_6mo <- predict(attritted_6mo_model,newdata=test_data_6mo)
# table(test_data_6mo$predicted_attritted_6mo>0, test_data_6mo$attritted_6mo)
boxplot(test_data_6mo$predicted_attritted_6mo ~ test_data_6mo$attritted_6mo,outline=FALSE)

g <- roc(attritted_6mo ~ predicted_attritted_6mo, data = test_data_6mo)
coords(g, "best")
table(test_data_6mo$predicted_attritted_6mo>0.1565399, test_data_6mo$attritted_6mo)
plot(g, main="6-month Attrition ROC Curve")

## Repeat for 1-year attrition ##
train_data_1y <- train_data[!is.na(train_data$tenured_1y) & 
                              !is.na(train_data$answer23_4) &
                              !is.na(train_data$answer25_4),]
test_data_1y <- test_data[!is.na(test_data$tenured_1y) & 
                            !is.na(test_data$answer23_4) &
                            !is.na(test_data$answer25_4),]

tenured_1y_model <- glm(tenured_1y ~ log_total_time + device_type + exp(log_total_time) +
                             answer9_4 + answer12_4 + answer16_4 + answer18_4 + answer23_4 + answer25_4,
                           data=train_data_1y)
summary(tenured_1y_model)

# test the predictions
test_data_1y$predicted_tenured_1y <- predict(tenured_1y_model,newdata=test_data_1y)
boxplot(test_data_1y$predicted_tenured_1y ~ test_data_1y$tenured_1y,outline=FALSE)

g <- roc(tenured_1y ~ predicted_tenured_1y, data = test_data_1y)
coords(g, "best")
table(test_data_1y$predicted_tenured_1y>0.7740606, test_data_1y$tenured_1y)
plot(g, main="1-Year Attrition ROC Curve")

## 6 month to 1 year ##
test_data_6mo_to_1y <- merge(subset(test_data_6mo, select=c(predicted_attritted_6mo,attritted_6mo)),
                             subset(test_data_1y, select=c(predicted_tenured_1y,tenured_1y)),
                             by="row.names")

dim(test_data_6mo_to_1y)
head(test_data_6mo_to_1y)

test_data_6mo_to_1y$predicted_6mo_to_1y <- (- test_data_6mo_to_1y$predicted_tenured_1y - test_data_6mo_to_1y$predicted_attritted_6mo)
test_data_6mo_to_1y$tenured_6mo_to_1y <- !test_data_6mo_to_1y$tenured_1y & !test_data_6mo_to_1y$attritted_6mo

g <- roc(tenured_6mo_to_1y ~ predicted_6mo_to_1y, data = test_data_6mo_to_1y)
coords(g, "best")
table(test_data_6mo_to_1y$predicted_6mo_to_1y>0.928865, test_data_6mo_to_1y$tenured_6mo_to_1y)
plot(g, main="6-Month to 1-Year Attrition ROC Curve")

