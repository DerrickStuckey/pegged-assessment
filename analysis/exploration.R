## Derrick Stuckey
## Pegged Software Assessment
## 07/16/2016

# load the data
data_exercise_applicants <- read.csv("../data/data_exercise_applicants.csv", stringsAsFactors = FALSE)
data_exercise_hires <- read.csv("../data/data_exercise_hires.csv", stringsAsFactors = FALSE)

# check it out
dim(data_exercise_applicants)
dim(data_exercise_hires)

names(data_exercise_hires)
names(data_exercise_applicants)

# View(data_exercise_applicants)
# View(data_exercise_hires)

# any duplicate ids in the datasets?
summary(duplicated(data_exercise_applicants$user_id))
summary(duplicated(data_exercise_hires$user_id))

# check out the duplicates
hire_dupe_ids <- data_exercise_hires$user_id[duplicated(data_exercise_hires$user_id)]
# View(data_exercise_hires[data_exercise_hires$user_id %in% hire_dupe_ids,])

# remove duplicates by adding tenure together for same user, same client, same category
# aka consider gaps in employment to be "leave of absence" rather than two separate instances
# TODO get feedback on this methodology
data_exercise_hires$currently_employed <- data_exercise_hires$currently_employed=="Y"
hires_deduped <- aggregate(data.frame(subset(data_exercise_hires, 
                                             select=c("tenure_length","currently_employed"))), 
                           by=list("user_id"=data_exercise_hires$user_id,
                                   "client"=data_exercise_hires$client,
                                   "hire_job_category"=data_exercise_hires$hire_job_category),
                           FUN=sum)
# change currently_employed back to boolean (effectively a logical OR)
hires_deduped$currently_employed <- as.logical(hires_deduped$currently_employed)

mean(duplicated(hires_deduped$user_id))
# a few duplicates remain due to hire_job_category; ignore these for now as they are less than 1%

# rename client vars to be clear when merged
names(hires_deduped)[names(hires_deduped)=="client"] <- "hired_client"
names(data_exercise_applicants)[names(data_exercise_applicants)=="client_name"] <- "app_client"

# merge the two datasets on user_id, keeping all entries of both sets
hires_applicants_outer_join <- merge(hires_deduped, data_exercise_applicants, by="user_id", 
                                 all.x=TRUE,all.y=TRUE)
dim(hires_applicants_outer_join)
dim(hires_deduped)
dim(data_exercise_applicants)
# View(hires_applicants_outer_join)

summary(hires_applicants_outer_join$hired_client)
summary(hires_applicants_outer_join$app_client)

# merge keeping only entries in applicants
all_applicants <- merge(hires_deduped, data_exercise_applicants, by="user_id", 
                                     all.x=FALSE,all.y=TRUE)
dim(all_applicants)
dim(data_exercise_applicants)

# do application client ids and hired client ids line up?
table(all_applicants$hired_client,all_applicants$app_client)
# no, but the mappings are consistent
# use the 'hired client' ids for consistency
all_applicants$app_client[all_applicants$app_client=="client1"] <- "client11"
all_applicants$app_client[all_applicants$app_client=="client2"] <- "client10"
all_applicants$app_client[all_applicants$app_client=="client3"] <- "client9"
all_applicants$app_client[all_applicants$app_client=="client4"] <- "client4"

## Percent Hired by Applicant ##

# calculate pct of applicants hired by each client
pct_hired <- aggregate(list("pct_hired"=!is.na(all_applicants$hired_client)),
                       by=list("app_client"=all_applicants$app_client),
                       FUN=mean)
pct_hired

pct_hired <- pct_hired[order(pct_hired$pct_hired),]

# plot the percentage hired by client
barplot(height=pct_hired$pct_hired*100, names.arg=pct_hired$app_client, col="light blue",
        main="Percent of Applications Hired",xlab="Client",ylab="Percent")

## 3-Month Attrition By Client ##

# throw out data with unknown tenure
attrition_data <- hires_deduped[!is.na(hires_deduped$tenure_length),]
dim(attrition_data)
dim(hires_deduped)

# basic data exploration
summary(attrition_data$tenure_length)
hist(attrition_data$tenure_length)
hist(attrition_data$tenure_length[attrition_data$tenure_length<10000],breaks=50)
hist(attrition_data$tenure_length[attrition_data$tenure_length<10000 & !attrition_data$currently_employed],breaks=50)

# throw out currently employed employees w/ under 3 months of tenure
# for better answer, use survival analysis w/ these employees censored; this is good enough for now
# assume 3 months == 90 days
attrition_data_3mo <- attrition_data[attrition_data$tenure_length >= 90 | !attrition_data$currently_employed,]
dim(attrition_data_3mo)
dim(attrition_data)

# do we have a reasonable number of data points for each job category?
table(attrition_data_3mo$hired_client)
# good enough

pct_attrition_3mo <- aggregate(list("pct_attrition"=attrition_data_3mo$tenure_length<90),
                               by=list("hired_client"=attrition_data_3mo$hired_client),
                               FUN=mean)
pct_attrition_3mo

# sort by attrition rate
pct_attrition_3mo <- pct_attrition_3mo[order(pct_attrition_3mo$pct_attrition),]

# plot the percentage 3-month attrition by client
barplot(height=pct_attrition_3mo$pct_attrition*100, names.arg=pct_attrition_3mo$hired_client, col="light blue",
        main="90-Day Attrition Rate",xlab="Percent",ylab="",horiz=TRUE,las=1)

## 6-Month Attrition by Job Category ##

# throw out currently employed employees w/ under 6 months of tenure
attrition_data_6mo <- attrition_data[attrition_data$tenure_length >= 180 | !attrition_data$currently_employed,]

dim(attrition_data_6mo)
dim(attrition_data)

# do we have a reasonable number of data points for each job category?
table(attrition_data_6mo$hire_job_category)
# a bit short on 'director'
# View(attrition_data_6mo[attrition_data_6mo$hire_job_category=="director",])
table(attrition_data$hire_job_category) # only 1 thrown out due to still being employed
# good enough

pct_attrition_6mo_jobcat <- aggregate(list("pct_attrition"=attrition_data_6mo$tenure_length<180),
                               by=list("job_category"=attrition_data_6mo$hire_job_category),
                               FUN=mean)
pct_attrition_6mo_jobcat

# sort by attrition rate
pct_attrition_6mo_jobcat <- pct_attrition_6mo_jobcat[order(pct_attrition_6mo_jobcat$pct_attrition),]

# plot the percentage 3-month attrition by client
par(mar=c(6,10,3,3))
barplot(height=pct_attrition_6mo_jobcat$pct_attrition*100, names.arg=pct_attrition_6mo_jobcat$job_category, col="light blue",
        main="180-Day Attrition Rate",xlab="Percent",ylab="",horiz=TRUE,las=1)

## Tenure vs Application Device Type ##

# bring in application data
app_attrition_data <- merge(attrition_data, data_exercise_applicants,
                            by="user_id", all.x=TRUE, all.y=FALSE)
summary(is.na(app_attrition_data$device)) # app device info only available for ~450 employees

boxplot(app_attrition_data$tenure_length ~ app_attrition_data$device,las=2)
table(app_attrition_data$device)

# construct broader categories
app_attrition_data$device_type <- NA
app_attrition_data$device_type[app_attrition_data$device=="iPad"] <- "Apple"
app_attrition_data$device_type[app_attrition_data$device=="iPhone"] <- "Apple"
app_attrition_data$device_type[grep("Samsung",app_attrition_data$device)] <- "Android"
app_attrition_data$device_type[grep("LG",app_attrition_data$device)] <- "Android"
app_attrition_data$device_type[grep("HTC",app_attrition_data$device)] <- "Android"
app_attrition_data$device_type[grep("Z9",app_attrition_data$device)] <- "Android"
app_attrition_data$device_type[app_attrition_data$device=="Other"] <- "Other"

table(app_attrition_data$device_type)
boxplot(app_attrition_data$tenure_length ~ app_attrition_data$device_type,outline=FALSE,
        main="Tenure vs Device Type",ylab="Tenure in Days", xlab="Device Type")
# Apple device applicants appear to have longer tenure

# look at 1 year tenure vs device type
app_attrition_data$tenured_1y <- app_attrition_data$tenure_length >= 365

# set 1 year tenure flag to NA if tenure < 1 year and still employed
app_attrition_data$tenured_1y[!app_attrition_data$tenured_1y & 
                                     app_attrition_data$currently_employed] <- NA
summary(app_attrition_data$tenured_1y)

year_tenure_vs_device_type <- table(app_attrition_data$tenured_1y, app_attrition_data$device_type)
year_tenure_vs_device_type

chisq.test(year_tenure_vs_device_type)
# p-value 0.24, not significant

# Try a survival analysis using device type as a factor

# set minimum tenure to 1 day, max to 30 years
app_attrition_data$tenure_length_adj <- app_attrition_data$tenure_length
app_attrition_data$tenure_length_adj[app_attrition_data$tenure_length_adj==0] <- 1
app_attrition_data$tenure_length_adj[app_attrition_data$tenure_length_adj>365*30] <- 365*30

library(survival)
surv.obj <- Surv(type="right",
                        time=app_attrition_data$tenure_length_adj,
                        event=(as.numeric(app_attrition_data$currently_employed))
)

hist(app_attrition_data$tenure_length_adj,breaks=20)
# distribution looks roughly exponential, possibly could get better fit with other but this should work
surv.exp.fit <- survreg(surv.obj~app_attrition_data$device_type, dist="exponential")
summary(surv.exp.fit)
# still insignificant p-values


