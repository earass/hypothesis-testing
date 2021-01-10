library('glue')
library("ggplot2")
library(dplyr)
library(reshape2)

data <- read.csv('CityPayrollDataset.csv')
data[] <- lapply(data, gsub, pattern='\\$', replacement='')
head(data)

get_t_test <- function(mean_samp, mean_pop, std_samp, sample_size){
  return((mean_samp - mean_pop)/(std_samp/sqrt(sample_size)))
}

############################################################
# Do the employees working as Police Officer-II have a better chance of getting Temporary Bonus Pay?

"Null Hypothesis: Police Officer II have better chance of getting temporary bonus pay"
"Alternate Hypothesis: Police Officer II do not have a better chance of getting temporary bonus pay"

data$Temporary.Bonus.Pay <- as.numeric(data$Temporary.Bonus.Pay)
data['is_temp_bonus_pay'] <- 0
data[data$Temporary.Bonus.Pay != 0, 'is_temp_bonus_pay'] <- 1

freq <- data.frame(table(data$is_temp_bonus_pay))
p <- ggplot(data=freq, aes(x=Var1, y=Freq)) + geom_bar(stat="identity")
p

police_2_cond <- data$Job.Class.Title == 'Police Officer II'
data_pol_2 <- data[police_2_cond, 'is_temp_bonus_pay']

mean_pol_2 <- mean(data_pol_2)
glue("Rate of Police Officer II getting temp bonus pay: {mean_pol_2}")

sample_size = 1000
samp_pol_2 <- sample(data_pol_2, size = sample_size)

mean_samp_pol_2 <- mean(samp_pol_2)
glue("Rate of sample Police Officer II getting temp bonus pay: {mean_samp_pol_2}")

std_samp_pol_2 <- sd(samp_pol_2)
glue("Sample Standard deviation: {std_samp_pol_2}")
z <- 1.645
t_test <- get_t_test(mean_samp_pol_2, mean_pol_2, std_samp_pol_2, sample_size)
glue("T test value: {t_test}")
if (t_test > -z){
  "Null Hypothesis is not rejected"
}else{
  "Null Hypothesis is rejected"
}



#######################################################################
# Employees who get Permanent Bonus Pay are most likely to be from Public Works-Sanitation Department?
"Null Hypothesis: PWSD employees are more likely to get Permanent bonus pay"
"Alternate Hypothesis: PWSD employees are not most likely to get Permanent bonus pay"
data$Permanent.Bonus.Pay <- as.numeric(data$Permanent.Bonus.Pay)

per_pay_data <- data[data$Permanent.Bonus.Pay != 0, ]

pwsd <- per_pay_data$Department.Title == 'Public Works - Sanitation'

per_pay_data['is_pwsd'] <- 0
per_pay_data[pwsd, 'is_pwsd'] <- 1
freq <- data.frame(table(per_pay_data$is_pwsd))
p <- ggplot(data=freq, aes(x=Var1, y=Freq)) + geom_bar(stat="identity")
p


data_pwsd_pay <- per_pay_data$is_pwsd
mean_pwsd_pop <- mean(data_pwsd_pay)
glue("Rate of Employees who are from Public Works - Sanitation department getting permanent bonus pay: {mean_pwsd_pop}")


sample_size = 1000
samp_pwsd <- sample(data_pwsd_pay, size = sample_size)
mean_pwsd_samp <- mean(samp_pwsd)
glue("Sample Rate of Employees who are from Public Works - Sanitation department getting permanent bonus pay: {mean_pwsd_samp}")
glue("Sample Rate of Employees who are from not Public Works - Sanitation department getting permanent bonus pay: {1 - mean_pwsd_samp}")


std_samp_pwsd <- sd(samp_pwsd)
std_samp_pwsd
z <- 1.645
t_test <- get_t_test(mean_pwsd_samp, mean_pwsd_pop, std_samp_pwsd, sample_size)
glue("T test value: {t_test}")
if (t_test > -z){
  "Null Hypothesis is rejected"
}else{
  "Null Hypothesis is not rejected"
}




############################################################################
# Do the employees working in Water and Power (DWP) Department have a better chance of being employed overtime?
"Null Hypothesis: employees in Water and Power (DWP) Department have a better chance of being employed overtime"
"Alternate Hypothesis: employees in Water and Power (DWP) Department do not have any better chance of being employed overtime"
data$Overtime.Pay <- as.numeric(data$Overtime.Pay)


overtime_pay_data <- data[data$Overtime.Pay != 0, ]
overtime_pay_data <- overtime_pay_data[!is.na(overtime_pay_data$Overtime.Pay), ]


dwp <- overtime_pay_data$Department.Title == 'Water And Power (DWP)'

overtime_pay_data['is_dwp'] <- 0
overtime_pay_data[dwp, 'is_dwp'] <- 1
overtime_pay_data['not_dwp'] = 1 - overtime_pay_data['is_dwp']
freq <- data.frame(table(overtime_pay_data$is_dwp))
p <- ggplot(data=freq, aes(x=Var1, y=Freq)) + geom_bar(stat="identity")
p

mean_dwp <- mean(overtime_pay_data$is_dwp)
glue("Rate of Employees who are from Water and Power (DWP) Department have a better chance of being employed overtime: {mean_dwp}")
glue("Rate of Employees who are not from Water and Power (DWP) Department have a better chance of being employed overtime: {1 - mean_dwp}")


sample_size = 1000
samp_not_dwp <- sample(overtime_pay_data$not_dwp, size = sample_size)
mean_not_dwp_samp <- mean(samp_not_dwp)
glue("Sample Rate of Employees who are not from Water and Power (DWP) Department have a better chance of being employed overtime: {mean_not_dwp_samp}")

std_not_dwp_samp = sd(samp_not_dwp)
glue("Stdev of sample: {std_not_dwp_samp}")
z <- 1.645
t_test <- get_t_test(mean_not_dwp_samp, mean_dwp, std_not_dwp_samp, sample_size)
glue("T test value: {t_test}")
if (t_test > -z){
  "Null Hypothesis is rejected"
}else{
  "Null Hypothesis is not rejected"
}



###########################################################################################
# Do the employees who work Part Time instead of Full Time have a better chance to be from the Airports (LAWA) Department?
"Null Hypothesis: Part time employees are more likely to be from LAWA dept than full time employees"
"Alternate Hypothesis: Part time employees are not likely to be from LAWA dept than full time employees"
temp_df <- subset(data, select=c('Department.Title', 'Employment.Type'))

airport_cond <- temp_df$Department.Title == 'Airports (LAWA)'

full_time_cond <- temp_df$Employment.Type == 'Full Time' & airport_cond
part_time_cond <- temp_df$Employment.Type == 'Part Time' & airport_cond

temp_df['is_full_time'] <- 0
temp_df[full_time_cond, 'is_full_time'] <- 1


temp_df['is_part_time'] <- 0
temp_df[part_time_cond, 'is_part_time'] <- 1

freq <- data.frame(table(temp_df$is_full_time))
p <- ggplot(data=freq, aes(x=Var1, y=Freq)) + geom_bar(stat="identity")
p

mean_part_time <- mean(temp_df$is_part_time)
glue('Rate of Part time employees in LAWA dept: {mean_part_time}')


sample_size = 1000
samp_full_time <- sample(temp_df$is_full_time, size = sample_size)
mean_full_time_samp <- mean(samp_full_time)
glue("Sample Rate of Full time employees in LAWA dept: {mean_full_time_samp}")

std_full_time_samp = sd(samp_full_time)
glue("Stdev of sample: {std_full_time_samp}")
z <- 1.645
t_test <- get_t_test(mean_full_time_samp, mean_part_time, std_full_time_samp, sample_size)
glue("T test value: {t_test}")
if (t_test > -z){
  "Null Hypothesis is rejected"
}else{
  "Null Hypothesis is not rejected"
}




################################################################################
# After 2013, Police (LAPD) Department has experienced the highest pay raise as compared to other departments?
"Null Hypothesis: After 2013, Police (LAPD) Department has experienced the highest pay raise as compared to other departments"
"Alternate Hypothesis: After 2013, Police (LAPD) Department has not experienced the highest pay raise as compared to other departments"
data$Year <- as.numeric(data$Year)
data$Base.Pay <- as.numeric(data$Base.Pay)

after_2013 <- data[data$Year > 2013, ]
lapd_cond <- after_2013$Department.Title == 'Police (LAPD)'

gr_lapd <- data.frame(data[lapd_cond, ] %>% group_by(Year) %>% 
                      summarise(avg_pay_lapd = mean(Base.Pay, na.rm = T)))

gr_not_lapd <- data.frame(data[!lapd_cond, ] %>% group_by(Year) %>% 
                        summarise(avg_pay_not_lapd = mean(Base.Pay, na.rm = T)))

merged <- inner_join(x=gr_lapd,y=gr_not_lapd,by='Year')
merged <- melt(merged, 'Year')

p <- ggplot(merged, aes(x=Year, y=value, color=variable)) + geom_line(size = 3)
p

avg_pay_lapd <- mean(after_2013[lapd_cond, 'Base.Pay'])
glue("Average pay for LAPD employess after 2013: {avg_pay_lapd}")

pay_not_lapd <- after_2013[!lapd_cond, 'Base.Pay']

sample_size = 1000
samp_not_lapd_pay <- sample(pay_not_lapd, size = sample_size)
mean_not_lapd_pay_samp <- mean(samp_not_lapd_pay)
glue("Sample Mean of pay of not LAPD dept: {mean_not_lapd_pay_samp}")

std_not_lapd_pay_samp = sd(samp_not_lapd_pay)
glue("Stdev of sample: {std_not_lapd_pay_samp}")
z <- 1.645
t_test <- get_t_test(mean_not_lapd_pay_samp, avg_pay_lapd, std_not_lapd_pay_samp, sample_size)
glue("T test value: {t_test}")
if (t_test > z){
  "Null Hypothesis is rejected"
}else{
  "Null Hypothesis is not rejected"
}



#################################################################################
# Is there any relationship between Department and Employment type?
"Null Hypothesis: There is a relationship between Department and Employment type"
"Alternate Hypothesis: There is no relationship between Department and Employment type"

# creating marginal distribution
marg_dist <- data.frame(prop.table(table(data$Department.Title))*100)
names(marg_dist)[names(marg_dist) == 'Var1'] <- 'Department.Title'
names(marg_dist)[names(marg_dist) == 'Freq'] <- 'MarginalDistribution'
marg_dist


# creating contingency table with percents
pt <- as.data.frame.matrix(prop.table(
  table(data$Department.Title, data$Employment.Type), margin = 1)*100)
pt['Department.Title'] <- rownames(pt)
pt

# joining marginal dist with contingency table
merged_df1 <- merge(pt,marg_dist,by='Department.Title')
rownames(merged_df1) <- merged_df1[['Department.Title']]
merged_df1['Department.Title'] <- NULL
merged_df1


# checking if values match with marginal distribution
matches = 0
df_cols <- names(merged_df1)
for (var in df_cols){
  if (var != 'MarginalDistribution'){
    match <- sum(merged_df1$MarginalDistribution == merged_df1[[var]])
    matches = matches + match
  }
}
if (matches > 0){
  print(glue("Department and Employement Type have no or weak relationship"))
} else {
  print(glue("Department and Employement Type have some relationship"))
}


##############################################################################
# Potential future employees heard a rumor that Average Base Pay of employees in General Services Department is more than $20,746 but they are not sure about this. 

"Null Hypothesis: General Services Department has an average pay of more than $20,746"
"Alternate Hypothesis: General Services Department has an average pay of less than or equal $20,746"

mean_pay_pop <- 20746
gs_dept_pay <- data[data$Department.Title == 'General Services', 'Base.Pay']


sample_size = 1000
samp_gs_dept_pay <- sample(gs_dept_pay, size = sample_size)
mean_samp_gs_dept_pay <- mean(samp_gs_dept_pay)
glue("Sample Mean of pay GS dept: {mean_samp_gs_dept_pay}")

std_samp_gs_dept_pay = sd(samp_gs_dept_pay)
glue("Stdev of sample: {std_samp_gs_dept_pay}")
z <- 1.645
t_test <- get_t_test(mean_samp_gs_dept_pay, mean_pay_pop, std_samp_gs_dept_pay, sample_size)
glue("T test value: {t_test}")
if (t_test < -z){
  "Null Hypothesis is rejected"
}else{
  "Null Hypothesis is not rejected"
}



#####################################################################################
# It has been established that Water and Power (DWP) Department pays on average more Benefit Cost to its employees than all other departments.

"Null Hypothesis: Water and Power (DWP) Department pays on average more Benefit Cost to its employees than all other departments"
"ALternate Hypothesis: Water and Power (DWP) Department pays on average less Benefit Cost to its employees than all other departments"
data$Average.Benefit.Cost <- as.numeric(data$Average.Benefit.Cost)

dwp_cond <- data$Department.Title == 'Water And Power (DWP)'


mean_dwp_pop <- mean(data[dwp_cond, 'Average.Benefit.Cost'])
glue("Mean benefit cost by DWP department: {mean_dwp_pop}")

mean_not_dwp_pop <- mean(data[!dwp_cond, 'Average.Benefit.Cost'])
glue("Mean benefit cost by other departments: {mean_not_dwp_pop}")

d <- data.frame(BenefitCost=c(mean_dwp_pop, Other=mean_not_dwp_pop), Department=c('DWP', 'Other'))
p <- ggplot(data=d, aes(x=Department, y=BenefitCost)) + geom_bar(stat="identity")
p


dwp_cost <- data[dwp_cond, 'Average.Benefit.Cost']

sample_size = 1000
samp_dwp_cost <- sample(dwp_cost, size = sample_size)
mean_samp_dwp_cost <- mean(samp_dwp_cost)
glue("Sample Mean of Benefit cost of DWP Department: {mean_samp_dwp_cost}")

std_samp_dwp_cost = sd(samp_dwp_cost)
glue("Stdev of sample: {std_samp_dwp_cost}")
z <- 1.645
t_test <- get_t_test(mean_samp_dwp_cost, mean_not_dwp_pop, std_samp_dwp_cost, sample_size)
glue("T test value: {t_test}")
if (t_test < -z){
  "Null Hypothesis is rejected"
}else{
  "Null Hypothesis is not rejected"
}



#################################################################################
# In 2014, employees of Recreation and Parks Department were complaining that they have been denied the Longevity Bonus Pay.

"Null Hypothesis: In 2014, employees of Recreation and Parks Department have been denied the Longevity Bonus Pay"
"Alternate Hypothesis: In 2014, employees of Recreation and Parks Department have not been denied the Longevity Bonus Pay"
data$Longevity.Bonus.Pay <- as.numeric(data$Longevity.Bonus.Pay)

filter_df <- data[data$Year == 2014 & data$Department.Title == 'Recreation And Parks', ]

long_bonus_cond <- filter_df$Longevity.Bonus.Pay != 0

filter_df['is_long_bonus'] <- 0
filter_df[long_bonus_cond, 'is_long_bonus'] = 1

rate_long_bonus_pop <- mean(filter_df$is_long_bonus)
glue("Rate of employees got Longevity bonus: {rate_long_bonus_pop}")

sample_size = 1000
samp_long_bonus <- sample(filter_df$is_long_bonus, size = sample_size)
mean_samp_long_bonus <- mean(samp_long_bonus)
glue("Sample rate of employees getting Longevity bonus: {mean_samp_long_bonus}")

std_samp_long_bonus = sd(samp_long_bonus)
glue("Stdev of sample: {std_samp_long_bonus}")
z <- 1.645
t_test <- get_t_test(mean_samp_long_bonus, rate_long_bonus_pop, std_samp_long_bonus, sample_size)
glue("T test value: {t_test}")
if (t_test < -z){
  "Null Hypothesis is not rejected"
}else{
  "Null Hypothesis is rejected"
}



###################################################################################
# Senior Clerk Typist from Harbor (Port of LA) Department has been telling Senior Clerk Typist of Water and Power (DWP) Department that they have more Average Health Cost than them.

"Null Hypothesis: Senior Clerk Typist from Harbor (Port of LA) Department have more Average Health Cost than Senior Clerk Typist of Water and Power (DWP) Department"
"Alternate Hypothesis: Senior Clerk Typist from Harbor (Port of LA) Department have less Average Health Cost than Senior Clerk Typist of Water and Power (DWP) Department"

data$Average.Health.Cost <- as.numeric(data$Average.Health.Cost)

clerk_filter <- data$Job.Class.Title == 'Senior Clerk Typist'

la_dept <- data[data$Department.Title == 'Harbor (Port of LA)' & clerk_filter, 'Average.Health.Cost']
dwp_dept <- data[data$Department.Title == 'Water And Power (DWP)' & clerk_filter, 'Average.Health.Cost']

mean_la_dept <- mean(la_dept)
glue("Mean Harbor (Port of LA) Health Cost of Senior Clerk Typist: {mean_la_dept}")
mean_dwp_dept_pop <- mean(dwp_dept)
glue("Mean DWP Health Cost of Senior Clerk Typist: {mean_dwp_dept}")

d <- data.frame(HealthBenefitCost=c(mean_la_dept, Other=mean_dwp_dept_pop), Department=c('LA', 'DWP'))
p <- ggplot(data=d, aes(x=Department, y=HealthBenefitCost)) + geom_bar(stat="identity")
p

sample_size = 50
samp_la_dept_cost <- sample(la_dept, size = sample_size)
mean_samp_la_dept_cost <- mean(samp_la_dept_cost)
glue("Sample mean of Harbor (Port of LA) department: {mean_samp_la_dept_cost}")

std_samp_la_dept_cost = sd(samp_la_dept_cost)
glue("Stdev of sample: {std_samp_la_dept_cost}")
z <- 1.676
t_test <- get_t_test(mean_samp_la_dept_cost, mean_dwp_dept_pop, std_samp_la_dept_cost, sample_size)
glue("T test value: {t_test}")
if (t_test > z){
  "Null Hypothesis is not rejected"
}else{
  "Null Hypothesis is rejected"
}

