set.seed(21)

# Create column of randomly selected race with probabilities based on the percentages in my research article
race <- c('white',sample(c('white','black','hispanic','asian'),size = 9999, replace = TRUE, prob = c(.5441,.2131,.1726,.0702)))
data<- data.frame(race)

# Create column of parental education with probabilities based on the percentages from my research article
parental_education_white <- sample(c('<HS','HS/GED','Some College','College Grad/Professional'),size=(length(data[which(data$race=="white"),])),replace = TRUE,prob = c(.0896,.3252,.2920,.2530))
parental_education_black <- sample(c('<HS','HS/GED','Some College','College Grad/Professional'),size=(length(data[which(data$race=="black"),])),replace = TRUE,prob = c(.1766,.3519,.2451,.1865))
parental_education_hispanic <- sample(c('<HS','HS/GED','Some College','College Grad/Professional'),size=(length(data[which(data$race=="hispanic"),])),replace = TRUE,prob = c(.4190,.2476,.1770,.1159))
parental_education_asian <- sample(c('<HS','HS/GED','Some College','College Grad/Professional'),size=(length(data[which(data$race=="asian"),])),replace = TRUE,prob = c(.1729,.1960,.17,.4107))

# Add column to data frame
data$education[which(data$race =='white')] <- parental_education_white
data$education[which(data$race =='black')] <- parental_education_black
data$education[which(data$race =='hispanic')] <- parental_education_hispanic
data$education[which(data$race =='asian')] <- parental_education_asian
data$education <- as.factor(data$education)

# Create column of incomes for each race with probabilities based on the percentages from my research article
income_level_white <- sample(c('$0-$20K','$20K-$40K','$40K-$60K','$60K-$80K','$80K+'),size=(nrow(data[which(data$race=="white"),])),replace = TRUE,prob = c(.1639,.3050,.2894,.1379,.1037))
income_level_black <- sample(c('$0-$20K','$20K-$40K','$40K-$60K','$60K-$80K','$80K+'),size=(nrow(data[which(data$race=="black"),])),replace = TRUE,prob = c(.4639,.3373,.1163,.0532,.0293))
income_level_hispanic <- sample(c('$0-$20K','$20K-$40K','$40K-$60K','$60K-$80K','$80K+'),size=(nrow(data[which(data$race=="hispanic"),])),replace = TRUE,prob = c(.3817,.3951,.1378,.0502,.0352))
income_level_asian <- sample(c('$0-$20K','$20K-$40K','$40K-$60K','$60K-$80K','$80K+'),size=(nrow(data[which(data$race=="asian"),])),replace = TRUE,prob = c(.1541,.4005,.2372,.1024,.1058))

# Add column to data frame
data$income.level[which(data$race =='white')] <- income_level_white
data$income.level[which(data$race =='black')] <- income_level_black
data$income.level[which(data$race =='hispanic')] <- income_level_hispanic
data$income.level[which(data$race =='asian')] <- income_level_asian
data$income.level <- as.factor(data$income.level)

# Create column for sex with probabilities based on the percentages from my research article
data$sex <- c('male',sample(c('male','female'),size=(nrow(data)-1),replace = TRUE,prob = c(.535,.465)))
data$sex <- as.factor(data$sex)


# Create column for heights, mean and sd determined from:
# https://www.cdc.gov/growthcharts/data/set1clinical/cj41l021.pdf and
# https://www.cdc.gov/growthcharts/data/set1clinical/cj41l022.pdf

male_height_mean <- 1.7526  # meters
male_height_sd <- 0.073406  # meters
male_weight_mean <- 79  # kg
male_weight_sd <- 12 #kg
male_n <- nrow(data[which(data$sex =='male'),])

female_height_mean <- 1.6256  # meters
female_height_sd <- 0.064262  # meters
female_weight_mean <- 67 # kg
female_weight_sd <- 12 #kg
female_n <- nrow(data[which(data$sex =='female'),])

mean_male_bmi <- male_weight_mean / male_height_mean^2 # 23.11
mean_female_bmi <- female_weight_mean / female_height_mean^2 # 22.33

# install.packages('fGarch')
# fGarch package allows for a skewed distribution to be created.
# this was necessary in order to mirror the weight data from the article
library(fGarch)
male_heights <- rnorm(male_n, mean=male_height_mean, sd=male_height_sd)
male_weights <- .6 * mean_male_bmi * (male_heights^2) + .4* (male_weight_mean + rsnorm(male_n, sd=male_weight_sd,xi=-1.5))

female_heights <- rnorm(female_n, mean=female_height_mean, sd=female_height_sd)
female_weights <- .6 * mean_female_bmi * (female_heights^2) + .4* (female_weight_mean + rsnorm(female_n, sd=female_weight_sd,xi=-1.5))

# Add heights and weights to data table
data$height[which(data$sex =="male")] <- male_heights
data$height[which(data$sex =="female")] <- female_heights

data$weight[which(data$sex =="male")] <- male_weights
data$weight[which(data$sex =="female")] <- female_weights

# Calculate BMI
data$bmi <- data$weight / data$height^2

# Create binary column for if person is obese (1) or not (0)
data$obese <- ifelse(data$bmi>=25,1,0)

# Create scatter plots to ensure that relationships between variables are appropriate
plot(data$height,data$weight, main = 'height vs. weight')
plot(data$height,data$bmi, main = 'height vs. bmi')
plot(data$weight, data$bmi, main = 'weight vs. bmi')

library(ggplot2)
library(dplyr)
# plot distributions of BMIs colored by sex,race, and income.level
g <- ggplot(data = data, aes(x=bmi))
g+geom_histogram(aes(fill=sex),position='dodge', bins = 15)
g+geom_histogram(aes(fill=race),position='dodge', bins = 15)
g+geom_histogram(aes(fill=income.level),position='dodge', bins = 15)

gbox <- ggplot(data = data, aes())
gbox+geom_boxplot(aes(x = sex, y = height),col=c('blue','red'))+
  labs(title = 'Height Distributions by Sex')
gbox+geom_boxplot(aes(x = race, y = height),col=c('blue','red','orange','purple'))+
  labs(title = 'Height Distributions by Race')

gbox+geom_boxplot(aes(x = sex, y = weight),col=c('blue','red'))+
  labs(title = 'Weight Distributions by Sex')

gbox+geom_boxplot(aes(x = race, y = weight),col=c('blue','red','orange','purple'))+
  labs(title = 'Weight Distributions by Race')

gbox+geom_boxplot(aes(x = sex, y = bmi),col=c('blue','red'))+
  labs(title = 'BMI Distributions by Sex')
gbox+geom_boxplot(aes(x = race, y = bmi),col=c('blue','red','orange','purple'))+
  labs(title = 'BMI Distributions by Race')
gbox+geom_boxplot(aes(x = income.level, y = bmi))+
  labs(title = 'BMI Distributions by Income Level')

# t-tests for mean values of height, weight, bmi across sex and race
# split data frame into two, one for males and one for females

data_male <- filter(data,sex == 'male')
data_female <- filter(data,sex == 'female')

t.test(data_male$height,data_female$height) 
t.test(data_male$weight,data_female$weight) 
t.test(data_male$bmi,data_female$bmi)

chisq.test(table(data$race, data$obese))
chisq.test(table(data$income.level,data$obese))
chisq.test(table(data$education,data$obese))


# chi-squared tests for race and income level
# split data into tables for each race and each income level

data_white = filter(data,race == 'white')
data_black = filter(data,race == 'black')
data_hispanic = filter(data,race == 'hispanic')
data_asian = filter(data,race == 'asian')

income_white <- data_white %>% 
  group_by(income.level) %>%
  summarise(no_rows = length(income.level))

income_black <- data_black %>% 
  group_by(income.level) %>%
  summarise(no_rows = length(income.level))

income_hispanic <- data_hispanic %>% 
  group_by(income.level) %>%
  summarise(no_rows = length(income.level))

income_asian <- data_asian %>% 
  group_by(income.level) %>%
  summarise(no_rows = length(income.level))

income_levels <- cbind(income_white,income_black$no_rows,income_hispanic$no_rows,income_asian$no_rows)
names(income_levels)[names(income_levels)=="no_rows"] <- "white"
names(income_levels)[names(income_levels)=="income_black$no_rows"] <- "black"
names(income_levels)[names(income_levels)=="income_hispanic$no_rows"] <- "hispanic"
names(income_levels)[names(income_levels)=="income_asian$no_rows"] <- "asian"


income_levels_prop <- data.frame('income.level'=income_levels$income.level)
income_levels_prop$white <- income_levels$white/sum(income_levels$white)*100
income_levels_prop$black <-  income_levels$black/sum(income_levels$black)*100
income_levels_prop$hispanic <-  income_levels$hispanic/sum(income_levels$hispanic)*100
income_levels_prop$asian <-  income_levels$asian/sum(income_levels$asian)*100


# Graph percent of each race that falls in each income level
library(reshape)
income_levels_gg <- melt(income_levels_prop, income_level.vars=c('black', 'hispanic','asian'),var='race')
g2 <- ggplot(income_levels_gg,aes(x=income.level, y = value))
g2 + geom_bar(position='dodge',aes(y = value,fill=race),stat = 'identity')+labs(title = '% of Sample by Race & Income Level',x = 'income level',y='percent of sample')
# Shows that whites and asians have a much higher proportion in the upper income levels

# Chi-squared test on income levels by race to see if differences in each level occured due to chance
chisq.test(select(income_levels_prop,-income.level)) # p = 1.325e-9
# Results show that the differneces seen in proportions of the sample population in each income level
# between races is not likely due to chance.

# Create a data frame with the percentage of individuals, sex, and race
overweight <- filter(data,data$bmi>=25)
male_overweight <- filter(overweight, sex == 'male')
female_overweight <- filter(overweight, sex == 'female')

whm_overweight <- length(male_overweight$race[which(male_overweight$race == 'white')])/length(which(data_white$sex == 'male'))*100
whf_overweight <- length(male_overweight$race[which(female_overweight$race == 'white')])/length(which(data_white$sex == 'female'))*100
blm_overweight <- length(male_overweight$race[which(male_overweight$race == 'black')])/length(which(data_black$sex == 'male'))*100
blf_overweight <- length(male_overweight$race[which(female_overweight$race == 'black')])/length(which(data_black$sex == 'female'))*100
asm_overweight <- length(male_overweight$race[which(male_overweight$race == 'asian')])/length(which(data_asian$sex == 'male'))*100
asf_overweight <- length(male_overweight$race[which(female_overweight$race == 'asian')])/length(which(data_asian$sex == 'female'))*100
him_overweight <- length(male_overweight$race[which(male_overweight$race == 'hispanic')])/length(which(data_hispanic$sex == 'male'))*100
hif_overweight <- length(male_overweight$race[which(female_overweight$race == 'hispanic')])/length(which(data_hispanic$sex == 'female'))*100

overweight_perc = data.frame('sex'=c('male','female'), 'white' = c(whm_overweight,whf_overweight), 'black' = c(blm_overweight,blf_overweight),'hispanic' = c(him_overweight,hif_overweight), 'asian'=c(asm_overweight,asf_overweight))

# Graphs of percentage overweight by race approximating those seen in the article
g3 <- ggplot(overweight_perc,aes(x = sex, y = white))
g3 + geom_bar(stat = 'identity', fill = c('red','blue'))+
    labs(x = 'Sex', y = '% overweight', title = 'Race = White, % Overweight by Sex')+
    geom_text(aes(label=round(white,2)), position=position_dodge(width=0.9), vjust=-0.25)

g4 <- ggplot(overweight_perc,aes(x = sex, y = black))
g4 + geom_bar(stat = 'identity', fill = c('red','blue'))+
    labs(x = 'Sex', y = '% overweight', title = 'Race = Black, % Overweight by Sex')+
    geom_text(aes(label=round(black,2)), position=position_dodge(width=0.9), vjust=-0.25)

g5 <- ggplot(overweight_perc,aes(x = sex, y = hispanic))
g5 + geom_bar(stat = 'identity', fill = c('red','blue'))+
    labs(x = 'Sex', y = '% overweight', title = 'Race = Hispanic, % Overweight by Sex')+
   geom_text(aes(label=round(hispanic,2)), position=position_dodge(width=0.9), vjust=-0.25)

g6 <- ggplot(overweight_perc,aes(x = sex, y = asian))
g6 + geom_bar(stat = 'identity', fill = c('red','blue'))+
    labs(x = 'Sex', y = '% overweight', title = 'Race = Asian, % Overweight by Sex')+
    geom_text(aes(label=round(asian,2)), position=position_dodge(width=0.9), vjust=-0.25)

chisq.test(select(overweight_perc,-sex)) 
# This chisq test shows that the differences in percent overweight are 
# not significantly different across races


ow_transpose <- t(overweight_perc)
colnames(ow_transpose) <- c('male','female')
ow_transpose <- as.data.frame(ow_transpose[2:5,],stringsAsFactors = F)
ow_transpose$male <- as.numeric(ow_transpose$male)
ow_transpose$female <- as.numeric(ow_transpose$female)
ow_transpose$race <- factor(c('white','black','hispanic','asian'))

t.test(ow_transpose$male,ow_transpose$female) 
# This t-test shows that there is a statistically significant difference in the mean percent of people
# overweight across sexes. 

ow_transpose_gg <- melt(ow_transpose, race.vars=c('black', 'hispanic','asian'),var='sex')
g7 <- ggplot(data = ow_transpose_gg, aes(x = race, y = value, group=sex, fill = sex))
g7+geom_bar(aes(x=race,y=value), stat = 'identity', position='dodge')+
  labs(x = 'Race', y = '% overweight', title = '% Overweight by Race and Sex')



# Create data frames of percent overweight by income level and race

zero_twenty <- filter(data, data$income.level == "$0-$20K")
twenty_forty<- filter(data, data$income.level == "$20K-$40K")
forty_sixty<- filter(data, data$income.level == "$40K-$60K")
sixty_eighty<- filter(data, data$income.level == "$60K-$80K")
eighty_plus<- filter(data, data$income.level == "80K+")


ow_zt <- filter(zero_twenty, zero_twenty$bmi >= 25)
ow_tf <- filter(twenty_forty, twenty_forty$bmi >= 25)
ow_fs <- filter(forty_sixty, forty_sixty$bmi >= 25)
ow_se <- filter(sixty_eighty, sixty_eighty$bmi >= 25)
ow_ep <- filter(eighty_plus, eighty_plus$bmi >= 25)

library(sqldf)
# Create data frames for each race and each income level

white_ow_zt <- sqldf('SELECT * from overweight WHERE race = "white" AND `income.level`=="$0-$20K"')
black_ow_zt <- sqldf('SELECT * from overweight WHERE race = "black" AND `income.level`=="$0-$20K"')
hispanic_ow_zt <- sqldf('SELECT * from overweight WHERE race = "hispanic" AND `income.level`=="$0-$20K"')
asian_ow_zt <- sqldf('SELECT * from overweight WHERE race = "asian" AND `income.level`=="$0-$20K"')

white_ow_tf <- sqldf('SELECT * from overweight WHERE race = "white" AND `income.level`=="$20K-$40K"')
black_ow_tf <- sqldf('SELECT * from overweight WHERE race = "black" AND `income.level`=="$20K-$40K"')
hispanic_ow_tf <- sqldf('SELECT * from overweight WHERE race = "hispanic" AND `income.level`=="$20K-$40K"')
asian_ow_tf <- sqldf('SELECT * from overweight WHERE race = "asian" AND `income.level`=="$20K-$40K"')

white_ow_fs <- sqldf('SELECT * from overweight WHERE race = "white" AND `income.level`=="$40K-$60K"')
black_ow_fs <- sqldf('SELECT * from overweight WHERE race = "black" AND `income.level`=="$40K-$60K"')
hispanic_ow_fs <- sqldf('SELECT * from overweight WHERE race = "hispanic" AND `income.level`=="$40K-$60K"')
asian_ow_fs <- sqldf('SELECT * from overweight WHERE race = "asian" AND `income.level`=="$40K-$60K"')

white_ow_se <- sqldf('SELECT * from overweight WHERE race = "white" AND `income.level`=="$60K-$80K"')
black_ow_se <- sqldf('SELECT * from overweight WHERE race = "black" AND `income.level`=="$60K-$80K"')
hispanic_ow_se <- sqldf('SELECT * from overweight WHERE race = "hispanic" AND `income.level`=="$60K-$80K"')
asian_ow_se <- sqldf('SELECT * from overweight WHERE race = "asian" AND `income.level`=="$60K-$80K"')

white_ow_ep <- sqldf('SELECT * from overweight WHERE race = "white" AND `income.level`=="$80K+"')
black_ow_ep <- sqldf('SELECT * from overweight WHERE race = "black" AND `income.level`=="$80K+"')
hispanic_ow_ep <- sqldf('SELECT * from overweight WHERE race = "hispanic" AND `income.level`=="$80K+"')
asian_ow_ep <- sqldf('SELECT * from overweight WHERE race = "asian" AND `income.level`=="$80K+"')

# Calculate percent of each income level & race that are obese

white_zt_ow_perc <- (nrow(white_ow_ep)/nrow(sqldf('SELECT * FROM data WHERE race = "white" AND `income.level`=="$0-$20K"')))*100
black_zt_ow_perc <- (nrow(black_ow_ep)/nrow(sqldf('SELECT * FROM data WHERE race = "black" AND `income.level`=="$0-$20K"')))*100
hispanic_zt_ow_perc <- (nrow(hispanic_ow_ep)/nrow(sqldf('SELECT * FROM data WHERE race = "hispanic" AND `income.level`=="$0-$20K"')))*100
asian_zt_ow_perc <- (nrow(asian_ow_ep)/nrow(sqldf('SELECT * FROM data WHERE race = "asian" AND `income.level`=="$0-$20K"')))*100

white_tf_ow_perc <- (nrow(white_ow_ep)/nrow(sqldf('SELECT * FROM data WHERE race = "white" AND `income.level`=="$20K-$40K"')))*100
black_tf_ow_perc <- (nrow(black_ow_ep)/nrow(sqldf('SELECT * FROM data WHERE race = "black" AND `income.level`=="$20K-$40K"')))*100
hispanic_tf_ow_perc <- (nrow(hispanic_ow_ep)/nrow(sqldf('SELECT * FROM data WHERE race = "hispanic" AND `income.level`=="$20K-$40K"')))*100
asian_tf_ow_perc <- (nrow(asian_ow_ep)/nrow(sqldf('SELECT * FROM data WHERE race = "asian" AND `income.level`=="$20K-$40K"')))*100

white_fs_ow_perc <- (nrow(white_ow_ep)/nrow(sqldf('SELECT * FROM data WHERE race = "white" AND `income.level`=="$40K-$60K"')))*100
black_fs_ow_perc <- (nrow(black_ow_ep)/nrow(sqldf('SELECT * FROM data WHERE race = "black" AND `income.level`=="$40K-$60K"')))*100
hispanic_fs_ow_perc <- (nrow(hispanic_ow_ep)/nrow(sqldf('SELECT * FROM data WHERE race = "hispanic" AND `income.level`=="$40K-$60K"')))*100
asian_fs_ow_perc <- (nrow(asian_ow_ep)/nrow(sqldf('SELECT * FROM data WHERE race = "asian" AND `income.level`=="$40K-$60K"')))*100

white_se_ow_perc <- (nrow(white_ow_ep)/nrow(sqldf('SELECT * FROM data WHERE race = "white" AND `income.level`=="$60K-$80K"')))*100
black_se_ow_perc <- (nrow(black_ow_ep)/nrow(sqldf('SELECT * FROM data WHERE race = "black" AND `income.level`=="$60K-$80K"')))*100
hispanic_se_ow_perc <- (nrow(hispanic_ow_ep)/nrow(sqldf('SELECT * FROM data WHERE race = "hispanic" AND `income.level`=="$60K-$80K"')))*100
asian_se_ow_perc <- (nrow(asian_ow_ep)/nrow(sqldf('SELECT * FROM data WHERE race = "asian" AND `income.level`=="$60K-$80K"')))*100

white_ep_ow_perc <- (nrow(white_ow_ep)/nrow(sqldf('SELECT * FROM data WHERE race = "white" AND `income.level`=="$80K+"')))*100
black_ep_ow_perc <- (nrow(black_ow_ep)/nrow(sqldf('SELECT * FROM data WHERE race = "black" AND `income.level`=="$80K+"')))*100
hispanic_ep_ow_perc <- (nrow(hispanic_ow_ep)/nrow(sqldf('SELECT * FROM data WHERE race = "hispanic" AND `income.level`=="$80K+"')))*100
asian_ep_ow_perc <- (nrow(asian_ow_ep)/nrow(sqldf('SELECT * FROM data WHERE race = "asian" AND `income.level`=="$80K+"')))*100

perc_ow_race_income <- data.frame("white" = c(white_zt_ow_perc,white_tf_ow_perc,white_fs_ow_perc,white_se_ow_perc,white_ep_ow_perc), "black" = c(black_zt_ow_perc,black_tf_ow_perc,black_fs_ow_perc,black_se_ow_perc,black_ep_ow_perc), "hispanic" = c(hispanic_zt_ow_perc,hispanic_tf_ow_perc,hispanic_fs_ow_perc,hispanic_se_ow_perc,hispanic_ep_ow_perc), "asian" = c(asian_lhs_ow_perc,asian_tf_ow_perc,asian_fs_ow_perc,asian_se_ow_perc,asian_ep_ow_perc), row.names = c("$0-$20K","$20K-$40K","$40K-$60K","$60K-$80K","$80K+"))
perc_ow_race_income_tran <- as.data.frame(t(perc_ow_race_income))
perc_ow_race_income_tran$race <- c('white','black','hispanic','asian')

perc_ow_race_income_gg <- income_levels_gg <- melt(perc_ow_race_income, income_level.vars=c('black', 'hispanic','asian'),var='race')
perc_ow_race_income_gg$income.level <- c("$0-$20K","$20K-$40K","$40K-$60K","$60K-$80K","$80K+")
perc_ow_race_income_gg$value <- ifelse(perc_ow_race_income_gg$value==Inf,0,perc_ow_race_income_gg$value)

g8 <- ggplot(data = perc_ow_race_income_gg, aes(group = race, fill= race))
g8 + geom_bar(aes(x = income.level, y = value), stat = 'identity', position = 'dodge')+
  labs(x = 'Income', y = '% of individuals who are obese', title = '% Obese by Income and Race')

# Education Levels

white_lhs_ow_perc <- (nrow(white_ow_ep)/nrow(sqldf('SELECT * FROM data WHERE race = "white" AND education=="<HS"')))*100
black_lhs_ow_perc <- (nrow(black_ow_ep)/nrow(sqldf('SELECT * FROM data WHERE race = "black" AND education=="<HS"')))*100
hispanic_lhs_ow_perc <- (nrow(hispanic_ow_ep)/nrow(sqldf('SELECT * FROM data WHERE race = "hispanic" AND education=="<HS"')))*100
asian_lhs_ow_perc <- (nrow(asian_ow_ep)/nrow(sqldf('SELECT * FROM data WHERE race = "asian" AND education=="<HS"')))*100

white_ged_ow_perc <- (nrow(white_ow_ep)/nrow(sqldf('SELECT * FROM data WHERE race = "white" AND education=="HS/GED"')))*100
black_ged_ow_perc <- (nrow(black_ow_ep)/nrow(sqldf('SELECT * FROM data WHERE race = "black" AND education=="HS/GED"')))*100
hispanic_ged_ow_perc <- (nrow(hispanic_ow_ep)/nrow(sqldf('SELECT * FROM data WHERE race = "hispanic" AND education=="HS/GED"')))*100
asian_ged_ow_perc <- (nrow(asian_ow_ep)/nrow(sqldf('SELECT * FROM data WHERE race = "asian" AND education=="HS/GED"')))*100

white_sc_ow_perc <- (nrow(white_ow_ep)/nrow(sqldf('SELECT * FROM data WHERE race = "white" AND education=="Some College"')))*100
black_sc_ow_perc <- (nrow(black_ow_ep)/nrow(sqldf('SELECT * FROM data WHERE race = "black" AND education=="Some College"')))*100
hispanic_sc_ow_perc <- (nrow(hispanic_ow_ep)/nrow(sqldf('SELECT * FROM data WHERE race = "hispanic" AND education=="Some College"')))*100
asian_sc_ow_perc <- (nrow(asian_ow_ep)/nrow(sqldf('SELECT * FROM data WHERE race = "asian" AND education=="Some College"')))*100

white_cp_ow_perc <- (nrow(white_ow_ep)/nrow(sqldf('SELECT * FROM data WHERE race = "white" AND education=="College Grad/Professional"')))*100
black_cp_ow_perc <- (nrow(black_ow_ep)/nrow(sqldf('SELECT * FROM data WHERE race = "black" AND education=="College Grad/Professional"')))*100
hispanic_cp_ow_perc <- (nrow(hispanic_ow_ep)/nrow(sqldf('SELECT * FROM data WHERE race = "hispanic" AND education=="College Grad/Professional"')))*100
asian_cp_ow_perc <- (nrow(asian_ow_ep)/nrow(sqldf('SELECT * FROM data WHERE race = "asian" AND education=="College Grad/Professional"')))*100

perc_ow_race_education <- data.frame("white" = c(white_lhs_ow_perc,white_ged_ow_perc,white_sc_ow_perc,white_cp_ow_perc), "black" = c(black_lhs_ow_perc,black_ged_ow_perc,black_sc_ow_perc,black_cp_ow_perc), "hispanic" = c(hispanic_lhs_ow_perc,hispanic_ged_ow_perc,hispanic_sc_ow_perc,hispanic_cp_ow_perc), "asian" = c(asian_lhs_ow_perc,asian_ged_ow_perc,asian_sc_ow_perc,asian_cp_ow_perc), row.names = c("<HS","HS/GED","Some College","College Grad/Professional"))
perc_ow_race_education_tran <- as.data.frame(t(perc_ow_race_education))
perc_ow_race_education_tran$race <- c('white','black','hispanic','asian')

perc_ow_race_education_gg <- income_levels_gg <- melt(perc_ow_race_education, income_level.vars=c('black', 'hispanic','asian'),var='race')
perc_ow_race_education_gg$education <- c("<HS","HS/GED","Some College","College Grad/Professional")
perc_ow_race_education_gg$education <- factor(perc_ow_race_education_gg$education, levels=c("<HS","HS/GED","Some College","College Grad/Professional"))
perc_ow_race_education_gg$value <- ifelse(perc_ow_race_education_gg$value==Inf,0,perc_ow_race_education_gg$value)

g9 <- ggplot(data = perc_ow_race_education_gg, aes(group = race, fill= race))
g9 + geom_bar(aes(x = education, y = value), stat = 'identity', position = 'dodge')+
  labs(x = 'Income', y = '% of individuals who are obese', title = '% Obese by Education and Race')

chisq.test((perc_ow_race_income))
# Logistic Regression Models for Each Variable to Predict Obesity
glm_all <- glm(obese~.-height-weight, data = data, family = binomial)
summary(glm_all)

glm_race <- glm(obese~race,data = data,family = binomial)
summary(glm_race)


glm_income <- glm(obese~income.level,data = data,family = binomial)
summary(glm_income)

glm_education <- glm(obese~education,data = data, family = binomial)
summary(glm_education)

lm_all <- lm(bmi~.-obese-height-weight,data = data)
summary(lm_all)
plot(lm_all)




