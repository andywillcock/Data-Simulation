## What condition or relationships are you investigating?

I will be investigating the relationship(s) between race, family income, and parental educational achievement and their effect on BMI in adolescents.

## Dataset Dimensions
rows = 13,113
columns = 8 (height, weight, bmi, sex, parental education, race, income, income.level)

## Predictor Variables
height = numeric (inches) - normal distribution
weight = numeric (pounds)
bmi = numeric
sex = factor (levels = female, male)
parental education = factor (levels = <HS, HS/GED, Some College, College_Grad/Professional)
race = factor (levels = white, black, hispanic, asian)
income = numeric (dollars)
income.level = factor (levels = 0-20, 20-40, 40-60, 60-80, 80+)

## Correlated Variables

Height, weight, and BMI are all correlated
Sex is correlated with height and weight
Income is correlated with parental education

Some confounding variables may include: age, living location, and parental work hours

## Response Variable

Number/Percentage of adolescents overweight (bmi>25), numeric.
This response should be close to normally distributed across the entire population

## Data Simulation Steps

Create distributions of the predictor variables based on the information 	found in the journal article
	
	1. Create correlated variables that are properly matched. For example, 
	taller people’s range of weights should be greater.
		
	2. Give each row a value for income using a normal distribution with the mean 
	found in the article.
		
	3. Create columns for race, sex and parental education level using sample() 
	with probabilities weighted based on the %s from the article population make 
	up.

	4. Calculate BMI using height and weight, and store this in a new column. 

## Data Analyzation	

	1. Perform t-tests or chi-square tests for means of BMI across sex, race, 
	education and income.

	2. Fit a main effects and full linear model on the full set of data and 		grouped by race, sex, parental education, and income level.

	3. Plot distributions of each predictor variable.

	4. Plot relationships between race and percentage of adolescents overweight
		a. Stratified by the other predictor variables

	5. Plot relationships of income and percentage of adolescents overweight
		a. Stratified by the other predictor variables
