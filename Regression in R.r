
#install.packages("car")
library(car)
#install.packages("corrplot")
library(corrplot)
#install.packages("MASS")
library(MASS)
#install.packages("ggplot2")
library(ggplot2)


kc.house = read.csv("kc_house_data.csv") # Read in the dataset from a .csv.
df <- data.frame(kc.house) # Put the data into a data frame.

summary(df) # View a 5 number summary the dataset. 

df$date <- substr(df$date,1 ,8) # Remove the time part of the time stamp because it is blank.

# Next, convert the date col into a data type (POSIXct) usable by R Libraries. 
df$date = strptime(df$date, format="%Y%m%d")
df$date = as.POSIXct(df$date)

df$date[1] # Display the newly formmated data column. 

df <- df[rowSums(is.na(df)) == 0,] # Clean out any null rows in the data frame.
key <- df[,1] # Save 1st column which the primary key to a new vector in case its needed later. 
df <- df[-c(1)] # Drop the primary key from dataframe for analysis. 

df.numeric <- df[,-1] # Create dataframe with only numeric values. This is necessary because to make a correlation plot
# you need only numeric values.

corrplot(cor(df.numeric), method = "shade") # Make a corraltion plot. 

df <- df[,-c(12)] # drop the 'sqft_above' col because it has a correlation value of 1 wiht 'sqrt_living'.

df.check.multi.co <- lm(formula = price ~ . ,data = df) # Function call to create a regression model with all 
# predictors. This is necessary to check VIF scores. 

round(vif(df.check.multi.co),2) 

df$bedrooms <- as.factor(df$bedrooms)
df$bathrooms <- as.factor(df$bathrooms)
df$floors <- as.factor(df$floors)
df$view <- as.factor(df$view)
df$waterfront <- as.factor(df$waterfront)
df$condition <- as.factor(df$condition)
df$grade <- as.factor(df$grade)
df$yr_built <- as.factor(df$yr_built)
df$yr_renovated <- as.factor(df$yr_renovated)
df$zipcode <- as.factor(df$zipcode)

ggplot(df, aes(x=price)) + geom_histogram(bins=100, color="black", fill="steelblue") + 
ggtitle("untransformed distribution of KC housing price") + 
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust=".5"))

df$price <- log2(df$price) # take the natrual log on price.

ggplot(df, aes(x=price)) + geom_histogram(bins=100, color="black", fill="steelblue") + 
ggtitle("ln of KC housing price") + 
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust=".5"))

set.seed(123)
# Here I chose a simple 70% 30% dataset split to prevent overfitting. My regression model will be trained on 70% of the
# dataset and the other 30% of the data will be used to validate the model at the end.

partition <- sample(2, nrow(df), replace=TRUE, prob = c(0.70, 0.30))   
train <- df[partition==1 ,] 
test <- df[partition==2 ,]

lm.train <- lm(formula = price ~ . ,data = train)
summary(lm.train)

# Stepwise regression model
step.model <- stepAIC(lm.train, direction = "both", trace = FALSE)
summary(step.model) # Display the model that resutls from the step-wise selection. 

df <- df[-c(11,13,14)] # remove columns grade, yr_renovated, yr_built
summary(df) # Summary of the new dataframe.

set.seed(123)
# Here I chose a simple 70% 30% dataset split to prevent overfitting. My regression model will be trained on 70% of the
# dataset and the other 30% of the data will be used to validate the model at the end.

partition <- sample(2, nrow(df), replace=TRUE, prob = c(0.70, 0.30))   
train <- df[partition==1 ,]
test <- df[partition==2 ,]

lm.train <- lm(formula = price ~ sqft_living + sqft_lot +
    floors + floors*sqft_living + waterfront + view + condition + sqft_basement 
               + zipcode + lat + long ,data = train)
summary(lm.train)

plot(train$price, lm.train$residuals)

plot(lm.train)

prediction <- predict(lm.train, test)
actual = test$price
plot(prediction,actual)
