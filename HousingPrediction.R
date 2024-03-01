library(dplyr)
library(ggplot2)
library(corrgram)
library(corrplot)
library(zoo)
library(fastDummies)
library(caTools)
# Read my data 
df <- read.csv("C:\\Users\\LA\\Documents\\R\\My Project\\housing.csv")

# Check the head, structure and summary of the data
head(df)
str(df)
summary(df)

### FEATURE ENGINEERING ###

# Check for missing values in the data
any(is.na(df))
# Check the column with the Na values
colSums(is.na(df))
# Replacing the Na values with the column mean value
for(i in 1:ncol(df)){
  df[is.na(df[,i]), i] <- mean(df[,i], na.rm = TRUE)
}
# Dropping the unwanted columns
df <- df %>% select(-c('longitude','latitude'))

## Exploratory Data Analysis
## Visualization of the columns using ggplot
# Housing median value
ggplot(df,aes(x=housing_median_age)) + geom_histogram(fill='blue')
ggplot(df,aes(x=total_rooms)) + geom_histogram(fill='blue')
ggplot(df,aes(x=total_bedrooms)) + geom_histogram(fill='blue')
ggplot(df,aes(x=population)) + geom_histogram(fill='blue')
ggplot(df,aes(x=households)) + geom_histogram(fill='blue')
ggplot(df,aes(x=median_income)) + geom_histogram(fill='blue')
## Visualizing each column against median house value
# Housing median age vs median house value
ggplot(df,aes(x=housing_median_age,y=median_house_value)) + geom_point(color='blue') + scale_y_continuous(name = "House Value",limits = c(0,500000),breaks = c(0,100000,200000,300000,400000,500000),labels = c('0','100000','200000','300000','400000','500000'))
# Total rooms vs median house value
ggplot(df,aes(x=total_rooms,y=median_house_value)) + geom_point(color='blue') + scale_y_continuous(name = "House Value",limits = c(0,500000),breaks = c(0,100000,200000,300000,400000,500000),labels = c('0','100000','200000','300000','400000','500000'))
# Total bedrooms vs median house value
ggplot(df,aes(x=total_bedrooms,y=median_house_value)) + geom_point(color='blue') + scale_y_continuous(name = "House Value",limits = c(0,500000),breaks = c(0,100000,200000,300000,400000,500000),labels = c('0','100000','200000','300000','400000','500000'))
# Population vs median house value
ggplot(df,aes(x=population,y=median_house_value)) + geom_point(color='blue') + scale_y_continuous(name = "House Value",limits = c(0,500000),breaks = c(0,100000,200000,300000,400000,500000),labels = c('0','100000','200000','300000','400000','500000'))
# Households vs median house value
ggplot(df,aes(x=households,y=median_house_value)) + geom_point(color='blue') + scale_y_continuous(name = "House Value",limits = c(0,500000),breaks = c(0,100000,200000,300000,400000,500000),labels = c('0','100000','200000','300000','400000','500000'))
 # Median income vs median house value
ggplot(df,aes(x=median_income,y=median_house_value)) + geom_point(color='blue') + scale_y_continuous(name = "House Value",limits = c(0,500000),breaks = c(0,100000,200000,300000,400000,500000),labels = c('0','100000','200000','300000','400000','500000'))
# Ocean Proximity vs median house value
ggplot(df, aes(x = ocean_proximity, y = median_house_value,fill=ocean_proximity)) + geom_bar(stat = "identity") + scale_y_continuous(name = "House Value",limits = c(0, 500000),breaks = seq(0, 500000, by = 100000),labels = c('0', '100000', '200000', '300000', '400000', '500000')) +  scale_fill_manual(values = c("blue", "green", "red", "yellow", "orange"))
# Using barplot to count the ocean proximity
ggplot(df,aes(x=ocean_proximity,fill=ocean_proximity)) + geom_bar() + scale_fill_manual(values = c("blue", "green", "red", "yellow", "orange"))

# Converting the Ocean_proximity column from a categorical variable to a numeric column
df <- dummy_cols(df, select_columns = "ocean_proximity", remove_first_dummy = TRUE, remove_selected_columns = TRUE)

# using corrplot to perform correlations between the variables
nums.col <- sapply(df,is.numeric)
corr.data <- cor(df[,nums.col])
#corrplot(corr.data, method = 'number', tl.cex = 0.6)

### Building the model
#set a seed
set.seed(101)
# Split up sample
sample <- sample.split(df$median_house_value,SplitRatio = 0.8)
# 80% of data <- train
train <- subset(df, sample==TRUE)
# 20% of data <- test
test <- subset(df,sample==FALSE)

# Train and Build model
model <- lm(median_house_value~., data = train)
print(summary(model))

# Test the model
median_house_value.prediction <- predict(model,test)


#Printing and comparing the results with the actual median house value against the predicted one
results <- cbind(median_house_value.prediction,test$median_house_value)
colnames(results) <- c('predicted','actual')
results <- as.data.frame(results)
#print(results)

## MEAN SQUARED ERROR
mse <- mean((results$actual - results$predicted)^2)
print('MSE')
print(mse)

## ROOT MEAN SQUARED ERROR
print('RMSE')
print(mse^0.5)

## R-SQUARED VALUE
# 1-SSE/SST
# SSE MEANS SUM OF SQUARED ERRORS
# SST MEANS SUM OF SQUARED TOTAL
SSE <- sum((results$predicted - results$actual)^2)
SST <- sum((mean(df$median_house_value) - results$actual)^2)

R2 <- 1 - SSE/SST
print('R2')
print(R2)

### Using only one independent variable to create a model i.e median income
#ggplot(test,aes(x=median_income,y=median_house_value)) + geom_point(color='blue',alph=0.5) + scale_y_continuous(name = "House Value",limits = c(0,500000),breaks = c(0,100000,200000,300000,400000,500000),labels = c('0','100000','200000','300000','400000','500000')) + ggtitle('Median income vs House Value(test)') + geom_smooth(method = 'lm', se=FALSE, color='red')
#ggplot(train,aes(x=median_income,y=median_house_value)) + geom_point(color='blue',alpha=0.5) + scale_y_continuous(name = "House Value",limits = c(0,500000),breaks = c(0,100000,200000,300000,400000,500000),labels = c('0','100000','200000','300000','400000','500000')) + ggtitle('Median income vs House Value(train)') + geom_smooth(method = 'lm', se=FALSE,color = 'red')


