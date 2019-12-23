# Multiple Linear Regression

# Importing the dataset
mydata= read.csv("C:\\Users\\DELL\\Desktop\\Multiple Linear Regrssion\\MLR Project 1 Profit Analysis\\50_Startups.csv", header = TRUE)
names(mydata)

# Encoding categorical data
mydata$State = factor(mydata$State,levels = c('New York', 'California', 'Florida'),labels = c(1, 2, 3))

# install.packages('caTools')
install.packages('caTools')

library(caTools)
set.seed(123)
split = sample.split(mydata$Profit, SplitRatio = 0.8)
training_set = subset(mydata, split == TRUE)
test_set = subset(mydata, split == FALSE)

# Fitting Multiple Linear Regression to the Training set

fit = lm(formula = Profit ~ .,data = training_set)
plot(fit)

# Check output of regression

summary(fit) 
# With all in, Adjusted R- sq is 0.9425, Also at sig level 0.05, state is not significant, removing state var

# Backward Elimination

# First iteration (remove State)

fit = lm(formula = Profit ~ `R.D.Spend` + Administration + `Marketing.Spend`,
               data = training_set)

summary(fit)

# Second iteration (Remove administration)

fit = lm(formula = Profit ~ `R.D.Spend` + `Marketing.Spend`,
               data = training_set)

summary(fit)

# Third iteration (Remove Marketing spend)

fit <- lm(formula = Profit ~ `R.D.Spend`,
                data = training_set)

summary(fit)

# We will consider Marketing spend in the model since it is very close to the significance level of 0.05 as well 
#including it increases the R- Sqaured

# Final

fit = lm(formula = Profit ~ `R.D.Spend` + `Marketing.Spend`,
               data = training_set)

summary(fit)

#prediction

y_pred = predict(fit, newdata = test_set)

y_pred

test_set$Profit

#plotting of y_pred v Profit
plot(y_pred, test_set$Profit)