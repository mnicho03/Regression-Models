# You work for Motor Trend, a magazine about the automobile 
# industry. Looking at a data set of a collection of cars, they 
# are interested in exploring the relationship between a set of 
# variables and miles per gallon (MPG) (outcome). They are 
# particularly interested in the following two questions:
# 
# 1) "Is an automatic or manual transmission better for MPG"
# 2) "Quantify the MPG difference between automatic and manual 
#       transmissions"


#load R packages
library(ggplot2) # for exploratory data analyis / visuals

#load dataset
data(mtcars)

#display top 6 rows and basic structure
head(mtcars)
str(mtcars)

#preprocessing - change certain variables to factors
mtcars$cyl <- factor(mtcars$cyl)
mtcars$vs <- factor(mtcars$vs)
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)

#update 'am' variable as automatic vs transmission and set to factor variable
#from the RStudio's documentation: am = Transmission (0 = automatic, 1 = manual)
mtcars$automatic <- as.factor(mtcars$am)
levels(mtcars$automatic) <-c("Automatic", "Manual")

#remove original variable
mtcars$am <- NULL


### Exploratory Data Analysis
#histogram of mpg by automatic vs transmission with density overlay
ggplot(mtcars, aes(x = mpg, color = automatic, fill = automatic)) + 
        geom_histogram(aes(y=..density..), alpha = .5, position = "identity", binwidth = 2)+
        geom_density(alpha=.2) + 
        labs(title = "Histogram of MPG by Transmission Type", subtitle = "Includes Density Overlay")

#compare automatic and manual by weight
ggplot(mtcars, aes(wt, mpg)) + 
        geom_point(aes(color = automatic)) + 
        labs(title = "Spread of MPG by Weight", subtitle = "Colors based On Transmission Type", x = "Weight", Y = "MPG")

#Analysis
#conduct basic t test to determine whether one transmission is better in terms of MPG
ttestMPG <- t.test(mpg ~ automatic, data = mtcars)
ttestMPG$p.value

#test correlation to mpg of all variables
#revert transmission type to numeric for correlation test
mtcars$automatic <- as.numeric(mtcars$automatic)
sort(abs(cor(mtcars)[1,]), decreasing = TRUE)

#Based on the correlations between each variable and mpg, transmission type is 6th most significant. We will consider the 6 more substantial correlations going forward.

#simple linear regression
fit <- lm(mpg ~ automatic, data = mtcars)
summary(fit)

#Interpretation:
#Coefficient and intercepts indicate on average manual transmission cars have 7.245 mpg more than automatic transmission cars.
#R^2 value = 0.3598; indicating the model explains 35.98% of the variance, which is not sufficient.
#Without further variance explained, we need to conduct further tests.

#multivariate regression analysis 
fitAll <- lm(mpg ~ ., data = mtcars)

#Run the step function (stepwise regression) to find the best choice of predictors.
bestFit <- step(fitAll, trace = 0, direction = "both")

#provide the summary
summary(bestFit)

#The resulting bestFit model evaluates mpg based on wt (weight), qsec (1/4 mile times), and automatic (transmimssion type).
#Best Fit Model R^2: 84.97%, meaning the model captures 84.97% of the variance, far more respectable than the 36% captured by transmission type alone.

#Answer to one of the initial questions:
"Quantify the MPG difference between automatic and manual transmissions"
summary(bestFit)$coef[4]
#On average, manual transmission cars average 2.94 MPG more than automatic transmission cars, considering other variables.

#Display residual diagnostics using the best fit model (mpg ~ wt + qsec + automatic).
par(mfrow=c(2,2))
plot(bestFit)

#appendix
plot(mtcars)

ggplot(mtcars, aes(x = factor(automatic), y = mpg, fill = factor(automatic))) +
        geom_boxplot(notch = F)+ 
        scale_x_discrete("Transmission")+
        scale_y_continuous("MPG")+
        ggtitle("MPG by Transmission Type")