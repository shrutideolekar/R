library(car)
library(corrplot)
library(olsrr)
library(ggplot2)

House = read.csv(file.choose())
colnames(House)[colnames(House)=="ï..Price"] <- "Price"

#1.1 Using a boxplot, histogram and summary. Describe the distribution of 
#the sales price of the houses.
boxplot(House$Price, main = "Price")

hist(House$Price,breaks=30)
d <- density(House$Price) # returns the density data
plot(d)
summary(House$Price)

#1.2. Convert all the categorical variables to factors. Using the summary and
#a boxplot describe how sales prices vary with respect to the number of bedrooms,
#bathrooms, garage size and school.
sapply(House, class)
str(House)
head(House)

#Convert all the categorical variables to factors
House$Bed <- factor(House$Bed,levels = c(2,3,4,5,6))
House$Garage <- factor(House$Garage,levels = c(0,1,2,3))
House$Bath <- factor(House$Bath,levels = c(1.0,1.1,2.0,2.1,3.0,3.1))

by(House$Price,House$Bed,summary)
boxplot(House$Price~House$Bed)

by(House$Price,House$Bath,summary)
boxplot(House$Price~House$Bath)

by(House$Price,House$Garage,summary)
boxplot(House$Price~House$Garage)

by(House$Price,House$School,summary)
boxplot(House$Price~House$School)

#1.3. Using the summary, correlation and the pairs plots discuss the relationship 
#between the response sales price and each of the numeric predictor variables. 
sapply(House, class)

by(House$Price,House$Size,summary)
pairs(House$Price~House$Size)

by(House$Price,House$Lot,summary)
pairs(House$Price~House$Lot)

by(House$Price,House$Year,summary)
pairs(House$Price~House$Year)

x <- House$Price
cor(x, House$Size)
cor(x, House$Year)
cor(x, House$Lot)

cor(House[,c(1,2,3,6)]) #Correlation Matrix

#2.1. Fit a multiple linear regression model to the data with sales price as 
#the response and size, lot, bath, bed, year, garage and school as the predictor 
#variables. Write down the equation for this model.

#check if data is suitable for fitting
qqnorm(House$Size)
qqnorm(House$Lot)
qqnorm(House$Year)
which(is.na(House))

#rescale data
House$Size <- House$Size - mean(House$Size)
House$Lot <- House$Lot - mean(House$Lot)
House$Year <- House$Year - mean(House$Year)

#Fit MLR model
model <- lm(Price ~ Lot + Size + Year + Bath + Bed + Garage + School, data = House)

summary(model)
coefficients(model)
plot(resid(model))

#2.10 Interpret the Adjusted R-squared value.
summary(model)$r.squared
summary(model)$adj.r.squared

#3.1 Type 1 ANOVA table
anova(model)

#3.3 Compute a type 2 anova table comparing the full model with all predictor
#variables to the the reduced model with the suggested predictor variable
# identified in the previous question removed.
model_red <- lm(Price ~ Lot + Size + Bath + Bed + Garage + School, data = House)
summary(model_red)

Anova(model, type = "II") #type two ANOVA model

anova(model,model_red) #comparison

#4.1 Check the linearity assumption by interpreting the added variable plots and component-plus-residual plots.
avPlots(model)
crPlots(model)

#4.2 Check the random/i.i.d. sample assumption by carefully reading the data description and computing the Durbin Watson test
dwt(model)

#4.3 Check the collinearity assumption by interpreting the correlation and variance inflation factors.
corr <- cor(House[,c(1,2,3,6)])
corrplot.mixed(corr)
vif(model)

#4.4 Check the zero conditional mean and homoscedasticity assumption by 
#interpreting the studentized residuals vrs fitted values plots and the 
#studentized residuals vrs predictor variable plots.
plot(fitted(model),rstudent(model))
abline(h=0)

par(mfrow=c(2,2))

plot(House$Lot,rstudent(model))
plot(House$Lot,House$Price)

plot(House$Size,rstudent(model))
plot(House$Size,House$Price)

plot(House$Year,rstudent(model))
plot(House$Year,House$Price)

#4.5 Check the Normality assumption by interpreting the histogram and quantile plot of the studentized residuals.
r = rstudent(model)
hist(r, freq=FALSE,breaks="FD")
lines(density(r,na.rm=T),lwd=2,col="Blue")

#qq plot
qqnorm(rstudent(model))
qqline(rstudent(model), col = 2)

#5.1 Use the leverage values and the leverage plots to see if there is any leverage points.
leveragePlots(model)
levPoints = as.numeric(which(hatvalues(model)>((2*7)/length(House$Price))))
levPoints

#5.2 Use the influence plot to see if there is any influence points.
par(mfrow=c(1,1))
influencePlot(model)

#5.3 Use the outlier test and outlier and leverage diagnostics plot to see if there is any outliers. Deal
#with the outliers if any are identified.
outlierTest(model)
plot(cooks.distance(model))
ols_plot_resid_lev(model)

#Q6 Plot the observed house prices, their expected vale (fitted value), 
#confidence intervals (in red) and prediction intervals (in blue).
confint(model)
confInt = predict(model,level = 0.95, interval = 'confidence')
predInt = predict(model, level = 0.95, interval = 'prediction')
Y = House$Price
X = fitted(model)
ggplot(House,aes(y = Y,x = X))+geom_point()+
  stat_smooth(aes(y=a[,'upr']),method=lm, se=F,col='blue')+
  stat_smooth(aes(y=a[,'lwr']),method=lm, se=F,col='blue')+
  stat_smooth(aes(y=b[,'upr']),method=lm, se=F,col='red')+
  stat_smooth(aes(y=b[,'lwr']),method=lm, se=F,col='red')+
  geom_line(aes(y=a[,'fit']),col='black')
