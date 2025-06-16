# GDPHealth_Clustering_Covid
Data Analysis GDP and Health


---
title: "Data Analysis GDP and Health"
output: pdf_document
date: '2022-04-16'
---

```R
library(knitr)
library(tidyverse)
library(kableExtra)
library(dplyr)
library(vtable)
library(qwraps2)
library(modest)
library(ggplot2)
library(caret)
library(gridExtra)
theme_set(theme_classic())
```

```{r setup, include=FALSE}
knitr::opts_chunk$set(fig.width=12, fig.height=8, options(scipen=999),
                      echo = TRUE)
```

```{r}
data <- read.csv("Gdphealth.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)
```

Remove comma from GDP and GDP.pc variables and convert them to numeric
```{r}
data <- data %>%
    mutate_each(funs(as.character(.)), GDP:GDP.pc) %>%
    mutate_each(funs(gsub(",", "", .)), GDP:GDP.pc) %>%
    mutate_each(funs(as.numeric(.)), GDP:GDP.pc)
head(data,3)
```
 
```{r}
p <- ggplot(data, aes(x=Country, y=GDP.pc, fill = Country))+
  geom_bar(stat="identity")
p + coord_flip()
```
 
 
 GDP are in local currencies, so convert to usd.
```{r}
# Add currecy conversion rate column
To_usd <- c(1.08,0.68,0.23,1.08,1.06,0.0077,1.08,0.74,1.31,1.08,
0.79,0.11,0.15,0.044,0.1,0.05,1.08,1.08,0.0029,1.08,
1.08,1,1.08,1.08,1.08,0.00081,0.0079,1.08,1.068)

#Multiply rates by GDP.pc and view 2 rows to confirm the operation
data['GDp'] <- To_usd*data$GDP.pc
head(data,2)
```


Select GDp, Growth and Health expenditures
```{r}
mydf <- select(data, -c(2,3))
head(mydf,5)
```


# Descriptive Statistics: Measures of location.
## Mean
```{r}
Mean <- mydf[2:4] %>% summarise_all(list(mean))
Median <- mydf[2:4] %>% summarise_all(list(median))
M <- rbind(Mean, Median)
MM <- cbind(M, MCT=c("Mean", "Median"))
knitr::kable(MM, caption = "Mean and Median")
```

## mode
```{r}
x <- table(mydf$Growth)
print("Mode of Growth is")
names(x)[which(x==max(x))]

y <- table(mydf$Health.exp)
print("Mode of Health.exp is")
names(y) [which(y==max(y))]

z <- table(mydf$GDp)
print("Mode of GDP is")
names(z) [which(z==max(z), TRUE)]
```

There is no Mode

Use histogram to check the distributions of Growth and Health expenditures
```{r}
Growth=mydf$Growth
Health_Expenditures=mydf$Health.exp

par(mfrow=c(1,2))
hist(Growth, breaks = 10)
hist(Health_Expenditures, breaks = 10)
```

AS can be seen, the distributions appears to right-skewed, and also we have an outlier in the Growth variable with growth index grater than 8. Hence we cannot repport mean as the measure of central tendency because mean is not centrally located, but rather the median is preferred.

```{r}
par(mfrow=c(1,2))
ggplot(mydf, aes(x=GDp)) + geom_density(fill="grey") + 
  labs(title="density") +
  theme(panel.background=element_rect(fill="yellow"),
        panel.grid.major=element_line(color="blue", size=2))
```

Interestingly, we have two peaks, hence the GDP is bimodal. It is therefore worth digging deeper to find out why this is the case.

## Measures of dispersion
The quartieles, Maximum and minimum values can be taken from the summary table below
```{r}
knitr::kable(summary(mydf[2:4]), caption = "summary statistics")
```
## Range
```{r}
# Range : maximum - mimimum
knitr::kable(mydf[2:4] %>% summarise_all(list(range)),
             caption = "Range")
```


## Interquartile, Variance, Standard deviation, Skewness, and Kurtosis.
```{r}
library(moments);
I_QR <- mydf[2:4] %>% summarise_all(list(IQR))
Var <- mydf[2:4] %>% summarise_all(list(var))
Stdv <- mydf[2:4] %>% summarise_all(list(sd))
Skness <- mydf[2:4] %>% summarise_all(list(skewness))
Kurtsis <- mydf[2:4] %>% summarise_all(list(kurtosis))
A = rbind(I_QR, Var)
B = rbind(A, Stdv)
C = rbind(B, Skness)
D = rbind(C, Kurtsis)
MOD <- cbind(D, Measure = c("I_Range","Variance","STDeviation", "Skewness","Kurtosis"))
MOD[1] = MOD$Measure
knitr::kable(MOD,
             caption = "Measures of Dispersion")
```

## Covariance and Correlation
```{r}
# covariance matrix
mydf1 <- mydf[2:4]
knitr::kable(cov(mydf1), caption = "covariance matrix")
```

```{r}
library(corrplot)
corrplot(cor(mydf1))
```

```{r}
knitr::kable(cor(mydf1), caption = "correlation")
```


```{r}
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
      {
          usr <- par("usr"); on.exit(par(usr))
          par(usr = c(0, 1, 0, 1))
          r <- abs(cor(x, y))
          txt <- format(c(r, 0.123456789), digits = digits)[1]
          txt <- paste0(prefix, txt)
          if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
          text(0.5, 0.5, txt, cex = cex.cor * r)
      }

pairs(mydf1,
      upper.panel = panel.cor,    # Correlation panel
      lower.panel = panel.smooth,
      main="Scatter plots") # Smoothed regression lines
```

# The OLS ( Ordinary Least Squares) Method
## GDP vs Growth
```{r}
# OLS model
mydf1 %>%
 ggplot(aes(x = Growth, y = GDp)) +
 geom_point(colour = "red") +
 geom_smooth(method = "lm", fill = NA)

# Correlation Coefficient
cor(mydf1$Growth, mydf1$GDp) 

ols1 <- lm(GDp ~ Growth, data = mydf1)
# model summary
summary(ols1)

# Confidence Interval
confint(ols1)
```



## GDP vs Health expenditure
```{r}
# OLS model2
mydf1 %>%
 ggplot(aes(x = Health.exp, y = GDp)) +
 geom_point(colour = "blue") +
 geom_smooth(method = "lm", fill = NA, colour = "red")
ols2 <- lm(GDp ~ Health.exp, data = mydf1)
```

```{r}
# Correlation coefficient
cor(mydf1$Health.exp, mydf1$GDp)
```

$R^2=0.3$, which shows a ly positive linear relationship between GDP and Health Expenditures.

```{r}
# Model2 summary
summary(ols2)
```

```{r}
# Confidence Interval
confint(ols2)
```


They both have weakly positive correlation coefficients with GDP, meaning that there are very small variations within the GDP that are being explained by Growth and Health expenditures.

# Non linear regression (OLS)
## 1. Prepare the data

```{r}
# Split the data into training and test set
set.seed(123)
training.samples <- mydf1$GDp %>%
  createDataPartition(p = 0.65, list = FALSE)
train.Gdp  <- mydf1[training.samples, ]
test.Gdp <- mydf1[-training.samples, ]

# Build the model
model <- lm(GDp ~ Growth, data = mydf1)
# Make predictions
predictions <- model %>% predict(test.Gdp)
# Model performance
data.frame(
  RMSE = RMSE(predictions, test.Gdp$GDp),
  R2 = R2(predictions, test.Gdp$GDp)
)
```

```{r}
# Lets Visualize and see how they look like
p1 <- ggplot(train.Gdp, aes(Growth, GDp) ) +
  geom_point() +
  stat_smooth()

p2 <- ggplot(train.Gdp, aes(Health.exp, GDp) ) +
  geom_point() +
  stat_smooth()
grid.arrange(p1,p2, ncol=2)
```



## GDP Vs. Growth

```{r}
# Now run the Regression models
model1 <- lm(GDp ~ Growth + I(Growth^2), data = train.Gdp)
summary(model1)
```

```{r}
#Confidence Intervals
as.data.frame(confint(model1))
```

$$
model1 = a + bX + cX^2
$$

which is the same as $\hat y = 16428.7 - 829.7X + 163.3X^2$

$R^2=0.024,p=0.7675$ means that its a poor fit, so we need to try a polynomial of a higher degree.

After trying some values I found that only polynomial of 6th order has some almost significant values.

```{r}
model_1 <- lm(GDp ~ poly(Growth, 6, raw = TRUE), data = train.Gdp)
summary(model_1)
```

Using such a high order polynomial would be a very huge abuse to linear regression so I considered the second order to make prediction

## GDP Vs. Health exp

```{r}
# Run the Regression models
model2 <- lm(GDp ~ Health.exp + I(Health.exp^2), data = train.Gdp)
summary(model2)
```

```{r}
#Confidence Intervals
as.data.frame(confint(model2))
```


# Predictive Analysis
```{r}
# Make predictions
new_data <- test.Gdp
predictions1 <- model1 %>% predict(new_data)
predictions2 <- model2 %>% predict(new_data)
# Models performance
grth <- data.frame(
  RMSE = RMSE(predictions1, new_data$Growth),
  R2 = R2(predictions1, new_data$Growth)
)

hlth <- data.frame(
  RMSE = RMSE(predictions2, new_data$Health.exp),
  R2 = R2(predictions2, new_data$Health.exp)
)
```

```{r}
# model1 performance
grth
```

```{r}
# model2 performance
hlth
```


```{r}
g <- ggplot(new_data, aes(Growth, GDp) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE))

h <- ggplot(new_data, aes(Health.exp, GDp) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE))
grid.arrange(g, h, ncol=2)
```
