## Git setup
git config credential.helper store    
git remote add --mirror=push origin https://github.com/JRTheStudent/ProgrammingAssignment3.git

## Github OAuth Example:
## https://github.com/hadley/httr/blob/master/demo/oauth2-github.r

library(datasets)
data(iris)
data(mtcars)


## General
## Get all rows when specific columns are in a set
mtcars[mtcars$cyl %in% c(4,8),]

## See R ProgrammingAssignment3 for context
## Return hospital name in that state with lowest 30-day death rate
## Return row of data frame with minimum of particular column
stateData <- data[data$State == state & ! is.na(data[oCol]), c(2,oCol)]
as.character(stateData[which.min(stateData[[2]]),1])

## Quiz 1
## Q 10: x <- c(3, 5, 1, 10, 12, 6) and I want to set all elements of this 
## vector that are less than 6 to be equal to zero.
## Set elements matching condition to a value
x[x %in% 1:5] <- 0

## Quiz 3 

## Q1: what is the mean of 'Sepal.Length' for the species virginica?
## What is the mean of column for another specific column value
tapply(iris$Sepal.Length, iris$Species, mean)
## or
colMeans(subset(iris, Species == "virginica", select = Sepal.Length))

## Q2: Vecvector of the means of the variables 'Sepal.Length', 'Sepal.Width', 
## 'Petal.Length', and 'Petal.Width'?
## Mean of several columns
apply(iris[, 1:4], 2, mean)
## or
colMeans(iris[,1:4])

## Q3: average miles per gallon (mpg) by num cylinders in the car (cyl)?
## Average of column group by another column
sapply(split(mtcars$mpg, mtcars$cyl), mean)

## Q4: what is the absolute difference between the average horsepower of 
## 4-cylinder cars and the average horsepower of 8-cylinder cars?
## Absolute difference mean of columns by specific column values
abs(mean(mtcars[mtcars$cyl == 4,"hp"]) - mean(mtcars[mtcars$cyl == 8,"hp"]))
## or
z <- lapply(split(mtcars, mtcars$cyl), function(y) mean(y$hp))
abs(z$`4` - z$`8`)



#Quiz 3
> gdp <- read.csv("gdp.csv")
> ed <- read.csv("ed_gdp.csv")
> mGDP <- merge(gdp, ed, by.x="X", by.y="CountryCode")
> mGDP$GDPr <- as.integer(as.character(mGDP[[2]]))
> mGDP <- arrange(mGDP, desc(GDPr))
> mGDP <- mGDP[! is.na(mGDP$GDPr),]
> mGDP[13,c(1,2,11,41)]
