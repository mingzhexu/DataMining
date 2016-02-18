# problem 1 Data Analysis and Cleaning
# For goldie:
# adult <- read.table("/Users/maggie/Documents/DataMiningCourse/kevinsmall/hw1/data/adult/combined/adult_combined_missing.csv", header = TRUE, sep = ",")
# for silver:
# adult <- read.table("/Users/mingzhexu/Documents/DataMiningCourse/kevinsmall/hw1/data/adult/combined/adult_combined_missing.csv", header = TRUE, sep = ",")
age <- adult$age
fnlwgt <- adult$fnlwgt
edu.num <- adult$education.num
cap.gain <- adult$capital.gain
cap.loss <- adult$capital.loss
hours <- adult$hours.per.week
income <- adult$income
#####################################
# (a) Basic Statistical Description
# i) Compute mean, min, 1st Qu, median, 3rd Qu, Max, mode
## sum
sumfun <- function(col){
  sum = 0
  for(i in 1 : length(col))
  {
    if(!(is.na(col[i])))
    {
      sum = sum + col[i]
    }
  }
  return(sum)
}
## Missing
missingfun <- function(col){
  num = 0
  for(i in 1 : length(col)){
    if(is.na(col[i])){
      num = num + 1
    }
  }
  return(num)
}
## Mean:
meanfun <- function(col){
  return(sumfun(col)/(length(col) - missingfun(col)))
}

## Median
medianfun <- function(col){
  col <- sort(col)
  len = length(col) - missingfun(col)
  #print(col[1])
  #print(col[length(col)])
  if(len%%2==1)
  {
    return(col[len%/%2 + 1])
  }
  return((col[len/2] + col[len/2 + 1])/2)
}
## max
maxfun <- function(col){
  max = 0
  for (i in 1 : length(col))
  {
    if(!(is.na(col[i])))
      if(col[i] > max)
      {
        max = col[i]
      }
    max = max
  }
  return(max)
}

## min
minfun <- function(c){
  min = .Machine$integer.max
  for (i in 1 : length(c))
  {
    if(!(is.na(c[i])))
      if(c[i] < min)
      {
        min = c[i]
      }
    min = min
  }
  return(min)
}

## mode: the number that appear most frequently
modefun <- function(col){
  return(names(sort(-table(col)))[1])
}

## Q1:
q1fun <- function (col){
  col <- sort(col)
  pos <- (length(col)+1)%/%4
  rem <- (length(col)+1)%%4
  if((length(col)+1) %% 4 == 0)
  {
    return(col[pos])
  }
  return(col[pos]+(rem*(col[pos+1] - col[pos])))
}

## Q3:
q3fun <- function (col){
  col <- sort(col)
  pos <- 3*((length(col)+1)%/%4)
  rem <- (3*(length(col)+1))%%4
  if(3*((length(col)+1)%%4) == 0)
  {
    return(col[pos])
  }
  return(col[pos] + rem * (col[pos + 1] - col[pos]))
}

printfun <- function(name, col){
  min <- minfun(col)
  max <- maxfun(col)
  q1 <- q1fun(col)
  q3 <- q3fun(col)
  median <- medianfun(col)
  mean <- meanfun(col)
  mode <- modefun(col)
  missing <- missingfun(col)
  cat("\nFor attribute", name, "\n min: ", min, "\n Q1: ", q1,"\n median: ", median, 
      "\n Q3: ", q3, "\n Max: ", max, "\n Mean: ", mean, "\n Mode: ", mode, "\n Missing: ", missing)
}

# ii)
## Calculation:
output <- cat(
printfun("age", age),
printfun("fnlwgt", fnlwgt),
printfun("edu.num", edu.num),
printfun("cap.gain", cap.gain),
printfun("cap.loss", cap.loss),
printfun("hours", hours)
)

out <- capture.output(cat(
  printfun("age", age),
  printfun("fnlwgt", fnlwgt),
  printfun("edu.num", edu.num),
  printfun("cap.gain", cap.gain),
  printfun("cap.loss", cap.loss),
  printfun("hours", hours)
))

cat("Output", out, file="/Users/mingzhexu/Documents/MSCS-NEU/2016Spring/DataMining/out.txt", sep="\n")

#####################################
# (b) Visualizing data
# setwd("/Users/mingzhexu/Documents/DataMiningCourse/kevinsmall/hw1/data/adult/combined/")
# setwd("/Users/maggie/Documents/DataMiningCourse/kevinsmall/hw1/data/adult/combined/")

# i)
summary(adult)
summary(age)

# ii)
boxplot(age, main="age boxplot")
boxplot(age ~ income, main = "age conditional boxplot")
#based on the box plot of age, we can derive several arguments:
# - the median is 37 so half of the people are below 37, half above.
# - half of the people are from 28 to 48
# - the conditional boxplot:
# -- the median age for income <= 50k is less than 37 and median age for income > 50k is more than 40

boxplot(edu.num, main = "education number boxplot")
boxplot(edu.num ~ income, main = "edu.num conditional boxplot")
#based on the box plot of education number, we can derive several arguments:
# - the median is 10 and half of the sample is within 9 - 12
# - 25% of the sample is 9 and 25% are 11 or 12
# - median of education number for those income <= 50k is 9, while for income more than 50k, the median education number is obviously more than 10

boxplot(hours, main = "hours boxplot")
boxplot(hours ~ income, main = "hours conditional boxplot")
#based on the box plot of hours per month, we can derive several arguments:
# - the median is 40 and around 25% of the sample is 40
# - 25% of the sample is between 40 and 45
# - the conditional boxplot shows that the median hours from people who has income less or equal to 50k is 40, the same as those who earn more than 50k
# - 75% people whose income is less or equal to 50k work less than or equal to 40 hours
# - 75% people whose income is more than 50k work more than or equal to 40 hours

# For capital.gain or capital.loss or capital gain, the value is either 0 or a large number and mostly are 0, so we couldn't tell much from the boxplot with median q1 q3 being 0.

# iii)
hist(age)
hist(cap.gain)
hist(cap.loss)
summary(cap.gain)
# Interpret
# The histogram tells more than boxplots.
# For age, it is a little bit skewed to the left.
# And there are more people range from age 20 to 45 than other ranges
# cap.gain and cap.loss have similar characteristics - both are heavily skewed with most values being 0.

# iv)
wc <- table(adult$workclass)
barplot(wc, main = "workclass")
wc2 <- table(income, adult$workclass)
barplot(wc2, beside=T, main = "conditional barplot for workclass")
barplot(prop.table(wc2, 1), beside=T, main = "conditional barplot for workclass - proportional")

edu <- table(adult$education)
barplot(edu, main = "education")
edu2 <- table(income, adult$education)
barplot(edu2, beside=T, main = "conditional barplot for education")
barplot(prop.table(edu2, 1), beside=T, main = "conditional barplot for education - proportional")

ms <- table(adult$marital.status)
ms2 <- table(income, adult$marital.status)
barplot(ms, main = "marital status")
barplot(ms2, beside=T, main = "conditional barplot for marital status")
barplot(prop.table(ms2, 1), beside=T, main = "conditional barplot for marital status - proportional")

ocp <- table(adult$occupation)
ocp2 <- table(income, adult$occupation)
barplot(ocp, main = "occupation")
barplot(ocp2, beside=T, main = "conditional barplot for occupation")
barplot(prop.table(ocp2, 1), beside=T, main = "conditional barplot for occupation - proportional")

rls <- table(adult$relationship)
rls2 <- table(income, adult$relationship)
barplot(rls, main = "relationship")
barplot(rls2, beside=T, main = "conditional barplot for relationship")
barplot(prop.table(rls2, 1), beside = T, main = "conditional barplot for relationship - prop")

rc <- table(adult$race)
rc2 <- table(income, adult$race)
barplot(rc, main = "race")
barplot(rc2, beside=T, main = "conditional barplot for race")
barplot(prop.table(rc2, 1), beside=T, main="conditional barplot for race - prop")

sex <- table(adult$sex)
sex2 <- table(income, adult$sex)
barplot(sex, main= "sex")
barplot(sex2, beside=T, main = "conditional barplot for sex")
barplot(prop.table(sex2, 1), beside=T, main = "conditional barplot for sex - prop")

nav <- table(adult$native.country)
nav2 <- table(income, adult$native.country)
barplot(nav, main = "native country")
barplot(nav2, beside=T, main = "conditional barplot for native country")
barplot(prop.table(nav2, 1), beside=T, main = "conditional barplot for native country - prop")

# v)
adult1k <- adult[sample(1:nrow(adult), 100, replace=FALSE),]
pairs(~adult1k$age + adult1k$education.num + adult1k$hours.per.week)

# vi)
plot(age, adult$hours.per.week)

# vii)
chisq.test(table(income, adult$workclass))

##########################################################################
# Problem 2: Item Similarity
