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
cat(
printfun("age", age),
printfun("fnlwgt", fnlwgt),
printfun("edu.num", edu.num),
printfun("cap.gain", cap.gain),
printfun("cap.loss", cap.loss),
printfun("hours", hours)
)
#####################################
# (b) Visualizing data
# setwd("/Users/mingzhexu/Documents/DataMiningCourse/kevinsmall/hw1/data/adult/combined/")
# setwd("/Users/maggie/Documents/DataMiningCourse/kevinsmall/hw1/data/adult/combined/")

# i)
summary(adult)
summary(age)

# ii)
boxplot(age)
boxplot(age ~ income)
boxplot(edu.num)
boxplot(edu.num ~ income)
boxplot(hours)
boxplot(hours ~ income)

# iii)
hist(age)
hist(cap.gain)
hist(cap.loss)
summary(cap.gain)
# Interpret

# iv)
wc <- table(adult$workclass)
barplot(wc)
wc2 <- table(income, adult$workclass)
barplot(wc2, beside=T)
barplot(prop.table(wc2, 1), beside=T)

# v)
adult1k <- adult[sample(1:nrow(adult), 100, replace=FALSE),]
pairs(~adult1k$age + adult1k$education.num + adult1k$hours.per.week)

# vi)
plot(age, adult$hours.per.week)

# vii)
chisq.test(table(income, adult$workclass))

##########################################################################
# Problem 2: Item Similarity
