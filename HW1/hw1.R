# problem 1 Data Analysis and Cleaning
# For goldie:
### adult <- read.table("/Users/maggie/Documents/DataMiningCourse/kevinsmall/hw1/data/adult/combined/adult_combined_missing.csv", header = TRUE, sep = ",")
# for silver:
# 
adult <- read.table("/Users/mingzhexu/Documents/DataMiningCourse/kevinsmall/hw1/data/adult/combined/adult_combined_missing.csv", header = TRUE, sep = ",")
age <- adult$age
fnlwgt <- adult$fnlwgt
edu.num <- adult$education.num
cap.gain <- adult$capital.gain
cap.loss <- adult$capital.loss
hours <- adult$hours.per.week
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
    num = num
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
  if(len%%2==1)
  {
    return(col[len%/%2 + 1])
  }
  return(col[len/2])
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
if(false)
{  col = sort(col)
   most = 0
   mostnum = col[1]
   freq = 0
  num = col[1]
  for(i in 1 : length(col)){
    if(!is.na(col[i])){
      if(col[i] == num){
        freq = freq + 1
      }
      if(col[i] != num){
        if(freq > most){
          most <- freq
          mostnum <- col[i-1]
          freq <- 1
          num <- col[i]
        }
      }
      if((i == length(col)) && (freq > most)){
        return(col[i])
      } 
    }
  }
  return(mostnum)
}
}
## Calculation:
minfun(age)
minfun(fnlwgt)
minfun(edu.num)
minfun(cap.gain)
minfun(cap.loss)
minfun(hours)
maxfun(age)
maxfun(fnlwgt)
maxfun(edu.num)
maxfun(cap.gain)
maxfun(cap.loss)
maxfun(hours)
meanfun(age)
meanfun(fnlwgt)
meanfun(edu.num)
meanfun(cap.gain)
meanfun(cap.loss)
meanfun(hours)
medianfun(age)
medianfun(fnlwgt)
medianfun(edu.num)
medianfun(cap.gain)
medianfun(cap.loss)
medianfun(hours)
modefun(age)
modefun(fnlwgt)
modefun(edu.num)
modefun(cap.gain)
modefun(cap.loss)
modefun(hours)
missingfun(age)
missingfun(fnlwgt)
missingfun(edu.num)
missingfun(cap.gain)
missingfun(cap.loss)
missingfun(hours)
# (b) Visualizing data
setwd("/Users/mingzhexu/Documents/DataMiningCourse/kevinsmall/hw1/data/adult/combined/")
# i)
summary(adult)
summary(adult$age)
# ii)
boxplot(adult$age)
boxplot(adult$age ~ adult$income)
# iii)
hist(adult$age)
hist(adult$capital.gain)
hist(adult$capital.loss)
summary(adult$capital.gain)
# iv)
wc <- table(adult$workclass)
barplot(wc)
wc2 <- table(adult$income, adult$workclass)
barplot(wc2, beside=T)
barplot(prop.table(wc2, 1), beside=T)

# v)
adult1k <- adult[sample(1:nrow(adult), 100, replace=FALSE),]
pairs(~adult1k$age + adult1k$education.num + adult1k$hours.per.week)

# vi)
plot(adult$age, adult$hours.per.week)

# vii)
chisq.test(table(adult$income, adult$workclass))


# Problem 2: Item Similarity
