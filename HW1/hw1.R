# problem 1 Data Analysis and Cleaning
# For goldie:
### adult <- read.table("/Users/maggie/Documents/DataMiningCourse/kevinsmall/hw1/data/adult/combined/adult_combined_missing.csv", header = TRUE, sep = ",")
# for silver:
# 
adult <- read.table("/Users/mingzhexu/Documents/DataMiningCourse/kevinsmall/hw1/data/adult/combined/adult_combined_missing.csv", header = TRUE, sep = ",")
# (a) Basic Statistical Description

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
