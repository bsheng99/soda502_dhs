## SODA 502 

# regression on TFR

data <- read.csv("Japan_TFR_pop.csv")

# Format geographical names; remove numbers, space, drop total, sort df alphabetically
data$Prefecture = gsub('[0-9]+', '', tolower(data$Prefecture))
data$Prefecture = gsub("^\\s+|\\s+$", "", data$Prefecture)
data = data[2:length(data$Prefecture),]
data <- with(data,  data[order(data$Prefecture) , ])

# transform population variable
hist(data$pop)
plot(density(data$pop))
hist(log(data$pop))
plot(density(log(data$pop)))
data$pop <- log(data$pop)-mean(log(data$pop))
plot(density(data$pop))

# compute change in TFR
data$change0607 <- data[,3]-data[,2]
data$change0708 <- data[,4]-data[,3]
data$change0809 <- data[,5]-data[,4]
data$change0910 <- data[,6]-data[,5]
data$change1011 <- data[,7]-data[,6]
data$change1112 <- data[,8]-data[,7]
data$change1213 <- data[,9]-data[,8]
data$change1012 <- data[,8]-data[,6]
data$change1013 <- data[,9]-data[,6]
data$change1113 <- data[,9]-data[,7]

##########################################################################

# run OLS on single year TFR
fit.2006 <- lm(X2006~pop+nucl, data=data)
summary(fit.2006)
fit.2007 <- lm(X2007~pop+nucl, data=data)
summary(fit.2007)
fit.2008 <- lm(X2008~pop+nucl, data=data)
summary(fit.2008)
fit.2009 <- lm(X2009~pop+nucl, data=data)
summary(fit.2009)
fit.2010 <- lm(X2010~pop+nucl, data=data)
summary(fit.2010)
fit.2011 <- lm(X2011~pop+nucl, data=data)
summary(fit.2011)
fit.2012 <- lm(X2012~pop+nucl, data=data)
summary(fit.2012)
fit.2013 <- lm(X2013~pop+nucl, data=data)
summary(fit.2013)

# run OLS on one-year changes
fit.change0607 <- lm(change0607~pop+nucl, data=data)
summary(fit.change0607)
fit.change0708 <- lm(change0708~pop+nucl, data=data)
summary(fit.change0708)
fit.change0809 <- lm(change0809~pop+nucl, data=data)
summary(fit.change0809)
fit.change0910 <- lm(change0910~pop+nucl, data=data)
summary(fit.change0910)
fit.change1011 <- lm(change1011~pop+nucl, data=data)
summary(fit.change1011)
fit.change1112 <- lm(change1112~pop+nucl, data=data)
summary(fit.change1112)
fit.change1213 <- lm(change1213~pop+nucl, data=data)
summary(fit.change1213)

# run OLS on changes since 2010
fit.change1011 <- lm(change1011~pop+nucl, data=data)
summary(fit.change1011)
fit.change1012 <- lm(change1012~pop+nucl, data=data)
summary(fit.change1012)
fit.change1013 <- lm(change1013~pop+nucl, data=data)
summary(fit.change1013)

# run OLS on changes since 2011
fit.change1112 <- lm(change1112~pop+nucl, data=data)
summary(fit.change1112)
fit.change1113 <- lm(change1113~pop+nucl, data=data)
summary(fit.change1113)

##########################################################################

# function to pull coefficients and p-values
summ_func <- function(fit) {
  return(c(summary(fit)$coefficients[1,c(1,4)],summary(fit)$coefficients[2,c(1,4)],summary(fit)$coefficients[3,c(1,4)]))
}

# table for individual year TFR
t1 <- rbind(summ_func(fit.2006),summ_func(fit.2007),summ_func(fit.2008),summ_func(fit.2009),
            summ_func(fit.2010),summ_func(fit.2011),summ_func(fit.2012),summ_func(fit.2013))
t1 <- round(t1, 2)
t1
write.csv(t1, file="Japan_TFR_t1.csv")

# table for one-year changes
t2 <- rbind(summ_func(fit.change0607),summ_func(fit.change0708),summ_func(fit.change0809),summ_func(fit.change0910),
            summ_func(fit.change1011),summ_func(fit.change1112),summ_func(fit.change1213))
t2 <- round(t2, 2)
t2
write.csv(t2, file="Japan_TFR_t2.csv")
