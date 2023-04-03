## Exercise 4.8.R

## Code for part (a)

library (Stat2Data)
data ("CountyHealth")
names (CountyHealth)
head (CountyHealth)
dim (CountyHealth)

CountyHealth$sqrtMDs = sqrt (CountyHealth$MDs)

# Split the data, per the book exercise.

train.CH = CountyHealth [1:35, ]
test.CH  = CountyHealth [36:53, ]

# Set up a vector for coloring the training set black and the test set red.

train.test = c (rep (1, 35), rep (2, 18))

plot (sqrtMDs ~ Hospitals, data=CountyHealth, col=train.test)

# Fit model on the training sample

train.fit = lm (sqrtMDs ~ Hospitals, data=train.CH)
summary (train.fit)
plot (sqrtMDs ~ Hospitals, data=CountyHealth, col=train.test)
abline (train.fit)

## Code for part (b)

# Predictions on the testing sample

test.predict = predict (train.fit, list (Hospitals = test.CH$Hospitals))
plot (test.CH$sqrtMDs ~ test.predict, main="Test Data with Training Fit")
abline (0, 1)

# Cross-validation correlation and R-squared

(crval.cor = cor (test.CH$sqrtMDs, test.predict))
(test.Rsq = crval.cor^2)

## Code for part (c)

# Shrinkage

summary (train.fit)$r.squared - test.Rsq
