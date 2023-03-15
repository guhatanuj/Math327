library (Stat2Data)
data ("Perch")
names (Perch)
Perch$Len.Wid = Perch$Length * Perch$Width
fit.per = lm (Weight ~ Len.Wid, data=Perch)
plot (Weight ~ Len.Wid, data=Perch)
abline (fit.per)
summary (fit.per)
plot (fit.per, which=1:2)

# Transform the response variable, Weight, using square root

plot (sqrt(Weight) ~ Len.Wid, data=Perch)
fit2.per = lm (sqrt(Weight) ~ Len.Wid, data=Perch)
summary (fit2.per)
plot (fit2.per, which=1:2)

# Choose a power transformation using Box-Cox analysis

Perch$Weight.bc = Perch$Weight^0.626
plot (Weight.bc ~ Len.Wid, data=Perch)
fit2.per = lm (Weight.bc ~ Len.Wid, data=Perch)
abline (fit2.per)
summary (fit2.per)
plot (fit2.per, which=1:2)

# Put the first order predictors in:

plot (Perch [, 2:5])
fit3.per = lm (Weight ~ Length * Width, data=Perch)
summary (fit3.per)
par (mfrow=c(1,2))
plot (fit3.per, which=1:2)

par (mfrow=c(1,1))
plot (Weight ~ predict (fit3.per), data=Perch)
abline (0, 1)

# Centered predictors

Perch$Length.c = Perch$Length - mean (Perch$Length)
Perch$Width.c = Perch$Width - mean (Perch$Width)
Perch$Len.Wid.c = Perch$Length.c * Perch$Width.c
plot (Perch [, c(2, 7:9)])
fit4.per = lm (Weight ~ Length.c * Width.c, data=Perch)
summary (fit4.per)
par (mfrow=c(1,2))
plot (fit4.per, which=1:2)

Perch$yhat = predict (fit3.per)
Perch$yhat.c = predict (fit4.per)
Perch [c(1:3,54:56), c(2:5,10,7:9,11)]

# Plot observed Weight vs. fitted Weight

par (mfrow=c(1,1))
plot (Weight ~ predict (fit4.per), data=Perch)
abline (0, 1)


# Interaction effect plot for the Perch data

categorize = function (x, quantiles=(1:3)/4) {
  cutoffs = quantile (x, quantiles)
  n.cutoffs = length (cutoffs)
  result = rep ("C1", length (x))
  for (j in 1:n.cutoffs) {
    result [x > cutoffs [j]] = paste ("C", j+1, sep="")
  }
  return (result)
}

Widthcat = categorize (Perch$Width, quantiles=(1:2)/3)
lengthcat = categorize (Perch$Length, quantiles=(1:2)/3)

library (ggplot2)

ggplot (data=Perch, aes (y=Weight, x=Length, color=Widthcat)) + geom_point() +
  geom_smooth (method="lm")

ggplot (data=Perch, aes (y=Weight, x=Width, color=lengthcat)) + geom_point() +
  geom_smooth (method="lm")

# 3D scatter plot and fitted surface for the Perch data

my_surface <- function(f, n=10, ...) { 
  ranges <- rgl:::.getRanges()
  x <- seq(ranges$xlim[1], ranges$xlim[2], length=n)
  y <- seq(ranges$ylim[1], ranges$ylim[2], length=n)
  z <- outer(x,y,f)
  surface3d(x, y, z, ...)
}

library (rgl)
plot3d(Perch$Length, Perch$Width, Perch$Weight, type="p", col="red", 
       xlab="Length", ylab="Width", zlab="Weight", site=5, lwd=15)

coeffs = fit3.per$coefficients
f = function (x1, x2) {
  # x1 is length, x2 is width
  coeffs[1] + coeffs[2]*x1 + coeffs[3]*x2 + coeffs[4]*x1*x2
}

my_surface(f, alpha=.2 )

# Add a line at Length=20?

f2 = function (x1, x2) {
  # x1 is length, x2 is width
  coeffs[1] + coeffs[2]*20 + coeffs[3]*x2 + coeffs[4]*20*x2
}

my_surface(f2, alpha=.2, col='blue' )

# Guessing IQ

data ("IQGuessing")
cor (IQGuessing)
fit.iq = lm (TrueIQ ~ GuessIQ*Age, data=IQGuessing)
summary (fit.iq)
par (mfrow=c(1,2))
plot (fit.iq, which=1:2)
plot (TrueIQ ~ predict (fit.iq), data=IQGuessing, xlab="Fitted TrueIQ")
abline (0, 1)

fit.iq2 = lm (TrueIQ ~ GuessIQ + Age, data=IQGuessing)
summary (fit.iq2)
par (mfrow=c(1,2))
plot (fit.iq2, which=1:2)
plot (TrueIQ ~ predict (fit.iq), data=IQGuessing, xlab="Fitted TrueIQ")
abline (0, 1)

# Germany CO2 data

data ("CO2Germany")
names(CO2Germany)
CO2Germany$daysq = CO2Germany$Day^2 
CO2Germany$day.c = CO2Germany$Day - mean (CO2Germany$Day)
CO2Germany$daysq.c = CO2Germany$day.c^2 
CO2Germany$daycb = CO2Germany$Day^3
CO2Germany$daycb.c = CO2Germany$day.c^3

fit.co = lm (CO2 ~ Day + daysq, data=CO2Germany)
summary (fit.co)

fit.co2 = lm (CO2 ~ day.c + daysq.c, data=CO2Germany)
summary (fit.co2)

fit.co3 = lm (CO2 ~ Day + daysq + daycb, data=CO2Germany)
summary (fit.co3)

fit.co4 = lm (CO2 ~ day.c + daysq.c + daycb.c, data=CO2Germany)
summary (fit.co4)

data ("FunnelDrop")
plot (FunnelDrop)
FunnelDrop$Funnelsq = FunnelDrop$Funnel^2
FunnelDrop$Tubesq = FunnelDrop$Tube^2
fit.fd = lm (Time ~ Funnel + Tube + Funnelsq + Tubesq + Funnel:Tube, data=FunnelDrop)
summary (fit.fd)
plot (Time ~ predict (fit.fd), data=FunnelDrop, xlab="Fitted Time")
abline (0, 1)


data ("HousesNY")
summary (lm (Price ~ Size, data=HousesNY))
summary (lm (Price ~ Beds, data=HousesNY))
fit.ny1 = lm (Price ~ Size + Beds, data=HousesNY)
summary (fit.ny1)
plot (Price ~ predict (fit.ny1), data=HousesNY, xlab="Fitted Price")
abline (0, 1)

fit.ny2 = lm (Price ~ Size + Beds + Baths + Lot, data=HousesNY)
summary (fit.ny2)
plot (Price ~ predict (fit.ny2), data=HousesNY, xlab="Fitted Price")
abline (0, 1)

fit.ny3 = step (fit.ny2)

fit.ny4 = lm (Price ~ Size + Baths + Lot, data=HousesNY)
summary (fit.ny4)

fit.ny5 = lm (Price ~ Size + Baths, data=HousesNY)
summary (fit.ny5)
plot (fit.ny5, which=1:2)
plot (Price ~ predict (fit.ny5), data=HousesNY, xlab="Fitted Price")
abline (0, 1)

# need car package
car::vif(fit.ny2)

data ("CountyHealth")
fit.ch = lm (sqrt(MDs) ~ Hospitals * Beds, data=CountyHealth)
summary (fit.ch)
car::vif(fit.ch)

fit.ch2 = lm (sqrt(MDs) ~ Hospitals + Beds, data=CountyHealth)
summary (fit.ch2)
car::vif(fit.ch2)

CountyHealth$Hospitals.c = CountyHealth$Hospitals - mean (CountyHealth$Hospitals)
CountyHealth$Beds.c = CountyHealth$Beds - mean (CountyHealth$Beds)

fit.ch3 = lm (sqrt(MDs) ~ Hospitals.c * Beds.c, data=CountyHealth)
summary (fit.ch3)
car::vif(fit.ch3)

plot (predict (fit.ch), predict (fit.ch3))
