## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(fma)
library(ggplot2)
library(dplyr)

## -----------------------------------------------------------------------------
beer %>%
  autoplot() +
  ggtitle("Monthly Australian Beer Production") +
  xlab("Year") +
  ylab("Megalitres") +
  labs(caption = "Figure 2-1: Time plot of monthly Australian beer production (megaliters, Ml) from January 1991–August 1995.")

## -----------------------------------------------------------------------------
ggseasonplot(beer, col = rainbow(5), year.labels = TRUE) +
  ggtitle('Monthly Australian Beer Production') +
  xlab('Months') + ylab('Megalitres') +
  labs(caption = 'Figure 2-2: A seasonal plot of the Australian beer production data. Note that production peaks in
    November and December in preparation for the southern hemisphere summer and is least in winter.')

## -----------------------------------------------------------------------------
auto %>%
  ggplot(aes(x = Mileage, y = Price)) +
  geom_point() +
  xlab("Mileage (mpg)") + ylab("Price ($US)") +
  ggtitle("Price/Mileage Relationship for 45 Automobiles") +
  labs(caption = 'Figure 2-3: A scatterplot of price versus mileage for the automobile data.')

## -----------------------------------------------------------------------------
auto %>%
  ggplot(aes(x = Mileage, y = Price, shape=Country)) +
  geom_point(size=2) +
  xlab("Mileage (mpg)") + ylab("Price ($US)") +
  ggtitle("Price/Mileage Relationship for 45 Automobiles") +
  labs(caption = 'Figure 2-4: A scatterplot showing price, mileage, and the country of origin for the automobile data.')

## ----warning=FALSE------------------------------------------------------------
auto_japan <- auto %>%
  filter(Country == 'Japan')
auto_japan

## -----------------------------------------------------------------------------
auto_japan %>%
  summarise(mean = mean(Mileage),
            median= median(Mileage),
            MAD = sum(abs(Mileage - mean(Mileage)))/n(),
            MSD = sum((Mileage - mean(Mileage))^2)/n(),
            Variance = var(Mileage),
            Std_Dev = sd(Mileage))

## -----------------------------------------------------------------------------
auto_japan %>%
  mutate(Price = Price/1000) %>%
  summarise(mean_milage = mean(Mileage),
            mean_price = mean(Price),
            sd_mileage = sd(Mileage),
            sd_price = sd(Price),
            covariance = cov(Price, Mileage),
            correlation = cor(Price, Mileage))

## -----------------------------------------------------------------------------
ggAcf(beer) +
  ggtitle('ACF of Beer Production') +
  labs(caption = 'Figure 2-6: The correlogram (or ACF plot) for the beer production data.')

## -----------------------------------------------------------------------------
window(beer, start=c(1994,12)) %>%
  naive() %>%
  accuracy()

## ---- fig.show="hold"---------------------------------------------------------
beer %>% naive() %>% residuals() -> e
autoplot(e) + ggtitle("Errors from NF1 forecasts")
ggAcf(e) + ggtitle("") +
  labs(caption="Figure 2-7: Top: Forecast errors obtained by applying the NF1 method to the beer data.
    Bottom: The ACF of the forecast errors.")

## -----------------------------------------------------------------------------
elec %>%
  autoplot() +
  ggtitle("Australian Monthly Electricity Production") +
  xlab("Year") + ylab("million kWh") +
  labs(caption = "Figure 2-10: Monthly Australian electricity production from January 1956 to August 1995.
       Note the increasing variation as the level of the series increases.")

## -----------------------------------------------------------------------------
elec %>%
  sqrt() %>%
  autoplot() +
  ggtitle("Square Root of Electricity Production") +
  xlab("Year") + ylab("sqrt(million kWh)")

## ---- fig.height=9------------------------------------------------------------
cbind(
    `Square root` = sqrt(elec),
    `Cube root` = elec^(1/3),
    `Log` = log(elec),
    `Inverse` = -1/elec) %>%
  autoplot(facet=TRUE) +
    xlab("Year") +
    ggtitle("Transformations of the electricity production data") +
    labs(caption="Figure 2-11: Transformations of the electricity production data.")

## -----------------------------------------------------------------------------
cbind(
    Milk = milk,
    `Milk per day` = milk/monthdays(milk)
  ) %>%
  autoplot(facet=TRUE) +
  ggtitle("Monthly Milk Production per Cow") +
  xlab("Months") + ylab("Pounds") +
  labs(caption="Figure 2-12: Monthly milk production per cow over 14 years.
    The second graph shows the data adjusted for the length of the month.")

