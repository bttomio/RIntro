# R for Econometricians
# Replicating Eviews
# Andrew P. Blake

#
#  What we need to understand :
#    Most things are contained in specialist libraries which need loading
#    A data frame (tibble) is a bit like a spreadsheet page
#    If we organise our data we can use the tidyverse tools 
#       that operate on rows and columns
#    The plotting method we will use (ggplot2) uses a distinct 'grammar'; 
#       once understood, this is very powerful framework
#    There are powerful libraries of routines that we can use for many 
#       econometric methods - we don't really need anything else
#    We only need to understand a bit to be able to use machine learning on 
#       our data
#

## Read data

library(readxl)

fred_data <- read_xls("C:/Users/145607/Downloads/fredgraph.xls",
                      col_names = TRUE,
                      col_types = c("date", "numeric", "numeric"),
                      skip = 11) 
head(fred_data, 4)

## Plot it
library(ggplot2)

ggplot(fred_data) + 
  geom_line(aes(x=observation_date, y=UNRATE), color="red") # Plot one column
                                                            # v another
ggplot(fred_data) + 
  geom_line(aes(x=observation_date, y=UNRATE), color="red") + 
  geom_line(aes(x=observation_date, y=INDPRO), color="blue") 

#### Make it look different
library(ggthemes)

ggplot(fred_data) + 
  geom_line(aes(x=observation_date, y=UNRATE), color="red") + 
  geom_line(aes(x=observation_date, y=INDPRO), color="blue") + 
  theme_stata()

## Histograms etc

ggplot(fred_data) + 
  geom_histogram(aes(x=UNRATE), color="red", fill="pink", bins=25) + 
  theme_economist()

ggplot(fred_data) + 
  geom_histogram(aes(x=UNRATE, y=after_stat(density)), color="red", fill="pink") + 
  geom_density(aes(x=UNRATE), color="blue") + 
  theme_economist()

## Tidyverse proper (ggplot2 is actually loaded by this)
library(tidyverse)

fred_data <- fred_data %>%                               # A pipe! (magrittr)
  mutate(dindpro = 100*(INDPRO/lag(INDPRO, 12) - 1))     # Data transformation

library(lubridate)

fred_data %>% 
  filter(year(observation_date) > 1948) %>%              # Operations on rows
  ggplot() + 
  geom_line(aes(x=observation_date, y=UNRATE), color="red") + 
  theme_economist()

fred_data %>% 
  select(-INDPRO) %>%                                    # Column op
  filter(year(observation_date) > 1958)  %>%             # Row ops
  slice(-(1:11)) %>%                                      # Row ops
  pivot_longer(cols      = -c(observation_date),         # Column ops
               names_to  = "Vars", 
               values_to = "Vals") %>%
  ggplot() + 
  geom_line(aes(x=observation_date, y=Vals, group=Vars, color=Vars)) + 
  theme_stata() +
  labs(x="", y="")

## Regression

reg1 <- lm(UNRATE ~ dindpro, data = fred_data)
print(reg1)
summary(reg1)
plot(reg1)

logLik(reg1)
AIC(reg1)
nobs(reg1)

## Directly access library
lmtest::dwtest(reg1)

library(lmtest)
dwtest(reg1)
bgtest(reg1)
resettest(reg1)

library(car)
dwt(reg1)
outlierTest(reg1)

## IV

library(ivreg)

iv1 <- ivreg(UNRATE ~ dindpro | lag(dindpro), data=fred_data)
summary(iv1)

car::compareCoefs(reg1, iv1)
