#Tidy Tuesday 13
#Emily Xing

library(readr)
library(ggplot2)
library(gapminder)
library(tidyverse)
library(ggmap)
library(ggpubr)
library(ggthemes)
library(corrplot)



week13_alcohol_global <- read_csv("data-science/Tidy Tuesday 13/week13_alcohol_global.csv")
alcohol <- week13_alcohol_global
View(alcohol)

#Alcohol dependence by % data
alcohol_dependence_data <- read_csv("data-science/alcohol_dependence_data.csv")
dependence <- alcohol_dependence_data

colnames(dependence) <- dependence[1,]
dependence <- dependence[-1,]

#Cleaning data
dependence <- dependence %>%
  separate(`Both sexes`, into = c("Both", "CI"), " ") %>%
  separate(Male, into = c("Male", "CI"), " ") %>%
  separate(Female, into = c("Female", "CI"), " ") %>%
  gather(Both, Female, Male, key = Gender, value = Prevalence)

colnames(dependence) <- c("country", "year", "CI", "Gender", "Prevalence")

#Clean data
alcohol <- alcohol %>%
  gather(beer_servings, spirit_servings, wine_servings, key = Alcohol, value = Servings) 

alcohol$Alcohol <- alcohol$Alcohol %>%
  str_replace("beer_servings", "beer") %>%
  str_replace("wine_servings", "wine") %>%
  str_replace("spirit_servings", "spirit")

#Filter for year 2007
gap2007 <- filter(gapminder, year == 2007)

View(gap2007)

#Combine gapminder and alcohol data
countries <- left_join(alcohol, gap2007)
countries <- left_join(countries, dependence, by = "country")
countries$Prevalence <- as.numeric(countries$Prevalence)
countries$total <- countries$total_litres_of_pure_alcohol


ggplot(countries, aes(x=gdpPercap ,y = total_litres_of_pure_alcohol)) + 
  geom_smooth(method = 'lm', formula = y~x) + 
  geom_point(aes(colour = continent)) +
  ylab("Total litres of pure alcohol per person") + xlab("GDP per capita")

longlat <- geocode(countries$country)

ggplot(countries, aes(x=total_litres_of_pure_alcohol, y=lifeExp)) + 
  geom_smooth(method = 'lm', formula = y~x) + 
  geom_point(aes(colour = continent))

countries <- na.omit(countries)

ggplot(countries, aes(x= total_litres_of_pure_alcohol, y=as.numeric(Prevalence))) + 
  geom_smooth(method = 'lm', formula = y~x) + 
  geom_point(aes(colour = continent))

#Correlation plot using spearman's correlation
ggscatter(countries, 
          x="total", 
          y="Prevalence",
          cor.coef = FALSE, 
          conf.int = TRUE, 
          add= "reg.line", 
          cor.method = "spearman")

countries.subset <- select(countries, total, 
                           Prevalence, lifeExp, pop, gdpPercap)
countries.subset <- na.omit(countries.subset)

  
countries.cor <- cor(countries.subset)
colnames(countries.subset) <- c("Total servings", 
                                "Dependence", "Life Expectancy", 
                                "Population", "Gdp per capita")
alcohol_plot <- corrplot(countries.cor, 
         type = "upper", order = "hclust", tl.col = "black",
         addCoef.col = "black",
         tl.srt = 45)
mtext("Correlations between country characteristics and alcohol consumption", at=3, line=-0.1, cex=1.5)

title <- "Correlations between country characteristics and alcohol consumption"


