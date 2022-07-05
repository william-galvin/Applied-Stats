###############################
#"filter" data

install.packages("reticulate") 
library(reticulate) #gets R to talk to python
attach(countries) #NAME OF DATA GOES HERE
source_python("data_filter.py") #put name of python file here (I think the .py file should
View(countries) 

filter_data = function(data_set) {
  indexes =  seq(1, length(names(data_set)), by = 2)
  common_names = py$getCommonNames(data_set[[1]], data_set[[3]])
  for (i in indexes) {
    common_names = py$getCommonNames(common_names, data_set[[i]])
  }
  final_data_frame = data.frame(common_names)
  names(final_data_frame)[1] = "Name"
  for (i in indexes){
    country_names = data_set[[i]]
    data = data_set[[i+1]]
    filtered_data = py$filterData(common_names, country_names, data)
    final_data_frame[names(data_set)[i+1]] = filtered_data
  }
  return(final_data_frame)
}

new_countries = filter_data(countries)

View(new_countries) 
###############################


###############################
#calculate and show correlations 
attach(countries)
correlations = data.frame(matrix(ncol = 2, nrow = 0))
col_names = c("Predictor", "Multiple R Squared")
colnames(correlations) = col_names

vals = seq(3, ncol(countries)-1,  2)

for (i in vals) {
  filtered_happiness = py$filterData(py$getCommonNames(countries[[1]], countries[[i]]), countries[[1]], countries[[2]])
  filtered_other = py$filterData(py$getCommonNames(countries[[1]], countries[[i]]), countries[[i]], countries[[i+1]])
  filtered_happiness = filtered_happiness[!is.na(filtered_happiness)]
  filtered_other = filtered_other[!is.na(filtered_other)]
  r2 = summary(lm(filtered_happiness~filtered_other))$r.squared
  new_df = data.frame(names(countries)[i+1], r2)
  correlations[nrow(correlations)+1, ] = new_df
  print(i)
}

correlations
names(correlations)[1] = "Predictor"
names(correlations)[2] = "Multiple R Squared"

View(correlations)
###############################







###############################
#add gdp per capita, population density, electricity consumption per cap to countries table
attach(countries)
density_names = py$getCommonNames(CountryPop, Country.Area)
pop = py$filterData(density_names, CountryPop, countries[[6]])
area = py$filterData(density_names, Country.Area, countries[[12]])
density = pop/area

for (i in range(length(density)+1, 241)) {
  density[i] = NA
}

for (i in range(length(density_names)+1, 241)) {
  density_names[i] = NA
}

countries["Country.Density"] <- density_names
countries["Population.Density"] <- density


gdp_names = py$getCommonNames(Country.GDP, CountryPop)
gdp = py$filterData(gdp_names, Country.GDP, countries[[10]])
pop = py$filterData(gdp_names, CountryPop, countries[[6]])
gdpp = gdp/pop

for (i in range(length(gdp_names)+1, 241)) {
  gdp_names[i] = NA
}

for (i in range(length(gdpp)+1, 241)) {
  gdpp[i] = NA
}

countries["Country.GDP.per.cap"] <- gdp_names
countries["GDP.per.capita"] <- gdpp

elec_names = py$getCommonNames(Country.Electricity, CountryPop)
elec = py$filterData(elec_names, Country.Electricity, countries[[14]])
pop = py$filterData(elec_names, CountryPop, countries[[6]])
elecpc = elec/pop

for (i in range(length(elec_names)+1, 241)) {
  elec_names[i] = NA
}

for (i in range(length(elecpc)+1, 241)) {
  elecpc[i] = NA
}

countries["Country.Electricity.per.cap"] <- elec_names
countries["Electricity.per.capita"] <- elecpc

###############################






###############################
#get all data in a presentable manner, with unavailable data as NA
present_all_data = function (data_set) {
  names = data_set[[1]]
  final_data_frame = data.frame(names) # df with names of countries from fist row
  indexes =  seq(1, length(names(data_set)), by = 2) #gets the cols with the country names
  names(final_data_frame)[1] = "Country"
  for (i in indexes) {
    country_names = data_set[[i]]
    data = data_set[[i+1]]
    new_data = py$getNewData(names, country_names, data)
    final_data_frame[names(data_set)[i+1]] = data.frame(matrix(unlist(new_data)))
  }
  return(final_data_frame)
}

present_data = present_all_data(countries)

View(present_data)

###Export as csv
write.csv(present_data, "Export.csv", row.names = F)



###############################


###############################
#t-tests

###GDP 
gdp_table = subset(countries, select = c(1,2, 75,76))

gdp_happy_names = py$getCommonNames(gdp_table[[1]],gdp_table[[3]])
gdp_happy_names = data.frame(gdp_happy_names)[c(1:136), ]

gdp_happy_vals = py$filterData(gdp_happy_names, countries[[75]], countries[[76]])
gdp_happy = py$filterData(gdp_happy_names, countries[[1]], countries[[2]])

gdp_table = data.frame(gdp_happy_names, gdp_happy, gdp_happy_vals)
names(gdp_table) = c("Country", "Happy", "GDPPC")

vect = vector()
for (i in seq(nrow(gdp_table))) {
  val = gdp_table[i,3]
  if (val > median(gdp_table$GDPPC)) {
    vect = append(vect, "High")
  } else {
    vect = append(vect, "Low")
  }
}

gdp_table["rank"] = vect

t.test(gdp_table$Happy~gdp_table$rank, alternative = "greater", detailed = T)    #sd(subset(gdp_table, gdp_table[4] == "High")[[3]])
qqnorm(rstandard(lm(gdp_table$Happy~gdp_table$GDPPC)), 
       ylab = "Standardized Residuals", xlab = "Normal Happiness Scores", main = "Figure 1.1: Normal Probability Plot of Happiness\nas a Function of GDP per Capita",
       col = "black", pch = 19, cex.lab = 1.25, cex.main = 1.25)
qqline(rstandard(lm(gdp_table$Happy~gdp_table$GDPPC)))

boxplot(gdp_table$Happy~gdp_table$rank,
        xlab = "Happiness Scores", ylab = "Group", main = "Happiness vs GDP per Capita Group", cex.lab = 1.25, cex.main = 1.25,
        col = "white", horizontal = T)
###
###M/D ratio
md_table = subset(countries, select = c(1,2, 67,68))

md_names = py$getCommonNames(md_table[[1]],md_table[[3]])
md_names = data.frame(md_names)[c(1:85), ]

md_vals = py$filterData(md_names, countries[[67]], countries[[68]])
md_happy = py$filterData(md_names, countries[[1]], countries[[2]])

md_table = data.frame(md_names, md_happy, md_vals)
names(md_table) = c("Country", "Happy", "MD")

vect = vector()
for (i in seq(nrow(md_table))) {
  val = md_table[i,3]
  if (val > median(md_table$MD)) {
    vect = append(vect, "High")
  } else {
    vect = append(vect, "Low")
  }
}

md_table["rank"] = vect

t.test(md_table$Happy~md_table$rank, alternative = "less", detailed = T)
qqnorm(rstandard(lm(md_table$Happy~md_table$MD)), 
       ylab = "Standardized Residuals", xlab = "Normal Happiness Scores", main = "Figure 1.2: Normal Probability Plot of Happiness\nas a Function of Marriage to Divorce Ratio", 
       pch = 19, cex.lab = 1.25, cex.main = 1.25)
qqline(rstandard(lm(md_table$Happy~md_table$MD)))

boxplot(md_table$Happy~md_table$rank,
        xlab = "Happiness Scores", ylab = "Group", main = "Happiness vs Marriage to Divorce Ratio Group", cex.lab = 1.25, cex.main = 1.25,
        col = "white", horizontal = T)
###
### Gini
gini_table = subset(countries, select = c(1,2, 15,16))

gini_names = py$getCommonNames(gini_table[[1]],gini_table[[3]])
gini_names = data.frame(gini_names)[c(1:128), ]

gini_vals = py$filterData(gini_names, countries[[15]], countries[[16]])
gini_happy = py$filterData(gini_names, countries[[1]], countries[[2]])

gini_table = data.frame(gini_names, gini_happy, gini_vals)
names(gini_table) = c("Country", "Happy", "Gini")

vect = vector()
for (i in seq(nrow(gini_table))) {
  val = gini_table[i,3]
  if (val > median(gini_table$Gini)) {
    vect = append(vect, "High")
  } else {
    vect = append(vect, "Low")
  }
}

gini_table["rank"] = vect

t.test(gini_table$Happy~gini_table$rank, alternative = "less", detailed = T)
qqnorm(rstandard(lm(gini_table$Happy~gini_table$Gini)), 
       ylab = "Standardized Residuals", xlab = "Normal Happiness Scores", main = "Figure 1.3: Normal Probability Plot of Happiness\nas a Function of Gini Index", 
       pch = 19, cex.lab = 1.25, cex.main = 1.25)
qqline(rstandard(lm(gini_table$Happy~gini_table$Gini)))

boxplot(gini_table$Happy~gini_table$rank,
        xlab = "Happiness Scores", ylab = "Group", main = "Happiness vs Gini Index Group", cex.lab = 1.25, cex.main = 1.25,
        col = "white", horizontal = T)
###
### education
ed_table = subset(countries, select = c(1,2, 31,32))

ed_names = py$getCommonNames(ed_table[[1]],ed_table[[3]])
ed_names = data.frame(ed_names)[c(1:142), ]

ed_vals = py$filterData(ed_names, countries[[31]], countries[[32]])
ed_happy = py$filterData(ed_names, countries[[1]], countries[[2]])

ed_table = data.frame(ed_names, ed_happy, ed_vals)
names(ed_table) = c("Country", "Happy", "EI")

vect = vector()
for (i in seq(nrow(ed_table))) {
  val = ed_table[i,3]
  if (val > median(ed_table$EI)) {
    vect = append(vect, "High")
  } else {
    vect = append(vect, "Low")
  }
}

ed_table["rank"] = vect

t.test(ed_table$Happy~ed_table$rank)
qqnorm(rstandard(lm(ed_table$Happy~ed_table$EI)), 
       ylab = "Standardized Residuals", xlab = "Normal Happiness Scores", main = "Figure 1.4: Normal Probability Plot of Happiness\nas a Function of Education Index", 
       pch = 19, cex.lab = 1.25, cex.main = 1.25)
qqline(rstandard(lm(ed_table$Happy~ed_table$EI)))

boxplot(ed_table$Happy~ed_table$rank,
        xlab = "Happiness Scores", ylab = "Group", main = "Happiness vs Education Index Group", cex.lab = 1.25, cex.main = 1.25,
        col = "white", horizontal = T)
###
### economic freedom
free_table = subset(countries, select = c(1,2, 17,18))

free_names = py$getCommonNames(free_table[[1]],free_table[[3]])
free_names = data.frame(free_names)[c(1:142), ]

free_vals = py$filterData(free_names, countries[[17]], countries[[18]])
free_happy = py$filterData(free_names, countries[[1]], countries[[2]])

free_table = data.frame(free_names, free_happy, free_vals)
names(free_table) = c("Country", "Happy", "Freedom")

vect = vector()
for (i in seq(nrow(free_table))) {
  val = free_table[i,3]
  if (val > median(free_table$Freedom)) {
    vect = append(vect, "High")
  } else {
    vect = append(vect, "Low")
  }
}

free_table["rank"] = vect

t.test(free_table$Happy~free_table$rank, alternative = "greater")
qqnorm(rstandard(lm(free_table$Happy~free_table$Freedom)), 
       ylab = "Standardized Residuals", xlab = "Normal Happiness Scores", main = "Figure 1.5: Normal Probability Plot of Happiness\nas a Function of Index of Economic Freedom", 
       pch = 19, cex.lab = 1.25, cex.main = 1.25)
qqline(rstandard(lm(free_table$Happy~free_table$Freedom)))

boxplot(free_table$Happy~free_table$rank,
        xlab = "Happiness Scores", ylab = "Group", main = "Happiness vs Index of Economic Freedom Group", cex.lab = 1.25, cex.main = 1.25,
        col = "white", horizontal = T)
###
### democracy
dem_table = subset(countries, select = c(1,2, 47,48))

dem_names = py$getCommonNames(dem_table[[1]],dem_table[[3]])
dem_names = data.frame(dem_names)[c(1:146), ]

dem_vals = py$filterData(dem_names, countries[[47]], countries[[48]])
dem_happy = py$filterData(dem_names, countries[[1]], countries[[2]])

dem_table = data.frame(dem_names, dem_happy, dem_vals)
names(dem_table) = c("Country", "Happy", "Dem")

vect = vector()
for (i in seq(nrow(dem_table))) {
  val = dem_table[i,3]
  if (val > median(dem_table$Dem)) {
    vect = append(vect, "High")
  } else {
    vect = append(vect, "Low")
  }
}

dem_table["rank"] = vect

t.test(dem_table$Happy~dem_table$rank)
qqnorm(rstandard(lm(dem_table$Happy~dem_table$Dem)), 
       ylab = "Standardized Residuals", xlab = "Normal Happiness Scores", main = "Figure 1.6: Normal Probability Plot of Happiness\nas a Function of Democracy Index", 
       pch = 19, cex.lab = 1.25, cex.main = 1.25)
qqline(rstandard(lm(dem_table$Happy~dem_table$Dem)))

boxplot(dem_table$Happy~dem_table$rank,
        xlab = "Happiness Scores", ylab = "Group", main = "Happiness vs Democracy Index Group", cex.lab = 1.25, cex.main = 1.25,
        col = "white", horizontal = T)
###
### SPI
spi_table = subset(countries, select = c(1,2, 59, 60))
spi_names = py$getCommonNames(spi_table[[1]],spi_table[[3]])
spi_names = data.frame(spi_names)[c(1:138), ]
spi_vals = py$filterData(spi_names, countries[[59]], countries[[60]])
spi_happy = py$filterData(spi_names, countries[[1]], countries[[2]])
spi_table = data.frame(spi_names, spi_happy, spi_vals)
names(spi_table) = c("Country", "Happy", "SPI")
###
###life expectacy
life_table = subset(countries, select = c(1,2, 41, 42))
life_names = py$getCommonNames(life_table[[1]],life_table[[3]])
life_names = data.frame(life_names)[c(1:141), ]
life_vals = py$filterData(life_names, countries[[41]], countries[[42]])
life_happy = py$filterData(life_names, countries[[1]], countries[[2]])
life_table = data.frame(life_names, life_happy, life_vals)
names(life_table) = c("Country", "Happy", "Life")


##############################
#Finding best multiple regressions
regres_countries = countries[, c(1,2,41,42,59,60, 31,32, 75, 76)]
regres_countries = filter_data(regres_countries)
attach(regres_countries)

library(olsrr)
model <- lm(formula = Happiness.Score~Social.progress.index*Life.Expectancy.at.Birth*GDP.per.capita*Education.Index)

ptm <- proc.time()
table3 <- ols_step_all_possible(model)
proc.time() - ptm


models3 <- subset(table3, select = c("rsquare", "adjr", "predictors"))
View(models3)

regres = lm(Happiness.Score~Social.progress.index+HDI+GDP.per.capita+Education.Index+Social.progress.index:HDI
            +HDI:GDP.per.capita+Social.progress.index:Education.Index+Social.progress.index:HDI:GDP.per.capita+
              Social.progress.index:HDI:Education.Index+Social.progress.index:GDP.per.capita:Education.Index+HDI:GDP.per.capita:Education.Index)
summary(regres)

regres2 = lm(Happiness.Score~GDP.per.capita+Education.Index+Social.progress.index:GDP.per.capita+Life.Expectancy.at.Birth:GDP.per.capita+Social.progress.index:Education.Index+Life.Expectancy.at.Birth:Education.Index+GDP.per.capita:Education.Index+Social.progress.index:Life.Expectancy.at.Birth:GDP.per.capita+Social.progress.index:Life.Expectancy.at.Birth:Education.Index+Social.progress.index:GDP.per.capita:Education.Index+Life.Expectancy.at.Birth:GDP.per.capita:Education.Index+Social.progress.index:Life.Expectancy.at.Birth:GDP.per.capita:Education.Index
)
summary(regres2)


##############################



##############################
#GGPLOTS
library(ggplot2)
library(extrafont)
library(scales)

#Scatter plot theme
scatter_theme = theme_minimal() + theme(
  text = element_text(family = "Times New Roman"),
  plot.title = element_text(face = "bold", hjust = .5, size = 20, vjust = 0),
  axis.title = element_text(size = 15),
  axis.text = element_text(size = 12), 
  plot.background = element_rect(color = "black")
)

heat_theme = theme_void() + theme(
  plot.title = element_text(face = "bold", hjust = .5, size = 20, vjust = 0),
  text = element_text(family = "Times New Roman"),
  plot.background = element_rect(color = "black"),
  legend.position = c(.93, .58)
)

#GDP per capita
gdp_col = "#3e9c35"
ggplot(data  = gdp_table, aes(x = GDPPC, y = Happy)) + 
  geom_point(color = gdp_col) + geom_smooth(method = lm, se = T, size = .5, color = gdp_col, fill = "gray85")+
  labs(title = "GDP per Capita vs Happiness", x = "GDP per Capita (2018 Internaitonal $)", y = 'Happiness Score') +
  scale_x_continuous(labels = comma)+
  scatter_theme


#Education Index
ed_color = "#7291f9"
ggplot(data = ed_table, aes(x = EI, y = Happy)) +
  geom_point(color = ed_color) + geom_smooth(method = lm, size = .5, color = ed_color, fill = "gray85") +
  labs(title = "Education Index vs Happiness", x = "Education Index", y = 'Happiness Score') +
  scatter_theme

#SPI
spi_col = "#ff6d00"
ggplot(data = spi_table, aes(x = SPI, y = Happy)) +
  geom_point(color = spi_col) + geom_smooth(method = lm, size = .5, color = spi_col, fill = "gray85") +
  labs(title = "Social Progess Index vs Happiness", x = "SPI", y = 'Happiness Score') +
  scatter_theme

#Life expectancy
life_col = "#b5179e"
ggplot(data = life_table, aes(x = Life, y = Happy)) +
  geom_point(color = life_col) + geom_smooth(method = lm, size = .5, color = life_col, fill = "gray85") +
  labs(title = "Life Expectancy vs Happiness", x = "Life Expectancy (years)", y = 'Happiness Score') +
  scatter_theme



####
#Heat maps

library(tidyverse)
library(rvest)
library(magrittr)
library(ggmap)
library(stringr)
library(maps)

life_table$Country <- recode(life_table$Country
                            ,'United States' = 'USA'
                           ,'United Kingdom' = 'UK'
)


#happiness
map <- map_data("world")
map <- left_join(map, gdp_table, by = c("region" = "Country"))
ggplot() +
  geom_polygon(data = map, aes(x = long, y = lat, group = group, fill = Happy))+
  ylim(-60,90) +
  labs(title = "Happiness Scores")+
  heat_theme+ 
  scale_fill_gradientn(name = "Score", colors = c("white", "#ffea83", "#ff7b00"), guide="colorbar",na.value="gray85", labels = comma)

#GDP
map <- map_data("world")
map <- left_join(map, gdp_table, by = c("region" = "Country"))
ggplot() +
  geom_polygon(data = map, aes(x = long, y = lat, group = group, fill = GDPPC))+
  ylim(-60,90) +
  labs(title = "GDP per Capita")+
  heat_theme+ 
  scale_fill_gradientn(name = "GDP per\nCapita", colors = c("white", "#7cb518","#134611"), guide="colorbar",na.value="gray85", labels = comma)

#education
map <- map_data("world")
map <- left_join(map, ed_table, by = c("region" = "Country"))
ggplot() +
  geom_polygon(data = map, aes(x = long, y = lat, group = group, fill = EI))+
  ylim(-60,90) +
  labs(title = "Education Index")+
  heat_theme+ 
  scale_fill_gradientn(colors = c("white","#4cc9f0", "#3a0ca3"), guide="colorbar",na.value="gray85", labels = comma)

#spi
map <- map_data("world")
map <- left_join(map, spi_table, by = c("region" = "Country"))
ggplot() +
  geom_polygon(data = map, aes(x = long, y = lat, group = group, fill = SPI))+
  ylim(-60,90) +
  labs(title = "Social Progress Index")+
  heat_theme+ 
  scale_fill_gradientn(colors = c("white","#fbc4ab", "#d00000"), guide="colorbar",na.value="gray85", labels = comma)

#life
map <- map_data("world")
map <- left_join(map, life_table, by = c("region" = "Country"))
ggplot() +
  geom_polygon(data = map, aes(x = long, y = lat, group = group, fill = Life))+
  ylim(-60,90) +
  labs(title = "Life Expectancy")+
  heat_theme+ 
  scale_fill_gradientn(name = "Life\nExpectancy",colors = c("white","#d2b7e5", "#8900f2"), guide="colorbar",na.value="gray85", labels = comma)



####
attach(regres_countries)
regres2 = lm(Happiness.Score~
               GDP.per.capita+
               Social.progress.index:Life.Expectancy.at.Birth+
               Social.progress.index:Education.Index+
               Social.progress.index:Life.Expectancy.at.Birth:GDP.per.capita+
               Social.progress.index:Life.Expectancy.at.Birth:Education.Index+
               Life.Expectancy.at.Birth:GDP.per.capita:Education.Index+
               Social.progress.index:Life.Expectancy.at.Birth:GDP.per.capita:Education.Index
)
summary(regres2)
#rediual and qq plots
attach(regres_countries)
plot(Happiness.Score, resid(regres2), main = "Figure 2.1:\nResidual Plot of Mulitple Linear Regression", ylab = "Residual", xlab = "Happiness Score", , pch = 19, col = "black")
abline(0,0, col = "black")


qqnorm(rstandard(regres2), 
       ylab = "Standardized Residuals", xlab = "Normal Happiness Scores", main = "Figure 2.2: Normal Probability Plot of\nMulitple Linear Regression",
       col = "black", pch = 19, cex.lab = 1.25, cex.main = 1.25)
qqline(rstandard(regres2))




#Standardized Slope plot
#visuallizing multiple regression model with a standardized slope plot https://stats.stackexchange.com/questions/89747/how-to-describe-or-visualize-a-multiple-linear-regression-model


attach(regres_countries)

z_happy <- (Happiness.Score - mean(Happiness.Score))/sd(Happiness.Score)
z_happy

z_gdp <- (GDP.per.capita - mean(GDP.per.capita))/sd(GDP.per.capita)
z_gdp

z_ed <- (Education.Index - mean(Education.Index))/sd(Education.Index)
z_ed

z_spi <- (Social.progress.index - mean(Social.progress.index))/sd(Social.progress.index)
z_spi

z_life <- (Life.Expectancy.at.Birth -mean(Life.Expectancy.at.Birth))/sd(Life.Expectancy.at.Birth)
z_life

z_1 <- (Social.progress.index*Life.Expectancy.at.Birth - mean(Social.progress.index*Life.Expectancy.at.Birth))/sd(Social.progress.index*Life.Expectancy.at.Birth)
z_2 <- (Social.progress.index*Education.Index - mean(Social.progress.index*Education.Index))/sd(Social.progress.index*Education.Index)
z_3 <- ( Social.progress.index*Life.Expectancy.at.Birth*GDP.per.capita - mean(Social.progress.index*Life.Expectancy.at.Birth*GDP.per.capita))/sd(Social.progress.index*Life.Expectancy.at.Birth*GDP.per.capita)
z_4 <- ( Social.progress.index*Life.Expectancy.at.Birth*Education.Index - mean(Social.progress.index*Life.Expectancy.at.Birth*Education.Index))/sd(Social.progress.index*Life.Expectancy.at.Birth*Education.Index)
z_5 <- ( Life.Expectancy.at.Birth*GDP.per.capita*Education.Index - mean(Life.Expectancy.at.Birth*GDP.per.capita*Education.Index))/sd(Life.Expectancy.at.Birth*GDP.per.capita*Education.Index)
z_6 <- ( Social.progress.index*Life.Expectancy.at.Birth*GDP.per.capita*Education.Index - mean(Social.progress.index*Life.Expectancy.at.Birth*GDP.per.capita*Education.Index))/sd(Social.progress.index*Life.Expectancy.at.Birth*GDP.per.capita*Education.Index)

z_regresion <- (lm(formula = z_happy~z_gdp + z_1 + z_2 + z_3 + z_4 + z_5 + z_6))
summary(z_regresion)

coefs <- c(4.837e+00, 7.929e-01,  -1.854e+00, -5.985e+00, 1.696e+00, -5.673e+00, 7.092e+00)
names <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7")
errors = c(1.891e+00, 3.637e-01, 8.049e-01, 1.932e+00, 1.102e+00, 3.050e+00, 2.844e+00)
test <- data.frame(coefs, names, errors)

ggplot(data = test, aes(x = coefs, y = names)) + 
  geom_point(show.legend = FALSE, size = 3) + 
  geom_segment(aes(x = 0, y = .75, xend = 0, yend = 8), lwd = .75, size = .5, color = "gray70", linetype = "dashed")+
  geom_errorbarh( show.legend = FALSE, aes(xmin = coefs - errors, xmax = coefs + errors), height = .25, lwd = .75) +
  scale_x_continuous(limits = c(-10, 10)) +
  scale_y_discrete(labels = c(expression('x'[1]), expression('x'[5]), expression('x'[6]), expression('x'[7]), expression('x'[8]), expression('x'[9]), expression('x'[10])))+
  labs(title = "Standardized Slope Plot", y = "", x = expression(paste("Standardized Slope (", Delta, "Y/", Delta, "sd(X))"))) + 
  scatter_theme+
  theme(axis.text.y = element_text(size = 30)) 






