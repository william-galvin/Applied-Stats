setwd("~/Documents/Applied Stats/Poster")
pairs(state_data[2:25], pch = 19)
attach(state_data)
library(extrafont)
library(ggplot2)
library(ggthemes)

plot(y = conservatives, x = prop.advanced..degree, pch = 19)
abline(lm(conservatives~prop.advanced..degree))


# X.white.prop, population.density, immigrant.prop, GDP.per.capita., prop.religious, prop.advanced..degree

regresion <- (lm(formula = conservatives~prop.advanced..degree*prop.religious*immigrant.prop + population.density ))  
# = summary(lm(formula = conservatives~prop.advanced..degree + prop.religious + immigrant.prop + population.density + prop.advanced..degree:prop.religious + prop.advanced..degree:immigrant.prop + prop.religious:immigrant.prop + prop.advanced..degree:prop.religious:immigrant.prop ))
summary(regression)


#residual plot
ggplot(data = state_data, aes(x= predict(regresion), y= resid(regresion)) ) + 
  geom_point(color = "gray90", size = 2.5) + 
  geom_segment(aes(x = .2, y = 0, xend = .8, yend = 0), lwd = 1.75, color = "gray50")+
  labs( #title= "Residual Plot" 
    x = "Predicted Conservative Proportion of State Legislators", y = "Residuals" )+
  theme_dark_square 


#normal probability plot
ggplot(data.frame(qq_data), aes(sample = qq_data))+
  geom_qq_line(color = "gray90", lwd = 1.75)+
  geom_qq(color = "gray90", size = 2.5 )+ 
  labs( #title= "Normal Probability Plot"
    x = "Theoretical Quantities", y = "Observed Quantities" )+
  theme_dark_square 



qqnorm(rstandard(regresion), main = "Normal Probability Plot",
       col = "#d90429", pch = 19)
qqline(rstandard(regresion), col = "#d90429")
abline(0,0, col = "lightgray")
abline(v = 0, col = "lightgray")


#find best model
library(olsrr)
model <- lm(formula = conservatives~prop.advanced..degree * immigrant.prop * prop.religious + population.density)
table <- ols_step_all_possible(model)
table1 <- subset(table, adjr > .7, select = c("adjr", "predictors"))
table1


#visuallizing multiple regression model with a standardized slope plot https://stats.stackexchange.com/questions/89747/how-to-describe-or-visualize-a-multiple-linear-regression-model

z_conservatives <- (conservatives - mean(conservatives))/sd(conservatives)
z_conservatives

z_degrees <- (prop.advanced..degree - mean(prop.advanced..degree))/sd(prop.advanced..degree)
z_degrees

z_religious <- (prop.religious - mean(prop.religious))/sd(prop.religious)
z_religious

z_immigrants <- (immigrant.prop - mean(immigrant.prop))/sd(immigrant.prop)
z_immigrants

z_density <- (population.density -mean(population.density))/sd(population.density)
z_density

z_regresion <- (lm(formula = z_conservatives~z_degrees*z_religious*z_immigrants + z_density ))
summary(z_regresion)

coefs <- c(--0.4377732, 0.5001412, -0.2751914, -0.2135454, -0.0324192, 0.2710492, 0.4604114, -0.2275922)
names <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8")
errors = c(0.1296660, 0.1205339, 0.1217003, 0.1176108, 0.0990041, 0.1221067, 0.1556324, 0.0838969)
test <- data.frame(coefs, names, errors)

#standardized slope plot
ggplot(data = test, aes(x = coefs, y = names, color = names)) + 
  geom_point(show.legend = FALSE, size = 4) + 
  geom_segment(aes(x = 0, y = .75, xend = 0, yend = 8), lwd = .75, size = .5, color = "gray70", linetype = "dashed")+
  geom_errorbarh( show.legend = FALSE, aes(xmin = coefs - errors, xmax = coefs + errors), height = .0, lwd = .75) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_y_discrete(labels = c(expression('x'[1]), expression('x'[2]), expression('x'[3]), expression('x'[4]), expression('x'[5]), expression('x'[6]), expression('x'[7]), expression('x'[8])))+
  labs(y = "", x = expression(paste("Standardized Slope (", Delta, "Y/", Delta, "sd(X))")))+
  scale_color_manual(values = c("#95d5b2", "#ffffb7", "#48cae4", "#e70e02", "gray90", "gray90", "gray90", "gray90"   ))+
  theme_dark +
  theme( 
    axis.text.y = element_text(size = 35)) 







#dark mode theme
theme_dark <- theme(text = element_text(family = "Times New Roman", color = "gray90"),
                    plot.title = element_text(face = "bold", hjust = .5, size = 30, vjust = 0),
                    plot.subtitle = element_text(hjust = .5),
                    axis.title = element_text(size = 30),
                    axis.text = element_text(size = 25, color = "gray80"),
                    legend.background = element_rect(fill = "#3A3839"),
                    legend.key = element_rect(fill = "#3A3839"),
                    panel.grid.major = element_line(colour = "gray25"), 
                    panel.grid.minor = element_line(colour = "gray25"),
                    plot.background = element_rect(fill = "#3A3839", color = "gray90"),
                    panel.background = element_rect(fill = "#3A3839",
                                                    color = "#3A3839",
                                                    size = 0.5, linetype = "solid"))


theme_dark_minimal <- theme(text = element_text(family = "Times New Roman", color = "gray90"),
                    plot.title = element_text(face = "bold", hjust = .5, vjust = 0, size = 30),
                    plot.subtitle = element_text(hjust = .5),
                    axis.title = element_text(size = 30),
                    axis.text = element_text(size = 25, color = "gray80"),
                    legend.background = element_rect(fill = "#3A3839"),
                    legend.key = element_rect(fill = "#3A3839"),
                    panel.grid.major = element_line(colour = "gray25"), 
                    panel.grid.minor = element_line(colour = "gray25"),
                    plot.background = element_rect(fill = "#3A3839", color = "#3A3839"),
                    panel.background = element_rect(fill = "#3A3839",
                                                    color = "#3A3839",
                                                    size = 0.5, linetype = "solid"))

theme_dark_heat <- theme(text = element_text(family = "Times New Roman", color = "gray90"),
                            plot.title = element_text(face = "bold", hjust = .5, size = 30),
                            plot.subtitle = element_text(hjust = .5),
                            legend.background = element_rect(fill = "#3A3839"),
                            legend.key = element_rect(fill = "#3A3839"),
                            panel.grid.major = element_line(colour = "gray25"), 
                            panel.grid.minor = element_line(colour = "gray25"),
                            plot.background = element_rect(fill = "#3A3839", color = "#3A3839"),
                            panel.background = element_rect(fill = "#3A3839",
                                                            color = "#3A3839",
                                                            size = 0.5, linetype = "solid"))


theme_dark_square <- theme(text = element_text(family = "Times New Roman", color = "gray90"),
                    plot.title = element_text(face = "bold", hjust = .5, size = 30, vjust = 0),
                    plot.subtitle = element_text(hjust = .5),
                    axis.title = element_text(size = 30),
                    axis.text = element_text(size = 20, color = "gray80"),
                    legend.background = element_rect(fill = "#3A3839"),
                    legend.key = element_rect(fill = "#3A3839"),
                    legend.title=element_text(size=18, hjust = 0),
                    legend.text = element_text(size = 20),
                    legend.position = c(.2, .8),
                    panel.grid.major = element_line(colour = "gray25"), 
                    panel.grid.minor = element_line(colour = "gray25"),
                    plot.background = element_rect(fill = "#3A3839", color = "gray90"),
                    panel.background = element_rect(fill = "#3A3839",
                                                    color = "#3A3839",
                                                    size = 0.5, linetype = "solid"))




#making correlation matrix heat map http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
mydata <- state_data[, c(2,3,5,8,9,10,12,15,17,19,20,21,22,25)]
cormat <- round(cor(mydata),2)
library(reshape2)


# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
lower_tri <- get_lower_tri(cormat)
lower_tri
melted_cormat <- melt(lower_tri, na.rm = TRUE)


library(extrafont)
library(ggplot2)
library(ggthemes)

#correlation heatmap export 1000X1000
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradientn(colors = c("#001845","#4ea8de","white", "#ff5a5f", "#660708" ), 
                       values = NULL,
                       limit = c(-1,1), 
                       name="Pearson\nCorrelation", 
                       guide = guide_colourbar(barwidth = 1, barheight = 15)) +
  labs( #title = "Correlation Matrix Heat Map", 
        x = "",
        y = "") +
  theme_dark_square + #theme_bw()
  theme(text = element_text(family = "Times New Roman"), 
        plot.title = element_text(face = "bold", hjust = .5, size = 30),
        legend.title=element_text(size=18, hjust = 0),
        legend.text = element_text(size = 12),
        legend.position = c(.2, .6),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust= .5)) +
  scale_x_discrete(labels = rev(c("Advanced\nDegree", "Gerrymandering", "Voting\nRate", "Religiosity", "Unemployment", "GDP\nGrowth", "GDP per\nCapita", "Immigrants", "Population\nDensity", "Age", "Proportion\nWhite", "Population\nGrowth Rate", "Population", "Proportion\nConservative\nLegislators")))+
  scale_y_discrete(labels = rev(c("Advanced\nDegree", "Gerrymandering", "Voting\nRate", "Religiosity", "Unemployment", "GDP\nGrowth", "GDP per\nCapita", "Immigrants", "Population\nDensity", "Age", "Proportion\nWhite", "Population Growth\nRate", "Population", "Proportion\nConservative\nLegislators")))
  





#histograms

#advacned degree
#get data of just state and advanced degreee, sorted
advanced_degree_states <- subset(state_data, select = c(state, prop.advanced..degree, conservatives))
advanced_degree_states_order <- advanced_degree_states[order(-advanced_degree_states$prop.advanced..degree),]
advanced_degree_states_order
degree_top_25 <- advanced_degree_states_order[1:25,]
degree_bottom_25 <- advanced_degree_states_order[26:50,]

ad_hist <- ggplot(data.frame(advanced_degree), aes(x = conservatives, fill = factor(highlow))) + 
  geom_histogram(binwidth = .075, color="gray20", lwd = 1,  alpha=.5, position="identity") +
  geom_density(alpha=.8) +
  geom_segment(aes(x = .339, y = 0, xend = .339, yend = 10), lwd = .75, size = .5, color = "black", linetype = "dashed")+
  geom_segment(aes(x = .664, y = 0, xend = .664, yend = 10), lwd = .75, size = .5, color = "black", linetype = "dashed")+
  labs(x = "Conservative Proportion of State Legislators", y = "Count", fill = "Proportion of\nPopulation with\nAdvanced Degrees") +
  theme_dark + #theme_minimal()
  theme(legend.position = c(.15, .8),
        legend.direction = "vertical",
        legend.title=element_text(size=30), 
        legend.text=element_text(size=25),
        text = element_text(family = "Times New Roman", size = 15),
        plot.title = element_text(face = "bold", hjust = .5, size = 30),
        axis.title = element_text(size = 40)
        ) +
  scale_fill_manual(values=c("#2d6a4f", "#d8f3dc"), labels = c("1" = "High","2" = "Low"))

#religiosity
religious_states <- subset(state_data, select = c(state, prop.religious, conservatives))
religious_states_order <- religious_states[order(-religious_states$prop.religious),]
religious_states_order

r_hist <- ggplot(data.frame(religion), aes(x = conservatives, fill = factor(highlow))) + 
  geom_histogram(binwidth = .075, color="gray20", size = 1, alpha=.5, position="identity") +
  geom_density(alpha=.8) +
  geom_segment(aes(x = 0.44, y = 0, xend = 0.44, yend = 8), lwd = .75, size = .5, color = "black", linetype = "dashed")+
  geom_segment(aes(x = 0.62, y = 0, xend = 0.62, yend = 8), lwd = .75, size = .5, color = "black", linetype = "dashed")+
  labs(x = "Conservative Proportion of State Legislators", y = "Count", fill = "Proportion of\nPopulation that\nIdentifies as\nHighly religious") +
  theme_dark +  #theme_minimal()
  theme(legend.position = c(.15, .8),
        legend.direction = "vertical",
        legend.title=element_text(size=30), 
        legend.text=element_text(size=25),
        plot.title = element_text(face = "bold", hjust = .5, size = 30),
        axis.title = element_text(size = 40)) +
  scale_fill_manual(values=c("#ffffb7", "#dc2f02"), labels = c("1" = "High","2" = "Low"))

#both
plot_grid(ad_hist, r_hist, nrow = 1, ncol =2, scale = .9) +
  theme_dark_minimal  #theme_bw() 
  labs(title = "Conservatism vs Education Levels and Religiosity")
  theme(panel.background = element_rect(fill = "#3A3839",
                                        color = "gray80",
                                        size = 0.5, linetype = "solid"))



#US map heat map https://liuyanguu.github.io/post/2019/04/17/ggplot-heatmap-us-50-states-map-and-china-province-map/
install.packages("ggsn")
library(maps)
library(usmap)
library(data.table)
library(ggsn)


#conservatives
dt1 <- as.data.table(copy(state_data))
dt1 <- dt1[,.(state, conservatives)]

us_map <- usmap::us_map()
con_heat <- usmap::plot_usmap(data = dt1, values = "conservatives", labels = F)+
  labs(fill ='Conservative Proportion of\nState Legislators', fill = "" ) + 
  scale_fill_gradientn(colors = c("#001845","#4ea8de","white", "#ff5a5f", "#660708" ), 
                       values = NULL,
                       limit = c(0,1),
                       guide = guide_colourbar(barwidth = 30, barheight = 0.4,
                                               #put legend title on top of legend
                                               title.position = "top", )) +
  theme_dark_heat + #remove
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title=element_text(size=25, hjust = 0), 
        legend.text=element_text(size=15),
  text = element_text(family = "Times New Roman"),
  plot.title = element_text(hjust = .5, size = 18))


#Advanced degree
dt2 <- as.data.table(copy(state_data))
dt2 <- dt2[,.(state, prop.advanced..degree)]

us_map <- usmap::us_map()
ad_heat <- usmap::plot_usmap(data = dt2, values = "prop.advanced..degree", labels = F)+
  labs(fill = 'Proportion of Population\nwith Advanced Degrees') + 
  scale_fill_gradientn(colors = c("white","#d8f3dc", "#95d5b2", "#2d6a4f", "#081c15"), 
                       values = NULL,
                       limit = c(0.05, .2),
                       guide = guide_colourbar(barwidth = 27, barheight = 0.4, hjust = .5,
                                               #put legend title on top of legend
                                               title.position = "top")) +
  theme_dark_heat + #remove
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title=element_text(size=25), 
        legend.text=element_text(size=15 ),
        text = element_text(family = "Times New Roman"),
        plot.title = element_text(face = "bold", hjust = .5, size = 30))

#religiosity
dt3 <- as.data.table(copy(state_data))
dt3 <- dt3[,.(state, prop.religious)]

us_map <- usmap::us_map()
r_heat <- usmap::plot_usmap(data = dt3, values = "prop.religious", labels = F)+
  labs(fill = 'Proportion of Population\nwho Identify as Highly Religious') + 
  scale_fill_gradientn(colors = c("white","#ffffb7", "#dc2f02", "#660f56" ), 
                       values = NULL,
                       limit = c(.3,.85),
                       guide = guide_colourbar(barwidth = 27, barheight = 0.4, hjust = .5,
                                               #put legend title on top of legend
                                               title.position = "top")) +
  theme_dark_heat + #remove
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title=element_text(size=25), 
        legend.text=element_text(size=15 ),
        text = element_text(family = "Times New Roman"),
        plot.title = element_text(face = "bold", hjust = .5, size = 30))

#immigrant proportion
dt4 <- as.data.table(copy(state_data))
dt4 <- dt4[,.(state, immigrant.prop)]

us_map <- usmap::us_map()
i_heat <- usmap::plot_usmap(data = dt4, values = "immigrant.prop", labels = F)+
  labs(fill = 'Proportion of Population\nthat are Immigrants' ) + 
  scale_fill_gradientn(colors = c("white", "#ade8f4", "#48cae4", "#0077b6", "#03045e" ), 
                       values = NULL,
                       limit = c(0,.4),
                       guide = guide_colourbar(barwidth = 27, barheight = 0.4, hjust = .5,
                                               #put legend title on top of legend
                                               title.position = "top")) +
  theme_dark_heat + #remove
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title=element_text(size=25), 
        legend.text=element_text(size=15 ),
        text = element_text(family = "Times New Roman"),
        plot.title = element_text(face = "bold", hjust = .5, size = 30))

#Population Density
dt5 <- as.data.table(copy(state_data))
dt5 <- dt5[,.(state, population.density)]

us_map <- usmap::us_map()
d_heat <- usmap::plot_usmap(data = dt5, values = "population.density", labels = F)+
  labs(fill = 'Population Density\n(Population per Square Mile)') + 
  scale_fill_gradientn(colors = c("white", "#f42b03", "#e70e02", "#9d0208" ), 
                       values = NULL,
                       limit = c(0,1500),
                       guide = guide_colourbar(barwidth = 27, barheight = 0.4, hjust = .5,
                                               #put legend title on top of legend
                                               title.position = "top")) +
  theme_dark_heat + #remove
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title=element_text(size=25), 
        legend.text=element_text(size=15 ),
        text = element_text(family = "Times New Roman"),
        plot.title = element_text(face = "bold", hjust = .5, size = 30))

#all on same page (export full screen)
var_heat <- plot_grid(ad_heat, r_heat, i_heat, d_heat, nrow = 2, ncol =2) +
  theme_dark_minimal + #remove
  theme( plot.title = element_text(family = "Times New Roman", face = "bold", hjust = .5, size = 30))

plot_grid(con_heat, var_heat, nrow = 1, ncol =2, scale = .9) +
  theme_dark + #theme_bw()
  theme(panel.background = element_rect(fill = "#3A3839",
                                        color = "gray80",
                                        size = 0.5, linetype = "solid"))



#Scatter plots
attach(state_data)
library("gridExtra")
library("cowplot")

#advanced degree
ad_plot <- ggplot(data.frame(conservatives), aes(x = prop.advanced..degree, y = conservatives)) +
  geom_point(color = "#95d5b2", size = 2.5) +
  geom_smooth(method = lm, se = FALSE, color = "#95d5b2", lwd = 1.75) + #color = "#2d6a4f"
  labs( x = "Proportion of Popultion with Advanced Degrees",
        y = "Conservative Proportion of\nState Legislators" )+
  theme_dark + #theme_minimal()
  theme(text = element_text(family = "Times New Roman"), 
        plot.title = element_text(face = "bold", hjust = .5, size = 16),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 20))
        #axis.text = element_text(size = 12, color = "gray40"))

#religiosity
r_plot <- ggplot(data.frame(conservatives), aes(x = prop.religious, y = conservatives)) +
  geom_point(color = "#ffffb7", size = 2.5) +
  geom_smooth(method = lm, se = FALSE, color = "#ffffb7", lwd = 1.75) + #color = "#660f56"
  labs( x = "Proportion of Population who Identify as Highly Religious",
        y = "Conservative Proportion of\nState Legislators" )+
  theme_dark + #theme_minimal()
  theme(text = element_text(family = "Times New Roman"), 
        plot.title = element_text(face = "bold", hjust = .5, size = 16),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 20))
        #axis.text = element_text(size = 12, color = "gray40"))

#immigrants
i_plot <- ggplot(data.frame(conservatives), aes(x = immigrant.prop, y = conservatives)) +
  geom_point(color = "#48cae4", size = 2.5) +
  geom_smooth(method = lm, se = FALSE, color = "#48cae4", lwd = 1.75) + #color = 0077b6
  labs( x = "Proportion of Population that are Immigrants",
        y = "Conservative Proportion of\nState Legislators" )+
  theme_dark + #theme_minimal()
  theme(text = element_text(family = "Times New Roman"), 
        plot.title = element_text(face = "bold", hjust = .5, size = 16),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 20))
        #axis.text = element_text(size = 12, color = "gray40"))

#population density
d_plot <- ggplot(data.frame(conservatives), aes(x = population.density, y = conservatives)) +
  geom_point(color = "#e70e02", size = 2.5) +
  geom_smooth(method = lm, se = FALSE, color = "#e70e02", lwd = 1.75) +
  labs( x = "Population Density (Population per Square Mile)",
        y = "Conservative Proportion of\nState Legislators" )+
  theme_dark + #theme_minimal()
  theme(text = element_text(family = "Times New Roman"), 
        plot.title = element_text(face = "bold", hjust = .5, size = 16),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 20))
        #axis.text = element_text(size = 12, color = "gray40"))

#arrange scatter plots on same chart
plot_grid(ad_plot, r_plot, i_plot, d_plot, nrow = 2, ncol =2, scale = .9) +
  theme_dark_minimal+ #theme_bw() +
  labs(title = "Conservativism vs Education Leveles, Religiosity,\nImmigrants, and Population Density\n")+
  theme( plot.title = element_text(family = "Times New Roman", face = "bold", hjust = .5, size = 30))

