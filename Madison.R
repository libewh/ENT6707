p.mad2019 <- read_csv("/Users/whiteman/Library/CloudStorage/OneDrive-TheOhioStateUniversity/School/Papers/Invert_Path/Data/2019_prcnt_imperv.csv")
p.mad2020 <- read_csv("/Users/whiteman/Library/CloudStorage/OneDrive-TheOhioStateUniversity/School/Papers/Invert_Path/Data/2020_prcnt_imperv.csv")
mad2019 <- read_csv("/Users/whiteman/Library/CloudStorage/OneDrive-TheOhioStateUniversity/School/Papers/Invert_Path/Data/2019_Mad.csv")
mad2020 <- read_csv("/Users/whiteman/Library/CloudStorage/OneDrive-TheOhioStateUniversity/School/Papers/Invert_Path/Data/2020_MADISON.csv")
impervspecies2019 <- read_csv("/Users/whiteman/Library/CloudStorage/OneDrive-TheOhioStateUniversity/School/Papers/Invert_Path/Data/2019_speciesimperv.csv")
madtotals <- read_csv("/Users/whiteman/Library/CloudStorage/OneDrive-TheOhioStateUniversity/School/Papers/Invert_Path/Data/madtotals.csv")
date2019 <- read_csv("/Users/whiteman/Library/CloudStorage/OneDrive-TheOhioStateUniversity/School/Papers/Invert_Path/Data/2019_date.csv")
date2020 <- read_csv("/Users/whiteman/Library/CloudStorage/OneDrive-TheOhioStateUniversity/School/Papers/Invert_Path/Data/2020_date.csv")
chi2019 <- read_csv("/Users/whiteman/Library/CloudStorage/OneDrive-TheOhioStateUniversity/School/Papers/Invert_Path/Data/2019_chi.csv")
chi2020 <- read_csv("/Users/whiteman/Library/CloudStorage/OneDrive-TheOhioStateUniversity/School/Papers/Invert_Path/Data/2020_chi.csv")
species <- read_csv("/Users/whiteman/Library/CloudStorage/OneDrive-TheOhioStateUniversity/School/Papers/Invert_Path/Data/2019_SpeciesStatus.csv")
infections <- read_csv("/Users/whiteman/Library/CloudStorage/OneDrive-TheOhioStateUniversity/School/Papers/Invert_Path/Data/2019SpeciesInfections.csv")
locations2019 <- read_csv("/Users/whiteman/Library/CloudStorage/OneDrive-TheOhioStateUniversity/School/Papers/Invert_Path/Data/locations2019.csv")
locations2020 <- read_csv("/Users/whiteman/Library/CloudStorage/OneDrive-TheOhioStateUniversity/School/Papers/Invert_Path/Data/locations2020.csv")
distances <- read_csv("/Users/whiteman/Library/CloudStorage/OneDrive-TheOhioStateUniversity/School/Papers/Invert_Path/Data/distances.csv")

library(readxl)
library(mapdata)
library(maptools)
library(ggmap)
library(ggplot2)
library(reshape2)
library(ggmap)
library(RgoogleMaps)
library(maps)
library(tidyverse)
library(mapview)
library(tidyr)
library(ggpubr)
library(viridis)
library(nortest)
library(emmeans)
library(dplyr)
library(sf)
library(multcompView)
library(multcomp)
library(mosaic)
library(car)
###############################################
# checking for normality of samples collected at each site #
## AKA number of bees collected from each site are similar between sites ##
z<-table(locations2019$location, locations2019$n)

chisq.test(table(locations2019$location, locations2019$n), correct = FALSE)
chisq.post.hoc(table(locations2019$location, locations2019$n))


#omit NA
## data <- na.omit(data)
mad2019 <- na.omit(mad2019)
mad2020 <- na.omit(mad2020)

###############################################
# checking for normality using a density plot #
## WILL NOT WORK WITH PRESENCE/ABSENCE ##
library("ggpubr")
ggdensity(p.mad2019$p.api, 
          main = "Density plot p.api",
          xlab = "p.api")

ggdensity(p.mad2020$p.api, 
          main = "Density plot p_api",
          xlab = "p_api")

#checking for normality using a Q-Q plot
library("ggpubr")
ggqqplot(p.mad2019$p.api) #points should fall along this line
ggqqplot(p.mad2020$p.api) #points should fall along this line

#checking for normality for one variable using Shapiro-Wilks
###sharpiro.test(data$variable)###
shapiro.test(p.mad2019$p.api)
shapiro.test(p.mad2020$p.api)
# the null hypothesis is "sample distribution is normal" #
# if the test is significant, the distribution is non-normal #

#######################################################################
#comparing infections between species#
x<-table(species$bombus, species$status)

ggplot(species) +
  aes(x = bombus, fill = status) +
  geom_bar(position = "stack")
# to visualize proportions use position = "fill"#
# to visualize the bars separate instead of stacked use position = "dodge"#
# also try stack

chisq.test(table(species$bombus, species$status), correct = FALSE)$expected
#This calculates expected values
#if any values are below 5 then use fisher exact 

chisq.test(table(species$bombus, species$status), correct = FALSE)
chisq.post.hoc(x)


install_version("fifer", "1.0")

dat <- data.frame(
  "infected" = c(49,25,303,2),
  "uninfected" = c(184,130,419,1),
  row.names = c("bimaculatus", "griseocollis", "impatiens", "vagans"),
  stringsAsFactors = FALSE
)

dat #review 

test<-fisher.test(dat) #fisher's exact
test




x <- c()
for (row in rownames(dat)) {
  for (col in colnames(dat)) {
    x <- rbind(x, matrix(rep(c(row, col), dat[row, col]), ncol = 2, byrow = TRUE))
  }
}
df <- as.data.frame(x)
df
colnames(df) <- c("bombus", "status")
df

# Fisher's exact test with raw data
test <- fisher.test(table(df))
test

# combine plot and statistical test with ggbarstats
library(ggstatsplot)
ggbarstats(
  df, status, bombus,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Fisher's exact test", ", p-value ",
    ifelse(test$p.value < 0.001, "< 0.001", round(test$p.value, 3))
  )
)

#######################################################################
# Create a linear model of Apicystis prevalence by percent impervious #
## this is using prevalence data, not presence/absence ##

lm2019imperv<-lm(formula = p.api ~ imperv, data = p.mad2019)
summary(lm2019imperv)

lm2019impervcount<-lm(formula = n ~ imperv, data = p.mad2019)
summary(lm2019impervcount)


lm2020imperv<-lm(formula = p.api ~ imperv, data = p.mad2020)
summary(lm2020imperv)

lm2020impervcount<-lm(formula = n ~ imperv, data = p.mad2020)
summary(lm2020impervcount)

# Add the fitted values as new column in the dataframe
p.mad2019$p_api.lm <- lm2019imperv$fitted.values
head(lm2019imperv)
plot(x = p.mad2019$p_api,
     y = lm2019imperv$fitted.values,
     xlab = "true values",
     ylab = "model fitted values",
     main = "regression fits of p_api")
abline(b = 1, a = 0) #values should fit around this line!


p.mad2020$p.api.lm <- lm2020imperv$fitted.values
head(lm2020imperv)
plot(x = p.mad2020$p.api,
     y = lm2020imperv$fitted.values,
     xlab = "true values",
     ylab = "model fitted values",
     main = "regression fits of p_api")
abline(b = 1, a = 0) #values should fit around this line!!


plot<-ggplot(p.mad2019, aes(x=imperv, y=p.api))+geom_point(size=3)
plot<-plot+geom_smooth(method="lm")
plot

plot<-ggplot(p.mad2020, aes(x=imperv, y=p.api))+geom_point(size=3)
plot<-plot+geom_smooth(method="lm")
plot


##binary logistic regression using glm(family = "binomial")
mad2019.glm <- glm(formula = Apicystis ~ AvImp_1500,
                   data = mad2019,
                   family = binomial)
summary(mad2019.glm)

mad2020.glm <- glm(formula = Apicystis ~ AvImp_1500,
                   data = mad2020,
                   family = binomial)
summary(mad2020.glm)

###############################################
### Timeline ###
ggplot(date2019, aes(date, p_api)) + 
  geom_point(aes(color="p_api")) + 
  geom_smooth(method=lm)


### ANOVA ###
### regression, as oppossed to ANOVA, is better for field data because
# because of its predictive power. ANOVA and regression (lm or glm)
# are essentially the same thing. Regression is inferential

aov2019 <- aov(p_api ~ factor(AvImp_1500), data = p.mad2019)
summary(aov2019)

aov2020 <- aov(p_api ~ factor(AvImp_1500), data = p.mad2020)
summary(aov2020)

#############################
## chi-square for families ##
#first, see if any expected values are below 5 using $expected
#if any values are below 5, then use fisher!
chisq.test(chi2019[,-1], correct = FALSE)$expected
chisq.test(chi2020[,-1], correct = FALSE)$expected

#run chi square if all are above 5
chisq.test(chi2019[,-1], correct = FALSE)
chisq.test(chi2020[,-1], correct = FALSE)


#creates data frame instead of uploading file#
dat<-data.frame(
  "infected" = c(134, 234),
  "uninfected" = c(260,582),
  row.names = c("Siblings", "Singles"),
  stringsAsFactors = FALSE
)
dat

#mosaic plot to visualize categories###
mosaicplot(dat,
           main = "Mosaic Plot",
           color = TRUE)


#calculate distance between sites in meters
library(geosphere)

#this creates a matrix with distances between locations in meters
loc2019<- distm(locations2019[, c('lon', 'lat')],
            fun = distHaversine)

loc2020<- distm(locations2020[, c('lon', 'lat')],
            fun = distHaversine)

loc2019df <- as.data.frame(loc2019)
loc2020df <- as.data.frame(loc2020)


##### basic plots for data visualization ####
ggplot(impervspecies2019) +
  aes(imperv, p.api) +
  geom_tile(aes(imperv, species, fill = p.api))

#plots for publication
# Load the ggplot2 package
library(ggplot2)

# Example data frame
data <- data.frame(
  species = c("Species A", "Species B", "Species C", "Species D"),
  prevalence = c(10, 25, 15, 30)
)

# Create visuals

library(ggplot2)
library(dplyr)
years <- read_csv("/Users/whiteman/Library/CloudStorage/OneDrive-TheOhioStateUniversity/School/Papers/Invert_Path/Data/byyearplot.csv")
year2019 <- years[which(years$year==2019),]
year2020 <- years[which(years$year==2020),]

# % adat# % api ~ % imp
ggplot(years, aes(imp, n, fill = species)) +
  geom_bar(position = position_dodge(1.0), stat = "identity", width = .50) +
  facet_wrap(~ year) +
  labs(title = "% api by % imp for four bumble bee species",
       x = "% imp",
       y = "% api") +
  theme_minimal()

ggplot(years, aes(imp, p.api, fill = species)) +
  geom_bar(position = "dodge", stat = "identity", width = .50) +
  #scale_x_continuous(breaks = round(seq(min(year2019$imp), max(year2019$imp), by = 2.0),5)) +
  #scale_y_continuous(breaks = round(seq(min(year2019$api), max(year2019$api), by = 0.5),1)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  #scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(title = "% api by % imp for four bumble bee species",
       x = "% imp",
       y = "% api") +
  theme_minimal()

ggplot(year2019, aes(x = imp, y = species, fill = p.api)) +
  geom_tile(color = "white", width = 2, height = 2) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "heatmap for your pleasure",
       x = "imp",
       y = "species",
       fill = "p.api") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_fixed(ratio = 1)

ggplot(year2019, aes(x = imp, y = p.api, color = species)) +
  geom_point() +
  labs(title = "scat(ter) for your pleasure",
       x = "imp",
       y = "p.api") +
  theme_minimal() +
  facet_wrap(~ year)

ggplot(year2019, aes(x = imp, y = p.api)) +
  stat_binhex(bins = 10) +
  scale_fill_viridis_c() +
  labs(title = "Hexbin it",
       x = "X-axis",
       y = "Y-axis",
       fill = "p.api") +
  theme_minimal()

ggplot(years, aes(p.api)) +
         geom_density()

ggplot(year2019, aes(p.api)) +
  geom_density()
ggplot(year2020, aes(p.api)) +
  geom_density()
