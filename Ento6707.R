ento <- read_csv("Ent6707.csv")

summary(ento)
str(ento)
head(ento)
tail(ento)

ento$species <- as.factor(ento$species)
ento$distance_km <- as.factor(ento$distance_km)

##create table with distances and parasite counts ###


# checking for normality of samples collected at each distance #


# checking for normality using a density plot #
## WILL NOT WORK WITH PRESENCE/ABSENCE ##



plot(conopid ~ species, family = binomial, data = ento)



glm(nosema ~ distance_km + species, family = binomial, data = ento)
glm(apicystis ~ distance_km + species, family = binomial, data = ento)
glm(crithidia ~ distance_km + species, family = binomial, data = ento)

