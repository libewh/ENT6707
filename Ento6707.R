#############################################################
#############################################################
#############################################################

ento <- read_excel("Ent6707.xlsx")

# how many bee species in the data set?
ento$species <- as.factor(ento$species)
levels(ento$species)
summary(ento$species)

# what are the distances in the data set?
ento$distance_km <- as.factor(ento$distance_km)
levels(ento$distance_km)
summary(ento$distance_km)

# how many plant genera in the data set?
ento$plantgen <- as.factor(ento$plantgen)
levels(ento$plantgen)
summary(ento$plantgen)

# how many conopids in the data set?
ento$conopid <- as.factor(ento$conopid)
summary(ento$conopid)

str(ento)
summary(ento)
summary(ento$species)


########   PLOTS   ##########################################
#############################################################

# bees by distance
ggplot(ento) +
  aes(x = distance_km, fill = species) +
  geom_bar(position = "stack") +
  xlab("distance from apiary (km)") +
  ylab("count")

# add column for species count 
df <- ento %>% group_by(species) %>% mutate(count_name_occurr = n())

# count of bees
ggplot(df) +
  aes(x = reorder(species, -count_name_occurr), fill = species) +
  geom_bar(position = "stack") +
  xlab("bee species") +
  ylab("count")

# add column for flower count
df2 <- ento %>% group_by(plantgen) %>%   mutate(plantgenoccur = n())

# bees by flowers
ggplot(df2) +
  aes(x = reorder(plantgen, -plantgenoccur), fill = species) +
  geom_bar(position = "stack") +
  xlab("flowers") +
  ylab("count")


# subset pathogens
paths <- ento[, c("apicystis", "crithidia", "nosema")]
print(paths)

df.pca <- rda(paths)
df.pca
summary(df.pca)

(inventory <- df.pca$CA$eig)
screeplot(df.pca, bstick=TRUE, npcs=length(df.pca$CA$eig))

par(mfrow=c(1,1))
biplot(df.pca, scaling=1, type="text", xlab="poop", ylab="pee")
