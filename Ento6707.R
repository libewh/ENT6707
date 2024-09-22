ento <- read_excel("Ent6707.xlsx")

# how many bee species in the data set?
ento$species <- as.factor(ento$species)
levels(ento$species)

# what are the distances in the data set?
ento$distance_km <- as.factor(ento$distance_km)
levels(ento$distance_km)

# how many plant genera in the data set?
ento$plantgen <- as.factor(ento$plantgen)
levels(ento$plantgen)


str(ento)
summary(ento)

# add column for species count
df <- ento %>% group_by(species) %>%   mutate(count_name_occurr = n())

# add column for flower count
df2 <- ento %>% group_by(plantgen) %>%   mutate(plantgenoccur = n())


# bees by count
ggplot(df) +
  aes(x = reorder(species, -count_name_occurr), fill = species) +
  geom_bar(position = "stack") +
  xlab("bee species") +
  ylab("count")

# bees by distance
ggplot(ento) +
  aes(x = distance_km, fill = species) +
  geom_bar(position = "stack") +
  xlab("distance from apiary") +
  ylab("count")

# bees by flowers
ggplot(df2) +
  aes(x = reorder(plantgen, -plantgenoccur), fill = species) +
  geom_bar(position = "stack") +
  xlab("flowers") +
  ylab("count")
