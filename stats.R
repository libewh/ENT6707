### 2019 api~species ###
#----2019----#
species <- read_csv("/Users/whiteman/Library/CloudStorage/OneDrive-TheOhioStateUniversity/School/Papers/Invert_Path/Data/2019_SpeciesStatus.csv")

chisq.test(table(species$bombus, species$status), correct = FALSE)$expected
#This calculates expected values
#if any values are below 5 then use fisher exact 

chisq.test(table(species$bombus, species$status), correct = FALSE)
chisq.post.hoc(x)


#### impervious surface ~ a. bombi ####
#----2019----#



#----2020----#