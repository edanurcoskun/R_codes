install.packages("tidyverse")
install.packages("starwars")

library(tidyverse)
library(starwars)

view(starwars)

# 1- How many unique species per their homeworld are there?


part <- na.omit(select(starwars, homeworld, species))

unique_spe <- part %>%
  group_by(homeworld) %>%
  summarize(num_unique = length(unique(species)))

num_per_homeworld <- sum(unique_spe$num_unique)

num_in_starwars <- length(na.omit(unique(starwars$species)))


view(unique_spe)





# 2- Which character(s) did play in the Star Wars movies most?


most_film <- starwars %>%
  group_by(name) %>%
  summarize(num_movies = lengths(films), .groups = 'drop') %>%
  arrange(desc(num_movies)) %>%
  filter(row_number() == 1)  # write for more than one max value

movie <- starwars %>%
  group_by(films) %>%
  summarize((which(name == most_film$name)))

film_names <- str_c(movie$films[1] %>%
                      .[[1]], collapse = ", ")



# 3- According to the data available, what were the average 
# height value and the average mass value across each species?

average <- (starwars %>%
              group_by(species) %>%
              summarize(
                count = length(species),
                ave_height = mean(height, na.rm = TRUE),
                ave_mass = mean(mass, na.rm = TRUE)) %>%
              filter(!is.na(species)))

view(average)





# 4- Create a new data set by adding a new observation to this data.
# This observation should be based on your own character (your name or
# nickname, your weight and height, your homeworld, your starships etc).
# Note that you can pick one or more than one Star Wars films in which you
# played as a movie star.


# First-Way

add_char <- starwars

add_char #before adding

new_char <- data.frame(name = "Ella Luna", height = 150, mass = 90.0, 
                       hair_color = "ginger", skin_color = "green", 
                       eye_color = "unstable", birth_year = 51.0, 
                       sex = "hermaphrodite", gender = "feminine", 
                       homeworld = "Asgard", species = "Titan", 
                       films = "Attack of the Clones", 
                       vehicles = "skate", starships = "X-wing")

add_char <- rbind(new_char, add_char)  

view(add_char)






# Second-Way

add_char <- starwars

add_char #before adding

new_char <- data.frame("Ella Luna", 150, 90.0, "ginger", "green", "unstable", 
                       51.0,"hermaphrodite", "feminine", "Asgard", "Titan", 
                       "Attack of the Clones", "skate", "Imperial shuttle")

names(new_char) <- c("name", "height", "mass", "hair_color", "skin_color",
                     "eye_color", "birth_year", "sex", "gender", "homeworld",
                     "species", "films", "vehicles", "starships")


add_char <- rbind(new_char, add_char) 

view(add_char)






# Third-Way

add_char <- starwars

add_char #before adding

add_char <- add_char %>% 
  add_row(name = "Ella Luna", height = 150, mass = 90.0, 
          hair_color = "ginger", skin_color = "green", 
          eye_color = "unstable", birth_year = 51.0, 
          sex = "hermaphroditic", gender = "feminine", 
          homeworld = "Asgard", species = "Titan", 
          films = list("Attack of the Clones"), 
          vehicles = list("skate"), starships = list("X-wing"), .before = 1)

view(add_char)









# 5- Calculate the body mass index (BMI) values (dividing the mass value in kg
# to the square of the height value in meter) for all observations and create
# a new data set including BMI values and the variables titled as name, mass, 
# height, species, hair color, skin color, eye color, sex and gender.


find_bmi <- starwars

find_bmi <- (find_bmi %>%
               group_by(name, height, mass, species, hair_color, 
                        skin_color, eye_color, sex, gender) %>%
               summarize(
                 BMI = mass/((height/100)^2), .groups = 'drop')) %>%
  select(name, height, mass, BMI, species, hair_color, 
         skin_color, eye_color, sex, gender)

view(find_bmi)









# 6- With using this new dataset, categorize the observations as underweight
# (BMI below 18.5), healthy (BMI between 18.5-24.99), overweight 
# (BMI between 25.0-29.99) and obese (BMI above 30.0). Find the counts of 
# these categories with respect to species.


part <- select(starwars, mass, height, species)
categorical <- (((part %>%
                    group_by(species) %>%
                    summarize(
                      BMI = mass/((height/100)^2), .groups = 'drop')) %>%
                   group_by(species) %>%
                   summarize(numOfNA = length(BMI[is.na(BMI)]),
                             underweight = (length(BMI[BMI<18.5])) - numOfNA,
                             healthy = (length(BMI[(18.5 <= BMI) & (BMI < 25.0 )])) - numOfNA,
                             overweight = (length(BMI[(25.0 <= BMI) & (BMI < 30.0)])) - numOfNA,
                             obese = (length(BMI[(30.0 < BMI)])) - numOfNA, .groups = 'drop')) %>%
                  select(-numOfNA)) %>%
  filter(!is.na(species))

all_species <- categorical %>%
  group_by() %>%
  summarize(
    Underweight = sum(underweight),
    Healthy = sum(healthy),
    Overweight = sum(overweight),
    Obese = sum(obese)
  )
per_species <- gather(all_species, key = "BMI_gr", value = "count")
view(categorical)









# 7- Plot the distribution of BMI according to sex and gender.


part <- select(starwars, sex, gender, mass, height)
plot <- (part %>%
           group_by(sex, gender) %>%
           summarize(BMI = mass/((height/100)^2), .groups = 'drop')) %>%
  filter(BMI<400)

ggplot(data = plot) +
  geom_boxplot( mapping = aes(x = gender, y = BMI, color = sex)) 






















