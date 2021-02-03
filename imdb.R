library(tidyverse)

#Sys.setlocale("LC_ALL", "C") 

# read the datasets
title_akas <- read_tsv("title.akas.tsv", na = c("", "\\N"))

title_basics <- read_tsv("title.basics.tsv", na = c("", "\\N"))

title_ratings <- read_tsv("title.ratings.tsv", na = c("", "\\N"))

# Filter data
title_akas <- title_akas %>%
  filter(region == "US") %>%
  rename(tconst = titleId) %>%
  select(tconst, title)

title_basics <- title_basics %>%
  select(tconst, startYear, runtimeMinutes, genres)

title_ratings <- title_ratings %>%
  select(tconst, averageRating)

# Join data
imdb <- title_akas %>%
  left_join(title_basics, by = "tconst") %>%
  left_join(title_ratings, by = "tconst")

imdb_genres <- imdb %>%
  separate_rows(genres, sep = ",") %>%
  rename(genre = genres) %>%
  filter(!is.na(genre))

rm(title_akas)
rm(title_basics)
rm(title_ratings)

# Summarise data

imdb %>%
  filter(startYear == min(startYear, na.rm = TRUE))

imdb %>%
  summarise(average_runtime = mean(runtimeMinutes, na.rm = TRUE),
            average_rating = mean(averageRating, na.rm = TRUE))

imdb_genres %>%
  summarise(genres = unique(genre))

# Visualize Data

imdb_average_by_genre <- imdb_genres %>%
  group_by(genre) %>%
  summarise(average_rating = mean(averageRating, na.rm = TRUE),
            average_runtime = mean(runtimeMinutes, na.rm = TRUE)) %>%
  arrange(desc(average_rating))


ggplot(data = imdb_average_by_genre) +
  geom_col(aes(x = genre, y = average_rating, fill = genre)) +
  geom_abline(slope = 0, 
                  intercept = median(imdb_average_by_genre$average_rating),
                  color = "red",
                  lty = 2
              ) +
  theme_dark() +
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

ggplot(data = imdb_average_by_genre) +
  geom_col(aes(x = genre, y = average_runtime, fill = genre)) +
  geom_abline(slope = 0, 
              intercept = median(imdb_average_by_genre$average_runtime),
              color = "red",
              lty = 2
  ) +
  theme_dark() +
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
