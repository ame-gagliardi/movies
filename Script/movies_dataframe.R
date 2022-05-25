library(tidyverse)
library(data.table)

# Data loading

netflix <- as_tibble(fread("Data/netflix_titles.csv", na.strings = "")) %>% 
  dplyr::select(-c(show_id, description)) %>% 
  dplyr::mutate(platform = "netflix")

disney <- as_tibble(fread("Data/disney_plus_titles.csv", na.strings = "")) %>% 
  dplyr::select(-c(show_id, description)) %>% 
  dplyr::mutate(platform = "disney")

hulu <- as_tibble(fread("Data/hulu_titles.csv", na.strings = "")) %>% 
  dplyr::select(-c(show_id, description)) %>% 
  dplyr::mutate(platform = "hulu")

amazon <- as_tibble(fread("Data/amazon_titles.csv", na.strings = "")) %>% 
  dplyr::select(-c(show_id, description)) %>% 
  dplyr::mutate(platform = "amazon")

# Genres dataframe

netflix_genres <- netflix %>% 
  dplyr::select(title, listed_in) %>% 
  tidyr::separate(col = listed_in, into = c("genre_1", "genre_2", "genre_3", "genre_4", "genre_5"), sep = ",", extra = "warn") %>% 
  dplyr::mutate(genre_1 = trimws(genre_1),
                genre_2 = trimws(genre_2),
                genre_3 = trimws(genre_3),
                genre_4 = trimws(genre_4),
                genre_5 = trimws(genre_5)) %>% 
  dplyr::mutate(genre_1 = ifelse(str_detect(genre_1, "and "), str_remove(genre_1, "and "), genre_1),
                genre_2 = ifelse(str_detect(genre_2, "and "), str_remove(genre_2, "and "), genre_2),
                genre_3 = ifelse(str_detect(genre_3, "and "), str_remove(genre_3, "and "), genre_3),
                genre_4 = ifelse(str_detect(genre_4, "and "), str_remove(genre_4, "and "), genre_4),
                genre_5 = ifelse(str_detect(genre_5, "and "), str_remove(genre_5, "and "), genre_5))

disney_genres <- disney %>% 
  dplyr::select(title, listed_in) %>% 
  tidyr::separate(col = listed_in, into = c("genre_1", "genre_2", "genre_3", "genre_4", "genre_5"), sep = ",", extra = "warn") %>% 
  dplyr::mutate(genre_1 = trimws(genre_1),
                genre_2 = trimws(genre_2),
                genre_3 = trimws(genre_3),
                genre_4 = trimws(genre_4),
                genre_5 = trimws(genre_5)) %>% 
  dplyr::mutate(genre_1 = ifelse(str_detect(genre_1, "and "), str_remove(genre_1, "and "), genre_1),
                genre_2 = ifelse(str_detect(genre_2, "and "), str_remove(genre_2, "and "), genre_2),
                genre_3 = ifelse(str_detect(genre_3, "and "), str_remove(genre_3, "and "), genre_3),
                genre_4 = ifelse(str_detect(genre_4, "and "), str_remove(genre_4, "and "), genre_4),
                genre_5 = ifelse(str_detect(genre_5, "and "), str_remove(genre_5, "and "), genre_5))

hulu_genres <- hulu %>% 
  dplyr::select(title, listed_in) %>% 
  tidyr::separate(col = listed_in, into = c("genre_1", "genre_2", "genre_3", "genre_4", "genre_5"), sep = ",", extra = "warn") %>% 
  dplyr::mutate(genre_1 = trimws(genre_1),
                genre_2 = trimws(genre_2),
                genre_3 = trimws(genre_3),
                genre_4 = trimws(genre_4),
                genre_5 = trimws(genre_5)) %>% 
  dplyr::mutate(genre_1 = ifelse(str_detect(genre_1, "and "), str_remove(genre_1, "and "), genre_1),
                genre_2 = ifelse(str_detect(genre_2, "and "), str_remove(genre_2, "and "), genre_2),
                genre_3 = ifelse(str_detect(genre_3, "and "), str_remove(genre_3, "and "), genre_3),
                genre_4 = ifelse(str_detect(genre_4, "and "), str_remove(genre_4, "and "), genre_4),
                genre_5 = ifelse(str_detect(genre_5, "and "), str_remove(genre_5, "and "), genre_5))

amazon_genres <- amazon %>% 
  dplyr::select(title, listed_in) %>% 
  tidyr::separate(col = listed_in, into = c("genre_1", "genre_2", "genre_3", "genre_4", "genre_5"), sep = ",", extra = "warn") %>% 
  dplyr::mutate(genre_1 = trimws(genre_1),
                genre_2 = trimws(genre_2),
                genre_3 = trimws(genre_3),
                genre_4 = trimws(genre_4),
                genre_5 = trimws(genre_5)) %>% 
  dplyr::mutate(genre_1 = ifelse(str_detect(genre_1, "and "), str_remove(genre_1, "and "), genre_1),
                genre_2 = ifelse(str_detect(genre_2, "and "), str_remove(genre_2, "and "), genre_2),
                genre_3 = ifelse(str_detect(genre_3, "and "), str_remove(genre_3, "and "), genre_3),
                genre_4 = ifelse(str_detect(genre_4, "and "), str_remove(genre_4, "and "), genre_4),
                genre_5 = ifelse(str_detect(genre_5, "and "), str_remove(genre_5, "and "), genre_5))



# Country dataframe

netflix_country <- netflix %>% 
  dplyr::select(title, country) %>% 
  tidyr::separate(country, into = c("country_1", "country_2", "country_3", "country_4", "country_5", 
                                    "country_6", "country_7", "country_8", "country_9", "country_10"), 
                  sep = ",", extra = "warn")

amazon_country <- amazon %>% 
  dplyr::select(title, country) %>% 
  tidyr::separate(country, into = c("country_1", "country_2", "country_3", "country_4", "country_5", 
                                    "country_6", "country_7", "country_8", "country_9", "country_10"), 
                  sep = ",", extra = "warn")

hulu_country <- hulu %>% 
  dplyr::select(title, country) %>% 
  tidyr::separate(country, into = c("country_1", "country_2", "country_3", "country_4", "country_5", 
                                    "country_6", "country_7", "country_8", "country_9", "country_10"), 
                  sep = ",", extra = "warn")

disney_country <- disney %>% 
  dplyr::select(title, country) %>% 
  tidyr::separate(country, into = c("country_1", "country_2", "country_3", "country_4", "country_5", 
                                    "country_6", "country_7", "country_8", "country_9", "country_10"), 
                  sep = ",", extra = "warn")


# Data merging

movies_genres <- rbind(netflix_genres,
                       disney_genres,
                       amazon_genres,
                       hulu_genres)

movies_country <- rbind(netflix_country,
                       disney_country,
                       amazon_country,
                       hulu_country)

movies <- rbind(netflix,
                disney,
                amazon,
                hulu)


# Saving data

write.table(movies, file = "Data/movies_merged.csv", quote = F, row.names = F, sep = "\t")
write.table(movies_genres, file = "Data/movies_genres_merged.csv", quote = F, row.names = F, sep = "\t")
write.table(movies_country, file = "Data/movies_country_merged.csv", quote = F, row.names = F, sep = "\t")
