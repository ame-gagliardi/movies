library(tidyverse)
library(data.table)

# Data Loading

netflix <- as_tibble(fread("Data/netflix_titles.csv")) %>% 
  dplyr::mutate(platform = "Netflix") %>% 
  tidyr::separate(col = "listed_in", into = "genre", sep = ",", remove = F) %>% 
  dplyr::mutate(genre = trimws(genre))

prime <- fread("Data/amazon_titles.csv") %>% 
  dplyr::mutate(platform = "Prime")

disney <- fread("Data/disney_plus_titles.csv") %>% 
  dplyr::mutate(platform = "Disney")

hulu <- fread("Data/hulu_titles.csv") %>% 
  dplyr::mutate(platform = "Hulu")


# df <- rbind(netflix, disney, prime, hulu) %>% 
#   dplyr::select(-c(show_id, description)) %>% 
#   dplyr::select(platform, title, type, country, listed_in, duration, date_added, release_year, rating, director, cast)



# Genres

netflix_genres <- netflix %>% 
  dplyr::select(title, listed_in) %>% 
  tidyr::separate(col = "listed_in", into = "genre", sep = ",")





netflix_genres <- sort(unique(trimws(unlist(str_split(netflix$listed_in, pattern = ",")))))
disney_genres <- sort(unique(trimws(unlist(str_split(disney$listed_in, pattern = ",")))))
hulu_genres <- sort(unique(trimws(unlist(str_split(hulu$listed_in, pattern = ",")))))
prime_genres <- sort(unique(trimws(unlist(str_split(prime$listed_in, pattern = ",")))))

                     