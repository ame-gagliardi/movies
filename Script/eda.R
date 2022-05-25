# Netflix EDA

library(tidyverse)
library(lubridate)
library(data.table)
library(ggrepel)
library(rgdal)
library(broom)
library(geojsonio)
library(rworldmap)
library(ggpubr)


# Data loading

movies <- as_tibble(fread("Data/cleaned/netflix_movies_df.csv"))
genres <- as_tibble(fread("Data/cleaned/netflix_genres_df.csv"))
countries <- as_tibble(fread("Data/cleaned/netflix_countries_df.csv"))

#########
## EDA ##
#########

# Content type

movies %>%
  dplyr::group_by(type) %>% 
  dplyr::summarise(count = n()) %>% 
  ggplot(aes(x = type, y = count, fill = type)) + 
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(x = type, label = count), position = position_dodge(0.9), vjust = -0.2) +
  ggtitle("Netflix content by type") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5))

# Content type over time

movies %>% 
  dplyr::group_by(type, added_date) %>%
  dplyr::summarise(count = n()) %>% 
  dplyr::arrange(added_date) %>% 
  dplyr::mutate(cs = cumsum(count)) %>% 
  ggplot(aes(x = added_date, y = cs, color = type)) + 
  geom_line(size = 1.3) +
  scale_x_date(date_breaks = "6 month", date_labels = "%m/%y") +
  scale_color_discrete(name = "Content type") +
  ggtitle("Netflix content over time by type") +
  theme_bw() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))

# Content type over time by genre

genres %>% 
  left_join(movies, by = "title") %>% 
  select(title, genre, type.x, added_date) %>% 
  filter(type.x == "Movie") %>% 
  group_by(type.x, genre, added_date) %>%
  summarise(count = n()) %>% 
  arrange(added_date) %>% 
  mutate(cs = cumsum(count)) %>% 
  ggplot(aes(x = added_date, y = cs, color = genre)) +
  geom_line(size = 1.3) + 
  geom_dl(aes(label = genre), method = list(dl.combine("last.polygons")), cex = 0.3) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))


genres %>% 
  left_join(movies, by = "title") %>% 
  select(title, genre, type.x, added_date) %>% 
  filter(type.x == "TV Show") %>% 
  group_by(type.x, genre, added_date) %>%
  summarise(count = n()) %>% 
  arrange(added_date) %>% 
  mutate(cs = cumsum(count)) %>% 
  ggplot(aes(x = added_date, y = cs, color = genre)) +
  geom_line(size = 1.3) + 
  geom_dl(aes(label = genre), method = list(dl.combine("last.polygons")), cex = 0.3) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))
  

# Distribution of content by release date

ggplot(movies, aes(x = release_date, fill = type)) +
  geom_density(alpha = 0.4) +
  theme_bw() +
  ggtitle("Distribution of film release date") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))

# Distribution of content by genre

p1 <- genres %>% 
  filter(type == "Movie") %>% 
  group_by(genre) %>% 
  summarise(count = n()) %>% 
  mutate(genre = as.factor(genre)) %>% 
  mutate(genre = fct_reorder(genre, desc(count))) %>% 
  ggplot(aes(x = genre, y = count, fill = genre)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(x = genre, label = count), position = position_dodge(0.9), vjust = -0.2) +
  scale_fill_viridis_d() + 
  ggtitle("Movies by genre") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5))
  
p2 <- genres %>% 
  filter(type == "TV Show") %>% 
  group_by(genre) %>% 
  summarise(count = n()) %>% 
  mutate(genre = as.factor(genre)) %>% 
  mutate(genre = fct_reorder(genre, desc(count))) %>% 
  ggplot(aes(x = genre, y = count, fill = genre)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(x = genre, label = count), position = position_dodge(0.9), vjust = -0.2) +
  scale_fill_viridis_d() + 
  ggtitle("TV Shows by genre") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5))

ggarrange(p1, p2, ncol = 1, nrow = 2)

# Distribution of content by rating

movies %>% 
  select(title, type, rating) %>% 
  group_by(type, rating) %>% 
  summarise(count = n ()) %>% 
  arrange(desc(count))

# Distribution rating by genre
# Correlation imdb - tmdb

movies %>% 
  select(title, tmdb_score, imdb_score, rating) %>% 
  filter(!(is.na(tmdb_score))) %>% 
  ggplot(aes(x = imdb_score, y = tmdb_score, color = rating)) + 
  geom_point()


movies %>% 
  select(title, tmdb_score, imdb_score, rating) %>% 
  filter(!(is.na(tmdb_score))) %>% 
  ggscatter(x = "imdb_score", y = "tmdb_score",
            color = "rating", 
            add = "reg.line", add.params = list(color = "blue", fill = "lightgray"),
            cor.coef = T, cor.coeff.args = list(method = "spearman"))

# Correlation imdb - # voti

genres %>% 
  left_join(movies, by = "title") %>% 
  select(title, type.x, genre, imdb_votes, imdb_score) %>% 
  filter(!(is.na(imdb_score)) & imdb_votes >= 100000) %>% 
  mutate(label = ifelse(imdb_votes > 1000000 | imdb_score > 9, title, NA),
         votes = case_when(
           imdb_votes < 200000 ~ "<200k",
           imdb_votes >= 200000 & imdb_votes < 300000 ~ "200k-300k",
           imdb_votes >= 300000 & imdb_votes < 400000 ~ "300k-400k",
           imdb_votes >= 400000 & imdb_votes < 500000 ~ "400k-500k",
           imdb_votes > 500000 ~ ">500k")) %>% 
  distinct(title, .keep_all = T) %>% 
  ggplot(aes(x = imdb_votes, y = imdb_score, color = type.x)) +
  geom_point() +
  geom_text_repel(aes(label = label), min.segment.length = 0, box.padding = 0.5, show.legend = F) + 
  scale_color_discrete(name = "Content type") + 
  scale_x_continuous(breaks = c(500000, 1000000, 1500000, 2000000), labels = c("500k", "1M", "1.5M", "2M")) +
  xlab("Number of votes on IMDB") +
  ylab("IMDB score") + 
  labs(title = "Relationship between number of votes and IMDB scores by content type",
       caption = "Only movies or TV shows with >100k votes have been plotted") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
  

# Correlation between duration and imdb score

movies %>% 
  select(title, type, duration, imdb_score) %>% 
  filter(type == "Movie") %>% 
  mutate(duration = parse_number(duration)) %>% 
  arrange(desc(duration)) %>% 
  filter(duration < 250) %>% 
  ggplot(aes(x = duration, y = imdb_score)) +
  geom_point() +
  stat_cor(method = "spearman")
  theme_bw()


movies %>% 
  select(title, type, duration, imdb_score) %>% 
  filter(type == "TV Show") %>% 
  mutate(duration = parse_number(duration)) %>% 
  arrange(desc(duration)) %>% 
  filter(duration < 250) %>% 
  ggplot(aes(x = duration, y = imdb_score)) +
  geom_point() +
  stat_cor(method = "spearman")


# Duration exploration

movies %>% 
  select(title, type, duration) %>% 
  filter(type == "TV Show") %>% 
  mutate(duration = parse_number(duration),
         label = ifelse(duration > 10, title, "")) %>% 
  ggplot(aes(x = duration)) + 
  geom_density() +
  geom_text_repel(aes(label = label))
  



#==================================================================================================#
#================================== Clorophlet map of production ==================================#
#==================================================================================================#

# Aggregate count of movies by country. 

film_countries <- countries %>% 
  group_by(type, country) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

# Renaming Country names and deleting old country names 
film_countries[film_countries$country == "United States", "country"] <- "United States of America"
film_countries[film_countries$country == "Hong Kong", "country"] <- "Hong Kong S.A.R."
film_countries[film_countries$country == "Serbia", "country"] <- "Republic of Serbia"
film_countries[film_countries$country == "Bahamas", "country"] <- "The Bahamas"
film_countries[film_countries$country == "Vatican City", "country"] <- "Vatican"

toRM <- which(film_countries$country == "" | film_countries$country == "West Germany" | film_countries$country == "East Germany" |
              film_countries$country == "Palestine" | film_countries$country == "Soviet Union")
film_countries <- film_countries[-toRM, ]

# Create the world map

worldmap <- getMap(resolution = "coarse")
worldmap_df <- tidy(worldmap)

# Merge the two dataframe

film_country_map<- worldmap_df %>% 
  left_join(film_countries, by = c("id" = "country")) 

# Plotting the map with the layer of movies

ggplot() +
  geom_polygon(data = film_country_map, 
               aes(x=long, y=lat, group = group, fill = log(count)), color = "white") +
  scale_fill_viridis_b() +
  theme_void() +
  ggtitle("Numbe")
         
