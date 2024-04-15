install.packages("tidyverse")
install.packages("maps")
install.packages("dplyr")
install.packages("plotly")

library(tidyverse)
library(maps)
library(dplyr)
library(ggplot2)
library(plotly)

unicef_metadata <- read.csv("unicef_metadata.csv")
unicef_indicator_1 <- read.csv("unicef_indicator_1.csv")
unicef_metadata_filtered <- filter(unicef_metadata, year >= 2011 & year <= 2018)
unicef_indicator_1_filtered <- filter(unicef_indicator_1, time_period >= 2011 & time_period <= 2018)

data_join <- full_join(unicef_metadata_filtered, unicef_indicator_1_filtered, by = c("country" = "country", "year" = "time_period"))


map_world <- map_data("world")





# vis 1-- map 

data_join_years <- data_join %>%
  filter(between(year, 2011, 2018))

map_data_join_years <- full_join(data_join_years, map_world, by = c("country" = "region"))

ggplot(map_data_join_years) +
  aes(x = long, y = lat, group = group, fill= obs_value) +
  geom_polygon() +
  scale_fill_gradient(low = "green", high = "red", name = "%CS2FD") +
  labs(x = "longitude", y = "latitude", title = "% of Children suffering 2 forms of deprivation") +
  theme_void()

#map 2 
high_gdp_color <- "green"
low_gdp_color <- "red"


ggplot(map_data_join_years) +
  aes(x = long, y = lat, group = group, fill = `GDP per capita`) +
  geom_polygon() +
  scale_fill_gradient(low = low_gdp_color, high = high_gdp_color, name = "GDP per capita") +
  labs(x = "Longitude", y = "Latitude", title = "GDP per capita") +
  theme_void()
# vis 2 

timeseries_plot_1 <- data_join %>%
  filter(Continent == "Africa") %>%
  ggplot() +
  aes(year, LifeExp, group = country, color = Continent) +
  geom_line()
ggplotly(timeseries_plot_1)

#vis 3

filtered_data <- data_join %>%
  filter(year %in% 2011:2018)

ggplot(filtered_data) +
  aes(`GDP.per.capita`, LifeExp, colour = Continent, size = Population) +
  geom_point(alpha = 0.2) +
  facet_wrap(~ year, ncol = 4) +  # Set ncol to 4 for two rows of four plots each
  scale_x_continuous(limits = c(0, 50000),
                     breaks = c(20000, 40000),
                     labels = scales::unit_format(unit = "K", scale = 0.001)) +
  labs(x = "GDP per Capita", y = "Life Expectancy", title = "Relationship between GDP and Life Expectancy between 2011 and 2018") +
  guides(colour = "none", size = "none")

#vis 4

cleaned_data_join <- data_join %>%
  filter(!is.na(obs_value) & !is.na(Continent))

continent_summary <- cleaned_data_join %>%
  group_by(Continent) %>%
  summarise(total_obs_value = mean(obs_value, na.rm = TRUE))


continent_colors <- c("Asia" = "blue", "Europe" = "red", "Africa" = "green", "North America" = "purple", "South America" = "orange", "Oceania" = "yellow")


ggplot(continent_summary, aes(x = Continent, y = total_obs_value, fill = Continent)) +
  geom_bar(stat = "identity") +
  labs(x = "Continent", y = "% Children suffering 2 Deprivation", title = "Avg % of Children suffering 2 forms of Deprivation") +
  scale_fill_manual(values = continent_colors) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(size = 10)) 


