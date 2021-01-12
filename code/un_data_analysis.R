library(tidyverse)

gapminder_data <- read_csv("data/gapminder_data.csv")
view(gapminder_data)


summarize(gapminder_data, averageLifeExp = mean(lifeExp))

# Piping function: %>%

gapminder_data %>% summarize(averageLifeExp = mean(lifeExp))

# Mean population of dataset

gapminder_data %>% summarize(meanPop = mean(pop),
                             recent_year = max(year))

# filter rows where year is 2007
gapminder_data %>% filter(year == 2007) %>% summarize(averageLifeExp = mean(lifeExp))

# Find average GDP per capita for teh first year in the dataset

gapminder_data %>% summarize(first_year = min(year))

gapminder_data %>% filter(year == 1952) %>%
  summarize(averageGDP = mean(gdpPercap))

#group_by
gapminder_data %>%
  group_by(year) %>%
  summarize(averageLifeExp = mean(lifeExp))

# Exerecise: find mean life expectancy for each continent

gapminder_data %>%
  group_by(continent) %>%
  summarize(averageLifeExp = mean(lifeExp))

# mutate - add more columns to the dataset
gapminder_data %>%
  mutate(gdp = gdpPercap * pop)

# Exercise: make a new column using mutate that is population in millions

gapminder_data %>%
  mutate(popMill = pop / 1000000)

year_pop <- gapminder_data %>%
  select(year, pop)


gapminder_data %>%
  select(-continent)

# Country, continent, year and lifeExp

data_select <- gapminder_data %>%
  select(country, continent, year, lifeExp)

# arrange(year)

# long vs wide
# pivot_longer, and pivot_wider
gapminder_data %>%
  select(country, continent, year, lifeExp) %>%
  pivot_wider(names_from = year, values_from = lifeExp)

# rename() rename columns

# Create a new dataset with only data from the Americas and 2007
# drop the continent and year columns

new_dataset <- gapminder_data %>%
  filter(year == 2007, continent == 'Americas') %>%
  select(-continent,-year)

gapminder_data <- read_csv("data/gapminder_data.csv") %>%
  filter(year == 2007, continent == 'Americas') %>%
  select(-continent,-year)


# Exercise: select only the country , year, series, value
# Goals: data from a year that is close to 2007
# A column for country and we want columns for different types of Co2 emissions(total, per capita emssions)
co2_emissions <- read_csv(
  "data/co2-un-data.csv",
  skip = 2,
  col_names = c(
    "region",
    "country",
    "year",
    "series",
    "value",
    "footnotes",
    "source"
  )
) %>%
  select(country, year, series, value) %>%
  mutate(
    series = recode(
      series,
      "Emissions (thousand metric tons of carbon dioxide)" = "totatl_emissions",
      "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions"
    )
  ) %>%
  pivot_wider(names_from = series, values_from = value) %>% 
  filter(year == 2005) %>% 
  select(-year) %>% 
mutate(country = recode(country,
                        "Bolivia (Plurin. State of)" = "Bolivia",
                        "United States of America" = "United States",
                        "Venezuela (Boliv. Rep. of)" = "Venezuela"))

View(co2_emissions)

# joining the dataset
# inner_join()
inner_join(gapminder_data,co2_emissions, by = 'country')
gapminder_data %>% inner_join(gapminder_data,co2_emissions, by = 'country')

anti_join(gapminder_data,co2_emissions)

gapminder_data <- read_csv("data/gapminder_data.csv") %>% 
  filter(year == 2007, continent == "Americas") %>% 
  select(-continent,-year) %>% 
  mutate(country = recode(country,"Puerto Rico" = "United States")) %>% 
  group_by(country) %>% 
  summarize(lifeExp = sum(lifeExp * pop)/sum(pop),
            gdpPercap = sum(gdpPercap * pop)/sum(pop),
            pop = sum(pop))

anti_join(gapminder_data,co2_emissions)
inner_join(gapminder_data,co2_emissions, by = 'country')

gapminder_co2 <- inner_join(gapminder_data,co2_emissions, by = 'country')

gap_co2_region <- gapminder_co2 %>% 
  mutate(region = if_else(country == "Canada" |
                            country == "United States" | country == "Mexico","north","south"))
# is there a relationship between gdp and co2
# Exercise a scatter plot of gdp vs co2 emissions, color it by region

ggplot(data = gap_co2_region) +
  aes(x = gdpPercap, y = per_capita_emissions, color = region) + 
  geom_point()

