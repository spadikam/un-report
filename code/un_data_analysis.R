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
  select(year,pop)


gapminder_data %>% 
  select(-continent)

# Country, continent, year and lifeExp

data_select <- gapminder_data %>% 
  select(country,continent,year,lifeExp)

# arrange(year)

# long vs wide
# pivot_longer, and pivot_wider
gapminder_data %>% 
  select(country,continent,year,lifeExp) %>% 
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






