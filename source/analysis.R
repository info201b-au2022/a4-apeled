library(tidyverse)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#
incarceration_df <- get_data()

# Data wrangling function that references "get_data()" and returns data frame 
# for summary list
get_imprison_data <- function() {
  imprison <- incarceration_df %>%
    group_by(year) %>%
    mutate(total_year_jail_pop = sum(total_jail_pop, na.rm = TRUE)) %>%
    distinct(year, total_year_jail_pop) %>%
    mutate_if(is.numeric, round)%>%
    select(total_year_jail_pop, year)
  return(imprison)
}

# Data wrangling function that references "get_data()" and returns data frame 
# for summary list
get_state_imprison <- function(){
  state_imprison <- incarceration_df %>%
    group_by(state) %>%
    mutate(total_state_jail_pop = sum(total_jail_pop, na.rm = TRUE)) %>%
    mutate(total_state_pop = sum(total_pop, na.rm = TRUE)) %>%
    distinct(total_state_jail_pop, total_state_pop, state) %>%
    mutate_if(is.numeric, round)%>%
    select(total_state_jail_pop, total_state_pop, state)
  return(state_imprison)
}

# Function that complies the summary list and references get_imprison_data and
# 
get_summary_list <- function(){
  imprison <- get_imprison_data()
  state_imprison <- get_state_imprison()
  summary_info <- list()
  summary_info$max_imprison_pop <- max(imprison$total_year_jail_pop, na.rm = TRUE)
  summary_info$max_imprison_year <- imprison %>%
    filter(total_year_jail_pop == summary_info$max_imprison_pop) %>%
    pull(year)
  summary_info$max_imprison_pop_state <- max(state_imprison$total_state_jail_pop, na.rm = TRUE)
  summary_info$max_imprison_state <- state_imprison %>%
    filter(total_state_jail_pop == summary_info$max_imprison_pop_state) %>%
    pull(state)
  summary_info$total_pop <- state_imprison %>%
    filter(total_state_jail_pop == summary_info$max_imprison_pop_state) %>%
    pull(total_state_pop)
  
  summary_info$total_pop_comp <- (100 * summary_info$max_imprison_pop_state/summary_info$total_pop)
  return(summary_info)
  
}

sum <- get_summary_list()
  
## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population

# This a data wrangling function that returns a data frame that is suitable for 
# visualization. The function groups the data by 'year' and calculates the sum 
# 'total_jail_pop' for each location. This function takes in no parameters. 
get_year_jail_pop <- function() {
  df <- incarceration_df %>%
    group_by(year) %>%
    mutate(total_year_jail_pop = sum(total_jail_pop, na.rm = TRUE)) %>%
    distinct(year, total_year_jail_pop)
return(df)   
}

# This function is a plotting function that returns the chart for Increase of
# Jail Population U.S. (1970-2018). This function takes no parameters and
# calls the data wrangling function "get_year_jail_pop" as it uses the data frame
# constructed there.
plot_jail_pop_for_us <- function()  {
  # Removes scientific notation
  options(scipen = 999)
  
  # Formulating the labels for the plot
  labels <- labs(
    x = "Year",
    y = "Total Jail Population",
    title = "Increase of Jail Population U.S. (1970-2018)",
    caption = "Figure 1. This chart shows change in jail population per year from the calcuated totals of each loaction.",
    alt = "Increase of Jail Population U.S. (1970-2018)"
  )
  
  # Code for the plot
  plot1 <- ggplot(get_year_jail_pop(), aes(x = year, y = total_year_jail_pop)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(labels = scales::comma) +
    labels
  
  return(plot1)   
} 
#----------------------------------------------------------------------------#

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 

# This a data wrangling function that returns a data frame that is suitable for 
# visualization. The function filters the main data frame to only rows with the
# desired states, groups the data by 'year' and 'state' and calculates the sum 
# 'total_jail_pop' for each state. This function takes in a parameter called
# 'states' which is a vector containing stings. 
get_jail_pop_by_states <- function(states) {
  df <- incarceration_df %>% 
    filter(str_detect(incarceration_df[['state']], paste(states, collapse = "|"))) %>%
    group_by(state, year) %>%
    mutate(state_year_jail_pop = sum(total_jail_pop, na.rm = TRUE)) %>%
    distinct(year, state_year_jail_pop, state)
  return(df)
}

# This function is a plotting function that returns the chart for Increase of
# Jail Population for each state (1970-2018). This function takes in a parameter
# called 'state' that is a vector containing strings and calls the data 
# wrangling function "get_jail_pop_by_year" as it uses the data frame
# constructed there.
plot_jail_pop_by_states <- function(states) {
  # Removes scientific notation
  options(scipen = 999)
  
  # Formulating the labels for the plot
  labels <- labs(
    x = "Year",
    y = "Total Jail Population",
    title = "Increase of Jail Population in Different States (1970-2018)",
    caption = "Figure 2. This chart shows change in jail population per year from the calcuated totals of each state.",
    alt = paste("Increase of Jail Population in", states, "(1970-2018)", sep = " ")
  )
  
  # Code for the plot
  plot2 <- ggplot(get_jail_pop_by_states(states), aes(x=year, y=state_year_jail_pop, group=state, color=state)) +
    geom_line(stat = "identity") +
    scale_y_continuous(labels = scales::comma) +
    labels

  return(plot2)
}

#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# Installing packages "viridis" and "hrbrthemes"
# install.packages("hrbrthemes")
# install.packages('viridis')
# install.packages("patchwork")

# Loading libraries
library(viridis)
library(hrbrthemes)
library(patchwork)


# This a data wrangling function that returns a data frame that is suitable for 
# visualization. The function groups the data by 'year' and 'urbanicity' and 
# calculates the sum 'male_jail_pop' and 'female_jail_pop' for each urbanicity.
# The function also converts the total to a value that is jail population per
# 10,000 citizens to allow for comparison in the visualization This function
# takes in no parameters. 

get_jail_pop_by_gender <- function() {
  male <- incarceration_df %>%
    group_by(urbanicity, year) %>%
    mutate(male_year_jail_pop_10k = (sum(male_jail_pop, na.rm = TRUE)/10000)) %>%
    distinct(year, male_year_jail_pop_10k, urbanicity)
  
  join_gender <- incarceration_df %>%
    group_by(urbanicity, year) %>%
    mutate(female_year_jail_pop_10k = (sum(female_jail_pop, na.rm = TRUE)/10000)) %>%
    distinct(year, female_year_jail_pop_10k, urbanicity) %>%
    left_join(male, by = c("year", "urbanicity")) %>%
    filter(female_year_jail_pop_10k > 0) %>%
    mutate(urbanicity2=urbanicity)
  
  return(join_gender)
}

# This function is a plotting function that returns the visual for Increase of
# Male and Female Jail Population U.S. (1970-2018). This function takes no 
# parameters and calls the data wrangling function "get_jail_pop_by_gender" as it
# uses the data frame constructed there.

plot_jail_pop_by_gender <- function(){
  
  labels_male <- labs(
    x = "Year",
    y = "Male Jail Population Per 10,000 Ciztens",
    title = "Increase of Male Jail Population in Different Urbanicities (1970-2018)",
    caption = "Figure 3. This chart shows change in male jail population per 10,00 citizens from the calcuated totals of each urbanicity",
    alt = "Increase of Male Jail Population in Different Urbanicities (1970-2018)"
  )
  
  labels_female <- labs(
    x = "Year",
    y = "Female Jail Population Per 10,000 Ciztens",
    title = "Increase of Female Jail Population in Different Urbanicities (1970-2018)",
    caption = "Figure 4. This chart shows change in female jail population per 10,00 citizens from the calcuated totals of each urbanicity",
    alt = "Increase of Female Jail Population in Different Urbanicities (1970-2018)"
  )
  
  join_gender <- get_jail_pop_by_gender()
  
  chart3 <- join_gender%>%
    ggplot( aes(x=year, y=male_year_jail_pop_10k)) +
    geom_line( data=join_gender %>% dplyr::select(-urbanicity), aes(group=urbanicity2), color="grey", size=0.5, alpha=0.5) +
    geom_line( aes(color=urbanicity), color="#69b3a2", size=1.2 )+
    scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
    scale_color_viridis(discrete = TRUE) +
    theme_ipsum() +
    theme(
      legend.position="none",
      plot.title = element_text(size=14),
      panel.grid = element_blank()
    ) +
    labels_male +
    facet_wrap(~urbanicity)
  
  chart4 <- join_gender%>%
    ggplot( aes(x=year, y=female_year_jail_pop_10k)) +
    geom_line( data=join_gender %>% dplyr::select(-urbanicity), aes(group=urbanicity2), color="grey", size=0.5, alpha=0.5) +
    geom_line( aes(color=urbanicity), color="#69b3a2", size=1.2 )+
    scale_x_discrete(guide = guide_axis(check.overlap = TRUE, n.dodge=2))+
    scale_color_viridis(discrete = TRUE) +
    theme_ipsum() +
    theme(
      legend.position="none",
      plot.title = element_text(size=14),
      panel.grid = element_blank()
    ) +
    labels_female +
    facet_wrap(~urbanicity)
  return(chart3 + chart4 + 
           plot_layout(ncol=1, widths = 2, heights = 6))
}

#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# Load the library "maps"
library(maps)

# This a data wrangling function that returns a data frame that is suitable for 
# visualization. The function uses the map data provided by R for each state.
# The 'incarceration_df' was wranggled to return certain values 
# calculates the sum 'male_jail_pop' and 'female_jail_pop' for 2018.
# A column called "region" was created to which the inner_join was applied to 
# combine the map data.

get_jail_pop_by_gender_map <- function(){
  
  us_states_map_data <- map_data("state")
  
  final_data <- incarceration_df %>%
    mutate(region = tolower(state.name[match(state, state.abb)])) %>%
    filter(year == 2018) %>%
    group_by(state) %>%
    mutate(male_year_jail_pop_10k = (sum(male_jail_pop, na.rm = TRUE)/10000)) %>%
    mutate(female_year_jail_pop_10k = (sum(female_jail_pop, na.rm = TRUE)/10000)) %>%
    distinct(region, male_year_jail_pop_10k, female_year_jail_pop_10k)
  
  merged_data <- inner_join(us_states_map_data, final_data, by='region')
  
  return(merged_data)
}

# This function is a plotting function that returns the visual for Jail 
# Population Ratio (Male to Female) in the Mainland United States in 2018. This 
# function takes no parameters and calls the data wrangling function 
# "get_jail_pop_by_gender_map" as it uses the data frame constructed there.
plot_jail_pop_by_gender_map <- function(){
  merged_data <- get_jail_pop_by_gender_map()
  
  chart5 <- ggplot() +
    geom_polygon(data=merged_data, 
                  aes(x=long, y=lat, group=group, fill = male_year_jail_pop_10k/female_year_jail_pop_10k), 
                  color="white", size = 0.2) +
    scale_fill_continuous(name="Male to Female Ratio", 
                        low = "lightgreen", high = "darkblue", na.value = "grey50") +
    
    labs(title="Jail Population Ratio (Male to Female) in the Mainland United States in 2018",
         x="Longitude",
         y="Latitude")
  return(chart5)
}

#----------------------------------------------------------------------------#

## Load data frame ---- 


