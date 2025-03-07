# 259 Homework - exploratory data analysis + integrating skills
# For full credit, answer at least 8/10 questions
# List students working with below:

install.packages("DataExplorer")
library(tidyverse)
library(lubridate)
library(DataExplorer)

#> These data are drawn from the fivethirtyeight article:
#> http://fivethirtyeight.com/features/what-12-months-of-record-setting-temperatures-looks-like-across-the-u-s/
#> The directory us-weather-history contains a data file for each of 10 cities, labelled by their station name
#> Each data file contains:
#> `date` | The date of the weather record, formatted YYYY-M-D
#> `actual_mean_temp` | The measured average temperature for that day
#> `actual_min_temp` | The measured minimum temperature for that day
#> `actual_max_temp` | The measured maximum temperature for that day
#> `average_min_temp` | The average minimum temperature on that day since 1880
#> `average_max_temp` | The average maximum temperature on that day since 1880
#> `record_min_temp` | The lowest ever temperature on that day since 1880
#> `record_max_temp` | The highest ever temperature on that day since 1880
#> `record_min_temp_year` | The year that the lowest ever temperature occurred
#> `record_max_temp_year` | The year that the highest ever temperature occurred
#> `actual_precipitation` | The measured amount of rain or snow for that day
#> `average_precipitation` | The average amount of rain or snow on that day since 1880
#> `record_precipitation` | The highest amount of rain or snow on that day since 1880

stations <- c("KCLT", "KCQT", "KHOU", "KIND", "KJAX", "KMDW", "KNYC", "KPHL", "KPHX", "KSEA")
cities <- c("Charlotte", "Los Angeles", "Houston", "Indianapolis", "Jacksonville", 
            "Chicago", "New York City", "Philadelphia", "Phoenix", "Seattle")


# QUESTION 1
#> The data files are in the directory 'us-weather-history'
#> Write a function that takes each station abbreviation and reads
#> the data file and adds the station name in a column
#> Make sure the date column is a date
#> The function should return a tibble
#> Call the function "read_weather" 
#> Check by reading/glimpsing a single station's file

#creating function 
read_weather <- read_csv("us-weather-history/KCLT.csv")

read_weather <- function(i) {
  read_csv(str_glue("us-weather-history/{i}.csv")) %>% 
  mutate(date = ymd(date)) %>% 
    mutate(station = i)
}

#reading single station's file
read_weather("KCLT")



# QUESTION 2
#> Use map() and your new function to read in all 10 stations
#> Note that because map_dfr() has been superseded, and map() does not automatically bind rows, you will need to do so in the code.
#> Save the resulting dataset to "ds"

#reading in all 10 stations
ds <- map(stations, read_weather) %>% bind_rows()
View(ds)
glimpse(ds)

# QUESTION 3
#> Make a factor called "city" based on the station variable
#> (station should be the level and city should be the label)
#> Use fct_count to check that there are 365 days of data for each city 

#making factor
ds <- ds %>% mutate(city = fct_relabel(factor(station), ~paste("city", .)))

#checking days of data for each city
ds %>% count(city) %>% filter(n == 365)


# QUESTION 4
#> Since we're scientists, let's convert all the temperatures to C
#> Write a function to convert F to C, and then use mutate across to 
#> convert all of the temperatures, rounded to a tenth of a degree

#creating function
celsius <- function(fahrenheit) {
  (fahrenheit - 32) * 5 / 9
}

#converting all temperatures
ds <- ds %>% mutate(across(ends_with("temp"), ~round(celsius(.), 1)))

head(ds)
View(ds)


### CHECK YOUR WORK
#> At this point, your data should look like the "compiled_data.csv" file
#> in data-clean. If it isn't, read in that file to use for the remaining
#> questions so that you have the right data to work with.

# QUESTION 5
#> Write a function that counts the number of extreme temperature days,
#> where the actual min or max was equal to the (i.e., set the) record min/max
#> A piped function starting with '.' is a good strategy here.
#> Group the dataset by city to see how many extreme days each city experienced,
#> and sort in descending order to show which city had the most:
#> (Seattle, 20, Charlotte 12, Phoenix 12, etc...)
#> Don't save this summary over the original dataset!

#creating function
extreme_temp <- function(ds) {
  ds %>%
    summarise(extreme_days = sum((actual_min_temp == record_min_temp) | (actual_max_temp == record_max_temp), na.rm = TRUE)) %>%
    pull(extreme_days)
}

extreme_temp(ds)

#grouping dataset by city
ds %>% group_by(city) %>% summarize(extreme_days = extreme_temp(.)) %>% arrange(desc(extreme_days))

#did something incorrect here.. I'm getting 20 extreme days for each city.. but I think there should be more variation?



# QUESTION 6
#> Pull out the month from the date and make "month" a factor
#> Split the tibble by month into a list of tibbles 

ds <- ds %>% mutate(month = factor(month(date)))

split_by_month <- split(ds, ds$month)
print(split_by_month)


# QUESTION 7
#> For each month, determine the correlation between the actual_precipitation
#> and the average_precipitation (across all cities), and between the actual and average mins/maxes
#> Use a for loop, and print the month along with the resulting correlation
#> Look at the documentation for the ?cor function if you've never used it before


for (month_data in split_by_month) {
  month_name <- unique(month_data$month)
  precip_corr <- cor(month_data$actual_precipitation, month_data$average_precipitation, use = "complete.obs")
  min_corr <- cor(month_data$actual_min_temp, month_data$average_min_temp, use = "complete.obs")
  max_corr <- cor(month_data$actual_max_temp, month_data$average_max_temp, use = "complete.obs")
  
  cat("Month:", month_name, "\n")
  cat("Precipitation Correlation:", precip_corr, "\n")
  cat("Min Temperature Correlation:", min_corr, "\n")
  cat("Max Temperature Correlation:", max_corr, "\n")
  cat("\n")  
}



# QUESTION 8
#> Use the Data Explorer package to plot boxplots of all of the numeric variables in the dataset
#> grouped by city, then do the same thing grouped by month. 
#> Finally, use plot_correlation to investigate correlations between the continuous variables only
#> Check the documentation for plot_correlation for an easy way to do this


ds <- ds %>%
  mutate(month = factor(month(date, label = TRUE, abbr = FALSE), levels = month.name))


ds %>%
  group_by(city) %>%
  plot_boxplot()

ds %>%
  group_by(month) %>%
  plot_boxplot()

plot_correlation(ds)


# QUESTION 9
#> Create a scatterplot of actual_mean_temp (y axis) by date (x axis)
#> Use facet_wrap to make a separate plot for each city (3 columns)
#> Make the points different colors according to month




# QUESTION 10
#> Write a function that takes the dataset and the abbreviate month as arguments
#> and creates a scatter and line plot of actual temperature (y axis) by date (x axis)
#> Note, just add geom_line() to your ggplot call to get the lines
#> use the ggtitle() function to add the month as a title
#> The function should save the plot as "eda/month_name.png"
#> The eda folder has an example of what each plot should look like
#> Call the function in a map or loop to generate graphs for each month


