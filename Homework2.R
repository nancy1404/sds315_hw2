# WHAT TO INSTALL BEFORE YOU START (in case you run codes not in order.)
library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr)
library(knitr)

# <Problem 1>

# PART A: create histogram to display overall data distributino of course evaluation scores.
library(tidyverse)
library(readr)
profs <- read_csv("SDS 315/profs.csv")

# histogram
ggplot(profs, aes(x=eval)) +
  geom_histogram(binwidth = 0.2, fill = "tomato", color = "black") +
  labs(
    title = "Distribution of Course Evaluation Scores",
    x = "Evaluation Score (1-5)",
    y = "Frequency"
  ) 


# PART B: use side-by-side boxplots to show distribution of course evaluation scores by whether or not professor is native English speaker.
library(ggplot2)

# side-by-side boxplots
ggplot(profs, aes(x = native, y = eval, fill = native)) +
  geom_boxplot() +
  labs(
    title = "Course Evaluation Scores by Professors' Native English Speaker Status",
    x = "Native English Speaker",
    y = "Course Evaluation Scores (1-5)"
  )

# for summary statistics for caption
mean_values <- profs |>
  group_by(native) |>
  summarize(mean_eval = mean(eval))


# PART C: use faceted histogram with two rows (male vs female)

# faceted histogram
ggplot(profs, aes(x=eval, fill=gender)) +
  geom_histogram(binwidth=0.2, color = "black") +
  facet_wrap(~gender, nrow = 2) +
  labs(
    title = "Distribution of Course Evaluation Scores by Instructor Gender",
    x = "Course Evaluation Scores (1-5)",
    y = "Count",
    fill = "Gender"
  )


# PART D: scatterplot 

# scatterplot
ggplot(profs, aes(x=beauty, y = eval)) +
  geom_point(alpha = 0.6, color = "black") +
  geom_smooth(method = "lm", color = "tomato", se = TRUE) +
  labs(
    title = "Relationship Between Physical Attractiveness and Course Evaluations",
    x = "Physical Attractiveness (Beauty Score)",
    y = "Course Evaluation Score"
  ) 

# <PROBLEM 2>

# PLOT A: line graph showing avg hourly bike rentals (total) across all hours of the day (hr)

bikeshare <- read_csv("SDS 315/bikeshare.csv")

# Data Wrangling: avg rentals by hour
avg_hour_rentals <- bikeshare |>
  group_by(hr) |>
  summarize(avg_rentals = mean(total))

# start of plot A
ggplot(avg_hour_rentals, aes(x=hr, y = avg_rentals)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Average Hourly Bike Rentals",
    x = "Hour of the Day (24-hour)",
    y = "Average Rented Bikes"
  )

# PLOT B: faceted line graph w/ avg bike rentals by hour of day
# faceted whether it's working day or not.

summary <- bikeshare |>
  group_by(hr, workingday) |>
  summarize(avg_rentals = mean(total))

# faceted line graph
ggplot(summary, aes(x=hr, y = avg_rentals, color = as.factor(workingday))) +
  geom_line() +
  facet_wrap(~workingday, labeller = labeller(workingday = c("0" = "Non-working Day", "1" = "Working Day"))) +
  scale_color_manual(
    name = "Day Type",
    values = c("0" = "pink", "1" = "skyblue"),
    labels = c("Non-working Day", "Working Day")
  ) +
  labs(
    title = "Average Hourly Bike Rentals by Hour and Working Day",
    x = "Hour of the Day (24-hour)",
    y = "Average Rented Bikes",
    color = "Day Type"
  ) 

# PLOT C: faceted bar plot avg ridership(y) during 9AM vs weather situation code (weathersit, x)
# faceted whether it's working day or not

summary_plotC <- bikeshare |>
  filter(hr == 9) |>
  group_by(weathersit, workingday) |>
  summarize(avg_ridership = mean(total)) |>
  mutate(weathersit = as.factor(weathersit))

# faceted bar plot
ggplot(summary_plotC, aes(x=weathersit, y = avg_ridership, fill = weathersit)) +
  geom_bar(stat = "identity")+ #take y values to avg_ridership
  facet_wrap(~workingday, labeller = labeller(workingday = c("0" = "Non-working Day", "1" = "Working Day"))) +
  scale_fill_manual(
    name = "Weather Situation",
    values = c("1" = "pink", "2" = "skyblue", "3" = "lightgreen"),
    labels = c("1: Clear/Partly Cloudy", "2: Mist/Cloudy", "3: Light Snow/Rain", "4: Heavy Rain/Snow")
  ) +
  labs(
    title = "Average Ridership During 9AM by Weather Situation and Day Type",
    x = "Weather Situation Code",
    y = "Average Rented Bikes",
    color = "Weather Situation",
    caption = "*weathersit code 4 (heavy rain/snow) is not found for filtered dataset."
  )

# <PROBLEM 3>

# 1st faceted graph: line graph plotting avg boarding by hour of day, day of week, and month

library(dplyr)
capmetro_UT <- read_csv("SDS 315/capmetro_UT.csv")

capmetro_UT = mutate(capmetro_UT, day_of_week = factor(day_of_week, 
                                                       levels=c("Mon", "Tue", "Wed","Thu", "Fri", "Sat", "Sun")),
                     month = factor(month, 
                                    levels=c("Sep", "Oct","Nov")))
avg_boarding <- capmetro_UT |>
  group_by(hour_of_day, day_of_week, month) |>
  summarize(avg_boarding = mean(boarding, na.rm = TRUE))

# faceted line graph
ggplot(avg_boarding, aes(x=hour_of_day, y = avg_boarding, color = month, group = month)) +
  geom_line() +
  facet_wrap(~day_of_week, nrow = 2) +
  scale_color_manual(values = c("Sep" = "orange", "Oct" = "black", "Nov" = "darkgrey")) +
  labs(
    title = "Average Boarding by Hour, Day of Week, and Month",
    x = "Hour of Day (24-hour format)",
    y = "Average Boardings",
    color = "Month"
  )

# 2nd faceted graph: scatter plot, boardings (y) vs. temp (x)
# faceted by hour of the day, with points colored according to whether it's weekday or weekend

capmetro_2 <- capmetro_UT |>
  select(boarding, temperature,hour_of_day, weekend) |>
  mutate(weekend = factor(weekend, levels = c("weekday", "weekend")))

# faceted scatterplot
ggplot(capmetro_2, aes(x=temperature, y = boarding, color = weekend)) +
  geom_point(alpha = 0.7, size = 1) +
  facet_wrap(~hour_of_day) +
  scale_color_manual(values = c("weekday" = "orange", "weekend" = "black")) +
  labs(
    title = "Boardings vs. Temperature by Hour of the Day",
    x = "Temperature (°F)",
    y = "Boardings",
    color = "Day Type"
  )


# PROBLEM 4

# PART A: make table of top 10 most popular songs since 1958
# measured by total # weeks that song spent on Billboard top 100

billboard <- read.csv("billboard.csv")
topsongs <- billboard |>
  group_by(performer, song) |>
  summarize(count = sum(weeks_on_chart, na.rm = TRUE), .groups="drop") |>
  arrange(desc(count)) |>
  slice_head(n=10)

library(knitr)
kable(topsongs, caption = "Top 10 most popular songs since 1958")


# PART B: is "musical diversity" of billboard top 100 changing over time?
# measure musical diversity of given year as number of unique songs that appeared in billboard top 100 that year.
# line graph that plots measure of musical diversity over years
# x axis: year / y axis: # unique songs on chart in any week of year

# filter to exclude 1958 and 2021

# filter-1
billboard_B <- billboard |>
  filter(year != 1958 & year != 2021)

#step 1
song_count <- billboard_B |>
  group_by(year, song) |>
  summarize(count = n(), .groups = "drop")

#step 2
unique_song_count <- song_count |>
  group_by(year) |>
  summarise(unique_songs = n_distinct(song), .groups = "drop")

# line graph
ggplot(unique_song_count, aes(x=year, y = unique_songs))+
  geom_line()+
  geom_point()+
  labs(
    title = "Musical Diversity of Billboard Top 100 Over Years",
    x = "Year",
    y = "Unique Songs (Count)"
  )


# PART C: define ten-week hit as single song that appeared on billboard top 100 for at least ten weeks
# 19 artists in US who had at least 30 songs that were ten-week hits.
# bar plot for 19 artists, showing how many ten-week hits each one had in musical career.

# ten-week hits??
ten_week_hits <- billboard |>
  group_by(performer, song) |>
  summarize(weeks_on_chart = max(weeks_on_chart, na.rm = TRUE), .groups = "drop") |>
  filter(weeks_on_chart >=10)

# step 1
hits_artist <- ten_week_hits |>
  group_by(performer) |>
  summarize(hits_artist_count = n(), .groups = "drop")

# step 2
artists_with_30_songs <- hits_artist |>
  filter(hits_artist_count >= 30) #found 19 artists

# bar plot
ggplot(artists_with_30_songs, aes(x=performer, y = hits_artist_count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Number of Hits for Artists with 30+ Ten-Week Hits",
    x = "Artist",
    y = "Number of Ten-Week Hits",
    caption = "* ten-week hit defines a song that appeared on the Billboard Top 100 for 10+ weeks."
  )




                     