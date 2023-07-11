#You can find the dataset to use for this assignment (ufo_subset.csv) under Module Session 7 on your Quercus course page.

#The original dataset contains over 80,000 reports of UFO sightings over the last century, from 1906 to 2014. This dataset was scraped, geolocated, and time standardized from NUFORC data by Sigmond Axel and downloaded from kaggle.com. The file in the dataset is tab delimited and contains a header row.

#For the purposes of this assignment, I took a subset of the original dataset, from 2010 to 2014, and did a little bit of cleaning to ensure you have an easier task.

#Data Dictionary

#datetime: Contains date and time of sighting
#city: City in which UFO was sighted
#state: State code in which UFO was sighted
#country: Country code of sighting
#shape: Shape of the UFO
#duration seconds: Duration of the sighting in seconds
#duration hours min: Duration of the sighting in hours and min
#comments: Sighting description
#date _osted: Posted date of the sighting
#latitude: Latitude coordinate of the sighting
#longitude: Longitude coordinate of the sighting
#Tasks

#NUFORC officials comment on sightings that may be hoax.  Figure out a way (go through the Comments and decide how a proper filter should look like) to identify possible hoax reports. Create a new boolean column "is_hoax", and populate this column with TRUE if the sighting is a possible hoax, FALSE otherwise.

#Write a script that accomplishes the following tasks:
library(dplyr)
# Read the data into a data frame (make sure that column names do not have spaces in them).
#Used trimws() to ensure the column names do not have any spaces in them
#Used na.strings to identify cells with empty strings as NA. 
ufo <- read.csv(trimws('ufo_subset.csv'), na.strings = c("", "NA"))

#Don't forget to visually inspect and compare your data frame to the original csv to make sure that all data is loaded as expected.
str(ufo)
summary(ufo)

#Find the rows where Shape information is missing and impute with "unknown".
ufo1 <- ufo %>%
  mutate(shape = ifelse(is.na(shape), "unknown", shape)) %>%
  #Remove the rows that do not have Country information.
  filter(!is.na(country))
#BONUS: For some of the rows where Country column is blank, the country name is found in the City column in brackets. Where Country info is missing, try to extract the information in brackets in the City column and impute that value in the Country column.
#Convert Datetime and Date_posted columns into appropriate formats
ufo1$date_posted <- as.Date(strptime(ufo1$date_posted, format = "%d-%m-%Y"))
ufo1$datetime <- strptime(ufo1$datetime, format = "%Y-%m-%d %H:%M")
#NUFORC officials comment on sightings that may be hoax.  Figure out a way (go through the Comments and decide how a proper filter should look like) to identify possible hoax reports. Create a new boolean column "is_hoax", and populate this column with TRUE if the sighting is a possible hoax, FALSE otherwise.

#Create a regex of words that when found in the comment section, likely indicate that the UFO sighting is a hoax. Words are seperated by the or | character, so that all words aread in the grepl function. 
hoax_comment <- "HOAX| hoax | fake | FAKE | Fake | False | false"
ufo2 <- ufo1 %>%
  mutate(is_hoax = grepl(hoax_comment, ufo1$comments))
  
#Create a table reporting the percentage of hoax sightings per country.
hoax_per_country <- ufo2 %>%
  group_by(country) %>%
  filter(is_hoax) %>%
  summarise(percent = n())

#Add another column to the dataset (report_delay) and populate with the time difference in days, between the date of the sighting and the date it was reported.
ufo3 <- ufo2 %>%
  mutate(report_delay = date_posted - datetime)
  
#Remove the rows where the sighting was reported before it happened.
#Create a table reporting the average report_delay per country.
#Check the data quality (missingness, format, range etc) of the "duration seconds" column. Explain what kinds of problems you have identified and how you chose to deal with them, in your comments.
#Create a histogram using the "duration seconds" column#.