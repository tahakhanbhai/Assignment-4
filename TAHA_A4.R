#ASSIGNMENT 4
#TAHA KHANBHAI

#Load in the dplyr library for tidyverse functions
library(dplyr)

# Read the data into a data frame (make sure that column names do not have spaces in them).
#Used trimws() to ensure the column names do not have any spaces in them
#Used na.strings to identify cells with empty strings as NA. This makes it easier to handle for future use. 
ufo <- read.csv(trimws('ufo_subset.csv'), na.strings = c("", "NA"))

#Don't forget to visually inspect and compare your data frame to the original csv to make sure that all data is loaded as expected.
str(ufo)
summary(ufo)

#Find the rows where Shape information is missing and impute with "unknown".
#Created new dataframe called ufo1, and used mutate() function to edit the shape column. The ifelse() function states that if shape is NA, impute the string 'unknown', else, just keep the initial shape value. 
ufo1 <- ufo %>%
  mutate(shape = ifelse(is.na(shape), "unknown", shape)) %>%
  #Remove the rows that do not have Country information. 
  #Used pipe to update ufo1 as well. filter(!is.na(country)) function keeps all the rows that do not have an NA value in the country column and assigns it to ufo1. 
  filter(!is.na(country))

#Convert Datetime and Date_posted columns into appropriate formats. 
#strptime() function converts characters into dates, if the format is inputted. 
#I used the as.Date() function to convert the date_posted column into the class date.
#For the datetime column, since there is a date and time, I converted them into "POSIXlt" and "POSIXt" classes, because of the additional time value. 
#If I used the as.Date() function for datetime, the time value disappears from the dataset, which is not ideal. 
ufo1$date_posted <- as.Date(strptime(ufo1$date_posted, format = "%d-%m-%Y"))
ufo1$datetime <- strptime(ufo1$datetime, format = "%Y-%m-%d %H:%M")

#NUFORC officials comment on sightings that may be hoax.  Figure out a way (go through the Comments and decide how a proper filter should look like) to identify possible hoax reports. Create a new boolean column "is_hoax", and populate this column with TRUE if the sighting is a possible hoax, FALSE otherwise.
#Initially, create a string that contains a regex of all the words that could indicate the sighting is a hoax if found in the comments. Words are separated by | so that all words are read by grepl. 
#Mutate function is used to create a new column called is_hoax. 
#grepl works so that if any of the words in hoax_comment string is found in the comments column, the is_hoax column will read TRUE. Otherwise, it would read FALSE.  
hoax_comment <- "HOAX| hoax | Hoax | fake | FAKE | Fake | False | false"
ufo2 <- ufo1 %>%
  mutate(is_hoax = grepl(hoax_comment, ufo1$comments))
  
#Create a table reporting the percentage of hoax sightings per country.
#Called upon ufo2 dataset, with the is_hoax column. 
#First I grouped the data by country, and used the summarize function to create new columns I could use in the dataset to calculate the percentage of hoaxes per country
#sightings = n() counts the total number of sightings per country
#hoax_count sums all the cells in which is_hoax = TRUE per country
#hoax_percentage calculates the percentage of hoaxes per country and adds to a new column. 
hoax_per_country <- ufo2 %>%
  group_by(country) %>%
  summarize(sightings = n(),
            hoax_count = sum(is_hoax),
            hoax_percentage = (hoax_count / sightings) * 100) 
#Remove the sightings and hoax_count columns from the new table. Select() function allows you to choose which columns to keep.
hoax_per_country <- hoax_per_country %>%
  select(country, hoax_percentage)

#Add another column to the dataset (report_delay) and populate with the time difference in days, between the date of the sighting and the date it was reported.
#Mutate function is used to create the report_delay column and add in the value obtained when you find the difference between datetime and date_posted. 
#as.numeric(date_posted) returns a number that corresponds to the specific date in that cell. 
#For datetime, I converted the class from "POSIXlt" and "POSIXt" to a date as well using the as.Date() function. This reomves the time value to allow for the subtraction. 
ufo3 <- ufo2 %>%
  mutate(report_delay = as.numeric(date_posted) - as.numeric(as.Date(datetime))) %>%
#Remove the rows where the sighting was reported before it happened.
#If the sighting was reported before the report, the report_delay value will be a negative value. The filter function returns all the rows where the report_delay value is greater than or equal to 0. 
  filter(report_delay >= 0)

#Create a table reporting the average report_delay per country.
#Grouped by country
#total_report_delay sums all the cells of the report_delay column per country
#total country = n() returns the counts of each country
#The mean is manually calculated by dividing total_report_delay by total_country, and the values are added to the new column avg_report_delay. 
delay_report <- ufo3 %>%
  group_by(country) %>%
  summarize(total_report_delay = sum(report_delay),
            total_country = n(),
            avg_report_delay = total_report_delay / total_country)
#total_report_delay and total_country can be removed from the table using select
delay_report <- delay_report %>%
  select(country, avg_report_delay)

#Range of the duration.seconds column is observed through the summary() function. The values range from 0 - 82800000 seconds, which is a plausible range of values given that some individuals wait a long period of time before reporting their UFO sighting. 
#The class() function was used to determine that the values in the duration.seconds() column were numerical, which was as expected. 
#There are no NA values observed in the duration.seconds column. 
#One problem identified in the duration.seconds column that could be fixed is the structure of the numbers in the column. Some numbers have multiple decimal places, while some have one, and other numbers are integers. 
summary(ufo$duration.seconds) #Find range of values. 
class(ufo$duration.seconds) #Determine class of values. 
sum(is.na(ufo$duration.seconds)) #Sum the number of NA values in the column

#Create a histogram using the "duration seconds" column.
#Normal histogram of data shows heavy skew. 
hist(ufo3$duration.seconds)
#So, I applied a log distribution to the data so that it resembles a more normal distribution that is easier to read. 
hist(log10(ufo3$duration.seconds), main = "Histogram of duration.seconds column", xlab = "log10 of duration.seconds", ylab = "Frequency")






