#Load necessary packages
library(tidyverse)
library(tidycensus)
library(ggplot2)
library(sf)
library(dplyr)

#Started by importing the csv file and renaming it airport_data. 
airport_data = read_csv("airport_pairs.csv")

# I filter the data to only include flights to and from RDU and created new data frames for it and then combine it
from_rdu_flights = airport_data %>% 
  filter(airport_data$origin == "RDU")

to_rdu_flights = airport_data %>% 
  filter(airport_data$dest == "RDU")

combined_flights = rbind(from_rdu_flights, to_rdu_flights)

# Then to find the most popular destinations I filtered only flights with greater than 10k passengers. 
all_popular_dest = combined_flights %>% 
  filter(passengers >= 10000)

all_popular_dest

# This is the most popular destination from RDU
popular_dest_from_rdu = from_rdu_flights %>% 
  filter(passengers >= 10000)

popular_dest_from_rdu
# most popular destination is from RDU ATL WITH 540190 passengers 



#QUESTION 2
# this code is getting the total population and median household income from census data 
census_data = get_acs(geography = "cbsa", variables = c('total_pop' = "B01003_001", 'median_income' = "B19013_001"), output = "wide", year = 2019)
census_data = census_data[-4]
census_data = census_data[-5]

#This code is selecting columns from the census data  frame and creating  two new data frames of origin_pop_data and dest_pop_data. FOr the population from a location (origin) to the a destination. 
# I am using the select function to  select three columns "GEOID", "total_popE", and "median_incomeE" fro both origin and destination.
origin_pop_data = census_data %>% 
  select(GEOID, origin_pop = total_popE, origin_median_income = median_incomeE) %>% 
  rename(origin_cbsa = GEOID)  

dest_pop_data = census_data %>% 
  select(GEOID, dest_pop = total_popE, dest_median_income = median_incomeE) %>% 
  rename(dest_cbsa = GEOID)

# Now I am merging airport_data_pop to origin_pop_data and dest_pop_data based in the CBSA codes. 
# I then removed any NA (missing values) in the airport_data_pop dataframe.
airport_data_pop <- merge(x = airport_data, y = origin_pop_data, by = "origin_cbsa", all.x = TRUE)
airport_data_pop <- merge(x = airport_data_pop, y = dest_pop_data, by = "dest_cbsa",  all.x = TRUE)
airport_data_pop = filter(airport_data_pop, !is.na(dest_cbsa) & !is.na(origin_cbsa))

# I then grouped the airport_data_pop dataframe by the origin and destination CBSA and calculates total passengers and mean distance for each group. 
both_cbsa = airport_data_pop %>% 
  group_by(origin_cbsa, dest_cbsa) %>%
  summarize(total_passengers = sum(passengers), distancemiles = mean(distancemiles))

#I am merging two data frames (origin_pop_data and dest_pop_data) with both_cbsa data frame based on the columns "origin_cbsa" and "dest_cbsa" using the merge function..
both_cbsa <- merge(x = both_cbsa, y = origin_pop_data, by = "origin_cbsa", all.x = TRUE)
both_cbsa <- merge(x = both_cbsa, y = dest_pop_data, by = "dest_cbsa", all.x = TRUE)

### I dont really need this code but kept it since I ended up using the metro_airport dataframe for the plots 
#This code is filtering out the rows in the both_cbsa dataframe where either the origin_cbsa or dest_cbsa values are missing and putting the filtered data to a new dataframe called metro_airports.
metro_airports <- both_cbsa %>% 
  filter(!is.na(origin_cbsa) & !is.na(dest_cbsa))

#The following are scatter plots
#This graph  looks at the metro_airports and the  original location population and total passengers from the origin and destination CBSA 
ggplot(metro_airports, aes(x = origin_pop, y = total_passengers)) +
  geom_point() +
  labs(title = "Origin Population vs. Total Passengers",
       x = "Origin CBSA Population",
       y = "Total Passengers")

#This graph  looks at the metro_airports and the destination location population and total passengers from the origin and destination CBSA 
ggplot(metro_airports, aes(x = dest_pop, y = total_passengers)) +
  geom_point() +
  labs(title = "Destination Population vs. Total Passengers",
       x = "Destination CBSA Population",
       y = "Total Passengers")

#I found online how geom_point can overlap the two graphs so I can better see the difference
ggplot(metro_airports, aes(x = origin_pop, y = total_passengers, color = "Origin")) +
  geom_point() +
  geom_point(aes(x = dest_pop, y = total_passengers, color = "Destination")) +
  labs(title = "Origin and Destination Population vs. Total Passengers",
       x = "CBSA Population",
       y = "Total Passengers",
       color = "CBSA Type") +
  scale_color_manual(values = c("Origin" = "blue", "Destination" = "red"))


#This graph  looks at the metro_airports and the destination miles travels and total passengers from the origin and destination CBSA 
ggplot(metro_airports, aes(x = distancemiles, y = total_passengers)) +
  geom_point() +
  labs(title = "Flight Distance vs. Total Passengers",
       x = "Miles traveled",
       y = "Total Passengers")

# Bonus: includes median household income from census data
#These two graphs looks at the metro_airports and the median income from the origin and destination and the total passengers from the origin and destination CBSA 

ggplot(metro_airports, aes(x = origin_median_income, y = total_passengers)) + 
  geom_point() + 
  labs(title = "Median Household income vs. Total popuation for Origin ", 
       x = "Median Household Income (Origin CBSA)", y = "Total Passengers")

ggplot(metro_airports, aes(x = dest_median_income, y = total_passengers)) + 
  geom_point() + 
  labs(title = "Median Household income vs. Total popuation for Destination ", 
       x = "Median Household Income (Destination CBSA)", y = "Total Passengers")

#I found a online that geom_point can over lap the two graphs so I could better see the difference
ggplot(metro_airports, aes(x = origin_median_income, y = total_passengers)) +
  geom_point(aes(color = "Origin")) +
  geom_point(aes(x = dest_median_income, color = "Destination")) +
  labs(title = "Median Household Income vs. Total Passengers",
       x = "Median Household Income",
       y = "Total Passengers",
       color = "CBSA") +
  scale_color_manual(values = c("blue", "red"))


#Q3

# this is the regression model using the grouped data (both_cbsa) looking at the total passenger, origin CBSA population, destination CBSA population, distance between cities, and median household income
model <- lm(total_passengers ~ origin_pop + dest_pop + distancemiles + origin_median_income + dest_median_income, data = both_cbsa)
summary(model)

#####This is from a wrong datafram i used
#The minimum error is -306980 and the maximum error is 1058371, indicating that there are some extreme values in the data.
#The p-values for all of the coefficients are less than 0.05, indicating that they are statistically significant predictors of demand.Unlikely to get result from randome chance. look at pos or negative coef. higher value associaeted with lower amount of travel 
#estimate of each shows show the effect of each of these variables on the predicted demand. How much change of dep variable is associated with chanfe in indepent variable 
#A positive coefficient is positive relationship between the variable and demand, while a negative coefficient indicates a negative relationship.
#### WRITE OUT EQUATION 
#Residual standard error: 127000 DOES THIS MEAN ITS WRONG BY THESE MANY PASSENGERSS? #### ignore 
#WHAT IS Adjusted R-squared:  0.1025 multiple r squared im expalining 10 % of variation. Adjusted adds a penalty for many variables. 

#Q4

# Create table with the cities
#This code is manipulating census data to look at metropolitan areas in cities in US. 
# In these codes I first select the metro cities and place them into a new data frame 
#This includes the GEOID, name, total population estimate, and median income estimate for the  areas.
#I am doing this for all but on origin_city_data and more_dest_city_data im adding in the routes Air Carolina is considering implementing.
# For dest_city_data and more_origin_city_data I am looking at just Raleigh-Cary, NC Metro Area. 

origin_city_data = census_data %>% 
  filter(NAME %in% c("Portland-Vancouver-Hillsboro, OR-WA Metro Area", "El Paso, TX Metro Area", "Tallahassee, FL Metro Area", "San Diego-Chula Vista-Carlsbad, CA Metro Area")) %>% 
  select(GEOID, NAME, total_popE, median_incomeE) %>% 
  rename(origin_cbsa = GEOID, origin_name = NAME, origin_pop = total_popE, origin_median_income = median_incomeE)

dest_city_data = census_data %>% 
  filter(NAME %in% c("Raleigh-Cary, NC Metro Area")) %>% 
  select(GEOID, NAME, total_popE, median_incomeE) %>% 
  rename(dest_cbsa = GEOID, dest_name = NAME, dest_pop = total_popE, dest_median_income = median_incomeE)

more_dest_city_data = census_data %>% 
  filter(NAME %in% c("Portland-Vancouver-Hillsboro, OR-WA Metro Area", "El Paso, TX Metro Area", "Tallahassee, FL Metro Area", "San Diego-Chula Vista-Carlsbad, CA Metro Area")) %>% 
  select(GEOID, NAME, total_popE, median_incomeE) %>% 
  rename(dest_cbsa = GEOID, dest_name = NAME, dest_pop = total_popE, dest_median_income = median_incomeE)

more_origin_city_data = census_data %>% 
  filter(NAME %in% c("Raleigh-Cary, NC Metro Area")) %>% 
  select(GEOID, NAME, total_popE, median_incomeE) %>% 
  rename(origin_cbsa = GEOID, origin_name = NAME, origin_pop = total_popE, origin_median_income = median_incomeE)

# I used the merged function to combine origin_city_data and dest_city_data, and store it in city_data
city_data = merge(x = origin_city_data, y = dest_city_data)
# I used the merged function to combine more_origin_city_data and more_dest_city_data, and store it in more_city_data
more_city_data = merge(x = more_origin_city_data, y = more_dest_city_data)
#Using rbind  i took the city_data and more_city_data data frames and combined it in finished_city_data. 
finished_city_data = rbind(city_data, more_city_data)

#create distance miles column
#These are mile values in distance miles 
distancemiles = c(1606,2363,2193,496,1606,2363,2193,496)
#i am then adding distancemiles values  as a new column to the finished_city_data data frame using the cbind function.
finished_city_data = cbind(finished_city_data, distancemiles)
#using model that already trained in  Q3, i am going to predict the total number of passengers between each origin and destination city pair in the finished_city_data data frame
# I am and adding these predicted values as a new column total_passengers to the finished_city_data data frame.
finished_city_data$total_passengers = predict(model, finished_city_data)
