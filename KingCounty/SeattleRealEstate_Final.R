#KING COUNTY REAL ESTATE
#https://www.kaggle.com/harlfoxem/housesalesprediction

library(tidyverse)
library(tidymodels)
library(gridExtra)
library(caret)
library(data.table)
library(tidyr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(ggpubr)
library(ggrepel)
#install.packages("corrplot")
library(corrplot)
library(Amelia) #missingness map
library(lubridate)
library("cowplot")
library("maps")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")


### colour information
# https://rdrr.io/cran/RColorBrewer/man/ColorBrewer.html
#RColorBrewer package for changing the colours on the map
library(RColorBrewer)
display.brewer.all()  

#displaing the colours used in this project for easy reference later
plot_Size + scale_color_brewer(palette="Dark2") #new pallate
# View a single RColorBrewer palette by specifying its name
display.brewer.pal(n = 8, name = 'Dark2')
# Hexadecimal color specification 
brewer.pal(n = 8, name = "Dark2")
brewer.pal(n = 8, name = "RdYlGn")


##############################
### BRINGING IN THE DATA
##############################

#In this project we will be looking at the King County data set of house prices.
#bring the data in
KingCounty <- read_csv('KingCounty/kc_house_data.csv')
head(KingCounty)
names(KingCounty) #21 columns of info including...
nrow(KingCounty) #21,613 observations
#view(KingCounty)

summary(KingCounty)
#mid 2014 to mid 2015
#prices from $75,000 to $7,700,000 USD

QuantileKingCounty <- quantile(KingCounty$price, c(.01, .05, .10, .50, .90, .95, .99))
saveRDS(QuantileKingCounty , file = "QuantileKingCounty.rds")

##############################
### EXPLORING THE DATASET
##############################

#missingness map - evrything is there!
#this will show us if any observations are missing data
missmap(KingCounty) 


#weekly sales
#looking at the weekly sales, this is fairly expected  to see more activity in the summer months and less activity near Christmas and through the winter
SaleDate <- KingCountyClean %>%
  group_by(date) %>%
  summarize (num = n(),
             date2 = as.Date(date))

#need to change to a date format and group by week in order to plot
SaleDate$week <- floor_date(SaleDate$date2, "week")

WeeklySalesPlot <- ggplot(data=SaleDate) +
  geom_bar(aes(x=week), fill="#7570B3") +
  ggtitle("Weekly Sales") + 
  xlab("Week") +
  ylab("Number of Sales")
saveRDS(WeeklySalesPlot, file = "WeeklySalesPlot.rds")


#correlation of variables
#this will show us which variables are highly correlated
KingCounty_Select <- KingCounty %>%
  select(price,bedrooms, bathrooms, sqft_living, sqft_lot, floors, view, condition, grade, sqft_above, sqft_basement, yr_built, yr_renovated, waterfront)

KingCounty_Select.cor <- cor(KingCounty_Select, use="pairwise.complete.obs")

corrplot(KingCounty_Select.cor)
#from this plot, it looks like price is highly correlated with bedrooms, bathrooms, sqft_living, view and grade.  It is also correlated with sqft_above and sqft_basement but this is also covered by sqft_living.


  
# to create some visualizations that describe characteristics of the average price and frequency by number of bedrooms, it is easiest to first create a small dataframe with this info
BedSummary <- KingCounty %>%
  group_by(bedrooms) %>%
  summarize (num = n(), 
             priceav = mean(price)) %>%
  arrange(desc(priceav))


BedOutliers <- KingCounty %>%
  group_by(bedrooms) %>%
  mutate (num = n(), 
          priceav = mean(price),
          delta = abs(price-priceav)
          ) %>%
  select(id, price, bedrooms, priceav, delta) %>%
  arrange(desc(delta))
# look deeper at id 6762700020 which is $6.8million higher than the average

#Average price for number of bedrooms
plot_BedsPrice01 <- BedSummary %>%
  #arrange(priceav) %>%
  #mutate(city=factor(city, levels=city)) %>%
  ggplot(aes(x= reorder(bedrooms, bedrooms))) + 
  geom_bar(aes(weight = priceav), fill="#7570B3") +
  ggtitle("Bedrooms and Average Price") + 
  xlab("Number of Bedrooms") +
  ylab("Average Price (USD)") +
  scale_y_continuous(labels = dollar_format()) +
  coord_flip()


#Frequency for number of bedrooms
plot_BedsFrequency01 <- BedSummary %>%
  #arrange(priceav) %>%
  #mutate(city=factor(city, levels=city)) %>%
  ggplot(aes(x= reorder(bedrooms, bedrooms))) + 
  geom_bar(aes(weight = num), fill="#7570B3") +
  ggtitle("Frequncy of Number of Bedrooms") + 
  xlab("Number of Bedrooms") +
  ylab("Frequency") +
  scale_y_continuous(labels = comma) +
  coord_flip()


#Compare plots
BedVsPrice_Plot <- ggarrange(plot_BedsPrice01, plot_BedsFrequency01, 
          ncol = 2, nrow = 1)


###
###Do some cleaning
###
KingCountyClean <- subset(KingCounty, id!="2402100895") #here we will remove one row, the house that does not make sense, looks like a typo
nrow(KingCounty)
nrow(KingCountyClean) #confirming that we removed one row


########## Compare number of Bedrooms
#this boxplot gives us some more insight into what is happening with bedrooms
BedVsPrice_BoxPlot <- ggplot(KingCounty, aes(x=factor(bedrooms), y=price)) + 
  geom_boxplot(
    
    # custom boxes
    color="#7570B3",
    fill="#7570B3",
    alpha=0.2,
    
    # custom outliers
    outlier.colour="#D95F02",
    outlier.fill="#D95F02",
    outlier.size=3
    
  ) +
  scale_y_continuous(labels = dollar_format()) +
  ggtitle("Bedrooms and Average Price") + 
  xlab("Number of Bedrooms") +
  ylab("Average Price (USD)") 


BedSummary2 <- KingCountyClean %>%
  group_by(bedrooms) %>%
  summarize (num = n(), 
             priceav = mean(price)) %>%
  arrange(desc(priceav))

#Average price for number of bedrooms
plot_BedsPrice02 <- BedSummary2 %>%
  ggplot(aes(x= reorder(bedrooms, bedrooms))) + 
  geom_bar(aes(weight = priceav), fill="#7570B3") +
  ggtitle("Bedrooms and Average Price") + 
  xlab("Number of Bedrooms") +
  ylab("Average Price (USD)") +
  scale_y_continuous(labels = dollar_format()) +
  coord_flip()

#Frequency for number of bedrooms
plot_BedsFrequency02 <- BedSummary2 %>%
  ggplot(aes(x= reorder(bedrooms, bedrooms))) + 
  geom_bar(aes(weight = num), fill="#7570B3") +
  ggtitle("Frequncy of Number of Bedrooms") + 
  xlab("Number of Bedrooms") +
  ylab("Frequency") +
  scale_y_continuous(labels = comma) +
  coord_flip()

#Compare plots - this combines the two plots
BedVsPrice_Plot2 <- ggarrange(plot_BedsPrice02, plot_BedsFrequency02, 
          ncol = 2, nrow = 1)



########## Compare number of Bathrooms

#here we can do the same analysis for bathrooms
BathVsPrice_BoxPlot <- ggplot(KingCounty, aes(x=factor(bathrooms), y=price)) + 
  geom_boxplot(
    
    # custom boxes
    color="#7570B3",
    fill="#7570B3",
    alpha=0.2,
    
    # custom outliers
    outlier.colour="#D95F02",
    outlier.fill="#D95F02",
    outlier.size=3
    
  ) +
  scale_y_continuous(labels = dollar_format()) +
  ggtitle("Bathrooms and Average Price") + 
  xlab("Number of Bathrooms") +
  ylab("Average Price (USD)") 

BathSummary <- KingCountyClean %>%
  group_by(bathrooms) %>%
  summarize (num = n(), 
             priceav = mean(price)) %>%
  arrange(desc(priceav))

#reviewing the charts it looks like there are some outliers in the data, we can dig deeper into seeing what is wrong here
BathOutliers <- KingCounty %>%
  group_by(bathrooms) %>%
  mutate (num = n(), 
          priceav = mean(price),
          delta = abs(price-priceav)
  ) %>%
  select(id, price, bathrooms, priceav, delta) %>%
  arrange(desc(delta))
# look deeper at id  1 9808700762 which is $5.7million higher than the average


#Average price for number of bathrooms
plot_BathPrice <- BathSummary %>%
  ggplot(aes(x= reorder(bathrooms, bathrooms))) + 
  geom_bar(aes(weight = priceav), fill="#7570B3") +
  ggtitle("Bathrooms and Average Price") + 
  xlab("Number of Bathrooms") +
  ylab("Average Price (USD)") +
  scale_y_continuous(labels = dollar_format()) +
  coord_flip()


#Frequency for number of bedrooms
plot_BathFrequency <- BathSummary %>%
  ggplot(aes(x= reorder(bathrooms, bathrooms))) + 
  geom_bar(aes(weight = num), fill="#7570B3") +
  ggtitle("Frequncy of Number of Bathrooms") + 
  xlab("Number of Bathrooms") +
  ylab("Frequency") +
  scale_y_continuous(labels = comma) +
  coord_flip()

#Compare plots
BathVsPrice_Plot <- ggarrange(plot_BathPrice, plot_BathFrequency, 
          ncol = 2, nrow = 1)


########## Compare Zipcode
#continuing with the same format to review zip code
ggplot(KingCounty, aes(x=factor(zipcode), y=price)) + 
  geom_boxplot(
    
    # custom boxes
    color="#7570B3",
    fill="#7570B3",
    alpha=0.2,
    
    # custom outliers
    outlier.colour="#D95F02",
    outlier.fill="#D95F02",
    outlier.size=3
    
  ) +
  scale_y_continuous(labels = dollar_format()) +
  ggtitle("Zipcode and Average Price") + 
  xlab("Zipcode") +
  ylab("Average Price (USD)") 
#this graph is really not very useful

#create a smaller dataframe with only the info we need for our plots
ZipSummary <- KingCountyClean %>%
  group_by(zipcode) %>%
  summarize (num = n(), 
             priceav = mean(price)) %>%
  arrange(desc(priceav))

#Average price for number of bedrooms
plot_ZipPrice <- ZipSummary %>%
  ggplot(aes(x= reorder(zipcode, priceav))) + 
  geom_bar(aes(weight = priceav), fill="#7570B3") +
  ggtitle("Zipcode and Average Price") + 
  xlab("Zipcoode") +
  ylab("Average Price (USD)") +
  scale_y_continuous(labels = dollar_format()) +
  coord_flip()


#Frequency for number of bedrooms
plot_ZipFrequency <- ZipSummary %>%
  ggplot(aes(x= reorder(zipcode, priceav))) + 
  geom_bar(aes(weight = num), fill="#7570B3") +
  ggtitle("Zipcode and Average Price") + 
  xlab("Zipcoode") +
  ylab("Frequency") +
  coord_flip()

#Compare plots
ZipVsPrice_Plot <- ggarrange(plot_ZipPrice, plot_ZipFrequency, 
          ncol = 2, nrow = 1)


#use zipcoder to plot the average price per zipcode
library(zipcodeR)

ZipSummary$lat <- reverse_zipcode(ZipSummary$zipcode)$lat
ZipSummary$lng <- reverse_zipcode(ZipSummary$zipcode)$lng

#convert this to a geographic variable so it is easier to plot later
ZipSummary_sf <- st_as_sf(ZipSummary, coords = c("lng", "lat"), 
                               crs = 4326, agr = "constant")

#create a map and plot the content
zipcode_map <- ggplot(data = world) +
  geom_sf() +
  geom_sf(data = washington_cities, color = "#80b3ff", size = 6) +
  geom_sf(data = ZipSummary_sf, aes(color = priceav)) +
  geom_text_repel(data = washington_cities, aes(x = lng, y = lat, label = city), 
                  fontface = "bold", size=6, 
                  nudge_x = c(0.6, -.15, 0.5), 
                  nudge_y = c(0.15, -0.1, -0.2),
  ) +
  coord_sf(xlim = c(-122.5, -121.3 ), ylim = c(47.16, 47.78), expand = FALSE) + 
  scale_color_gradientn(colors = brewer.pal(11, "RdYlGn")) +
  ggtitle("Zipcode and Average Price") + 
  xlab("Longitude") +
  ylab("Latitude") 

########## Compare Waterfront
#continue the same methods for waterfront

WaterfrontSummary  <- KingCountyClean %>%
  group_by(waterfront) %>%
  summarize (num = n(), 
             priceav = mean(price)) %>%
  arrange(desc(priceav))


#Average price depending on whether it is waterfront
plot_WaterfrontPrice <- WaterfrontSummary %>%
  ggplot(aes(x= reorder(waterfront, priceav))) + 
  geom_bar(aes(weight = priceav), fill="#7570B3") +
  ggtitle("Waterfront and Average Price") + 
  xlab("Waterfront") +
  ylab("Average Price (USD)") +
  scale_y_continuous(labels = dollar_format()) +
  coord_flip()


#Frequency for waterfront
plot_WaterfrontFrequency <- WaterfrontSummary %>%
  ggplot(aes(x= reorder(waterfront, priceav))) + 
  geom_bar(aes(weight = num), fill="#7570B3") +
  ggtitle("Frequency of Waterfront") + 
  xlab("Waterfront") +
  ylab("Number of Properties") +
  scale_y_continuous(labels = comma) +
  coord_flip()

#Compare plots
WaterfrontVsPrice_Plot <- ggarrange(plot_WaterfrontPrice, plot_WaterfrontFrequency, 
          ncol = 2, nrow = 1)


########## Compare Square footage
#continue the same process for size of the home
#visualize the data on a scatterplot that also shows the home type
plot_Size <- ggplot(KingCountyClean, aes(x = sqft_living, y = price)) +
  geom_point(color="#1B9E77", alpha = 0.4) +
  ggtitle("Size vs Price") + 
  xlab("House Size (square footage)") +
  ylab("Price (USD)") +
  scale_y_continuous(labels = dollar_format()) +
  scale_x_continuous(labels = comma, breaks = seq(0, 15000, 2500)) +
  labs(colour="Home Type") 
plot_Size


# change to categorical variable (sqft), chose bins of 250sqft but this could be optimized later
# adding this to see if it will help explain size
KingCountyClean$sqft2<-cut(KingCountyClean$sqft_living, seq(0,15000,250), right=FALSE, labels=c(1:60))

#just like in earlier analysis, create a summary dataframe for plotting later
SqftSummary <- KingCountyClean %>%
  group_by(sqft2) %>%
  summarize (num = n(), 
             priceav = mean(price)) %>%
  arrange(desc(priceav))

#Average price based on size
plot_SqftPrice <- SqftSummary %>%
  ggplot(aes(x= reorder(sqft2, sqft2))) + 
  geom_bar(aes(weight = priceav), fill="#7570B3") +
  ggtitle("SQFT (Category) and Price") + 
  xlab("Category of Sqft") +
  ylab("Average Price (USD)") +
  scale_y_continuous(labels = dollar_format()) +
  coord_flip()

#Frequency based on size
plot_SqftFrequency <- SqftSummary %>%
  ggplot(aes(x= reorder(sqft2, sqft2))) + 
  geom_bar(aes(weight = num), fill="#7570B3") +
  ggtitle("Frequency of SQFT (Category)") + 
  xlab("Category of Sqft") +
  ylab("Number of Properties") +
  coord_flip()

#Compare plots
SizeVsPrice_Plot <- ggarrange(plot_SqftPrice, plot_SqftFrequency, 
          ncol = 2, nrow = 1)

###MAPPING
#https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-2.html

#start creating the map of the entire world
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

#add states
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

#add counties
counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))
counties <- subset(counties, grepl("washington", counties$ID))
counties$area <- as.numeric(st_area(counties))


#manually enter data for the locations of cities
washington_cities <- data.frame(state = rep("Washington", 3), 
                         city = c("Seattle", 
                                  "Kent", 
                                  "Bellevue"), 
                         lat = c(47.6062, 
                                 47.3809, 
                                 47.6101), 
                         lng = c(-122.3320, 
                                 -122.3321, 
                                 -122.2015))

#convert to sf object
washington_cities <- st_as_sf(washington_cities, coords = c("lng", "lat"), remove = FALSE, 
                        crs = 4326, agr = "constant")

#chaning to an sf object for better projection
KingCountyClean_sf <- st_as_sf(KingCountyClean, coords = c("long", "lat"), 
                           crs = 4326, agr = "constant")

price_map <- ggplot(data = world) +
  geom_sf() +
  geom_sf(data = washington_cities, color = "#80b3ff", size = 6) +
  geom_sf(data = KingCountyClean_sf, aes(color = price), size=0.25) +
  geom_text_repel(data = washington_cities, aes(x = lng, y = lat, label = city), 
                  fontface = "bold", size=6, 
                  nudge_x = c(0.6, -.15, 0.5), 
                  nudge_y = c(0.15, -0.1, -0.2),
  ) +
  coord_sf(xlim = c(-122.5, -121.3 ), ylim = c(47.16, 47.78), expand = FALSE) +
scale_color_gradientn(colors = brewer.pal(11, "RdYlGn")) +
  ggtitle("Location and Average Price") + 
  xlab("Longitude") +
  ylab("Latitude") 



#where are the 50 most expensive properties?
#create a small dataframe with the 50 most expensive
MostExpensive50 <- KingCountyClean_sf %>%
  arrange(desc(price)) %>%
  slice(1:50)

#plot this dataframe on a map
MostExpensive_map <- ggplot(data = world) +
  geom_sf() +
  geom_sf(data = washington_cities, color = "#80b3ff", size = 6) +
  geom_sf(data = MostExpensive50, color="#1A9850", size=2, alpha = 0.6) +
  geom_text_repel(data = washington_cities, aes(x = lng, y = lat, label = city), 
                  fontface = "bold", size=6, 
                  nudge_x = c(0.6, -.15, 0.5), 
                  nudge_y = c(0.15, -0.1, -0.2),
  ) +
  coord_sf(xlim = c(-122.5, -121.3 ), ylim = c(47.16, 47.78), expand = FALSE) +
  ggtitle("50 Most Expensive Properties") + 
  xlab("Longitude") +
  ylab("Latitude") 



##############################
### PREDICTION MODELS
##############################

# Validation set will be 10% of real estate data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use ‘set.seed(1)‘
test_index <- createDataPartition(y = KingCountyClean$price, times = 1, p = 0.1, list = FALSE)
trainNF <- KingCountyClean[-test_index,]
testNF <- KingCountyClean[test_index,]
view(testNF)

# Make sure city and zipcode in test set are also in train set
validation <- testNF %>%
  semi_join(trainNF, by = "zipcode") %>%
  semi_join(trainNF, by = "bathrooms")
# Add rows removed from validation set back into train set
removed <- anti_join(testNF, validation)
trainNF <- rbind(trainNF, removed)


#RMSE function
#we are going to evaluate the accuracy of our models using RMSE, here we will create the function that calculates this
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


#Naive rmse is our frist model, which is just the average
#find the average
mu_hat <- mean(trainNF$price)

#use the RMSE function to evaluate our model
naive_rmse <- RMSE(testNF$price, mu_hat)

#put the results into a table
rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)
rmse_results %>% knitr::kable()


#modelling bedroom effects
#perform the same steps as the naive model
mu <- mean(trainNF$price)
beds_avgs <- trainNF %>%
  group_by(bedrooms) %>%
  summarize(b_beds = mean(price - mu))

predicted_ratings_beds <- mu + testNF %>%
  left_join(beds_avgs, by="bedrooms") %>%
  pull(b_beds)
BedsEffect <- RMSE(predicted_ratings_beds, testNF$price)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Bed Effect",
                                 RMSE = BedsEffect ))
rmse_results %>% knitr::kable()


#modelling bath effects
#perform the same steps as the naive model
mu <- mean(trainNF$price)
bath_avgs <- trainNF %>%
  group_by(bathrooms) %>%
  summarize(b_bath = mean(price - mu))

predicted_ratings_bath <- mu + testNF %>%
  left_join(bath_avgs, by="bathrooms") %>%
  pull(b_bath)
BathEffect <- RMSE(predicted_ratings_bath, testNF$price)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Bath Effect",
                                 RMSE = BathEffect ))
rmse_results %>% knitr::kable()

#modelling waterfront effects
#perform the same steps as the naive model
mu <- mean(trainNF$price)
bath_avgs <- trainNF %>%
  group_by(waterfront) %>%
  summarize(b_waterfront = mean(price - mu))

predicted_ratings_bath <- mu + testNF %>%
  left_join(bath_avgs, by="waterfront") %>%
  pull(b_waterfront)
WaterfrontEffect <- RMSE(predicted_ratings_bath, testNF$price)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="wWaterfront Effect",
                                 RMSE = WaterfrontEffect ))
rmse_results %>% knitr::kable()

#modelling grade effects
#perform the same steps as the naive model
mu <- mean(trainNF$price)
grade_avgs <- trainNF %>%
  group_by(grade) %>%
  summarize(b_grade = mean(price - mu))

predicted_ratings_grade <- mu + testNF %>%
  left_join(grade_avgs, by="grade") %>%
  pull(b_grade)
GradeEffect <- RMSE(predicted_ratings_grade, testNF$price)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Grade Effect",
                                 RMSE = GradeEffect ))
rmse_results %>% knitr::kable()

#modelling view effects
#perform the same steps as the naive model
mu <- mean(trainNF$price)
view_avgs <- trainNF %>%
  group_by(view) %>%
  summarize(b_view = mean(price - mu))

predicted_ratings_view <- mu + testNF %>%
  left_join(view_avgs, by="view") %>%
  pull(b_view)
ViewEffect <- RMSE(predicted_ratings_view, testNF$price)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="View Effect",
                                 RMSE = ViewEffect ))
rmse_results %>% knitr::kable()


#modelling age effects
#perform the same steps as the naive model
mu <- mean(trainNF$price)
yr_built_avgs <- trainNF %>%
  group_by(yr_built) %>%
  summarize(b_yr = mean(price - mu))

predicted_yrbuilt <- mu + testNF %>%
  left_join(yr_built_avgs, by="yr_built") %>%
  pull(b_yr)
YearEffect <- RMSE(predicted_yrbuilt_view, testNF$price)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Year Effect",
                                 RMSE = YearEffect ))
rmse_results %>% knitr::kable()


#modelling zipcode effects
#perform the same steps as the naive model
mu <- mean(trainNF$price)
zip_avgs <- trainNF %>%
  group_by(zipcode) %>%
  summarize(b_zip = mean(price - mu))

predicted_ratings_zip <- mu + testNF %>%
  left_join(zip_avgs, by="zipcode") %>%
  pull(b_zip)
ZipEffect <- RMSE(predicted_ratings_zip, testNF$price)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Zip Effect",
                                 RMSE = ZipEffect ))
rmse_results %>% knitr::kable()


#modelling categorical sqft effects
#perform the same steps as the naive model
mu <- mean(trainNF$price)
sqftcat_avgs <- trainNF %>%
  group_by(sqft2) %>%
  summarize(b_sqftcat = mean(price - mu))

predicted_ratings_sqftcat <- mu + testNF %>%
  left_join(sqftcat_avgs, by="sqft2") %>%
  pull(b_sqftcat)
sqftcatEffect <- RMSE(predicted_ratings_sqftcat, testNF$price)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="SQFT Cat Effect",
                                 RMSE = sqftcatEffect ))
rmse_results %>% knitr::kable()

#combine zip and sqft
#this is the first model where we combine two other models; zip and sqft
predicted_ratings_zipsqftcat <- testNF %>%
  left_join(zip_avgs, by="zipcode") %>%
  left_join(sqftcat_avgs, by="sqft2") %>%
  mutate(pred = mu + b_zip + b_sqftcat) %>%
  pull(pred)
ZipSQFTcatEffect <- RMSE(predicted_ratings_zipsqftcat, testNF$price)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Zip Effect + SQFT Cat Effect",
                                 RMSE = ZipSQFTcatEffect ))
rmse_results %>% knitr::kable()

#combine zip and sqft and year
#just like the previous model, combine two
yr_built_avgs <- trainNF %>%
  group_by(yr_built) %>%
  summarize(b_yr = mean(price - mu))

predicted_yrbuilt <- mu + testNF %>%
  left_join(yr_built_avgs, by="yr_built") %>%
  pull(b_yr)

predicted_ratings_zipsqftcatyear <- testNF %>%
  left_join(zip_avgs, by="zipcode") %>%
  left_join(sqftcat_avgs, by="sqft2") %>%
  left_join(yr_built_avgs, by="yr_built") %>%
  mutate(pred = mu + b_zip + b_sqftcat + b_yr) %>%
  pull(pred)
ZipSQFTcatYearEffect <- RMSE(predicted_ratings_zipsqftcatyear, testNF$price)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Zip Effect + SQFT Cat Effect + Year Effect",
                                 RMSE = ZipSQFTcatYearEffect ))
rmse_results %>% knitr::kable()



#####
### Regularization
#####

#regularization is important because we have some infrequent observations that might be making our model less accurate
#we will start with labmda = 3 to test things out
lambda <- 3
mu <- mean(trainNF$price)
sqft_reg_avgs <- trainNF %>%
  group_by(sqft2) %>%
  summarize(b_sqft = sum(price - mu)/(n()+lambda), n_i = n()) #using lambda as a penalty term

#we can graph this to get a better idea of what is happening visually when we change lambda
tibble(original = sqftcat_avgs$b_sqftcat,
       regularlized = sqft_reg_avgs$b_sqft,
       n = sqft_reg_avgs$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) +
  geom_point(shape=1, alpha=0.5)

#accuracy of the old list - see where we went the most wrong and the most right
#worst predictions
LeastAccurateOld <- trainNF %>%
  left_join(sqftcat_avgs, by = "sqft2") %>%
  arrange(desc(abs(b_sqftcat))) %>%
  slice(1:10)
view(LeastAccurateOld)

#best predictions
MostAccurateOld <- trainNF %>%
  left_join(sqftcat_avgs, by = "sqft2") %>%
  arrange(abs(b_sqftcat)) %>%
  slice(1:10)
view(MostAccurateOld)


#accuracy of the regularized list
#worst predictions
LeastAccurate <- trainNF %>%
  left_join(sqft_reg_avgs, by = "sqft2") %>%
  arrange(desc(abs(b_sqft))) %>%
  slice(1:10)
view(LeastAccurate)

#best predictions
MostAccurate <- trainNF %>%
  left_join(sqft_reg_avgs, by = "sqft2") %>%
  arrange(abs(b_sqft)) %>%
  slice(1:10)
view(MostAccurate)

#RMSE of regularized sqft2 at lambda = 3
predicted_price_regsqft3 <- testNF %>%
  left_join(sqft_reg_avgs, by = "sqft2") %>%
  mutate(pred = mu + b_sqft) %>%
  pull(pred)
RegularizedSQFT_3 <- RMSE(predicted_price_regsqft3, testNF$price)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Regularized SQFT (Lambda 3)",
                                 RMSE = RegularizedSQFT_3 ))
rmse_results %>% knitr::kable()



### regularized sqft
lambdas <- seq(-1, 10, 0.25)
mu <- mean(trainNF$price)
just_the_sum <- trainNF %>%
  group_by(sqft2) %>%
  summarize(s = sum(price - mu), n_i = n())
rmses <- sapply(lambdas, function(l){
  predicted_price <- testNF %>%
    left_join(just_the_sum, by="sqft2") %>%
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  return(RMSE(predicted_price, validation$price))
})

lambdas_sqft_df <- data.frame(rmses, lambdas)

lambdas_qplot_sqft2 <- lambdas_sqft_df %>%
  ggplot(aes(lambdas, rmses)) + 
  geom_point() +
  #geom_point(aes(weight = priceav), fill="#7570B3") +
  ggtitle("Optimal Lambda") + 
  xlab("lambda") +
  ylab("RMSE")

lambda_sqft2 <- lambdas[which.min(rmses)] #lambda_sqft2 is -0.25

mu <- mean(trainNF$price)
sqft2_avgs_reg <- trainNF %>%
  group_by(sqft2) %>%
  summarize(b_sqft2 = sum(price - mu)/(n()+lambda_sqft2), n_i = n())

predicted_price_regsqft2 <- testNF %>%
  left_join(sqft2_avgs_reg, by = "sqft2") %>%
  mutate(pred = mu + b_sqft2) %>%
  pull(pred)
RegularizedSqft2 <- RMSE(predicted_price_regsqft2, testNF$price)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Regularized SQFT2 Effect",
                                 RMSE = RegularizedSqft2 ))
rmse_results %>% knitr::kable()


### regularized zip
#same as previously but with zip code
lambdas <- seq(-1, 10, 0.25)
mu <- mean(trainNF$price)
just_the_sum <- trainNF %>%
  group_by(zipcode) %>%
  summarize(s = sum(price - mu), n_i = n())
rmses_zip <- sapply(lambdas, function(l){
  predicted_price <- testNF %>%
    left_join(just_the_sum, by="zipcode") %>%
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  return(RMSE(predicted_price, validation$price))
})
lambdas_szip_df <- data.frame(rmses_zip, lambdas)

qplot(lambdas, rmses_zip)
lambda_zip <- lambdas[which.min(rmses_zip)] #lambda_zip is 7

lambdas_qplot_zip <- lambdas_szip_df %>%
  ggplot(aes(lambdas, rmses_zip)) + 
  geom_point() +
  ggtitle("Optimal Lambda") + 
  xlab("lambda") +
  ylab("RMSE")


mu <- mean(trainNF$price)
zip_avgs_reg <- trainNF %>%
  group_by(zipcode) %>%
  summarize(b_zip = sum(price - mu)/(n()+lambda_zip), n_i = n())

predicted_price_regzip <- testNF %>%
  left_join(zip_avgs_reg, by = "zipcode") %>%
  mutate(pred = mu + b_zip) %>%
  pull(pred)
RegularizedZip <- RMSE(predicted_price_regzip, testNF$price)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Regularized Zip Effect",
                                 RMSE = RegularizedZip ))
rmse_results %>% knitr::kable()


### regularized zip + regularized sqft
mu <- mean(trainNF$price)

sqft2_avgs_reg2 <- trainNF %>%
  group_by(sqft2) %>%
  summarize(b_sqft2 = sum(price - mu)/(n()+lambda_sqft2))

zip_avgs_reg2 <- trainNF %>%
  left_join(sqft2_avgs_reg2, by="sqft2") %>%
  group_by(zipcode) %>%
  summarize(b_zip = sum(price - mu)/(n()+lambda_zip), n_i = n())

predicted_price_regsqftzip <- testNF %>%
  left_join(sqft2_avgs_reg2, by = "sqft2") %>%
  left_join(zip_avgs_reg2, by = "zipcode") %>%
  mutate(pred = mu + b_sqft2 + b_zip) %>%
  pull(pred)
RegularizedSQFTZip <- RMSE(predicted_price_regsqftzip, testNF$price)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Regularized SQFT + Zip",
                                 RMSE = RegularizedSQFTZip ))
rmse_results %>% knitr::kable()


#rather than average RMSE, try to show this error as a percentage of price.  If we have doe this well, the lower priced homes should have a lower error and the higher priced homes should have a higher error.  Practically this would be more appropriate for an actual situation where someone is purchasing a home.

RegSqftZip_Percent <- testNF %>%
  left_join(sqft2_avgs_reg2, by = "sqft2") %>%
  left_join(zip_avgs_reg2, by = "zipcode") %>%
  mutate(pred = mu + b_sqft2 + b_zip,
         AbsError = abs(price-pred),
         PercentError = AbsError/price*100
  )
  
mean(RegSqftZip_Percent$PercentError)


### regularized grade
lambdas <- seq(-5, 10, 0.25)
mu <- mean(trainNF$price)
just_the_sum <- trainNF %>%
  group_by(grade) %>%
  summarize(s = sum(price - mu), n_i = n())
rmses <- sapply(lambdas, function(l){
  predicted_price <- testNF %>%
    left_join(just_the_sum, by="grade") %>%
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  return(RMSE(predicted_price, validation$price))
})
qplot(lambdas, rmses)
lambda_grade <- lambdas[which.min(rmses)] #lambda_grade is 5

mu <- mean(trainNF$price)
grade_avgs_reg <- trainNF %>%
  group_by(grade) %>%
  summarize(b_grade = sum(price - mu)/(n()+lambda_grade), n_i = n())

predicted_price_reggrade <- testNF %>%
  left_join(grade_avgs_reg, by = "grade") %>%
  mutate(pred = mu + b_grade) %>%
  pull(pred)
RegularizedGrade <- RMSE(predicted_price_reggrade, testNF$price)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Regularized Grade Effect",
                                 RMSE = RegularizedGrade ))
rmse_results %>% knitr::kable()


### regularized zip + regularized sqft + regularized grade
predicted_price_new <- testNF %>%
  left_join(sqft2_avgs_reg, by = "sqft2") %>%
  left_join(zip_avgs_reg, by = "zipcode") %>%
  left_join(grade_avgs_reg, by = "grade") %>%
  mutate(pred = mu + b_sqft2 + b_zip + b_grade) %>%
  pull(pred)
Regularizednew <- RMSE(predicted_price_new, testNF$price)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Reg SQFT + Reg Zip + Reg Grade",
                                 RMSE = Regularizednew ))
rmse_results %>% knitr::kable()


###
#nearest neighbours
# scenario of 2000 sqft house
###

#how much should a 2000 sqft house cost?
#visualize the data on a scatterplot of the training data 
neighbours_plot <- ggplot(trainNF, aes(x = sqft_living, y = price)) +
  geom_point(color="#1B9E77", alpha = 0.4) +
  ggtitle("Size vs Price (training set)") + 
  xlab("House Size (square footage)") +
  ylab("Price (USD)") +
  scale_y_continuous(labels = dollar_format()) +
  scale_x_continuous(labels = comma, breaks = seq(0, 15000, 2500)) +
  labs(colour="Home Type") +
  geom_vline(xintercept = 2000, linetype = "dotted")
neighbours_plot

#next we can use neighbouring pints to see what the price should be
nearest_neighbours <- trainNF %>% 
  mutate(diff = abs(2000 - sqft_living)) %>% 
  arrange(diff) %>% 
  head(5)
nearest_neighbours

#now we have the 5 closest values, we can take their average
prediction <- nearest_neighbours %>% 
  summarise(predicted = mean(price))
prediction


#including standardization even though we dont really need to, one variable
king_recipe <- recipe(price ~ sqft_living, data = trainNF) %>%
  step_scale(all_predictors()) %>%
  step_center(all_predictors())

king_spec <- nearest_neighbor(weight_func = "rectangular", neighbors = tune()) %>%
  set_engine("kknn") %>%
  set_mode("regression")

king_vfold <- vfold_cv(trainNF, v = 5, strata = price)

king_wkflw <- workflow() %>%
  add_recipe(king_recipe) %>%
  add_model(king_spec)
king_wkflw

#we are telling the model to use regression
gridvals <- tibble(neighbors = seq(1,200))

king_results <- king_wkflw %>%
  tune_grid(resamples = king_vfold, grid = gridvals) %>%
  collect_metrics() 

# show all the results
king_results

# show only the row of minimum RMSPE
king_min <- king_results %>%
  filter(.metric == 'rmse') %>%
  filter(mean == min(mean))
king_min

#evaluate the test set
set.seed(1234)
kmin <- king_min %>% pull(neighbors)
king_spec <- nearest_neighbor(weight_func = "rectangular", neighbors = kmin) %>%
  set_engine("kknn") %>%
  set_mode("regression")

king_fit <- workflow() %>%
  add_recipe(king_recipe) %>%
  add_model(king_spec) %>%
  fit(data = trainNF)

king_summary <- king_fit %>% 
  predict(testNF) %>%
  bind_cols(testNF) %>%
  metrics(truth = price, estimate = .pred) 

king_summary_df <- as.data.frame(king_summary)
king_knn <- king_summary_df$.estimate[1]

rmse_results <- bind_rows(rmse_results,
                          tibble(method="KNN",
                                 RMSE = king_knn ))
rmse_results %>% knitr::kable()



#what does the model look like when we predict against other house sizes?
set.seed(1234)
king_preds <- king_fit %>%
  predict(trainNF) %>%
  bind_cols(trainNF)

knn_plot <- ggplot(king_preds, aes(x = sqft_living, y = price)) +
  geom_point(color="#1B9E77", alpha = 0.4) +
  xlab("House size (square footage)") +
  ylab("Price (USD)") +
  scale_y_continuous(labels = dollar_format())  +
  geom_line(data = king_preds, aes(x = sqft_living, y = .pred), color = "blue") +
  ggtitle(paste0("K = ", kmin))
knn_plot

#exploratory analysis
library(GGally)
plot_pairs <- KingCountyClean %>% 
  select(price, sqft_living, bedrooms) %>% 
  ggpairs()
plot_pairs

#build a new model and recipe
king_recipe <- recipe(price ~ sqft_living + bedrooms, data = trainNF) %>%
  step_scale(all_predictors()) %>%
  step_center(all_predictors())

king_spec <- nearest_neighbor(weight_func = "rectangular", neighbors = tune()) %>%
  set_engine("kknn") %>%
  set_mode("regression")

#5 fold cross validation
gridvals <- tibble(neighbors = seq(1,200))
king_k <- workflow() %>%
  add_recipe(king_recipe) %>%
  add_model(king_spec) %>%
  tune_grid(king_vfold, grid = gridvals) %>%
  collect_metrics() %>%
  filter(.metric == 'rmse') %>%
  filter(mean == min(mean)) %>%
  pull(neighbors)
king_k
#the smallest rmspe occurs when k=16


#retrain the model
king_spec <- nearest_neighbor(weight_func = "rectangular", neighbors = king_k) %>%
  set_engine("kknn") %>%
  set_mode("regression")

knn_mult_fit <- workflow() %>%
  add_recipe(king_recipe) %>%
  add_model(king_spec) %>%
  fit(data = trainNF)

knn_mult_preds <- knn_mult_fit %>%
  predict(testNF) %>%
  bind_cols(testNF)

knn_mult_mets <- metrics(knn_mult_preds, truth = price, estimate = .pred) 
knn_mult_mets

knn_mult_mets_df <- as.data.frame(knn_mult_mets)
king_knn_mult <- knn_mult_mets_df$.estimate[1]

rmse_results <- bind_rows(rmse_results,
                          tibble(method="KNN Multiple",
                                 RMSE = king_knn_mult ))
rmse_results %>% knitr::kable()


#build a new model and recipe
sacr_recipe <- recipe(price ~ sqft_living + bedrooms + grade, data = trainNF) %>%
  step_scale(all_predictors()) %>%
  step_center(all_predictors())

sacr_spec <- nearest_neighbor(weight_func = "rectangular", neighbors = tune()) %>%
  set_engine("kknn") %>%
  set_mode("regression")

#5 fold cross validation
gridvals <- tibble(neighbors = seq(1,200))
sacr_k <- workflow() %>%
  add_recipe(sacr_recipe) %>%
  add_model(sacr_spec) %>%
  tune_grid(sacr_vfold, grid = gridvals) %>%
  collect_metrics() %>%
  filter(.metric == 'rmse') %>%
  filter(mean == min(mean)) %>%
  pull(neighbors)
sacr_k
#the smallest rmspe occurs when k=16

#retrain the model
sacr_spec <- nearest_neighbor(weight_func = "rectangular", neighbors = sacr_k) %>%
  set_engine("kknn") %>%
  set_mode("regression")

knn_mult_fit <- workflow() %>%
  add_recipe(sacr_recipe) %>%
  add_model(sacr_spec) %>%
  fit(data = trainNF)

knn_mult_preds <- knn_mult_fit %>%
  predict(testNF) %>%
  bind_cols(testNF)

knn_mult_mets <- metrics(knn_mult_preds, truth = price, estimate = .pred) 
knn_mult_mets

knn_mult_mets_df <- as.data.frame(knn_mult_mets)
sacr_knn_mult <- knn_mult_mets_df$.estimate[1]

rmse_results <- bind_rows(rmse_results,
                          tibble(method="KNN Multiple (living bedroooms grade",
                                 RMSE = sacr_knn_mult ))
rmse_results %>% knitr::kable()



#########################
##### LINEAR REGRESSION
#########################

data(KingCountyClean)
set.seed(1234)
small_sacramento <- sample_n(KingCountyClean, size = 30)

small_plot <- ggplot(small_sacramento, aes(x = sqft_living, y = price)) +
  geom_point() +
  xlab("House size (square footage)") +
  ylab("Price (USD)") +
  scale_y_continuous(labels=dollar_format()) +
  geom_smooth(method = "lm", se = FALSE) 
small_plot

small_model <- lm(price ~ sqft_living, data = small_sacramento)
prediction <- predict(small_model, data.frame(sqft_living = 2000))

small_plot + 
  geom_vline(xintercept = 2000, linetype = "dotted") +
  geom_point(aes(x = 2000, y = prediction[[1]], color = "red", size = 2.5)) +
  theme(legend.position="none")

print(prediction[[1]])





set.seed(1234)
sacramento_split <- initial_split(KingCountyClean, prop = 0.6, strata = price)
sacramento_train <- training(sacramento_split)
sacramento_test <- testing(sacramento_split)


lm_spec <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

lm_recipe <- recipe(price ~ sqft_living, data = trainNF)

lm_fit <- workflow() %>%
  add_recipe(lm_recipe) %>%
  add_model(lm_spec) %>%
  fit(data = trainNF)
lm_fit


lm_test_results <- lm_fit %>%
  predict(testNF) %>%
  bind_cols(testNF) %>%
  metrics(truth = price, estimate = .pred)
lm_test_results  #rmse = $263,507


lm_plot_final <- ggplot(trainNF, aes(x = sqft_living, y = price)) +
  geom_point(color="#1B9E77", alpha = 0.4) +
  xlab("House size (square footage)") +
  ylab("Price (USD)") +
  scale_y_continuous(labels = dollar_format()) +
  geom_smooth(method = "lm", se = FALSE)
lm_plot_final

coeffs <- tidy(pull_workflow_fit(lm_fit))
coeffs



#####
# multivariate linear regression 1 
#####

lm_recipe <- recipe(price ~ sqft_living + bedrooms, data = sacramento_train)

lm_fit <- workflow() %>%
  add_recipe(lm_recipe) %>%
  add_model(lm_spec) %>%
  fit(data = sacramento_train)
lm_fit

lm_mult_test_results <- lm_fit %>%
  predict(sacramento_test) %>%
  bind_cols(sacramento_test) %>%
  metrics(truth = price, estimate = .pred)
lm_mult_test_results #rmse = $263,025

coeffs <- tidy(pull_workflow_fit(lm_fit))
coeffs



#####
# multivariate linear regression 2 
#####

lm_recipe <- recipe(price ~ sqft_living + bedrooms + grade, data = trainNF)

lm_fit <- workflow() %>%
  add_recipe(lm_recipe) %>%
  add_model(lm_spec) %>%
  fit(data = trainNF)
lm_fit

lm_mult_test_results <- lm_fit %>%
  predict(testNF) %>%
  bind_cols(testNF) %>%
  metrics(truth = price, estimate = .pred)
lm_mult_test_results #rmse = $250,179

coeffs <- tidy(pull_workflow_fit(lm_fit))
coeffs


###FUTURE THINGS TO LOOK AT ...

#which observations are we the most wrong about?
#this might help us figure out where to focus some more time and energy

#worst predictions from the sqft2 model
validation %>%
  left_join(sqftcat_avgs, by="sqft2") %>%
  mutate(residual = price - (mu + b_sqftcat)) %>%
  arrange(desc(abs(residual))) %>%
  slice(1:10)
#other than the homes being larger than average, there does not seem to be any obvious pattern


#looking at how the number of bathrooms impacts price
#for some reason, it is showing that 3.5 bathrooms is more valuable than 4 bathrooms
mu <- mean(trainNF$price)
bath_avgs2 <- trainNF %>%
  group_by(bathrooms) %>%
  mutate(b_bath2 = mean(price - mu))
view(bath_avgs2)


#how good of a job does the sqft2 category do at showing price changes

emptyDF <- data.frame(sqft2 = seq(1,55,1)) #creating an empty dataframe with numbers 1 to 55 to correspond to the different categories of sqft2, otherwise the graph will not give a true impression of the values
class(emptyDF$sqft2)
class(sqft_plot)


emptyDF$sqft2 <- as.factor(emptyDF$sqft2)

sqft_plot <- KingCountyClean %>% 
  group_by(sqft2) %>%
  summarise(avp = mean(price))

sqft_plot[is.na(sqft_plot)] <- 0


sqft_plot <- sqft_plot %>%
  right_join(emptyDF, by = "sqft2") %>%
  arrange(sqft2)
view(sqft_plot)


ggplot(data = sqft_plot, aes(x = sqft2, y = avp)) +
geom_col() +
xlab("House size (square footage categories)") +
ylab("Price (USD)")

#plot the accuracy
#combine zip and sqft
predicted_ratings_zipsqftcat <- testNF %>%
  left_join(zip_avgs, by="zipcode") %>%
  left_join(sqftcat_avgs, by="sqft2") %>%
  mutate(pred = mu + b_zip + b_sqftcat) %>%
  pull(pred)
ZipSQFTcatEffect <- RMSE(predicted_ratings_zipsqftcat, testNF$price)

predicted_ratings_zipsqftcat1 <- testNF %>%
  left_join(zip_avgs, by="zipcode") %>%
  left_join(sqftcat_avgs, by="sqft2") %>%
  mutate(pred = mu + b_zip + b_sqftcat,
         error = abs(price-pred))
 
ggplot(predicted_ratings_zipsqftcat1, aes(x = sqft_living, y = error)) +
  geom_point(color="#1B9E77", alpha = 0.4) 

ggplot(predicted_ratings_zipsqftcat1, aes(x = price, y = error)) +
  geom_point(color="#1B9E77", alpha = 0.4)

sqft2_error <- predicted_ratings_zipsqftcat1 %>%
group_by(sqft2) %>%
  summarise(mer = mean(error),
            num = n()
  ) %>%
  arrange(desc(mer))
view(sqft2_error)
#this model does not perform as well on giant houses
quantile(KingCounty$sqft_living, c(.01, .05, .10, .50, .90, .95, .99, .995))




quantile(KingCounty$price, c(.01, .05, .10, .50, .90, .95, .99))

predicted_ratings_zipsqftcat1 %>%
  group_by(zipcode) %>%
  summarise(mer = mean(error),
            num = n()
  ) %>%
  arrange(desc(mer))
#performs fairly consistently across zip codes
#performs worst on "Mercer Island", with an average error of $494,456
MercerIsland <- predicted_ratings_zipsqftcat1 %>%
  filter(zipcode==98040) 
view(MercerIsland)
#this is likely due to the fact that Mercer Island has a lot of very large homes


predicted_ratings_zipsqftcat1 %>%
  group_by(bedrooms) %>%
  summarise(mer = mean(error),
             num = n()
  ) %>%
  arrange(desc(mer))
#not a lot of room to improve bedrooms


predicted_ratings_zipsqftcat1 %>%
  group_by(bathrooms) %>%
  summarise(mer = mean(error),
            num = n()
            ) %>%
  arrange(desc(mer))
#the model is bad at predicting price in homes with a large number of bathrooms
quantile(KingCounty$bathrooms, c(.01, .05, .10, .50, .90, .95, .999)) 


predicted_ratings_zipsqftcat1 %>%
  group_by(floors) %>%
  summarise(mer = mean(error),
            num = n()
  ) %>%
  arrange(desc(mer))
#not much to improve with floors

predicted_ratings_zipsqftcat1 %>%
  group_by(view) %>%
  summarise(mer = mean(error),
            num = n()
  ) %>%
  arrange(desc(mer))
#not much to improve with view

predicted_ratings_zipsqftcat1 %>%
  group_by(condition) %>%
  summarise(mer = mean(error),
            num = n()
  ) %>%
  arrange(desc(mer))
#not much to improve with condition


predicted_ratings_zipsqftcat1 %>%
  group_by(grade) %>%
  summarise(mer = mean(error),
            num = n()
  ) %>%
  arrange(desc(mer))
#the higher grade homes have a larger error, but that is likely due to a larger sale price.  Not much to improve here.


predicted_ratings_zipsqftcat1 %>%
  mutate(ints=cut(yr_built, breaks = 13)) %>%
  group_by(ints) %>%
  summarise(mer = mean(error),
            num = n()
  ) %>%
  arrange(desc(mer))
#not much to improve with year built



#modelling zipcode effects removing the largest 0.5% of homes (those larger than 5,583 sqft of living space)
trainNF_revised <- trainNF %>%
  filter(sqft_living < 5583,
         bathrooms < 5.5)

testNF_revised <- testNF %>%
  filter(sqft_living < 5583,
         bathrooms < 5.5)

mu_revised <- mean(trainNF_revised$price)
zip_avgs_revised <- trainNF %>%
  group_by(zipcode) %>%
  summarize(b_zip = mean(price - mu_revised))

predicted_ratings_zip_revised <- mu_revised + testNF_revised %>%
  left_join(zip_avgs, by="zipcode") %>%
  pull(b_zip)
ZipEffect_revised <- RMSE(predicted_ratings_zip_revised, testNF_revised$price)


#modelling categorical sqft effects on homes less than 5583 sqft and less than 5.5 bathrooms
sqftcat_avgs_revised <- trainNF_revised %>%
  group_by(sqft2) %>%
  summarize(b_sqftcat = mean(price - mu_revised))

predicted_ratings_sqftcat_revised <- mu_revised + testNF_revised %>%
  left_join(sqftcat_avgs, by="sqft2") %>%
  pull(b_sqftcat)
sqftcatEffect_revised <- RMSE(predicted_ratings_sqftcat_revised, testNF_revised$price)

#modelling bathroom effects on homes less than 5583 sqft and less than 5.5 bathrooms
bathroom_avgs_revised <- trainNF_revised %>%
  group_by(bathrooms) %>%
  summarize(b_bathrooms = mean(price - mu_revised))

predicted_ratings_bathrooms_revised <- mu_revised + testNF_revised %>%
  left_join(bathroom_avgs_revised, by="bathrooms") %>%
  pull(b_bathrooms)
bathroomEffect_revised <- RMSE(predicted_ratings_bathrooms_revised, testNF_revised$price)


#combine zip and sqft on smaller homes
predicted_ratings_zipsqftcatbathrooms_revised <- testNF_revised %>%
  left_join(zip_avgs_revised, by="zipcode") %>%
  left_join(sqftcat_avgs_revised, by="sqft2") %>%
  left_join(bathroom_avgs_revised, by="bathrooms") %>%
  mutate(pred = mu_revised + b_zip + b_sqftcat + b_bathrooms) %>%
  pull(pred)
ZipSQFTcatBathroomsEffect_revised <- RMSE(predicted_ratings_zipsqftcatbathrooms_revised, testNF_revised$price)

rmse_results <- bind_rows(rmse_results,
                          tibble(method="Zip Effect + SQFT Cat Effect (Excluding 0.5% of Largest Homes, Excluding homes with 5.5 bathrooms or more)",
                                 RMSE = ZipSQFTcatEffect_revised ))
rmse_results %>% knitr::kable()




bedrooms <- 3
bathrooms <- 2.5
sqft2 <- as.factor(9)
zipcode <- 98117
TestHouse <- data.frame(bedrooms, bathrooms, sqft2, zipcode)

class(TestHouse$sqft2)
  
withbath <- TestHouse %>%
  left_join(zip_avgs_revised, by="zipcode") %>%
  left_join(sqftcat_avgs_revised, by="sqft2") %>%
  left_join(bathroom_avgs_revised, by="bathrooms") %>%
  mutate(pred = mu_revised + b_zip + b_sqftcat + b_bathrooms) %>%
  pull(pred)
withbath

withoutbath <- TestHouse %>%
  left_join(zip_avgs_revised, by="zipcode") %>%
  left_join(sqftcat_avgs_revised, by="sqft2") %>%
  mutate(pred = mu_revised + b_zip + b_sqftcat) %>%
  pull(pred)
withoutbath

#check to see if we are close...
TestHouseCompare <- trainNF_revised %>%
  filter(bedrooms == 3,
         bathrooms == 2.5,
        # sqft2 >=8 & sqft2 <=10,
         zipcode == 98117
  )
