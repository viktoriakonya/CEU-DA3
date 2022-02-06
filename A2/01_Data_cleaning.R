
################################################################################
# Data cleaning and workfile creation code for Barcelona Airbnb apartment price prediction
# DA3 - Assignment 3
# Viktoria Konya 
# What the code does:
#   - Imports Barcelona Airbnb data from github
#   - Data cleaning and feature engineering
#   - Sample selection
################################################################################


################################################################################
# Folder setup
################################################################################

# Clear environment
rm(list=ls())

# Folder locations
main <- getwd()
data <- paste0(main, '/data')
data_raw <- paste0(main, '/data/raw')
data_work <- paste0(main, '/data/work')
output <- paste0(main, '/output')


################################################################################
# Import libraries
################################################################################

# Import libraries
library(tidyverse)
library(skimr)
library(data.table)
library(modelsummary)


################################################################################
# Data cleaning
################################################################################

### 1. step: Import data - 15.707 obs, 74 vars
# listings <- read_csv("https://raw.githubusercontent.com/viktoriakonya/CEU-DA3/main/A2/data/raw/listings_20211207.csv") 
listings <- read_csv(paste0(data_raw,"/listings_20211207.csv")) 


### 2. step: Convert to data table
df <- data.table(listings)

# Look at the dataset - 74 vars; 15.707 obs.
glimpse(listings)
datasummary_skim(listings, 'numeric')
skim(listings )

################################################################################
# I. Sample selection
################################################################################

### 1. step: Filter for Entire home / Apartments only and filter out irrelevant property types
df[, .(count = .N), by = .(property_type)]
df[, .(count = .N), by = .(room_type)]

# Entire home / Apartments only - 8.407 obs.
df <- df[room_type %in% c("Entire home/apt")]

# Filter out irrelevant property types - 8.341 obs.
df[, .(count = .N), by = .(property_type)]
df <- df[!(property_type %in% c("Boat", "Barn", "Floor", "Camper/RV", "", "Room in aparthotel", "Entire vacation home", "Entire villa"))]

### 2. step: Filter for apartments with 2-6 accommodates - 7260 obs.
df[, .(count = .N), by = .(accommodates)][order(accommodates)]
df <- df[accommodates >= 2 & accommodates <= 6]

### 3. step: Filter for apartments where the number of beds <= 6 - 7241 obs.
df[, .(count = .N), by = .(beds)][order(beds)] # 19 obs.
df <- df[beds <= 6 | is.na(beds)]

### 4. step: Filter out apartments where host related information is not available - 7217 obs.
df[is.na(host_name), .(count = .N)] # 24 obs.
df <- df[!is.na(host_name)]


################################################################################
# II. Drop useless fields
################################################################################

# Check variables with only missing values
missings <- sapply(df, function(x) sum(is.na(x)))
missings[missings > 0]
# calendar_updated, bathrooms have only missings

### 1. step: Drop variables with only NA and useless fields 
drops <- c("listing_url", "scrape_id", "last_scraped", "name", "description", "neighborhood_overview",
           "picture_url", "host_url", "host_location", "host_about", "host_response_time",
           "host_thumbnail_url","host_picture_url","host_neighbourhood", "bathrooms", "calendar_updated")

# 58 vars, 15.775 obs
df <- df[, (drops) := NULL]


################################################################################
# IIÍ. Data cleaning
################################################################################

### 1. step: Price per night
# Clean outcome var and rename
df$price <- gsub("\\,","",df$price) # delete commas
df$n_price <- as.numeric(gsub("\\$","",df$price)) # delete $ from the end

# Missings
skim(df$n_price) # 0 missing

# Distribution
hist(df$n_price, breaks = 100)
df[df$n_price > 1000] # 16 extremes

# Price distr. by property type
df[, .(count = .N, min_price = min(n_price), max_price = max(n_price)), by = .(property_type)]
# Prices abobe 1000USD in: Entire rental unit, Entire loft, Entire serviced apartment, Entire residential home , Entire villa 

# Distribution is highly skewed -> Take log of prices (no 0 values)
df$n_ln_price <- log(df$n_price)


### 2. step: Rates
# Remove percentage sign 
for (perc in c("host_response_rate","host_acceptance_rate")){
  df[[perc]]<-gsub("%","",as.character(df[[perc]]))
}

# Replace NAs
df$host_response_rate <- ifelse(df$host_response_rate == 'N/A', NA, df$host_response_rate)
df$host_acceptance_rate <- ifelse(df$host_acceptance_rate == 'N/A', NA, df$host_acceptance_rate)

# Convert to numeric, devide by 
df$p_host_response_rate <- as.numeric(df$host_response_rate)/100
df$p_host_acceptance_rate <- as.numeric(df$host_acceptance_rate)/100

# Distributions
hist(df$p_host_response_rate) # skewed with long left tail
hist(df$p_host_acceptance_rate) # skewed with long left tail

# Missings
datasummary_skim(df$p_host_response_rate) # 13% missing
datasummary_skim(df$p_host_acceptance_rate) # 13% missing


### 3. step: reformat binary variables to numeric
for (binary in c("host_is_superhost","host_has_profile_pic","host_identity_verified","instant_bookable", "has_availability")){
  df[[binary]][df[[binary]]=="f"] <- 0
  df[[binary]][df[[binary]]=="t"] <- 1

}

# Rename
binaries <- c("host_is_superhost","host_has_profile_pic","host_identity_verified","instant_bookable", "has_availability")
names(df)[names(df) %in% binaries] <- paste("d" ,binaries ,sep="_")

# Missings
datasummary_skim(df[, c("d_host_is_superhost","d_host_has_profile_pic","d_host_identity_verified","d_instant_bookable", "d_has_availability")])
# Very little variation in:  d_host_has_profile_pic, d_host_identity_verified, d_has_availability -> Later drop these vars


### 4. step: Bathrooms 
df[, .(count = .N), by = .(bathrooms_text)][order(bathrooms_text)] # 3 not numeric, 1 NA

# Add number to the beginning
df$bathrooms_text_mod <- ifelse(df$bathrooms_text == "Shared half-bath", "0.5 shared half-bath", df$bathrooms_text)
df$bathrooms_text_mod <- ifelse(df$bathrooms_text == "Half-bath", "0.5 half-bath", df$bathrooms_text_mod)
df$bathrooms_text_mod <- ifelse(df$bathrooms_text == "Private half-bath", "0.5 private half-bath", df$bathrooms_text_mod)

# Create variable for number of bathroom
df$n_bathrooms <- as.numeric(sub( " .*$", "", df$bathrooms_text_mod )) 

# Check
df[, .(count = .N), by = .(bathrooms_text, bathrooms_text_mod, n_bathrooms)][order(bathrooms_text)] 
# 1 NA and 1 with 0
hist(df$n_bathrooms, breaks = 10)

# Create factor for bathroom with 3 levels: [0-0.5], [1-1.5], [2+]
df[, f_bathrooms:= cut(n_bathrooms, c(1,2,3,10), labels=c(1,2,3), right = F)  ]
df[, .(count = .N, min = min(n_bathrooms), max = max(n_bathrooms)), by = .(f_bathrooms)]


### 5. step: Amenities 

# Clean list
df$amenities <- gsub("\\{","",df$amenities)
df$amenities <- gsub("\\}","",df$amenities)
df$amenities <- gsub('\\"',"",df$amenities)
df$amenities <- gsub('\\[',"",df$amenities)
df$amenities <- gsub('\\]',"",df$amenities)
df$amenities <- tolower(df$amenities)

# Check most frequent items
out_db <- df %>% 
  mutate(amenities = tolower(amenities))

list_amenities <- map(out_db$amenities, function(x) x %>% strsplit(", ") %>% unlist() ) %>% 
  unlist()

df_amenities <- data.frame(amenities = list_amenities) 

df_viz <- df_amenities %>% 
  count(amenities, name = "frequency") %>% 
  mutate(n_data = nrow(out_db),
         ratio = frequency/nrow(out_db)) %>% 
  arrange(desc(ratio)) %>% 
  head(20)

df_viz %>% 
  ggplot(aes(x = ratio,
             y = amenities %>% reorder(ratio))) +
  geom_col(fill = "skyblue") +
  labs(x = "Percentage of Listing",
       y = "Amenities",
       title = "Most Common Amenities") +
  theme_bw()


# Add dummies for the most frequent and sensible amenities 
df[, d_wifi := ifelse(df$amenities %like% "wifi" == T, 1, 0)]
df[, d_long_term_stays_allowed := ifelse(df$amenities %like% "long term stays" == T, 1, 0)]
df[, d_essentials := ifelse(df$amenities %like% "essentials" == T, 1, 0)]
df[, d_kitchen := ifelse(df$amenities %like% "kitchen" == T, 1, 0)]
# df[, d_hangers := ifelse(df$amenities %like% "hangers" == T, 1, 0)]
# df[, d_hair_dryer := ifelse(df$amenities %like% "hair dryer" == T, 1, 0)]
# df[, d_heating := ifelse(df$amenities %like% "heating" == T, 1, 0)]
df[, d_washer := ifelse(df$amenities %like% "washer" == T, 1, 0)]
# df[, d_iron := ifelse(df$amenities %like% "iron" == T, 1, 0)]
# df[, d_hot_water := ifelse(df$amenities %like% "hot water" == T, 1, 0)]
df[, d_air_conditioning := ifelse(df$amenities %like% "air conditioning" == T, 1, 0)]
df[, d_tv := ifelse(df$amenities %like% "tv" == T, 1, 0)]
df[, d_shampoo := ifelse(df$amenities %like% "shampoo" == T, 1, 0)]
df[, d_elevator := ifelse(df$amenities %like% "elevator" == T, 1, 0)]
# df[, d_dishes_and_silverware := ifelse(df$amenities %like% "dishes and silverware" == T, 1, 0)]
df[, d_cooking_basics := ifelse(df$amenities %like% "cooking basics" == T, 1, 0)]
df[, d_refrigerator := ifelse(df$amenities %like% "refrigerator" == T | df$amenities %like% "fridge" == T, 1, 0)]
df[, d_dedicated_workspace := ifelse(df$amenities %like% "dedicated workspace" == T, 1, 0)]
df[, d_microwave := ifelse(df$amenities %like% "microwave" == T, 1, 0)]
# df[, d_coffee_maker:= ifelse(df$amenities %like% "coffee maker" == T, 1, 0)]

df[, d_breakfast := ifelse(df$amenities %like% "breakfast" == T, 1, 0)]
df[, d_children_friendly := ifelse(df$amenities %like% "children" == T | df$amenities %like% "baby" == T, 1, 0)]
df[, d_closet := ifelse(df$amenities %like% "closet" == T, 1, 0)]
df[, d_garden := ifelse(df$amenities %like% "garden" == T | df$amenities %like%  "backyard" == T, 1, 0)]
# df[, d_first_aid_kit := ifelse(df$amenities %like% "first aid kit" == T, 1, 0)]
df[, d_balcony := ifelse(df$amenities %like% "balcony" == T, 1, 0)]
df[, d_gym := ifelse(df$amenities %like% "gym" == T, 1, 0)]
df[, d_free_parking := ifelse(df$amenities %like% "free parking" == T, 1, 0)]
df[, d_smoking_allowed := ifelse(df$amenities %like% "smoking allowed" == T, 1, 0)]
# df[, d_lock_on_bedroom_door := ifelse(df$amenities %like% "lock on bedroom" == T, 1, 0)]
df[, d_private_entrance := ifelse(df$amenities %like% "private entrance" == T, 1, 0)]
df[, d_pool := ifelse(df$amenities %like% "pool" == T, 1, 0)]
df[, d_lockbox := ifelse(df$amenities %like% "lockbox" == T, 1, 0)]
df[, d_indoor_fireplace := ifelse(df$amenities %like% "indoor fireplace" == T, 1, 0)]
df[, d_pets_allowed := ifelse(df$amenities %like% "pets allowed" == T, 1, 0)]
df[, d_luggage_dropoff_allowed := ifelse(df$amenities %like% "luggage dropoff allowed" == T, 1, 0)]
df[, d_smart_lock := ifelse(df$amenities %like% "smart lock" == T, 1, 0)]

# Frequencies
amenities_vars <- grep("^d_.*", names(df), value=TRUE)
amenities_vars <- setdiff(amenities_vars, paste("d" ,binaries ,sep="_")) # remove not amenites
datasummary_skim(df[, amenities_vars, with = FALSE], fmt =  '%.3f' )
# Common features - Above 90%: d_wifi, d_long_term_stays_allowed, d_essentials, d_kitchen, d_washer, d_tv, d_closet
# Rare features - Below 10%: d_indoor_fireplace, d_pets_allowed, d_smart_lock, d_smoking_allowed, d_breakfast, d_garden


### 6. step: Distance from city center
# City center: Plaça de Catalunya (lat: 41.38705287353137, lon: 2.1700683272487105)
lat_center <- 41.38705287353137
lon_center <- 2.1700683272487105

# Add distance in km based on the Great-circle distance formula
df$distance_from_center_km <- acos(sin(lat_center*pi/180)*sin(df$latitude*pi/180) + cos(lat_center*pi/180)*cos(df$latitude*pi/180) * cos((df$longitude*pi/180)-(lon_center*pi/180))) * 6371

ggplot(df, aes(distance_from_center_km)) +
  geom_histogram() +
  theme_bw()

# Create distance categories
df$n_distance_from_center_km <- ceiling(df$distance_from_center_km)
df[, .(count = .N), by = .(n_distance_from_center_km)][order(n_distance_from_center_km)] 
datasummary_skim(df$n_distance_from_center_km)


### 8. step: Host since
today <- as.Date("2021-12-07")

df$n_host_since_days <- as.numeric(difftime(today, df$host_since,units='days'))

# Distribution
hist(df$n_host_since_days, breaks = 100)
skim(df$host_since)


### 9. step: Convert property_type to factor
df$f_property_type <- as.factor(df$property_type)
skim(df$property_type)
df[, .(count = .N), by = .(f_property_type)][order(desc(count))] 

### 10. Convert neighborhood_cleansed to factor
df$f_neighbourhood_group_cleansed <- as.factor(df$neighbourhood_group_cleansed)
skim(df$neighbourhood_group_cleansed)
df[, .(count = .N), by = .(f_neighbourhood_group_cleansed)][order(desc(count))] 

### Rename rest of the fields

# Numeric
df$n_minimum_nights <- df$minimum_nights
df$n_maximum_nights <- df$maximum_nights
df$n_bedrooms <- df$bedrooms
df$n_beds <- df$beds
df$n_accommodates <- df$accommodates
df$n_host_listings_count <- df$calculated_host_listings_count # host_total_listings_count
df$n_host_listings_count_entire_homes <- df$calculated_host_listings_count_entire_homes
df$n_reviews <- df$number_of_reviews
df$n_review_scores_rating <- df$review_scores_rating


################################################################################
# IV. Keep only cleaned variables + IDs
################################################################################

df_final <- df[, .(id, host_id, host_name, latitude, longitude)]

# Numeric vars - 14 vars
numeric_vars <- grep("^n_.*", names(df), value=TRUE)
df_numeric <- df[, .SD, .SDcols =  names(df) %in% numeric_vars]
df_final <- cbind(df_final, df_numeric)

# Percentages - 2 vars
pct_vars <- grep("^p_.*", names(df), value=TRUE)
df_pct <- df[, .SD, .SDcols =  names(df) %in% pct_vars]
df_final <- cbind(df_final, df_pct)

# Factors - 3 vars
factor_vars <- grep("^f_.*", names(df), value=TRUE)
df_factor <- df[, .SD, .SDcols =  names(df) %in% factor_vars]
df_final <- cbind(df_final, df_factor)

# Dummies - 41 vars
binary_vars <- grep("^d_.*", names(df), value=TRUE)
df_binary <- df[, .SD, .SDcols =  names(df) %in% binary_vars]
df_final <- cbind(df_final, df_binary)

# Summary - 65 vars
skim(df_final)



################################################################################
# V. Manage missings
################################################################################

# Missings
missings <- sapply(df_final, function(x) sum(is.na(x)))
missings[missings > 0]
# host_name, n_bathrooms, n_host_since_days, n_bedrooms, n_beds, n_review_scores_rating, p_host_response_rate 
# p_host_acceptance_rate, f_bathrooms, d_host_is_superhost,  d_host_has_profile_pic, d_host_identity_verified 


### 1. step: Missing host information - rest of the fields are filled, do not drop these
df_final[is.na(host_name)]


### 2. step: Missing bathrooms - 1 obs. - Add 1 assuming that apartments have at least one bathroom
df_final$flg_n_bathrooms <- ifelse(is.na(df_final$n_bathrooms), 1, 0) # 1 missing
df_final$n_bathrooms <- ifelse(is.na(df_final$n_bathrooms), median(df_final$n_bathrooms, na.rm = T), df_final$n_bathrooms) 
df_final$f_bathrooms <- ifelse(is.na(df_final$f_bathrooms), 2, df_final$f_bathrooms)

# Summary
df_final[, .(count = .N), by = .(n_bathrooms, f_bathrooms, flg_n_bathrooms)][order(flg_n_bathrooms, n_bathrooms)] 


### 3. step: Missing bedrooms - 340 obs. - Add 0 assuming that apartments do not have separate bedrooms (studio)
df_final$flg_n_bedrooms <- ifelse(is.na(df_final$n_bedrooms),1, 0) # 340 missing
df_final$n_bedrooms <- ifelse(is.na(df_final$n_bedrooms),0, df_final$n_bedrooms)

# Summary
table(df_final$flg_n_bedrooms, df_final$n_bedrooms)


### 4. step: Missing beds - 134 obs. - Add number of accommodates to missing beds, add 0 to ln_n_beds
df_final[is.na(n_beds), .(count = .N), by = .(n_accommodates)]

df_final$flg_n_beds <- ifelse(is.na(df_final$n_beds),1, 0)  # 134 missing
df_final$n_beds <-  ifelse(is.na(df_final$n_beds), 
                           df_final$n_accommodates, df_final$n_beds)

df_final$n_ln_beds <- log(df_final$n_beds)
df_final$flg_n_ln_beds <- ifelse(is.na(df_final$n_ln_beds),1, 0) # 134 missing
df_final$n_ln_beds <- ifelse(is.na(df_final$n_ln_beds),0, df_final$n_ln_beds)

# Summary
table(df_final$flg_n_beds, df_final$n_beds)
hist(df_final$n_beds)


### 5. step: Missing n_review_scores_rating - 1631 obs. - Add median 
df_final$flg_n_review_scores_rating <- ifelse(is.na(df_final$n_review_scores_rating),1, 0) # 1631 missing
df_final$n_review_scores_rating <-  ifelse(is.na(df_final$n_review_scores_rating), 
                                           median(df_final$n_review_scores_rating, na.rm = T), df_final$n_review_scores_rating)
# Summary
hist(df_final$n_review_scores_rating)


### 6. step: Missing p_host_response_rate - 975 obs. - Add median 
df_final$flg_p_host_response_rate <- ifelse(is.na(df_final$p_host_response_rate),1, 0) # 1631 missing
df_final$p_host_response_rate <-  ifelse(is.na(df_final$p_host_response_rate), 
                                           median(df_final$p_host_response_rate, na.rm = T), df_final$p_host_response_rate)
# Summary
hist(df_final$p_host_response_rate)


### 7. step: Missing p_host_acceptance_rate - 912 obs. - Add median 
df_final$flg_p_host_acceptance_rate <- ifelse(is.na(df_final$p_host_acceptance_rate),1, 0) # 1631 missing
df_final$p_host_acceptance_rate <-  ifelse(is.na(df_final$p_host_acceptance_rate), 
                                         median(df_final$p_host_acceptance_rate, na.rm = T), df_final$p_host_acceptance_rate)

### 8. step: Host since days - 24 obs. - Add median 
df_final$flg_n_host_since_days <- ifelse(is.na(df_final$n_host_since_days),1, 0) 
df_final$n_host_since_days <-  ifelse(is.na(df_final$n_host_since_days), 
                                         median(df_final$n_host_since_days, na.rm = T), df_final$n_host_since_days)
# Summary
hist(df_final$n_host_since_days)


# Summary
hist(df_final$p_host_acceptance_rate)

# Check missings again - these are missing because of the missing host info
missings <- sapply(df_final, function(x) sum(is.na(x)))
missings[missings > 0]

# datasummary( factor(d_long_term_stays_allowed) *n_price ~ Mean + SD + P25 + P75 + N, data = df )
                                           

################################################################################
# VI. Variable categorization
################################################################################

### 1. step: n_reviews: [0], [1-51], [51-100] [100+]
hist(df_final$n_reviews)

df_final$f_reviews <- cut(df_final$n_reviews, c(0,1,51,101,9999), labels=c(0,1,2,3), right = F) 
df_final[, .(count = .N, min = min(n_reviews), max = max(n_reviews), avg_price = mean(n_price)), by = .(f_reviews)]

# Check relationship with price (excl. extremes)
boxplot(n_price ~ f_reviews, data=df_final[n_price < 400])


### 2. step: n_host_listings_count: [1], [2-10], [11-50] [50+]
hist(df_final$n_host_listings_count)

df_final$f_host_listings_count <- cut(df_final$n_host_listings_count, c(1,2,11,51,999), labels=c(1,2,3,4), right = F) 
df_final[, .(count = .N, min = min(n_host_listings_count), max = max(n_host_listings_count)), by = .(f_host_listings_count)]

# Check relationship with price (excl. extremes)
boxplot(n_price ~ f_host_listings_count, data=df_final[n_price < 400])


### 3. step: n_host_listings_count_entire_homes: [1], [2-10], [11-50] [50+]
hist(df_final$n_host_listings_count_entire_homes)

df_final$f_host_listings_count_entire_homes <- cut(df_final$n_host_listings_count_entire_homes, c(1,2,11,51,999), labels=c(1,2,3,4), right = F) 
df_final[, .(count = .N, min = min(n_host_listings_count_entire_homes), max = max(n_host_listings_count_entire_homes)), by = .(f_host_listings_count_entire_homes)]

# Check relationship with price (excl. extremes)
boxplot(n_price ~ f_host_listings_count_entire_homes, data=df_final[n_price < 400])


### 4. step: n_minimum_nights: [1],[2-7],[8-14],[14+]
hist(df_final$n_minimum_nights, breaks = 100)

df_final$f_minimum_nights <- cut(df_final$n_minimum_nights, c(1,2,8,15,9999), labels=c(1,2,3,4), right = F)
df_final[, .(count = .N, min = min(n_minimum_nights), max = max(n_minimum_nights), avg_price = mean(n_price)), by = .(f_minimum_nights)]

# Check relationship with price (excl. extremes)
boxplot(n_price ~ f_minimum_nights, data=df_final[n_price < 400])


### 5. step: n_host_since_days: less than a year, 2 years, 2-5 years, 5 years +
hist(df_final$n_host_since_days, breaks = 100)

df_final$f_host_since_years <- cut(df_final$n_host_since_days, c(1,366,733,1827,99999), labels=c(1,2,3,4), right = F)
df_final[, .(count = .N, min = min(n_host_since_days), max = max(n_host_since_days), avg_price = mean(n_price)), by = .(f_host_since_years)]

# Check relationship with price (excl. extremes)
boxplot(n_price ~ f_host_since_years, data=df_final[n_price < 400])

### 6. step: Property type categories based on mean price
df_final[, .(count = .N, mean_price = mean(n_price)), by = .(f_property_type)][order(mean_price)] 

df_final <- df_final %>% mutate(
  f_property_type = as.character(f_property_type), 
  f_property_type2 = case_when(
    f_property_type %in% c("Entire rental unit", "Entire home/apt", "Entire residential home", "Entire townhouse", "Casa particular", "Entire place") ~ "Entire home / Apartment",
    f_property_type %in% c("Entire chalet", "Tiny house", "Entire guesthouse", "Entire guest suite") ~ "Entire guesthouse / chalet",
    TRUE ~ f_property_type))
  
df_final[, .(count = .N, mean_price = mean(n_price)), by = .(f_property_type2)][order(mean_price)] 
# Entire vacation home, Entire villa are very different
  
# Overwrite the original field
df_final$f_property_type <- df_final$f_property_type2
df_final[,f_property_type2:=NULL]
                                   

################################################################################
# Check again
################################################################################

# Check missings again
missings <- sapply(df_final, function(x) sum(is.na(x)))
missings[missings > 0]

# Summary

# Numeric
numeric_vars <- grep("^n_.*", names(df_final), value=TRUE)
df_numeric <- df_final[, .SD, .SDcols =  names(df_final) %in% numeric_vars]
datasummary_skim(df_numeric, fmt =  '%.3f' )

# Dummies
dummy_vars <- grep("^d_.*", names(df_final), value=TRUE)
df_dummy <- df_final[, .SD, .SDcols =  names(df_final) %in% dummy_vars]
datasummary_skim(df_dummy, fmt =  '%.3f' )

# Factors
factor_vars <- grep("^f_.*", names(df_final), value=TRUE)
df_factor <- df_final[, .SD, .SDcols =  names(df_final) %in% factor_vars]
datasummary_skim(df_factor, fmt =  '%.3f' , 'categorical')

# Pct
pct_vars <- grep("^p_.*", names(df_final), value=TRUE)
df_pct <- df_final[, .SD, .SDcols =  names(df_final) %in% pct_vars]
datasummary_skim(df_pct, fmt =  '%.3f' )


# Drop not needed fields
#   - host related variables, as db is on listing level (duplicates) ?
#   - dummies with one value -> d_host_has_profile_pic
#drops <- c("p_host_response_rate",
#          "p_host_acceptance_rate",
#          "n_host_listings_count_entire_homes",
#          "n_host_listings_count",
#          "d_free_parking",
#          "d_lock_on_bedroom_door",
#          "d_host_has_profile_pic",
#          "d_essentials",
#           )

# df_final <- df_final[, (drops) := NULL]


################################################################################
# Create workfile
################################################################################

# Create work file
write.csv(df_final, file = paste0(data_work,'/airbnb_barcelona_20211207_cleaned.csv'))





