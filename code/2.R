setwd("C:/Users/haochen shi/OneDrive/Desktop/Built enviroment/cod")

srl=read.csv("C:/Users/haochen shi/OneDrive/Desktop/Built enviroment/8963csv_8C43FE54F329413FB24F2AC300533BDAC905257AD97E9F7E727E13C65ADDCF50_V1/UKDA-8963-csv/csv/serl_energy_use_in_GB_domestic_buildings_2021_aggregated_statistics_v01.csv")

head(srl)

library(dplyr)


# Remove rows where summary_stat is not 'mean'
srl_mean <- srl %>% 
  filter(summary_stat == "mean")

head(srl_mean)
summary(srl_mean)
srl_mean

srl_mean_clean <- srl_mean %>% 
  filter(summary_time != "heating_season_2019_2020")
srl_mean_clean <- srl_mean_clean %>% 
  filter(summary_time != "heating_season_2020_2021")
srl_mean_clean <- srl_mean_clean %>% 
  filter(summary_time != "2021")
srl_mean_clean <- srl_mean_clean %>% 
  filter(summary_time != "2020")
srl_mean_clean <- srl_mean_clean %>% 
  filter(summary_time != "2019")

srl_mean_clean
library(dplyr)

# Assuming your data is in a data.frame called df
# Filter out rows where summary_time matches a pattern like 'HH:MM'
srl_mean_clean_MY <- srl_mean_clean %>% 
  filter(!grepl("^\\d{2}:\\d{2}$", summary_time))

srl_mean_clean_24H <- srl_mean_clean %>% 
  filter(!grepl("-", summary_time))

# View the filtered data frame
print(srl_mean_clean_MY)

print(srl_mean_clean_24H)

# Standardize the numeric columns
srl_MY_normalized <- srl_mean_clean_MY %>%
  mutate_if(is.numeric, scale) # scale() function standardizes the data

# If you want the results as a dataframe (since scale returns a matrix)
srl_MY_normalized <- as.data.frame(srl_MY_normalized)

# View the normalized dataframe
head(srl_MY_normalized)



summary(srl_mean_clean_MY)

#model MY back
model <- glm(value ~ n_sample + segmentation_variable_1 + mean_temp + mean_hdd + weekday_weekend + mean_floor_area + 
               mean_bedrooms+mean_occupants  , data =  srl_mean_clean_MY  )
round(coef(summary(model)), 7)


model_back <- step(model, direction = "backward")

# model MY normalised back

model_normalised <- glm(value ~ n_sample + segmentation_variable_1 + mean_temp + mean_hdd + weekday_weekend + mean_floor_area + 
               mean_bedrooms+mean_occupants  , data =  srl_MY_normalized  )
round(coef(summary(model_normalised)), 7)


model_normalised_back <- step(model_normalised, direction = "backward")


#matrix 
library(caTools)

set.seed(123)  # Setting a seed to make the example reproducible
split_srl <- sample.split(srl_mean_clean_MY, SplitRatio = 0.9) 
train_data <- subset(srl_mean_clean_MY, split_srl == TRUE)
test_data <- subset(srl_mean_clean_MY, split_srl == FALSE)

srl_matrix_train <- as.matrix(train_data)

srl_matrix_train<-na.omit(srl_matrix_train)
summary(is.na(srl_matrix_train))
srl_matrix_train<-as.numeric(srl_matrix_train)
srl_matrix_train <- scale(srl_matrix_train)


colnames(srl_normal_matrix)
print(srl_normal_matrix)

# Load the factoextra package
library(factoextra)

distance_test<-get_dist(srl_matrix_train)
head(distance_test)







fviz_dist(distance_test, gradient = list(low = "cyan", mid = "white", high = "red"))


srl_mean_non <- srl_mean_clean %>% 
  filter(segmentation_variable_1 == "None")

srl_mean_ocp <- srl_mean_clean %>% 
  filter(segmentation_variable_1 == "num_occupants")

srl_mean_IMD <- srl_mean_clean %>% 
  filter(segmentation_variable_1 == "IMD_quintile")

srl_mean_CER <- srl_mean_clean %>% 
  filter(segmentation_variable_1 == "currentEnergyRating_merge")

srl_mean_bed<- srl_mean_clean %>% 
  filter(segmentation_variable_1 == "num_bedrooms")

(srl_mean_clean_MY$segmentation_variable_1)



# Median
srl_median <- srl %>% 
  filter(summary_stat == "median")

head(srl_median)


# standard deviation
srl_sd <- srl %>% 
  filter(summary_stat == "standard deviation")

head(srl_sd)


# 75th percentile
srl_75 <- srl %>% 
  filter(summary_stat == "75th percentile")

head(srl_75)

# 25th percentile 
srl_25 <- srl %>% 
  filter(summary_stat == "25th percentile")

head(srl_25)



