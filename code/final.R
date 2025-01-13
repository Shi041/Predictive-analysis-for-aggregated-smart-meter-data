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





summary(srl_mean_clean_MY)



#linear regression 


#electricity
srl_ele_MY <- srl_mean_clean_MY %>% 
  filter(fuel != "Gas")

set.seed(123)  # for reproducibility
split_2 <- sample.split(srl_ele_MY, SplitRatio = 0.7)  # Assuming you want a 70-30 split

# Subset the data into training and testing
train_data_2 <- subset(srl_ele_MY, split_2 == TRUE)
test_data_2 <- subset(srl_ele_MY, split_2 == FALSE)

model_1 <- glm(value ~ segmentation_variable_1 + mean_temp + mean_hdd + weekday_weekend + mean_floor_area + 
                mean_bedrooms+mean_occupants  , data =  train_data_2  )
round(coef(summary(model_1)), 7)


model_back <- step(model_1, direction = "backward")

#nodel 2
model_2 <- glm(value ~ segmentation_variable_1 + mean_hdd + mean_floor_area + 
                mean_bedrooms + mean_occupants , data =  train_data_2  )
round(coef(summary(model_2)), 7)
summary(model_2)

model_back <- step(model_2, direction = "backward")
plot(model_2, which = 1:2)


#prediction 

predicted1 <- predict(model_2, newdata = test_data_2, type = "response")
head(predicted1)

#plot(y=test_data_2$summary_time,x=test_data_2$value)

plot(predicted1,test_data_2$value,main = "Linear regression")
abline(lm((predicted1~test_data_2$value)))
actual_1<-test_data_2$value


# normalised linear regression 


srl_ele_MY$unit<-NULL
srl_ele_MY$summary_stat<-NULL
srl_ele_MY$subsample<-NULL
srl_ele_MY$time_period<-NULL
srl_ele_MY$decimal_places<-NULL
srl_ele_MY$n_sample<-NULL
srl_ele_MY$n_mean_floor_area<-NULL
srl_ele_MY$n_mean_bedrooms<-NULL
srl_ele_MY$n_mean_occupants<-NULL
srl_ele_MY$n_statistic<-NULL


# Standardize the numeric columns
srl_MY_normalized <- srl_ele_MY %>%
  mutate_if(is.numeric, scale) # scale() function standardizes the data

# If you want the results as a dataframe (since scale returns a matrix)
srl_MY_normalized <- as.data.frame(srl_MY_normalized)

srl_MY_normalized$value<-srl_ele_MY$value

# View the normalized dataframe
head(srl_MY_normalized)










set.seed(123)  # for reproducibility
split_3 <- sample.split(srl_MY_normalized, SplitRatio = 0.7)  # Assuming you want a 70-30 split

# Subset the data into training and testing
train_data_3 <- subset(srl_MY_normalized, split_3 == TRUE)
test_data_3 <- subset(srl_MY_normalized, split_3 == FALSE)

#model 3
model_3 <- glm(value ~ segmentation_variable_1+  mean_temp + mean_hdd + weekday_weekend + mean_floor_area + 
                mean_bedrooms+mean_occupants  , data =  train_data_3  )
round(coef(summary(model_3)), 7)
summary(model_3)

model_back <- step(model_3, direction = "backward")
plot(model_3, which = 1:2)

#model 4
model_4 <- glm(value ~ segmentation_variable_1 + mean_temp + mean_hdd + mean_floor_area + 
                 mean_bedrooms + mean_occupants , data =  train_data_3  )
round(coef(summary(model_4)), 7)
summary(model_4)

model_back <- step(model_4, direction = "backward")
plot(model_4, which = 1:2)


predicted_normal <- predict(model_4, newdata = test_data_3, type = "response")
head(predicted1)

plot(predicted_normal,test_data_3$value,main = "Normalised Linear regression")
abline(lm((predicted_normal~test_data_3$value)))
actual_2<-test_data_3$value

## pannel regression 


##pannel regression 
library(plm)
srl_MY_normalized$segments = paste(srl_MY_normalized$segmentation_variable_1, srl_MY_normalized$segment_1_value, sep = " ")
library(dplyr)

# Check the dataset to ensure column names and data
print(head(srl_MY_normalized))

# More detailed duplicate check using dplyr
duplicates <- srl_MY_normalized %>%
  group_by(segments, summary_time) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(count > 1)

# Display any duplicates
print(duplicates)

# If there are duplicates, examine them
if(nrow(duplicates) > 0) {
  print(srl_MY_normalized %>%
          filter(segments %in% duplicates$segments & summary_time %in% duplicates$summary_time))
}
# Remove duplicates by keeping the first occurrence
srl_MY_normalized_clean <- srl_MY_normalized %>%
  group_by(segments, summary_time) %>%
  slice(1) %>%
  ungroup()

# spliting into train and test 
set.seed(123)  # for reproducibility
library(caTools)

split_pan <- sample.split(srl_MY_normalized_clean$value, SplitRatio = 0.7)  # Assuming you want a 70-30 split

train_data_pan <- subset(srl_MY_normalized_clean, split_pan == TRUE)
test_data_pan <- subset(srl_MY_normalized_clean, split_pan == FALSE)

library(plm)


# Attempt to create pdata.frame again
pdata <- tryCatch({
  pdata.frame(train_data_pan, index = c("segments", "summary_time"))
}, warning = function(w) {
  message("Warning caught again: ", w$message)
  return(NULL)
})

# Check if pdata was created successfully
if (!is.null(pdata)) {
  print("pdata.frame created successfully.")
} else {
  print("Failed to create pdata.frame again due to warnings.")
}

# test pannel
test_pdata <- tryCatch({
  pdata.frame(test_data_pan, index = c("segments", "summary_time"))
}, warning = function(w) {
  message("Warning caught again: ", w$message)
  return(NULL)
})

# Check if pdata was created successfully
if (!is.null(test_pdata)) {
  print("pdata.frame created successfully.")
} else {
  print("Failed to create pdata.frame again due to warnings.")
}

#model pooling
model_pan<- plm(value ~  mean_temp + mean_hdd + weekday_weekend + mean_floor_area + 
                  mean_bedrooms+mean_occupants , data =  pdata, index=c("segments", "summary_time")   , model = "pooling")
round(coef(summary(model_pan)), 7)
summary(model_pan)

#predict 
predicted_pooling <- predict(model_pan, newdata = test_pdata, type = "response")
names(predicted_pooling) <- NULL
head(predicted_pooling)
test_data_pan<-na.omit(test_data_pan)
length(test_data_pan$value)
length(predicted_pooling)

#plot(y=test_data_2$summary_time,x=test_data_2$value)
predicted_pooling<-as.numeric(predicted_pooling)
plot(predicted_pooling,test_data_pan$value)


## model fixed 
model_pan_fixed<- plm(value ~  mean_temp + mean_hdd + weekday_weekend + mean_floor_area + 
                        mean_bedrooms+mean_occupants , data =  pdata , index=c("segments", "summary_time") , model = "within")
round(coef(summary(model_pan_fixed)), 7)
summary(model_pan_fixed)

pFtest(model_pan_fixed, model_pan)

#predict 
predicted_fixed <- predict(model_pan_fixed, newdata = test_pdata ,type = "link")
names(predicted_fixed) <- NULL
head(predicted_fixed)
test_data_pan<-na.omit(test_data_pan)
length(test_data_pan$value)
length(predicted_fixed)

#plot(y=test_data_2$summary_time,x=test_data_2$value)
predicted_fixed<-as.numeric(predicted_fixed)
plot(predicted_fixed,test_data_pan$value)

predicted_fixed<-as.numeric(predicted_fixed)
test_data_pan$predicted<-predicted_fixed 
plot(test_data_pan$value,test_data_pan$predicted)
abline(lm((predicted_fixed~test_data_pan$value)))

test_data_pan$predicted<-predicted_fixed 

library(plotly)

ggplot(test_data_pan, aes(x = summary_time))+
  geom_boxplot(aes( y = predicted, color="predicted")) +
  geom_boxplot(aes( y = value, color="value"))+# Use geom_line if you want a line plot
  # Use geom_point if you also want the individual points
 # facet_wrap(~summary_time)+
  labs(x = "Summary Time", y = "Value", title = "Plot of Summary Time vs Value") +  
  theme_minimal()  # Adds a minimal theme to the plot


#model random 

model_pan_random<- plm(value ~  mean_temp + mean_hdd + weekday_weekend + mean_floor_area + 
                         mean_bedrooms+mean_occupants , data =  pdata  , index=c("segments", "summary_time") , model = "random")
round(coef(summary(model_pan_random)), 7)
summary(model_pan_random)
phtest(model_pan_fixed, model_pan_random)

# Ensure data types match those in the training set
sapply(test_pdata, class)  # Check data types in the test set
sapply(pdata, class)  # Check data types in the training set


#predict 
predicted_random <- predict(model_pan_random, newdata = test_pdata, type = "response")
names(predicted_random) <- NULL
head(predicted_random)
test_data_pan<-na.omit(test_data_pan)
length(test_data_pan$value)
length(predicted_random)
test_data_pan$predicted<-predicted_random 

#plot(y=test_data_2$summary_time,x=test_data_2$value)
predicted_random<-as.numeric(predicted_random)
plot(test_data_pan$value,test_data_pan$predicted)
abline(lm((predicted_random~test_data_pan$value)))
actual_3<-test_data_pan$value
library(plotly)

ggplot(test_data_pan)+
  geom_boxplot(aes(x = summary_time, y = predicted, color="predicted")) +
  geom_boxplot(aes(x = summary_time, y = value, color="value"))+# Use geom_line if you want a line plot
   # Use geom_point if you also want the individual points
 # facet_wrap(~summary_time)+
  labs(x = "Summary Time", y = "Value", title = "Plot of Summary Time vs Value") +  

  theme_minimal()  # Adds a minimal theme to the plot

library(broom)
tidy(model_pan)


pFtest(model_pan_fixed, model_pan)
pFtest(model_pan_fixed, model_pan_random)
phtest(model_pan_fixed, model_pan)
phtest(model_pan_fixed, model_pan_random)
phtest(model_pan, model_pan_random)

anova(model_1, model_2)
anova(model_3, model_4, test="Chisq")

pFtest(model_2, model_4)


mean(abs(test_data_pan$predicted-test_data_pan$value))

# Load necessary libraries
library(caret) # for RMSE and MAE
library(Metrics) # for additional metrics

evaluation_table <- data.frame(
  Model = c("Linear Regression", "normalised Linear Regression", "pannel Regression pooling","pannel Regression fixed","pannel Regression random")
  MAE = c(mean(abs(actual_1, predicted1)), 
        mean(abs(actual_2, predicted_normal)), 
        mean(abs(actual_1, predicted_pooling)), 
        mean(abs(actual_1, predicted_fixed)), 
        mean(abs(actual_1, predicted_random)))
RMSE = c(caret::RMSE(predicted1, actual_1), 
         caret::RMSE(predicted_normal, actual_1), 
         caret::RMSE(predicted_pooling, actual_1), 
         caret::RMSE(predicted_fixed, actual_1), 
         caret::RMSE(predicted_random, actual_1)),
R_Squared = c(summary(model_2)$r.squared, 
              summary(model_4)$r.squared, 
              summary(model_pan)$r.squared, 
              summary(model_pan_fixed)$r.squared, 
              summary(model_pan_random)$rsq)
)

MAE_linear<-c(summary(abs(actual_1- predicted1)))
MAE_normal<-summary(abs(actual_2- predicted_normal))
MAE_pooling<-summary(abs(actual_3- predicted_pooling))
MAE_fixed<-summary(abs(actual_3- predicted_fixed))
MAE_random<-summary(abs(actual_3- predicted_random))

MAE = c("MAE_linear",MAE_linear,
        "MAE_normal",MAE_normal,
        "MAE_pooling",MAE_pooling,
        "MAE_fixed",MAE_fixed,
        "MAE_random",MAE_random)
MAE
