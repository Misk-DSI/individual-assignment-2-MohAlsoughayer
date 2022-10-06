# Author: Mohammed Alsoughayer (mohammed.alsoughayer@gmail.com)
# Date: Oct 6th, 2022
# Description: Exploratory Data Analysis of McDonald's food nutrition. 

# Initialize packages 
library(tidyverse)
library(tidyr)
library(janitor)
library(ggplot2)
library(DataExplorer)
library(GGally)

# Read data and clean col names 
menu_df <- read_csv("data/menu.csv")
menu_df %>%
  clean_names() -> menu_df

# Questions: 
# Consider what each variable is measuring, their univariate & bivariate distributions and trends. 
# What are the most informative, interesting and/or important aspects of this data set? 
# What research question will it help you answer? 
# Goal: show some Calories trends and graphs, analyse the nutritional value of an average meal, 

# Preliminary EDA to get some insight: 
# create_report(menu_df, 
#               output_file = "menu_EDA_r1.html", 
#               output_dir = "data/")

# get calories average and stdev:
menu_df %>% 
  summarise(avgCal = mean(calories), stdevCal = sd(calories))
# Conc: The spread of the calories is pretty large implying a skew (mean might not be a very good representation)

#Add to Report 
# plot calorie dist
ggplot(menu_df, aes(calories)) +
  geom_density()
# Conc: The density graph shows that the calories have a positive skew

#Add to report 
# get & plot calories data based on category
menu_df %>% 
  group_by(category) %>% 
  summarise(avg = mean(calories), 
            stdev = sd(calories)
            ) -> calCat_df
ggplot(calCat_df, aes(x = category, y = avg)) +
  geom_pointrange(aes(ymin = avg - stdev, ymax = avg + stdev)) +
  geom_point()
# Conc: The beverages, coffee&tea, desserts, salads, and snacks are shifting the positve skew slightly to the left.

# Plot data excluding the categories listed above. 
menu_df %>% 
  filter(category == "Beef & Pork" | category == "Breakfast" | category == "Chicken & Fish" | category == 'Smoothies & Shakes') -> menu_bbcs
ggplot(menu_bbcs, aes(calories)) + 
  geom_density()
# Conc: The positive skew prevalent, however shifted slightly to the right. The range of the calories is large.  

#Add to Report
# Plot Boxplot based on category to show quartiles (more robust metric of location and spread)
menu_df %>% 
  group_by(category) -> menuCat_df
ggplot(menuCat_df, aes(x = category, y = calories)) + 
  geom_boxplot()
# Conc: The boxplot shows that the majority of the data lies below 1000 cal with some outliers in the chicken and breakfast category. 
#       This shows that the 5 points are the main conributers for the positive skew. 

# Get number of obs for each cat
table(menu_df$category)

# Analysis of other variables: 

# plot proportion of calories from fat exclusing the 0 calories entries 
ggplot(menu_df, aes(calories_from_fat / calories)) +
  geom_density()
# Conc: the plot shows a trend with two modes implying that further investigating is required.

#Show table summary of nutrients and their daily percents 
menuCat_df %>% 
  summarise(avgTFat = mean(total_fat), stdevTFat = sd(total_fat), 
            avgSFat = mean(saturated_fat), stdevSFat = sd(saturated_fat),
            avgCho = mean(cholesterol), stdevCho = sd(cholesterol), 
            avgSo = mean(sodium), stdevSo = sd(sodium), 
            avgCarb = mean(carbohydrates), stdevCarb = sd(carbohydrates), 
            avgDF = mean(dietary_fiber), stdevDF = sd(dietary_fiber), 
            avgSug = mean(sugars), stdevSug = sd(sugars), 
            avgProt = mean(protein), stdevProt = sd(protein)
            ) %>% 
  knitr::kable(caption = "The nutrient location and spread of each category of foods.")

menuCat_df %>% 
  summarise(avgTFat = mean(total_fat_percent_daily_value), stdevTFat = sd(total_fat_percent_daily_value),
            avgSFat = mean(saturated_fat_percent_daily_value), stdevSFat = sd(saturated_fat_percent_daily_value),
            avgCho = mean(cholesterol_percent_daily_value), stdevCho = sd(cholesterol_percent_daily_value),
            avgSo = mean(sodium_percent_daily_value), stdevSo = sd(sodium_percent_daily_value),
            avgCarb = mean(carbohydrates_percent_daily_value), stdevCarb = sd(carbohydrates_percent_daily_value),
            avgDF = mean(dietary_fiber_percent_daily_value), stdevDF = sd(dietary_fiber_percent_daily_value),
            avgVa = mean(vitamin_a_percent_daily_value), stdevVa = sd(vitamin_a_percent_daily_value), 
            avgVc = mean(vitamin_c_percent_daily_value), stdevVc = sd(vitamin_c_percent_daily_value), 
            avgCalc = mean(calcium_percent_daily_value), stdevCalc = sd(calcium_percent_daily_value), 
            avgIron = mean(iron_percent_daily_value), stdevIron = sd(iron_percent_daily_value)
            ) %>% 
  knitr::kable(caption = "The daily percent nutrient location and spread of each category of foods.")
# Conc: The tables are long and a bit hard to read in the console but will look better in mark down. 
# plot data using boxplots for better view

# Total fat
ggplot(menuCat_df, aes(x= category, y = total_fat)) + 
  geom_boxplot()

# Total fat percent daily 
ggplot(menuCat_df, aes(x= category, y = total_fat_percent_daily_value)) + 
  geom_boxplot()
# Conc: Most meals (comb of sandwich drink and side) add up to around 100% of daily intake

# Cholestrol 
ggplot(menuCat_df, aes(x= category, y = cholesterol)) + 
  geom_boxplot()

# Cholestrol percent daily 
ggplot(menuCat_df, aes(x= category, y = cholesterol_percent_daily_value)) + 
  geom_boxplot()
# Conc: breakfast menu is very high in cholestrol 

# Sodium  
ggplot(menuCat_df, aes(x= category, y = sodium)) + 
  geom_boxplot()

# Sodium percent daily
ggplot(menuCat_df, aes(x= category, y = sodium_percent_daily_value)) + 
  geom_boxplot()
# Conc: complete meal would add up to close to 100% daily sodium

# Carbs  
ggplot(menuCat_df, aes(x= category, y = carbohydrates)) + 
  geom_boxplot()

# Carbs  percent daily
ggplot(menuCat_df, aes(x= category, y = carbohydrates_percent_daily_value)) + 
  geom_boxplot()
# Conc: complete meal would add up to around 50% of daily carb

# Dietary fiber  
ggplot(menuCat_df, aes(x= category, y = dietary_fiber)) + 
  geom_boxplot()

# Dietary fiber percent daily 
ggplot(menuCat_df, aes(x= category, y = dietary_fiber_percent_daily_value)) + 
  geom_boxplot()
# Conc: complete meal would add up to around  25% of daily intake 

# Sugars  
ggplot(menuCat_df, aes(x= category, y = sugars)) + 
  geom_boxplot()
# Conc: assuming grams units coffee&tea, deserts, and smoothies&shakes have largest values 

# Protein   
ggplot(menuCat_df, aes(x= category, y = protein)) + 
  geom_boxplot()
# Conc: high protein value in complete meals 

# Vitamin A percent daily 
ggplot(menuCat_df, aes(x= category, y = vitamin_a_percent_daily_value)) + 
  geom_boxplot()
# Conc: salads and chicken&fish have high vitamin value, and smoothies have some but low

# Vitamin C percent daily 
ggplot(menuCat_df, aes(x= category, y = vitamin_c_percent_daily_value)) + 
  geom_boxplot()
# Conc: some drinks have high vitamin c value

# Calcium percent daily 
ggplot(menuCat_df, aes(x= category, y = calcium_percent_daily_value)) + 
  geom_boxplot()
# Conc: Smoothies, and coffee have high value calcium 

# Iron percent daily 
ggplot(menuCat_df, aes(x= category, y = iron_percent_daily_value)) + 
  geom_boxplot()
# Conc: red meats have high iron values
