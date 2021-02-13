library(tidyverse)

ourdata <- read_csv2("data/assignment2_data.csv")
ourdata

# Take a sample of 100 observations from this data, 
# and save this sample as a new data. 

set.seed(2016555019) 
new_data <- ourdata[sample(nrow(ourdata), 100),]
view(new_data)




# 1.Calculate the mean sales’ values for each color.

(mean_for_color <- new_data %>% 
    group_by(Color) %>%
    summarize(Mean_Sales = mean(Sales)))




# 2.Calculate the mean sales’ values for each region.

(mean_for_region <- new_data %>% 
    group_by(Region) %>%
    summarize(Mean_Sales = mean(Sales)))




# 3.Identify the five highest sales’ value according to date, 
#   color and region separately.

highest_sales <- new_data %>%
  group_by() %>%
  summarize(Date, Color, Region, Sales) %>% 
  arrange(desc(Sales)) %>%
  filter(row_number() == 1:5)

view(highest_sales)




# 4.Identify the month in which the mean sales’ value is the highest.

sep_date<-new_data %>%
  separate(Date, into = c("Day", "Month", "Year"), sep ="-")

(highest_month <- sep_date %>% 
    group_by(Month) %>%
    summarize(Mean_Sales = mean(Sales)) %>%
    arrange(desc(Mean_Sales)) %>%
    filter(row_number() == 1))




# 5.Plot the sales according to years.

sep_date <- new_data %>%
  separate(Date, into = c("Day", "Month", "Year"), sep ="-")

ggplot(data = sep_date) +
  geom_boxplot( mapping = aes(x = Year, y = Sales, color = Year)) 


















