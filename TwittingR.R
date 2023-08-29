


setwd("C:/Users/jwainaina/OneDrive - Kemri Wellcome Trust/Documents/R Codes/X-Twitter-Posts/")


"📊 Mastering R Functions 📊
Functions are the building blocks of #RStats programming. 🧱✨ 
They help you encapsulate logic, reuse code, 
and make your analysis more efficient. 
From simple calculations to complex data manipulations, 
R functions are your secret sauce! 🍔🔍 #DataScience #CodingTips"

"Today`s example: Want to calculate and report 
documented/data available (N), (%) across a set of selected variables"


my_function <- function(data, selected_columns) {
  
  results <- lapply(data[, selected_columns], function(x) {
    doc_counts <- sum(!is.na(x)) # sum of not missing/data available
    doc_percentages <- doc_counts / length(x) * 100 # % data available
    # create a dataframe to store the results
    data.frame( 
      N = doc_counts,
      N_Percent = doc_percentages
    )
  })
  
  results_df <- do.call(rbind, results) # use rbind to bind above N and N_percent rows for each col
  
  return(results_df)
}

# Assuming 'data' is your dataframe and 
# 'selected_columns' are the columns you want to analyze
selected_cols <- names(data[, 10:15]) # select your columns 
results <- my_function(data, selected_cols) # apply your function on selected columns
print(results)



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Explore efficiency of data.table when using large datasets
# Using dplyr
# Calculate average value for each group
library(dplyr)
result_df <- df %>%
  group_by(group) %>%
  summarize(avg_value = mean(value))

# Using data.table
library(data.table)
# Convert data frame to data.table
dt <- as.data.table(df)
# Calculate average value for each group
result_dt <- dt[, .(avg_value = mean(value)), by = group]


# +++++++++++++++++++++++++++++++++++++++++++++++++++

# 14th August 2023
## Using lubridate package
## Load the lubridate package
library(lubridate)

# Create a date-time object
datetime <- ymd_hms("2023-08-12 15:30:45")

# Extract components
year_val <- year(datetime)
month_val <- month(datetime)
day_val <- day(datetime)
hour_val <- hour(datetime)
minute_val <- minute(datetime)
second_val <- second(datetime)

# Perform calculations
future_datetime <- datetime + days(7)
time_difference <- difftime(future_datetime, datetime, units = "hours")

print(time_difference)


# ++++++++++++++++++++++++++++++++++++++++++


# Load necessary libraries
library(dplyr)

# Sample dataset: Yearly mortality counts
mortality_data <- data.frame(
  year = c(2010, 2011, 2012, 2013, 2014),
  mortality_count = c(1500, 1600, 1550, 1700, 1800)
)

# Calculate change in mortality using lag function
mortality_changes <- mortality_data %>%
  arrange(year) %>%
  mutate(
    mortality_change = mortality_count - lag(mortality_count),
    mortality_change_percentage = (mortality_change / lag(mortality_count)) * 100,
    change_color = ifelse(mortality_change >= 0, "red", "green")
  )


library(ggplot2)

ggplot(mortality_changes, aes(x = year, y = mortality_change,
                              fill = change_color)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = mortality_change), vjust = -.5, color = "black") +
  labs(title = "Change in Mortality Over Years",
       x = "Year",
       y = "Mortality Change") +
  theme_minimal() +
  scale_fill_identity() +
  scale_y_continuous(limits = c(-60, 200)) +
  theme(legend.position = "none")


# ++++++++++++++++++++++++++++++++++++++++++++++++++++

library(data.table)

# create sample data
dt <- data.table(
  id = c(1, 2, 3, 4),
  value = c(10, 20, 30, 40)
)

# Update a value using :=
dt[id == 3, value := 35]

# Print the updated data.table
print(dt)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++

# Drugs overdose in USA

library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(ggplot2)
library(extrafont)


data <- read.csv(file = 'VSRR_Provisional_Drug_Overdose_Death_Counts.csv', 
               #  row.names = F,
                 header = T)

# combine state abbreviation and state name

data <- unite(data, State_and_abbr, State, State.Name, sep = "-")

data$Value <- gsub(',', "", data$Data.Value)

data$Value <- as.numeric(data$Value)

deaths <- data %>% filter(Indicator == "Number of Deaths")

summ <- 
deaths %>% 
  group_by(Year, Month, State_and_abbr) %>% 
  summarise(Deaths = sum(Value, na.rm = T)) %>% 
  filter(Year >= "2017" &
           !State_and_abbr %in% c("YC-New York City", "US-United States"))


month_order <- month.abb


# Create plots for each state
for (state in unique(summ$State_and_abbr)) {
  state_data <- summ %>% filter(State_and_abbr == state)
  
  # Convert Month to a factor with specified order
  state_data$Month <- month.abb[match(state_data$Month, month.name)]
  state_data$Month <- factor(state_data$Month, levels = month_order)

  p <- ggplot(data = state_data, aes(x = Month, y = Deaths)) +
    geom_line(aes(group = State_and_abbr), color = "blue") +
    geom_point(aes(color = State_and_abbr)) +
    theme_minimal() +
    labs(title = state,
         subtitle = paste0("Monthly Provisional Drug Overdose Death Counts in ", state)) +
    facet_wrap(~ Year, scales = 'free') +
    theme(legend.position = 'none',
          plot.title = element_text(hjust = .5, size = 20),
          plot.subtitle = element_text(hjust = .5, size = 15, color = "red"),
          axis.title = element_text(size = 15),
          text = element_text(face = 'bold', family = 'Maiandra GD'))
    
  dev.off()
  
  print(p)
}






