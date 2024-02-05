library(readr)
library(dplyr)
library(ggplot2)
library(reshape)
library(tidyr)

#Graph 1
mental_health <- read_csv("mental_health.csv" , col_types = cols(), skip = 0)
mental_health

# Bar Chart for perceived mental health for Canadians
ggplot(mental_health, aes(x = as.factor(Year), y = num_persons, fill = mental_health_status)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "Number of persons (in thousands)", 
       title = "Perceived Mental Health by Year",
       subtitle = "Comparison of persons with very good or excellent vs fair or poor perceived mental health") +
  scale_fill_manual(values = c("lightgreen", "lightblue")) +
  theme_minimal()

#Graph 2
Job <- read_csv("Job satisfaction.csv" , col_types = cols(), skip = 0)
Job

# Reshape the data into a longer format for plotting
Job_new <- tidyr::pivot_longer(Job, cols = c(Satisfied, Neither, Dissatisfied),
                               names_to = "Job_satisfaction", values_to = "Proportion")

# Create the grouped bar chart
ggplot(Job_new, aes(x = Age_group, y = Proportion, fill = Job_satisfaction)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("lightgreen", "orange", "lightblue")) +
  labs(title = "Job satisfaction by age group",
       x = "Age group",
       y = "Proportion",
       fill = "Job satisfaction category") +
  theme_minimal()

#Graph 3
count <- read_csv("Someone_to_count_on.csv" , col_types = cols(), skip = 0)
count

# convert the data frame from wide to long format using tidyr
df_count <- count %>% 
  pivot_longer(cols = -Indicators, names_to = "Region", values_to = "Percentage")

# create the stacked bar chart using ggplot2
ggplot(df_count, aes(x = Region, y = Percentage, fill = Indicators)) + 
  geom_col() + 
  labs(title = "Survey Results by Region", x = "Region", y = "Percentage") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

#graph 4
local_env <- read_csv("local_environment.csv" , col_types = cols(), skip = 0)
local_env

# Summarize the data by indicators
local_env_summary <- data.frame(
  Indicators = local_env$Indicators,
  Percentage = rowSums(local_env[, -1])
)

# Create the pie chart
ggplot(local_env_summary, aes(x = "", y = Percentage, fill = Indicators)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(fill = "Indicators") +
  ggtitle("Distribution of Local Environment ratings among Canadians") +
  scale_fill_manual(values = c("lightgreen", "lightblue", "lightpink")) +
  theme_bw() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 14)
  )

#graph 5
df<- read_csv("childhood_maltreatment.csv" , col_types = cols(), skip = 0)
df

#creating plot
ggplot(df, aes(x = Age_group)) +
  geom_line(aes(y = Physical_abuse, color = "Physical abuse", group = 1), size = 1.5) +
  geom_line(aes(y = Sexual_abuse, color = "Sexual abuse", group = 1), size = 1.5)+
  labs(title = "Childhood abuse by Age group among Canadians",
       x = "Age Group",
       y = "Percentage") +
  scale_size(range = c(1, 2), guide = "none") +
  theme(plot.title = element_text(hjust = 0.5))

