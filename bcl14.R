# Load necessary packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(gridExtra)
library(RColorBrewer)
library(treemap)
library(plotly)

# Choose a color palette
colors <- brewer.pal(9, "YlGnBu")

budget_data <- read.csv('/Users/rajkanwarsingh/code/CSE3006/assignment/2014_Endorsed_Budget_-_Expenditure_Allowance_by_Budget_Control_Level__BCL_.csv')

#check dimensions of data
dim(budget_data)

# Check for missing values in the entire dataset
sum(is.na(budget_data))

# Check for missing values in all columns
sapply(budget_data, function(x) sum(is.na(x)))

#duplicate check
sum(duplicated(budget_data))

# View structure of data
str(budget_data)

# View summary of data
summary(budget_data)

# Correct data types
budget_data$Fund <- as.factor(budget_data$Fund)
budget_data$Department <- as.factor(budget_data$Department)
budget_data$BCL.Code <- as.factor(budget_data$BCL.Code)
budget_data$BCL.Name <- as.factor(budget_data$BCL.Name)
budget_data$BCL.Purpose <- as.factor(budget_data$BCL.Purpose)

#outlier check
boxplot(budget_data$Fund, main = "Fund", ylab = "Expenditure Allowance", col = "steelblue")
boxplot(budget_data$Department, main = "Department", ylab = "Expenditure Allowance", col = "darkgreen")
boxplot(budget_data$BCL.Code, main = "BCL Code", ylab = "Expenditure Allowance", col = "purple")
boxplot(budget_data$BCL.Name, main = "BCL Name", ylab = "Expenditure Allowance", col = "red")
boxplot(budget_data$BCL.Purpose, main = "BCL Purpose", ylab = "Expenditure Allowance", col = "orange")

boxplot(
  budget_data$`X2014.Expenditure.Allowance`,
  main = "2014 Expenditure Allowance",
  ylab = "Expenditure Allowance",
  col = "blue",
  boxwex = 0.5, # Narrower box
  outpch = 20, # Solid circle for outlier points
  outcol = "red" # Red color for outlier points
)

# Create new variable for total expenditure
budget_data$total_expenditure <- budget_data$`X2014.Expenditure.Allowance`

# Summarize total expenditure by department
department_summary <- budget_data %>%
  group_by(Department) %>%
  summarise(total_expenditure = sum(total_expenditure)) %>%
  arrange(desc(total_expenditure))

# Create a vector of custom colors
my_colors <- c("steelblue", "orange", "green", "purple", "red", "blue", "yellow", "brown", "grey", "pink")

# Create a bar plot of top 10 departments by total expenditure
ggplot(head(department_summary, 10), aes(x = Department, y = total_expenditure, fill = Department)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = my_colors) +  # Use custom colors
  xlab("Department") +
  ylab("Total Expenditure") +
  ggtitle("Top 10 Departments by Total Expenditure in 2014") 

# Summarize total expenditure by fund
fund_summary <- budget_data %>%
  group_by(Fund) %>%
  summarise(total_expenditure = sum(total_expenditure)) %>%
  arrange(desc(total_expenditure))

# Calculate the total expenditure allowance for each BCL code
expenditure_by_bcl <- budget_data %>%
  group_by(BCL.Code) %>%
  summarize(total_expenditure = sum(`X2014.Expenditure.Allowance`))


# Create a bar plot without key
ggplot(expenditure_by_bcl, aes(x = BCL.Code, y = total_expenditure, fill = BCL.Code)) +
  geom_bar(stat = "identity") +
  xlab("BCL Code") +
  ylab("Total Expenditure Allowance") +
  ggtitle("Distribution of Expenditure Allowance by BCL Code") +
  scale_fill_hue(l = 40, c = 80) + # adds a color scale
  guides(fill = FALSE) # removes the legend/key

# count unique elements
length(unique(budget_data$BCL.Code))

# Create the scatterplot
ggplot(budget_data, aes(x = `X2014.Expenditure.Allowance`, y = Department, color = Department)) +
  geom_point() +
  scale_color_hue(l = 40, c = 80) + # adds a color scale
  xlab("Expenditure Allowance") +
  ylab("Department Count") +
  ggtitle("Relationship Between Expenditure Allowance and Department Count")

# Create a density plot without key
ggplot(budget_data, aes(x = `X2014.Expenditure.Allowance`, fill = Department)) +
  geom_density(alpha = 0.5) +
  xlab("Expenditure Allowance (in R)") +
  ylab("Density") +
  ggtitle("Expenditure Allowance by Department") +
  scale_fill_hue(l = 40, c = 80) + # adds a color scale
  theme_classic() +
  guides(fill = FALSE) # removes the legend/key

# Create a summary table with counts of departments by fund and expenditure allowance
dept_summary <- budget_data %>%
  group_by(Fund, `X2014.Expenditure.Allowance`) %>%
  summarize(count = n())

# Create a heatmap
ggplot(dept_summary, aes(x = `X2014.Expenditure.Allowance`, y = Fund, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(x = "Expenditure Allowance", y = "Fund", title = "Department Distribution by Fund and Expenditure Allowance")

# Define the expenditure allowance ranges
budget_data$ranges <- cut(budget_data$X2014.Expenditure.Allowance, breaks = seq(0, 8000000, by = 1000000))

# Create a summary table with counts of departments by fund and expenditure allowance range
dept_summary <- budget_data %>%
  group_by(Fund, ranges) %>%
  summarize(count = n())

# Create a stacked bar chart
ggplot(dept_summary, aes(x = Fund, y = count, fill = ranges)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#FF9999", "#FF6666", "#FF3333", "#FF0000", "#CC0000", "#990000", "#660000", "#330000")) +
  labs(x = "Fund", y = "Number of Departments", title = "Number of Departments by Fund and Expenditure Allowance Ranges") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#highest expenditure allowance in department
expenditure_by_dept <- budget_data %>%
  group_by(Department) %>%
  summarize(total_expenditure = sum(`X2014.Expenditure.Allowance`)) %>%
  arrange(desc(total_expenditure))

# Create the bar chart
ggplot(expenditure_by_dept, aes(x = Department, y = total_expenditure, fill = total_expenditure)) +
  geom_bar(stat = "identity") +
  scale_fill_gradientn(colors = colors, guide = FALSE) +
  xlab("Department") +
  ylab("Total Expenditure Allowance") +
  ggtitle("Total Expenditure Allowance by Department in 2014") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Create bubble chart
ggplot(budget_data, aes(x = `X2014.Expenditure.Allowance`, y = Department, size = `X2014.Expenditure.Allowance`, color = Department)) +
  geom_point(alpha = 0.7) +
  scale_color_hue(l = 40, c = 80) + # adds a color scale
  xlab("Expenditure Allowance (in R)") +
  ylab("Department") +
  ggtitle("Expenditure Allowance by Department") +
  theme_classic()

# Create bubble chart
ggplot(department_summary, aes(x = total_expenditure, y = Department, size = total_expenditure, color = Department)) +
  geom_point(alpha = 0.7) +
  scale_color_hue(l = 40, c = 80) + # adds a color scale
  xlab("Total Expenditure (in R)") +
  ylab("Department") +
  ggtitle("Total Expenditure by Department") +
  theme_classic()

# Aggregate the data by BCL code and department
agg_data <- aggregate(X2014.Expenditure.Allowance ~ BCL.Code + Department, data = budget_data, sum)

# Create the treemap
treemap(agg_data, 
        index = c("Department", "BCL.Code"), 
        vSize = "X2014.Expenditure.Allowance", 
        type = "index",
        palette=brewer.pal(9, "Set1"),
        title = "Seattle 2014 Endorsed Budget by BCL Code and Department")

budget_data$Fund <- as.numeric(budget_data$Fund)
budget_data$Department <- as.numeric(budget_data$Department)

plot_ly(budget_data, x = ~Fund, y = ~Department, z = ~`X2014.Expenditure.Allowance`, color = ~`X2014.Expenditure.Allowance`,
        marker = list(size = 7, opacity = 0.8, colorscale = 'Viridis')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = "Fund"),
                      yaxis = list(title = "Department"),
                      zaxis = list(title = "2014 Expenditure Allowance")))