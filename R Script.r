### Course 3: Advanced Analytics for Organisational Impact

### Contents: 
### 1. Loading and cleaning the data
### 2. Exploratory analysis
### 3. Multi-linear regression of loyalty points
### 4. Predictive analysis 
### 5. Visualisations
### 6. Statistical testing 

#############################################################

# Section 1: Loading and cleaning the data 

#############################################################

# Import the necessary packages.
library (tidyverse) 
library (moments)
library(readr)
library(dplyr)
library(tidyr)
library(skimr)
library(car)
library(plotly)
library(htmlwidgets)

# Load the data
turtle <- read.csv('turtle_reviews.csv', header=TRUE)

# View the data
View(turtle)
as_tibble(turtle)
colnames(turtle)

# View the dimensions of the dataset
dim(turtle)

# Check for NAs
sum(is.na(turtle))

# Summarise the dataset
summary(turtle)

# Drop unnecessary columns
turtle <- subset(turtle,
                     select = -c(language, platform))

# Rename columns
turtle <- turtle %>%
  rename(spend_score = spending_score..1.100.,
        income = remuneration..k..)

# Check the values in the education column 
turtle %>% count(education)

# Combine the 'Basic' and 'diploma' category
turtle <- turtle %>% 
  mutate(education = case_when(
    education %in% c("Basic", "diploma") ~ "high_school_or_less",
    TRUE ~ education
  ))

# Check
turtle %>% count(education)

# Change education column to categorical 
turtle$education <- as.factor(turtle$education)
class(turtle$education)

# Change gender column to categorical 
turtle$gender <- as.factor(turtle$gender)
class(turtle$gender)

# Check for duplicated rows
sum(duplicated(turtle))
# No duplicated rows

### Section 1 Summary

# Data is already very clean: no nulls, no duplicates, seemingly no incorrectly inputted data
# Minor cleaning required, incl. 
  # renaming columns, 
  # cleaning and converting education into category, 
  # converting gender into category.
# Dropped language and platform columns as all the same value. Note: worth checking if true with new data. 

#############################################################

# Section 2: Exploratory analysis 

#############################################################

# Create function to explore each variable easily 
variable_explore <- function(df, col) {
  # Convert col (string) to a symbol
  col_sym <- rlang::sym(col)
  
  # Print descriptive statistics
  print(summary(df[[col]]))
  
  # Plot histogram
  ggplot(df, aes(x = !!col_sym)) +
    geom_histogram(color = "white", fill = "skyblue") +
    labs(
      title = paste("Histogram of", col),
      x = col,
      y = "Count"
    ) +
    theme_minimal()
}

# Explore main variables using a loop
cols <- c('income', 'age', 'spend_score', 'loyalty_points')

for (col in cols) {
  cat("\n===== Column:", col, "=====\n")
  p <- variable_explore(turtle, col)
  print(p)
}

# Check correlations between all numeric columns
numeric_cols <- sapply(turtle, is.numeric)
cor(turtle[, numeric_cols])

# Visualise relationship between loyalty points and income / spend_score
# Given two variables with highest correlations
cols <- c('income', 'spend_score')

for (col in cols) {
  p <- ggplot(turtle, aes_string(x=col, y='loyalty_points')) + 
    geom_point(colour='skyblue') +
    labs(
      title = paste("Loyalty Points vs", col),
      x = col,
      y = "Loyalty Points") +
      theme_minimal()
    print(p)
}

# Visualise relationship between income and spend_score
ggplot(turtle, aes(x=spend_score, y=income)) + 
  geom_point(colour='skyblue') +
labs(
  title = paste("Spend score vs. Income"),
  x = "Spend Score",
  y = "Income") +
  theme_minimal()

# Check whether numerical columns are normally distributed
cols <- c('income', 'age', 'spend_score', 'loyalty_points')

for (col in cols) {
  cat("\n=== Shapiro test for:", col, "===\n")
  print(shapiro.test(turtle[[col]]))
}
# P values for all numeric columns is <0.01 so can assume not normally distributed

### Section 2 Summary
# Numerical columns are not normally distributed. 
# Income, age, spend_score all on similar scales. 
# Loyalty points clustered around lower end of scale, with long tail. 
# Strong correlations between loyalty points and income/spend.
# Income and spend are not themselves highly correlated so lower risk of multicollinearity.
# Potential emerging clusters of customers when looking at spend vs. income. 

#############################################################

# Section 3: Multi-Linear Regression

#############################################################

# Based on analysis completed in Python, most appropriate regression is: 
# loyalty points ~ (income, spend_score)
model1 <- lm(loyalty_points ~ income + spend_score,
             data = turtle)

summary(model1)

# Plot residuals
plot(model1$residuals)
qqnorm(residuals(model1))

# Checking against full model 
model2 <- lm(loyalty_points ~ 
               income + spend_score + age + gender + education,
             data = turtle)

summary(model2)

# Plot residuals
plot(model1$residuals)
qqnorm(residuals(model1))

# Check multi-collinearity
vif(model2)

### Notes: 
# All variables in model 2 are statistically significant. 
# However only very small gains in the R^2.
# In this instance, choose simplicity over small gains in predictive capacity.
# Use model 1

# Test whether model improves by taking log(loyalty_points)
# To reduce issues of non-normality
model3 <- lm(log(loyalty_points) ~ income + spend_score,
             data = turtle)

summary(model3)

# Plot residuals
plot(model3$residuals)
qqnorm(residuals(model3))
# No improvements in R^2

# Using model 1 moving forward, can be used to predict loyalty_points
# E.g. a new batch of customers have starting buying from Turtle Games
# But they have not yet joined the loyalty points scheme: 
new_customers <- data.frame(
  income = c(38.4, 50, 22.5),     
  spend_score = c(57, 70, 40)       
)

predict(model1, newdata = new_customers)

# Binding predictions to original table and adding confidence intervals
predicted_intervals <- predict(model1, newdata = new_customers, interval = "prediction")
cbind(new_customers, predicted_intervals)

# Spend_score is one of the biggest drivers of loyalty_points
# However, it itself is not a completely independent variable
# i.e. could not predict loyalty_points for potential customers
# as we don't yet know their spend.
# Therefore, need to predict spend_score with customer characteristics. 

# Start modelling all explanatory variables
model4 <- lm(spend_score ~ income + age + gender + education,
             data = turtle)
summary(model4)

# Check multi-collinearity
vif(model4)

# Remove gender
model5 <- lm(spend_score ~ income + age + education,
             data = turtle)
summary(model5)

# Remove income
model6 <- lm(spend_score ~ age + education,
             data = turtle)
summary(model6)
# Although statistically significant variables, 
# very low explanatory power (R^2 only 5.5%)

### Section 3 Summary
# Multi-linear regressions are not the best predictor of loyalty points
# This is due to presence of clear sub-groups within customer base
# which act differently creating a non-linear relationship. 
# Better to use decision trees (see Python script).



#############################################################

# Section 4: Predictive Analysis

#############################################################

# To improve the uptake of the loyalty points scheme, need to target
# Group 0 which is the largest proportion by far.
# Based on python clustering analysis, group 0 (mid income, mid spend),
# have a lot of potential to spend more and benefit from the loyalty scheme, 
# but there is very low uptake and potential risk of churn. 
# If managed to increase spend_score by 10% through marketing, 
# what impact would this have on loyalty_points? 

# Import dataset with clusters attached. 

turtle_clusters <- read.csv('turtle_clusters.csv', header=TRUE)

# View the data
View(turtle_clusters)
as_tibble(turtle_clusters)
colnames(turtle_clusters)

# Change columns name 
turtle_clusters <- turtle_clusters %>%
  rename(cluster = K.Means.Predicted)

# Select just group 0
group0 <- turtle_clusters %>%
  filter(cluster == 1) %>%
  select('spend_score', 'income', 'loyalty_points')

as_tibble(group0)

# Uplift spend_score by 10 points
group0_uplift <- group0 %>%
  mutate(spend_score = spend_score * 1.1)

as_tibble(group0_uplift)

# Create a group 0 specific model
model_group0 = lm(loyalty_points ~ income + spend_score,
                    data = group0)

# Predict using original spend_score
old_pred <- predict(model_group0, newdata = group0, interval = "prediction")

# Predict using uplifted spend_score
new_pred <- predict(model_group0, newdata = group0_uplift, interval = "prediction")

# Compare average predicted values
mean(old_pred[, "fit"])
mean(new_pred[, "fit"])

# Visualise the difference
df_compare <- data.frame(
  old = old_pred[, "fit"],
  new = new_pred[, "fit"]
)

ggplot(df_compare, aes(x = old, y = new)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    x = "Predicted loyalty points (original spend_score)",
    y = "Predicted loyalty points (spend_score + 10)"
  ) +
  theme_minimal()

# Percentage increase
(mean(new_pred[, "fit"]) - mean(old_pred[, "fit"])) / mean(old_pred[, "fit"]) * 100


# Group 2 are the high income, high spend group who have the most loyalty points. 
# We want to target more customers like this. 
# If we increased Group 2 customers by 10%, what would be the impact on 
# average spend score for the whole customer base?

# Current average spend score
current_avg_spend = mean(turtle_clusters$spend_score)

# Current number of Group 2 customers 356
# Assume 10% increase in customer base, i.e. 200
# New Group 2 customers would be 556

# Uplift average spend score
current_total_spend = sum(turtle_clusters$spend_score)

group1_avg_spend = mean(turtle_clusters$spend_score[turtle_clusters$cluster == 2], 
                        na.rm = TRUE)

new_total_spend = current_total_spend + (200*group1_avg_spend)

new_avg_spend = new_total_spend / 2200
new_avg_spend

#############################################################

# Section 5: Visualisations

#############################################################

# Interactive visualisation to show the different clusters of customers

# Make cluster a factor
turtle_clusters <- turtle_clusters %>%
  mutate(cluster = factor(cluster))

# Interactive scatter plot
p <- plot_ly(
  data = turtle_clusters,
  x = ~income,
  y = ~spend_score,
  color = ~cluster,           
  colors = "#0E4714",         
  type = "scatter",
  mode = "markers",
  frame = ~cluster,
  marker = list(size = 10, line = list(width = 1, color = 'white')),
  text = ~paste("Cluster:", cluster, "<br>Income:", income, "<br>Spend:", spend_score),
  hoverinfo = "text"
) %>%
  layout(
    title = "Customers by Cluster",   
    xaxis = list(title = "Income (£000s)", fixedrange = TRUE), 
    yaxis = list(title = "Spend Score (0-100)", fixedrange = TRUE)  
  )

p<- p %>% animation_slider(currentvalue = list(prefix = 'Cluster ',
                                           font = list(color = 'black'))) %>% 
  animation_opts(frame = 10000,
                 easing = 'circle-in')

# Save scatterplot
saveWidget(p, "Customer Clusters.html", selfcontained = TRUE)



# Visualisation for each group's loyalty points

# Convert cluster to labeled factor
turtle_clusters <- turtle_clusters %>%
  mutate(cluster = factor(cluster,
                          levels = sort(unique(cluster)),
                          labels = paste("Group", sort(unique(cluster)))))

# Create scatter plot
ggplot(turtle_clusters, aes(x = cluster, y = loyalty_points, color = cluster)) +
  geom_jitter(width = 0.2, size = 3, alpha = 0.7) +  # jitter adds horizontal spread for visibility
  labs(
    title = "Loyalty Points by Customer Group",
    x = "Customer Group",
    y = "Loyalty Points"
  ) +
  scale_color_manual(values = c("#0E4714", "#1B6B1F", "#2F8B30", "#55AA55", "#9CD49C")) +
  theme_minimal(base_size = 14) +
  theme(legend.position= "none",
        plot.title = element_text(hjust = 0.5))
› 


#############################################################

# Section 6: Statistical testing  

#############################################################

# Check that Group 2 has statistically different loyalty points
# To the rest of the population

# Create two groups
group2 = turtle_clusters %>%
        filter(cluster == 2) %>%
        select(loyalty_points)

all_other_groups = turtle_clusters %>%
                  filter(cluster != 2 ) %>%
                  select(loyalty_points)

# Check their variance 
var(group2$loyalty_points)
var(all_other_groups$loyalty_points)

# Check whether normally distributed
shapiro.test(group2$loyalty_points)
shapiro.test(all_other_groups$loyalty_points)

# Use Mann-Whitney U test given not normal and big diff in variance
wilcox.test(group2$loyalty_points, all_other_groups$loyalty_points)

# Very low p value, so reject the null hypothesis that they're the same
# Therefore, statistically significant that group 1 has more loyalty_points


# Check whether there is any difference in the loyalty points
# of men and women 

# Create two groups 
women = turtle %>% 
        filter(gender == 'Female') %>%
        select(loyalty_points)

men = turtle %>% 
  filter(gender == 'Male') %>%
  select(loyalty_points)

# Check their variance
var(women$loyalty_points)
var(men$loyalty_points)

# Check whether normally distributed
shapiro.test(women$loyalty_points)
shapiro.test(men$loyalty_points)

# Use Mann-Whitney U test given not normal and big diff in variance
wilcox.test(women$loyalty_points, men$loyalty_points)

median(women$loyalty_points)
median(men$loyalty_points)

# Statistically significant at 5% level (i.e. good but modest)


