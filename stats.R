setwd("C:/Users/fahad/OneDrive/Documents")
getwd()
data <- read.csv("income_evaluation.csv")

str(data)



# Correlation Analysis
cor(data$capital.gain, data$hours.per.week)

head(data)
summary(data$avg_age)
mean(age)
mean(data$age)

data$avg_age <- as.numeric(as.character(data$avg_age))
data$avg_age 
summary(data$age)

summary(data$avg_age)

table(data$sex)

gender_distribution <- table(data$sex)

barplot(gender_distribution, main="Gender Distribution", xlab="All-Gender", ylab="Count", col=c("pink", "lightblue"))


table(data$sex, data$income)

table(data$education, data$race)



education_race_distribution <- table(data$education, data$race)

# Display the contingency table
print(education_race_distribution)
barplot(education_race_distribution, 
        main="Education Level Across Different Races", 
        xlab="Education Level", 
        ylab="Count", 
        col=c("red", "green", "blue", "purple", "orange"),
        legend=rownames(education_race_distribution), 
        beside=TRUE)


cor(data$education.num, data$capital.gain)


subset_data <- data[, c("education.num", "capital.gain")]
correlation <- cor(subset_data$education.num, subset_data$capital.gain)
print(paste("Correlation between Education Level and Capital Gain:", correlation))

cor(data$hours.per.week, data$capital.loss)



subset_data <- data[, c("hours.per.week", "capital.loss")]


correlation <- cor(subset_data$hours.per.week, subset_data$capital.loss)
print(paste("Correlation between Hours Worked per Week and Capital Loss:", correlation))


aov_result <- aov(data$capital.gain ~ data$occupation)
summary(aov_result)

#below ques not working, level issue
t.test(data$age ~ data$race)

model <- lm(hours.per.week ~ education.num + occupation, data = data)
summary(model)


model <- lm(capital.gain ~ age * marital.status, data = data)
summary(model)



subset_data <- data[, c("education.num", "capital.gain")]

plot(subset_data$education.num, subset_data$capital.gain, 
     main="Scatter Plot of Education Level vs. Capital Gain",
     xlab="Education Level",
     ylab="Capital Gain",
     col="blue",
     pch=16)


subset_data <- data[, c("hours.per.week", "capital.loss")]

boxplot(subset_data$capital.loss ~ subset_data$hours.per.week,
        main="Box Plot of Hours Worked per Week vs. Capital Loss",
        xlab="Hours Worked per Week",
        ylab="Capital Loss",
        col="skyblue")



subset_data <- data[, c("occupation", "capital.gain")]
library(ggplot2)
ggplot(subset_data, aes(x = occupation, y = capital.gain, fill = occupation)) +
  geom_violin(trim = FALSE) +
  labs(title = "Violin Plot of Capital Gain Across Different Occupations",
       x = "Occupation",
       y = "Capital Gain") +
  theme_minimal() 


subset_data <- data[, c("education.num", "occupation", "hours.per.week")]
library(plotly)

ggplot(subset_data, aes(x = education.num, y = hours.per.week, color = occupation)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatter Plot of Hours Worked per Week vs. Education Level",
       x = "Education Level",
       y = "Hours Worked per Week") +
  theme_minimal()


subset_data <- data[, c("age", "capital.gain", "marital.status")]
plot(1, type = "n", xlab = "Age", ylab = "Capital Gain", main = "Capital Gain by Age and Marital Status")


marital_status <- unique(subset_data$marital.status)
for (status in marital_status) {
  subset_status <- subset_data[subset_data$marital.status == status, ]
  lines(subset_status$age, subset_status$capital.gain, type = "o", pch = 16, col = status)
}

# Add legend
legend("topright", legend = marital_status, col = marital_status, pch = 16, title = "Marital Status")


subset_data <- data[, c("age", "capital.gain", "marital.status")]
library(rgl)
open3d()

marital_status <- unique(subset_data$marital.status)
for (status in marital_status) {
  subset_status <- subset_data[subset_data$marital.status == status, ]
  points3d(subset_status$age, subset_status$capital.gain, rep(1, nrow(subset_status)), col = status, type = "p", size = 2)
}

legend3d("topright", legend = marital_status, col = marital_status, pch = 16, title = "Marital Status")


subset_data <- data[, c("age", "capital.gain", "marital.status")]
mosaic(~ marital.status + age + capital.gain, data = subset_data, shade = TRUE)



----
  
  
  subset_data <- data[, c("age", "capital.gain", "marital.status")]

# Calculate Average Capital Gain by Age and Marital Status
average_data <- aggregate(capital.gain ~ age + marital.status, data = subset_data, FUN = mean)

# Create Line Plot
plot(average_data$age, average_data$capital.gain, type = "n", xlab = "Age", ylab = "Average Capital Gain", main = "Average Capital Gain by Age and Marital Status")

# Loop through unique marital status categories
marital_status <- unique(average_data$marital.status)
for (status in marital_status) {
  subset_status <- subset(average_data, marital.status == status)
  lines(subset_status$age, subset_status$capital.gain, col = rainbow(length(marital_status)))
}

# Add Legend
legend("topright", legend = marital_status, col = rainbow(length(marital_status)), lty = 1, title = "Marital Status")


---
  
  
  subset_data <- data[, c("age", "capital.gain", "marital.status")]
boxplot(capital.gain ~ interaction(age, marital.status), data = subset_data, 
        col = rainbow(length(unique(subset_data$age))),
        xlab = "Age and Marital Status",
        ylab = "Capital Gain",
        main = "Distribution of Capital Gain by Age and Marital Status",
        names = paste(unique(subset_data$age), unique(subset_data$marital.status)),
        las = 2)


legend("topright", legend = marital_status, col = rainbow(length(marital_status)), lty = 1, title = "Marital Status")


-------
  

  subset_data <- data[, c("age", "capital.gain", "marital.status")]

# Create Box Plot
boxplot(capital.gain ~ interaction(age, marital.status), data = subset_data, 
        col = rainbow(length(unique(subset_data$age))),
        xlab = "Age and Marital Status",
        ylab = "Capital Gain",
        main = "Distribution of Capital Gain by Age and Marital Status",
        names = paste(unique(subset_data$age), unique(subset_data$marital.status)),
        las = 2)  

---
  
  
  library(vcd)
subset_data <- data[, c("age", "capital.gain", "marital.status")]

mosaic(~ marital.status + age + capital.gain, data = subset_data, shade = TRUE)



----------------
  
  
  
  subset_data <- data[, c("age", "capital.gain", "marital.status")]

# Create Scatter Plot
plot(subset_data$age, subset_data$capital.gain, 
     col = as.numeric(subset_data$marital.status), pch = 16,
     xlab = "Age", ylab = "Capital Gain",
     main = "Scatter Plot of Capital Gain by Age and Marital Status")

# Add Legend
legend("topright", legend = levels(subset_data$marital.status), col = 1:4, pch = 16, title = "Marital Status")

----------
  
  
  subset_data <- data[, c("age", "capital.gain", "marital.status")]

# Create Scatter Plot
plot(subset_data$age, subset_data$capital.gain, 
     col = as.numeric(subset_data$marital.status), pch = 16,
     xlab = "Age", ylab = "Capital Gain",
     main = "Scatter Plot of Capital Gain by Age and Marital Status")

# Add Legend
legend("topright", legend = levels(subset_data$marital.status), col = 1:4, pch = 16, title = "Marital Status")


-------
  
  subset_data <- data[, c("age", "capital.gain", "marital.status")]

# Create Scatter Plot
plot(subset_data$age, subset_data$capital.gain, 
     col = subset_data$marital.status, pch = 16,
     xlab = "Age", ylab = "Capital Gain",
     main = "Scatter Plot of Capital Gain by Age and Marital Status")

# Add Legend
legend("topright", legend = levels(subset_data$marital.status), col = 1:4, pch = 16, title = "Marital Status")


-----
  
  subset_data <- data[, c("age", "capital.gain", "marital.status")]

boxplot(capital.gain ~ interaction(age, marital.status), data = subset_data, 
        col = rainbow(length(unique(subset_data$age))),
        xlab = "Age and Marital Status",
        ylab = "Capital Gain",
        main = "Distribution of Capital Gain by Age and Marital Status",
        names = paste(unique(subset_data$age), unique(subset_data$marital.status)),
        las = 2)


-------------
  
  subset_data <- data[, c("age", "capital.gain", "marital.status")]
barplot(
  tapply(subset_data$capital.gain, list(subset_data$age, subset_data$marital.status), mean),
  beside = TRUE,
  col = c("red", "blue", "green", "purple"),
  main = "Average Capital Gain by Age and Marital Status",
  xlab = "Age",
  ylab = "Average Capital Gain",
  legend.text = unique(subset_data$marital.status),
  args.legend = list(title = "Marital Status")
)

-----------checka


  
  
  subset_data <- data[, c("age", "capital.gain", "race")]
barplot(
  tapply(subset_data$capital.gain, list(subset_data$age, subset_data$race), mean),
  beside = TRUE,
  main = "Average Capital Gain by Age and race",
  xlab = "Age",
  ylab = "Average Capital Gain",
  args.legend = list(title = "race")
)
------
  library(tidyverse)
library(scales)

education_percentage <- data %>%
  group_by(race, education) %>%
  summarise(count = n()) %>%
  group_by(race) %>%
  mutate(percentage = (count / sum(count)) * 100) %>%
  arrange(race, desc(education))

ggplot(education_percentage, aes(x = race, y = percentage, fill = education)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Percentage Distribution of Education Levels by Race",
       x = "Race",
       y = "Percentage") +
  theme_minimal()

-----
  
install.packages("corrplot")
library(corrplot)

library("ggplot2")
library("dplyr")
library("tm") 
library("stats")  
options(warn=-1) 


df <- data

install.packages("corrplot")
library(corrplot)
install.packages("knitr")
library(knitr)


numeric_df <- df[sapply(df, is.numeric)]


correlation_matrix <- cor(numeric_df)


print(correlation_matrix)
kable(correlation_matrix)


---
  #  to remove a specific column , 'df' is  data frame
  df <- df[, -which(names(df) == "income_less_than_equal_to_50K_or_not")]

corrplot(correlation_matrix, method="color", 
         col=colorRampPalette(c("white", "red"))
         (200), 
         type="upper", 
         addCoef.col = "black", 
         tl.col="black", tl.srt=46) 

-----------
  subset_data <- data[, c("occupation", "capital.gain")]

library(ggplot2)

ggplot(subset_data, aes(x = occupation, y = capital.gain, fill = occupation)) +
  geom_violin(trim = FALSE) +
  labs(title = "Violin Plot of Capital Gain Across Different Occupations",
       x = "Occupation",
       y = "Capital Gain") +
  theme_minimal()

------
  library(plotly)
# Create a pie chart for the sum of capital gains by occupation
  capital_gain_by_occupation <- tapply(subset_data$capital.gain, subset_data$occupation, sum)

# Create a color palette for the pie chart
colors <- rainbow(length(capital_gain_by_occupation))

# Create a plotly pie chart
pie_chart <- plot_ly(labels = names(capital_gain_by_occupation),
                     values = capital_gain_by_occupation,
                     type = 'pie',
                     marker = list(colors = colors),
                     textinfo = 'label+percent',
                     textposition = 'inside',
                     insidetextfont = list(color = '#FFFFFF'),  # Text color inside the pie
                     hole = 0.4  # Hole in the middle for a donut chart effect
)

# Set layout options for better visibility
layout_options <- list(title = "Distribution of Capital Gain by Occupation",
                       showlegend = FALSE  # Hide legend for better clarity
)

# Add layout options to the pie chart
pie_chart <- pie_chart %>% layout(layout_options)

# Display interactive pie-chart
pie_chart

-----------
  library(ggplot2)
ggplot(subset_data, aes(x = age, y = capital.gain, fill = marital.status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average Capital Gain by Age and Marital Status",
    x = "Age",
    y = "Average Capital Gain",
    fill = "Marital Status"
  ) +
  theme_minimal()
------







