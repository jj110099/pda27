install.packages("Hmisc")
install.packages("datarium")
library(readr) #read csv file
library(dplyr) #data wrangling & manipulation
library(Hmisc) #describe data
library(ggplot2) #data visualization
library(datarium) #data used in project
library(caret) #splitting training and testing data
data("marketing", package = "datarium")
marketing_plan <- marketing
marketing_plan

#1

marketing_plan %>%
  ggplot(aes(x = youtube, y = sales)) +
  geom_point() +
  labs(x = "Spending on YouTube ads",
       y = "Sales",
       title = "Graph 1: Relationship between YouTube ads and sales") +
  stat_smooth(se = FALSE) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))

marketing_plan %>%
  ggplot(aes(x = facebook, y = sales)) +
  geom_point() +
  labs(x = "Spending on Facebook ads",
       y = "Sales",
       title = "Graph 2: Relationship between Facebook ads and sales") +
  stat_smooth(se = FALSE) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))

marketing_plan %>%
  ggplot(aes(x = newspaper, y = sales)) +
  geom_point() +
  labs(x = "Spending on newspaper",
       y = "Sales",
       title = "Graph 3: Relationship between newspaper and sales") +
  stat_smooth(se = FALSE) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))

#4
set.seed(1)
train_indices <- createDataPartition(y = marketing[["sales"]],
                                     p = 0.8,
                                     list = FALSE)
train_listings <- marketing[train_indices,]
test_listings <- marketing[-train_indices,]

#5

model_0 <- lm(sales ~ youtube + facebook + newspaper,
              data = train_listings)
summary(model_0)

#We can see that the p-values for youtube and facebook is extremely small, which means that we reject the null hypothesis that YouTube and Facebook do not impact sales. On the other hand, the p-value for newspaper is greater than 0.05, which means it’s not a significant value. We fail to reject the null hypothesis that there is any significant relationship between newspaper ads and sales. I will create a second model to exclude the variable newspaper.

model_1 <- lm(sales ~ youtube + facebook,
              data = train_listings)
summary(model_1)

model_2 <- lm(sales ~ facebook + I(facebook^2) + youtube + I(youtube^2),
              data = train_listings)

summary(model_2)

model_3 <- lm(sales ~ facebook + poly(youtube, 5),
              data = train_listings)

summary(model_3)

#6
model_4 <- lm(sales ~ facebook + poly(youtube, 3) + facebook*youtube,
              data = train_listings)

summary(model_4)
