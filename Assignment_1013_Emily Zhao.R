library(dplyr)
library(stringr)
library(lubridate)
library(readxl)
library(tidyverse)
library(tidyr)
library(data.table)
library(glmnet)
library(ggcorrplot)
library(randomForest)

Customers <- read.csv("C:\\Data\\customers.csv")
Locations<-read.csv("C:\\Data\\locations.csv")
order_items<-read.csv("C:\\Data\\order_items.csv")
orders<-read.csv("C:\\Data\\orders.csv")
products<-read.csv("C:\\Data\\products.csv")

#their favorite item, the number of orders they've placed during the last month, and the number of orders they've placed during their lifetime

#If R Code

Customer4 <- orders %>%
  left_join(Customers, by = "CUSTOMER_ID") %>%
  left_join(order_items, by = "ORDER_ID") %>%
  left_join(products, by = "PRODUCT_ID") %>%
  left_join(Locations, by = "LOCATION_ID")

Brooklyn <- Customer4 %>%filter(LOCATION_NAME=="Brooklyn")
frequency_table <- table(Brooklyn$PRODUCT_ID)
print(frequency_table)


Brooklyn <- Customer4 %>%filter(LOCATION_NAME=="Brooklyn")
frequency_table <- table(Brooklyn$PRODUCT_ID)
print(frequency_table)
Philadelphia <- Customer4 %>%filter(LOCATION_NAME=="Philadelphia")
frequency_table <- table(Philadelphia$PRODUCT_ID)
print(frequency_table)




Selected<-Customer4%>%select(CUSTOMER_ID,CUSTOMER_NAME,COUNT_LIFETIME_ORDERS,LAST_ORDERED_AT,ORDER_ID,PRODUCT_NAME,ORDER_TOTAL,ORDER_ID,ORDERED_AT.x,PRODUCT_ID)
Selected<-Selected%>%rename(ORDERED_AT=ORDERED_AT.x)
Selected$ORDERED_AT<- month(Selected$ORDERED_AT)

#my answer1
#fav item
customer_data <- Selected %>%
  group_by(CUSTOMER_ID) %>%
  summarize(
    favorite_item = names(which.max(table(PRODUCT_ID)))
  )

#order in the last month
Order_last<-Selected%>%group_by(CUSTOMER_ID)%>%filter(ORDERED_AT=="5")%>%summarize(orders = n_distinct(ORDER_ID))


#the number of orders they've placed during their lifetime
Order_lifetime<-Selected%>%group_by(CUSTOMER_ID)%>%summarise(sum = sum(COUNT_LIFETIME_ORDERS))
Order_lifetime%>%
  summarise(sum(sum, na.rm = TRUE))

#Answer: 8411037

#my answer2

###If SQL Code

# WITH favorite_items AS (
#   SELECT 
#   o.CUSTOMER_ID, 
#   oi.PRODUCT_ID, 
#   p.PRODUCT_NAME,
#   COUNT(oi.ORDER_ID) AS order_count
#   FROM orders o
#   JOIN order_items oi ON o.ORDER_ID = oi.ORDER_ID
#   JOIN products p ON oi.PRODUCT_ID = p.PRODUCT_ID
#   GROUP BY o.CUSTOMER_ID, oi.PRODUCT_ID, p.PRODUCT_NAME
# ),
# customer_favorite AS (
#   SELECT CUSTOMER_ID, PRODUCT_NAME AS favorite_item
#   FROM (
#     SELECT CUSTOMER_ID, PRODUCT_NAME, 
#     ROW_NUMBER() OVER (PARTITION BY CUSTOMER_ID ORDER BY order_count DESC) AS rank
#     FROM favorite_items
#   ) ranked_favorites
#   WHERE rank = 1
# ),
# monthly_orders AS (
#   SELECT CUSTOMER_ID, COUNT(ORDER_ID) AS orders_last_month
#   FROM orders
#   WHERE ORDERED_AT BETWEEN '2017-05-01' AND '2017-05-31'
#   GROUP BY CUSTOMER_ID
# ),
# lifetime_orders AS (
#   SELECT CUSTOMER_ID, COUNT(ORDER_ID) AS lifetime_orders
#   FROM orders
#   GROUP BY CUSTOMER_ID
# )
# SELECT 
# c.CUSTOMER_ID, 
# c.CUSTOMER_NAME,
# COALESCE(cf.favorite_item, 'No favorite item') AS favorite_item,
# COALESCE(mo.orders_last_month, 0) AS orders_last_month,
# lo.lifetime_orders
# FROM customers c
# LEFT JOIN customer_favorite cf ON c.CUSTOMER_ID = cf.CUSTOMER_ID
# LEFT JOIN monthly_orders mo ON c.CUSTOMER_ID = mo.CUSTOMER_ID
# LEFT JOIN lifetime_orders lo ON c.CUSTOMER_ID = lo.CUSTOMER_ID
# ORDER BY c.CUSTOMER_ID;


#Model
#Question a:How would you formulate a model to answer this request? What modeling tools and techniques are worth considering, and how would you choose between them?
#I considered linear regression, general linear regression, lasso and ridge regression, random forest and chose linear regression. The reasons for that is 1) it is easy to understand
#and write with model equations in 1.5 hour time 2) in the summary results it has transparent coefficients which are easy to interpret 3) In my current work I use linear
#regression as the first trial usually to understand model and data since it is the most classy way 

#Question b: What measures would you use to assess the model's performance?
#R square for accuracy, P value and F statistic for significance,estimate to see the change and correlation, and MSE to check the predicted and actual value difference
#I will also run residual plot to check MSE

#Below are the steps I do model
Customer4<-Customer4%>%rename(ORDERED_AT=ORDERED_AT.x)

Model<-Customer4%>%select(ORDERED_AT,CUSTOMER_ID,CUSTOMER_NAME,COUNT_LIFETIME_ORDERS,LIFETIME_SPEND,ORDER_TOTAL,ORDER_COST,SUPPLY_COST,FIRST_ORDERED_AT,LAST_ORDERED_AT,CUSTOMER_TYPE,PRODUCT_PRICE.x,PRODUCT_TYPE)

Model<-Model%>%rename(PRODUCT_PRICE=PRODUCT_PRICE.x)

#Step 1: Data Exploration
cor_data <- Model %>%
  select(LIFETIME_SPEND, COUNT_LIFETIME_ORDERS, PRODUCT_PRICE, SUPPLY_COST, ORDER_TOTAL,ORDER_COST) %>%
  cor()
ggcorrplot(cor_data)

#correlation_coefficient <- cor( Model$PRODUCT_PRICE,Model$Revenue)
#cat("Correlation coefficient:", correlation_coefficient, "\n")

#Found the Order_total and count_lifetime_orders have strong correlations, which might cause potential leakage with them together

#Step 2:Feature Engineering:Recency, Frequency, Interval, and Monetary value
#Step3: split training and test

customer_features_training <-Model%>%filter(ORDERED_AT<"2017-06-01")
customer_features_testing <-Model%>%filter(ORDERED_AT>="2017-06-01")

customer_features_training <- Model %>%
  group_by(CUSTOMER_ID) %>%
  summarise(
    recency = as.numeric(difftime(as.Date("2017-06-01",format = "%Y-%m-%d"), max(ORDERED_AT), units = "days")),
    Interval=difftime(LAST_ORDERED_AT,FIRST_ORDERED_AT,units="days"),
    frequency = n(),
    monetary = sum(ORDER_TOTAL),
    avg_product_price = mean(PRODUCT_PRICE, na.rm = TRUE)
  )


customer_features_training <-customer_features_training[!duplicated(customer_features_training$CUSTOMER_ID), ]

customer_features_testing <- Model %>%
  group_by(CUSTOMER_ID) %>%
  summarise(
    recency = as.numeric(difftime(as.Date("2017-06-01",format = "%Y-%m-%d"), max(ORDERED_AT), units = "days")),
    Interval=difftime(LAST_ORDERED_AT,FIRST_ORDERED_AT,units="days"),
    frequency = n(),
    monetary = sum(ORDER_TOTAL),
    avg_product_price = mean(PRODUCT_PRICE, na.rm = TRUE)
  )
customer_features_testing <-customer_features_testing[!duplicated(customer_features_testing$CUSTOMER_ID), ]


#linear
#option 1: linear
Model_Training <- lm(formula=monetary ~ recency + Interval + frequency + avg_product_price, data = customer_features_training)
summary(Model_Training)

#option 2: random forest
rf_model <- randomForest(monetary ~ recency + Interval + frequency + avg_product_price,  data = customer_features_training)
summary(rf_model)
importance(rf_model)
#prediction lm

# Evaluate model
predictions <- predict(Model_Training, customer_features_testing)
actual <- customer_features_testing$monetary

# Calculate RMSE
rmse <- sqrt(mean((predictions - actual)^2))
print(paste("RMSE:", rmse))

# Calculate MAPE
mape <- mean(abs(predictions - actual)/actual)
print(paste("mape:", mape))


#prediction rf
# Evaluate model
predictions <- predict(rf_model, customer_features_testing)
actual <- customer_features_testing$monetary

# Calculate RMSE
rmse <- sqrt(mean((predictions - actual)^2))
print(paste("RMSE:", rmse))

# Calculate MAPE
mape <- mean(abs(predictions - actual)/actual)
print(paste("mape:", mape))

# Model_Training.res = resid(Model_Training)
# plot(fitted(Model_Training), Model_Training.res) 
# qqnorm(Model_Training.res) 
# qqline(Model_Training.res) 

#QQ plot did show linear trend and the training and testing are pretty similar

#After test above I figured  Monetory has the most significant influence, so I am curious to see whether the model will be different if I 
#segment the customers based on this criteria

#Below is the navigation on distribution
graphics.off()
hist(customer_features_training$monetary)
hist(customer_features_training$frequency)

#Then I segment the customers 
customer_features_training<-customer_features_training%>% mutate(
  Segment= case_when(
    monetary<=2500 & frequency>=175  ~ "Low Monetary high frequency",
    monetary>2500 & frequency<175~ "High Monetary low frequency",
    TRUE ~ "Medium Monetary Medium frequency"
  )
)

customer_features_training<-customer_features_training%>% mutate(
  Cluster= case_when(
    monetary<=2500 & frequency>=175  ~ 1,
    monetary>2500 & frequency<175~ 2,
    TRUE ~ 0
  )
)

hist(customer_features_training$Cluster)

Model_cluster1<-customer_features_training%>%filter(Cluster=="1")
Model_cluster2<-customer_features_training%>%filter(Cluster=="2")
Model_cluster3<-customer_features_training%>%filter(Cluster=="0")
Cluster1 <- lm(formula=monetary  ~ recency + Interval + frequency  + avg_product_price, data = Model_cluster1)
summary(Cluster1)
Cluster2 <- lm(formula=monetary  ~ recency + Interval + frequency  + avg_product_price, data = Model_cluster2)
summary(Cluster2)
Cluster3 <- lm(formula=monetary  ~ recency + Interval + frequency  + avg_product_price, data = Model_cluster3)
summary(Cluster3)
#Cluster 2 and 3  which are the High/Medium Monetary with medium/low frequency seems to perform better
#this can be polished more but I want to show my thoughts on creative customer segmentation


#I think the assignment desp is missing question c and directly goes to question d, so I answer below straight from b to d

#Question d. What data would you explore in future model iterations in order to extend the model's capability?
#1: I think the customer type is interesting but since the "new" customers is very few in current dataset, I expect the new and existing customers
#to be more evenly distributed, so this is one point to add in the future
#2: When the email is launched, I also expect the marketing related data to be added in this analysis, like email clicks, sents, opens. And emails
#can be categorized by campaign themes to help me run attribution model to see which creative has the most engagement.
#3. If we build MMM model on top of this, I am also interested to see if I add time series data, microeconomic data, and base data like labor hours, inflation rate
#to see how this will make the marketing effect(email promotion) change.
#unsupervised learning and add revenue






