#install necessary packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("tree")
install.packages("tidyverse")
install.packages("gbm")

library(dplyr)
library(ggplot2)
library(lubridate)
library(gbm)
library(tidyverse)
library(tree)
library(scales)

#load data
sales_data = read.csv("sales_train.csv")
item_data = read.csv("items.csv")
test_data = read.csv("test.csv")

#summary of the datasets - min, max, mean
summary(sales_data)
summary(item_data)
summary(test_data)

#number of rows and columns in each dataset
dim(sales_data)
dim(item_data)
dim(test_data)

#check for outliers in sales train data
boxplot(sales_data$item_price)
boxplot(sales_data$item_cnt_day)

#item_cnt_day remove the data instances that sold more than 800 in one day.
#item_price remove the item with a price greater than 80,000.

sales_data = subset(sales_data, sales_data$item_price < 80000 & sales_data$item_cnt_day < 800
                    & sales_data$item_price > 0 & sales_data$item_cnt_day > 0)

#merge item_data to sales data 
item_sales_data = merge(sales_data, item_data[,c("item_id", "item_category_id")], by = "item_id", all.x = T)
view(item_sales_data)
head(item_sales_data)
#check for NA
which(is.na(item_sales_data))

#check for negative values 
item_sales_data %>%
  gather(var, val, -c(1)) %>%
  group_by(var) %>%
  summarise(res = any(val < 0))
#we can see no negative values 

#add year and month column
item_sales_data$date <- dmy(item_sales_data$date)
item_sales_data$year <- year(item_sales_data$date)
item_sales_data$month <- month(item_sales_data$date)
item_sales_data$year <- as.factor(item_sales_data$year)
item_sales_data$month <- as.factor(item_sales_data$month)

#add item_cnt_month and mean_price column
new_sales_data <- item_sales_data %>% 
  group_by(date_block_num,shop_id,item_id,year,month,item_category_id) %>% 
  summarise(item_cnt_month = sum(item_cnt_day,na.rm = TRUE),
            mean_price = mean(item_price,na.rm = TRUE)) %>% ungroup()

#total monthly sales 
new_sales_data <- new_sales_data %>% mutate(monthly_sales = item_cnt_month * mean_price)

#EDA 
#ITEMS SALES BY SHOP
sales_by_shop <- item_sales_data %>%
        mutate(shop_id = as.factor(shop_id)) %>%
        select(shop_id, item_cnt_day) %>%
        group_by(shop_id) %>%
        summarise(item_cnt_day =  sum(item_cnt_day, na.rm = TRUE))

ggplot(data =  sales_by_shop, 
       mapping = aes(x = reorder(shop_id, item_cnt_day), 
                     y = item_cnt_day, 
                     fill = shop_id)) +
        geom_histogram(stat = "identity", color = "red") +
        scale_y_continuous(breaks= seq(0, 300000, by=20000), labels = comma) +
        xlab("Shop ID") + ylab("Item Sales Count")+
        ggtitle(label = "Item Sales by Shop")

## item sales by category 
item_sales_by_category <- item_sales_data %>%
        mutate(item_category_id = as.factor(item_category_id)) %>%
        select(item_category_id, item_cnt_day) %>%
        group_by(item_category_id) %>%
        summarise(item_cnt_day =  sum(item_cnt_day, na.rm = TRUE))

ggplot(data =  item_sales_by_category, 
       mapping = aes(x = reorder(item_category_id,item_cnt_day), 
                     y = item_cnt_day,
                     fill = item_category_id)) +
        geom_histogram(stat = "identity", color = "orange") +
        scale_y_continuous(breaks= seq(0, 650000, by=50000), labels = comma) +
        xlab("Item Category") + ylab("Item Sales Count") +
        ggtitle("Item sales by Category") 

#THE MOST SOLD ITEM IN EACH SHOP
most_sold_items_in_shop  <-  item_sales_data %>%
        mutate(item_id = as.factor(item_id)) %>%
        group_by(shop_id, item_id) %>%
        summarise(most_sold_item = sum(item_cnt_day, na.rm=TRUE)) %>%
        filter(most_sold_item == max(most_sold_item)) %>%
        arrange(desc(most_sold_item))

ggplot(data = most_sold_items_in_shop,
       mapping = aes(x = reorder(shop_id, most_sold_item),
                     y = most_sold_item,
                     fill = item_id)) +
        geom_histogram(stat = "identity", color = "orange") +
        scale_y_continuous(breaks= seq(0, 20000, by=4000), labels = comma) +
        xlab("Shop ID") + ylab("Item Sales Count") +
        ggtitle("The Most Sold Items in each Shop ") 

####TOTAL SALES BY ITEM CATEGORY
total_sales_by_category <- new_sales_data %>%
  mutate(item_category_id = as.factor(item_category_id)) %>%
  group_by(item_category_id) %>%
  summarise(sales_category = sum(monthly_sales, na.rm = TRUE)) %>%
  arrange(desc(sales_category))

ggplot(data = na.omit(total_sales_by_category), 
       aes(x = reorder(item_category_id, sales_category),
           y = sales_category,
           fill = item_category_id)) +
  geom_histogram(stat = "identity", color = "orange") +
  scale_y_continuous(breaks= seq(0, 400000000, by=60000000), labels=comma) +
  xlab("Category ID") + ylab("Sales Count")+
  ggtitle("Total Sales by Item Category")  


#for model
new_sales_data = subset(new_sales_data,select = c("shop_id","item_id",
                                                  "item_category_id","year","month","item_cnt_month"))

#train test split 
set.seed(10)
dt = sort(sample(nrow(new_sales_data), nrow(new_sales_data)*.7))
dt_train = new_sales_data[dt,]
dt_test = new_sales_data[-dt,]

#check with this split if better rmse?
#train = new_sales_data[date_block_num<33]
#test = new_sales_data[date_block_num==33]
#subset of test 

#fit models 
linear_model = lm(formula = item_cnt_month ~ ., data = dt_train) 
result.lm = predict(linear_model,dt_test)
rmse.lm = sqrt(mean((dt_test$item_cnt_month - result.lm)^2))
rmse.lm

gbm_model <- gbm(
  formula = item_cnt_month ~ .,
  distribution = "gaussian",
  data = dt_train,
  n.trees = 100,
  interaction.depth = 3,
  shrinkage = 0.2,
  n.minobsinnode = 5,
  bag.fraction = .65, 
  train.fraction = 1,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

result.gbm = predict(gbm_model,dt_test)
rmse.gbm = sqrt(mean((dt_test$item_cnt_month - result.gbm)^2))
rmse.gbm

sales_regtree = tree(item_cnt_month~., data= dt_train)
result.tree = predict(sales_regtree,dt_test)
rsme.tree = sqrt(mean((dt_test$item_cnt_month - result.tree)^2))
rsme.tree

#modify test data to include category, month,year
new_test_data <- merge(test_data, item_data, by = c("item_id") , all.x = TRUE)
new_test_data = subset(new_test_data, select = -item_name)
new_test_data <- new_test_data %>% mutate(month = 11, year=2015)

#predict in test data
result = predict(gbm_model,newdata = new_test_data[,-2])

# Submission file 
submission = data.frame(ID = new_test_data$ID,
                        item_cnt_month =  result)

write.csv(submission, "submission.csv", row.names = F)


