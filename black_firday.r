###################################################################################### 
#Title    : Exploring a Hypothetical Store's Shopper Data Dataset in Anticipation of #
#           the Black Friday Holiday Season                                          #
#Author   : Delpagodage Ama Nayanahari Jayaweera                                     #
#Subtitle : Data Science: Capstone Project for Harvardx Professional Data Science    #
#           Certificate (Choose your own project: PH125.9x)                          #
#Date     : 2024-12-05                                                               #
######################################################################################

######################################################################################
#Method and Analysis in Exploratory Data Analysis (EDA)                              #
###################################################################################### 

######################################################################################
# 1. Import the libraries we will be utilizing in this kernel                        #
######################################################################################

if (!require(tidyverse)) {install.packages("tidyverse", repos = "http://cran.us.r-project.org")}
if (!require(scales)) {install.packages("scales", repos = "http://cran.us.r-project.org")}
if (!require(arules)) {install.packages("arules", repos = "http://cran.us.r-project.org")}
if (!require(gridExtra)) {install.packages("gridExtra", repos = "http://cran.us.r-project.org")}
if (!require(purrr)) {install.packages("purrr", repos = "http://cran.us.r-project.org")}
if (!require(readr)) {install.packages("readr", repos = "http://cran.us.r-project.org")}
if (!require(tidyr)) {install.packages("tidyr", repos = "http://cran.us.r-project.org")}
if (!require(dplyr)) {install.packages("dplyr", repos = "http://cran.us.r-project.org")}
if (!require(arulesViz)) {install.packages("arulesViz", repos = "http://cran.us.r-project.org")}

library(tidyverse)
library(scales)
library(arules)
library(gridExtra)

######################################################################################
#2. Load the dataset that be using for this Exploratory Data Analysis (EDA)          #
######################################################################################
dataset = read.csv("BlackFriday.csv")

######################################################################################
#3. Overview of the entire dataset.                                                  #
######################################################################################
summary(dataset)
head(dataset)

#4. Group the data by User_ID to remove duplicates.
dataset_gender = dataset %>%
  select(User_ID, Gender) %>%
  group_by(User_ID) %>%
  distinct()  

head(dataset_gender)
summary(dataset_gender$Gender)

#5. Plot the distribution of gender across.
options(scipen=10000)   # To remove scientific numbering

genderDist  = ggplot(data = dataset_gender) +
  geom_bar(mapping = aes(x = Gender, y = ..count.., fill = Gender)) +
  labs(title = 'Gender of Customers') + 
  scale_fill_brewer(palette = 'PuBuGn')
print(genderDist)

#6. Compute the average spending amount as it relates to Gender
total_purchase_user = dataset %>%
  select(User_ID, Gender, Purchase) %>%
  group_by(User_ID) %>%
  arrange(User_ID) %>%
  summarise(Total_Purchase = sum(Purchase))

user_gender = dataset %>%
  select(User_ID, Gender) %>%
  group_by(User_ID) %>%
  arrange(User_ID) %>%
  distinct()

head(user_gender)
head(total_purchase_user)

user_purchase_gender = full_join(total_purchase_user, user_gender, by = "User_ID")
head(user_purchase_gender)

#7.  The average transaction for Females 
average_spending_gender = user_purchase_gender %>%
  group_by(Gender) %>%
  summarize(Purchase = sum(as.numeric(Total_Purchase)), 
            Count = n(), 
            Average = Purchase/Count)
head(average_spending_gender)

#8. Visualize results for gender
genderAverage  = ggplot(data = average_spending_gender) +
  geom_bar(mapping = aes(x = Gender, y = Average, fill = Gender), stat = 'identity') +
  labs(title = 'Average Spending by Gender') +
  scale_fill_brewer(palette = 'PuBuGn')
print(genderAverage)

#9. Top Sellers
top_sellers = dataset %>%
  count(Product_ID, sort = TRUE)

top_5 = head(top_sellers, 5)

top_5

#10. Examine the best selling product
best_seller = dataset[dataset$Product_ID == 'P00265242', ]

######################################################################################
#Results and Discussion                                                              #
###################################################################################### 
head(best_seller)

#11. Analyze best seller to see if any relationship to Gender
genderDist_bs  = ggplot(data = best_seller) +
  geom_bar(mapping = aes(x = Gender, y = ..count.., fill = Gender)) +
  labs(title = 'Gender of Customers (Best Seller)') +
  scale_fill_brewer(palette = 'PuBuGn')
print(genderDist_bs)

#12. Distribution between genders to our overall dataset gender
genderDist_bs_prop = ggplot(data = best_seller) + 
  geom_bar(fill = 'lightblue', mapping = aes(x = Gender,
                                             y = ..prop.., group = 1, fill = Gender)) +
  labs(title = 'Gender of Customers (Best Seller - Proportion)') +
  theme(plot.title = element_text(size=9.5))

genderDist_prop = ggplot(data = dataset_gender) + 
  geom_bar(fill = "lightblue4", mapping = aes(x = Gender, 
                                              y = ..prop.., group = 1)) +
  labs(title = 'Gender of Customers (Total Proportion)') +
  theme(plot.title = element_text(size=9.5)) 

grid.arrange(genderDist_prop, genderDist_bs_prop, ncol=2)

#13. Examining Age
customers_age = dataset %>%
  select(User_ID, Age) %>%
  distinct() %>%
  count(Age)
customers_age

#14. Age category of customers at store
customers_age_vis = ggplot(data = customers_age) + 
  geom_bar(color = 'black', stat = 'identity', mapping = aes(x = Age, y = n, fill = Age)) +
  labs(title = 'Age of Customers') +
  theme(axis.text.x = element_text(size = 10)) +
  scale_fill_brewer(palette = 'Blues') +
  theme(legend.position="none")
print(customers_age_vis)

#15. Age category that purchased the best selling product
ageDist_bs  = ggplot(data = best_seller) +
  geom_bar(color = 'black', mapping = aes(x = Age, y = ..count.., fill = Age)) +
  labs(title = 'Age of Customers (Best Seller)') +
  theme(axis.text.x = element_text(size = 10)) +
  scale_fill_brewer(palette = 'GnBu') + 
  theme(legend.position="none")
print(ageDist_bs)

#16. Compare this observation to the overall dataset
grid.arrange(customers_age_vis, ageDist_bs, ncol=2)

#17. City
customers_location =  dataset %>%
  select(User_ID, City_Category) %>%
  distinct()
head(customers_location)

#18 Pairing each distinct User_ID with its associated City_Category
customers_location_vis = ggplot(data = customers_location) +
  geom_bar(color = 'white', mapping = aes(x = City_Category, y = ..count.., fill = City_Category)) +
  labs(title = 'Location of Customers') + 
  scale_fill_brewer(palette = "Dark2") + 
  theme(legend.position="none")
print(customers_location_vis)

#19 Analysis aims to shed light on which city's customers have made the most substantial expenditures at store
purchases_city = dataset %>%
  group_by(City_Category) %>%
  summarise(Purchases = sum(Purchase))

purchases_city_1000s = purchases_city %>%
  mutate(purchasesThousands = purchases_city$Purchases / 1000)

purchases_city_1000s

#20 Shopping behaviors across different cities
purchaseCity_vis = ggplot(data = purchases_city_1000s, aes(x = City_Category, y = purchasesThousands, fill = City_Category)) +
  geom_bar(color = 'white', stat = 'identity') +
  labs(title = 'Total Customer Purchase Amount (by City)', y = '($000s)', x = 'City Category') +
  scale_fill_brewer(palette = "Dark2") + 
  theme(legend.position="none", plot.title = element_text(size = 9))
print(purchaseCity_vis)

#21 calculating the total number of purchases corresponding to each User_ID
grid.arrange(customers_location_vis, purchaseCity_vis, ncol=2)

customers = dataset %>%
  group_by(User_ID) %>%
  count(User_ID)
head(customers)

#22 Data, shedding light on the shopping behaviors across different cities
customers_City =  dataset %>%
  select(User_ID, City_Category) %>%
  group_by(User_ID) %>%
  distinct() %>%
  ungroup() %>%
  left_join(customers, customers_City, by = 'User_ID') 
head(customers_City)

city_purchases_count = customers_City %>%
  select(City_Category, n) %>%
  group_by(City_Category) %>%
  summarise(CountOfPurchases = sum(n))
city_purchases_count

city_count_purchases_vis = ggplot(data = city_purchases_count, aes(x = City_Category, y = CountOfPurchases, fill = City_Category)) +
  geom_bar(color = 'white', stat = 'identity') +
  labs(title = 'Total Purchase Count (by City)', y = 'Count', x = 'City Category') +
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position="none", plot.title = element_text(size = 9))
print(city_count_purchases_vis)

grid.arrange(purchaseCity_vis, city_count_purchases_vis, ncol = 2)

#23 Results and Discussion
head(best_seller)

#24 Examination will potentially unveil nuanced trends or preferences specific to different city categories
best_seller_city = best_seller %>%
  select(User_ID, City_Category) %>%
  distinct() %>%
  count(City_Category)
best_seller_city

#25 Compared to residents of City A and City B
best_seller_city_vis = ggplot(data = best_seller_city, aes(x = City_Category, y = n, fill = City_Category)) +
  geom_bar(color = 'white', stat = 'identity') +
  labs(title = 'Best Seller Purchase Count (by City)', y = 'Count', x = 'City Category') +
  scale_fill_brewer(palette = "Blues") +
  theme(legend.position="none", plot.title = element_text(size = 9))
grid.arrange(city_count_purchases_vis,best_seller_city_vis, ncol = 2)

#26 Stay in Current City
customers_stay = dataset %>%
  select(User_ID, City_Category, Stay_In_Current_City_Years) %>%
  group_by(User_ID) %>%
  distinct()
head(customers_stay)

#27 Data to reveal trends and patterns that might emerge from the distribution of customers' residence duration
residence = customers_stay %>%
  group_by(City_Category) %>%
  tally()
head(residence)

#28 Residency patterns across different city categories
stay_cities = customers_stay %>%
  group_by(City_Category, Stay_In_Current_City_Years) %>%
  tally() %>%
  mutate(Percentage = (n/sum(n))*100)
head(stay_cities)

#29 Stacked bar chart that segregates customers' length of residency based on different city categories
ggplot(data = stay_cities, aes(x = City_Category, y = n, fill = Stay_In_Current_City_Years)) + 
  geom_bar(stat = "identity", color = 'white') + 
  scale_fill_brewer(palette = 2) + 
  labs(title = "City Category + Stay in Current City", 
       y = "Total Count (Years)", 
       x = "City", 
       fill = "Stay Years")

#30 Purchase
customers_total_purchase_amount = dataset %>%
  group_by(User_ID) %>%
  summarise(Purchase_Amount = sum(Purchase))

head(customers_total_purchase_amount)

#31 Total purchase amount attributed to each User_ID
customers_total_purchase_amount = arrange(customers_total_purchase_amount, desc((Purchase_Amount)))

head(customers_total_purchase_amount)

#32 grouped purchases and grouped by User ID, sort and find top spenders
summary(customers_total_purchase_amount)

#33 the overall shape and skewness of the data, revealing where the highest concentration of similar purchase amounts lies within the customer base
library(ggplot2)
library(dplyr)

# Assuming customers_total_purchase_amount is your data frame
# and it contains a column named Purchase_Amount

# Filter out non-finite values
cleaned_data <- customers_total_purchase_amount %>%
  filter(is.finite(Purchase_Amount))

mean_purchase <- mean(cleaned_data$Purchase_Amount, na.rm = TRUE)
median_purchase <- median(cleaned_data$Purchase_Amount, na.rm = TRUE)

ggplot(cleaned_data, aes(Purchase_Amount)) +
  geom_density(adjust = 1) +
  geom_vline(aes(xintercept = median_purchase), color = "blue", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = mean_purchase), color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = mean_purchase, label = round(mean_purchase), y = 1.2e-06, color = "red",
           size = 4, vjust = 3, hjust = -0.1) +
  annotate("text", x = median_purchase, label = round(median_purchase), y = 1.2e-06, color = "blue",
           size = 4, vjust = 0, hjust = -0.1) +
  scale_x_continuous(name = "Purchase Amount", limits = c(0, 7500000), breaks = seq(0, 7500000, by = 1000000), expand = c(0, 0)) +
  scale_y_continuous(name = "Density", limits = c(0, 0.00000125), labels = scales::scientific, expand = c(0, 0))

#34 Marital Status
dataset_maritalStatus = dataset %>%
  select(User_ID, Marital_Status) %>%
  group_by(User_ID) %>%
  distinct()

head(dataset_maritalStatus)

#35 Change Marital_Status from a numeric variable to a categorical type
dataset_maritalStatus$Marital_Status = as.character(dataset_maritalStatus$Marital_Status)
typeof(dataset_maritalStatus$Marital_Status)


marital_vis = ggplot(data = dataset_maritalStatus) +
  geom_bar(mapping = aes(x = Marital_Status, y = ..count.., fill = Marital_Status)) +
  labs(title = 'Marital Status') +
  scale_fill_brewer(palette = 'Pastel2')
print(marital_vis)


#36 Investigation can shed light on potential regional patterns in shopping behaviors
dataset_maritalStatus = dataset_maritalStatus %>%
  full_join(customers_stay, by = 'User_ID') 
head(dataset_maritalStatus)

maritalStatus_cities = dataset_maritalStatus %>%
  group_by(City_Category, Marital_Status) %>%
  tally()
head(maritalStatus_cities)

#37 The distribution of single shoppers across different city categories
ggplot(data = maritalStatus_cities, aes(x = City_Category, y = n, fill = Marital_Status)) + 
  geom_bar(stat = "identity", color = 'black') + 
  scale_fill_brewer(palette = 2) + 
  labs(title = "City + Marital Status", 
       y = "Total Count (Shoppers)", 
       x = "City", 
       fill = "Marital Status")

#38 Customers' current city residence across different city segments
Users_Age = dataset %>%
  select(User_ID, Age) %>%
  distinct()
head(Users_Age)

#40 Visualizing this data will help understand how customer residence durations differ across various city segments, potentially unveiling trends and insights about customer behaviors and preferences
dataset_maritalStatus = dataset_maritalStatus %>%
  full_join(Users_Age, by = 'User_ID')
head(dataset_maritalStatus)

City_A = dataset_maritalStatus %>%
  filter(City_Category == 'A')
City_B = dataset_maritalStatus %>%
  filter(City_Category == 'B')
City_C = dataset_maritalStatus %>%
  filter(City_Category == 'C')
head(City_A)
head(City_B)
head(City_C)

City_A_stay_vis = ggplot(data = City_A, aes(x = Age, y = ..count.., fill = Age)) + 
  geom_bar(stat = 'count') +
  scale_fill_brewer(palette = 8) +
  theme(legend.position="none", axis.text = element_text(size = 6)) +
  labs(title = 'City A', y = 'Count', x = 'Age', fill = 'Age')
City_B_stay_vis = ggplot(data = City_B, aes(x = Age, y = ..count.., fill = Age)) +
  geom_bar(stat = 'count') +
  scale_fill_brewer(palette = 9) +
  theme(legend.position="none", axis.text = element_text(size = 6)) +
  labs(title = 'City B', y = 'Count', x = 'Age', fill = 'Age')
City_C_stay_vis = ggplot(data = City_C, aes(x = Age, y = ..count.., fill = Age)) +
  geom_bar(stat = 'count') +
  scale_fill_brewer(palette = 11) +
  theme(legend.position="none", axis.text = element_text(size = 6)) +
  labs(title = 'City C', y = 'Count', x = 'Age', fill = 'Age')

grid.arrange(City_A_stay_vis, City_B_stay_vis, City_C_stay_vis, ncol = 3)

#41 Top Shoppers
top_shoppers = dataset %>%
  count(User_ID, sort = TRUE)

head(top_shoppers)

#42 Total purchase amounts can provide a comprehensive view of their shopping behavior and expenditure patterns
top_shoppers =  top_shoppers %>%
  select(User_ID, n) %>%
  left_join(customers_total_purchase_amount, Purchase_Amount, by = 'User_ID')

head(top_shoppers)

#43 the behaviors of high-frequency shoppers and potentially inform strategies for customer engagement and retention
top_shoppers = mutate(top_shoppers,
                      Average_Purchase_Amount = Purchase_Amount/n)

head(top_shoppers)

#44 Comprehensive view of the shopping behavior of the top shoppers, highlighting both the User_ID with the highest number of total purchases and the User_ID with the highest total Purchase_Amount
top_shoppers_averagePurchase = top_shoppers %>%
  arrange(desc(Average_Purchase_Amount))

head(top_shoppers_averagePurchase)

#45 Occupation
customers_Occupation =  dataset %>%
  select(User_ID, Occupation) %>%
  group_by(User_ID) %>%
  distinct() %>%
  left_join(customers_total_purchase_amount, Occupation, by = 'User_ID')

head(customers_Occupation)

#46 The total Purchase_Amount for each Occupation identifier
totalPurchases_Occupation = customers_Occupation %>%
  group_by(Occupation) %>%
  summarise(Purchase_Amount = sum(Purchase_Amount)) %>%
  arrange(desc(Purchase_Amount))

totalPurchases_Occupation$Occupation = as.character(totalPurchases_Occupation$Occupation)
typeof(totalPurchases_Occupation$Occupation)

head(totalPurchases_Occupation)

#47 total Purchase_Amount
occupation = ggplot(data = totalPurchases_Occupation) +
  geom_bar(mapping = aes(x = reorder(Occupation, -Purchase_Amount), y = Purchase_Amount, fill = Occupation), stat = 'identity') +
  scale_x_discrete(name="Occupation", breaks = seq(0,20, by = 1), expand = c(0,0)) +
  scale_y_continuous(name="Purchase Amount ($)", expand = c(0,0), limits = c(0, 750000000)) +
  labs(title = 'Total Purchase Amount by Occupation') + 
  theme(legend.position="none")
print(occupation)

######################################################################################
#Modeling Results and Model Performance with Apriori (Association Rule Learning)     #                                                          #
###################################################################################### 
# Import the libraries we will be utilizing in this kernel

#48 Modeling Results and Model Performance
if (!require(arules)) {install.packages("arules", repos = "http://cran.us.r-project.org")}
if (!require(arulesViz)) {install.packages("arulesViz", repos = "http://cran.us.r-project.org")}
if (!require(tidyverse)) {install.packages("tidyverse", repos = "http://cran.us.r-project.org")}

library(arules)
library(arulesViz)
library(tidyverse)

######################################################################################
# Data Preprocessing                                                                 #
# Getting the dataset into the correct format                                        #
######################################################################################
#49 Apriori (Association Rule Learning)
customers_products = dataset %>%
  select(User_ID, Product_ID) %>%   # Selecting the columns we will need
  group_by(User_ID) %>%             # Grouping by "User_ID"          
  arrange(User_ID) %>%              # Arranging by "User_ID" 
  mutate(id = row_number()) %>%     # Defining a key column for each "Product_ID" and its corresponding "User_ID" (Must do this for spread() to work properly)
  spread(User_ID, Product_ID) %>%   # Converting our dataset from tall to wide format, and grouping "Product_IDs" to their corresponding "User_ID"
  t()                               # Transposing the dataset from columns of "User_ID" to rows of "User_ID"

# Now we can remove the Id row we created earlier for spread() to work correctly.
customers_products = customers_products[-1,]
write.csv(customers_products, file = 'customers_products.csv')
customersProducts = read.transactions('customers_products.csv', sep = ',', rm.duplicates = TRUE) # remove duplicates with rm.duplicates

summary(customersProducts)

summary(customersProducts)

itemFrequencyPlot(customersProducts, topN = 25)    # topN is limiting to the top 50 products

#50 The Apriori algorithm using these parameters, paving the way to uncovering significant associations among purchased items
rules = apriori(data = customersProducts,
                parameter = list(support = 0.008, confidence = 0.80, maxtime = 0)) # maxtime = 0 will allow our algorithim to run until completion with no time limit

inspect(sort(rules, by = 'lift'))
plot(rules, method = 'graph')

#51 The association rules created by apriori algorithm
rules = apriori(data = customersProducts,
                parameter = list(support = 0.008, confidence = 0.75, maxtime = 0))

inspect(head(sort(rules, by = 'lift'))) # limiting to the top 6 rules

plot(rules, method = 'graph', max = 25)
plot(rules, method = 'grouped', max = 25)