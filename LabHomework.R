getwd()
setwd("/Users/xueweiquan/Downloads")
library("tidyverse")
library("reshape")



# Activity 2_____________________________________________________________
data <- read.csv("lab2.csv")
tidydata <- data %>% 
  gather("base_sport", "base_pain", "base_qol", "first_sport", "first_pain", "first_qol", 
         "second_sport", 'second_qol', 'second_pain', key = "time_category", value = "score") %>%
  separate(time_category, into = c("time", "category")) %>%
  mutate(time = fct_recode(factor(time), "baseline" = "base", "one year" = "first", "two years" = "second"))

tidydata %>%
  group_by(time, category) %>%
  summarise(mean = mean(score), sd = sd(score)) %>%
  ggplot(aes(x = time, y = mean, group = category, color = category))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(x = time, ymin = mean - sd, ymax = mean + sd, group = category, color = category, width = 0.05))+
  labs(y = "Sample mean and standard deviation", x = "Time of report")+
  ylim(0,100)
  


  
# Assignment 2____________________________________________________________

data1 <- read.csv("coverage.csv", header = TRUE, skip = 2, nrows = 52)
data2 <- read.csv("expenditures.csv", header = TRUE, skip = 2, nrows = 52)
data3 <- cbind(data1, data2[,24:25])

data4 <- data3 %>%
  rename(c(X2013__Total.Health.Spending = "X2013__TotalHealthSpending",	
           X2014__Total.Health.Spending = "X2014__TotalHealthSpending" )) %>%
  gather("X2013__Employer", "X2013__Non.Group",	"X2013__Medicaid",	"X2013__Medicare",	"X2013__Other.Public",	"X2013__Uninsured",	"X2013__Total",	
         "X2014__Employer",	"X2014__Non.Group",	"X2014__Medicaid",	"X2014__Medicare",	"X2014__Other.Public",	"X2014__Uninsured",	"X2014__Total",	
         "X2015__Employer",	"X2015__Non.Group",	"X2015__Medicaid",	"X2015__Medicare",	"X2015__Other.Public",	"X2015__Uninsured",	"X2015__Total",
         "X2016__Employer",	"X2016__Non.Group",	"X2016__Medicaid",	"X2016__Medicare",	"X2016__Other.Public",	"X2016__Uninsured",	"X2016__Total",
         "X2013__TotalHealthSpending",	"X2014__TotalHealthSpending",
         key = "year_category", value = "number", na.rm = TRUE) 

data5 <- separate(data4, year_category, into = c("year", "category"))

data5$year <- gsub("X", "", data5$year)
data5$category <- gsub("Non", "NoneGroup", data5$category)
data5$category <- gsub("Other", "OtherPublic", data5$category)
  
