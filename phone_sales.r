########## setting the research goal ###########
# our task is to analyse the phone sales dataset

######## data retrieval #########
#loading data
phsale<-read.csv(file.choose(),header=T)
View(phsale)
phsale

###libraies
library(dplyr)
library(ggplot2)

######## data preparation ############

#### Data cleaning ####
##str
str(phsale)

##columns
colnames(phsale)

###let's look descriptive statistics of the data
summary(phsale)

####checking null values
colSums(is.na(phsale))

###storage
class(phsale$Storage)
sum(is.na(phsale$Storage))
phsale$Storage[phsale$Storage==""]<-"8 GB"
phsale$Storage[is.na(phsale$Storage)]<-"8 GB"

sum(is.na(phsale$Storage))

### selling.price
class(phsale$Selling.Price)
sum(is.na(phsale$Selling.Price))
m<-round(mean(phsale$Selling.Price,na.rm = TRUE),0)
phsale$Selling.Price[is.na(phsale$Selling.Price)]<-m

sum(is.na(phsale$Selling.Price))

####Original price
class(phsale$Original.Price)
sum(is.na(phsale$Original.Price))
phsale%>%select(Brand,Selling.Price,Original.Price)
phsale$Original.Price[is.na(phsale$Original.Price)]<-phsale$Selling.Price[is.na(phsale$Original.Price)]
sum(is.na(phsale$Original.Price))
phsale%>%select(Brand,Selling.Price,Original.Price)
View(phsale)

######### data Transformation ############
###memory
class(phsale$Memory)
table(phsale$Memory)
####changing into factor
phsale$Memory<-as.factor(phsale$Memory)
class(phsale$Memory)

##storage
class(phsale$Storage)
table(phsale$Storage)
phsale$Storage<-as.factor(phsale$Storage)
class(phsale$Storage)

#####renaming column names
phsale<-rename(phsale,Selling_Price=Selling.Price)
phsale<-rename(phsale,Original_Price=Original.Price)
phsale
View(phsale)


########## Exploratory data analysis ############
phsale

##oppo brand with 128 gb storage
oppo_128<-phsale%>%
  select(Brand,Storage,Selling_Price)%>%
  filter(Brand=="OPPO" & Storage=="128 GB")

oppo_128

##apple phones under 30000
apple_30k<-phsale%>%
  select(Brand,Model,Selling_Price)%>%
  filter(Brand=="Apple" & Selling_Price<=30000)
apple_30k

### adding new column
phsale<-phsale%>%
  mutate(price_catg=ifelse(Selling_Price<=30000,"low cost",
      ifelse(Selling_Price>30000 & Selling_Price<=70000,
             "medium cost","high cost")))
phsale%>%select(Brand,Selling_Price,price_catg)

##lenovo phones with 4.0 above rating and 
## 64 gb storage
len_64gb<-phsale%>%
  select(Brand,Rating,Storage,Selling_Price)%>%
  filter(Brand=="Lenovo" & Rating>4.0 & Storage=="64 GB")
len_64gb

####relationship between selling price and
str(phsale)
ggplot(phsale,aes(x=Original_Price,y=Selling_Price))+
  geom_point(col="blue",pch=6)
###it is positively correlated if selling price
###increases original price also increases

### most rated mobiles
tot_r<-phsale%>%group_by(Brand)%>%
  summarise(tol_ratings=sum(Rating>=4.0))
tot_r

##barplot
barplot(tot_r$tol_ratings,
        names.arg = tot_r$Brand)
##adding x and y lab
barplot(tot_r$tol_ratings,
        names.arg = tot_r$Brand,
        xlab="Brand_name",ylab="Ratings_count")
##adding title and color
barplot(tot_r$tol_ratings,
        names.arg = tot_r$Brand,
        xlab="Brand_name",ylab="Ratings_count",
        main="Barplot for highest ratings",
        col=rainbow(length(tot_r$Brand)))

##from the above graph most rated phones are 
##samsung and then apple

###barplot for rating division
ggplot(phsale,aes(x=price_catg))+geom_bar()
##adding color
ggplot(phsale,aes(x=price_catg,fill=price_catg))+
  geom_bar()

###visualizing statistical information of selling
###price column
ggplot(data=phsale,aes(x=Selling_Price))+
  geom_boxplot(fill="palegreen")
##from the above graph the maximum rate of 
##mobile phone is nearly 170000 and minimum rate
##is 1000 and the average rate of mobile phones
##is in between 14000 to 15000.
