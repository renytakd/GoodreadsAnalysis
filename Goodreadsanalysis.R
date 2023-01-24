#install excel library to read excel file
library("readxl")

#import data and create variable for file
booklist <- read_excel("D:/Kemeng/Project/Bestbooksever.xlsx")
View(booklist)

#To see the summary of dataset
summary(booklist)

#Top 10 best Indonesia books pada goodreads berdasarkan rating goodreads
#Install necessary packages
install.packages("tidyr")
library(tidyr)
library(dplyr)
#Find the top 10 books based on rating using slicing func
topbooks <- booklist %>%  
  arrange(desc(rating)) %>%  
  slice(1:10)
View(topbooks)
#Create the visualization
library(ggplot2)
ggplot(topbooks, aes(x =rating, y = reorder(title, +rating),fill=rating))+
  geom_bar(stat="identity") + guides(fill="none") + labs(title="Data of top 10 Indonesian books based on Rating",
                                   subtitle = "Goodreads Indonesian Books 2020",
                                   x="Rating",y="Title")+
  theme(plot.title.position = "plot")

#top 10 indonesian books based on rating
rat_pub <- subset(booklist, numRatings > 2222)
View(rat_pub)
#Convert the table to dataframe
rat_publish <- as.data.frame(rat_pub)
#Slice the data to top 10 rank
ratingpublish <- rat_publish %>%  
  arrange(desc(numRatings)) %>%  
  slice(1:10)
View(ratingpublish)
#summarize the data to see the average of each column
summary(rat_publish)
#visualize the data
library(ggplot2)
ggplot(ratingpublish, aes(x =rating, y = reorder(title, +rating), fill=rating))+
  geom_bar(stat="identity")+ geom_text(aes(label=rating),position=position_dodge(width=0), vjust=0.5, hjust=2, colour="White") +
  labs(title="Data of top 10 Indonesian books based on Rating",
       subtitle = "Goodreads Indonesian Books 2020",
       x= "Rating", y="Title")+
  guides(fill="none") + theme(plot.title.position = "plot")

#Top 10 indonesian author with active book release
#Create table using group_by func to count the value in author column
au_top <- booklist %>% 
  group_by(author) %>% 
  count(author)
View(au_top)
#Convert the table to data frame format
author_top <- as.data.frame(au_top)
View(author_top)
#Find the top 5 authors based on the number of books published
topau <- author_top %>% 
  arrange(desc(n)) %>% 
  slice(1:5)
View(topau)
#summary of frequency data
summary(author_top)
#Create the visualization
ggplot(topau, aes(x =n, y = reorder(author, +n),fill=n))+
  geom_bar(stat="identity")+ guides(fill="none") +labs(title="Data of top 5 Indonesian author with active book release",
                                 subtitle = "Goodreads Indonesian Books 2020",
                                 x = "Frequency", y = "Author")+
  theme(plot.title.position = "plot")

#Year with the most release books (life time chart)
#Create table to count the value in Year column
yearpublish <- table(booklist['Year'])
View(yearpublish)
#Convert the table to data frame format
year_pub <- as.data.frame(yearpublish)
View(year_pub)
#Find the top 10 Years that released the most books
yearpub <- year_pub %>% 
  arrange(desc(Freq)) %>% 
  slice(1:10)
View(yearpub)
#Create the visualization; without linear line
ggplot(data=yearpub)+
  geom_point(mapping=aes(x=Year, y=Freq))
#Create the visualization; with linear line
yearpub %>% 
  ggplot(.,aes(x=as.numeric(as.character(Year)), y=Freq))+geom_point()+geom_smooth(se=FALSE)+
  labs(title="Data on the published book trend line over time", subtitle = "Goodreads Indonesian Books 2020",
       x="Year",y="Frequency")+
  theme(plot.title.position = "plot")
#Create the visualization with bar chart
ggplot(yearpub, aes(x =Freq, y = reorder(Year, +Freq),fill=Freq))+
  geom_bar(stat="identity") + geom_text(aes(label=Freq),position=position_dodge(width=0.15), vjust=0.5, hjust=2, colour="White") +
  guides(fill="none") + labs(title="Data of top 10 years with the most published books",
                                   subtitle = "Goodreads Indonesian Books 2020",
                                   x= "Frequency", y="Year")+
  theme(plot.title.position = "plot")

#trendline of the frequency of book publish in each year
#Create table to count the value in Year column
yeartimeline <- table(booklist['Year'])
View(yeartimeline)
#Convert the table to data frame format
year_tl <- as.data.frame(yeartimeline)
View(year_tl)
#Create the visualization; with linear line
year_tl %>% 
  ggplot(.,aes(x=as.numeric(as.character(Year)), y=Freq))+geom_point()+geom_smooth(se=FALSE)+
  labs(title="Data on the published book trend line over time", subtitle = "Goodreads Indonesian Books 2020",
       x="Year",y="Frequency")+
  theme(plot.title.position = "plot")

#Top 5 most popular genres
#Create table to count the value in genres column
topgenres <- table(booklist['genres'])
View(topgenres)
#Convert the table to data frame format
top_genres <- as.data.frame(topgenres)
View(top_genres)
#Find the top 5 most popular genres
topgen <- top_genres %>% 
  arrange(desc(Freq)) %>% 
  slice(1:5)
View(topgen)
#Create the visualization
ggplot(topgen, aes(x =Freq, y = reorder(genres, +Freq), fill=Freq))+
  geom_bar(stat="identity") + geom_text(aes(label=Freq),position=position_dodge(width=0.15), vjust=0.5, hjust=2, colour="White") +
  labs(title="Data of top 5 most popular genres that have been published",
                                   subtitle = "Goodreads Indonesian Books 2020",
                                   x= "Frequency", y="Genre")+
  guides(fill="none") + theme(plot.title.position = "plot")

#Most publish publisher
#Create table to count the value in publisher column
toppublisher <- table(booklist['publisher'])
View(toppublisher)
#Convert the table to data frame format
top_publisher <- as.data.frame(toppublisher)
View(top_publisher)
#Find the top 5 most published publisher
toppub <- top_publisher %>% 
  arrange(desc(Freq)) %>% 
  slice(1:5)
View(toppub)
#summary of frequency data
summary(top_publisher)
#Create the visualization
ggplot(toppub, aes(x =Freq, y = reorder(publisher, +Freq), fill = Freq))+
  geom_bar(stat="identity")+ geom_text(aes(label=Freq),position=position_dodge(width=0.15), vjust=0.5, hjust=2, colour="White") +
  labs(title="Data of top 5 most published publisher",
       subtitle = "Goodreads Indonesian Books 2020",
       x= "Frequency", y="Publisher")+
  guides(fill="none") + theme(plot.title.position = "plot")

#Do most awarded books had the best rating books?
#Create new data frame only for rating and numaward column
mab_rating <- data.frame(booklist$rating,booklist$numaward)
View(mab_rating)
#summary of number of award
summary(mab_rating)
#Create the visualization
ggplot(data=mab_rating)+geom_point(mapping=aes(x=booklist.numaward, y=booklist.rating),color='darkblue')+ 
  labs(title="Data of correlation between the number of awards given for\neach book and the rating",
       subtitle = "Goodreads Indonesian Books 2020",
       x= "Number of award", y="Rating") + guides(fill="none") + theme(plot.title.position = "plot")


# Do pages affected the rating of books?
#Create new data frame only for rating and pages column
library(data.table)
rob_pages <- data.table(booklist$rating,booklist$pages)
View(rob_pages)
#Delete the null data
clean <- na.omit(rob_pages)
View(clean)
#convert to dataframe
pages_rating <- as.data.frame(clean)
View(pages_rating)
#Create the visualization
ggplot(data=pages_rating)+geom_point(mapping=aes(x=V1, y=V2),color='darkblue')+ 
  labs(title="Data of correlation between the number of pages and the rating",
       subtitle = "Goodreads Indonesian Books 2020",
       x= "Rating", y="Number of Pages") + guides(fill="none") + theme(plot.title.position = "plot")

# Top 5 highest rated author based on the num of rating
#Find the top 5 most rated author based on number of rating
iya <- aggregate(numRatings~ author, booklist, sum)
View(iya)
#Convert previous table to dataframe
yangbener <- as.data.frame(iya)
#Arrange the publisher to top 5 most published
yb1 <- yangbener %>% 
  arrange(desc(numRatings)) %>% 
  slice(1:5)
View(yb1)
#summarize the data
summary(yangbener$numRatings)
#Visualize the data
ggplot(yb1, aes(x =numRatings, y = reorder(author, +numRatings), fill=numRatings))+
  geom_bar(stat="identity")+ geom_text(aes(label=numRatings),position=position_dodge(width=0), vjust=0.5, hjust=2, colour="White") +
  labs(title="Data of top 5 most rated author based on the number of rating",
       subtitle = "Goodreads Indonesian Books 2020",
       x= "Number of Ratings", y="Author")+
  guides(fill="none") + theme(plot.title.position = "plot")


#Top 5 Best publisher (min publish more than 2, and had >3782 rate)
library(dplyr)
#Count the frequency of publisher
best_publish <- table(booklist['publisher'])
View(best_publish)
#Summarize the data
summary(bpublisher)
#Create the subset for numRatings data that only more than 5296
bpublisher <- aggregate(numRatings~ publisher, booklist, sum)
View(bpublisher)
#bestp <- subset(bpublisher, numRatings > 5296)
#View(bestp)
#Grouping the table
table1 <- merge(best_publish, bpublisher, all=TRUE)
View(table1)
#Convert previous table to dataframe
best_publi <- as.data.frame(table1)
#Arrange the publisher to most published
bestpublisher_num <- best_publi %>% 
  arrange(desc(numRatings)) %>% 
  slice(1:5)
View(bestpublisher_num)
#Create the visualization
ggplot(bestpublisher_num, aes(x =numRatings, y = reorder(publisher, +numRatings), fill=numRatings))+
  geom_bar(stat="identity")+ geom_text(aes(label=numRatings),position=position_dodge(width=0), vjust=0.5, hjust=2, colour="White") +
  labs(title="Data of top 5 best published publisher",
       subtitle = "Goodreads Indonesian Books 2020",
       x= "Frequency and Number of Ratings", y="Publisher")+
  guides(fill="none") + theme(plot.title.position = "plot")


