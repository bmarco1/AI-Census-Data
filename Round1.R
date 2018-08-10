 setwd("C:/Users/bmarco1/Desktop/R/App Data Sample")
 library(ggplot2)
 library(readr)
 AppleStore <- read_csv("AppleStore.csv")
 View(AppleStore)


rating <- group_by(AppleStore, user_rating, prime_genre, rating_count_tot)

summary(rating)

options(scipen = 1000)
ggplot(rating, aes(x = user_rating, fill = ..count..)) + 
  geom_histogram(binwidth = 1) + 
  ggtitle("User Ratings")+
  ylab("Count")+
  xlab("Rating")+
  theme(plot.title = element_text(hjust = 0.5))


ggplot(data = rating)+
  (mapping =aes(rating_count_tot, user_rating))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~ prime_genre, nrow = 5)

genre<- group_by(AppleStore, prime_genre, user_rating)
genre1<- summarise(genre, count=n())


ggplot(data = genre1) + 
  geom_bar(mapping = aes(x = prime_genre, fill = count))

ggplot(data = genre1) + 
  geom_point(mapping = aes(x = count, y = user_rating, color = prime_genre))

ggplot(rating, aes(x= price, y=user_rating)) + 
  geom_point(shape=1) +  
  geom_smooth(method=lm , color="red", se=FALSE)


ggplot(rating, aes(x=sup_devices.num, y=user_rating)) + 
  geom_point(shape=1) +  
  geom_smooth(method=lm , color="red", se=FALSE)

ggplot(rating, aes(x=lang.num, y=user_rating)) + 
  geom_point(shape=1) +  
  geom_smooth(method=lm , color="red", se=FALSE)

ggplot(rating, aes(x=size_bytes, y=user_rating)) + 
  geom_point(shape=1) +  
  geom_smooth(method=lm , color="red", se=FALSE)




