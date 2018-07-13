library(twitteR)
library(ggplot2)
library(lubridate)
library(scales)

setup_twitter_oauth("iklXL3AVVZyIKTDRfLEPUZWZO", "XykyteGlQxsKxAX4MeDzyE8XiZ8OsMpoDxQ60krBoS2sFrYa1w", "189974290-GWe34867VXvvY8OapY8tMG4qkKK0sy0fsB57vyx8", "b44vnOA6Ghcc8UewDhPPwNUNCgVbSbqPsjWZQiLROx8F9")

tweets_ru <-userTimeline(user = "@RyersonU", n = 1000, includeRts= TRUE)

tweetsDF<-twListToDF(tweets_ru)

#Task 1 Number of Reply posts versus other posts 

ggplot(tweetsDF, aes(factor(isRetweet!=FALSE))) +
  geom_bar(fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  ylab("Number of tweets") + 
  ggtitle("Retweeted Tweets") +
  scale_x_discrete(labels=c("Not retweeted", "Retweeted tweets"))


#Task1.1 -Change the display in percentage
ggplot(tweetsDF, aes(x = tweetsDF$isRetweet)) +
  geom_bar(aes(y=(..count..) / sum(..count..)), fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  scale_y_continuous(labels=percent) +
  labs(title = "Tweets by Type", y="Percentage of tweets") + 
  scale_x_discrete(labels=c("Not Retweeted", "Retweeted"))


#Task 2: Number of Retweets versus other posts
ggplot(tweetsDF, aes(factor(!is.na(replyToSN)))) +
  geom_bar(fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  ylab("Number of tweets") + 
  ggtitle("Replied Tweets") +
  scale_x_discrete(labels=c("Not in reply", "Replied tweets"))

#Task 2.1  Change the display in percentage
ggplot(tweetsDF, aes(x = factor(!is.na(tweetsDF$replyToSN))) +
  geom_bar(aes(y=(..count..) / sum(..count..)),fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  scale_y_continuous(labels=percent) +
  labs(y="Number of tweets", title="Tweets by Type (Reply vs. Not in Reply)") +
  scale_x_discrete(labels=c("Not in Reply", "Replied"))
       
#Task 3: Plot comparing the distribution of the number of times a post was retweeted for the original tweets VS for replies.
  tweetsDF$type <- "original tweet"   #Create a new column "Type" which initially filled with all tweet info
  tweetsDF[(tweetsDF$isRetweet!=FALSE),17] <- "RT"     #Overwrite Retweets as RT
  tweetsDF[(!is.na(tweetsDF$replyToSN)),17] <- "Reply"    #Overwrite reply as Reply
  tweetsDF$type <- as.factor(tweetsDF$type) 
  tweetsDF$type = factor(tweetsDF$type,levels(tweetsDF$type)[c(3,1,2)])
       
  sp2 <- ggplot(tweetsDF, aes(x= type, y= retweetCount)) + geom_point(shape=1)
  sp2 + facet_grid(. ~ type)


#Task 4 Propose a new chart to visualize any other metadata field(s) available in your dataset. 
# I have used favoriteCount field to identify which tweets are most favorite
sp_fav <- ggplot(tweetsDF, aes(x= retweetCount, y= favoriteCount)) + geom_point(shape=1)
sp_fav + facet_grid(. ~ type)       