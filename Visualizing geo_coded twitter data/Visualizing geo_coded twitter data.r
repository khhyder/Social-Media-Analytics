
if (!require("twitteR")) {
  install.packages("twitteR", repos="http://cran.rstudio.com/") 
  library("twitteR")
}

# REPLACE WITH YOUR API KEY
myapikey <- "*************************"
# REPLACE WITH YOUR API SECRET
myapisecret <- "*************************"
# REPLACE WITH YOUR ACCESS TOKEN
myaccesstoken <- "*************************-*************************"
# REPLACE WITH YOUR ACCESS TOKEN SECRET
myaccesstokensecret <- "*************************"

options(httr_oauth_cache=T) #This will enable the use of a local file to cache OAuth access credentials between R sessions

setup_twitter_oauth(myapikey,
                    myapisecret,
                    myaccesstoken,
                    myaccesstokensecret)

jessi <- getUser("Jessiereyez")
location(jessi)

num_followers <- jessi$followersCount
sh_follower_ids <- jessi$getFollowers(n=2500)

length(sh_follower_ids)
if (!require("data.table")) {
  install.packages("data.table", repos="http://cran.rstudio.com/") 
  library("data.table")
}

jes_followers_df = rbindlist(lapply(sh_follower_ids,as.data.frame))

head(jes_followers_df$location, 10)

jes_followers_df<-subset(jes_followers_df, location!=" ")

head(jes_followers_df$location, 10)

jes_followers_df$location<-gsub("%", "", jes_followers_df$location)

jes_followers_df$location <- gsub("[^0-9A-Za-z#///' ]", "", jes_followers_df$location)

#Replace with your google API.
google_api_key <- "*************************-*************************-*************************"

#Install key package helpers:
source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/geocode_helpers.R")
#Install modified version of the geocode function
#(that now includes the api_key parameter):
source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/modified_geocode.R")



geocode_apply<-function(x){
    geocode(x, source = "google", output = "all", api_key=google_api_key)
}

geocode_results <- sapply(jes_followers_df$location, geocode_apply, simplify = F)
geocode_results_backup <- geocode_results
geocode_results <- geocode_results_backup

condition_a <- sapply(geocode_results, function(x) x["status"]=="OK")
geocode_results<-geocode_results[condition_a]

condition_b <- lapply(geocode_results, lapply, length)




condition_b2<-sapply(condition_b, function(x) x["results"]=="1")


geocode_results<-geocode_results[condition_b2]


length(geocode_results)

source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/cleaning_geocoded_results.R")


results_b <- lapply(geocode_results, as.data.frame)

results_c<-lapply(results_b,function(x) subset(x, select=c("results.formatted_address",
                                                           "results.geometry.location")))

results_d<-lapply(results_c,function(x) data.frame(Location=x[1,"results.formatted_address"],
                                                   lat=x[1,"results.geometry.location"],
                                                   lng=x[2,"results.geometry.location"]))


head(results_d,10)

results_e<-rbindlist(results_d)

results_f<-results_e[,Original_Location:=names(results_d)]

canadian_results<-subset(results_f,
                  grepl(", Canada", results_f$Location)==TRUE)

head(canadian_results)

canadian_results$commas<-sapply(canadian_results$Location, function(x)
  length(as.numeric(gregexpr(",", as.character(x))[[1]])))

canadian_results<-subset(canadian_results, commas==2)

#Drop the "commas" column:
canadian_results<-subset(canadian_results, select=-commas)

nrow(canadian_results)



### Step 6: Map the Geocoded Results


# Load / install maps and mapproj packages
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE, repos="http://cran.rstudio.com/")
    sapply(pkg, require, character.only = TRUE)
}


ipak(c("maps", "mapproj"))
library(mapdata)

#Generate a blank map:
canada_map <- map("worldHires", "Canada", proj="albers", param=c(39,45), 
                  xlim=c(-141, -53), ylim=c(40,85), col="grey90", fill=T, bg=NA, lwd=0.2, add=FALSE, resolution=1)

#Add points to it:
points(mapproject(canadian_results$lng, canadian_results$lat), col=NA, bg="blue", pch=21, cex=1.0)
#Add a title:
mtext("The Geography of @Jessie Reyes's Followers", side = 3, line = -3.5, outer = T, cex=1.5, font=3)

visit.x<-results_f$lng
visit.y<-results_f$lat

#Using GGPLOT, plot the Base World Map

library(ggplot2)
mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() +   mapWorld

#Now Layer the cities on top
mp <- mp+ geom_point(aes(x=visit.x, y=visit.y) ,color="blue", size=3) 
mp
