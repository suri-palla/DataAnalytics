# Program : Yelp data aggregated summary statistics by week 

# CLearing all the existing variables in workspace, if any
rm(list = ls())

# Setting working directory
# setwd("/Users/Naveen/Documents/Srikanth/")
setwd("C:/Surendra/Courses/Business Analytics/Yelp/Effects of events in restaurants/test run V007")

# #################################################################
# COnfiguration Settings

# category.config is to identity if a business belongs to a particular category 
category.config <- "mexican"
# category.delis is to identity if a business belongs to a delis category 
category.delis <- "delis"

start.date <- as.Date("2014-09-03")
end.date <- as.Date("2016-01-29")
event.date <- as.Date("2015-08-27")
# #################################################################

# ########################
# loading the libraries 
library(plyr)
library(tm)
# ########################

# #############################################################################################################
# ############################## LOAD REQUIRED FILES ##########################################################

# load the reviews file 
load("yelp.reviews.RData")
# The following restaurants are mobile or truck businesses and needed to be removed
# finding the index of the business 
index.r.1 <- which(read.reviews.final.merge$restaurant.id == "tamale-trolley-memphis" )
index.r.2 <- which(read.reviews.final.merge$restaurant.id == "big-boys-filipino-food-truck-seattle")
index.r.3 <- which(read.reviews.final.merge$restaurant.id == "cycle-dogs-vegan-hot-dogs-seattle")
index.r.4 <- which(read.reviews.final.merge$restaurant.id == "brown-bag-baguette-seattle")
index.r.5 <- which(read.reviews.final.merge$restaurant.id == "little-italy-seattle-seattle")
# removing all the identified business by index
read.reviews.final.merge <- read.reviews.final.merge[-c(index.r.1,index.r.2,index.r.3,index.r.4,index.r.5),]

# load the business file - Source API
load("business.master.memphis.seattlecity_3_23.RData")
dup.index.biz <- which(duplicated(business.master.memphis.seattlecity$id))
business.master.memphis.seattlecity <- business.master.memphis.seattlecity[-c(dup.index.biz),]

# The following restaurants are mobile or truck businesses and needed to be removed
# finding the index of the business 
index.1 <- which(business.master.memphis.seattlecity$id == "tamale-trolley-memphis" )
index.2 <- which(business.master.memphis.seattlecity$id == "big-boys-filipino-food-truck-seattle")
index.3 <- which(business.master.memphis.seattlecity$id == "cycle-dogs-vegan-hot-dogs-seattle")
index.4 <- which(business.master.memphis.seattlecity$id == "brown-bag-baguette-seattle")
index.5 <- which(business.master.memphis.seattlecity$id == "little-italy-seattle-seattle")
business.master.memphis.seattlecity <- business.master.memphis.seattlecity[-c(index.1,index.2,index.3,index.4,index.5),]

# load the price data - Source scrapping program
load("business.price.range.RData")
# remove duplicate records from price data 
dup.index <- which(duplicated(business.price.range))
business.price.range <- business.price.range[-c(dup.index),]

# The following restaurants are mobile or truck businesses and needed to be removed
# finding the index of the business 
index.p.1 <- which(business.price.range$id == "tamale-trolley-memphis" )
index.p.2 <- which(business.price.range$id == "big-boys-filipino-food-truck-seattle")
index.p.3 <- which(business.price.range$id == "cycle-dogs-vegan-hot-dogs-seattle")
index.p.4 <- which(business.price.range$id == "brown-bag-baguette-seattle")
index.p.5 <- which(business.price.range$id == "little-italy-seattle-seattle")
business.price.range <- business.price.range[-c(index.p.1,index.p.2,index.p.3,index.p.4,index.p.5),]

# #############################################################################################################

# ###############################################################################
# pre-process data

# converting dates into a single format - added new column formatted.review.date
temp.date <- as.Date(read.reviews.final.merge$review.date, format="%m/%d/%Y")
temp.date[is.na(temp.date)] <- as.Date(read.reviews.final.merge$review.date[is.na(temp.date)], format="%Y-%m-%d")
read.reviews.final.merge$formatted.review.date <- temp.date

# new columns - extract week and year
read.reviews.final.merge$year <- strftime(read.reviews.final.merge$formatted.review.date,format = "%Y")
read.reviews.final.merge$week <- strftime(read.reviews.final.merge$formatted.review.date,format = "%W")

# converting the year and week into numeric 
read.reviews.final.merge$year <- as.numeric(read.reviews.final.merge$year)
read.reviews.final.merge$week <- as.numeric(read.reviews.final.merge$week)

# creating a new column for indicating zero week
# PURPOSE : to identify the week zero and corresponding year while replacing 
read.reviews.final.merge$week.zero.indicator <- ifelse(read.reviews.final.merge$week == 0,1,0)

# sorting the years in ascending order
unique.year <- sort(unique(read.reviews.final.merge$year),decreasing = F)

# Replacing the week zero in a year with previous years maximun week
# also replacing the week zero's year to the previous year
for(i in 1:length(unique.year)){
  # check for zero week days
  check.week.exists <- read.reviews.final.merge[read.reviews.final.merge$year == unique.year[i] & read.reviews.final.merge$week == 0,]
  if(nrow(check.week.exists) > 0){
    # replace zero week with previous year's maximum week
    read.reviews.final.merge$week[read.reviews.final.merge$year == unique.year[i] & read.reviews.final.merge$week.zero.indicator == 1] <- 
      max(read.reviews.final.merge$week[read.reviews.final.merge$year == (unique.year[i] - 1)])
    # replace week zero's year with previous year
    read.reviews.final.merge$year[read.reviews.final.merge$year == unique.year[i] & read.reviews.final.merge$week.zero.indicator == 1] <- (unique.year[i] - 1)
  }
  
}

# removing the week.zero.indicator column 
read.reviews.final.merge$week.zero.indicator <- NULL

# filtering only current and previous reviews
read.reviews.final.merge <- read.reviews.final.merge[read.reviews.final.merge$review.comment == "Current Review"
                                                     | read.reviews.final.merge$review.comment == "Previous Review",]

# filtering data based on start and end dates 
read.reviews.final.merge <- read.reviews.final.merge[(read.reviews.final.merge$formatted.review.date > start.date &
                                                        read.reviews.final.merge$formatted.review.date < end.date),]

# check for NA's in review rating and cool,funny and useful votes, replace with zero's if any
read.reviews.final.merge$review.rating[is.na(read.reviews.final.merge$review.rating)] <- 0 
read.reviews.final.merge$cool.vote.count[is.na(read.reviews.final.merge$cool.vote.count)] <- 0 
read.reviews.final.merge$funny.vote.count[is.na(read.reviews.final.merge$funny.vote.count)] <- 0 
read.reviews.final.merge$useful.vote.count[is.na(read.reviews.final.merge$useful.vote.count)] <- 0 

# convert categories to lower case
business.master.memphis.seattlecity$categories <- tolower(business.master.memphis.seattlecity$categories)

# check for NA's in the column zipcode in business file, replace with zero's if any
business.master.memphis.seattlecity$location.postal_code[is.na(business.master.memphis.seattlecity$location.postal_code)] <- 0

#############################################################################################################
################ Compute Fraction of mexican (configured) restaurants at a zip code level ###################

# zipcode aggregation 
# getting unique business from the filtered data between start and end date
temp <- as.data.frame(unique(read.reviews.final.merge$restaurant.id))
colnames(temp) <- "restaurant.id"

# merging reviews(within configured start and end date) with business file
zipcodes.agg.df <- merge(temp,business.master.memphis.seattlecity,by = "restaurant.id",by.y = "id", all.x = T)

# convert to lower case
category.config <- tolower(category.config)

# creating binary column to represent category 
zipcodes.agg.df$category.indicator <- ifelse((grepl(glob2rx(category.config),zipcodes.agg.df$categories)),1,0)

# aggregate number of businesses in a zipcode
zipcode.agg.no.rest <- aggregate(zipcodes.agg.df$restaurant.id,
                                 by = list(zipcodes.agg.df$location.postal_code),FUN = length)
colnames(zipcode.agg.no.rest) <- c("zipcodes","no.of.rest.zipcode")                                 
# replace na with zero if na is produced as a result of aggregation
zipcode.agg.no.rest$no.of.rest.zipcode[is.na(zipcode.agg.no.rest$no.of.rest.zipcode)] <- 0

# aggregate (sum) number business from a particular category (mexican) in a zipcode
zipcode.agg.no.rest.cat <- aggregate(zipcodes.agg.df$category.indicator,
                                     by = list(zipcodes.agg.df$location.postal_code),
                                     FUN = sum)
colnames(zipcode.agg.no.rest.cat) <- c("zipcodes","no.of.rest.cat.zipcode")
# replace na with zero if na is produced as a result of aggregation
zipcode.agg.no.rest.cat$no.of.rest.cat.zipcode[is.na(zipcode.agg.no.rest.cat$no.of.rest.cat.zipcode)] <- 0

# merge number of business in a zipcode and number business from a particular category in a zipcode 
merge.zipcode.rest.cat <- merge(zipcode.agg.no.rest,zipcode.agg.no.rest.cat,
                                by = "zipcodes", all.x = T)
# Replacing NA's with zero resulting from left outter join 
merge.zipcode.rest.cat$no.of.rest.cat.zipcode[is.na(merge.zipcode.rest.cat$no.of.rest.cat.zipcode)] <- 0

# creating a column for number of business that fo not belong to a particular category in a zipcode
merge.zipcode.rest.cat$no.of.rest.non.cat.zipcode <- merge.zipcode.rest.cat$no.of.rest.zipcode - merge.zipcode.rest.cat$no.of.rest.cat.zipcode

############################################################################################################


#######################################################################################################################
############################ Compute restaurant level  rating Statistics (mean, min, sd, max) by week #################

# aggregate (mean) rating by week
agg.avg.rating <- aggregate(read.reviews.final.merge$review.rating,
                            by = list(read.reviews.final.merge$restaurant.id,
                                      read.reviews.final.merge$week,
                                      read.reviews.final.merge$year),FUN = mean)
colnames(agg.avg.rating) <- c("restaurant.id","week","year","avg.rating")
# replace na with zero if na is produced as a result of aggregation
agg.avg.rating$avg.rating[is.na(agg.avg.rating$avg.rating)] <- 0

# aggregate (minimum) rating by week
agg.min.rating <- aggregate(read.reviews.final.merge$review.rating,
                            by = list(read.reviews.final.merge$restaurant.id,
                                      read.reviews.final.merge$week,
                                      read.reviews.final.merge$year),FUN = min)
colnames(agg.min.rating) <- c("restaurant.id","week","year","min.rating")
# replace na with zero if na is produced as a result of aggregation
agg.min.rating$min.rating[is.na(agg.min.rating$min.rating)] <- 0

# aggregate maximum rating by week
agg.max.rating <- aggregate(read.reviews.final.merge$review.rating,
                            by = list(read.reviews.final.merge$restaurant.id,
                                      read.reviews.final.merge$week,
                                      read.reviews.final.merge$year),FUN = max)
colnames(agg.max.rating) <- c("restaurant.id","week","year","max.rating")
# replace na with zero if na is produced as a result of aggregation
agg.max.rating$max.rating[is.na(agg.max.rating$max.rating)] <- 0

# aggregate standard deviation rating by week
agg.sd.rating <- aggregate(read.reviews.final.merge$review.rating,
                           by = list(read.reviews.final.merge$restaurant.id,
                                     read.reviews.final.merge$week,
                                     read.reviews.final.merge$year),FUN = sd)
colnames(agg.sd.rating) <- c("restaurant.id","week","year","sd.rating")
# Replacing the expected NA's with zero 
# We get NA for a single review for a week
agg.sd.rating$sd.rating[is.na(agg.sd.rating$sd.rating)] <- 0

# merge maximum,minimum,average and standard deviation of review rating by week
merge.agg.min.max <- merge(agg.max.rating,agg.min.rating,by = c("restaurant.id","week","year"),all.x = T)
# Replacing NA's with zero resulting from left outter join 
merge.agg.min.max$min.rating[is.na(merge.agg.min.max$min.rating)] <- 0

merge.agg.min.max.avg <- merge(merge.agg.min.max,agg.avg.rating,by = c("restaurant.id","week","year"),all.x = T)
# Replacing NA's with zero resulting from left outter join 
merge.agg.min.max.avg$avg.rating[is.na(merge.agg.min.max.avg$avg.rating)] <- 0

merge.agg.min.max.avg.sd <- merge(merge.agg.min.max.avg,agg.sd.rating,by = c("restaurant.id","week","year"),all.x = T)
# Replacing NA's with zero resulting from left outter join 
merge.agg.min.max.avg.sd$sd.rating[is.na(merge.agg.min.max.avg.sd$sd.rating)] <- 0

#########################################################################################################################
################ Compute restaurant level  rating Statistics (votes) by week ############################################

# aggregate sum of useful votes by week
agg.use.votes.sum <- aggregate(read.reviews.final.merge$useful.vote.count,
                               by = list(read.reviews.final.merge$restaurant.id,
                                         read.reviews.final.merge$week,
                                         read.reviews.final.merge$year),FUN = sum)
colnames(agg.use.votes.sum) <- c("restaurant.id","week","year","sum.useful.votes")
# replace na with zero if na is produced as a result of aggregation
agg.use.votes.sum$sum.useful.votes[is.na(agg.use.votes.sum$sum.useful.votes)] <- 0

# aggregate average of useful votes by week
agg.use.votes.avg <- aggregate(read.reviews.final.merge$useful.vote.count,
                               by = list(read.reviews.final.merge$restaurant.id,
                                         read.reviews.final.merge$week,
                                         read.reviews.final.merge$year),FUN = mean)
colnames(agg.use.votes.avg) <- c("restaurant.id","week","year","avg.useful.votes")
# replace na with zero if na is produced as a result of aggregation
agg.use.votes.avg$avg.useful.votes[is.na(agg.use.votes.avg$avg.useful.votes)] <- 0

# aggregate sum of cool votes by week
agg.cool.votes.sum <- aggregate(read.reviews.final.merge$cool.vote.count,
                                by = list(read.reviews.final.merge$restaurant.id,
                                          read.reviews.final.merge$week,
                                          read.reviews.final.merge$year),FUN = sum)
colnames(agg.cool.votes.sum) <- c("restaurant.id","week","year","sum.cool.votes")
# replace na with zero if na is produced as a result of aggregation
agg.cool.votes.sum$sum.cool.votes[is.na(agg.cool.votes.sum$sum.cool.votes)] <- 0

# aggregate average of cool votes by week
agg.cool.votes.avg <- aggregate(read.reviews.final.merge$cool.vote.count,
                                by = list(read.reviews.final.merge$restaurant.id,
                                          read.reviews.final.merge$week,
                                          read.reviews.final.merge$year),FUN = mean)
colnames(agg.cool.votes.avg) <- c("restaurant.id","week","year","avg.cool.votes")
# replace na with zero if na is produced as a result of aggregation
agg.cool.votes.avg$avg.cool.votes[is.na(agg.cool.votes.avg$avg.cool.votes)] <- 0

# aggregate sum of funny votes by week
agg.funny.votes.sum <- aggregate(read.reviews.final.merge$funny.vote.count,
                                 by = list(read.reviews.final.merge$restaurant.id,
                                           read.reviews.final.merge$week,
                                           read.reviews.final.merge$year),FUN = sum)
colnames(agg.funny.votes.sum) <- c("restaurant.id","week","year","sum.funny.votes")
# replace na with zero if na is produced as a result of aggregation
agg.funny.votes.sum$sum.funny.votes[is.na(agg.funny.votes.sum$sum.funny.votes)] <- 0


# aggregate average of funny votes by week
agg.funny.votes.avg <- aggregate(read.reviews.final.merge$funny.vote.count,
                                 by = list(read.reviews.final.merge$restaurant.id,
                                           read.reviews.final.merge$week,
                                           read.reviews.final.merge$year),FUN = mean)
colnames(agg.funny.votes.avg) <- c("restaurant.id","week","year","avg.funny.votes")
# replace na with zero if na is produced as a result of aggregation
agg.funny.votes.avg$avg.funny.votes[is.na(agg.funny.votes.avg$avg.funny.votes)] <- 0


# merge cool,useful,funny aggregated votes
merge.use.sum.avg <- merge(agg.use.votes.sum,agg.use.votes.avg,by = c("restaurant.id","week","year"),all.x = T)
# Replacing NA's with zero resulting from left outter join 
merge.use.sum.avg$avg.useful.votes[is.na(merge.use.sum.avg$avg.useful.votes)] <- 0

merge.cool.sum.avg <- merge(agg.cool.votes.sum,agg.cool.votes.avg,by = c("restaurant.id","week","year"),all.x = T)
# Replacing NA's with zero resulting from left outter join 
merge.cool.sum.avg$avg.cool.votes[is.na(merge.cool.sum.avg$avg.cool.votes)] <- 0

merge.funny.sum.avg <- merge(agg.funny.votes.sum,agg.funny.votes.avg,by = c("restaurant.id","week","year"),all.x = T)
# Replacing NA's with zero resulting from left outter join 
merge.funny.sum.avg$avg.funny.votes[is.na(merge.funny.sum.avg$avg.funny.votes)] <- 0

merge.use.cool <- merge(merge.use.sum.avg,merge.cool.sum.avg,by = c("restaurant.id","week","year"),all.x = T)
# Replacing NA's with zero resulting from left outter join 
merge.use.cool$sum.cool.votes[is.na(merge.use.cool$sum.cool.votes)] <- 0
merge.use.cool$avg.cool.votes[is.na(merge.use.cool$avg.cool.votes)] <- 0

merge.use.cool.funny <- merge(merge.use.cool,merge.funny.sum.avg,by = c("restaurant.id","week","year"),all.x = T)
# Replacing NA's with zero resulting from left outter join 
merge.use.cool.funny$sum.funny.votes[is.na(merge.use.cool.funny$sum.funny.votes)] <- 0
merge.use.cool.funny$avg.funny.votes[is.na(merge.use.cool.funny$avg.funny.votes)] <- 0

# #############################################################################################################

#########################################################################################################################
################ Compute the length of review length ############################################

# aggregate word length of the reviews
read.reviews.final.merge$review.text <- as.character(read.reviews.final.merge$review.text)
# initialise the review lengths to zero
read.reviews.final.merge$length.review.text.short <- 0L
read.reviews.final.merge$length.review.text.full <- 0L

# calculating review length in terms number of words
for(i in 1:nrow(read.reviews.final.merge))
{
  # creating a corpus for a review 
  input.review <- Corpus(VectorSource(read.reviews.final.merge$review.text[i]))
  # convert to lower case
  input.review <- tm_map(input.review, content_transformer(tolower))
  # remove punctuation
  input.review <- tm_map(input.review, removePunctuation)
  # remove numbers
  input.review <- tm_map(input.review, removeNumbers)
  # remove stop words
  input.review <- tm_map(input.review, removeWords, c(stopwords("english")))
  # creating term document matrix
  term.doc.matrix <- TermDocumentMatrix(input.review)
  double.term.doc.matrix <- as.matrix(term.doc.matrix)
  # get the sum of TDM to find the number of words
  read.reviews.final.merge$length.review.text.short[i] <- as.integer(colSums(double.term.doc.matrix))
  # get the sum of words as it is in the review
  read.reviews.final.merge$length.review.text.full[i] <- vapply(strsplit(read.reviews.final.merge$review.text[i], "\\W+"), length, integer(1))
  print(i)
}

# get the required columns - year,week rest id, review lengths
# get the index of the columns
index.col1 <- which(colnames(read.reviews.final.merge) == "restaurant.id")
index.col2 <- which(colnames(read.reviews.final.merge) == "year")
index.col3 <- which(colnames(read.reviews.final.merge) == "week")
index.col4 <- which(colnames(read.reviews.final.merge) == "length.review.text.short")
index.col5 <- which(colnames(read.reviews.final.merge) == "length.review.text.full")
# use the indexes to get the required columns
reviews.length <- read.reviews.final.merge[c(index.col1,index.col2,index.col3,index.col4,index.col5)]

# replace na's with zeros if any, before aggregating
reviews.length$length.review.text.short[is.na(reviews.length$length.review.text.short)] <- 0
reviews.length$length.review.text.full[is.na(reviews.length$length.review.text.full)] <- 0

# aggregate the review lengths by week
agg.reviews.length.sum <- ddply(reviews.length, 
                                c("restaurant.id", "week","year"),  numcolwise(sum))
colnames(agg.reviews.length.sum) <- c("restaurant.id", "week","year","sum.length.review.text.short","sum.length.review.text.full")
# replace na with zero if na is produced as a result of aggregation
agg.reviews.length.sum$sum.length.review.text.short[is.na(agg.reviews.length.sum$sum.length.review.text.short)] <- 0
agg.reviews.length.sum$sum.length.review.text.full[is.na(agg.reviews.length.sum$sum.length.review.text.full)] <- 0

# aggregate the average review lengths by week
agg.reviews.length.mean <- ddply(reviews.length, 
                                 c("restaurant.id", "week","year"),  numcolwise(mean))
colnames(agg.reviews.length.mean) <- c("restaurant.id", "week","year","mean.length.review.text.short","mean.length.review.text.full")
# replace na with zero if na is produced as a result of aggregation
agg.reviews.length.mean$mean.length.review.text.short[is.na(agg.reviews.length.mean$mean.length.review.text.short)] <- 0
agg.reviews.length.mean$mean.length.review.text.full[is.na(agg.reviews.length.mean$mean.length.review.text.full)] <- 0

# standard deviation for length of reviews
agg.reviews.length.sd <- ddply(reviews.length, 
                               c("restaurant.id", "week","year"),  numcolwise(sd))
colnames(agg.reviews.length.sd) <- c("restaurant.id", "week","year","sd.length.review.text.short","sd.length.review.text.full")
# Replacing the expected NA's with zero 
# We get NA for a single review for a week
agg.reviews.length.sd$sd.length.review.text.short[is.na(agg.reviews.length.sd$sd.length.review.text.short)] <- 0
agg.reviews.length.sd$sd.length.review.text.full[is.na(agg.reviews.length.sd$sd.length.review.text.full)] <- 0

# merge review length sum,mean and standard deviation of review lengths by week
merge.review.length.sum.mean <- merge(agg.reviews.length.sum,agg.reviews.length.mean, by = c("restaurant.id", "week","year"), all.x = T) 
# Replacing NA's with zero resulting from left outter join 
merge.review.length.sum.mean$mean.length.review.text.short[is.na(merge.review.length.sum.mean$mean.length.review.text.short)] <- 0
merge.review.length.sum.mean$mean.length.review.text.full[is.na(merge.review.length.sum.mean$mean.length.review.text.full)] <- 0

merge.review.length.sum.mean.sd <- merge(merge.review.length.sum.mean,agg.reviews.length.sd, by = c("restaurant.id", "week","year"), all.x = T) 
# Replacing NA's with zero resulting from left outter join 
merge.review.length.sum.mean.sd$sd.length.review.text.short[is.na(merge.review.length.sum.mean.sd$sd.length.review.text.short)] <- 0
merge.review.length.sum.mean.sd$sd.length.review.text.full[is.na(merge.review.length.sum.mean.sd$sd.length.review.text.full)] <- 0

# ##############################################################################################################################################

# ######################################################################################
# # calculate number of days and weeks from the start and event dates 

# getting week and years of the event date 
event.date.week <- as.numeric(strftime(event.date,format = "%W"))
event.date.year <- as.numeric(strftime(event.date,format = "%Y"))
start.week.number <- as.numeric(strftime(start.date,format = "%W"))
start.year <- as.numeric(strftime(start.date,format = "%Y"))

# create a list of dates from start to end dates
list.start.end.dates <- as.data.frame(seq(start.date,end.date, by = "days"))
colnames(list.start.end.dates) <- "all.dates"
list.start.end.dates$all.weeks <- strftime(list.start.end.dates$all.dates,format = "%W")
list.start.end.dates$all.years <- strftime(list.start.end.dates$all.dates,format = "%Y")
list.start.end.dates$all.weeks <- as.numeric(list.start.end.dates$all.weeks)
list.start.end.dates$all.years <- as.numeric(list.start.end.dates$all.years)

# create a column to indicate the records with week zero 
list.start.end.dates$week.zero.indicator <- ifelse(list.start.end.dates$all.weeks == 0,1,0)

# get the unique years 
unique.all.years <- unique(list.start.end.dates$all.years)

# take care of the zero week
for(i in 1:length(unique.all.years)){
  
  # check if there are records with week zero
  check.zero.week <- list.start.end.dates[list.start.end.dates$all.years == unique.all.years[i] & list.start.end.dates$all.weeks == 0,]
  if(nrow(check.zero.week) > 0){
    # replace zero week with previous year's maximum week
    list.start.end.dates$all.weeks[list.start.end.dates$all.years == unique.all.years[i] & list.start.end.dates$week.zero.indicator == 1] <- 
      max(list.start.end.dates$all.weeks[list.start.end.dates$all.years == (unique.all.years[i] - 1)])
    # replace week zero's year with previous year
    list.start.end.dates$all.years[list.start.end.dates$all.years == unique.all.years[i] & list.start.end.dates$week.zero.indicator == 1] <- (unique.all.years[i] - 1)
  }  
  
}

# aggregate number of days from start to end dates by week
# 
no.of.days.week <- aggregate(list.start.end.dates$all.dates,
                             by = list(list.start.end.dates$all.weeks,
                                       list.start.end.dates$all.years),
                             FUN = length)
colnames(no.of.days.week) <- c("week","year","no.of.days.week.start")
# replace na with zero if na is produced as a result of aggregation
no.of.days.week$no.of.days.week.start[is.na(no.of.days.week$no.of.days.week.start)] <- 0


# filter the days after event
all.days.after.event <- list.start.end.dates[list.start.end.dates$all.dates >= event.date,]

# aggregate number of days from event to end dates by week
no.of.days.week.after.event <- aggregate(all.days.after.event$all.dates,
                                         by = list(all.days.after.event$all.weeks,
                                                   all.days.after.event$all.years),
                                         FUN = length)
colnames(no.of.days.week.after.event) <- c("week","year","no.of.days.week.event")
# replace na with zero if na is produced as a result of aggregation
no.of.days.week.after.event$no.of.days.week.event[is.na(no.of.days.week.after.event$no.of.days.week.event)] <- 0

# merge the aggreaged number of days from start and event by week
merge.week.days <- merge(no.of.days.week,no.of.days.week.after.event,by = c("week","year"),all.x = T)
# ordering by year and week
merge.week.days <- merge.week.days[order(merge.week.days$year,merge.week.days$week),]

# replacing NA's with zero for weeks before the event 
merge.week.days$no.of.days.week.event[(is.na(merge.week.days$no.of.days.week.event))] <- 0

# removing the columns 
list.start.end.dates$week.zero.indicator <- NULL
list.start.end.dates$all.dates <- NULL

# get the unique week and year
list.start.end.dates <- unique(list.start.end.dates)
# order by week and year 
list.start.end.dates <- list.start.end.dates[order(list.start.end.dates$all.years,list.start.end.dates$all.weeks),]
# find out the index of the record that is associated with event week and year
index.event.date <- min(which((list.start.end.dates$all.weeks == event.date.week) &
                                (list.start.end.dates$all.years == event.date.year)))
# initializing the event week indicator
list.start.end.dates$event.week.indicator <- 0
# seting ones in event week indicator after from event week
list.start.end.dates$event.week.indicator[index.event.date:nrow(list.start.end.dates)] <- 1
# creating and setting one to a column start week indicator 
list.start.end.dates$start.week.indicator <- 1
colnames(list.start.end.dates) <- c("week","year","event.week.indicator","start.week.indicator")

# merge event and start week indicators and number days from start and event 
dates.week.year <- merge(list.start.end.dates,merge.week.days,by = c("year","week"), all.x = T)
# order by year and week
dates.week.year <- dates.week.year[order(dates.week.year$year,dates.week.year$week),]

# cumulative sum of weeks from start and event 
dates.week.year$no.of.weeks.from.event <- cumsum(dates.week.year$event.week.indicator)
dates.week.year$no.of.weeks.from.start <- cumsum(dates.week.year$start.week.indicator)

# cumulative sum of number of days from start and event 
dates.week.year$no.of.days.from.event <- cumsum(dates.week.year$no.of.days.week.event)
dates.week.year$no.of.days.from.start <- cumsum(dates.week.year$no.of.days.week.start)

# removing the unwanted columns
dates.week.year$event.week.indicator <- NULL
dates.week.year$start.week.indicator <- NULL
dates.week.year$no.of.days.week.start <- NULL
dates.week.year$no.of.days.week.event <- NULL

# ##############################################################################################################################

# ################################################################################################
########################## Calculate cumulative sum, number and average of reviews by week #######

# aggregation up untill the week before
# aggregate number of reviews per week
agg.no.of.reviews <- aggregate(read.reviews.final.merge$review.unique.id,
                               by = list(read.reviews.final.merge$restaurant.id,
                                         read.reviews.final.merge$week,
                                         read.reviews.final.merge$year),FUN = length)
colnames(agg.no.of.reviews) <- c("restaurant.id","week","year","no.of.reviews")
# replace na with zero if na is produced as a result of aggregation
agg.no.of.reviews$no.of.reviews[is.na(agg.no.of.reviews$no.of.reviews)] <- 0

# aggregate sum of reviews per week
agg.sum.of.rating <- aggregate(read.reviews.final.merge$review.rating,
                               by = list(read.reviews.final.merge$restaurant.id,
                                         read.reviews.final.merge$week,
                                         read.reviews.final.merge$year),FUN = sum)
colnames(agg.sum.of.rating) <- c("restaurant.id","week","year","sum.of.rating")
# replace na with zero if na is produced as a result of aggregation
agg.sum.of.rating$sum.of.rating[is.na(agg.sum.of.rating$sum.of.rating)] <- 0

# merge number and sum of reviews per week
merge.agg.reviews.sumofrating <- merge(agg.no.of.reviews,agg.sum.of.rating,by = c("restaurant.id","week","year"),all.x = T)
# Replacing NA's with zero resulting from left outter join 
merge.agg.reviews.sumofrating$sum.of.rating[is.na(merge.agg.reviews.sumofrating$sum.of.rating)] <- 0

# assigning variable 
merge.agg.reviews.sumofrating.event <- merge.agg.reviews.sumofrating
# order by business, year and week
merge.agg.reviews.sumofrating.event <- merge.agg.reviews.sumofrating.event[order(merge.agg.reviews.sumofrating.event$restaurant.id,
                                                                                 merge.agg.reviews.sumofrating.event$year,
                                                                                 merge.agg.reviews.sumofrating.event$week),]

# get the unique business 
temp.rest.list <- unique(merge.agg.reviews.sumofrating.event$restaurant.id)

# initialise the cumulative number, sum and average of ratings weekly to zero
merge.agg.reviews.sumofrating.event$cum.no.of.reviews <- 0
merge.agg.reviews.sumofrating.event$cum.sum.of.rating <- 0
merge.agg.reviews.sumofrating.event$cum.avg.rating <- 0

# for loop for each business to calculate cumulative sum and number of ratings 
for(i in 1:length(temp.rest.list)){
  
  # IF CONDITION : check if there are more than one row a particular business
  # ELSE CONDITION : sets the cumulative sum to zero if there is only row for a business
  # this is due to the reason that cumulative aggregation is done until the week before
  if(length(cumsum(merge.agg.reviews.sumofrating.event$no.of.reviews[merge.agg.reviews.sumofrating.event$restaurant.id == temp.rest.list[i]])) > 1){
    
    merge.agg.reviews.sumofrating.event$cum.no.of.reviews[merge.agg.reviews.sumofrating.event$restaurant.id == temp.rest.list[i]] <- 
      c(0,cumsum(merge.agg.reviews.sumofrating.event$no.of.reviews[merge.agg.reviews.sumofrating.event$restaurant.id == temp.rest.list[i]])[1:(length(cumsum(merge.agg.reviews.sumofrating.event$no.of.reviews[merge.agg.reviews.sumofrating.event$restaurant.id == temp.rest.list[i]]))- 1)])
  } else {
    merge.agg.reviews.sumofrating.event$cum.no.of.reviews[merge.agg.reviews.sumofrating.event$restaurant.id == temp.rest.list[i]] <- 0
  }
  
  if(length(cumsum(merge.agg.reviews.sumofrating.event$sum.of.rating[merge.agg.reviews.sumofrating.event$restaurant.id == temp.rest.list[i]])) > 1){
    merge.agg.reviews.sumofrating.event$cum.sum.of.rating[merge.agg.reviews.sumofrating.event$restaurant.id == temp.rest.list[i]] <- 
      c(0,cumsum(merge.agg.reviews.sumofrating.event$sum.of.rating[merge.agg.reviews.sumofrating.event$restaurant.id == temp.rest.list[i]])[1:(length(cumsum(merge.agg.reviews.sumofrating.event$sum.of.rating[merge.agg.reviews.sumofrating.event$restaurant.id == temp.rest.list[i]])) - 1)])
  } else {
    merge.agg.reviews.sumofrating.event$cum.sum.of.rating[merge.agg.reviews.sumofrating.event$restaurant.id == temp.rest.list[i]] <- 0
    
  }
  
  print(paste0("processing ",i))
}

# calculate the cumulative average rating 
merge.agg.reviews.sumofrating.event$cum.avg.rating <- merge.agg.reviews.sumofrating.event$cum.sum.of.rating / merge.agg.reviews.sumofrating.event$cum.no.of.reviews
# there will NAN due 0/0 condition while computing cumulative average by week
# replacing NAN with zero
merge.agg.reviews.sumofrating.event$cum.avg.rating[is.nan(merge.agg.reviews.sumofrating.event$cum.avg.rating)] <- 0

# #################################################################################################################################

# #########################################################################################
# Merge the individual aggregated data frame

# merge cumulative sum, number and average of reviews and number of days and weeks from the start and event dates
merge.agg.reviews.sumofrating.event.list <- merge(merge.agg.reviews.sumofrating.event,dates.week.year,
                                                  by = c("week","year"), all.x = T)

# binary column for indicating if the week is before or after the event happened 
merge.agg.reviews.sumofrating.event.list$event.binary <- ifelse(merge.agg.reviews.sumofrating.event.list$no.of.weeks.from.event > 0,1,0)

# merge "votes" and "cumulative sum, number and average of reviews and number of days and weeks from the start and event dates" by week
merge.agg.reviews.sumofrating.votes <- merge(merge.use.cool.funny,merge.agg.reviews.sumofrating.event.list,by = c("restaurant.id","week","year"),all.x = T)

# merge "cumulative sum, number and average of reviews and number of days and weeks from the start and event dates" and "review length" by week
merge.agg.reviews.sumofrating.votes.reviews.length <- merge(merge.agg.reviews.sumofrating.votes,merge.review.length.sum.mean.sd,by = c("restaurant.id","week","year"),all.x = T)

# adding std errors for review short and full text
# formula : standard deviation(review lengths of a week) / square root of number of reviews for the week
merge.agg.reviews.sumofrating.votes.reviews.length$std.error.mean.length.review.text.short <- merge.agg.reviews.sumofrating.votes.reviews.length$sd.length.review.text.short / sqrt(merge.agg.reviews.sumofrating.votes.reviews.length$no.of.reviews)
merge.agg.reviews.sumofrating.votes.reviews.length$std.error.mean.length.review.text.full <- merge.agg.reviews.sumofrating.votes.reviews.length$sd.length.review.text.full / sqrt(merge.agg.reviews.sumofrating.votes.reviews.length$no.of.reviews)

# removing the standard deviation columns
merge.agg.reviews.sumofrating.votes.reviews.length$sd.length.review.text.short <- NULL
merge.agg.reviews.sumofrating.votes.reviews.length$sd.length.review.text.full <- NULL

# #########################################################################################

# #########################################################################################
# to create a indicator varibles 
# 1) seattle.memphis.indicator - to indicate if a restaurant is from seattle or memphis
#    seattle - 0
#    memphis - 1
# 2) sea.mem.memsub.indicator - to indicate if a restaurant is from seattle city, memphis city or memphis suburbs
#    seattle city - 0
#    memphis city - 1
#    memphis suburbs - 2

# get unique restaurants from reviews file
reviews.city <- unique(read.reviews.final.merge[c("restaurant.id","city")])

# binary column to indicate seattle or memphis
reviews.city$seattle.memphis.indicator <- ifelse(reviews.city$city == "Seattle-City",0,1)

# adding a categorical column to indicate seattle - 0 ; memphis - 1 and memphis suburbs - 2
reviews.city$sea.mem.memsub.indicator <- reviews.city$city
reviews.city$sea.mem.memsub.indicator[reviews.city$sea.mem.memsub.ind == "Seattle-City"] <- 0
reviews.city$sea.mem.memsub.indicator[reviews.city$sea.mem.memsub.ind == "Memphis-City"] <- 1
reviews.city$sea.mem.memsub.indicator[reviews.city$sea.mem.memsub.ind == "Memphis-Suburbs"] <- 2

# ########################################################################################################

# filtering price symbol, maximum and minimum price from  business price file
index.price.range <- which(colnames(business.price.range) == "price.range")
business.price.range <- business.price.range[,-c(index.price.range)]
# there will be NA's in business price file; there is no price data for some businesses 
# replacing the the NA's with zero 
business.price.range$price.range.min[business.price.range$price.range.min] <- 0
business.price.range$price.range.max[business.price.range$price.range.max] <- 0

# merge price data and city indicators variables by restaurant
merge.temp <- merge(reviews.city,business.price.range,by.x = "restaurant.id",by.y = "id",all.x = T)

# ########################################################################################################
# Creating indicator variables from business file 
# Adding a column - category.config indicator (mexican - 1 and non-mexican - 0)
business.master.memphis.seattlecity$category.indicator <- ifelse((grepl(category.config,business.master.memphis.seattlecity$categories)),1,0)
# adding delis indicator column - deli category - 1 and non-deli category - 0
category.delis <- tolower(category.delis)
business.master.memphis.seattlecity$category.delis <- ifelse((grepl(category.delis,business.master.memphis.seattlecity$categories)),1,0)
index.b1 <- which((colnames(business.master.memphis.seattlecity) == "id"))
index.b2 <- which((colnames(business.master.memphis.seattlecity) == "location.postal_code"))
index.b3 <- which((colnames(business.master.memphis.seattlecity) == "location.neighborhoods"))
index.b4 <- which((colnames(business.master.memphis.seattlecity) == "category.indicator"))
index.b5 <- which((colnames(business.master.memphis.seattlecity) == "category.delis"))
business.master.memphis.seattlecity <- business.master.memphis.seattlecity[, c(index.b1,index.b2,index.b3,index.b4,index.b5)]
# there will be blanks in location.neighborhoods fields, as there is no neighborhood data for 
# business some businesses in the input business file 

# ######################################################################################################################
# merge the data frames created above 

# merge "price data and city indicators variables" and "busines price data" by restaurant
merge.temp1 <- merge(merge.temp,business.master.memphis.seattlecity,by.x = "restaurant.id", by.y = "id",all.x = T)

# merge the below two data frame by restaurant ID
# 1) price data, city indicators variables, busines price data
# 2) cumulative sum, number and average of reviews and number of days and weeks from the start and event dates and review length
merge.temp2 <- merge(merge.temp1,merge.agg.reviews.sumofrating.votes.reviews.length, by = c("restaurant.id"),all.x = T)

# merge the below data frame by week
# 1) cumulative sum, number and average of reviews, number of days and weeks from the start and event dates,  review length
#    price data, city indicators variables, busines price data
# 2) maximum,minimum,average and standard deviation of review rating by week
merge.final <- merge(merge.temp2,merge.agg.min.max.avg.sd,by = c("restaurant.id","week","year"),all.x = T)

# add standard error for avg rating 
merge.final$std.error.avg.rating <- merge.final$sd.rating / sqrt(merge.final$no.of.reviews)

# remove the sd.rating 
merge.final$sd.rating <- NULL

# ######################################################################################################################
# weather data aggreagation by week
library(plyr)

#load("weather_data.RData")
load("weather_data_3_26.RData")

# new columns - extract week and year
weather.data$year <- strftime(weather.data$DATE,format = "%Y")
weather.data$week <- strftime(weather.data$DATE,format = "%W")

weather.data$year <- as.numeric(weather.data$year)
weather.data$week <- as.numeric(weather.data$week)

# creating a new column for indicating zero week
weather.data$week.zero.indicator <- ifelse(weather.data$week == 0,1,0)
weather.unique.year <- sort(unique(weather.data$year))
# class(unique.year)
# class(read.reviews.final.merge$week)

# removing the zero week records
rm.weather.records.index <- which(weather.data$week == 0 & weather.data$year == min(weather.data$year))
weather.data <- weather.data[-rm.weather.records.index,]

for(i in 2:length(weather.unique.year)){
  
  weather.check.week.exists <- weather.data[weather.data$year == weather.unique.year[i] & weather.data$week == 0,]
  if(nrow(weather.check.week.exists) > 0){
    # read.reviews.final.merge$week.zero.indicator <- 
    weather.data$week[weather.data$year == weather.unique.year[i] & weather.data$week.zero.indicator == 1] <- max(weather.data$week[weather.data$year == (weather.unique.year[i] - 1)])
    weather.data$year[weather.data$year == weather.unique.year[i] & weather.data$week.zero.indicator == 1] <- (weather.unique.year[i] - 1)
    
  }
  
  weather.check.week.exists <- NULL
  
}

# removing the week.zero.indicator column 
weather.data$week.zero.indicator <- NULL

weather.data <- weather.data[(weather.data$DATE > start.date &
                                weather.data$DATE < end.date),]

# ##################################################################################################
# creating new variable city in weather.data to match the city names with in the reviews file 
# reviews file has city names as Seattle-City, Memphis-City and Memphis-Suburbs

weather.data$city[weather.data$CITY == "Seattle"] = "Seattle-City"
weather.data$city[weather.data$CITY == "Memphis"] = "Memphis-City"

# duplicate memphis weather data for memphis suburbs  
temp.memphis.suburb <- weather.data[weather.data$CITY == "Memphis",]
temp.memphis.suburb$city[temp.memphis.suburb$CITY == "Memphis"] = "Memphis-Suburbs"

# combine duplicated memphis suburbs weather data with seattle and memphis weather data 
weather.data <- rbind(weather.data, temp.memphis.suburb)

# ##################################################################################################

# Compute daily average weather
weather.data$DailyAvWeather <- rowMeans(cbind(weather.data$TMAX, weather.data$TMIN))

# Below Freezing
# we assume a day below freezing if the average temperature is below 32
weather.data$BelowFreezingDay <- ifelse(weather.data$TMIN < 32, 1, 0)

# Rain Days
# we consider a day rainy, if the precipitation is greater than zero
weather.data$RainDay <- ifelse(weather.data$PRCP > 0, 1, 0)

# replace zero if na is found in snow and snow depth input weather data
weather.data$SNOW[is.na(weather.data$SNOW)] <- 0
weather.data$SNWD[is.na(weather.data$SNWD)] <- 0

# Snow Day
weather.data$SnowDay <- ifelse(weather.data$SNOW > 0, 1, 0)

# Snow Depth Day
# set snow depth indicator to 1, if snow depth is greater than 1
weather.data$SnowDepthDay <- ifelse(weather.data$SNWD > 0, 1, 0)

# Count metrics from Weather data
# weather.data.count <- unique(weather.data[c("DATE","city", "short.review.date", "BelowFreezingDay", "RainDay", "SnowDay", "SnowDepthDay")])
weather.data.count <- unique(weather.data[c("DATE","city", "year","week", "BelowFreezingDay", "RainDay", "SnowDay", "SnowDepthDay")])

# http://stackoverflow.com/questions/10787640/ddply-summarize-for-repeating-same-statistical-function-across-large-number-of
weather.data.count.summarize <- ddply(weather.data.count, 
                                      c("city", "year","week"),  numcolwise(sum))
# replace na with zero if na is produced as a result of aggregation
weather.data.count.summarize$BelowFreezingDay[is.na(weather.data.count.summarize$BelowFreezingDay)] <- 0
weather.data.count.summarize$RainDay[is.na(weather.data.count.summarize$RainDay)] <- 0
weather.data.count.summarize$SnowDay[is.na(weather.data.count.summarize$SnowDay)] <- 0
weather.data.count.summarize$SnowDepthDay[is.na(weather.data.count.summarize$SnowDepthDay)] <- 0

# Aggregate weather (temp) by weekly average
AvWeather.week <- ddply(weather.data , c("city", "year","week"), 
                        summarise, WeekAvTemp = mean(DailyAvWeather))
# replace na with zero if na is produced as a result of aggregation
AvWeather.week$WeekAvTemp[is.na(AvWeather.week$WeekAvTemp)] <- 0

# Aggregated standard deviation of  weather (temp) by weekly average
SDWeather.week <- ddply(weather.data , c("city", "year","week"), 
                        summarise, WeekSDTemp = sd(DailyAvWeather))
# Replacing the expected NA's with zero 
# We get NA for a single record for a week
SDWeather.week$WeekSDTemp[is.na(SDWeather.week$WeekSDTemp)] <- 0

# Number of days in a week
DaysInWeek <- ddply(weather.data , c("city", "year","week"), 
                    summarise, DaysINWeek = length(DATE))

# merge mean,sd,days in week
merge.weather.mean.sd <- merge(AvWeather.week,SDWeather.week,by = c("city", "year","week"), all.x = T) 
merge.weather.mean.sd.daysinweek <- merge(merge.weather.mean.sd,DaysInWeek,by = c("city", "year","week"), all.x = T) 

# calculating the standard error for 
merge.weather.mean.sd.daysinweek$std.error.WeekAvTemp <- merge.weather.mean.sd.daysinweek$WeekSDTemp / sqrt(merge.weather.mean.sd.daysinweek$DaysINWeek)

# remowing the columns sd and days in a week
merge.weather.mean.sd.daysinweek$WeekSDTemp <- NULL
merge.weather.mean.sd.daysinweek$DaysINWeek <- NULL

Weather.Data.Combine <- merge(x = merge.weather.mean.sd.daysinweek,
                              y = weather.data.count.summarize, by = c("city", "year","week"), all.x = TRUE)

# colnames(Weather.Data.Combine)[2] <- "week"
colnames(Weather.Data.Combine) <- c("city","year","week","WeekAvTemp","std.error.WeekAvTemp","WeeklyBelowFreezingDay","WeeklyRainDay","WeeklySnowDay","WeeklySnowDepthDay")
Weather.Data.Combine$week <- as.numeric(Weather.Data.Combine$week)

####################################################################################################################################

# merge "combined weekly aggregated review and business data" and "weather data" by week
merge.final.weather <- merge(merge.final,Weather.Data.Combine,by = c("city", "week","year"), all.x = TRUE)

# merge "combined weekly aggregated review and business data, weather data" and "aggregated business data
# at zipcode level" by zipcode 
merge.final.weather.zipcode <- merge(merge.final.weather,merge.zipcode.rest.cat,by.x = "location.postal_code",
                                     by.y = "zipcodes",all.x = T)

# ######################################################################################################################

# read the demography data
demography.data <- read.csv("Demography_data.csv", header = T,sep = ",")
# colnames(demography.data)
demography.data <- demography.data[c(1,4,6,14,48,82,188,190,192)]

# merge all data with demography data
merge.final.weather.zipcode.demography <- merge(merge.final.weather.zipcode,demography.data,
                                                by = "location.postal_code", all.x = T)

save(file = "rest.week.zip.demo.RData",merge.final.weather.zipcode.demography)
write.csv(merge.final.weather.zipcode.demography, file = "rest.week.zip.demo.csv",row.names = F)