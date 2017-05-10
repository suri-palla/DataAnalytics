# Yelp - Business Owner Response (BOR) - Similarity Matrix - restaurant wise
# finding out mean 

# clear the workspace
rm(list = ls())

# set working directory 
setwd("C:/Surendra/Courses/Business Analytics/Yelp/Business Owner Respone - Similarity Index")

# loads the text mining package
library(tm)

# Configurations
num.index <- 10

# ------------------
# Functions
# ------------------


# --- START --------------
# PRE-PROCESS BOR REVIEWS
# ------------------------

bor.preprocess <- function(bor.vectors){
  
  # -------------------------------------------------------
  # creating a corpus for BOR
  # -------------------------------------------------------
  
  # creating corpus for text processing 
  bor.corpus <- Corpus(VectorSource(bor.vectors))
  
  # converting into lowercase
  bor.corpus <- tm_map(bor.corpus,content_transformer(tolower))
  
  # removing punctuations
  bor.corpus <-tm_map(bor.corpus,removePunctuation)
  
  # removing all the stopwords
  bor.corpus <- tm_map(bor.corpus, removeWords, c(stopwords("english")))
  
  # ---------------------------------------------------------------------------
  return(bor.corpus)
  
}

# --- END ----------------
# PRE-PROCESS BOR REVIEWS
# ------------------------

# ------------------------------------------------------------------------------

# --- START ---------
# COSINE SIMILARITY
# -------------------

calculate.cosine.similarity <- function(tdm){
  
  # CALCULATES THE COSINE SIMILARITY MATRIX
  library(slam)
  cosine_dist_mat <- crossprod_simple_triplet_matrix(tdm)/(sqrt(col_sums(tdm^2) %*% t(col_sums(tdm^2))))
  
  # Making all the diagonals elements Zero
  
  for(i in 1:ncol(cosine_dist_mat)){
    cosine_dist_mat[i,i] <- 0
  }
  
  return(cosine_dist_mat)
}

# --- END -----------
# COSINE SIMILARITY
# -------------------

# # ---------------------- START -----------------------------
# # -------------- GET THE SIMILAR CASES ---------------------
# # ----------------------------------------------------------
# 
extract.sim.index.value <- function(cosine_dist_mat,matrix.similar,bor.out.ids){
  
  # FORMS A MATRIX OF N SIMILAR VALUES AND CORRESPONDING SIMILARITY INDEX
  k <- 0
  
  for(j in 1:ncol(cosine_dist_mat)){
    
    # GET THE HIGHEST SIMILARITY INDEX VALUES AND CORRESPONDING INDEX RECORD
    # large.values <- tail(sort(cosine_dist_mat[j,]),num.index)
    large.values <- sort(cosine_dist_mat[j,],decreasing = T)
    large.values <- large.values[large.values >= 0.75]
    if(length(large.values) == 0){
      matrix.similar[j,] <- 0
    } else {
      
      # number of values greater than cut-off similarity index
      k <- k + 1
      matrix.similar[j,k] <- length(large.values)
      
      for(jj in 1:length(large.values)){
        
        # GET THE SIMILARITY INDEX RECORDS ALONE 
        file.index <- names(large.values)
        
        # MOVE THE VALUES INTO MATRIX
        k <- k + 1
        matrix.similar[j,k] <- large.values[jj]
        k <- k + 1
        matrix.similar[j,k] <- bor.out.ids[as.numeric(file.index[jj])]
        k <- k + 1
        matrix.similar[j,k] <- file.index[jj]
        
        print(paste0(i," ",j," ",jj))
        
      } # jj - loop
      
    } # if - loop
    
    large.values <- ""
    k <- 0
    
  } # for loop j 
  
  return(matrix.similar)
  
}

# ---------------------- END -------------------------------
# -------------- GET THE SIMILAR CASES ---------------------
# ----------------------------------------------------------


# --------------------------------------------------

# loading the yelp reviews file
load("yelp.reviews.RData")

# filter the business owner respones alone 
bor.reviews <- read.reviews.final.merge[read.reviews.final.merge$review.comment == "Business Owner Comment",]
bor.reviews$review.text <- as.character(bor.reviews$review.text)
bor.reviews$review.unique.id <- as.character(bor.reviews$review.unique.id)

# order the file by restaurant
bor.reviews <- bor.reviews[order(bor.reviews$restaurant.id),]

# removing full reviews full
rm(read.reviews.final.merge)

# -------------------------------------------------------
# aggregating number of reviews per restaurant
# -------------------------------------------------------
agg.no.of.reviews <- aggregate(bor.reviews$review.unique.id,
                               by = list(bor.reviews$restaurant.id),
                               FUN = length)

# merge agg number of reviews and bor reviews
# add number of bor reviews per business to the bor reviews
bor.reviews <- merge(bor.reviews,agg.no.of.reviews,by.x = "restaurant.id",by.y = "Group.1")

# remove restaurants with only one response 
bor.reviews <- bor.reviews[bor.reviews$x != 1,]

# initializing the matrix
bor.similar <- matrix(nrow = nrow(bor.reviews), ncol = (max(agg.no.of.reviews$x) * 3) + 1)

# remove the agg no of reviews
# rm(agg.no.of.reviews)

# filter the columns to create a output file 
bor.out <- bor.reviews[,c("review.unique.id","restaurant.id")]

# get the unique restaurants
unique.rest <- unique(bor.reviews$restaurant.id)

# initialize counter
count = 0

for(i in 1:length(unique(bor.reviews$restaurant.id))){
  
  bor.rest.vector <- bor.reviews$review.text[bor.reviews$restaurant.id == unique.rest[i]]
  
  bor.corpus <- bor.preprocess(bor.rest.vector)
  
  # Form a term document matrix
  bor.tdm <- TermDocumentMatrix(bor.corpus)
  
  # converting the tdm in to a matrix
  bor.matrix <- as.matrix(bor.tdm)
  
  # form a cosine similarity matrix
  cosine_dist_mat <- calculate.cosine.similarity(bor.tdm)
  
  # get the bor reviews ids
  bor.out.ids <- bor.reviews$review.unique.id[bor.reviews$restaurant.id == unique.rest[i]]
  
  # # data frame
  # if(i == 1){
  #   
  #   bor.stats <- as.data.frame(bor.out.ids)
  #   colnames(bor.stats) <- c("bor.ids")
  #   bor.stats$number <- length(cosine_dist_mat[i,])
  #   bor.stats$mean <- mean(cosine_dist_mat[i,])
  #   
  # } else {
  #   
  #   # temp <- bor.stats
  #   bor.stats$number <- length(cosine_dist_mat[i,])
  #   bor.stats$mean <- mean(cosine_dist_mat[i,])
  #   
  # }
  # 
  
#   # extract similarity index and value
#   # 1 - number values greater than similarity index
#   matrix.similar <- matrix(nrow = nrow(cosine_dist_mat), ncol = (nrow(cosine_dist_mat) * 3) + 1)
#   sim.index.value <- extract.sim.index.value(cosine_dist_mat,matrix.similar,bor.out.ids)
#   
#   
#   fill.zeros <- rep.int(0,(max(agg.no.of.reviews$x) - ncol(cosine_dist_mat)))   
#   for(j in 1:nrow(cosine_dist_mat)){
#     
#     count = count + 1
#     bor.similar[count,] <- c(sim.index.value[j,],fill.zeros,fill.zeros,fill.zeros)
#     
# }
  
}


final.output <- data.frame(bor.out,bor.similar)
final.output$X1 <- as.numeric(levels(final.output$X1))[final.output$X1]
final.output$X3 <- as.character(final.output$X3)

agg.final.output <- aggregate(final.output$X1,by = list(final.output$restaurant.id),
                              FUN = sum)

write.csv(file = "bor.similar.rest.filter.selected.csv",final.output)
write.csv(file = "bor.similar.rest.filter.selected.agg.csv",agg.final.output)





# ---------------------------------------------------------------------------
# Reports
temp <- bor.reviews$review.text[bor.reviews$restaurant.id == "anthonys-pier-66-seattle-3"]
write.csv(file = "anthonys-pier-66-seattle-3.csv",temp)

temp <- bor.reviews$review.text[bor.reviews$restaurant.id == "nanas-family-mexican-kitchen-and-cantina-seattle"]
write.csv(file = "anthonys-pier-66-seattle-3.csv",temp)

report.temp <- final.output[final.output$X1 != 0,]
report.temp1 <- report.temp[,c(1,2,3,4,5)]


report.temp1$act.text <- ""
report.temp1$sim.text <- ""

for(i in 1:nrow(report.temp1)){
  
  report.temp1$act.text[i] <- bor.reviews$review.text[bor.reviews$review.unique.id == report.temp1$review.unique.id[i]]
  report.temp1$sim.text[i] <- bor.reviews$review.text[bor.reviews$review.unique.id == report.temp1$X3[i]]
  
}

write.csv(file = "bor.similar.text.csv",report.temp1)
