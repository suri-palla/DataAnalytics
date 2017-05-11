# -----------------------------------------------------------------------
# ---------- PROGRAM TO GENERATE SIMILARITY INDEX BETWEEN FILES ---------
# -----------------------------------------------------------------------

# SETWD FOR THE PROGRAM
setwd("C:/Surendra/Courses/Business Analytics/F")

# CLEAR ALL THE IN MEMORY VARIABLES
rm(list = ls())

# LOAD THE REQUIRED LIBRARIES
library(tm)
library(reshape2) 
library(ggplot2)

# -------- CONFIGURATION SETTINGS -------------------------------------------------
# num.index --> to get the number of similar test cases
# file.name.grt --> GRT input file name
# file.name.GTM --> GTM input file name
# num.index --> Number of similar test cases to extract
# file.location.grt --> File location of the GRT File
# file.location.gtm --> File location of the GTM file
# file.location.outputs --> Location of the Output Files
# drop.columns --> Enter the attributes to exclude in comparing similar records
# info --> Initializing with NULL; used for writing the output
# report.index --> Initializing with 0; used for writing the output
#                  
# 
# ENTER Y OR N FOR THE FOLLOWING FIELDS
# 
# create.grt.similar.file  --> Y WILL CREATE A GRT SIMILARITY FILE ALONE
# create.gtm.similar.file  --> Y WILL CREATE A GTM SIMILARITY FILE ALONE
# cross.grt.gtm.file       --> Y WILL CREATE A CROSS GRT & GTM SIMILARITY FILE
# single.grt.gtm.file      --> Y WILL CREATE A COMBINED GRT & GTM SIMILARITY FILE

# --------------------------------------------------------------------------------
num.index <- 8
file.name.grt <- "CL1601C5_L3RFXRS0000ILGD_BL_DATA"
file.name.gtm <- "CL1601C5_L3BFXRS0000ILEP_BL_DATA"
file.location.grt <- "C:/Surendra/Courses/Business Analytics/F/ppt data 2"
file.location.gtm <- "C:/Surendra/Courses/Business Analytics/F/ppt data 2"
file.location.outputs <- "C:/Surendra/Courses/Business Analytics/F/final 8"
drop.columns <- c("TEST_ID","TEST_ID_CHAR")
create.grt.similar.file <- "Y"
create.gtm.similar.file <- "Y"
cross.grt.gtm.file <- "Y"
single.grt.gtm.file <- "Y"
info <- list()
report.index <- 0
# -----------------------------------------------------

# -----------------------------------------------------
# --------------- FUNCTIONS ---------------------------
# -----------------------------------------------------


# ------------- START -------------------
# FORM TERM DOCUMENT MATRIX OF THE INPUT
# ---------------------------------------

form.tdm <- function(read.test.temp){
  
  for(jj in 1:ncol(read.test.temp)){
    
    # ATTACHING COLUMN NAMES TO THE DATA
    read.test.temp[,jj] <- paste0(read.test.temp[,jj],"_",colnames(read.test.temp[jj]))
    
    # CLEANING THE DATA COLUMN WISE
    read.test.temp[,jj] <- gsub(","," ",read.test.temp[,jj])
    read.test.temp[,jj] <- gsub("[\f\n\r\t\v]"," ",read.test.temp[,jj])
    read.test.temp[,jj] <- gsub(" ","_",read.test.temp[,jj])
  }
  
  # COLLAPSING ALL THE COLUMNS INTO A SINGLE COLUMN
  read.test.temp$test <- ""
  for(j in 1:nrow(read.test.temp)){
    read.test.temp$test[j] <- paste(read.test.temp[j,],collapse = ",")
  }
  read.test.temp <- data.frame(read.test.temp$test)
  
  # NAMING THE SINGLE COLUMN 
  colnames(read.test.temp) <- "test"
  
  # CREATING INDIVIDUAL WORDS FROM A SINGLE TEST CASE SEPARATED BY SPACE
  read.test.temp <- gsub(","," ",read.test.temp$test)
  
  # CREATING INPUT CORPUS
  input.corpus <- read.test.temp
  input.corpus <- Corpus(VectorSource(input.corpus))
  
  # FORM TERM DOC MATRIX INCLUDING SINGLE LENGTH CHARACTERS
  # wordLengths is used to include single length characters as well
  term.doc.matrix <- TermDocumentMatrix(input.corpus,control=list(wordLengths=c(1,Inf)))

  # inspect(term.doc.matrix)
  tdm <- term.doc.matrix
  
  return(tdm)
  
}

# --------------- END -------------------
# FORM TERM DOCUMENT MATRIX OF THE INPUT
# ---------------------------------------

# ------------ START ---------------------
# CREATE A SINGLE GRT/GTM FILE
# ----------------------------------------

grt.gtm.file <- function(read.test,num.index,file.name.grt.input,indicator){
  
  # DROP THE UNWANTED COLUMNS TO BE CONSIDERED IN SIMILARITY MATRIX
  read.test.temp <- read.test[,!names(read.test) %in% drop.columns]
  
  # CALL THE FUNCTIONS TO FORM TERM DOCUMENT MATRIX (TDM) AND
  # COSINE SIMILARITY MATRIX (CSM)
  tdm <- form.tdm(read.test.temp)
  tdm.v <- inspect(tdm)
  cosine_dist_mat <- calculate.cosine.similarity(tdm)
  
  # ----------------------------------------------------------------
  # NCOL = num.index * 4
  # THE FOUR IS FOR FORMING MATRIX WITH FOLLOWING ATTRIBUTES
  #     1. Similarity index 
  #     2. index record
  #     3. difference in the number of attributes
  #     4. number of common attributes
  # ----------------------------------------------------------------
  
  # forming the matrix 
  matrix.similar <- matrix(nrow = nrow(read.test), ncol = (num.index * 4))
  
  
  # FORMS A MATRIX OF N SIMILAR VALUES AND CORRESPONDING SIMILARITY INDEX
  k <- 0
  
  for(j in 1:ncol(cosine_dist_mat)){
    
    # GET THE HIGHEST SIMILARITY INDEX VALUES AND CORRESPONDING INDEX RECORD
    large.values <- tail(sort(cosine_dist_mat[j,]),num.index)
    for(jj in length(large.values):1){
      
      # GET THE SIMILARITY INDEX RECORDS ALONE  
      file.index <- names(large.values)
      
      # MOVE THE VALUES INTO MATRIX
      k <- k + 1
      matrix.similar[j,k] <- large.values[jj]
      k <- k + 1
      matrix.similar[j,k] <- file.index[jj]
      
      # ----------------------------------------------------
      # RETREIVING NUMBER OF DIFFERENT FEILDS IN A TEST CASE
      # ----------------------------------------------------
      
      doc1 <- tdm.v[,j]
      doc2 <- tdm.v[,file.index[jj]]
      diff.doc <- doc1 - doc2
      abs.diff.doc<- (abs(diff.doc) / 2)
      k <- k + 1
      matrix.similar[j,k] <- sum(abs.diff.doc)
      
      # --------------------------
      # GET THE COMMON ATTRIBUTES
      # --------------------------
      doc1.c <- tdm.v[,j]
      doc2.c <- tdm.v[,file.index[jj]]
      k <- k + 1
      matrix.similar[j,k] <- sum(doc1.c * doc2.c)
      
    }
    
    large.values <- ""
    k <- 0
  }
  
  
  matrix.similar.df <- as.data.frame(matrix.similar)
  
  # ATTACH THE SIMILARITY INDEX VALUES TO THE ORIGINAL DATA
  read.test.write <- data.frame(read.test,matrix.similar.df)
  
  creat.heat.map(matrix.similar,num.index,file.name.grt.input,read.test.write)
  
  # build the output file name 
  file.name.output <- paste0(file.name.grt.input,"_OUTPUT.csv")
  # set working directory for output files
  setwd(file.location.outputs)
  # write output file
  write.csv(read.test.write,file = file.name.output,row.names = T)
  
  
  # --------------------------------------------
  # REPORT - GET THE HIGHEST SIMILARITY INDEX
  # --------------------------------------------
  
  get.highest.index <- max(matrix.similar[,1])
  
  if(indicator == "grt"){
    grt.filename = file.name.grt.input
    gtm.filename = "NA"
    max.grt = get.highest.index
    max.gtm = "NA"
    max.cross.gtm = "NA"
    max.cross.grt = "NA"
    combined.grt.gtm = "NA"
  }
  
  if(indicator == "gtm"){
    grt.filename = "NA"
    gtm.filename = file.name.grt.input
    max.grt = "NA"
    max.gtm = get.highest.index
    max.cross.gtm = "NA"
    max.cross.grt = "NA"
    combined.grt.gtm = "NA"
  }
  
  
  report.index = report.index + 1
  #     info[[attributes.index]] <- list(user.attribute = paste0("user.",user.rating.table.temp$user.rating.table.temp.X1[y]),
  info[[report.index]] <- list(grt.filename = grt.filename,
                               gtm.filename = gtm.filename,
                               max.grt = max.grt,
                               max.gtm = max.gtm,
                               max.cross.gtm = max.cross.gtm,
                               max.cross.grt = max.cross.grt,
                               combined.grt.gtm = combined.grt.gtm)
  
  call.write.report <- write.report.file(info[[report.index]])
  
  
}



# --------------------------------------------------------- 
# ------ CROSS GTM GRT SIMILARITY MATRIX ------------------
# ---------------------------------------------------------

cross.gtm.grt.similarity.mat <- function(cosine_dist_mat,tdm.v,grt.df,merge.gtm.grt) {
  
  
  large.values <- ""
  k <- 0
  matrix.similar <- matrix(nrow = nrow(merge.gtm.grt), ncol = (num.index * 4))
  
  for(j in 1:ncol(cosine_dist_mat)){
    # get the first row of the cosine matrix
    temp <- cosine_dist_mat[j,]  
    # spliting the cosine similarity values corresponding to grt 
    grt.temp <- temp[seq(1:nrow(grt.df))]
    # spliting the cosine similarity values corresponding to grt 
    gtm.temp <- temp[seq(from = ((nrow(grt.df)) + 1), to = length(temp))]
    
    # IF loop : to find out cross similarity values between grt and gtm
    
    if(j <= nrow(grt.df)){
      # get the most similar records index and similarity index
      # i.e. get most similar gtm records for a grt record
      # sort the gtm similarity index and get the indexes of largest similarity index
      large.values <- tail(sort(gtm.temp),num.index)
      # FOR Loop : get the following for each record
      #            1) cosine similarity index value
      #            2) similar record index
      #            3) Number of different attributes between the two records
      #            4) Number of common attributes between the two records
      for(jj in length(large.values):1){
        # get all the indexes of the similar records
        file.index <- as.numeric(names(large.values)) 
        # increment the counter to assign values in the matrix
        k <- k + 1
        # move the cosine similarity value into matrix
        matrix.similar[j,k] <- large.values[jj]
        # increment the counter to assign values in the matrix
        k <- k + 1
        # cosine similarity index of the similar record
        matrix.similar[j,k] <- as.numeric(file.index[jj])
        
        # ----------------------------
        # GET THE DIFFERENT ATTRIBUTES
        # ----------------------------
        # get the jth record vector indicating the words containing in the record
        doc1 <- tdm.v[,j]
        # get the similarity record vector indicating the words containing in the record
        doc2 <- tdm.v[,file.index[jj]]
        # taking the difference 
        diff.doc <- doc1 - doc2
        # finding the absolute difference 
        abs.diff.doc<- (abs(diff.doc) / 2)
        k <- k + 1
        # sum of the absolute difference -  results in difference number of words between the records
        matrix.similar[j,k] <- sum(abs.diff.doc)
        
        # --------------------------
        # GET THE COMMON ATTRIBUTES
        # --------------------------
        doc1.c <- tdm.v[,j]
        doc2.c <- tdm.v[,file.index[jj]]
        k <- k + 1
        # sum of the product of doc1.c and doc2.s - results in number of common attributes between the records
        matrix.similar[j,k] <- sum(doc1.c * doc2.c)
        
      } 
      
    } else {
      
      # else loop : get most similar grt records for a gmt record
      large.values <- tail(sort(grt.temp),num.index)
      for(jj in length(large.values):1){
        file.index <- names(large.values)
        k <- k + 1
        matrix.similar[j,k] <- large.values[jj]
        k <- k + 1
        matrix.similar[j,k] <- as.numeric(file.index[jj])
        
        doc1 <- tdm.v[,j]
        doc2 <- tdm.v[,file.index[jj]]
        diff.doc <- doc1 - doc2
        abs.diff.doc<- (abs(diff.doc) / 2)
        k <- k + 1
        matrix.similar[j,k] <- sum(abs.diff.doc)
        
        # --------------------------
        # GET THE COMMON ATTRIBUTES
        # --------------------------
        doc1.c <- tdm.v[,j]
        doc2.c <- tdm.v[,file.index[jj]]
        k <- k + 1
        matrix.similar[j,k] <- sum(doc1.c * doc2.c)
        
      }
      
      
    }  # ELSE
    
    large.values <- ""
    k <- 0
    
    # }  # IF
    
  }  # FOR
  
  return(matrix.similar)
  
}

# -------------------------------------------------------------------- 
# ------ END OF FUNCTION : cROSS GTM GRT SIMILARITY MATRIX -----------
# --------------------------------------------------------------------


# --------------------------------------------------------- 
# ------ COMBINED GTM GRT SIMILARITY MATRIX ---------------
# ---------------------------------------------------------


combined.grt.gtm.matrix <- function(cosine_dist_mat,tdm.v,merge.gtm.grt){
  
  # matrix.similar <- matrix(nrow = nrow(merge.gtm.grt), ncol = (num.index * 2) + num.index)
  matrix.similar <- matrix(nrow = nrow(merge.gtm.grt), ncol = (num.index * 4))
  large.values <- ""
  k <- 0
  # FORMS A MATRIX OF N SIMILAR VALUES AND CORRESPONDING SIMILARITY INDEX
  
  for(j in 1:ncol(cosine_dist_mat)){
    
    large.values <- tail(sort(cosine_dist_mat[j,]),num.index)
    for(jj in length(large.values):1){
      file.index <- names(large.values)
      k <- k + 1
      matrix.similar[j,k] <- large.values[jj]
      k <- k + 1
      matrix.similar[j,k] <- file.index[jj]
      
      doc1 <- tdm.v[,j]
      doc2 <- tdm.v[,file.index[jj]]
      diff.doc <- doc1 - doc2
      abs.diff.doc<- (abs(diff.doc) / 2)
      k <- k + 1
      matrix.similar[j,k] <- sum(abs.diff.doc)
      
      # --------------------------
      # GET THE COMMON ATTRIBUTES
      # --------------------------
      doc1.c <- tdm.v[,j]
      doc2.c <- tdm.v[,file.index[jj]]
      k <- k + 1
      matrix.similar[j,k] <- sum(doc1.c * doc2.c)
      
    }
    
    #   matrix.similar[j,jj] <- write.index[jj]
    #   matrix.similar[j,(jj+1)] <- file.index
    
    #   write.value <- ""
    #   write.index <- ""
    large.values <- ""
    k <- 0
  }
  
  return(matrix.similar)
  
}

# -------------------------------------------------------------------------- 
# ------END OF FUNCTION : COMBINED GTM GRT SIMILARITY MATRIX ---------------
# --------------------------------------------------------------------------

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

# --- START ---------
# CREAT HEAT MAP
# -------------------

creat.heat.map <- function(matrix.similar,num.index,file.name.heat.map,read.test.write){
  
  # ----------------------------------------
  # PROCESSING FOR PLOTTING SIMILARITY INDEX
  # ----------------------------------------
  # plotting all the values into heatmap was not clear, the test id's were overlapping with each other
  # so here we plot only the first 30 test cases 
  
  # get the cosine similarity columns from the matrix
  heat.temp.df <- matrix.similar[,c(seq(from = 1, to = (4 * num.index), by = 4))]
  # converting matrix to mumeric class
  class(heat.temp.df) <- "numeric"
  # extracting only 30 rows from the matrix
  heat.temp.df <- heat.temp.df[seq(1,30),]
  # creating transpose matrix
  dat.sm <- t(heat.temp.df)
  # melt creates a matrix for each for heatmap ; it creates a colour value for each cell
  dat2.sm <- melt(dat.sm)
  
  # ------------------------------------------
  # PROCESSING FOR PLOTTING SIMILAR TEST CASES
  # ------------------------------------------
  
  # get the test ids with respect to the index from similarity matrix
  test.id.numbers <- read.test.write$TEST_ID[(as.numeric(matrix.similar[,c(seq(from = 2, to = (4 * num.index), by = 4))]))]
  heat.temp.df.records <- matrix(test.id.numbers,ncol = num.index)
  class(heat.temp.df.records) <- "numeric"
  heat.temp.df.records <- heat.temp.df.records[seq(1,30),]
  
  dat.sr <- t(heat.temp.df.records)
  dat2.sr <- melt(dat.sr)
  
  # ------------------------------------------------------
  # PROCESSING FOR PLOTTING NUMBER OF DIFFERENT ATTRIBUTES
  # ------------------------------------------------------
  # get the number of different attributes 
  heat.temp.df.common <- matrix.similar[,c(seq(from = 4, to = (4 * num.index), by = 4))]
  class(heat.temp.df.common) <- "numeric"
  heat.temp.df.common <- heat.temp.df.common[seq(1,30),]
  
  dat.common <- t(heat.temp.df.common)
  dat2.common <- melt(dat.common)
  
  # ------------------------------------------
  # PROCESSING FOR PLOTTING COMMON ATTRIBUTES
  # ------------------------------------------
  
  heat.temp.df.attributes <- matrix.similar[,c(seq(from = 3, to = (4 * num.index), by = 4))]
  class(heat.temp.df.attributes) <- "numeric"
  heat.temp.df.attributes <- heat.temp.df.attributes[seq(1,30),]
  
  dat.diff <- t(heat.temp.df.attributes)
  dat2.diff <- melt(dat.diff)
  
  # -------------------------
  # PLOT FOR SIMILARITY INDEX
  # -------------------------
  
  filename.save = paste0(file.location.outputs,"/",file.name.heat.map,"_Similarity_Index",".png")
  
  ggplot(dat2.sm, aes(as.factor(Var1), as.factor(Var2), group=Var2)) +
    geom_tile(aes(fill = value)) + 
    # geom_text(aes(fill = dat2.sm$value, label = round(dat2.sm$value, 2))) +
    geom_text(aes(fill = dat2.sm$value, label = (trunc(dat2.sm$value * 10^2)/10^2))) +
    labs(x="Similarity Index", y="Test Records") +
    scale_fill_gradient(low = "white", high = "blue") +
    scale_y_discrete(labels=c(read.test.write$TEST_ID[seq(1,30)])) 
  
  ggsave(filename = filename.save)
  
  
  # ---------------------------
  # PLOT FOR SIMILAR TEST CASES
  # ---------------------------
  
  filename.save = paste0(file.location.outputs,"/",file.name.heat.map,"_Similar_Test_Cases",".png")
  
  ggplot(dat2.sm, aes(as.factor(Var1), as.factor(Var2), group=Var2)) +
    geom_tile(aes(fill = value)) + 
    geom_text(aes(fill = dat2.sm$value, label = round(dat2.sr$value, 2))) +
    labs(x="Similar Test Records", y="Test Records") +
    scale_fill_gradient(low = "white", high = "blue") +
    scale_y_discrete(labels=c(read.test.write$TEST_ID[seq(1,30)])) 
  
  ggsave(filename = filename.save)
  
  
  
  # -----------------------------------------------
  # PLOT FOR SIMILAR NUMBER OF DIFFERENT ATTRIBUTES
  # -----------------------------------------------
  
  
  filename.save = paste0(file.location.outputs,"/",file.name.heat.map,"_No_Different_Attributes",".png")
  
  ggplot(dat2.sm, aes(as.factor(Var1), as.factor(Var2), group=Var2)) +
    geom_tile(aes(fill = value)) + 
    geom_text(aes(fill = dat2.sm$value, label = round(dat2.diff$value, 2))) +
    labs(x="Number of Different Attributes between Test Cases", y="Test Records") +
    scale_fill_gradient(low = "white", high = "blue") +
    scale_y_discrete(labels=c(read.test.write$TEST_ID[seq(1,30)])) 
  
  ggsave(filename = filename.save)
  
  # -----------------------------------------------
  # PLOT FOR SIMILAR NUMBER OF COMMON ATTRIBUTES
  # -----------------------------------------------
  
  
  filename.save = paste0(file.location.outputs,"/",file.name.heat.map,"_Common_Attributes",".png")
  
  ggplot(dat2.sm, aes(as.factor(Var1), as.factor(Var2), group=Var2)) +
    geom_tile(aes(fill = value)) + 
    geom_text(aes(fill = dat2.sm$value, label = round(dat2.common$value, 2))) +
    labs(x="Number of Common Attributes between Test Cases", y="Test Records") +
    scale_fill_gradient(low = "white", high = "blue") +
    scale_y_discrete(labels=c(read.test.write$TEST_ID[seq(1,30)])) 
  
  ggsave(filename = filename.save)
  
  # -----------------------------------------------
  # PLOT HISTOGRAM
  # -----------------------------------------------
  
  class(matrix.similar) <- "numeric"
  
  filename.save = paste0(file.location.outputs,"/",file.name.heat.map,"_histogram",".png")
  
  png(filename = filename.save)
  hist(matrix.similar[,1],xlab = "Number of pairwise comparisons",  ylab = "Count",
       main = NULL)
  
  dev.off()
  
  filename.save = paste0(file.location.outputs,"/",file.name.heat.map,"_histogram_A",".png")
  
  hist.temp <- matrix.similar[,1]
  
  no.of.bins <- 9 
  
  max.sim <- max(hist.temp) 
  min.sim <- min(hist.temp) 
  
  
  each.bin.width <- ((max.sim - min.sim)/no.of.bins) 
  
  bin.start.value <- min.sim 
  bin.stop.value <- bin.start.value + (no.of.bins*each.bin.width) 
  
  
  hist.bins <- seq(bin.start.value, bin.stop.value, by = each.bin.width) 
  # hist(hist.temp, breaks = hist.bins, ylim = c(0,90), xlab = "Telephone Bills", ylab =
  #        "Count", col = rainbow(20), main = "Telephone Bills Distribution")
  hist.bins.p <- trunc(hist.bins*10^2)/10^2
  #   hist.bins.p <- c(0,0.36,0.38,0.40,0.42,0.44,0.46,0.48,0.50,0.51,0.53,1)
  
  
  png(filename = filename.save)
  hist(hist.temp, breaks = hist.bins, xlab = "Similarity Index", ylab =
         "Number of pairwise comparisons", col = rainbow(9), main = NULL,xaxt = 'n')
  # main = "Histogram - Similarity index of most similar test cases")
  # axis(1, at=seq(0.35,0.55, by=0.025), labels=seq(0.35,0.55, by=0.025))
  # axis(1, at=hist.bins, labels=hist.bins.p)
  axis(1, at=hist.bins.p, labels=hist.bins.p, xlim = c(0,1))
  
  
  
  
  dev.off()
  
  # ---------------------------
  # GRAPH 
  # --------------------------- 
  library(igraph) 
  
  # heat.temp.df.records <- matrix.similar[,c(seq(from = 2, to = (3 * num.index), by = 3))] 
  heat.temp.df.records <- heat.temp.df.records[seq(1,30),] 
  b <- heat.temp.df.records[,1]
  # a <- seq(1:length(heat.temp.df.records[,2])) 
  a <- read.test.write$TEST_ID[seq(1,30)]
  # a <- read.test.write$TEST_ID
  
  # a <- seq(1:30) 
  
  g.mat <- data.frame(a,b) 
  
  graph.df.test <- graph.data.frame(g.mat)
  
  filename.save = paste0(file.location.outputs,"/",file.name.heat.map,"GRAPH",".png")
  
  
  png(filename = filename.save)
  plot( graph.df.test,
        # layout = layout.reingold.tilford,
        edge.width = 1,
        edge.arrow.width = 0.3,
        vertex.size = 10,
        edge.arrow.size = 0.5,
        vertex.size2 = 3,
        vertex.label.cex = 1,
        asp = 0.4,
        margin = -0.1)
  dev.off()
  
}

# --- END -----------
# CREAT HEAT MAP
# -------------------


# -----START---------
# WRITE REPORT
# -------------------

write.report.file <- function(info)
  
{
  
  write.report.record <- paste(info[["grt.filename"]][1],
                               info[["gtm.filename"]][1],
                               info[["max.grt"]][1],
                               info[["max.gtm"]][1],
                               info[["max.cross.gtm"]][1],
                               info[["max.cross.grt"]][1],
                               info[["combined.grt.gtm"]][1],
                               sep="\t")
  
  write(write.report.record, file = "GRT_GTM_REPORT.txt", append = TRUE)
  #     print("I am in write routinee")
  
}




# -------END---------
# WRITE REPORT
# -------------------


# -- START -------------------
# MAIN PROGRAM
# ----------------------------

main.program <- function(){
  
  
  # CREATE INPUT FILE NAMES
  file.name.grt.input <- paste0(file.name.grt,".csv")
  file.name.gtm.input <- paste0(file.name.gtm,".csv")
  
  # read GRT files
  setwd(file.location.grt)
  read.grt <- read.csv(file.name.grt.input, header = T,sep = ",",stringsAsFactors = F)
  
  # read GTM files
  setwd(file.location.gtm)
  read.gtm <- read.csv(file.name.gtm.input, header = T,sep = ",",stringsAsFactors = F)
  
  # CALL THE FUNCTION - TO CREATE WITHIN FILE GRT COMPARISION
  # based on configuration setting create similarity matrix within the GRT file
  if(create.grt.similar.file == "Y"){
    # indicator to indicate a grt file 
    indicator <- "grt"
    # call the funtion grt.gtm.file to identify the similar records with the grt file
    # INPUTS TO THE FUNCTION: 
    # 1) dataframe with the records to be identify the similar records
    # 2) num.index - number of similar cases to be indetified
    # 3) input file name
    # 4) indicator to indicate - srt or gtm
    grt.gtm.file(read.grt,num.index,file.name.grt.input,indicator)
    
  }
  
  # CALL THE FUNCTION - TO CREATE WITHIN FILE GTM COMPARISION
  
  if(create.gtm.similar.file == "Y"){
    indicator = "gtm"
    grt.gtm.file(read.gtm,num.index,file.name.gtm.input,indicator)
    
  }
  
  
  # MERGE FILES - TO GENERATE SIMILARITY INDEX ACROSS FILES(GTM and GRT)
  
  if(cross.grt.gtm.file == "Y" | single.grt.gtm.file == "Y"){
    
    # get the common attributes between the files 
    common.attributes <- intersect(colnames(read.grt),colnames(read.gtm))
    # filter grt data based on the common attributes
    grt.df <- read.grt[c(common.attributes)]
    # add an index column to indicate the grt records
    grt.df$index <- "GRT"
    # filter gtm data based on the common attributes
    gtm.df <- read.gtm[c(common.attributes)]
    # add an index column to indicate the gtm records
    gtm.df$index <- "GTM"
    # combine the common atttributes from grt and gtm files 
    merge.gtm.grt <- rbind(grt.df,gtm.df)
    
    # DROP UNWANTED COLUMNS
    # drop the columns in the configuration varible drop.columns 
    read.test.temp <- merge.gtm.grt[,!names(merge.gtm.grt) %in% drop.columns]
    # drop the column index 
    read.test.temp <- read.test.temp[,!names(read.test.temp) %in% "index"]
    
    # CALL FUNTION TO FORM TERM DOCCUMENT MATRIX
    
    tdm <- form.tdm(read.test.temp)
    tdm.v <- inspect(tdm)
    
    # CALL THE FUNCTION TO CALCULATE COSINE SIMILARITY
    cosine_dist_mat <- calculate.cosine.similarity(tdm)
    
    # CALL THE FUNCTION - GENERATE SIMILARITY RECORDS CROSS COMPARING ACROSS GRT AND GTM FILES
    
    if(cross.grt.gtm.file == "Y"){
      matrix.similar <- cross.gtm.grt.similarity.mat(cosine_dist_mat,tdm.v,grt.df,merge.gtm.grt)
      # converting the retured matrix into a dataframe 
      matrix.similar.df <- as.data.frame(matrix.similar)
      # combing the actual test data with the similarity matrix
      read.test.write <- data.frame(merge.gtm.grt,matrix.similar.df)
      
      # WRITE OUTPUT FILE
      # setting the working directory 
      setwd(file.location.outputs)
      # creating the output file name
      file.name.output <- paste0(file.name.grt,"_","GRT","_",file.name.gtm,"_","GTM","_CROSS_OUTPUT.csv")
      # write the file 
      write.csv(read.test.write,file = file.name.output,row.names = T)
      
      # CALL FUNCTION TO GENERATE HEAT MAP
      
      file.name.heatmap.cross <- paste0(file.name.grt,"_","GRT","_",file.name.gtm,"_","GTM","_CROSS")
      creat.heat.map(matrix.similar,num.index,file.name.heatmap.cross,read.test.write)
      
      
      # GET THE MAXIMUM SIMILARITY VALUES CROSS COMPARING GRT AND GTM - FOR REPORT PURPOSE 
      
      get.highest.index.temp <- matrix.similar[,1]
      get.highest.cross.grt <- max(get.highest.index.temp[(nrow(read.grt) + 1):nrow(read.gtm)])
      get.highest.cross.gtm <- max(get.highest.index.temp[1:nrow(read.grt)])
      
      
      report.index = report.index + 1
      info[[report.index]] <- list(grt.filename = file.name.grt,
                                   gtm.filename = file.name.gtm,
                                   max.grt = "NA",
                                   max.gtm = "NA",
                                   max.cross.gtm = get.highest.cross.gtm,
                                   max.cross.grt = get.highest.cross.grt,
                                   combined.grt.gtm = "NA")
      
      call.write.report <- write.report.file(info[[report.index]])
      
      
      
    }
    
    
    # CALL THE FUNCTION - GENERATE SIMILARITY RECORDS COMBINING FILES 
    # HERE SIMILARITY IS OBTAINED COMPARING ALL THE GRT AND GTM TOGETHER
    
    if(single.grt.gtm.file == "Y"){
      # matrix.similar <- matrix(nrow = nrow(merge.gtm.grt), ncol = (num.index * 2) + num.index)
      matrix.similar <- combined.grt.gtm.matrix(cosine_dist_mat,tdm.v,merge.gtm.grt)
      matrix.similar.df <- as.data.frame(matrix.similar)
      read.test.write <- data.frame(merge.gtm.grt,matrix.similar.df)
      
      # WRITE OUTPUT FILES
      
      setwd(file.location.outputs)
      file.name.output <- paste0(file.name.grt,"_","GRT","_",file.name.gtm,"_","GTM","_SINGLE_OUTPUT.csv")
      write.csv(read.test.write,file = file.name.output,row.names = T)
      
      # CALL FINCTION TO GENERATE HEAT MAP
      
      file.name.heatmap.single <- paste0(file.name.grt,"_","GRT","_",file.name.gtm,"_","GTM","_SINGLE")
      creat.heat.map(matrix.similar,num.index,file.name.heatmap.single,read.test.write)
      
      # REPORT - GET THE MOST SIMILAR RECORD COMPARING THE COMBINED FILES
      
      get.highest.index <- max(matrix.similar[,1])
      
      report.index = report.index + 1
      info[[report.index]] <- list(grt.filename = file.name.grt,
                                   gtm.filename = file.name.gtm,
                                   max.grt = "NA",
                                   max.gtm = "NA",
                                   max.cross.gtm = "NA",
                                   max.cross.grt = "NA",
                                   combined.grt.gtm = get.highest.index)
      
      call.write.report <- write.report.file(info[[report.index]])

    }

  }
  
}

# -- END ---------------------
# MAIN PROGRAM
# ----------------------------

main.program()



