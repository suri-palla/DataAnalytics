# -----------------------------------------------------------
# YOUTUBE SCRAPING SCRIPT
# EXTRACTS THE FOLLOWING :-
#   - VIEWS 
#   - LIKES 
#   - DISLIKES  
#   - VIEWS (MORE ACTIONS BUTTON - STATISTICS)
#   - TIME WATCHED (MORE ACTIONS BUTTON - STATISTICS)
#   - SUBCRIPTIONS DRIVEN (MORE ACTIONS BUTTON - STATISTICS)
#   - SHARES (MORE ACTIONS BUTTON - STATISTICS)
# -----------------------------------------------------------

# loading the required libraries 
rm(list = ls())
library(rvest)
library(RSelenium)

# ---------
# FUNCTION
# ---------

# --------------------------------------------------------------------
# the functions extracts any contents between the tags '<' and '>'
# --------------------------------------------------------------------

extract.content.value <- function(content)
{
  start.tag <- regexpr(">",content)
  stop.tag <- regexpr("<",content)
  act.content <- substr(content,start.tag + 1,stop.tag - 1)
  return(act.content)
}


# -------------- Configuration  ----------------------------------
# Enter the file name that has youtube links
file.name <- "webpage_list.csv"

# Enter the sleep time(seconds) between two consecutive pulls here 
# time.sleep <- 50

# # Enter the youtube webpage here
# y.webpage <- "https://www.youtube.com/watch?v=7Y9W1Gex660"
# -------------------------------------------------------------

# ------------------
# Creates the header 
# ------------------
unlink("youtube_stats.txt")

write.header <- paste("youtube.webpage",
                      "Sys.time",
                      "view.count",
                      "likes.count",
                      "dislikes.count",
                      "stats.views",
                      "stats.time.watched",
                      "stats.subscriptions.driven",
                      "stats.shares",
                      sep = "\t")


write(write.header, file = "youtube_stats.txt", append = TRUE)

# -------------------------------------------------------------

# READS THE FILE WITH YOUTUBE WEBPAGES
read.webpage <- read.csv(file.name,header = F,sep = ",")
y.webpage <- as.character(read.webpage$V1)


for(i in 1:length(y.webpage))
{
  # ---------------------------------------------
  # CLEANING UP VARIABLES FOR EACH ITERATION OF i 
  # ---------------------------------------------
  
  # moves all the workspace variables into all.variables
  all.variables <- ls()
  # gets the index of y.webpage
  index.y.webpage <- which(all.variables == "y.webpage")
  index.i <- which(all.variables == "i")
  index.extract.content.value <- which(all.variables == "extract.content.value")
  # list with all the variables expect y.webpage, i and extract.content.value
  rem.variables <- all.variables[-c(index.y.webpage,index.i,index.extract.content.value)]
  # removes all the varibles in rem.variables
  rm(list = rem.variables)
  Sys.sleep(20)
  
  # -------------------------------------------------------------------------------------
  
  # moves the youtube url
  youtube.webpage <- y.webpage[i]
  # gets the source code
  youtube.htmlpage <- read_html(youtube.webpage)
  
  # GETS THE XPATH NODE FOR VIEWS
  views.count.node <- tryCatch(html_node(youtube.htmlpage,xpath = "//div[@id='watch7-views-info']"),
                               error = function(err){return(err)})
  # GETS THE NUMBER OF VIEWS WITH TEXT "VIEWS"
  views.count.temp <- html_text(views.count.node,trim = T)
  
  # GETS THE STARTING POSITION OF THE NUMBER VIEWS
  pos.ext <- regexpr("views",views.count.temp)
  # EXTRACTS THE VIEWS
  view.count <- substr(views.count.temp,1,pos.ext - 1)
  
  # GETS THE XPATH NODE FOR LIKES AND DISLIKES
  likes.count.node <- tryCatch(html_nodes(youtube.htmlpage,xpath = "//div[@id='watch8-sentiment-actions']//span[@class='yt-uix-button-content']"),
                               error = function(err){return(err)})
  # GETS THE LIKES AND DISLIKES COUNT
  likes.count.temp <- html_text(likes.count.node)
  likes.count <- likes.count.temp[1]
  dislikes.count <- likes.count.temp[4]
  
  # print(Sys.time())
  # print(view.count)
  # print(likes.count)
  # print(dislikes.count)
  
  # -----------------------------------------------------
  # START SCRAPPING THE STATISTICS UNDER THE MORE ACTIONS
  # -----------------------------------------------------
  
  pJS <- phantom() # as i am using windows
  Sys.sleep(5) # give the binary a moment
  remDr <- remoteDriver(browserName = 'phantomjs')
  remDr$open()
  remDr$navigate(y.webpage[i])
  Sys.sleep(10)
  remDr$getTitle()[[1]]
  
  # --------------------------------------------------------------------------------------------
  # OBSERVATION
  # line by line execution of the code was working fine, but while running all the lines of code 
  # as a program, automatic click was failing. So, sleep statements were added.
  # --------------------------------------------------------------------------------------------

  # AUTOMATE THE CLICK 'MORE ACTIONS'
  wxbutton <- remDr$findElement(using = 'xpath', "//*[@id='action-panel-overflow-button']")
  wxbutton$clickElement()
  
  Sys.sleep(10)
  
  # AUTOMATE THE CLICK STATISTICS UNDER MORE ACTIONS
  wxbutton1 <- remDr$findElement(using = 'xpath', "//*[@id='action-panel-overflow-menu']/li[3]/button")
  Sys.sleep(10)
  wxbutton1$clickElement()
  Sys.sleep(10)
  
  # GET THE SOURCE CODE AFTER EXPANDING THE MORE ACTIONS - STATISTICS 
  page_source <- remDr$getPageSource()
  Sys.sleep(10)
  
  # ALL THE STATISTICS VALUES ARE UNDER THE 'bragbar-metric'
  pos.views <- gregexpr("bragbar-metric",page_source)
  content.views <- substr(page_source,pos.views[[1]][1],pos.views[[1]][1] + 30)
  content.time <- substr(page_source,pos.views[[1]][2],pos.views[[1]][2] + 30)
  content.sub.driven <- substr(page_source,pos.views[[1]][3],pos.views[[1]][3] + 30)
  content.shares <- substr(page_source,pos.views[[1]][4],pos.views[[1]][4] + 30)
  
  
  # ------------------------------------------
  # extract the days or years info TIME WACHED
  # ------------------------------------------
  start.content.time.tag.years.days <- regexpr("<em>",content.time)
  content.time.tag.years.days.end <- substr(content.time,start.content.time.tag.years.days + 1 ,nchar(content.time))
  # act.start.content.time.tag.years.days <- regexpr("<em>",content.time.tag.years.days.end)
  # stop.content.time.tag.years.days
  # -------------------------------------------
  
  # GET THE VEDIO STATISTICS CONTENTS
  act.content.views <- extract.content.value(content.views)
  act.content.time <- extract.content.value(content.time)
  act.content.time.years.or.days <- extract.content.value(content.time.tag.years.days.end)
  act.content.time.watched <- paste0(act.content.time," ",act.content.time.years.or.days)
  act.content.sub.driven <- extract.content.value(content.sub.driven)
  act.content.shares <- extract.content.value(content.shares)
  
  # STOP THE SELENIUM BROWSER
  pJS$stop()
  
  
  
  #   print(Sys.time())
  #   print(view.count)
  #   print(likes.count)
  #   print(dislikes.count)
  #   
  #   print(act.content.views)
  #   # print(act.content.time)
  #   # print(act.content.time.years.or.days)
  #   print(act.content.time.watched)
  #   print(act.content.sub.driven)
  #   print(act.content.shares)
  
  
  # WRITE THE RECORD TO THE OUTPUT FILE
  write.record <- paste(youtube.webpage,
                        Sys.time(),
                        view.count,
                        likes.count,
                        dislikes.count,
                        act.content.views,
                        act.content.time.watched,
                        act.content.sub.driven,
                        act.content.shares,
                        sep = "\t")
  write(write.record,file = "youtube_stats.txt",append = TRUE)
  
  # Sys.sleep(time.sleep)
  
}

