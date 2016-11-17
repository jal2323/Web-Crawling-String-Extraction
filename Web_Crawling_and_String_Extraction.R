######################################################################
## Web Crawling and String Extraction
######################################################################


################################################################
## XML function to pull out anchor tags with href
################################################################

library(XML)


links <- function(URL) 
{
  getLinks <- function() {
    links <- character()
    list(a = function(node, ...) {
      links <<- c(links, xmlGetAttr(node, "href"))
      node
    },
    links = function() links)
  }
  h1 <- getLinks()
  htmlTreeParse(URL, handlers = h1)
  h1$links()
}


##############################################################################################
## Build a web crawler to extract hrefs from www.sustainablebusiness.com
##############################################################################################

num <- 2
links.test <- list()
index.test <- list()
for (i in 1:18){
  url <- paste("http://www.sustainablebusiness.com/index.cfm/go/greendreamjobs.main/state/0/country/0/category/0/level/0?page=", num, sep = "")
  links.test[[i]] <- links(url)
  index.test[[i]] <- grepl("/index.cfm/go.greendreamjobs.display/id.3*", links.test[[i]])
  links.test[[i]] <- links.test[[i]][index.test[[i]]]
  links.test[[i]] <- paste("http://www.sustainablebusiness.com", links.test[[i]], sep="")
  num <- num + 1
} 

links.test.unlist <- unlist(links.test) 
links.test.unlist <- links.test.unlist[!duplicated(links.test.unlist)]


########################################################################
## Extract the text
###########################################################

library(rvest)

htmlpage <- list()
nodes <- list()
text <- list()
for (i in 1:length(links.test.unlist)){
  htmlpage[[i]] <- html(links.test.unlist[i])
  nodes[[i]] <- html_nodes(htmlpage[[i]], "#section-body-main") ## Use the SelectorGadget weblet to find appropriate CSS (Cascading Style Sheet)
  text[[i]] <- html_text(nodes[[i]])
}

################################################################
## Clean the text
################################################################

## Get rid of all numeric values
## Get rid of all special characters (i.e. "/")
## Convert to lower case


library(stringr)

clean.text <- list()
for (i in 1:length(text)){
  clean.text[[i]] <- gsub("\\d", "", text[[i]])
  clean.text[[i]] <- str_replace_all(clean.text[[i]], "[^[:alnum:]]", " ")
  clean.text[[i]] <- strsplit(tolower(clean.text[[i]]), "[^a-z]+")
}


######################################################################
## Unlist
######################################################################

big.text <- unlist(clean.text)
length(big.text) ## 116,653 words

######################################################################
## Word counts
######################################################################

word_count <- table(big.text)
word_count <- word_count[order(word_count, decreasing = TRUE)] 


sorted_words <- names(sort(word_count, decreasing = TRUE))


head(word_count, 400) ## Gives you the 400 most commonly used words in the 182 web
## pages scraped by the crawler.










