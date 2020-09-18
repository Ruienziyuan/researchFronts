# global.R

library(shiny)
library(ggplot2)
#library(wordcloud)
library(stringr)
library(plyr)
library(tidytext)
library(memoise)
library(reshape2)

load("Data_statistics.RData")
load("simiIndex.RData")
load("simiGrade.RData")
load("Fronts.RData")

issuelist <- list(
  "202007",
  "202005",
  "202003",
  "202001", 
  "201911",
  "201909",
  "201907",
  "201905",
  "201903",
  "201901",
  "201811",
  "201809"
)

fieldnames <- list(
  "Agricultural Sciences",
  "Biology",
  "Chemistry",
  "Clinical Medicine",
  "Computer Science",
  "Economics",
  "Engineering",
  "Environment",
  "Geosciences",
  "Immunology",
  "Materials Science",
  "Mathematics",
  "Microbiology",
  "Molecular biology",
  "Multidisciplinary",
  "Neuroscience",
  "Pharmacology",
  "Physics",
  "Plant",
  "Psychiatry",
  "Social Sciences",
  "Space Science"
)


# Function:similarity_jac
similarity_jac <- memoise(function(New, Old){
  if (nrow(Old) < 10)  {jc = 0 ; return(jc)}
  New <- rf.current
  Old <- rf.past    
  x1 <- New[, match("Research Fronts", names(New))]
  x2 <- strsplit(x1, split = ";")
  x3 <- unlist(x2)
  
  y1 <- Old[, match("Research Fronts", names(Old))]
  y2 <- strsplit(y1, split = ";")
  y3 <- unlist(y2)
  
  fronts <- unique(c(x3, y3))  
  fronts <- fronts[!is.na(fronts)]
  
  a <- rep(0, length(fronts))
  b <- rep(0, length(fronts))
  
  pos1 <- match(fronts, x3)
  a[which(!is.na(pos1))] <- 1
  pos2 <- match(fronts, y3)
  b[which(!is.na(pos2))] <- 1
  
  jc <- round(sum(a*b)/(sum(a*a) + sum(b*b) - sum(a*b)), 3)
  return(jc)
})

# Function:similarity_grade
similarity_grade <- memoise(function(New, Old) {
  Result <- New
  Result$`Similarity Grade` <- NA
  if (nrow(Old) < 2)  {return(Result)}
  words <- unlist(strsplit(unique(Old[,c("Research Fronts")]), split = ";"))
  var <- names(New)
  names(New) <- c("IS","RF","TP","CT","CP","MY","FI")
  Result <- ddply(New, .(IS,RF,TP,CT,CP,MY,FI), function(x){
    X1 <- x$RF
    X2 <- strsplit(X1, split = ";")
    X3 <- unlist(X2)
    X4 <- words[match(X3, words)]
    X5 <- nrow(na.omit(as.data.frame(X4)))
  })
  names(Result) <- c(var, "Similarity Grade")
  return(Result)
})

# Function:stability
stability <- memoise(function(x){
  y = (0*x[1] + 1*x[2] + 2*x[3] + 3*x[4] + 4*x[5] + 5*x[6]) / (5*sum(x))
  y = round(y, 3)  
  return(y)
})

# 1
# Function:statistics_fronts
statistics_fronts <- function(dataset){
  rfTab <- as.data.frame.array(with(dataset, table(Fields, esi_issue)))
  return(rfTab)
}    

# 2
# Function:similarity_fronts
similarity_fronts <- function() {
  #listissue <- issuelist[12:1]
  #k <- length(listissue)
  k <- 12
  result <- matrix(data = 0, nrow = 22, ncol = k,
                   dimnames = list(fieldnames, issuelist[1:k]))
  
  for (i in 1:22) {
    rf.current  <- subset(rf.dataset, 
                          rf.dataset$esi_issue == issuelist[[1]] & 
                            rf.dataset$Fields == fieldnames[i])
    for (j in 1:k) { 
      rf.past  <- subset(rf.dataset, 
                         rf.dataset$esi_issue == issuelist[[j]] & 
                           rf.dataset$Fields == fieldnames[i])
      
      result[i,j] <- similarity_jac(rf.current, rf.past) 
    }    
  }
  
  result <- as.data.frame.array(result)
  return(result)
}

# 3
# Function:simi_grade_fronts
simi_grade_fronts <- function(dataset){
  I <- c(1,2)
  result <- matrix(data = 0, nrow = 22, ncol = 7,
                   dimnames = list(fieldnames))
  for (i in 1:22) {
    rf.current  <- subset(dataset, esi_issue == issuelist[I[1]] & Fields == fieldnames[i])
    rf.past     <- subset(dataset, esi_issue == issuelist[I[2]] & Fields == fieldnames[i])
    rdf <- similarity_grade(rf.current, rf.past)
    x1 <- as.vector(with(rdf, table(`Similarity Grade`)))
    result[i, ] <- c(x1, stability(x1))  
  }
  rf.grade <- as.data.frame.array(result)
  names(rf.grade) <- c("a0","a1","a2","a3","a4","a5","grade")
  return(rf.grade)
}

# 4
# Function:compare_fronts
compare_fronts <- memoise(function(field   = "Agricultural Sciences",
                                   issue   = "202007") {
  rf1 <- subset(rf.dataset, esi_issue == issuelist[[1]] & Fields == field)
  rf2 <- subset(rf.dataset, esi_issue == issue & Fields == field)      
  rf.result <- similarity_grade(rf1, rf2)
  rf.result$`Similarity Grade` <- as.factor(rf.result$`Similarity Grade`)     
  return(rf.result)
})

# 5
# Function:evolute_fronts
evolute_fronts <- memoise(function(topic = c("CH3NH3PbI3")) {
  topic <- toupper(topic)
  rf.df <- rf.dataset[which(str_count(rf.dataset$`Research Fronts`, topic) != 0),]  
  rf.issue <- as.data.frame(sort(with(rf.df, table(esi_issue)), decreasing = TRUE))
  rf.issue <- as.character(rf.issue$esi_issue)
  rf.issue <- sort(as.numeric(rf.issue), decreasing = TRUE)
  
  rf.result <- data.frame()
  for (i in 1:(length(rf.issue) - 1)){
    rf.temp <- subset(rf.df, esi_issue == as.character(rf.issue[i])) 
    rf.field <- as.data.frame(sort(with(rf.temp, table(Fields)), decreasing = TRUE))  
    rf.field <- as.character(rf.field$Fields)  
    for (k in 1:length(rf.field)){
      rf.current <- subset(rf.temp, Fields == rf.field[k])   
      rf.past <- subset(rf.dataset, esi_issue == as.character(rf.issue[i + 1]) & Fields == rf.field[k]) 
      xx <- similarity_grade(rf.current, rf.past)
      rf.result <- rbind(rf.result, xx)
    }
  }
  xx <- subset(rf.df, esi_issue == as.character(rf.issue[length(rf.issue)])) 
  xx$`Similarity Grade` <- 0
  rf.result <- rbind(rf.result, xx)
  rf.result$`Similarity Grade` <- as.factor(rf.result$`Similarity Grade`)
  
  return(rf.result)
})

# 6
# Function£ºwordcloud_fronts
wordcloud_fronts <- memoise(function(field = "Agricultural Sciences"){
  dataset = subset(rf.dataset, 
                   rf.dataset$Fields == field)
  terms_table <- unnest_tokens(dataset, `Research Fronts`, `Research Fronts`, 
                               token = stringr::str_split, pattern = ";")
  terms_table <- as.data.frame(sort(with(terms_table, table(`Research Fronts`)), decreasing = TRUE))
  # terms_table <- terms_table[1:freq,]
  names(terms_table) <- c("Research Fronts", "Freq")
  return(terms_table)
})

# 1
# Function:plot
statisticsPlot <- function(){
  md <- Data_statistics
  x1 <- data.frame(fields = rownames(md), md)
  x2 <- melt(x1, id = "fields")
  ggplot(x2, aes(x = `fields`, y= `value`)) +
    geom_boxplot() +
    coord_flip()     
}

# 2
# Function:plot
similarityPlot <- function(field = "Agricultural Sciences"){
  # x <- simiIndex[which(match(rownames(simiIndex), field) == 1),]
  pos <- which(match(rownames(simiIndex), field) == 1)
  mydata <- as.data.frame(t(simiIndex))
  ggplot(mydata, aes(x = rownames(mydata), y = mydata[,pos])) +
    geom_point() +
    labs(x = "Issue", y = "Similarity")  
}

# 3
# Function:plot
simigradePlot <- function(field = "Agricultural Sciences"){
  # x <- simiIndex[which(match(rownames(simiIndex), field) == 1),]
  pos <- which(match(rownames(simiGrade), field) == 1)
  mydata <- as.data.frame(t(simiGrade))[c(1:6),]
  ggplot(mydata, aes(x = rownames(mydata), y = mydata[,pos])) +
    geom_bar(stat = "identity") +
    labs(x = "Grade", y = "Number")  
}

# 4
# Function:plot
comparePlot <- function(mydata){
  ggplot(mydata, 
         aes(x = `Top Papers`, 
             y = `Mean Year`,
             size = `Cites/Top Paper`,
             color = `Similarity Grade`,
             shape = `Similarity Grade`)) + 
    geom_point() +
    theme(legend.position = "none")
}

# 5
# Function:plot
evolutionPlot <- function(mydata){
  ggplot(mydata, 
         aes(x = `Top Papers`, 
             y = `Mean Year`,
             size = `Cites/Top Paper`,
             shape = `Similarity Grade`,
             color = `Fields`)) +
    geom_point() +
    facet_grid(Fields ~ esi_issue) +         
    theme(legend.position = "none")    
}

# 6
# Function:plot
cloudPlot <- function(dataset, wordcloud_rep, minfreq, maxwords){
  terms_table <- unnest_tokens(dataset, `Research Fronts`, `Research Fronts`, 
                               token = stringr::str_split, pattern = ";")
  terms_table <- as.data.frame(sort(with(terms_table, table(`Research Fronts`)), 
                                    decreasing = TRUE))
  # terms_table <- head(terms_table, freq)
  names(terms_table) <- c("Research Fronts", "Freq")  
  wordcloud_rep(terms_table[,1], 
                terms_table[,2], 
                scale = c(3, 0.5),
                min.freq  = minfreq, 
                max.words = maxwords,
                colors    = brewer.pal(8, "Dark2"))
}


# Function:getTerms
getTerms <- function(dataset= rf.dataset){
  terms_table <- unnest_tokens(dataset, `Research Fronts`, `Research Fronts`, 
                               token = stringr::str_split, pattern = ";")
  terms_table <- as.data.frame(sort(with(terms_table, table(`Research Fronts`)), decreasing = TRUE))
  # terms_table <- head(terms_table, freq)
  names(terms_table) <- c("Research Fronts", "Freq")
  return(terms_table)
}
