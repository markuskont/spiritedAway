print("looking for alerts over timestamps")
library(wordcloud)

for( i in seq_along(1:nrow(confident))){
  names <- names(which(confident[i, ] > REPORT))
  
  if (length(names)>0) {
    print(i)
    names <- strsplit(names, split = "_")
    names <- strsplit(unlist(names), split = "\\.lab")
    names <- strsplit(unlist(names), split = "coe\\.")
    names <- gsub("\\.", "/", unlist(names))
    names <- gsub("elasticsearch", "elastic", unlist(names))
    names <- unlist(names)
    names <- names[!names %in% c("mean", "median", "n", "all", "in")]
    names <- table(names)
    
    dark2 <- brewer.pal(6, "Dark2")
    wordcloud(names(names), unlist(names), max.words=200, min.freq=1, col=dark2, scale=c(4,.5), random.order = FALSE)
    
    Sys.sleep(0.3)
  }
}