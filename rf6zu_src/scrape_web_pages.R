library(rvest)
library(stringr)

# scrape employees from homepage of the statistics institute
person_page <- read_html("http://www.statistik.uni-muenchen.de/personen/index.html")
stats_persons <- person_page %>% html_nodes(".tcol-name a") %>% html_text()
stats_persons <- sort(gsub(" ", "", stats_persons))
stats_persons <- strsplit(stats_persons, split = ",")
# abbreviate first names to match with abbreviations in publication list
stats_persons <- lapply(stats_persons, function(x){
  x[2]<-str_sub(x[2], 1, 1)
  x
})
stats_persons <- lapply(stats_persons, function(x)paste(x[1], x[2], sep = ","))

# scrape publications
paper_page <- read_html("http://www.statistik.uni-muenchen.de/forschung/veroeffentlichungen/index.html")
paper_authors <- paper_page %>% html_nodes(".autoren") %>% html_text()
paper_authors <- gsub("\\n", "", paper_authors)
paper_authors <- gsub(" ", "", paper_authors)
paper_authors <- strsplit(paper_authors, ";")

# scrape working papers
reports_page <- html("http://www.statistik.uni-muenchen.de/forschung/technical_reports/index.html")
reports_authors <- reports_page %>% html_nodes(".autoren") %>% html_text()
reports_authors <- gsub("\\n", "", reports_authors)
reports_authors <- gsub(" ", "", reports_authors)
reports_authors <- strsplit(reports_authors, ";")

# create sociomatrix
n <- length(stats_persons)
sociomatrix <- matrix(nrow = n, ncol = n, dimnames = list(stats_persons, stats_persons))

matchfun <- function(paper, i, j, sociomatrix){
  stringi <- colnames(sociomatrix)[i]
  stringj <- colnames(sociomatrix)[j]
  # return TRUE if authors of the provided paper contain both names identified by 
  # column names i and j in the sociomatrix
  return(sum(grepl(paste0(stringi, "|", stringj), paper)) == 2)
}

for (i in 1:(n-1)){
  for (j in (i+1):n){
    sociomatrix[i,j] <- sum(unlist(lapply(paper_authors, matchfun,
                                          i, j, sociomatrix)),
                            unlist(lapply(reports_authors, matchfun,
                                          i, j, sociomatrix)))
  }
}

sociomatrix[is.na(sociomatrix)] <- 0
sociomatrix <- sociomatrix + t(sociomatrix)
# remove persons with zero edges
sociomatrix <- sociomatrix[colSums(sociomatrix) != 0, colSums(sociomatrix) != 0]

# write sociomatrix to disc
# ATTENTION: DO NOT ACCIDENTIALLY OVERWRITE!
# saveRDS(sociomatrix, file = "sociomatrix.rds")

