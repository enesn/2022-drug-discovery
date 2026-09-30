## ===========================================================================================#                    
# SESSION INFO
# R version 3.6.3 (2020-02-29)
# Platform: x86_64-pc-linux-gnu (64-bit)
# Running under: Ubuntu 18.04.4 LTS
## ===========================================================================================#

correct <- function(word) { 
  c(sorted_words[adist(word, sorted_words) <= min(adist(word, sorted_words), 2)], word)[1]}


multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
  
}

#cleaning function
clean_chr <- function(x){
  x<-gsub(pattern = "-", "",x)
  # x<-gsub(pattern = "\'", "",x)
  # x<-gsub(pattern = "\:", "",x)
  # x<-gsub(pattern = "\;", "",x)
  # x<-gsub(pattern = "\'", "",x)
 x<-gsub(pattern = ",", "",x)
 x<-gsub(pattern = "\\.", "",x)
 x<-gsub(pattern = "\"", "",x)
 x<- toupper(x)
 x<-gsub(pattern = "INC", "",x)
 x<-gsub(pattern = "LLC", "",x)
 x<-gsub(pattern = "CORPORATION", "",x)
 x<-gsub(pattern = "COMPANY", "",x)
 x<-gsub(pattern = "CORP", "",x)
 x<-gsub(pattern = "LTD", "",x)
 x<-gsub(pattern = "LLP", "",x)
 x<-gsub(pattern = "INCORPORATED", "",x)
 x<-gsub(pattern = "ORPORATED", "",x)
 x<-gsub(pattern = "GMBH", "",x)
 x<-gsub(pattern = "ST STE 3460", "",x)
 x<-gsub(pattern = "GMBH", "",x)
 x<-gsub(pattern = "LIMITED", "",x)
 x<-gsub(pattern = "LP", "",x)
 x[x == "NULL" ] <- NA
 x <- gsub("^\\s+|\\s+$", "", x)
 x <- gsub("(A OF).*", "\\1", x)
 x <- gsub("A OF", "", x)
 x <- gsub("(A  OF).*", "\\1", x)
 x <- gsub("A  OF", "", x)
 x<- replace(x, x == "THE","")
 x<- replace(x, x == "CO","")

 return(x)
}

#remove leading 0s 
rm_lead0 <- function(x) {
  
  str_remove(x, "^0+")
  
}


#find duplicates
find_duplicate <- function(x, variable) {
  x[which(duplicated(x[ , variable])), ]
}

#initial arrangements
# install any missing packages, then load them
packages <- c("data.table", "stringdist", "ggrepel", "directlabels", "fuzzyjoin",
              "haven", "ggthemes", "lubridate", "DescTools", "tidyverse")
# loaded later by the scripts themselves (load order matters: plyr must come after dplyr)
later_packages <- c("scales", "reshape2", "plyr", "stargazer", "dyn", "sjPlot")
missing_packages <- setdiff(c(packages, later_packages), rownames(installed.packages()))
if (length(missing_packages) > 0) install.packages(missing_packages)
invisible(lapply(packages, library, character.only = TRUE))

filter <- dplyr::filter
select <- dplyr::select


#theme
enes_theme <- theme_base() +
  theme(axis.text.x = element_text(angle = 70, hjust = 1),legend.position="bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.minor.y = element_line(color = "gray97"),
        plot.background = element_rect(fill = "transparent",colour = NA))



