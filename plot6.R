generatePlot6 <- function() {
  #function for coursera course Exploratory Data Analysis, course project 2, to generate plot 6 which
  #is described as follows Compare emissions from motor vehicle sources in Baltimore City with emissions
  #from motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen 
  #greater changes over time in motor vehicle emissions?
  
  getData <- function() {
    fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
    NEIfilename <- "summarySCC_PM25.rds"
    SCCfilename <- "Source_Classification_Code.rds"
    files <- c(NEIfilename, SCCfilename)
    
    #quality control, i would wrap this in trycatch for real code
    for (file in files){
      if (!file %in% list.files()){
        message("it seems you are missing one or more file, pardon me while I download the .zip")
        download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", destfile = "FNEI_data.zip", method = "curl")
        unzip("FNEI_data.zip")
        break #break the for loop 
      } 
    }
  }
  
  #function to load required libs
  loadPackages <- function() {
    tryCatch(
    {
      library(dplyr)
      library(ggplot2)
      library(gridExtra)
    },
    error = function(cond) {
      message("loading packages threw an error")
      message("here is the original error message: ")
      message(cond)
      stop("please install required packages with install.packages", call. = F)
    })
  }
  
  #define datasets, load packages
  loadPackages()
  getData()
  NEI <- readRDS(NEIfilename)
  SCC <- readRDS(SCCfilename)
  cities <- c("24510", "06037") #define cities to filter by
  
  #Merge before filter, this would be faster to merge after filter but the code reads more linearly this way
  SCC <- SCC[grepl("[Mm]obile | [Vv]ehicles", SCC$EI.Sector), ] #filter matching on mobile vehicles
  df <- merge(SCC, NEI, by = "SCC") 
  
  #create calculated columns then filter, I'm doing the column first to avoid repetitive code later
  df <- df %>% 
    mutate(City = ifelse(cities[1] == fips, "Baltimore, Maryland", "Los Angeles, California")) %>%
    filter(fips %in% cities) %>%
    arrange(year, City)
  
  #filter for Baltimore and LA
  df_sum <- df %>% 
    group_by(year, City) %>%
    summarise(
      Total_Emissions = sum(Emissions, na.rm = T)
    )
  
  #Im using the same combined approach here, this plot is the exact same code as plot5 just with different
  #filter and a added calculated column for City name. I chose to free the y axes here because I wasn't
  #sure if the message should be a comparison between LA and Bmore, or in contrast a one page look at what
  #occured in Bmore and LA. Choosing the coloring scheme and the particular plot styles sways the message
  #of this plot to one or the other, I prefer this one because if the viewer is careful they can draw both
  #pieces of information out of the plot
  myplot_bar <- ggplot(df_sum, aes(x = factor(year), y = Total_Emissions, fill = factor(year))) + 
    geom_bar(stat="identity") +
    facet_wrap( ~ City, scales = "free_y") +
    labs(title = "Emissions from Motor Vehicle Sources in Baltimore and Los Angeles, from 1998-2008") +
    xlab("Year") + 
    guides(fill = FALSE) + #hide legend 
    theme(strip.text.x = element_text(size = 13)) +
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 1.25), units = "cm")) 
  
  myplot_count <- ggplot(df, aes(x = Emissions, fill = factor(year))) + 
    geom_histogram(binwidth = 0.4) +
    scale_x_log10() +
    facet_grid(year ~ City, scales = "free_y") +
    theme(strip.text.y = element_text(size = 13, angle = 90), strip.text.x = element_text(size = 13)) +
    guides(fill = FALSE) + #hide legend
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 1.25), units = "cm"))
  
  #save plot
  png("plot6.png", width = 1080, height = 880)
  grid.arrange(myplot_bar, myplot_count, nrow=2)
  dev.off()
}
