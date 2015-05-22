generatePlot4 <- function() {
  #function for coursera course Exploratory Data Analysis, course project 2, to generate plot 4 which
  #is described as follows "Across the United States, how have emissions from coal combustion-related
  #sources changed from 1999–2008?
  
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
  
  SCC <- SCC[grepl("combustion", tolower(SCC$SCC.Level.One)), ] #filter for Combustion
  SCC <- SCC[grepl("coal", tolower(SCC$SCC.Level.Three)), ] #filter for coal 

  #merge NEI and SCC post filter
  df <- merge(SCC, NEI, by = "SCC")
  df_sum <- df %>% 
    group_by(year) %>% 
    summarise(
      Total_Emissions = sum(Emissions, na.rm = T)
    )
                      
  
  #Im using a combined approach here, I figured the viewer would want to know the distributions over each
  #year as well as if the total emissions have decreased. Having one without the other doesn't really
  #portray then entire story of this data, that is why I'm combining them into one plot. I figured faceting
  #is legal so binding with the ggplot helper library gridExtra should be too
  myplot_bar <- ggplot(df_sum, aes(x = factor(year), y = Total_Emissions, fill = factor(year))) + 
    geom_bar(stat="identity") +
    labs(title = "Coal Emissions from Combustion Sources, Country Wide, from 1998-2008") +
    xlab("Year") + 
    guides(fill = FALSE) + #hide legend 
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 1.25), units = "cm")) 
  
  myplot_count <- ggplot(df, aes(x = Emissions, fill = factor(year))) + 
    geom_histogram(binwidth = 0.4) +
    scale_x_log10() +
    facet_wrap( ~ year, nrow = 1) +
    guides(fill = FALSE) + #hide legend
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 1.25), units = "cm"))
  
  #save plot
  png("plot4.png", width = 680, height = 680)
  grid.arrange(myplot_bar, myplot_count, nrow=2)
  dev.off()
}
