generatePlot1 <- function() {
  #function for coursera course Exploratory Data Analysis, course project 2, to generate plot 1 which
  #is described as follows "Using the base plotting system, make a plot showing the total PM2.5 
  #emission from all sources for each of the years 1999, 2002, 2005, and 2008. "
  
  getData <- function() {
    fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
    NEIfilename <- "summarySCC_PM25.rds"
    SCCfilename <- "Source_Classification_Code.rds"
    files <- c(NEIfilename, SCCfilename)
    
    #quality control, i would wrap this in trycatch for real code
    for (file in files){
      if (file %in% list.files()){
        print(file)
      } else {
        message("it seems you are missing one or more file, pardon me while I download the .zip")
        download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", destfile = "FNEI_data.zip", method = "curl")
        unzip("FNEI_data.zip")
      }
    }
  }
  getData()
  
  #function to load required libs
  loadPackages <- function() {
    tryCatch(
      {
        library(dplyr)
      },
      error = function(cond) {
        message("loading packages threw an error")
        message("here is the original error message: ")
        message(cond)
        stop("please install required packages with install.packages", call. = F)
      }
    )
  }
  
  #define datasets, load packages
  loadPackages()
  NEI <- readRDS(NEIfilename)
  SCC <- readRDS(SCCfilename)
  
  #Going to have to perfom some sort of transform on Emissions data, fix x-axis, add trendline
}
