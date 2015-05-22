generatePlot2 <- function() {
  #function for coursera course Exploratory Data Analysis, course project 2, to generate plot 2 which
  #is described as follows "Using the base plotting system, make a plot showing the total PM2.5 
  #emission from all sources for each of the years 1999, 2002, 2005, and 2008 only from Baltimore "
  
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
getData()
NEI <- readRDS(NEIfilename)
SCC <- readRDS(SCCfilename)

#initialize PNG file
png("plot2.png", width = 680, height = 480)

#Get Emission totals for each year
Emissions_Total <- NEI %>%
  group_by(year, fips) %>%
  filter(fips == "24510") %>%
  summarise(
    Total_Emissions = sum(Emissions)
  )

#plot a line plot with year as x-axis
options(scipen=5) #change the threshold for scientific notation in R
par(mar = c(5, 7, 4, 2))
with(Emissions_Total
     , plot(Total_Emissions ~ year, pch = 19, xaxt="n", las = 1, ylab=""))

#get axes to be meaningful
axis(1, at = 1998:2008, labels = 1998:2008, las = 2)
title(ylab = "Total Emissions", line = 5)
title(main = "Total Emissions of PM2.5 by Year in Baltimore City")

#add trend line
model <- lm(Total_Emissions ~ year, Emissions_Total)
abline(model,lwd = 2)

#Close graphics device, save PNG
dev.off()
}
