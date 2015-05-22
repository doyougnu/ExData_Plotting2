generatePlot3 <- function() {
  #function for coursera course Exploratory Data Analysis, course project 2, to generate plot 2 which
  #is described as follows "Using the base plotting system, make a plot showing the total PM2.5 
  #emission from all sources for each of the years 1999, 2002, 2005, and 2008. "
  
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
png("plot3.png", width = 680, height = 480)

#Get Emission totals for each year
NEI_filtered <- NEI %>%
  group_by(year, fips, type) %>%
  filter(fips == "24510") #%>%
  #summarise(
  #  Total_Emissions = sum(Emissions)
  #)

#plot a line plot with year as x-axis, facet by type, save it
#I went with a boxplot here because "year" is a categorical variable in this dataset, thus
#making a line plot with a linear regression can be misleading because we have missing data in years
#I also specifically didn't include a trend line for this boxplot because it could also be misleading
#with this data, the y-axis is on a log scale
myplot <- ggplot(NEI_filtered, aes(as.factor(year), Emissions)) + 
  facet_wrap( ~ type) + 
  geom_boxplot(aes(fill = type)) + 
  scale_y_log10() 
  
ggsave(filename = "plot3.png", plot = myplot, width = 6, height = 4)

}
