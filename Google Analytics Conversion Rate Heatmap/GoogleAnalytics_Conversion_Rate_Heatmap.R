# Load Packages -----------------------------------------------------------
library(RGoogleAnalytics)
library(RColorBrewer)
library(plyr)
library(scales)
library(reshape2)
library(ggplot2)

# Open Google Analytics API, Set Profile & Set Variables -------------------------------
query <- QueryBuilder()
access_token <- query$authorize()

ga <- RGoogleAnalytics()
( ga.profiles <- ga$GetProfileData(access_token) )

my_profile    <- 2 #CLIENT ID FOR ANALYSIS
my_start_date <- "2014-01-01" 
my_end_date   <- "2014-01-29"


# Build Google Analytics Query --------------------------------------------
query$Init(start.date = my_start_date,
           end.date = my_end_date,
           dimensions = "ga:dayOfWeek, ga:hour",
           metrics = "ga:transactions, ga:visits, ga:transactionRevenue",
           filters = "ga:medium==organic",
           max.results = 10000,
           access_token=access_token,
           table.id = paste("ga:",ga.profiles$id[my_profile],sep="",collapse=","))

ga.data <- ga$GetReportData(query)


# Clean & Manipulate Data -------------------------------------------------
## Create Conversion Rate Variable
ga.data$conversionRate <- ga.data$transactions/ga.data$visits

ga.data$dayOfWeek <- as.character(ga.data$dayOfWeek)
ga.data$dayOfWeek[ga.data$dayOfWeek == "0"] <- "Sunday"
ga.data$dayOfWeek[ga.data$dayOfWeek == "1"] <- "Monday"
ga.data$dayOfWeek[ga.data$dayOfWeek == "2"] <- "Tuesday"
ga.data$dayOfWeek[ga.data$dayOfWeek == "3"] <- "Wednesday"
ga.data$dayOfWeek[ga.data$dayOfWeek == "4"] <- "Thursday"
ga.data$dayOfWeek[ga.data$dayOfWeek == "5"] <- "Friday"
ga.data$dayOfWeek[ga.data$dayOfWeek == "6"] <- "Saturday"

ga.data$dayOfWeek <- factor(ga.data$dayOfWeek, levels = c("Sunday", 
                                                          "Monday", 
                                                          "Tuesday", 
                                                          "Wednesday", 
                                                          "Thursday", 
                                                          "Friday", 
                                                          "Saturday"))

ga.data[order(ga.data$dayOfWeek),]

p.data <- ga.data
p.data$transactions <- NULL
p.data$visits <- NULL
p.data$transactionRevenue <- NULL
p.data$conversionRate <- 100*(p.data$conversionRate)

p.data <- melt(p.data, id=c("dayOfWeek", "hour"), na.rm=TRUE)
data.wide <- dcast(p.data, dayOfWeek ~ hour, sum)
summary(data.wide)

data.wide$dayOfWeek <- NULL
names(data.wide) <- NULL
data.plotly <- as.list(data.wide)

library(RColorBrewer)
library(plotly)

py <- plotly(username='XXXXX', key='XXXXX')

x <- c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday') 
y <- c('12AM', '1AM', '2AM', '3AM', '4AM', '5AM', '6AM', '7AM', '8AM', '9AM', '10AM', '11AM', '12PM', '1PM', '2PM', '3PM', '4PM', '5PM', '6PM', '7PM', '8PM', '9PM', '10PM', '11PM')
z <- data.plotly

# Color brewer YIOrBr colorscale http://colorbrewer2.org
# ['rgb(255,255,229)','rgb(255,247,188)','rgb(254,227,145)','rgb(254,196,79)','rgb(254,153,41)','rgb(236,112,20)','rgb(204,76,2)','rgb(153,52,4)','rgb(102,37,6)']

scl <- brewer.pal(9,'YlOrBr')
data <- list(
  x = x,
  y = y,
  z = z,
  scl= list(
    c(0,"rgb(255,255,229)"),
    c(0.125,"rgb(255,247,188)"),
    c(0.25,"rgb(254,227,145)"),
    c(0.375,"rgb(254,196,79)"),
    c(0.5,"rgb(254,153,41)"),
    c(0.625,"rgb(236,112,20)"),
    c(0.75,"rgb(204,76,2)"),
    c(0.875,"rgb(153,52,4)"),
    c(1,"rgb(102,37,6)")
  ),
  type = 'heatmap'
)

response <- py$plotly(data)

# url and filename
url <- response$url
filename <- response$filename
url
