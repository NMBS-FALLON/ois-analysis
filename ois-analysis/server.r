library("ggplot2")
library("stringr")
library("repmis")
library("ggthemes")
library("reshape")
library("xts")
#library("quantmod")
#library("foreach")
library("scales")
#library("reshape2")
#library("lubridate")
library("shiny")
library("leaflet")
library("plyr")
library("plotly")

pdf(NULL)
#source("./scripts/dataframes.r")
AddCumulativeCol <- function(df, columnIndex) 
{
  newColumnName <-str_c(columnIndex, "_", "Cumulative")
  column <- df[,columnIndex]
  df[1,newColumnName] <- column[1]
  for(i in 2:length(column))
  {
    df[i,newColumnName] <- df[i-1,newColumnName]+column[i]
  }
  return(df)
}

GetLatLong <- function(df, cityState)
{
  city <- unlist(strsplit(cityState, ","))[1]
  state <- unlist(strsplit(cityState, ","))[2]
  lat <- df[which(df[,"City"] == city & df[,"State"] == state), ][1,"Lat"]
  long <- df[which(df[,"City"] == city & df[,"State"] == state), ][1,"Long"]
  latLong <- paste(lat,long,sep = ",")
}
GetLatLong2 <- function(df, city, state)
{
  lat <- df[which(df[,"City"] == city & df[,"State"] == state), ][1,"Lat"]
  long <- df[which(df[,"City"] == city & df[,"State"] == state), ][1,"Long"]
  latLong <- paste(lat,long,sep = ",")
}

SplitByInterval <- function(df,dateColumn,valueColumn, interval)
{
  df_new = data.frame(DATE = df[,dateColumn])
  df_new[,"VALUE"] = df[,valueColumn]
  df_new <- as.xts(df_new[,"VALUE"], order.by = as.Date(df_new[,"DATE"]))
  if(interval == "daily")
  {
    df_new_inter <- apply.daily(df_new,sum)
  }
  if(interval == "weekly")
  {
    df_new_inter <- apply.weekly(df_new,sum)
  }
  if(interval =="monthly")
  {
    df_new_inter <- apply.monthly(df_new,sum)
  }
  if(interval == "quarterly")
  {
    df_new_inter <- apply.quarterly(df_new,sum)
  }
  if(interval == "yearly")
  {
    df_new_inter <- apply.yearly(df_new,sum)
  }
  df_new = data.frame(dateColumn = index(df_new_inter))
  df_new[, valueColumn] = df_new_inter[,1]
  df_new = data.frame(df_new)
  
  return(df_new)
}

# MonthlyTable <- function(df1,df1Name,dateColumn1,valueColumn1,df2,df2Name,dateColumn2,valueColumn2, startDate,endDate)
# {
#   numMonths <- 0
#   endDateCounter <- endDate
#   while(endDateCounter>startDate)
#   {
#     endDateCounter <- as.Date(endDateCounter) - months(1)
#     numMonths <- numMonths+1
#   }
#   month <- NULL
#   value1 <- NULL
#   value2 <- NULL
#   
#   for(i in 1:numMonths)
#   {
#     subtractMonths = numMonths-i
#     end2 <- as.Date(endDate)-months(subtractMonths)
#     start2 <- as.Date(end2)-months(1)
#     df_Monthly1 <- data.frame(date=df1[,dateColumn1], Value = df1[,valueColumn1])
#     df_Monthly1 <- df_Monthly1[df_Monthly1[,1]>=startDate & df_Monthly1[,1]<end2,]
#     df_Monthly1 <- df_Monthly1[order(df_Monthly1[,1]),]
#     df_Monthly1 <- AddCumulativeCol(df_Monthly1,2)
#     
#     df_Monthly2 <- data.frame(date=df2[,dateColumn2], Value = df2[,valueColumn2])
#     df_Monthly2 <- df_Monthly2[df_Monthly2[,1]>=startDate & df_Monthly2[,1]<end2,]
#     df_Monthly2 <- df_Monthly2[order(df_Monthly2[,1]),]
#     df_Monthly2 <- AddCumulativeCol(df_Monthly2,2)
#     
#     
#     month = c(month,format(start2,"%b %Y"))
#     value1 = c(value1,df_Monthly1[nrow(df_Monthly1),3])
#     value2 = c(value2,df_Monthly2[nrow(df_Monthly2),3])
#     
#   }
#   percent <-  (value1/value2)*100
#   df_MonthlyTable <- data.frame(Month=month)
#   df_MonthlyTable[,df1Name] = value1
#   df_MonthlyTable[,df2Name] = value2
#   df_MonthlyTable[,"PERCENT"] = percent
#   
#   return(df_MonthlyTable)
# }
# 
# MonthlyTable2 <- function(df1,df1Name,dateColumn1,valueColumn1,df2,df2Name,dateColumn2,valueColumn2, startDate,endDate)
# {
#   numMonths <- 0
#   endDateCounter <- endDate
#   while(endDateCounter>startDate)
#   {
#     endDateCounter <- as.Date(endDateCounter) - months(1)
#     numMonths <- numMonths+1
#   }
#   month <- NULL
#   value1 <- NULL
#   value2 <- NULL
#   
#   for(i in 1:numMonths)
#   {
#     subtractMonths = numMonths-i
#     end2 <- as.Date(endDate)-months(subtractMonths)
#     start2 <- as.Date(end2)-months(1)
#     df_Monthly1 <- data.frame(date=df1[,dateColumn1], Value = df1[,valueColumn1])
#     df_Monthly1 <- df_Monthly1[df_Monthly1[,1]>=start2 & df_Monthly1[,1]<end2,]
#     df_Monthly1 <- df_Monthly1[order(df_Monthly1[,1]),]
#     df_Monthly1 <- AddCumulativeCol(df_Monthly1,2)
#     
#     df_Monthly2 <- data.frame(date=df2[,dateColumn2], Value = df2[,valueColumn2])
#     df_Monthly2 <- df_Monthly2[df_Monthly2[,1]>=start2 & df_Monthly2[,1]<end2,]
#     df_Monthly2 <- df_Monthly2[order(df_Monthly2[,1]),]
#     df_Monthly2 <- AddCumulativeCol(df_Monthly2,2)
#     
#     
#     month = c(month,format(start2,"%b %Y"))
#     value1 = c(value1,df_Monthly1[nrow(df_Monthly1),3])
#     value2 = c(value2,df_Monthly2[nrow(df_Monthly2),3])
#     
#   }
#   percent <-  (value1/value2)*100
#   df_MonthlyTable <- data.frame(Month=month)
#   df_MonthlyTable[,df1Name] = value1
#   df_MonthlyTable[,df2Name] = value2
#   df_MonthlyTable[,"PERCENT"] = percent
#   
#   return(df_MonthlyTable)
# }




###############################################################################################
#PREPARE DATA#

#DOWNLOAD DATA


# df_fallon_released <- source_DropboxData(file="FALLON_RELEASED.csv", key = "eg4yqi3780vqkja", header = TRUE)
# df_fallon_sold_all <- source_DropboxData(file="FALLON_SOLD.csv", key = "3oe8vv9kb3p16y4", header = TRUE)
# df_fallon_quoted <- source_DropboxData(file="FALLON_QUOTED.csv", key="fe7g5p34ufpmnvr", header=TRUE)
# df_fallon_deck_quoted_all <- source_DropboxData(file = "FALLON_DECK QUOTED.csv", key ="j1yzoq33l20bm5a", header = TRUE)
# df_fallon_deck_sold <- source_DropboxData(file = "FALLON_DECK SOLD.csv", key = "hygxu9d0l4q8cxz")
# df_colors <- source_DropboxData(file = "colors.csv", key = "7am08m32umo3lxh")
# df_joist_quoted_map <- source_DropboxData(file = "df_quoted_map.csv", key = "lccwbr6g13qvt9h" )
# df_joist_sold_map  <- source_DropboxData(file ="df_sold_map.csv", key =  "21n2dwta3kkon41")
# df_fallon_joist_shipped <- source_DropboxData(file = "FALLON_JOIST SHIPED.csv", key = "hxn64tb6yzga3x3", header = TRUE)

df_fallon_released <- read.csv("./Data/FALLON_RELEASED.csv", header = TRUE, check.names=FALSE)
df_fallon_sold_all <- read.csv("./Data/FALLON_SOLD.csv", header = TRUE, check.names=FALSE)
df_fallon_quoted <- read.csv("./Data/FALLON_QUOTED.csv", header = TRUE, check.names=FALSE)
df_fallon_deck_quoted_all <- read.csv("./Data/FALLON_DECK QUOTED.csv", header = TRUE, check.names=FALSE)
df_fallon_deck_sold <- read.csv("./Data/FALLON_DECK SOLD.csv", header = TRUE, check.names=FALSE)
df_colors <- read.csv("./Data/colors.csv", header = TRUE, check.names=FALSE)
df_joist_quoted_map <- read.csv("./Data/df_quoted_map.csv", header = TRUE, check.names=FALSE)
df_joist_sold_map  <- read.csv("./Data/df_sold_map.csv", header = TRUE, check.names=FALSE)
df_fallon_joist_shipped <- read.csv("./Data/FALLON_JOIST SHIPPED.csv", header = TRUE, check.names=FALSE)




#CONVERT DATE COLUMN TO DATE FORMAT

df_fallon_released[,"Date"] <- as.Date(df_fallon_released[,"Date"], "%m/%d/%Y")
df_fallon_sold_all[,"Date"] <- as.Date(df_fallon_sold_all[,"Date"], "%m/%d/%Y")
df_fallon_quoted[,"Date"] <- as.Date(df_fallon_quoted[,"Date"], "%m/%d/%Y")
df_fallon_deck_quoted_all[,"Date"] <- as.Date(df_fallon_deck_quoted_all[,"Date"], "%m/%d/%Y")
df_fallon_deck_sold[,"Date"] <- as.Date(df_fallon_deck_sold[,"Date"], "%m/%d/%Y")
df_fallon_joist_shipped[,"Date"] <- as.Date(df_fallon_joist_shipped[,"Date"], "%m/%d/%Y")

#Sort Columns By Date

df_fallon_released <- df_fallon_released[order(df_fallon_released[,"Date"]),]
row.names(df_fallon_released) <- 1:nrow(df_fallon_released)
df_fallon_sold_all <- df_fallon_sold_all[order(df_fallon_sold_all[,"Date"]),]
row.names(df_fallon_sold_all) <- 1:nrow(df_fallon_sold_all)
df_fallon_quoted <- df_fallon_quoted[order(df_fallon_quoted[,"Date"]),]
row.names(df_fallon_quoted) <- 1:nrow(df_fallon_quoted)
df_fallon_deck_quoted_all <- df_fallon_deck_quoted_all[order(df_fallon_deck_quoted_all[,"Date"]),]
row.names(df_fallon_deck_quoted_all) <- 1:nrow(df_fallon_deck_quoted_all)
df_fallon_deck_sold <- df_fallon_deck_sold[order(df_fallon_deck_sold[,"Date"]),]
row.names(df_fallon_deck_sold) <- 1:nrow(df_fallon_deck_sold)
df_fallon_joist_shipped <- df_fallon_joist_shipped[order(df_fallon_joist_shipped[,"Date"]),]
row.names(df_fallon_joist_shipped) <- 1:nrow(df_fallon_joist_shipped)

# GET UPDATE DATE (ASSUMES EVERTHING IS UPDATED AT THE SAME TIME)
df_fallon_released_date <- df_fallon_released[nrow(df_fallon_released),"Date"]
df_fallon_sold_all_date <- df_fallon_sold_all[nrow(df_fallon_sold_all),"Date"]
df_fallon_quoted_date <- df_fallon_quoted[nrow(df_fallon_quoted),"Date"]
df_fallon_deck_quoted_all_date <- df_fallon_deck_quoted_all[nrow(df_fallon_deck_quoted_all),"Date"]
df_fallon_deck_sold_date <- df_fallon_deck_sold[nrow(df_fallon_deck_sold),"Date"]
df_fallon_joist_shipped_date <- df_fallon_joist_shipped[nrow(df_fallon_joist_shipped),"Date"]
lastDate <- max(c(df_fallon_released_date,df_fallon_sold_all_date,df_fallon_quoted_date,df_fallon_deck_quoted_all_date,df_fallon_deck_sold_date, df_fallon_joist_shipped_date))



#Calculate Time Between SOld Date and Average Released Date and Store it with Sold Data (FOR JOBS BUILT IN FALLON ONLY)
#df_fallon_sold_fallon <- df_fallon_sold_all
# for(i in 1:length(df_fallon_sold_fallon[,1]))
# {
#   regexString <- paste("^",df_fallon_sold_fallon[i,"Job Number"],"$", sep="")
#   df_temp <- df_fallon_released[grep(regexString, df_fallon_released[,"Job Number"]),]
#   timeBetween <- mean(df_temp[,1])-df_fallon_sold_fallon[i,1]
#   if(grepl("A",df_fallon_sold_fallon[i,"Job Number"])==FALSE && grepl("R",df_fallon_sold_fallon[i,"Job Number"])==FALSE)
#   {
#     df_fallon_sold_fallon[i,"Days Untill Release"] <- timeBetween
#   }
# }
# rm(i)
# rm(regexString)
# rm(timeBetween)
# rm(df_temp)

# Get Rid of all jobs that calculated out to NA for date between sold and released; these are jobs that were sold in fallon, yet have not been released
#df_fallon_sold_fallon <- df_fallon_sold_fallon[complete.cases(df_fallon_sold_fallon),]



#############
# df_test <- df_fallon_quoted[,]
# df_test[,"Location"] <- lapply(data.frame(df_test[,"Location"]), function(x) {gsub("  ", " ", x)})
# df_test <- merge(df_cityData,df_test, all.y = TRUE, by = c("Location", "State"))
# rm(df_cityData)
# 
# latLong <- as.list(df_test[,"LatLong"])
# latLongApplied <- data.frame(unlist(lapply(latLong, function(x) {GetLatLong(df=df_cityData, x)})))
# df_test[,"LatLong"] <- latLongApplied
# rm(latLong)
# rm(latLongApplied)
# df_test[,"LatLong"] <- data.frame(lapply(unlist(as.list(df_test[,"LatLong"])), function(x) {GetLatLong(df = df_cityData, cityState = as.character(x))}))

# 
# 
# 
# for (i in c(1:nrow(df_test)))
# {
#   df_test[i,"LAT"] <- df_cityData[which(df_cityData$City == df_test[i,"Location"] & df_cityData$State == df_test[i,"State"]), ][1,"Lat"]
#   df_test[i,"LONG"] <- df_cityData[which(df_cityData$City == df_test[i,"Location"] & df_cityData$State == df_test[i,"State"]), ][1,"Long"]
# }
# 
# df_test <- df_test[complete.cases(df_test),]
# write.csv(df_test,"./data/df_quoted_map.csv")
#df_joist_quoted_map <- read.csv("./data/df_quoted_map.csv", header = TRUE, check.names=FALSE, stringsAsFactors = FALSE, sep = ",", strip.white = TRUE)
df_listUniqueTakeoffPerson <- data.frame(unique(df_joist_quoted_map$TakeoffPerson))
colnames(df_listUniqueTakeoffPerson) <- "TakeoffPerson"
df_listUniqueTakeoffPerson[,"Color"] <- df_colors[1:nrow(df_listUniqueTakeoffPerson),1]

df_joist_quoted_map[,"Date"] <- as.Date(df_joist_quoted_map[,"Date"], "%m/%d/%Y")
#df_joist_sold_map <- read.csv("./data/df_test.csv", header = TRUE, check.names=FALSE, stringsAsFactors = FALSE, sep = ",", strip.white = TRUE)
df_joist_sold_map[,"Date"] <- as.Date(df_joist_sold_map[,"Date"], "%m/%d/%Y")
colnames(df_joist_sold_map)[colnames(df_joist_sold_map)=="takeoffperson"] <- "TakeoffPerson"
#df_joist_quoted_map <- merge(df_listUniqueTakeoffPerson, df_joist_quoted_map, all.y=TRUE, by ="TakeoffPerson")
#df_joist_sold_map <- merge(df_listUniqueTakeoffPerson, df_joist_sold_map, all.y = TRUE, by = "TakeoffPerson")
# Define server logic required to draw a histogram

df_fallon_joist_shipped[,"Weight"] <- df_fallon_joist_shipped[,"Weight"]/2000.0
colnames(df_fallon_joist_shipped)[grep("Weight", colnames(df_fallon_joist_shipped))] <- "Tons"














shinyServer(function(input, output, session) {
  
  output$myPlot <- renderPlotly({
    
    
    start <- input$startDate
    end <- input$endDate
    #start <- as.Date("2013-01-01")
    #end <- as.Date("2014-01-01")
    
    soldJoist <- df_fallon_sold_all
    soldJoist <- soldJoist[soldJoist[,"Date"]>=start & soldJoist[,"Date"]<end,]
    
    if (is.null(input$estimators1) == FALSE)
    {
      soldJoist <- soldJoist[soldJoist$takeoffperson %in% input$estimators1,]
    }
    soldJoist[,"Total Tons_Cumulative"] <- ave(soldJoist$`Total Tons`, FUN = cumsum)
    
    released <- df_fallon_released
    released <- released[released[,"Date"]>=start & released[,"Date"]<end,]
    if (is.null(input$designers1) == FALSE)
    {
      released <- released[released$Designer %in% input$designers1,]
    }
    released[,"Total Tons_Cumulative"] <- ave(released$`Total Tons`, FUN = cumsum)
    
    soldDeck <- df_fallon_deck_sold
    soldDeck <- soldDeck[soldDeck[,"Date"]>=start & soldDeck[,"Date"]<end,]
    soldDeck[,"Tons_Cumulative"] <- ave(soldDeck$`Tons`, FUN = cumsum)
    
    quotedJoist <- df_fallon_quoted
    quotedJoist <- quotedJoist[quotedJoist[,"Date"]>=start & quotedJoist[,"Date"]<end,]
    if (is.null(input$estimators1) == FALSE)
    {
      quotedJoist <- quotedJoist[quotedJoist$TakeoffPerson %in% input$estimators1,]
    }
    quotedJoist[,"Total Tons_Cumulative"] <- ave(quotedJoist$`Total Tons`, FUN = cumsum)
    
    quotedDeck <- df_fallon_deck_quoted_all
    quotedDeck <- quotedDeck[quotedDeck[,"Date"]>=start & quotedDeck[,"Date"]<end,]
    quotedDeck[,"Tons_Cumulative"] <- ave(quotedDeck$`Total Tons`, FUN = cumsum)
    
    shippedJoist <- df_fallon_joist_shipped
    shippedJoist <- shippedJoist[shippedJoist[,"Date"]>=start & shippedJoist[,"Date"]<end,]
    shippedJoist[, "Tons_Cumulative"] <- ave(shippedJoist$Tons, FUN = cumsum)
    
    
    #releasedVsSold <- ggplot()+
      #guides(color=guide_legend(title=NULL))+
      #labs(title="Cumulative Tons") +
      #xlab("Date") + ylab("Tons") +
      #ggthemes::theme_gdocs()
    
    releasedVsSold <- plot_ly(type = "line") %>%
      layout(  title = "Cumulative Tons",
               xaxis = list(title = "Date"), 
               yaxis = list(title = "Tons"))
    
    graphs <- input$graphs
    
    if('Joist Released' %in% graphs == TRUE)
    {
      #releasedVsSold = releasedVsSold + geom_line(data=released, aes(released[,"Date"], released[,"Total Tons_Cumulative"], color="JOIST RELEASED"), size = 0.1)
      releasedVsSold <- add_trace(releasedVsSold, x=released[,"Date"], y = as.numeric(released[,"Total Tons_Cumulative"]), name = "Joist Released", type = "line")
    }
    
    if('Joist Sold' %in% graphs == TRUE)
    {
      releasedVsSold <- add_trace(releasedVsSold, x=soldJoist[,"Date"], y = as.numeric(soldJoist[,"Total Tons_Cumulative"]), name = "Joist Sold", type = "line")
      #releasedVsSold = releasedVsSold + geom_line(data=soldJoist, aes(soldJoist[,"Date"], soldJoist[,"Total Tons_Cumulative"], color="JOIST SOLD"), size = 0.1)
    }
    if('Joist Quoted' %in% graphs == TRUE)
    {
      releasedVsSold <- add_trace(releasedVsSold, x=quotedJoist[,"Date"], y = as.numeric(quotedJoist[,"Total Tons_Cumulative"]), name = "Joist Quoted", type = "line")
      #releasedVsSold = releasedVsSold + geom_line(data=quotedJoist, aes(quotedJoist[,"Date"], quotedJoist[,"Total Tons_Cumulative"], color="JOIST QUOTED"), size = 0.1)
    }
    if('Deck Sold' %in% graphs == TRUE)
    {
      releasedVsSold <- add_trace(releasedVsSold, x=soldDeck[,"Date"], y = as.numeric(soldDeck[,"Tons_Cumulative"]), name = "Deck Sold", type = "line")
      #releasedVsSold = releasedVsSold + geom_line(data=soldDeck, aes(soldDeck[,"Date"], soldDeck[,"Tons_Cumulative"], color="DECK SOLD"), size = 0.1)
    }
    if('Deck Quoted' %in% graphs == TRUE)
    {
      releasedVsSold <- add_trace(releasedVsSold, x=quotedDeck[,"Date"], y = as.numeric(quotedDeck[,"Tons_Cumulative"]), name = "Deck Quoted", type = "line")
      #releasedVsSold = releasedVsSold + geom_line(data=soldDeck, aes(soldDeck[,"Date"], soldDeck[,"Tons_Cumulative"], color="DECK SOLD"), size = 0.1)
    }
    if('Joist Shipped' %in% graphs == TRUE)
    {
      releasedVsSold <- add_trace(releasedVsSold, x=shippedJoist[,"Date"], y = as.numeric(shippedJoist[,"Tons_Cumulative"]), name = "Joist Shipped", type = "line")
      #releasedVsSold = releasedVsSold + geom_line(data=shippedJoist, aes(shippedJoist[,"Date"], shippedJoist[,"Tons_Cumulative"], color="JOIST SHIPPED"), size = 0.1)
    }
    
    #tryCatch({
      #(gg<-ggplotly(releasedVsSold))
    #}, error = function(e) {
    #})
    releasedVsSold
    
    
  })
  
  output$myBarPlot <- renderPlotly({
    
    
    start <- input$startDate
    end <- input$endDate
    interval <- input$interval
    #start <- as.Date("2013-01-01")
    #end <- as.Date("2014-01-01")
    #interval <- "daily"
    
    
    
    designers1 <<- as.character(unique(df_fallon_released$Designer))
    estimators1 <<- as.character(unique(df_fallon_quoted$TakeoffPerson))
    
    #barPlot <- ggplot()+
    #guides(color=guide_legend(title=NULL))+
    #xlab("Date") + ylab("Tons") +
    #ggthemes::theme_gdocs()    
    
    
    barPlot <- plot_ly(type = "bar") %>%
      layout(  xaxis = list(title = "Date"), 
               yaxis = list(title = "Tons"))
    
    
    graphs <- input$graphs
    
    plotTitle <<- ""
    if (input$interval=="daily"){plotTitle <<- "Daily Tons"}
    if (input$interval=="weekly"){plotTitle <<- "Weekly Tons"}
    if (input$interval=="monthly"){plotTitle <<- "Monthly Tons"}
    if (input$interval=="quarterly"){plotTitle <<- "Quarterly Tons"}
    if (input$interval=="yearly"){plotTitle <<- "Yearly Tons"}
    
    if('Joist Released' %in% graphs == TRUE)
    {
      released <- df_fallon_released
      released <- released[released[,"Date"]>=start & released[,"Date"]<=end,]
      if (is.null(input$designers1) == FALSE)
      {
        released <- released[released$Designer %in% input$designers1,]
      }
      released_inter <- SplitByInterval(released,dateColumn="Date", valueColumn = "Total Tons", interval = interval)
      released_inter[,"TYPE"] <- "released"
      colnames(released_inter) <- c("DATE", "TONS")
      barPlot <- add_trace(barPlot, x=released_inter[,1], y = as.numeric(released_inter[,2]), name = "Joist Released", type = "bar")
      #barPlot = barPlot +geom_bar(data=released_inter, aes(released_inter[,1], released_inter[,2], color="JOIST RELEASED"), stat = "identity", size = 2, position = "dodge") 
      #barPlot = barPlot +geom_bar(data=released_inter, aes(released_inter[,1], released_inter[,2], color="JOIST RELEASED"),stat = "identity", position = "dodge")
    }
    
    if('Joist Sold' %in% graphs == TRUE)
    {
      soldJoist <- df_fallon_sold_all
      soldJoist <- soldJoist[soldJoist[,"Date"]>=start & soldJoist[,"Date"]<=end,]
      if (is.null(input$estimators1) == FALSE)
      {
        soldJoist <- soldJoist[soldJoist$takeoffperson %in% input$estimators1,]
      }
      soldJoist_inter <- SplitByInterval(soldJoist,dateColumn="Date", valueColumn = "Total Tons", interval = interval)
      soldJoist_inter[,"TYPE"] <- "soldJoist"
      colnames(soldJoist_inter) <- c("DATE", "TONS")
      barPlot <- add_trace(barPlot, x=soldJoist_inter[,1], y = as.numeric(soldJoist_inter[,2]), name = "Joist Sold", type = "bar")
      #barPlot=barPlot + geom_bar(data=soldJoist_inter, aes(soldJoist_inter[,1], soldJoist_inter[,2],color="JOIST SOLD"), stat = "identity", size = 2, position = "dodge")
      #barPlot = barPlot +geom_bar(data=soldJoist_inter, aes(soldJoist_inter[,1], soldJoist_inter[,2], color="JOIST SOLD"),  stat = "identity", position = "dodge")
    }
    if('Joist Quoted' %in% graphs == TRUE)
    {
      quotedJoist <- df_fallon_quoted
      quotedJoist <- quotedJoist[quotedJoist[,"Date"]>=start & quotedJoist[,"Date"]<=end,]
      if (is.null(input$estimators1) == FALSE)
      {
        quotedJoist <- quotedJoist[quotedJoist$TakeoffPerson %in% input$estimators1,]
      }
      quotedJoist_inter <- SplitByInterval(quotedJoist,dateColumn="Date", valueColumn = "Total Tons", interval = interval)
      quotedJoist_inter[,"TYPE"] <-  "quotedJoist"
      colnames(quotedJoist_inter) <- c("DATE", "TONS")
      barPlot <- add_trace(barPlot, x=quotedJoist_inter[,1], y = as.numeric(quotedJoist_inter[,2]), name = "Joist Quoted", type = "bar")
      #barPlot = barPlot +geom_bar(data=quotedJoist_inter, aes(quotedJoist_inter[,1], quotedJoist_inter[,2], color="JOIST QUOTED"), stat = "identity", size = 2, position = "dodge")
      #barPlot = barPlot +geom_bar(data=quotedJoist_inter, aes(quotedJoist_inter[,1], quotedJoist_inter[,2], color="JOIST QUOTED"),  stat = "identity", position = "dodge")
    }
    if('Deck Sold' %in% graphs == TRUE)
    {
      soldDeck <- df_fallon_deck_sold
      soldDeck <- soldDeck[soldDeck[,"Date"]>=start & soldDeck[,"Date"]<=end,]
      soldDeck_inter <- SplitByInterval(soldDeck,dateColumn="Date", valueColumn = "Tons", interval = interval)
      soldDeck_inter[,"TYPE"] <- "soldDeck"
      colnames(soldDeck_inter) <- c("DATE", "TONS")
      barPlot <- add_trace(barPlot, x=soldDeck_inter[,1], y = as.numeric(soldDeck_inter[,2]), name = "Deck Sold", type = "bar")
      #barPlot = barPlot +geom_bar(data=soldDeck_inter, aes(soldDeck_inter[,1], soldDeck_inter[,2], color="DECK SOLD"), stat = "identity", size = 2, position = "dodge")
      #barPlot = barPlot +geom_bar(data=soldDeck_inter, aes(soldDeck_inter[,1], soldDeck_inter[,2], color="DECK SOLD"),  stat = "identity", position = "dodge")
    }
    if('Deck Quoted' %in% graphs == TRUE)
    {
      quotedDeck <- df_fallon_deck_quoted_all
      quotedDeck <- quotedDeck[quotedDeck[,"Date"]>=start & quotedDeck[,"Date"]<=end,]
      quotedDeck_inter <- SplitByInterval(quotedDeck,dateColumn="Date", valueColumn = "Tons", interval = interval)
      quotedDeck_inter[,"TYPE"] <- "quotedDeck"
      colnames(quotedDeck_inter) <- c("DATE", "TONS")
      barPlot <- add_trace(barPlot, x=quotedJoist_inter[,1], y = as.numeric(quotedJoist_inter[,2]), name = "Deck Sold", type = "bar")
      #barPlot = barPlot +geom_bar(data=quotedDeck_inter, aes(quotedDeck_inter[,1], quotedDeck_inter[,2], color="DECK QUOTED"), stat = "identity", size = 2, position = "dodge")
      #barPlot = barPlot +geom_bar(data=quotedDeck_inter, aes(quotedDeck_inter[,1], quotedDeck_inter[,2], color="DECK QUOTED"),  stat = "identity", position = "dodge")
    }
    if('Joist Shipped' %in% graphs == TRUE)
    {
      joistShipped <- df_fallon_joist_shipped
      joistShipped <- joistShipped[joistShipped[,"Date"]>=start & joistShipped[,"Date"]<=end,]
      joistShipped_inter <- SplitByInterval(joistShipped,dateColumn="Date", valueColumn = "Tons", interval = interval)
      joistShipped_inter[,"TYPE"] <- "joistShipped"
      colnames(joistShipped_inter) <- c("DATE", "TONS")
      barPlot <- add_trace(barPlot, x=joistShipped_inter[,1], y = as.numeric(joistShipped_inter[,2]), name = "Joist Shipped", type = "bar")
      #barPlot = barPlot +geom_bar(data=joistShipped_inter, aes(joistShipped_inter[,1], joistShipped_inter[,2], color="JOIST SHIPPED"), stat = "identity", size = 2, position = "dodge")
      #geom_point(data = joistShipped_inter, aes(joistShipped_inter[,1], joistShipped_inter[,2], color="JOIST SHIPPED"), size =1.5)
      #barPlot = barPlot +geom_bar(data=joistShipped_inter, aes(joistShipped_inter[,1], joistShipped_inter[,2], color="DECK QUOTED"),  stat = "identity", position = "dodge")
    }
    barPlot <-  barPlot %>%
      layout(title = plotTitle)
    #labs(title=plotTitle)
    
    #tryCatch({
    #(gg<-ggplotly(barPlot))
    #}, error = function(e) {
    
    #})
    
    
    barPlot
    
    #p <- plot_ly() #x=joistShipped_inter[,1], y=as.numeric(joistShipped_inter[,2]), name = "Shipped", type = "bar")
    #p <- add_trace(p, x=soldJoist_inter[,1], y = as.numeric(soldJoist_inter[,2]), name = "Joist Sold", type = "bar")
    #p
    
  })
  
  output$map <- renderLeaflet({
    
    if(input$whatToMap == "joistQuoted")
    {
      df_test <- df_joist_quoted_map
    }
    if(input$whatToMap == "joistSold")
    {
      df_test <- df_joist_sold_map
    }
    
    
    start_map <- input$startDate_Map
    end_map <- input$endDate_Map
    #start_map <- as.Date("2016-01-01")
    #end_map <- as.Date("2017-01-01")
    df_test <- df_test[df_test[,"Date"]>=start_map & df_test[,"Date"]<end_map,]
    df_listUniqueTakeoffPerson <<- data.frame(unique(df_test$TakeoffPerson))
    colnames(df_listUniqueTakeoffPerson) <- "TakeoffPerson"
    df_listUniqueTakeoffPerson[,"Color"] <- df_colors[1:nrow(df_listUniqueTakeoffPerson),1]
    estimators2 <<- as.character(unique(df_fallon_quoted$TakeoffPerson))
    df_test <- merge(df_listUniqueTakeoffPerson, df_test, all.y=TRUE, by ="TakeoffPerson")
    if (is.null(input$estimators2) == FALSE)
    {
      df_test <- df_test[df_test$TakeoffPerson %in% input$estimators2,]
    }
    
    df_test <- df_test[order(-df_test[,"Total Tons"]),]
    row.names(df_test) <- 1:nrow(df_test)
    m <- leaflet(data = df_test[,]) %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      #addMarkers(~LONG, ~LAT, popup = as.character(paste(df_test$`Job Name`,df_test$`Total Tons`, sep="\n")) )
      addCircles(lng = df_test$LONG, lat = df_test$LAT,color=~df_test$`Color`, radius = ~160*df_test$`Total Tons`^.7, popup = as.character(paste(df_test$`Job Name`,df_test$`Total Tons`, sep="\n")) )%>%
      addLegend(position = "bottomright", labels=~df_listUniqueTakeoffPerson$`TakeoffPerson`, colors = ~df_listUniqueTakeoffPerson$`Color`) %>%
      addProviderTiles("CartoDB.Positron")
    m
  })
  
  output$estimators1 <- renderUI({
    selectizeInput("estimators1", label = h5("Filter Estimator:"), choices = list(
      Sales = estimators1
    ), multiple = TRUE)
  })
  
  output$designers1 <- renderUI({
    selectizeInput("designers1", label = h5("Filter Designer:"), choices = list(
      Sales = designers1
    ), multiple = TRUE)
  })
  
  output$estimators2 <- renderUI({
    selectizeInput("estimators2", label = h5("Filter Estimator:"), choices = list(
      Sales = estimators2
    ), multiple = TRUE)
  })
  
  output$dataTableInfo <- renderPrint({
    nrow(input$joistSoldTable)
  })
  
  output$joistSoldTable <-  renderDataTable(
    df_fallon_sold_all
  )
  output$joistQuotedTable = renderDataTable({
    df_fallon_quoted
  })
  output$deckSoldTable = renderDataTable({
    df_fallon_deck_sold
  })
  output$deckQuotedTable = renderDataTable({
    df_fallon_deck_quoted_all
  })
  output$joistReleasedTable = renderDataTable({
    df_fallon_released
  })
  output$joistShippedTable = renderDataTable({
    df_fallon_joist_shipped
  })
  
  
})

