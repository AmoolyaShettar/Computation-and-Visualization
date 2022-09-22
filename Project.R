
install.packages("dplyr")
library(dplyr)

install.packages("tidyr")
library(tidyr)

install.packages("corrgram")
library(corrgram)

install.packages("ggplot2")
library(ggplot2)

install.packages("gridExtra")
library(gridExtra)

install.packages("ggpubr")
library("ggpubr")

install.packages("treemap")
library(treemap)

install.packages("tidyverse")
library(tidyverse)

library(lubridate)

install.packages("chron")
library(chron)

install.packages("ggwordcloud")
library(ggwordcloud)

install.packages("leaflet")
library(leaflet)

library(ggrepel)

#===================================================================================================

#Reading the csv  file
df <- read.csv('Police_Department_Incidents_-_Previous_Year__2016_.csv')


#-------------------------------------------------------------------------------------------------------

#Checking for null values in the dataset

#sum(is.na(df$Category))

sum(is.na(df))

#---------------------------------------------------------------------------------------------------------

#Number of categories

categories <- unique(df$Category)
number <- length(categories)

#=========================================================================================
# MAP OF SAN FRANCISCO

df$popup <- paste("<b>Incident #: </b>", df$IncidntNum, "<br>", "<b>Category: </b>", df$Category,
                  "<br>", "<b>Description: </b>", df$Descript,
                  "<br>", "<b>Day of week: </b>", df$DayOfWeek,
                  "<br>", "<b>Date: </b>", df$Date,
                  "<br>", "<b>Time: </b>", df$Time,
                  "<br>", "<b>PD district: </b>", df$PdDistrict,
                  "<br>", "<b>Resolution: </b>", df$Resolution,
                  "<br>", "<b>Address: </b>", df$Address,
                  "<br>", "<b>Longitude: </b>", df$X,
                  "<br>", "<b>Latitude: </b>", df$Y)

leaflet(df, width = "100%") %>% addTiles() %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(provider = "Esri.WorldStreetMap",group = "World StreetMap") %>%
  addProviderTiles(provider = "Esri.WorldImagery",group = "World Imagery") %>%
  # addProviderTiles(provider = "NASAGIBS.ViirsEarthAtNight2012",group = "Nighttime Imagery") %>%
  addMarkers(lng = ~X, lat = ~Y, popup = df$popup, clusterOptions = markerClusterOptions()) %>%
  addLayersControl(
    baseGroups = c("OSM (default)","World StreetMap", "World Imagery"),
    options = layersControlOptions(collapsed = FALSE)
  )

#===============================================================================

df_theft_daily <- df %>%
  mutate(Date = as.Date(Date, "%m/%d/%Y")) %>%
  group_by(Date) %>%
  summarize(count = n()) %>%
  arrange(Date)

str(df_theft_daily)

#m_count <- month(as.POSIXlt(df_theft_daily$Date, format="%d/%m/%Y"))

count_monthwise <- aggregate(df_theft_daily$count, by=list(Category=month(as.POSIXlt(df_theft_daily$Date, format="%d/%m/%Y"))), FUN=sum)


plot(count_monthwise$Category, count_monthwise$x, type = "l", lty = 1, col = "red", xlab = "Months", ylab = "Count") 

#==========================================================================================


#Day-wise crime
#--------------------------- 1----------------------------------------------------------


df_day <- sort(table(df$DayOfWeek), decreasing = TRUE)
df_day <- data.frame(df_day[df_day > 0])
colnames(df_day) <- c("Day", "Count")

bp1<-ggplot(df_day, aes(x=Day, y=Count)) + geom_bar(stat="identity", fill="red")

plot(df_day$Count, df_day$Day, type = "l", lty = 1, col = "red", xlab = "Count", ylab = "Day", xlim = c(5000,20000)) 

bp1

#---------------------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------------------------
# WORD CLOUD

number_of_cases <- df %>% 
  group_by(Category) %>% 
  summarise(number_count = n()) %>%
  arrange(desc(number_count))

ggplot(number_of_cases, aes(label = Category, size = number_count)) +
  geom_text_wordcloud_area(area_corr_power = 1) +
  scale_size_area(max_size = 24) +
  theme_minimal()


#----------------------------2----------------------------------
#Frequency

district_S <- df[df$PdDistrict == "SOUTHERN", ]
df_S <- sort(table(district_S$Category),decreasing = TRUE) 
df_S <- data.frame(df_S[df_S > 0])
colnames(df_S) <- c("Category", "Frequency")
df_S$Percentage <- df_S$Frequency / sum(df_S$Frequency)


#Box plot

boxplot_S <- ggplot(df_S, aes(y = Frequency)) + 
  labs(y = "Value", x = "Category") + 
  geom_boxplot()
boxplot_S

#---------------------------------------------------------------------------------------------------------

district_I <- df[df$PdDistrict == "INGLESIDE", ]
df_I <- sort(table(district_I$Category),decreasing = TRUE) 
df_I <- data.frame(df_I[df_I > 0])
colnames(df_I) <- c("Category", "Frequency")
df_I$Percentage <- df_I$Frequency / sum(df_I$Frequency)


#Box plot

boxplot_I <- ggplot(df_I, aes(y = Frequency)) + 
  labs(y = "Value", x = "Category") + 
  geom_boxplot()
boxplot_I

#---------------------------------------------------------------------------------------------------------

district_P <- df[df$PdDistrict == "PARK", ]
df_P <- sort(table(district_P$Category),decreasing = TRUE) 
df_P <- data.frame(df_P[df_P > 0])
colnames(df_P) <- c("Category", "Frequency")
df_P$Percentage <- df_P$Frequency / sum(df_P$Frequency)


#Box plot

boxplot_P <- ggplot(df_P, aes(y = Frequency)) + 
  labs(y = "Value", x = "Category") + 
  geom_boxplot()
boxplot_P


#=====================================================================================================

#Selecting Theft category
df_theft <- df %>% filter(grepl("LARCENY/THEFT", Category))

#Function for hour-wise
get_hour <- function(x) {
  return (as.numeric(strsplit(x,":")[[1]][1]))
}

#Checking theft count hour-wise  
df_theft_time <- df_theft %>%
  mutate(Hour = sapply(Time, get_hour)) %>%
  group_by(DayOfWeek, Hour) %>%
  summarize(count = n())


#Put in proper format
day_format <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
hour_format <- c(paste(c(12,1:11),"AM"), paste(c(12,1:11),"PM"))

df_theft_time$DayOfWeek <- factor(df_theft_time$DayOfWeek, level = rev(day_format))
df_theft_time$Hour <- factor(df_theft_time$Hour, level = 0:23, label = hour_format)


#Heat map plot

plot <- ggplot(df_theft_time, aes(x = Hour, y = DayOfWeek, fill = count)) +
  geom_tile() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6), legend.title = element_blank(), legend.position="top", legend.direction="horizontal", legend.key.width=unit(2, "cm"), legend.key.height=unit(0.25, "cm"), legend.spacing=unit(-0.5,"cm"), panel.spacing=element_blank()) +
  labs(x = "Hour of Theft", y = "Day of Week of Theft", title = "Number of Thefts in San Francisco, by Time of Theft") +
  scale_fill_gradient(low = "white", high = "#27AE60")

#Displaying the graph
plot

#===============================================================================

# TREE MAP

#set.seed(42)

district_count <- df %>% 
  group_by(PdDistrict) %>% 
  summarise(number_count = n()) %>%
  arrange(desc(number_count))

p <- treemap(district_count,
             index=c("PdDistrict","number_count"),
             vSize="number_count",
             type="index",
             palette = "Reds",
             bg.labels=c("white"),
)

#----------------------------------4-----------------------------------------
#Districts with highest crimes

df_district <- df %>%
  group_by(PdDistrict, Category, Time) %>%
  summarize(count = n())


bp2<-ggplot(df_district, aes(x=PdDistrict, y=count, fill=Category)) + geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 

bp2

#-------------------------------------------------------------------------------------------

df_SOUTHERN <- df_district %>%
  filter(PdDistrict == "SOUTHERN")

bp3<-ggplot(df_SOUTHERN, aes(x=Category, y=count, fill=Category)) + geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

bp3
#TRYFOR SOME OTHER GRAPH
#---------------------------3-----------------------------------------------------------

#===================================================================================

df_resolution <- df %>% 
  filter(PdDistrict == "SOUTHERN") %>%
  group_by(Resolution) %>%
  summarize(count = n()) 

ggplot(df_resolution, aes(x="", y=count, fill=Resolution)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)


#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------
#What more can be done
#1.highest crime in that top crime rated district and punishment served there
#2.Grid wise compare(Refer visualization part2)
#3 correlation (Refer visualization 2)

#===================================================================================================
#===============================================================================

#Resolution vs Category in Southern

distr <- df[df$PdDistrict == "SOUTHERN", ]

g1 <- ggplot(data = df, aes(x = Category, y = Resolution))
g2 <- g1 + geom_count(color = "darkgreen")
g2

#=====================================================================================


# IN SOUTHERN, CHECK FOR NON CRIMINAL, ASSAULT AND OTHER OFFENCES IN DETAIL


# Density spread of top 3 Categories other than Theft
distr <- df[df$PdDistrict == "SOUTHERN", ]

crimeList <- c("ASSAULT", "OTHER OFFENSES", "NON-CRIMINAL")
categ <- distr[distr$Category %in% crimeList, ]

g2 <- ggplot(data = categ, aes(x = X, y = Y, colour = Category))
g2 + geom_density2d()

#---------------------------------------------------------------------------------------------------------

df_area <- df %>%
  group_by(Address) %>%
  summarize(count = n())



#=================================================================================

# EXTRA

df_resolution <- df %>% 
  filter(PdDistrict == "SOUTHERN") %>%
  group_by(Resolution) %>%
  summarize(count = n()) 

distr <- df[df$PdDistrict == c("SOUTHERN", "BAYVIEW", "PARK"), ]
crimeList <- c("ASSAULT", "OTHER OFFENSES", "NON-CRIMINAL")
categ <- distr[distr$Category %in% crimeList, ]
categ <- distr[distr$Category %in% crimeList, ]
df_catego <- categ %>% select(PdDistrict, Category)

df_catt <- df_catego%>% group_by(PdDistrict) %>% summarise(count=n())








