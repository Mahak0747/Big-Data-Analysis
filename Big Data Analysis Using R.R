# Q1
video_game_data <- read.csv("F:/vgsales.csv")
sales_by_genre <- aggregate(Global_Sales ~ Genre, data = video_game_data, sum)
sales_percentages <- round(100 * sales_by_genre$Global_Sales / sum(sales_by_genre$Global_Sales), 1)
labels <- paste(sales_by_genre$Genre, sales_percentages, "%")
pie(sales_by_genre$Global_Sales,
    labels = labels,
    main = "Video Game Sales by Genre",
    col = rainbow(length(sales_by_genre$Genre))
)
highest_sales_genre <- sales_by_genre[which.max(sales_by_genre$Global_Sales), ]
cat("Genre with the highest sales:\n")
print(highest_sales_genre)


# Q2
library(ggplot2)
library(dplyr)
library(lubridate)
data <- read.csv("F:/SeoulBikeData.csv")
data$Date <- dmy(data$Date) 
data$Month <- floor_date(data$Date, "month") 
# 1. The line charts of rented bike count over the months. 
monthly_data_count <- aggregate(data$Rented.Bike.Count, by = list(data$Month), sum) 
colnames(monthly_data_count) <- c("Month", "Rented.Bike.Count")
ggplot(monthly_data_count, aes(x = Month, y = Rented.Bike.Count)) + 
  geom_line(color = "blue") + 
  labs(title = "Rented Bike Count Over Months", x = "Month", y = "Rented.Bike.Count") + 
  theme_minimal()
# 2. The line charts of temperature over the months.
monthly_data_temp <- aggregate(data$Temperature..C., by = list(data$Month), mean) 
colnames(monthly_data_temp) <- c("Month", "Temperature.C")
ggplot(monthly_data_temp, aes(x = Month, y = Temperature.C)) + 
  geom_line(color = "red") + 
  labs(title = "Average Temperature Over Months", x = "Month", y = "Temperature (°C)") + 
  theme_minimal()
# 3. The pie chart of rented bike count by seasons. 
season_data <- aggregate(data$Rented.Bike.Count, by = list(data$Seasons), sum)
colnames(season_data) <- c("Seasons", "Rented.Bike.Count")
ggplot(season_data, aes(x = "", y = Rented.Bike.Count, fill = Seasons)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Rented Bike Count by Seasons") +
  theme_void() +
  theme(legend.position = "right")
# 4. The stacked bar chart of rented bike count by the holiday.
holiday_data <- aggregate(data$Rented.Bike.Count, by = list(data$Holiday), sum) 
colnames(holiday_data) <- c("Holiday", "Rented.Bike.Count")
ggplot(holiday_data, aes(x = Holiday, y = Rented.Bike.Count, fill = Holiday)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Rented Bike Count by Holiday", x = "Holiday", y = "Rented Bike Count") + 
  theme_minimal() + 
  theme(legend.position = "none")
# 5. The scatter plot of rented bike count and temperature.
ggplot(data, aes(x = Temperature..C., y = Rented.Bike.Count)) + 
  geom_point(color = "blue") + 
  labs(title = "Scatter Plot of Rented Bike Count vs Temperature", x = "Temperature (°C)", y = "Rented Bike Count") + 
  theme_minimal()
# 6. The scatter plot of rented bike count and rainfall.
ggplot(data, aes(x = Rainfall.mm., y = Rented.Bike.Count)) + 
  geom_point(color = "blue") + 
  labs(title = "Scatter Plot of Rented Bike Count vs Rainfall", x = "Rainfall (mm)", y = "Rented Bike Count") + 
  theme_minimal()


# Q3 Data set : CallVoiceQualityExperience-2018-April
library(reshape2)
library(ggplot2)
data <- read.csv("F:/CallVoiceQualityExperience-2018-April.csv")
# 1. Vertical bar chart of average call quality rate per operator. 
average_rating <- aggregate(data$Rating, by = list(data$Operator), mean)
colnames(average_rating) <- c("Operator", "Average_Rating")
ggplot(average_rating, aes(x = Operator, y = Average_Rating, fill = Operator)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Call Quality Rate per Operator",
       x = "Operator",
       y = "Average Call Quality Rate") +
  theme_minimal() +
  theme(legend.position = "none")
# 2. The vertical bar chart of the quality level per each state in India. 
state_data <- aggregate(data$Rating, by = list(data$State.Name), mean)
colnames(state_data) <- c("State.Name", "Average_Rating")
ggplot(state_data, aes(x = State.Name, y = Average_Rating, fill = State.Name)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Quality Level per State in India",
       x = "State",
       y = "Average Rating") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))
# 3. The vertical bar chart showing the relationship between the call quality and the network type. 
network_data <- aggregate(data$Rating, by = list(data$Network.Type), mean)
colnames(network_data) <- c("Network.Type", "Average_Rating")
ggplot(network_data, aes(x = Network.Type, y = Average_Rating, fill = Network.Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Call Quality by Network Type",
       x = "Network Type",
       y = "Average Call Quality Rating") +
  theme_minimal() +
  theme(legend.position = "none")
# 4. Horizontal bar chart of average call quality rate per Call Drop Category 
category_data <- aggregate(data$Rating, by = list(data$Call.Drop.Category), mean)
colnames(category_data) <- c("Call.Drop.Category", "Average_Rating")
ggplot(category_data, aes(x = Average_Rating, y = Call.Drop.Category, fill = Call.Drop.Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Call Quality Rate per Call Drop Category",
       x = "Average Rating",
       y = "Call Drop Category") +
  theme_minimal() +
  theme(legend.position = "none")
# 5. Heat map between state , Network Type and rating.
heatmap_data <- aggregate(Rating ~ State.Name + Network.Type, data = data, mean)
heatmap_data_wide <- dcast(heatmap_data, State.Name ~ Network.Type, value.var = "Rating")
ggplot(melt(heatmap_data_wide, id.vars = "State.Name"), aes(x = variable, y = State.Name, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red", na.value = "white") +
  labs(title = "Heat Map of Average Call Quality Rating by State and Network Type",
       x = "Network Type",
       y = "State",
       fill = "Average Rating") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 6. Vertical bar chart of average call quality rate per Indoor_Outdoor_Travelling . 
travelling_data <- aggregate(data$Rating, by = list(data$Indoor_Outdoor_Travelling), mean)
colnames(travelling_data) <- c("Indoor_Outdoor_Travelling", "Average_Rating")
ggplot(travelling_data, aes(x = Indoor_Outdoor_Travelling, y = Average_Rating, fill = Indoor_Outdoor_Travelling)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Call Quality Rate per Indoor_Outdoor_Travelling Category",
       x = "Indoor/Outdoor/Travelling",
       y = "Average Call Quality Rating") +
  theme_minimal() +
  theme(legend.position = "none")







# Q3 Data set : CallVoiceQuality_Data_2018_May
library(reshape2)
library(ggplot2)
data <- read.csv("F:/CallVoiceQuality_Data_2018_May.csv")
# 1. Vertical bar chart of average call quality rate per operator. 
average_rating <- aggregate(data$Rating, by = list(data$Operator), mean)
colnames(average_rating) <- c("Operator", "Average_Rating")
ggplot(average_rating, aes(x = Operator, y = Average_Rating, fill = Operator)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Call Quality Rate per Operator",
       x = "Operator",
       y = "Average Call Quality Rate") +
  theme_minimal() +
  theme(legend.position = "none")
# 2. The vertical bar chart of the quality level per each state in India. 
state_data <- aggregate(data$Rating, by = list(data$State.Name), mean)
colnames(state_data) <- c("State.Name", "Average_Rating")
ggplot(state_data, aes(x = State.Name, y = Average_Rating, fill = State.Name)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Quality Level per State in India",
       x = "State",
       y = "Average Rating") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))
# 3. The vertical bar chart showing the relationship between the call quality and the network type. 
network_data <- aggregate(data$Rating, by = list(data$Network.Type), mean)
colnames(network_data) <- c("Network.Type", "Average_Rating")
ggplot(network_data, aes(x = Network.Type, y = Average_Rating, fill = Network.Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Call Quality by Network Type",
       x = "Network Type",
       y = "Average Call Quality Rating") +
  theme_minimal() +
  theme(legend.position = "none")
# 4. Horizontal bar chart of average call quality rate per Call Drop Category 
category_data <- aggregate(data$Rating, by = list(data$Call.Drop.Category), mean)
colnames(category_data) <- c("Call.Drop.Category", "Average_Rating")
ggplot(category_data, aes(x = Average_Rating, y = Call.Drop.Category, fill = Call.Drop.Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Call Quality Rate per Call Drop Category",
       x = "Average Rating",
       y = "Call Drop Category") +
  theme_minimal() +
  theme(legend.position = "none")
# 5. Heat map between state , Network Type and rating.
heatmap_data <- aggregate(Rating ~ State.Name + Network.Type, data = data, mean)
heatmap_data_wide <- dcast(heatmap_data, State.Name ~ Network.Type, value.var = "Rating")
ggplot(melt(heatmap_data_wide, id.vars = "State.Name"), aes(x = variable, y = State.Name, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red", na.value = "white") +
  labs(title = "Heat Map of Average Call Quality Rating by State and Network Type",
       x = "Network Type",
       y = "State",
       fill = "Average Rating") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 6. Vertical bar chart of average call quality rate per Indoor_Outdoor_Travelling . 
travelling_data <- aggregate(data$Rating, by = list(data$In.Out.Travelling), mean)
colnames(travelling_data) <- c("In.Out.Travelling", "Average_Rating")
ggplot(travelling_data, aes(x = In.Out.Travelling, y = Average_Rating, fill = In.Out.Travelling)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Call Quality Rate per In/Out/Travelling Category",
       x = "In/Out/Travelling",
       y = "Average Call Quality Rating") +
  theme_minimal() +
  theme(legend.position = "none")