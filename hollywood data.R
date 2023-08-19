

#import library
library(tidyverse)

#check for data types
str(hwdf)

dim(hwdf)

#check for missing values
colSums(is.na(hwdf))

#Drop missing values
drops <- c('X_input')

df <- hwdf[ ,!(names(hwdf) %in% drops)]

# check to make sure that the column has been removed
head(hwdf)

#Remove missing values in rows
df <- na.omit(df)

#check for missing values
colSums(is.na(df))


#Check for duplicates and removing them

df <-df %>% distinct(Film, .keep_all = TRUE)
df<-df%>%  filter(!row_number() %in% c(38))

dim(df)

#round off values to 2 places
df$Profitability <- round(df$Profitability ,digit=2)
df$Worldwide.Gross <- round(df$Worldwide.Gross,digit=2)
head(df)

#Check for outliers using a boxplot

library(ggplot2)

#Create a boxplot that highlights the outliers
ggplot(df,aes(x=Profitability, y=Worldwide.Gross)) +geom_boxplot(outlier.colour= "red",outlier.shape= 1)+scale_x_continuous(labels = scales::comma)+coord_cartesian(ylim= c(0,1000))

#Remove outliers in 'Profitability'
# Calculate the IQR for the Profitability column
Q1 <- quantile(df1$Profitability, 0.25)
Q3 <- quantile(df1$Profitability, 0.75)
IQR <- Q3 - Q1

no_outliers <- subset(df, df$Profitability> (Q1 - 1.5*IQR) & df$Profitability< (Q3 + 1.5*IQR))

dim(no_outliers)

# Remove outliers in 'Worldwide.Gross'
Q1 <- quantile(no_outliers$Worldwide.Gross, .25)
Q3 <- quantile(no_outliers$Worldwide.Gross, .75)
IQR <- IQR(no_outliers$Worldwide.Gross)

df1 <- subset(no_outliers, no_outliers$Worldwide.Gross> (Q1 - 1.5*IQR) & no_outliers$Worldwide.Gross< (Q3 + 1.5*IQR))

dim(df1)



#bivariate analysis
ggplot(no_outliers, aes(x=Genre, y=Profitability)) + geom_point()+ scale_y_continuous(labels=scales::comma)+coord_cartesian(ylim = c(0, 15))


2#scatterplot
ggplot(df1, aes(x=Lead.Studio, y=Rotten.Tomatoes..)) + geom_point()+ scale_y_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0, 110))+theme(axis.text.x = element_text(angle = 90))

#bar chart
ggplot(df1, aes(x=Year)) + geom_bar()

ggplot(df1, aes(x=Genre)) + geom_bar()

#average rotten tomatoes ratings by genre

average_ratings_by_genre <- df1 %>%
  group_by(Genre) %>%
  summarise(Average_Rotten_Tomatoes_Rating = mean(Rotten.Tomatoes.., na.rm = TRUE))

ggplot(average_ratings_by_genre, aes(x = Genre, y = Average_Rotten_Tomatoes_Rating)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Average Rotten Tomatoes Ratings by Genre",
       x = "Genre",
       y = "Average Rotten Tomatoes Rating")

# Plotting line chart of average Rotten Tomatoes ratings per genre
ggplot(df1, aes(x = Year, y = as.numeric(Rotten.Tomatoes), color = Genre, group = Genre)) +
  geom_line() +
  labs(x = "Year", y = "Average Rotten Tomatoes Rating", title = "Average Rotten Tomatoes Ratings per Genre Over the Years") +
  theme_minimal() +
  theme(legend.position = "top")



# Average Profitability per Studio
ggplot(data = df1, aes(x = Lead.Studio, y = Profitability)) +
  geom_bar(stat = "summary", fun = "mean", fill = "red") +
  labs(title = "Average Profitability per Studio")

# Aggregate the data to get counts of each audience score
score_counts <- table(df1$Audience..score..)


# Using typeof()
data_type_typeof <- typeof(df1$Audience..score..)
print(data_type_typeof)

# Check the data range of AudienceScore
audience_score_range <- range(df1$AudienceScore)
print(audience_score_range)



# Convert AudienceScore to numeric
df1$Audience..score.. <- as.numeric(df1$Audience..score..)

# Plotting audience score against film
ggplot(df1, aes(x = Film, y = Audience..score..)) +
  geom_point() +
  labs(x = "Film", y = "Audience Score", title = "Audience Score for Each Film") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



#bar chart for total worldwide gross per genre
ggplot(data = df1, aes(x = Genre, y = Worldwide.Gross)) +
  geom_bar(stat = "summary", fun = "sum", fill = "orange") +
  labs(title = "Total Worldwide Gross per Genre")

# Plotting line chart of worldwide gross per genre
ggplot(df1, aes(x = Year, y = Worldwide.Gross, color = Genre, group = Genre)) +
  geom_line() +
  labs(x = "Year", y = "Worldwide Gross", title = "Worldwide Gross per Genre Over the Years") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
  theme_minimal() +
  theme(legend.position = "top")



#  pie chart for distrubution of movies across genres
ggplot(number_of_movies_per_genre, aes(x = "", y = Count_Movies, fill = Genre)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Distribution of Movies Across Genres",
       fill = "Genre") +
  theme_void()


#number of movies per year
number_of_movies_per_year <- df1%>%
  group_by(Year) %>%
  summarise(Count_Movies_Produced = n())

ggplot(number_of_movies_per_year, aes(x = Year, y = Count_Movies_Produced)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Number of Movies Produced per Year",
       x = "Year",
       y = "Number of Movies Produced")



# Plotting histogram of number of movies produced per year
ggplot(df1, aes(x = as.numeric(Year))) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(x = "Year", y = "Number of Movies", title = "Number of Movies Produced per Year") +
  scale_x_continuous(breaks = seq(min(df1$Year), max(df1$Year), by = 1)) +
  theme_minimal()






write.csv(df1, "clean_df.csv")

