'''
Name of DS: Mr.GODSON SALVATORY MATEMU (TP067389)
Name of R program: CP2_TP067389.r
Description: A personalized subscription model using Machine Learning.
Date first written:22nd September 2022
Date last updated:8th December 2022

'''
#Importing the data set. 
library(readxl)
df <- read_excel("Divvydataset.xlsx")
View(df)

#Data exploration
summary(df)
str(df)
attach(df)

#Univariate analysis
rideable_type <- as.factor(rideable_type)
summary(rideable_type)
plot(rideable_type,col=5)

summary(started_at)
summary(ended_at)

start_station_name <- as.factor(start_station_name)
summary(start_station_name)
plot(start_station_name)
end_station_name <- as.factor(end_station_name)
plot(end_station_name)
summary(end_station_name)

member_casual <- as.factor(member_casual)
summary(member_casual)
plot_str(member_casual,col=5)

plot(gender,col=5)
summary(gender)

#Feature engineering
# Calculating geo-spatial distance between two points
distance = data.frame()
for (i in 1:228496) {
  my_points <- matrix(c(df$start_lng[i], df$end_lng[i],    # Create longitude/latitude matrix
                        df$start_lat[i], df$end_lat[i]),
                    nrow = 2)
  distance <- distHaversine(my_points)
  df <- rbind(df,distance)}
summary(distance)
df$distance <- distance

# Calculating time difference between two time interval
time <- difftime(df$ended_at,df$started_at, units = "mins")
df$time <- time
summary(time)
boxplot(time,horizontal = F,col = 5)

#Creating a new variable speed
speed <- df$distance/(df$time*60)
summary(speed)

#Binning speed into three levels
level <- cut(speed, breaks = c(-Inf,2.051,3.648,Inf), labels = c("low","medium","high"))
table(level)



#Bivariate graphical exploration.
ggplot(data = df, aes(x = rideable_type, fill = level)) +
  geom_bar(position = "dodge", stat = "count") +
  theme_classic()


ggplot(data = df, aes(x = rideable_type, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "count") +
  theme_classic()


ggplot(data = df, aes(x = level, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "count") +
  theme_classic()


plot(distance~ rideable_type, data = df)
tapply(df$distance,df$rideable_type, mean)
tapply(df$distance,df1$rideable_type, sd)


plot(time~ rideable_type, data = df)
tapply(df$time,df$rideable_type, mean)
tapply(df$time,df$rideable_type, sd)


plot(time ~ distance, data = df, pch=as.integer(rideable_type))

ggplot(data = df, aes(x = distance,
                       y = time,
                       colour = rideable_type )) +
  geom_point() +
  geom_smooth(method = lm,
              se = F) +
  theme_minimal()

plot(time ~ distance, data = df, pch=as.integer(rideable_type))

ggplot(data = df, aes(x = distance,
                       y = time,
                       colour = member_casual )) +
  geom_point() +
  geom_smooth(method = lm,
              se = F) +
  theme_minimal()


#Data preparation for clustering
df$riderid <- as.factor(df$riderid)
times <- aggregate(time ~ riderid, df, sum)
distance <- aggregate(distance ~ riderid, cw, sum)
table(df$riderid)
rfreq <- table(df$riderid)
frequency <- rfreq

dataa <- cbind(distance,frequency,times)


#Clustering
library(factoextra)
#Scale the dataset
dataa_scale <- scale(dataa)

#distance between the data points
dat <- dist(dataa_scale)

#Calculating the number of clusters
#within sum squares
fviz_nbclust(dataa_scale, kmeans, method = "wss") +
  labs(subtitle = "elbow method")

#k-means
km.out <- kmeans(dataa_scale, centers = 4, nstart = 100)
print(km.out)

sil <- silhouette(km.out$cluster, dist(dataa_scale))
fviz_silhouette(sil)


#visualizing the clustering algorithms
km.clusters <- km.out$cluster

#row.names(dataa_scale) <- km.clusters
fviz_cluster(list(data = dataa_scale, cluster = km.clusters))
table(km.clusters)
summary(km.clusters)
km.clusters

#Hierarchial clustering
hc_out <- hclust(dat,method = "complete")
hc_in <- hclust(dat, method = "ward.D")


#dendogram
plot(hc_out)
rect.hclust(hc_out, k = 4, border = 2:6)

plot(hc_in)
rect.hclust(hc_out, k = 4, border = 2:6)

#clusters
hc.clusters <- cutree(hc_out, k=4)
summary(hc.clusters)

#visualize
fviz_cluster(list(data=dataa_scale, cluster = hc.clusters ))
table(hc.clusters)

#Removing outliers
dataa[c(71,144,138,139),]
summary(dataa)
new_data_scale <- dataa_scale[-c(71,144,138,139),]
new_dat <- dat

#Calculating the number of clusters
#within sum squares
fviz_nbclust(new_data_scale, kmeans, method = "wss") +
  labs(subtitle = "elbow method")

#kmeans
km.out <- kmeans(new_data_scale, centers = 3, nstart = 100)
print(km.out)

#validation
sil <- silhouette(km.out$cluster, dist(new_data_scale))
fviz_silhouette(sil)

#visualizing the clustering algorithms
km.clusters <- km.out$cluster

#row.names(dataa_scale) <- km.clusters
fviz_cluster(list(data = new_data_scale, cluster = km.clusters))
table(km.clusters)
summary(km.clusters)
km.clusters

#distance between the data points
new_dat <- dist(new_data_scale)

#Hierarchial clustering
hc_out <- hclust(new_dat,method = "complete")

#dendogram
plot(hc_out)
rect.hclust(hc_out, k = 3, border = 2:6)

#clusters
hc.clusters <- cutree(hc_out, k=3)
summary(hc.clusters)

#visualize
fviz_cluster(list(data=new_data_scale, cluster = hc.clusters ))
table(hc.clusters)


#Interpretation
final_data <- final_data |> mutate(cluster = km.clusters)
summary(final_data$cluster)
final_data$Frequency <- as.numeric(final_data$Frequency)
ggplot(data = final_data, aes(x = `distance(m)`, y = times, col =  cluster)) + geom_point()


#Renaming the classifiers
final_data$cluster <- factor(final_data$cluster, levels = c("1","2","3"), 
                             labels = c("first","second","third"))
summary(final_data)


# Scale data (Min - Max normalization)
scl <- function(x){ (x - min(x))/(max(x) - min(x)) }
final_data[, 1:3] <- data.frame(lapply(final_data[, 1:3], scl))
head(final_data)

#Splitting the dataset to training and test data
training_data_rows <- floor(0.70 * nrow(final_data))         
set.seed(123)
training_indices <- sample(c(1:nrow(final_data)), training_data_rows)

training_data <- final_data[training_indices,]
validation_data <- final_data[-training_indices,]
head(validation_data)


#Building a neural network
variables <- names(training_data)
formular <- as.formula(paste("cluster ~", paste(variables[!variables %in% c("cluster")],
                                                collapse = " + ")))          


NN <- neuralnet(formular,
                data = training_data,
                hidden = c(13, 10, 3),
                act.fct = "logistic",
                linear.output = FALSE,
                lifesign = "minimal")
plot(NN)

#function for prediction
predict00 <- function(data){
  prediction <- data.frame(neuralnet::compute(NN, 
                                              data.frame(data[,-4]))$net.result)
  labels <- c("first","second","third")
  prediction_label <- data.frame(max.col(prediction)) %>% 
    mutate(prediction=labels[max.col.prediction.]) %>% 
    select(2) %>% 
    unlist()
  
  confusionMatrix(table(data$cluster, prediction_label))
}

#Prediction
predict00(training_data)
predict00(validation_data)


#Predicting the test set
#Scaling the test data
scl_test_data <- data.frame(lapply(test_data, scl))

head(test_data)


#Function for prediction
predict <- function(data){ 
  prediction <- data.frame(neuralnet::compute(NN,  
                                              data.frame(data))$net.result) 
  labels <- c("first", "second", "third") 
  prediction_label <- data.frame(max.col(prediction)) %>%  
    mutate(prediction=labels[max.col.prediction.]) %>%  
    select(2) %>%  
    unlist() 
  as.data.frame(prediction_label) 
}

predicted_clusters <- predict(scl_test_data)

final_pred_data <- cbind(test_data,predicted_clusters$prediction_label)
colnames(final_pred_data)[4] <- "clusters_pred"
final_pred_data$clusters_pred <- factor(final_pred_data$clusters_pred, levels = c("first","second","third"),
                                        labels = c("Gold","Silver","Bronze"))
summary(final_pred_data)

