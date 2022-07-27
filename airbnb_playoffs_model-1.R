airbnb_data_playoffs<-read.csv("~/Downloads/playoffs-train.csv") 

library(dplyr)
library(gapminder)
library(randomForest)
library(e1071)
library(ModelMetrics)
library(geosphere)
library(hutils)
library(data.table)
library(rpart)

colors<-c("cyan", "pink", "yellow")
mosaicplot(airbnb_data_playoffs$neighbourhood_group~airbnb_data_playoffs$room_type,
           main = "Relationship between Neighbourhood Group and Room Type", 
           xlab = "Neighbourhood Group",                               
           ylab = "Room Type",
           col = colors)

boxplot(price~room_type, 
        data=airbnb_data_playoffs, 
        main = "Price Distribution for each Room Type",      
        xlab = "Room Type",                                 
        ylab = "Price",                                        
        col = colors)

boxplot(price~neighbourhood_group, 
        data=airbnb_data_playoffs, 
        main = "Price Distribution for each Neighbourhood Group",      
        xlab = "Neighbourhood Group",                                 
        ylab = "Price",                                        
        col = colors)

plot(airbnb_data_playoffs$price,airbnb_data_playoffs$floor,
     main = "Price Distribution for each Floor",      
     xlab = "Price",                                 
     ylab = "Floor", 
     col = colors)

#most expensive starts at 15, v lil exception to 500 having anything below floor 15

sub_Manhattan <- subset(airbnb_data_playoffs, neighbourhood_group == "Manhattan")
sub_Brooklyn <- subset(airbnb_data_playoffs, neighbourhood_group == "Brooklyn")
sub_Queens <- subset(airbnb_data_playoffs, neighbourhood_group == "Queens")

boxplot(price~room_type, 
        data=sub_Manhattan, 
        main = "Price Distribution for each Room Type",      
        xlab = "Room Type",                                 
        ylab = "Price",                                        
        col = colors)

boxplot(price~room_type, 
        data=sub_Brooklyn, 
        main = "Price Distribution for each Room Type",      
        xlab = "Room Type",                                 
        ylab = "Price",                                        
        col = colors)

boxplot(price~room_type, 
        data=sub_Queens, 
        main = "Price Distribution for each Room Type",      
        xlab = "Room Type",                                 
        ylab = "Price",                                        
        col = colors)

plot(sub_Manhattan$price,sub_Manhattan$floor,
     main = "Price Distribution for each Floor",      
     xlab = "Price",                                 
     ylab = "Floor", 
     col = colors)

plot(sub_Brooklyn$price,sub_Brooklyn$floor,
     main = "Price Distribution for each Floor",      
     xlab = "Price",                                 
     ylab = "Floor", 
     col = colors)

plot(sub_Queens$price,sub_Queens$floor,
     main = "Price Distribution for each Floor",      
     xlab = "Price",                                 
     ylab = "Floor", 
     col = colors)


# splitting the dataset into training and testing.
idx <- sample( 1:2, size = nrow(airbnb_data_playoffs), replace = TRUE, prob = c(.8, .2))
train <- airbnb_data_playoffs[idx == 1,]
test <- airbnb_data_playoffs[idx == 2,]

idx_man <- sample( 1:2, size = nrow(sub_Manhattan), replace = TRUE, prob = c(.8, .2))
train_man <- sub_Manhattan[idx_man == 1,]
test_man <- sub_Manhattan[idx_man == 2,]


#Random Forest
random_forest_play <- randomForest(price ~ neighbourhood_group + room_type + floor, 
                                                 data = train)

pred_train_play <- predict(random_forest_play,newdata = train)
mse(train$price,pred_train_play)
pred_test_play <- predict(random_forest_play,newdata = test)
mse(test$price,pred_test_play)


#Rpart
rpart_play <- rpart(price ~ neighbourhood_group + room_type + floor,
      data = train, method = "class")

pred_train_rpart <- predict(rpart_play, train, type="class")
mse(train$price,pred_train_rpart)


#SVM
svm_fit <- svm(price ~ neighbourhood_group + room_type + floor, 
               data = airbnb_data_playoffs)

pred_train_svm <- predict(svm_fit,newdata = train)
mse(train$price,pred_train_svm)
pred_test_svm <- predict(svm_fit,newdata = test)
mse(test$price,pred_test_svm)

#incorporation of POIs
data_copy <- airbnb_data_playoffs

latvalues = list(data_copy$latitude)
latvalues 
lat <- as.numeric(unlist(latvalues))

longvalues = list(data_copy$longitude)
longvalues 
long <- as.numeric(unlist(longvalues))

memlat = 40.7114
memlon = 74.0125

cplat = 40.7812
cplon = 73.9665

empslat = 40.7484
empslon = 73.9857

statuelat = 40.6892
statuelon = 74.0445

jfklat = 40.6413
jfklon = 73.7781

msglat = 40.7593
msglon = 73.9794

rows <- nrow(data_copy)
rows

#Method 1
for (col1 in colnames(data_copy)) {
  if (col1 == "latitude") {
    lat = data_copy[, col1]
    print(lat)
    for (col2 in colnames(data_copy)) {
      if (col2 == "longitude") {
        lon = data_copy[, col2]
        print(lon)
      }
    }
  } 

  proximity = distm(c(data_copy[[lon]], data_copy[[lat]]), c(memlon, memlat), fun = distHaversine)
  
  if(proximity < 500) {
    data_copy$proximitytomemorial<- TRUE
  } else {
    data_copy$proximitytomemorial<- FALSE
  }
  
}

#Method 2 
res<-matrix(FALSE,nrow=length(long),ncol=length(lat))
longood<-which(distm(c(cplon,cplat),cbind(long,cplat))<=500000)
latgood<-which(distm(c(cplon,cplat),cbind(cplon,lat))<=500000)
allCoords<-cbind(long[longood],rep(lat[latgood],each=length(longood)))
res[longood,latgood]<-distm(c(cplon,cplat),allCoords)<=500000


#Method 3
data_copy$proximitytomemorial <- hutils::haversine_distance(lat, long, memlat, memlon) < 500
data_copy$proximitytocpl <- hutils::haversine_distance(lat, long, cplat, cplon) < 500
data_copy$proximitytoemps <- hutils::haversine_distance(lat, long, empslat, empslon) < 500
data_copy$proximitytostatue <- hutils::haversine_distance(lat, long, statuelat, statuelon) < 500
data_copy$proximitytojfk <- hutils::haversine_distance(lat, long, jfklat, jfklon) < 500
data_copy$proximitytomsg <- hutils::haversine_distance(lat, long, msglat, msglon) < 500

# splitting the dataset into training and testing.
idx2 <- sample( 1:2, size = nrow(data_copy), replace = TRUE, prob = c(.8, .2))
train2 <- data_copy[idx2 == 1,]
test2 <- data_copy[idx2 == 2,]

#SVM
svm_fit2 <- svm(price ~ neighbourhood_group + room_type + floor +
                 proximitytomsg + proximitytojfk + proximitytostatue + proximitytoemps + proximitytocpl 
               + proximitytomemorial, 
               data = train2)

pred_train_svm <- predict(svm_fit2,newdata = train2)
mse(train2$price,pred_train_svm)
pred_test_svm <- predict(svm_fit2,newdata = test2)
mse(test2$price,pred_test_svm)

#testing data and submission
testData<-read.csv("~/Downloads/playoffs-test-without-pricecsv.csv")

decision_play<-rep(0,nrow(airbnb_data_playoffs))
decision_play<-predict(svm_fit, newdata = testData)

testData$price = decision_play

mySubmissionplay<- read.csv("~/Desktop/mysubplay.csv")
mySubmissionplay$price = decision_play

write.csv(mySubmissionplay, file = "mysubplay.csv", row.names = TRUE)