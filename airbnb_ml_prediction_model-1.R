airbnb_data<-read.csv("~/Downloads/airbnb_fully_cleaned_data.csv") 

library(dplyr)
library(gapminder)
library(randomForest)
library(e1071)
library(ModelMetrics)
library(geosphere)
library(hutils)
library(data.table)
  
airbnb_data$price <- as.factor(airbnb_data$price)
airbnb_data$noise.dB. <- as.factor(airbnb_data$noise.dB.)
airbnb_data$minimum.nights <- as.factor(airbnb_data$minimum.nights)
airbnb_data$number.of.reviews..total. <- as.factor(airbnb_data$number.of.reviews..total.)
airbnb_data$last.review..date. <- as.factor(airbnb_data$last.review..date.)
airbnb_data$reviews.per.month <- as.factor(airbnb_data$reviews.per.month)

#typeof(airbnb_data$price)


#POIs

latvalues = list(airbnb_data$latitude)
latvalues 
lat <- as.numeric(unlist(latvalues))

longvalues = list(airbnb_data$longitude)
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

airbnb_data$proximitytomemorial <- hutils::haversine_distance(lat, long, memlat, memlon) < 500
airbnb_data$proximitytocpl <- hutils::haversine_distance(lat, long, cplat, cplon) < 500
airbnb_data$proximitytoemps <- hutils::haversine_distance(lat, long, empslat, empslon) < 500
airbnb_data$proximitytostatue <- hutils::haversine_distance(lat, long, statuelat, statuelon) < 500
airbnb_data$proximitytojfk <- hutils::haversine_distance(lat, long, jfklat, jfklon) < 500
airbnb_data$proximitytomsg <- hutils::haversine_distance(lat, long, msglat, msglon) < 500



#Aborts R
Points <- CJ(lon = lon,
             lat = lat)
Points[, dist := haversine_distance(lat, lon, mylat, mylon)]
Points[, sum(dist < 500)]

#Method 2 - aborts R

res<-matrix(FALSE,nrow=length(long),ncol=length(lat))

longood<-which(distm(c(mylon,mylat),cbind(long,mylat))<=500000)
#Same for latitude
latgood<-which(distm(c(mylon,mylat),cbind(mylon,lat))<=500000)

allCoords<-cbind(long[longood],rep(lat[latgood],each=length(longood)))
res[longood,latgood]<-distm(c(mylon,mylat),allCoords)<=500000

names(airbnb_data) <- c('X','id','name', 'host_id', 'host_name', 'neighbourhood_group', 'neighbourhood', 
                        'latitude', 'longitude', 'room_type', 'minimum_nights', 'number_of_reviews',
                        'last_review', 'reviews_per_month', 'floor', 'noise.dB.', 'price', 'proximitytomemorial',
                        'proximitytocpl','proximitytoemps','proximitytostatue','proximitytojfk','proximitytomsg')


#Subsetting based on neighbourhood group

sub_Manhattan <- subset(airbnb_data, neighbourhood_group == "Manhattan")
sub_Bronx <- subset(airbnb_data, neighbourhood_group == "Bronx")
sub_Brooklyn <- subset(airbnb_data, neighbourhood_group == "Brooklyn")
sub_Queens <- subset(airbnb_data, neighbourhood_group == "Queens")
sub_Staten <- subset(airbnb_data, neighbourhood_group == "Staten Island")

#private rooms 
sub_private_Man <- subset(sub_Manhattan, room_type == "Private room")
sub_private_Bronx <- subset(sub_Bronx, room_type == "Private room")
sub_private_Brooklyn <- subset(sub_Brooklyn, room_type == "Private room")
sub_private_Queens <- subset(sub_Queens, room_type == "Private room")
sub_private_Staten <- subset(sub_Staten, room_type == "Private room")

#shared rooms
sub_shared_Man <- subset(sub_Manhattan, room_type == "Shared room")
sub_shared_Bronx <- subset(sub_Bronx, room_type == "Shared room")
sub_shared_Brooklyn <- subset(sub_Brooklyn, room_type == "Shared room")
sub_shared_Queens <- subset(sub_Queens, room_type == "Shared room")
sub_shared_Staten <- subset(sub_Staten, room_type == "Shared room")

#entire homes/apts
sub_entire_Man <- subset(sub_Manhattan, room_type == "Entire home/apt")
sub_entire_Bronx <- subset(sub_Bronx, room_type == "Entire home/apt")
sub_entire_Brooklyn <- subset(sub_Brooklyn, room_type == "Entire home/apt")
sub_entire_Queens <- subset(sub_Queens, room_type == "Entire home/apt")
sub_entire_Staten <- subset(sub_Staten, room_type == "Entire home/apt")



# splitting the dataset into training and testing.
idx <- sample( 1:2, size = nrow(airbnb_data), replace = TRUE, prob = c(.8, .2))
train <- airbnb_data[idx == 1,]
test <- airbnb_data[idx == 2,]

#splitting the subsetted dataset into training and testing.
idx_man <- sample( 1:2, size = nrow(sub_Manhattan), replace = TRUE, prob = c(.8, .2))
train_man <- sub_Manhattan[idx_man == 1,]
test_man <- sub_Manhattan[idx_man == 2,]

idx_queens <- sample( 1:2, size = nrow(sub_Queens), replace = TRUE, prob = c(.8, .2))
train_q <- sub_Queens[idx_queens == 1,]
test_q <- sub_Queens[idx_queens == 2,]

idx_bronx <- sample( 1:2, size = nrow(sub_Bronx), replace = TRUE, prob = c(.8, .2))
train_bx <- sub_Queens[idx_bronx == 1,]
test_bx <- sub_Queens[idx_bronx == 2,]

idx_brooklyn <- sample( 1:2, size = nrow(sub_Brooklyn), replace = TRUE, prob = c(.8, .2))
train_b <- sub_Queens[idx_brooklyn == 1,]
test_b <- sub_Queens[idx_brooklyn == 2,]

idx_staten <- sample( 1:2, size = nrow(sub_Staten), replace = TRUE, prob = c(.8, .2))
train_s <- sub_Queens[idx_staten == 1,]
test_s <- sub_Queens[idx_staten == 2,]


#Random Forest
# 1. Build prediction model using randomForest() function.

random_forest <- randomForest::randomForest(factor(price) ~ neighbourhood.group +
                                              room.and.type + minimum.nights +
                                              number.of.reviews..total. + last.review..date.+ 
                                              reviews.per.month + floor + noise.dB.,
                                              data = train, min_samples_leaf=3)


random_forest

pred.train <- predict(random_forest,newdata = train)
mse(factor(train$price),pred.train)
pred.test <- predict(random_forest,newdata = test)
mse(factor(test$price),pred.test)


random_forest2 <- randomForest::randomForest(factor(price) ~ neighbourhood_group +
                                              room_type 
                                              
                                               + floor + proximitytomemorial 
                                             + proximitytocpl + proximitytoemps 
                                             + proximitytostatue + proximitytojfk + proximitytomsg,
                                            data = train)

pred.train2 <- predict(random_forest2,newdata = train)
mse(factor(train$price),pred.train2)
pred.test2 <- predict(random_forest2,newdata = test)
mse(factor(test$price),pred.test2)

#Random Forest on subsetted datasets

summary(train_man)
str(train_man)

#not working
mtry <- tuneRF(train_man[-1],factor(train_man$price), ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)


sub_Manhattan$price <- as.factor(sub_Manhattan$price)
sub_Manhattan$noise_dB <- as.factor(sub_Manhattan$noise_dB)
sub_Manhattan$minimum_nights <- as.factor(sub_Manhattan$minimum_nights)
sub_Manhattan$number_of_reviews <- as.factor(sub_Manhattan$number_of_reviews)
sub_Manhattan$last_review <- as.factor(sub_Manhattan$last_review)
sub_Manhattan$reviews_per_month <- as.factor(sub_Manhattan$reviews_per_month)


random_forest_man <- randomForest::randomForest(factor(price) ~ neighbourhood_group +
                      room_type + floor+ proximitytomemorial 
                      + proximitytocpl + proximitytoemps 
                      + proximitytostatue + proximitytojfk + proximitytomsg,
                      data = train_man)

pred.train_man <- predict(random_forest_man,newdata = train_man)
mse(factor(train_man$price),pred.train_man)
pred.test_man <- predict(random_forest_man,newdata = test_man)
mse(factor(test_man$price),pred.test_man)

install.packages('rfUtilities')
library(rfUtilities)



random_forest_queens <- randomForest::randomForest(factor(price) ~ neighbourhood_group +
                                                  room_type 
                                                + floor  + proximitytomemorial 
                                                + proximitytocpl + proximitytoemps 
                                                + proximitytostatue + proximitytojfk + proximitytomsg,
                                                data = train_q)

pred.train_q <- predict(random_forest_queens,newdata2 = train_q)
mse(factor(train_q$price),pred.train_q)
pred.test_q <- predict(random_forest_queens,newdata2 = test_q)
mse(factor(test_q$price),pred.test_q)


#testing data
testData<-read.csv("~/Downloads/test_without_price.csv")

testData['price'] <- NA

latvalues1 = list(testData$latitude)
lat1 <- as.numeric(unlist(latvalues1))

longvalues1 = list(testData$longitude)
long1 <- as.numeric(unlist(longvalues1))

testData$proximitytomemorial <- hutils::haversine_distance(lat1, long1, memlat, memlon) < 500
testData$proximitytocpl <- hutils::haversine_distance(lat1, long1, cplat, cplon) < 500
testData$proximitytoemps <- hutils::haversine_distance(lat1, long1, empslat, empslon) < 500
testData$proximitytostatue <- hutils::haversine_distance(lat1, long1, statuelat, statuelon) < 500
testData$proximitytojfk <- hutils::haversine_distance(lat1, long1, jfklat, jfklon) < 500
testData$proximitytomsg <- hutils::haversine_distance(lat1, long1, msglat, msglon) < 500

decision<-rep(0,nrow(airbnb_data))
decision<-predict(random_forest_man, newdata = testData)

mySubmission<- read.csv("~/Desktop/mysub2.csv")
mySubmission$price = decision

write.csv(mySubmission, file = "mysub2.csv", row.names = TRUE)



random_forest_bronx <- randomForest::randomForest(factor(price) ~ neighbourhood.group +
                                                     room.and.type  + as.numeric(as.character(train_bx$minimum.nights))
                                                   + as.numeric(as.character(train_bx$number.of.reviews..total.))
                                                   + as.numeric(as.character(train_bx$reviews.per.month)) 
                                                   + floor + noise.dB. + proximitytomemorial 
                                                   + proximitytocpl + proximitytoemps 
                                                   + proximitytostatue + proximitytojfk + proximitytomsg,
                                                   data = train_bx)

pred.train_bx <- predict(random_forest_bronx,newdata3 = train_bx)
mse(factor(train_bx$price),pred.train_bx)
pred.test_bx <- predict(random_forest_bronx,newdata3 = test_bx)
mse(factor(test_bx$price),pred.test_bx)


random_forest_brooklyn <- randomForest::randomForest(factor(price) ~ neighbourhood.group +
                                                     room.and.type  + as.numeric(as.character(train_b$minimum.nights))
                                                   + floor + noise.dB. + proximitytomemorial 
                                                   + proximitytocpl + proximitytoemps 
                                                   + proximitytostatue + proximitytojfk + proximitytomsg,
                                                   data = train_b)

pred.train_b <- predict(random_forest_brooklyn,newdata4 = train_b)
mse(factor(train_b$price),pred.train_b)
pred.test_b <- predict(random_forest_brooklyn,newdata4 = test_b)
mse(factor(test_b$price),pred.test_b)

random_forest_staten <- randomForest::randomForest(factor(price) ~ neighbourhood.group +
                                                     room.and.type  + as.numeric(as.character(train_s$minimum.nights))
                                                   + as.numeric(as.character(train_s$number.of.reviews..total.))
                                                   + as.numeric(as.character(train_s$reviews.per.month)) 
                                                   + floor + noise.dB. + proximitytomemorial 
                                                   + proximitytocpl + proximitytoemps 
                                                   + proximitytostatue + proximitytojfk + proximitytomsg,
                                                   data = train_s)

pred.train_s <- predict(random_forest_staten,newdata5 = train_s)
mse(factor(train_s$price),pred.train_s)
pred.test_s <- predict(random_forest_staten,newdata5 = test_s)
mse(factor(test_s$price),pred.test_s)


#SVM

# 1. Build prediction model using svm() function.


#svm_fit <- tune.svm(factor(price) ~ neighbourhood.group +
#                      room.and.type + minimum.nights +
#                      number.of.reviews..total. + last.review..date.+ 
#                      reviews.per.month + floor + noise.dB.,
#                    data = train, tunecontrol = tune.control(cross=2))
#summary(svm_fit)

svm_fit <- svm(factor(price) ~ neighbourhood.group +
                 room.and.type + minimum.nights +
                 number.of.reviews..total. + last.review..date.+ 
                 reviews.per.month + floor + noise.dB.,
               data = train)


svm_fit
pred.train <- predict(svm_fit,newdata = train)
mse(train$price,pred.train)
pred.test <- predict(svm_fit,newdata = test)
mse(test$price,pred.test)

#Linear Regression

simple.fit <- lm(price~minimum.nights,
                 data=train)

summary(simple.fit)

plot(train$minimum.nights,train$price)
abline(simple.fit , col="red")


PredictedPrice.simple <- predict(simple.fit,test)
# Predicted Values
head(as.integer(unname(PredictedPrice.simple)))
# Actual Values
head(test$Price)



#rpart

library(rpart)

rpart.fit <- rpart(price ~ neighbourhood.group +
                     room.and.type + minimum.nights +
                     number.of.reviews..total. + last.review..date.+ 
                     reviews.per.month + floor + noise.dB.,
                   data = data.frame(train),method = "anova")
rpart.fit

levels(droplevels(airbnb_data$minimum.nights))


PredictedPrice <- predict(rpart.fit,test, type = "class")
# Predicted Values
head(as.integer(unname(PredictedPrice)))
# Actual Values
head(test$price)

mse(test$price,PredictedPrice)