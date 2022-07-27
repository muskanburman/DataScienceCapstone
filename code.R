airbnb_data<-read.csv("~/Downloads/airbnb_fully_cleaned_data.csv") 

colors<-c("cyan", "pink", "yellow")
mosaicplot(airbnb_data$neighbourhood.group~airbnb_data$room.and.type,
           main = "Relationship between Neighbourhood Group and Room Type", 
           xlab = "Neighbourhood Group",                               
           ylab = "Room Type",
           col = colors)

sapply(airbnb_data, class)

boxplot(price~room.and.type, 
        data=airbnb_data, 
        main = "Price Distribution for each Room Type",      
        xlab = "Room Type",                                 
        ylab = "Price",                                        
        col = colors)

sub_ds<-subset(airbnb_data, price>1000)  

boxplot(price~room.and.type, 
        data=sub_ds, 
        main = "Price Distribution for each Room Type",     
        xlab = "Room Type",                                
        ylab = "Price (>1000)",                                      
        col = colors)

sub_ds2<-subset(airbnb_data, price>3500)  

boxplot(price~room.and.type, 
        data=sub_ds2, 
        main = "Price Distribution for each Room Type",      
        xlab = "Room Type",                                
        ylab = "Price (>3500)",                                       
        col = colors)


boxplot(price~neighbourhood.group, 
        data=airbnb_data, 
        main = "Price Distribution for each Neighbourhood Group",      
        xlab = "Neighbourhood Groups",                                 
        ylab = "Prices",                                        
        col = colors)

sub_ds3<-subset(airbnb_data, price>800) 

boxplot(price~neighbourhood.group, 
        data=sub_ds3, 
        main = "Price Distribution for each Neighbourhood Group",      
        xlab = "Neighbourhood Groups",                                 
        ylab = "Prices (>800)",                                        
        col = colors)

sub_ds4<-subset(airbnb_data, price>3000) 

boxplot(price~neighbourhood.group, 
        data=sub_ds4, 
        main = "Price Distribution for each Neighbourhood Group",      
        xlab = "Neighbourhood Groups",                                 
        ylab = "Prices (>3000)",                                        
        col = colors)


#hypotheses:for high-end rooms, private rooms are more expensive than entire home/apt

private_data<-subset(sub_ds2,sub_ds2$room.and.type == "Private room")
home_data<-subset(sub_ds2,sub_ds2$room.and.type == "Entire home/apt")
private_price<-private_data$price
home_price<-home_data$price

#calculate standard deviation
sd_private<-sd(private_price)
sd_private
sd_home<-sd(home_price)
sd_home

#calculate mean
mean_private<-mean(private_price)
mean_private
mean_home<-mean(home_price)
mean_home

#length
len_private<-length(private_price)
len_private
len_home<-length(home_price)
len_home

sd_private_home<-sqrt(sd_private^2/len_private + sd_home^2/len_home)
sd_private_home

#z-score
zeta<-(mean_private - mean_home)/sd_private_home
zeta

#p-value
p = (1-pnorm(zeta))
p

#hypotheses:for high-end rooms, Manhattan is more expensive than Brooklyn

man_data<-subset(sub_ds4,sub_ds4$neighbourhood.group == "Manhattan")
brook_data<-subset(sub_ds4,sub_ds4$neighbourhood.group == "Brooklyn")
man_price<-man_data$price
brook_price<-brook_data$price

#calculate standard deviation
sd_man<-sd(man_price)
sd_man
sd_brook<-sd(brook_price)
sd_brook

#calculate mean
mean_man<-mean(man_price)
mean_man
mean_brook<-mean(brook_price)
mean_brook

#length
len_man<-length(man_price)
len_man
len_brook<-length(brook_price)
len_brook

sd_man_brook<-sqrt(sd_man^2/len_man + sd_brook^2/len_brook)
sd_man_brook

#z-score
zeta2<-(mean_brook - mean_man)/sd_man_brook
zeta2

#p-value
p2 = (1-pnorm(zeta2))
p2

########## Code for Checkpoint 2 ##########

#Subsetting based on neighbourhood group

sub_Manhattan <- subset(airbnb_data, neighbourhood.group == "Manhattan")
sub_Bronx <- subset(airbnb_data, neighbourhood.group == "Bronx")
sub_Brooklyn <- subset(airbnb_data, neighbourhood.group == "Brooklyn")
sub_Queens <- subset(airbnb_data, neighbourhood.group == "Queens")
sub_Staten <- subset(airbnb_data, neighbourhood.group == "Staten Island")

#private rooms 
sub_private_Man <- subset(sub_Manhattan, room.and.type == "Private room")
sub_private_Bronx <- subset(sub_Bronx, room.and.type == "Private room")
sub_private_Brooklyn <- subset(sub_Brooklyn, room.and.type == "Private room")
sub_private_Queens <- subset(sub_Queens, room.and.type == "Private room")
sub_private_Staten <- subset(sub_Staten, room.and.type == "Private room")

#shared rooms
sub_shared_Man <- subset(sub_Manhattan, room.and.type == "Shared room")
sub_shared_Bronx <- subset(sub_Bronx, room.and.type == "Shared room")
sub_shared_Brooklyn <- subset(sub_Brooklyn, room.and.type == "Shared room")
sub_shared_Queens <- subset(sub_Queens, room.and.type == "Shared room")
sub_shared_Staten <- subset(sub_Staten, room.and.type == "Shared room")

#entire homes/apts
sub_entire_Man <- subset(sub_Manhattan, room.and.type == "Entire home/apt")
sub_entire_Bronx <- subset(sub_Bronx, room.and.type == "Entire home/apt")
sub_entire_Brooklyn <- subset(sub_Brooklyn, room.and.type == "Entire home/apt")
sub_entire_Queens <- subset(sub_Queens, room.and.type == "Entire home/apt")
sub_entire_Staten <- subset(sub_Staten, room.and.type == "Entire home/apt")

colors<-c("cyan", "pink", "yellow")

plot(sub_Manhattan$price,sub_Manhattan$minimum.nights, 
     col = colors)
#min nights is more for cheaper hotels - price in nights decrease

plot(sub_private_Man$price,sub_private_Man$minimum.nights,col = colors)
#some outliers

plot(sub_shared_Man$price,sub_shared_Man$minimum.nights, col = colors)
#regardless of price, min niggts remian same

plot(sub_entire_Man$price,sub_entire_Man$minimum.nights, col = colors)

plot(sub_Manhattan$price,sub_Manhattan$number.of.reviews..total.,
     col = colors)
#same trend - more ppl going to cheaper places may lead to more reviews for those places

plot(sub_private_Man$price,sub_private_Man$number.of.reviews..total.,col = colors)

plot(sub_Brooklyn$price,sub_Brooklyn$number.of.reviews..total.,
     col = colors)

plot(sub_Queens$price,sub_Queens$number.of.reviews..total.,
     col = colors)

plot(sub_entire_Staten$price,sub_entire_Staten$number.of.reviews..total.,
     col = colors)

plot(sub_Manhattan$price,sub_Manhattan$floor, col = colors)
#price goes up, floor goes up

plot(sub_Bronx$price,sub_Bronx$floor, col = colors)

plot(sub_shared_Bronx$price,sub_shared_Bronx$floor, col = colors)
#nothing
plot(sub_private_Brooklyn$price,sub_private_Brooklyn$floor, col = colors)
#hger floors jave higer prices
plot(sub_private_Queens$price,sub_private_Queens$floor, col = colors)

plot(sub_private_Queens$price,sub_private_Queens$noise.dB., col = colors)
#less noisy more price

plot(sub_Manhattan$price,sub_Manhattan$noise.dB., col = colors)
#both up

plot(sub_Queens$price,sub_Queens$noise.dB., col = colors)
#both up

#Points of Interest

#9/11 Memo 
memo_sub <- subset(airbnb_data, latitude == "40.7114" | longitude == "74.0125" )
#2 places, v diff prices
memo_sub

#central park
cp_sub <- subset(airbnb_data, latitude == "40.7812" | longitude == "73.9665" )
# 0 places
cp_sub

#empire state building
esb_sub <- subset(airbnb_data, latitude == "40.7484" | longitude == "73.9857" )
# 1 in Manhattan - price not high
esb_sub

#statue of liberty
sl_sub <- subset(airbnb_data, latitude == "40.6892" | longitude == "74.0445" )
# 6 observations, varrying prices not really based on anythinhg
sl_sub
plot(sl_sub$price,sl_sub$longitude, col = colors)


#madison square garden
msg_sub <- subset(airbnb_data, latitude == "40.7593" | longitude == "73.9794" )
# 4 observations, prices similar range not really based on anything
msg_sub
plot(msg_sub$price,msg_sub$longitude, col = colors)

#jfk airport
gc_sub <- subset(airbnb_data, latitude == "40.6413" | longitude == "73.7781" )
# 3 observations, price range v different
gc_sub
plot(gc_sub$price,gc_sub$longitude, col = colors)


####Code for Checkpoint 3####

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

random_forest <- randomForest::randomForest(factor(price) ~ neighbourhood_group +
                                              room_type + minimum_nights +
                                              number_of_reviews + last_review+ 
                                              reviews_per_month + floor + noise.dB.,
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
decision<-predict(random_forest2, newdata = testData)

mySubmission<- read.csv("~/Desktop/mysub2.csv")
mySubmission$price = decision

write.csv(mySubmission, file = "mysub2.csv", row.names = TRUE)

####Playoffs####

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

airbnb_data_playoffs<- airbnb_data_playoffs %>%
  mutate(higherfloor = case_when(
    floor > 10 ~ "1",
    floor <= 10 ~ "0"
  ))

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






