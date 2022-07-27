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







