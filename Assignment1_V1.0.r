rm(list = ls())
library(ggplot2)
library(plotly)
library(reshape2)
library(corrplot)
library(binr)
sharing_data<-read.csv("D:/bhupesh/Bootcamp/BootCamp Assignment/OnlineNewsPopularity/OnlineNewsPopularity.csv")

##Looking at the data file##
head(sharing_data)
summary(sharing_data)

## Division of features

dummy_Features<-c("data_channel_is_lifestyle","data_channel_is_bus",
                  "data_channel_is_socmed","data_channel_is_tech",
                  "data_channel_is_world","weekday_is_monday",
                  "data_channel_is_entertainment",
                  "weekday_is_tuesday","weekday_is_wednesday",
                  "weekday_is_thursday","weekday_is_friday",
                  "weekday_is_saturday","weekday_is_sunday","is_weekend")
conti_features<-colnames(sharing_data)[!(colnames(sharing_data) %in% dummy_Features)]
conti_data<-sharing_data[,conti_features]
conti_data<-conti_data[,-c(1:2)]
dummy_data<-sharing_data[,dummy_Features]


table(sharing_data[,"data_channel_is_lifestyle"])
table(sharing_data[,"data_channel_is_bus"])
table(sharing_data[,"data_channel_is_socmed"])
table(sharing_data[,"data_channel_is_tech"])
table(sharing_data[,"data_channel_is_world"])
table(sharing_data[,"weekday_is_monday"])
table(sharing_data[,"weekday_is_tuesday"])
table(sharing_data[,"weekday_is_wednesday"])
table(sharing_data[,"weekday_is_thursday"])
table(sharing_data[,"weekday_is_friday"])
table(sharing_data[,"weekday_is_saturday"])
table(sharing_data[,"weekday_is_sunday"])
table(sharing_data[,"is_weekend"])


#### Looking at the distribution of the response Variable
temp_melt<-melt(sharing_data[,"shares"])
p<-ggplot(temp_melt,aes(value))+geom_density(alpha = 0.5)+ggtitle("Density Plots")
ggplotly(p)%>%
  layout(plot_bgcolor="transparent",paper_bgcolor= "transparent")

qplot(y = shares, data = sharing_data, colour = 'blue')

summary(sharing_data$shares)
sd(x = sharing_data$shares)
cuts<-bins(sharing_data$shares,2,minpts =  1, exact.groups = FALSE)
response<-cut(sharing_data$shares,cuts$binct)
#### Box Plot of the response variable
p<-plot_ly(y=sharing_data[,"shares"], type = "box")%>%
  layout( title = "Distribution of response")
p
## summary of response
summary(sharing_data$shares)

quantile(sharing_data$shares,c(.1,.2,.3,.4,.5,.6,.7,.8,.9,.95,.96,.97,.98,.99,1))

#### looks like the response variable is highly skewed towards left
#### either take log of response of the model if using linear regression

##### Looking for Na values

missing_prct<-data.frame(variable=colnames(sharing_data),missing=sapply(sharing_data,function(x)
{sum(is.na(x))}/nrow(sharing_data)))

### No missing value in the data

##### Box plots for continous variable ####
#### Words variable 
temp1<-conti_data[1:5]
plot_ly( data=melt(temp1), type = "box",
            split = ~variable,y = ~value)%>%
  layout( title = "Box-Plots of variables")
#### Accept ntoken_title and n_token context no other variable seems to be well distributed
#### looks like other features are highly concentrated
#### there is one outlier in these 3 variables. look for that outlier. in every result

#### Digital media variables

temp1<-conti_data[6:11]
plot_ly( data=melt(temp1), type = "box",
         split = ~variable,y = ~value)%>%
  layout( title = "Box-Plots of variables")

##### Though the distribution of value is not very much for num_imgs, num_videos,
##based on the importance of features we can go for relevent feature engineering 
##method. It makes intutive sense as the as most of the document will have a 
##few(0,1,2)  imgs or videos and very few will have high imgs and videos.

#### keywords variable based on number of share

temp1<-conti_data[12:20]
plot_ly( data=melt(temp1), type = "box",
         split = ~variable,y = ~value)%>%
  layout( title = "Box-Plots of variables")

## For kw_min_min the values are primarily concentrated at -1,4,217 (what does it means)
## Since -1 does not makes sence therefore we can probably replace it by 0
## A few very high values for kw_max_min this means that the data is a bit skewed
## COncentrated at 0 for kw_min_max (makes sense)
## High conventration at 84k check what can be doen about (some feature engineering can be useful)
## or probably we can use Average fo max and min
## average variables is having some outlier but is fairly well distributed

temp1<-conti_data[21:23]
plot_ly( data=melt(temp1), type = "box",
         split = ~variable,y = ~value)%>%
  layout( title = "Box-Plots of variables")

##The variables look fine

temp1<-conti_data[21:23]
plot_ly( data=melt(temp1), type = "box",
         split = ~variable,y = ~value)%>%
  layout( title = "Box-Plots of variables")


#### NLP variables

temp1<-conti_data[24:28]
plot_ly( data=melt(temp1), type = "box",
         split = ~variable,y = ~value)%>%
  layout( title = "Box-Plots of variables")

#### Data for LDA seems to be well distribured

temp1<-conti_data[29:33]
plot_ly( data=melt(temp1), type = "box",
         split = ~variable,y = ~value)%>%
  layout( title = "Box-Plots of variables")

temp1<-conti_data[34:38]
plot_ly( data=melt(temp1), type = "box",
         split = ~variable,y = ~value)%>%
  layout( title = "Box-Plots of variables")

temp1<-conti_data[39:44]
plot_ly( data=melt(temp1), type = "box",
         split = ~variable,y = ~value)%>%
  layout( title = "Box-Plots of variables")

### Entire NLP data seems to be well distributed 

##### To look at the data in a better way we plot the densities.

temp_melt<-melt(conti_data[,1:5])
p<-ggplot(temp_melt,aes(value, fill = variable ))+geom_density(alpha = 0.5)+ggtitle("Density Plots")
ggplotly(p)%>%
  layout(plot_bgcolor="transparent",paper_bgcolor= "transparent")
#### WOrds variable is poorly distributed

temp_melt<-melt(conti_data[6:11])
p<-ggplot(temp_melt,aes(value, fill = variable ))+geom_density(alpha = 0.5)+ggtitle("Density Plots")
ggplotly(p)%>%
  layout(plot_bgcolor="transparent",paper_bgcolor= "transparent")
#### Most of the distribution for Digital Media is skewed therefore can take log

temp_melt<-melt(conti_data[12:20])
p<-ggplot(temp_melt,aes(value, fill = variable ))+geom_density(alpha = 0.5)+ggtitle("Density Plots")
ggplotly(p)%>%
  layout(plot_bgcolor="transparent",paper_bgcolor= "transparent")


temp_melt<-melt(conti_data[21:23])
p<-ggplot(temp_melt,aes(value, fill = variable ))+geom_density(alpha = 0.5)+ggtitle("Density Plots")
ggplotly(p)%>%
  layout(plot_bgcolor="transparent",paper_bgcolor= "transparent")


temp_melt<-melt(conti_data[24:28])
p<-ggplot(temp_melt,aes(value, fill = variable ))+geom_density(alpha = 0.5)+ggtitle("Density Plots")
ggplotly(p)%>%
  layout(plot_bgcolor="transparent",paper_bgcolor= "transparent")


temp_melt<-melt(conti_data[29:33])
p<-ggplot(temp_melt,aes(value, fill = variable ))+geom_density(alpha = 0.5)+ggtitle("Density Plots")
ggplotly(p)%>%
  layout(plot_bgcolor="transparent",paper_bgcolor= "transparent")


temp_melt<-melt(conti_data[34:38])
p<-ggplot(temp_melt,aes(value, fill = variable ))+geom_density(alpha = 0.5)+ggtitle("Density Plots")
ggplotly(p)%>%
  layout(plot_bgcolor="transparent",paper_bgcolor= "transparent")


temp_melt<-melt(conti_data[39:44])
p<-ggplot(temp_melt,aes(value, fill = variable ))+geom_density(alpha = 0.5)+ggtitle("Density Plots")
ggplotly(p)%>%
  layout(plot_bgcolor="transparent",paper_bgcolor= "transparent")


corrplot(cor(conti_data[,1:5]), method = "circle")
## Very high correlation amongst n_unique_token, n_non_stop_words,n_non_stop_unique_tokens

corrplot(cor(conti_data[6:11]), method = "circle")
### All logical correlation

corrplot(cor(conti_data[12:20]), method = "circle")
### We have some strong correlation amongts variable here as well


corrplot(cor(conti_data[21:23]), method = "circle")
## Very high correlation

corrplot(cor(conti_data[24:28]), method = "circle")
### some correlation

corrplot(cor(conti_data[29:38]), method = "circle")
### some correlatioin

corrplot(cor(conti_data[38:44]), method = "circle")
### some high correlation

#### There is some degree of correlation amongst variable therefore we can go for Group wise PCA


