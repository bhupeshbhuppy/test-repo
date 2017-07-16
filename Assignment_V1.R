rm(list = ls())
library(ggplot2)
library(plotly)
library(reshape2)
library(corrplot)
library(binr)
library(xgboost)
library(Metrics)
library(performanceEstimation)
library(caret)
library(DMwR)
library(rpart)
sharing_data<-read.csv("E:/Study/Kaggle/UCI/OnlineNewsPopularity/OnlineNewsPopularity.csv")

##Looking at the data file##
head(sharing_data)
summary(sharing_data)

## Division of features

dummy_Features<-c("data_channel_is_lifestyle" , "data_channel_is_bus",
                  "data_channel_is_socmed" , "data_channel_is_tech",
                  "data_channel_is_world",
                  "data_channel_is_entertainment","weekday_is_monday",
                  "weekday_is_tuesday" , "weekday_is_wednesday",
                  "weekday_is_thursday" , "weekday_is_friday",
                  "weekday_is_saturday" , "weekday_is_sunday" , "is_weekend")
conti_features<-colnames(sharing_data)[!(colnames(sharing_data) %in% dummy_Features)]
conti_data<-sharing_data[,conti_features]
conti_data<-conti_data[,-c(1:2)]
dummy_data<-sharing_data[,dummy_Features]
dummy_data$shares<-sharing_data$shares



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


### Discrete Variable

channel_features<-dummy_data[,c(1:6)]
k<-c()
for(i in colnames(channel_features)){
  temp_var<-data.frame(table(channel_features[,i]))
  temp_var$Var2<-i
  k<-rbind(k,temp_var)
}
plot_ly(k, x = ~Var2, y =~Freq , color = ~Var1)%>%
  layout(title ="Missing vs Response Chart")

############ MODEL1- Data set 1 ( Feature engineering)###############

sharing_data_f1<-sharing_data

###  kw_min_min -1 as o
conti_data$kw_min_min[conti_data$kw_min_min == -1]<-0
summary(conti_data$kw_min_min)

### Binning of data
## n_token_title 13 bins 
additional_features<-c()
cuts<-bins(sharing_data_f1$n_tokens_title,13,minpts =  1, exact.groups = FALSE)
additional_features$n_tokens_title_f<-as.integer(cut(sharing_data_f1$n_tokens_title, bins.getvals(cuts)))
## average_token_length 2 bins
cuts<-bins(sharing_data_f1$average_token_length,2,minpts =  1, exact.groups = FALSE)
additional_features$average_token_length_f<-as.integer(cut(sharing_data_f1$average_token_length, bins.getvals(cuts)))

## num_videos 5 bins
cuts<-bins(sharing_data_f1$num_videos,5,minpts =  1, exact.groups = FALSE)
additional_features$num_videos_f<-as.integer(cut(sharing_data_f1$num_videos, bins.getvals(cuts)))
## num_keywords 8 bin (generated 7)
cuts<-bins(sharing_data_f1$num_keywords,8,minpts =  1, exact.groups = FALSE)
additional_features$num_keywords_f<-as.integer(cut(sharing_data_f1$num_keywords, bins.getvals(cuts)))
## kw_min_min 3 bins (generated 2)
cuts<-bins(sharing_data_f1$kw_min_min,3,minpts =  1, exact.groups = FALSE)
additional_features$kw_min_min_f<-as.integer(cut(sharing_data_f1$kw_min_min, bins.getvals(cuts)))
## kw_max_max 4 bins
cuts<-bins(sharing_data_f1$kw_max_max,4,minpts =  1, exact.groups = FALSE)
additional_features$kw_max_max_f<-as.integer(cut(sharing_data_f1$kw_max_max, bins.getvals(cuts)))
sharing_data_f1<-cbind(sharing_data_f1,additional_features)
summary(sharing_data_f1[,c(62:67)])


## randomly divide data 
shares<-sharing_data$shares
sharing_data_f1<-cbind(conti_data[,-45],dummy_data[-15],additional_features,shares)
#sharing_data_f1<-sharing_data_f1[,-c(1,2)]
sharing_data_f1$random <- runif(nrow(sharing_data_f1))
train <- sharing_data_f1[sharing_data_f1$random <= 0.7,] 
test <- sharing_data_f1[sharing_data_f1$random > 0.7,] 
col_name<-colnames(train[,-c(65,66)])
col_name_base<-col_name[1:58]

## xgboost
## Base model
model_xg_base<-xgboost(data        = data.matrix(train[,col_name_base]),
                       label       = train$shares,
                       nrounds     = 100,
                       objective   = "reg:linear",
                       eval_metric = "rmse")
test$Prediction_base<-as.integer(round(predict(model_xg, data.matrix(test[,col_name_base]))))
rmse(test$shares,test$Prediction_base)
### XGBoost extended features (feature bining)
model_xg<-xgboost(data        = data.matrix(train[,col_name]),
                  label       = train$shares,
                  nrounds     = 500,
                  objective   = "reg:linear",
                  eval_metric = "rmse")
test$Prediction<-as.integer(round(predict(model_xg, data.matrix(test[,col_name]))))
imp_matrix<-xgb.importance(colnames(train[,col_name]),model_xg)
xgb.plot.importance(imp_matrix)
rmse(test$shares,test$Prediction)




######## other performance metrices 
### Predicting pouplarity instaed of shares

cuts<-bins(sharing_data_f1$shares,5,minpts =  1, exact.groups = FALSE)
cuts$binct
test$actual_bins<-as.integer(cut(test$shares, bins.getvals(cuts)))
test$predicted_bins<-as.integer(cut(test$Prediction, bins.getvals(cuts)))
kappa_xg<-ScoreQuadraticWeightedKappa(test$predicted_bins,test$actual_bins)

############ Model 2-  3 sum models

####dividing the data itself
cuts<-bins(sharing_data_f1$shares,2,minpts =  1, exact.groups = FALSE)
train$shares_bin_2<-as.integer(cut(train$shares, bins.getvals(cuts)))
test$shares_bin_2<-as.integer(cut(test$shares, bins.getvals(cuts)))
model_xg<-xgboost(data        = data.matrix(train[,col_name]),
                  label       = train$shares,
                  nrounds     = 500,
                  objective   = "reg:linear",
                  eval_metric = "rmse")
table(train$shares_bin_2)
test$shares_bin_2<-test$shares_bin_2-1
train$shares_bin_2<-train$shares_bin_2-1
### 0  is unpopular
model_1_data<-train[which(train$shares_bin_2 == 0),]
model_2_data<-train[which(train$shares_bin_2 == 1),]
### unpopular
model_1_xg<-xgboost(data        = data.matrix(model_1_data[,col_name]),
                    label       = model_1_data$shares,
                    nrounds     = 500,
                    objective   = "reg:linear",
                    eval_metric = "rmse")
### popular
model_2_xg<-xgboost(data        = data.matrix(model_2_data[,col_name]),
                    label       = model_2_data$shares,
                    nrounds     = 500,
                    objective   = "reg:linear",
                    eval_metric = "rmse")
### binning
model_3_xg<-xgboost(data        = data.matrix(train[,col_name]),
                    label       = train$shares_bin_2,
                    nrounds     = 500,
                    objective   = "binary:logistic",
                    eval_metric = "rmse")
test$Predict_share_bin_2<-as.numeric(test$Predict_share_bin_2 > 0.5)
table(test$Predict_share_bin_2)
classificationMetrics(test$shares_bin_2,test$Predict_share_bin_2)

Predict_share_0<-as.integer(predict(model_1_xg,data.matrix(test[which(test$Predict_share_bin_2 == 0),col_name])))
rmse(test$shares[test$Predict_share_bin_2 == 0],Predict_share_0)
Predict_share_1<-as.integer(predict(model_2_xg,data.matrix(test[which(test$Predict_share_bin_2 == 1),col_name])))
rmse(test$shares[test$Predict_share_bin_2 == 1],Predict_share_1)
index_0<-1
index_1<-1
for(i in 1:length(test[,1])){
  if(test$shares_bin_2[i]== 0){
    test$Prediction[i]<-Predict_share_0[index_0]
    index_0<-index_0 + 1
  }
  else{
    test$Prediction[i]<-Predict_share_1[index_1]
    index_1<-index_1 + 1
  }
}
rmse(test$shares, test$Prediction)
summary(test$Prediction)


############# Stanarized of variables
## Due to variying range and variance of the contimous variables we 
conti_data<-predict(preProcess(conti_data, method = c("center" ,"scale")),conti_data)
sharing_data_f1<-cbind(conti_data[,-45],dummy_data[-15],additional_features,shares)
### Learn Again
sharing_data_f1$random <- runif(nrow(sharing_data_f1))

train <- sharing_data_f1[sharing_data_f1$random <= 0.7,] 
test <- sharing_data_f1[sharing_data_f1$random > 0.7,] 
col_name<-colnames(train[,-c(65,66)])
model_xg_base<-xgboost(data        = data.matrix(train[,col_name]),
                       label       = train$shares,
                       nrounds     = 500,
                       objective   = "reg:linear",
                       eval_metric = "rmse")

test$Prediction<-as.integer(round(predict(model_xg_base, data.matrix(test[,col_name]))))
imp_matrix<-xgb.importance(colnames(train[,col_name]),model_xg_base)
xgb.plot.importance(imp_matrix)
rmse(test$shares,test$Prediction)
## not good

# for (i in 1:length(conti_data)){
#   for( j in 1: length(conti_data[,i])){
#     conti_data[j,i]<-(conti_data[j,i]- min(conti_data[,i]))/(max(conti_data[,i])-min(conti_data[,i]))
#   }
# }
## Normalization
shares<-sharing_data$shares
conti_data<-as.data.frame(sapply(conti_data, function(x){LinearScaling(x)}))
sharing_data_f1<-cbind(conti_data[,-45],dummy_data[-15],additional_features,shares)
### Learn Again
sharing_data_f1$random <- runif(nrow(sharing_data_f1))

train <- sharing_data_f1[sharing_data_f1$random <= 0.7,] 
test <- sharing_data_f1[sharing_data_f1$random > 0.7,] 
col_name<-colnames(train[,-c(65,66)])
model_xg_base<-xgboost(data        = data.matrix(train[,col_name]),
                       label       = train$shares,
                       nrounds     = 500,
                       objective   = "reg:linear",
                       eval_metric = "rmse")

test$Prediction<-as.integer(round(predict(model_xg_base, data.matrix(test[,col_name]))))
imp_matrix<-xgb.importance(colnames(train[,col_name]),model_xg_base)
xgb.plot.importance(imp_matrix)
rmse(test$shares,test$Prediction)
## not good


rpart_model<-rpart(shares~.,train[,-66])
test$Prediction<-as.integer(round(predict(rpart_model, test[,col_name])))
rmse(test$shares,test$Prediction)

## Not good

