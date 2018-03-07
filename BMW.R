require(magrittr) # pipe %>%
require(dplyr)
files_tar = untar("bmwprod_2017-10-31-lookup_data.tar.gz", list = TRUE)

#Extract files to _temp directory. Directory will be created if it doesn't exist
untar("bmwprod_2017-10-31-lookup_data.tar.gz", exdir = "_temp")

#Read each file into a data frame
for (file in files_tar) {u
  df_name <- unlist(strsplit(file, split = ".tsv", fixed = TRUE))
  temp_df <- read.delim(paste("_temp", file, sep = "/"), header=FALSE, stringsAsFactors=FALSE)
  
#column_headers not used as lookup table
  if (df_name != "column_headers"){
    names(temp_df) <- c("id", df_name)
  }
  assign(df_name, temp_df)
  rm(temp_df)
}

require(tidyquant)
#gz files can be read directly into dataframes from base R
servercall_data = read.delim("01-bmwprod_2017-10-31.tsv.gz", header = F, stringsAsFactors = F, sep = "\t", quote="\t")

#Use column_headers to label servercall_data data frame using first row of data
names(servercall_data) <- column_headers[1,]

#original referrer_type has three columns
referrer_type <- referrer_type[,0:2]

library("sqldf")
require("sqldf")
query <- "select 
browser.browser as browser_name, 
connection_type.connection_type as connection_name,
country.country as country_name,
languages.languages,
operating_systems.operating_systems,
referrer_type.referrer_type,
resolution.resolution as screen_resolution,
search_engines.search_engines
from servercall_data as sc
left join connection_type 
on sc.connection_type = connection_type.id
left join browser 
on sc.browser = browser.id
left join country
on sc.country = country.id
left join languages
on sc.language = languages.id
left join operating_systems
on sc.os = operating_systems.id
left join referrer_type
on sc.ref_type = referrer_type.id
left join resolution 
on sc.resolution = resolution.id
left join search_engines
on sc.search_engine = search_engines.id
;"
denormalized_df <- sqldf(query)

#Create friendly names in events table replacing spaces with underscores
event$names_filled <- tolower(gsub(" ", "_", event$event))


#remove values in post_event_list that contains "=" and "."
require(stringi)
rev_post_event_list = servercall_data$post_event_list[-c(
which(stri_detect_fixed(servercall_data$post_event_list,".") == T),
which(stri_detect_fixed(servercall_data$post_event_list,"=") == T),
which(servercall_data$post_event_list == "")
)]

leadcompleted = rev_post_event_list[which(stri_detect_fixed(rev_post_event_list,"201") == T)]

#find the unique variables in post_event_list
unq_event = unique(unlist(strsplit(leadcompleted, "[,]")))

#Initialize a data frame with all 0 values
#Dimensions are number of observations as rows, with a column for every possible event
postleadcompleted = which(stri_detect_fixed(servercall_data$post_event_list,"201") == T)
event_df <- data.frame(matrix(data = 0, ncol = length(unq_event), nrow = length(leadcompleted)))
names(event_df) <- sort(unq_event)

#Parse comma-delimited string into vector
#Each vector value represents column name in event_df, assign value of 1
for(n in 1:length(leadcompleted)){
    event_df[n,][,unlist(strsplit(leadcompleted[n],","))] = 1
  }


#Rename columns with "friendly" names
for (n in 1: length(names(event_df))){
  names(event_df)[n] = event$names_filled[which(names(event_df)[n] == event$id)]
  
}
     
#Horizontally join datasets to create final dataset
oneday_df = cbind(denormalized_df[c(postleadcompleted),], event_df)
oneday_df$Content_Indicator = servercall_data$pagename[postleadcompleted]
oneday_df$Model_Selected = servercall_data$evar10[postleadcompleted]

#Remane instance_of_evarxx in oneday_df as the names in servercall_data
postinstance = grep("^instance_of_", names(oneday_df))
for (n in postinstance){
  names(oneday_df)[n] = gsub("instance_of_", "post_", names(oneday_df)[n])
  oneday_df[,n]= servercall_data[,which(names(oneday_df)[n] == names(servercall_data))][postleadcompleted]
}

#repeat above steps for "02-bmwprod_2017-10-31.tsv.gz", creating two oneday files and combining them together
#The combined dataframe contains all lead.completed users data
oneday01<-oneday_df
oneday02<-oneday_df2
oneday<-merge(oneday01,oneday02,all = T) 

###Create a new dataframe with both lead non-lead users and their other variables
#servercall_alldata2 was created when repeating the same step for "02-bmwprod_2017-10-31.tsv.gz"
#create servercall_dataall by combining servercall_data & servercall_data2
servercall_dataall = merge(servercall_data,servercall_data2,all = T) 
unique(servercall_dataall$post_event_list)
require(stringi)
rev_post_event_list10.31 = servercall_dataall$post_event_list[-c(
  which(stri_detect_fixed(servercall_dataall$post_event_list,".") == T),
  which(stri_detect_fixed(servercall_dataall$post_event_list,"=") == T),
  which(servercall_dataall$post_event_list == "")
)]

#Find the unique userID of lead and non-lead user
leadID = unique(servercall_dataall$post_evar47[which(stri_detect_fixed(rev_post_event_list10.31,"201") == T)])
nonleadID = unique(servercall_dataall$post_evar47[which(stri_detect_fixed(rev_post_event_list10.31,"201") == F)])

#Remove all users without userID
leadID = leadID[leadID != ""]
nonleadID = nonleadID[nonleadID != ""]

#add to a new dataframe 
dfleadnonlead = data.frame(matrix(data = 0, ncol = 2, nrow = length(leadID) + length(nonleadID)))
colnames(dfleadnonlead) = c("user.ID", "lead.complete")
dfleadnonlead$user.ID = c(leadID , nonleadID)
dfleadnonlead$lead.complete[1:length(leadID)]= 1

#count how many times each user visit site
dfleadnonlead$visit.freq = rep(0, dim(dfleadnonlead)[1])
for (i in 1:dim(dfleadnonlead)[1]){
  dfleadnonlead$visit.freq[i] = 
    sum(servercall_dataall$post_event_list[which(servercall_dataall$post_evar47 == dfleadnonlead$user.ID[i])] != "")
}

#add new column for BYO
dfleadnonlead$BYO = rep(0, dim(dfleadnonlead)[1])
for (i in 1:dim(dfleadnonlead)[1]){
  chars = unique(toupper(servercall_dataall$post_prop17[which(servercall_dataall$post_evar47 == dfleadnonlead$user.ID[i])]))
  decision = any(grepl("BYO", chars))
  if (decision == T){
    dfleadnonlead$BYO[i] = "1"
  }
}


#add new column for Reserve
dfleadnonlead$RESERVE = rep(0, dim(dfleadnonlead)[1])
for (i in 1:dim(dfleadnonlead)[1]){
  chars = unique(toupper(servercall_dataall$post_prop17[which(servercall_dataall$post_evar47 == dfleadnonlead$user.ID[i])]))
  decision = any(grepl("RESERVE", chars))
  if (decision == T){
    dfleadnonlead$RESERVE[i] = "1"
  }
}

#add new column for ordernow
dfleadnonlead$OrderNow = rep(0, dim(dfleadnonlead)[1])
for (i in 1:dim(dfleadnonlead)[1]){
  chars = unique(toupper(servercall_dataall$post_prop17[which(servercall_dataall$post_evar47 == dfleadnonlead$user.ID[i])]))
  decision = any(grepl("ORDERNOW", chars))
  if (decision == T){
    dfleadnonlead$OrderNow[i] = "1"
  }
}


#add new column for testdrive
dfleadnonlead$Test.Drive = rep(0, dim(dfleadnonlead)[1])
for (i in 1:dim(dfleadnonlead)[1]){
  chars = unique(toupper(servercall_dataall$post_prop17[which(servercall_dataall$post_evar47 == dfleadnonlead$user.ID[i])]))
  decision = any(grepl("TESTDRIVE", chars))
  if (decision == T){
    dfleadnonlead$Test.Drive[i] = "1"
  }
}

#add new column for getquote
dfleadnonlead$Getquote = rep(0, dim(dfleadnonlead)[1])
for (i in 1:dim(dfleadnonlead)[1]){
  chars = unique(toupper(servercall_dataall$post_prop17[which(servercall_dataall$post_evar47 == dfleadnonlead$user.ID[i])]))
  decision = any(grepl("GETQUOTE", chars))
  if (decision == T){
    dfleadnonlead$Getquote[i] = "1"
  }
}

#add new column for special offer
dfleadnonlead$Special.Offers = rep(0, dim(dfleadnonlead)[1])
for (i in 1:dim(dfleadnonlead)[1]){
  chars = unique(toupper(servercall_dataall$post_prop17[which(servercall_dataall$post_evar47 == dfleadnonlead$user.ID[i])]))
  toMatch = c("SPECIAL-OFFER" , "EXCLUSIVE-OFFER")
  decision = any(grepl(paste(toMatch,collapse="|"), chars))
  if (decision == T){
    dfleadnonlead$Special.Offers[i] = "1"
  }
}


#add new column for gallery
dfleadnonlead$Gallery = rep(0, dim(dfleadnonlead)[1])
for (i in 1:dim(dfleadnonlead)[1]){
  chars = unique(toupper(servercall_dataall$post_prop17[which(servercall_dataall$post_evar47 == dfleadnonlead$user.ID[i])]))
  decision = any(grepl("GALLERY", chars))
  if (decision == T){
    dfleadnonlead$Gallery[i] = "1"
  }
}

#add new colum for specifications
dfleadnonlead$Specifications = rep(0, dim(dfleadnonlead)[1])
for (i in 1:dim(dfleadnonlead)[1]){
  chars = unique(toupper(servercall_dataall$post_prop17[which(servercall_dataall$post_evar47 == dfleadnonlead$user.ID[i])]))
  toMatch = c("SPECIFICATION" , "FEATURES_AND_SPECS")
  decision = any(grepl(paste(toMatch,collapse="|"), chars))
  if (decision == T){
    dfleadnonlead$Specifications[i] = "1"
  }
}

#add new colum for lease
dfleadnonlead$Lease = rep(0, dim(dfleadnonlead)[1])
for (i in 1:dim(dfleadnonlead)[1]){
  chars = unique(toupper(servercall_dataall$post_prop17[which(servercall_dataall$post_evar47 == dfleadnonlead$user.ID[i])]))
  decision = any(grepl("LEASE", chars))
  if (decision == T){
    dfleadnonlead$Lease[i] = "1"
  }
}

#add new column for compare
dfleadnonlead$Compare = rep(0, dim(dfleadnonlead)[1])
for (i in 1:dim(dfleadnonlead)[1]){
  chars = unique(toupper(servercall_dataall$post_prop17[which(servercall_dataall$post_evar47 == dfleadnonlead$user.ID[i])]))
  decision = any(grepl("COMPARE", chars))
  if (decision == T){
    dfleadnonlead$Compare[i] = "1"
  }
}

#add new column for BMW.Value (what can drivers get after purchase)
dfleadnonlead$BMW.Value = rep(0, dim(dfleadnonlead)[1])
for (i in 1:dim(dfleadnonlead)[1]){
  chars = unique(toupper(servercall_dataall$post_prop17[which(servercall_dataall$post_evar47 == dfleadnonlead$user.ID[i])]))
  decision = any(grepl("BMWVALUE", chars))[]
  if (decision == T){
    dfleadnonlead$BMW.Value[i] = "1"
  }
}

#add new column for BMW.Experience (performance driving schools, delievery programs)
dfleadnonlead$BMW.Exp = rep(0, dim(dfleadnonlead)[1])
for (i in 1:dim(dfleadnonlead)[1]){
  chars = unique(toupper(servercall_dataall$post_prop17[which(servercall_dataall$post_evar47 == dfleadnonlead$user.ID[i])]))
  decision = any(grepl("EXPERIENCE", chars))
  if (decision == T){
    dfleadnonlead$BMW.Exp[i] = "1"
  }
}

#add new column for Esimate payment
dfleadnonlead$Estimate.Pay = rep(0, dim(dfleadnonlead)[1])
for (i in 1:dim(dfleadnonlead)[1]){
  chars = unique(toupper(servercall_dataall$post_prop17[which(servercall_dataall$post_evar47 == dfleadnonlead$user.ID[i])]))
  decision = any(grepl("ESTIMATEAPAYMENT", chars))
  if (decision == T){
    dfleadnonlead$Estimate.Pay[i] = "1"
  }
}

#add new column for addvehicle (only get access after creating user account)
dfleadnonlead$Add.Vehicle = rep(0, dim(dfleadnonlead)[1])
for (i in 1:dim(dfleadnonlead)[1]){
  chars = unique(toupper(servercall_dataall$post_prop17[which(servercall_dataall$post_evar47 == dfleadnonlead$user.ID[i])]))
  decision = any(grepl("ADDVEHICLE", chars))
  if (decision == T){
    dfleadnonlead$Add.Vehicle[i] = "1"
  }
}


#add new column for Concept/Future
dfleadnonlead$Concept.Future = rep(0, dim(dfleadnonlead)[1])
for (i in 1:dim(dfleadnonlead)[1]){
  chars = unique(toupper(servercall_dataall$post_prop17[which(servercall_dataall$post_evar47 == dfleadnonlead$user.ID[i])]))
  toMatch = c("CONCEPT" , "FUTURE")
  decision = any(grepl(paste(toMatch,collapse="|"), chars))
  if (decision == T){
    dfleadnonlead$Special.Offers[i] = "1"
  }
}


#add new column for safety
dfleadnonlead$Safety = rep(0, dim(dfleadnonlead)[1])
for (i in 1:dim(dfleadnonlead)[1]){
  chars = unique(toupper(servercall_dataall$post_prop17[which(servercall_dataall$post_evar47 == dfleadnonlead$user.ID[i])]))
  decision = any(grepl("SAFETY", chars))
  if (decision == T){
    dfleadnonlead$Safety[i] = "1"
  }
}

#add geo_city
dfleadnonlead$Geo.City = rep(0, dim(dfleadnonlead)[1])
for (i in 1:dim(dfleadnonlead)[1]){
  dfleadnonlead$Geo.City[i] = tail(servercall_dataall$geo_city[which(servercall_dataall$post_evar47 == dfleadnonlead$user.ID[i])], 1)
}

#add geo_region
dfleadnonlead$Geo.Region = rep(0, dim(dfleadnonlead)[1])
for (i in 1:dim(dfleadnonlead)[1]){
  dfleadnonlead$Geo.Region[i] = tail(servercall_dataall$geo_region[which(servercall_dataall$post_evar47 == dfleadnonlead$user.ID[i])], 1)
}

#add geo_country 
dfleadnonlead$Geo.Country = rep(0, dim(dfleadnonlead)[1])
for (i in 1:dim(dfleadnonlead)[1]){
  dfleadnonlead$Geo.Country[i] = tail(servercall_dataall$geo_country[which(servercall_dataall$post_evar47 == dfleadnonlead$user.ID[i])], 1)
}

#add first_hit_referrer
dfleadnonlead$First.Hit.Referrer = rep(0, dim(dfleadnonlead)[1])
for (i in 1:dim(dfleadnonlead)[1]){
  dfleadnonlead$First.Hit.Referrer[i] = tail(servercall_dataall$first_hit_referrer[which(servercall_dataall$post_evar47 == dfleadnonlead$user.ID[i])], 1)
}

#add visit_ref_type
dfleadnonlead$Visit.Ref.Type = rep(0, dim(dfleadnonlead)[1])
for (i in 1:dim(dfleadnonlead)[1]){
  dfleadnonlead$Visit.Ref.Type[i] = tail(servercall_dataall$visit_ref_type[which(servercall_dataall$post_evar47 == dfleadnonlead$user.ID[i])], 1)
}


#add visit_num
dfleadnonlead$Visit.Num = rep(0, dim(dfleadnonlead)[1])
for (i in 1:dim(dfleadnonlead)[1]){
  dfleadnonlead$Visit.Num[i] = tail(servercall_dataall$visit_num[which(servercall_dataall$post_evar47 == dfleadnonlead$user.ID[i])], 1)
}

#add model selected
dfleadnonlead$Model.Selected = rep(0, dim(dfleadnonlead)[1])
for (i in 1:dim(dfleadnonlead)[1]){
  dfleadnonlead$Model.Selected[i] = 
    as.character(as.data.frame(sort(table(servercall_dataall$post_evar10[which(servercall_dataall$post_evar47 == dfleadnonlead$user.ID[i])]), decreasing = T))[2,1])
}


#make column names consistent
names(dfleadnonlead)[names(dfleadnonlead) == 'lead.complete'] = 'Lead.Complete'
names(dfleadnonlead)[names(dfleadnonlead) == 'visit.freq'] = 'Visit.Freq'
names(dfleadnonlead)[names(dfleadnonlead) == 'RESERVE'] = 'Reserve'
names(dfleadnonlead)[names(dfleadnonlead) == 'OrderNow'] = 'Order.Now'
names(dfleadnonlead)[names(dfleadnonlead) == 'Getquote'] = 'Get.Quote'


#remove redundant column Visit.Freq, correct the values from character to numeric/factor.
#dfleadnonlead =  dfleadnonlead[ , -which(names(dfleadnonlead) %in% "Visit.Freq")]
dfleadnonlead[, c(2:16, 21,22)] = sapply(dfleadnonlead[, c(2:16, 21,22)], as.numeric)
dfleadnonlead$Geo.City = as.factor(dfleadnonlead$Geo.City)
dfleadnonlead$Geo.Country = as.factor(dfleadnonlead$Geo.Country)
dfleadnonlead$Geo.Region = as.factor(dfleadnonlead$Geo.Region)
dfleadnonlead$First.Hit.Referrer = as.factor(dfleadnonlead$First.Hit.Referrer)

#Aggregate keywords in First Hit Referrer
dfleadnonlead$FH.Ref.Site = rep(0, dim(dfleadnonlead)[1])
for (i in 1:dim(dfleadnonlead)[1]){
  chars = toupper(dfleadnonlead$First.Hit.Referrer[i])
  if (grepl("GOOGLE", chars) == T){
    dfleadnonlead$FH.Ref.Site[i] = "Google"
  }
  else if (grepl("YAHOO", chars) == T){
    dfleadnonlead$FH.Ref.Site[i] = "Yahoo"
  }
  else if (grepl("BAIDU", chars) == T){
    dfleadnonlead$FH.Ref.Site[i] = "Baidu"
  }
  else if (grepl("EMAIL", chars) == T){
    dfleadnonlead$FH.Ref.Site[i] = "Email"
  }
  else if (grepl("BMWUSA", chars) == T){
    dfleadnonlead$FH.Ref.Site[i] = "BMWUSA"
  }
  else if (grepl("ADSERVER", chars) == T){
    dfleadnonlead$FH.Ref.Site[i] = "adServer"
  }
  else if (grepl("BING", chars) == T){
    dfleadnonlead$FH.Ref.Site[i] = "Bing"
  }
  else if (grepl("TIER2", chars) == T){
    dfleadnonlead$FH.Ref.Site[i] = "Tier2"
  }
  else if (grepl("CARFAX", chars) == T){
    dfleadnonlead$FH.Ref.Site[i] = "Carfax"
  }
  else if (grepl("ADS", chars) == T){
    dfleadnonlead$FH.Ref.Site[i] = "Ads"
  }
  else if (grepl("EBAY", chars) == T){
    dfleadnonlead$FH.Ref.Site[i] = "Ebay"
  }
  else if (grepl("INSTAGRAM", chars) == T){
    dfleadnonlead$FH.Ref.Site[i] = "Instagram"
  }
  else if (grepl("FACEBOOK", chars) == T){
    dfleadnonlead$FH.Ref.Site[i] = "Facebook"
  }
  else if (grepl("AUTOBLOG", chars) == T){
    dfleadnonlead$FH.Ref.Site[i] = "Autoblog"
  }
  else if (grepl("FIXYA", chars) == T){
    dfleadnonlead$FH.Ref.Site[i] = "Fixya"
  }
  else if (grepl("OUTLOOK", chars) == T){
    dfleadnonlead$FH.Ref.Site[i] = "Outlook"
  }
  else if (grepl("FORUMS", chars) == T){
    dfleadnonlead$FH.Ref.Site[i] = "Forums"
  }
  else if (grepl("TWITTER", chars) == T){
    dfleadnonlead$FH.Ref.Site[i] = "twitter"
  }
  else{
    dfleadnonlead$FH.Ref.Site[i] = "Others"
  }
}


# Logistic Regression
dfleadnonlead1<- dfleadnonlead[, c(2,4:17,20, 23,22)] 
#dfleadnonlead1<-na.omit(dfleadnonlead1)
dfleadnonlead1$Geo.Country[which(dfleadnonlead1$Geo.Country!="usa")]<-"other country"
#dfleadnonlead1$Geo.Country<-as.factor(dfleadnonlead1$Geo.Country)
dfleadnonlead2<- apply(dfleadnonlead1,2,as.factor)
dfleadnonlead2<-as.data.frame(dfleadnonlead2)
dfleadnonlead2$Lead.Complete<-as.numeric(dfleadnonlead2$Lead.Complete)
dfleadnonlead2$Visit.Num<-as.numeric(dfleadnonlead2$Visit.Num)
dfleadnonlead2$Lead.Complete<-dfleadnonlead2$Lead.Complete-1

sample<-sample.int(n=nrow(dfleadnonlead),size=floor(0.75*nrow(dfleadnonlead)),replace=F)
train<-dfleadnonlead2[sample, ]
test<-dfleadnonlead2[-sample, ]
glm<-glm(Lead.Complete ~.,family="binomial",data=train)
summary(glm)
#
stepglm<-step(glm,direction = "both")  
summary(stepglm)  

real <- test$Lead.Complete
predict. <- predict.glm(glm,type='response',newdata=test)  
predict =ifelse(predict.>0.5,1,0)  

library(pROC)  
modelroc <- roc(real,predict.)  
plot(modelroc, print.auc=TRUE, auc.polygon=TRUE,legacy.axes=TRUE, grid=c(0.1, 0.2),  
     grid.col=c("green", "red"), max.auc.polygon=TRUE,  
     auc.polygon.col="skyblue", print.thres=TRUE)        #，，  

res <- data.frame(real,predict )  
table(real,predict =ifelse(predict>0.5,'lead','notlead'))  


glm2<-glm(formula = Lead.Complete ~ BYO + Order.Now + Special.Offers + 
            Gallery + Specifications + Lease + Estimate.Pay, family = "binomial", 
          data = train) 
summary(glm2)
coef(glm2)
real<- test$Lead.Complete
predict2. <- predict.glm(glm2,type='response',newdata=test)  
predict2 =ifelse(predict2.>mean(predict2.),1,0)  

##ROC 
library(pROC)  
modelroc2 <- roc(real,predict2.)  
plot(modelroc2, print.auc=TRUE, auc.polygon=TRUE,legacy.axes=TRUE, grid=c(0.1, 0.2),  
     grid.col=c("green", "red"), max.auc.polygon=TRUE,  
     auc.polygon.col="skyblue", print.thres=TRUE)        
##
res2 <- data.frame(real,predict2 )  
table(real,predict2 =ifelse(predict2>mean(predict2.),'lead','notlead'))  

error = predict2-real
accuracy = (nrow(test)-sum(abs(error)))/nrow(test) 
accuracy

#install.packages("caret")  
library(caret)
set.seed(7)  
folds <- createFolds(y=dfleadnonlead2$Lead.Complete,k=10)  
max=0  
num=0  
for(i in 1:10){  
  fold_test <- dfleadnonlead2[folds[[i]],]   #[[i]]  
  fold_train <- dfleadnonlead2[-folds[[i]],]   #   
  print("***group number***")  
  
  fold_pre <- glm(Lead.Complete ~.,family=binomial(link='logit'),data=fold_train)  
  fold_predict <- predict(fold_pre,type='response',newdata=fold_test)  
  fold_predict =ifelse(fold_predict>mean(predict2.),1,0)  
  fold_test$predict = fold_predict  
  fold_error = fold_test$predict-fold_test$Lead.Complete 
  fold_accuracy = (nrow(fold_test)-sum(abs(fold_error)))/nrow(fold_test)   
  print(i)  
  print("***accuracy of test dataset***")  
  print(fold_accuracy)  
  print("***accuracy of train dataset***")  
  fold_predict2 <- predict(fold_pre,type='response',newdata=fold_train)  
  fold_predict2 =ifelse(fold_predict2>mean(predict2.),1,0)  
  fold_train$predict = fold_predict2  
  fold_error2 = fold_train$predict-fold_train$Lead.Complete
  fold_accuracy2 = (nrow(fold_train)-sum(abs(fold_error2)))/nrow(fold_train)   
  print(fold_accuracy2)  
  
  if(fold_accuracy>max)  
  {  
    max=fold_accuracy    
    num=i  
  }  
}  
print(max)  
print(num)  

testi <- dfleadnonlead2[folds[[num]],]  
traini <- dfleadnonlead2[-folds[[num]],]   
prei <- glm(Lead.Complete ~.,family=binomial(link='logit'),data=traini)  
predicti <- predict.glm(prei,type='response',newdata=testi)  
predicti =ifelse(predicti>mean(predict2.),1,0)  
testi$predict = predicti  
errori = testi$predict-testi$Lead.Complete
accuracyi = (nrow(testi)-sum(abs(errori)))/nrow(testi)   
accuracyi
predicti2 <- predict.glm(prei,type='response',newdata=traini)  
predicti2 =ifelse(predicti2>mean(predict2.),1,0)  
traini$predict = predicti2  
errori2 = testi$predict-testi$Lead.Complete
accuracyi2 = (nrow(traini)-sum(abs(errori2)))/nrow(traini)   
accuracyi2
accuracyi;num;accuracyi2

View(dfleadnonlead)
library(randomForest)
dfleadnonlead1<-dfleadnonlead[,-1]
#data partition
set.seed(113)
dfleadnonlead1<-na.omit(dfleadnonlead1)
id<-sample(2,nrow(dfleadnonlead1),replace=TRUE,prob=c(0.7,0.3))
train<-dfleadnonlead1[id==1,]
test<-dfleadnonlead1[id==2,]
dfleadnonlead1$Geo.Country[which(dfleadnonlead1$Geo.Country!="usa")]<-"other country"
set.seed(1234)
rf<-randomForest(factor(Lead.Complete) ~ .,data=train,ntree=100,proximity=TRUE)
print(rf)
plot(rf)
#what variables are important
varImpPlot(rf)
# Contruct ROC curve
library(ROCR)
pred<-predict(rf,newdata=test,'prob')
pred<-prediction(pred,test$Lead.Complete)
roc<-performance(pred,'tpr','fpr')
plot(roc,colorize=T,main='ROC curve',xlab='1-Specificity',ylab='Sensitivity',print.curoffs.at=seq(0,1,0.5), grid=c(0.1, 0.2),)
abline(0,1)
#Area Under Curve(AUC)
auc<-performance(pred,'auc')
unlist(slot(auc,'y.values'))
auc
#prediction with train data
p1<-predict(rf,train)
ct<-table(train$Lead.Complete,p1); ct
accuracy<-(ct[1,1]+ct[2,2])/nrow(train); accuracy
confusionMatrix(p1,train$Lead.Complete)
#prediction with test data
p2<-predict(rf,newdata=test)
confusionMatrix(p2,test$Lead.Complete)
accuracy<-(ct[1,1]+ct[2,2])/nrow(test); accuracy
# the importance of different variables
importance<-importance(rf)
varImportance<-data.frame(Variables=row.names(importance),Importance=round(importance[ ,'MeanDecreaseGini'],2))
install.packages("magrittr")
library(magrittr)
rankImportance<-varImportance %>% 
  mutate(Rank=paste0('#',dense_rank(desc(importance))))
library(ggplot2)
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_bw()

#drawing graphs
df <- dfleadnonlead
str(df)
library('ggplot2')

#distribution of visit frequence (0-100) 
ggplot(df, aes(x=df$Visit.Freq)) + 
  geom_histogram(aes(fill='Private'), color='black', bins=50, alpha=0.7) + 
  theme_bw() + 
  scale_x_continuous(limits=c(0,100))

library('data.table')
referrer_type <- read.csv("referrer.csv")
library('dplyr')
df_refer <- df %>% select(First.Hit.Referrer, Visit.Ref.Type)
df_refer <- left_join(df_refer, referrer_type, by=c("Visit.Ref.Type"="ID"))
df$refer <- df_refer$type
by_type <- df %>% group_by(refer) %>% summarize(count=n()) 
by_type$type <- 0
by_type_lead <- df %>% filter(Lead.Complete == "1") %>% group_by(refer) %>% summarize(count=n())
by_type_lead$type <- 1
by_type_total <- rbind(by_type, by_type_lead)

library('ggplot2')
ggplot(by_type, aes(x=refer, y=count)) + geom_col()

df = read.csv('LeadsFull_Verified_CleanV3.csv')

#summary statistics
dim(df)
head(df)
summary(df)

#map plotting - United States
install.packages('maps')
library(ggplot2)
library(maps)
#subset the geographic dataframe and correct the mistake in data
library('dplyr')
geography <- df %>% select(Longitude, Latitude) %>% filter(Longitude> -130 & Latitude<50)

#plotting the United States map and the datapoints
us <- map_data("state")
mapplot2 <- ggplot() +geom_polygon(data=us, aes(x=long, y=lat, group=group), color="white")
mapplot2
mapplot2 <- mapplot2 + geom_point(data=geography, aes(x=geography$Longitude, y=geography$Latitude), color="skyblue") 
mapplot2
  
  

