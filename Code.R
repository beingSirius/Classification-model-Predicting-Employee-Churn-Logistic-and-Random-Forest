#loading the data
achurn<-read.csv("C://Users/shaur/Downloads/google-amazon-facebook-employee-reviews (1) - Copy/employee_reviews.csv",header = TRUE)
#Verifying the data
head(achurn)
str(achurn)

#loading the dplyr library required for data manulupation
library(dplyr)


#dropping variable link which are not valid for the model
achurn1 <- select(achurn, -link)


#dropping string variables which are not required for intial model and will be apart of phase 2
achurn2<-achurn1 %>% select(-c(pros,cons,summary,advice.to.mgmt))


#checking for distict values in each column
achurn2 %>% summarise_each(funs(n_distinct(.)))

#checking for na values
sapply(achurn2, function(x) sum(is.na(x)))

achurn2$Year<-as.factor(achurn2$Year)
achurn2$overall.ratings<-as.factor(achurn2$overall.ratings)

# Library extra grid and plotting graph in a single pane
library(gridExtra)
grid.arrange(p1, p2, nrow = 1)
library(ggplot2)

#EDA graphs for analysing features
p1 <- ggplot(achurn2, aes(x=overall.ratings)) + ggtitle("Overall") + xlab("Overall Ratings") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p1

p2 <- ggplot(achurn2, aes(x=work.balance.stars)) + ggtitle("Work Balance") + xlab("Work Balance Ratings") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p2
grid.arrange(p1, p2, nrow = 1)
p3 <- ggplot(achurn2, aes(x=culture.values.stars)) + ggtitle("Culture Values") + xlab("Culture Values Ratings") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p3

p4 <- ggplot(achurn2, aes(x=carrer.opportunities.stars)) + ggtitle("Carrer Opportunities") + xlab("Carrer Opportunities Ratings") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p4

p5 <- ggplot(achurn2, aes(x=comp.benefit.stars)) + ggtitle("Compensation Benefit") + xlab("Compensation Benefit Ratings") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p5
p6 <- ggplot(achurn2, aes(x=senior.mangemnet.stars)) + ggtitle("Senior Mangemnent") + xlab("Senior Mangemnent Ratings") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p6
p7 <- ggplot(achurn2, aes(x=Year)) + ggtitle("Year") + xlab("Year") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p7
p8 <- ggplot(achurn2, aes(x=Month)) + ggtitle("Month") + xlab("Month") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p8


#Graph generation in single pane
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,nrow = 2)
str(achurn2)

#checking none counts
library(dplyr) 
None_in_achurn2<-group_by(achurn2) %>% 
  summarise(counts.job = sum(job=='none'),
            counts.overall.ratings = sum(overall.ratings=='none'), 
            counts.work.balance.stars = sum(work.balance.stars=='none'),
            counts.culture.values.stars = sum(culture.values.stars=='none'),
            counts.carrer.opportunities.stars = sum(carrer.opportunities.stars=='none'),
            counts.comp.benefit.stars = sum(comp.benefit.stars=='none'),
            counts.Month = sum(Month=='none'),
            counts.Year = sum(Year=='none'),
            counts.senior.mangemnet.stars = sum(senior.mangemnet.stars=='none'))

None_in_achurn2

#Backup point creation for recovery
achurn4<-achurn2


library(plyr)

# Updating feature names for better convience
str(achurn4)
colnames(achurn4)<- c("Index","company","location","Month","Date","Year","churn_status","overall_ratings","work_balance_stars","culture_values_stars","Carrer_opp_stars","comp_ben_stars","senior_mgmt_stars","helpful_count")
head(achurn4)



#back up point
achurn5<-achurn4
achurn5 %>% summarise_each(funs(n_distinct(.)))

# None value deletion
achurn5<-achurn5 %>%
  filter(work_balance_stars != c('none')) %>%
  filter(culture_values_stars != c('none')) %>%
  filter(Carrer_opp_stars != c('none')) %>%
  filter(comp_ben_stars != c('none')) %>%
  filter(senior_mgmt_stars != c('none')) 
par(mfrow=c(1,1))

#Data verification and replotting
str(achurn5)


achurn5 %>% summarise_each(funs(n_distinct(.)))
str(achurn5)

p111 <- ggplot(achurn5, aes(x=overall_ratings)) + ggtitle("Overall Ratings") + xlab("Overall Ratings") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p111

p222 <- ggplot(achurn5, aes(x=work_balance_stars)) + ggtitle("Work Balance Ratings") + xlab("Work Balance Ratings") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p222

p333 <- ggplot(achurn5, aes(x=culture_values_stars)) + ggtitle("Culture Values Ratings") + xlab("Culture Values Ratings") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p333

p444 <- ggplot(achurn5, aes(x=Carrer_opp_stars)) + ggtitle("Carrer Opportunities Ratings") + xlab("Carrer Opportunities Ratings") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p444

p555 <- ggplot(achurn5, aes(x=comp_ben_stars)) + ggtitle("Compensation Benefit Ratings") + xlab("Compensation Benefit Ratings") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p555
p666 <- ggplot(achurn5, aes(x=senior_mgmt_stars)) + ggtitle("Senior Mangemnent Ratings") + xlab("Senior Mangemnent Ratings") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p666

p777 <- ggplot(achurn5, aes(x=company)) + ggtitle("Organizations") + xlab("Organizations") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p777

p888 <- ggplot(achurn5, aes(x=churn_status)) + ggtitle("Churn Status") + xlab("Churn Status") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p888

p999 <- ggplot(achurn5, aes(x=Month)) + ggtitle("Month") + xlab("Month") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p999

p1010 <- ggplot(achurn5, aes(x=Year)) + ggtitle("Year") + xlab("Year") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p1010

grid.arrange(p111,p222,p333,p444,p555,p666,p777,p888,p999,p1010,nrow = 3)

#---------------------------------------------
#install.packages("sqldf")
library(sqldf)
require(sqldf)
az<-sqldf('select churn_status,count(*) as COUNT  from achurn5 GROUP BY churn_status')
az
#install.packages("wesanderson")
library(wesanderson)
Plot_UCR = ggplot(az, aes(x=churn_status, y=COUNT)) + geom_bar(stat="identity", width=.5, fill = "#FF6666")
Plot_UCR=Plot_UCR + theme(axis.text.x = element_text(colour = 'black', angle = 45, size = 10,hjust = 0.5, vjust = 0.5,face = 'bold'),axis.title.x=element_blank())+
  theme(axis.text.y = element_text(colour = 'black', size = 10), axis.title.y = element_text(size = 10, hjust = 0.5, vjust = 0.2)) + 
  theme(strip.text.y = element_text(size = 10, hjust = 0.5, vjust = 0.5, face = 'bold'))+
  theme(panel.background = element_rect(fill = 'cornsilk2'))+
  theme(legend.text=element_text(size=15,face ='bold'),legend.title =element_text(size = 15,face ='bold') )  
Plot_UCR 

#----------------------------------------------------------------------------
#churn5$overall_ratings<-as.factor(churn5$overall_ratings)
aEmp_status<-sqldf('select churn_status,company,Month,Year,overall_ratings,work_balance_stars, culture_values_stars,Carrer_opp_stars,comp_ben_stars,senior_mgmt_stars,Count(*) as Count from achurn5 GROUP BY churn_status,company,Month,Year,overall_ratings,work_balance_stars,culture_values_stars,Carrer_opp_stars,comp_ben_stars,senior_mgmt_stars ORDER BY Count  DESC')

aEmp_status

ES1 <-ggplot(aEmp_status, aes(x=churn_status, y=Count,fill=overall_ratings))
ES1 = ES1 +geom_bar(stat = "identity")
ES1 = ES1 + labs(x = "Employee Status", y = 'Total Employee Count', fill = 'Overall ratings', title = "Overall rating by employee status")
ES1 = ES1 + theme(axis.title.x=element_text(size = 10,face ='bold'),
                  axis.text.x=element_text(size = 10,face ='bold'),
                  axis.ticks.x=element_blank())+
  theme(axis.text.y = element_text( size = 10,face = 'bold'), 
        axis.title.y = element_text(size = 10,face ='bold'))+
  theme(legend.text=element_text(size=10,face ='bold'),legend.title =element_text(size = 10,face ='bold') )
ES1


ES2 <-ggplot(aEmp_status, aes(x=churn_status, y=Count,fill=work_balance_stars))
ES2 = ES2 +geom_bar(stat = "identity")
ES2 = ES2 + labs(x = "Employee Status", y = 'Total Employee Count', fill = 'Work Balance Rating', title = "Work balance rating by employee status")
ES2 = ES2 + theme(axis.title.x=element_text(size = 10,face ='bold'),
                  axis.text.x=element_text(size = 10,face ='bold'),
                  axis.ticks.x=element_blank())+
  theme(axis.text.y = element_text( size = 10,face = 'bold'), 
        axis.title.y = element_text(size = 10,face ='bold'))+
  theme(legend.text=element_text(size=10,face ='bold'),legend.title =element_text(size = 10,face ='bold') )
ES2

ES3 <-ggplot(aEmp_status, aes(x=churn_status, y=Count,fill=culture_values_stars))
ES3 = ES3 +geom_bar(stat = "identity")
ES3 = ES3 + labs(x = "Employee Status", y = 'Total Employee Count', fill = 'Culture Value Rating', title = "Culture Value rating by employee status")
ES3 = ES3 + theme(axis.title.x=element_text(size = 10,face ='bold'),
                  axis.text.x=element_text(size = 10,face ='bold'),
                  axis.ticks.x=element_blank())+
  theme(axis.text.y = element_text( size = 10,face = 'bold'), 
        axis.title.y = element_text(size = 10,face ='bold'))+
  theme(legend.text=element_text(size=10,face ='bold'),legend.title =element_text(size = 10,face ='bold') )
ES3

ES4 <-ggplot(aEmp_status, aes(x=churn_status, y=Count,fill=Carrer_opp_stars))
ES4 = ES4 +geom_bar(stat = "identity")
ES4 = ES4 + labs(x = "Employee Status", y = 'Total Employee Count', fill = 'Career opp Rating', title = "Career rating by employee status")
ES4 = ES4 + theme(axis.title.x=element_text(size = 10,face ='bold'),
                  axis.text.x=element_text(size = 10,face ='bold'),
                  axis.ticks.x=element_blank())+
  theme(axis.text.y = element_text( size = 10,face = 'bold'), 
        axis.title.y = element_text(size = 10,face ='bold'))+
  theme(legend.text=element_text(size=10,face ='bold'),legend.title =element_text(size = 10,face ='bold') )
ES4


ES5 <-ggplot(aEmp_status, aes(x=churn_status, y=Count,fill=comp_ben_stars))
ES5 = ES5 +geom_bar(stat = "identity")
ES5 = ES5 + labs(x = "Employee Status", y = 'Total Employee Count', fill = 'Company benifits Rating', title = "Company benifits by employee status")
ES5 = ES5 + theme(axis.title.x=element_text(size = 10,face ='bold'),
                  axis.text.x=element_text(size = 10,face ='bold'),
                  axis.ticks.x=element_blank())+
  theme(axis.text.y = element_text( size = 10,face = 'bold'), 
        axis.title.y = element_text(size = 10,face ='bold'))+
  theme(legend.text=element_text(size=10,face ='bold'),legend.title =element_text(size = 10,face ='bold') )
ES5

ES6 <-ggplot(aEmp_status, aes(x=churn_status, y=Count,fill=senior_mgmt_stars))
ES6 = ES6 +geom_bar(stat = "identity")
ES6 = ES6 + labs(x = "Employee Status", y = 'Total Employee Count', fill = 'Senior management Rating', title = "Senior management by employee status")
ES6 = ES6 + theme(axis.title.x=element_text(size = 10,face ='bold'),
                  axis.text.x=element_text(size = 10,face ='bold'),
                  axis.ticks.x=element_blank())+
  theme(axis.text.y = element_text( size = 10,face = 'bold'), 
        axis.title.y = element_text(size = 10,face ='bold'))+
  theme(legend.text=element_text(size=10,face ='bold'),legend.title =element_text(size = 10,face ='bold') )
ES6


ES7 <-ggplot(aEmp_status, aes(x=churn_status, y=Count,fill=company))
ES7 = ES7 +geom_bar(stat = "identity")
ES7 = ES7 + labs(x = "Employee Status", y = 'Total Company Count', fill = 'Companies', title = "Companies")
ES7 = ES7 + theme(axis.title.x=element_text(size = 10,face ='bold'),
                  axis.text.x=element_text(size = 10,face ='bold'),
                  axis.ticks.x=element_blank())+
  theme(axis.text.y = element_text( size = 10,face = 'bold'), 
        axis.title.y = element_text(size = 10,face ='bold'))+
  theme(legend.text=element_text(size=10,face ='bold'),legend.title =element_text(size = 10,face ='bold') )
ES7

ES8 <-ggplot(aEmp_status, aes(x=churn_status, y=Count,fill=Month))
ES8 = ES8 +geom_bar(stat = "identity")
ES8 = ES8 + labs(x = "Employee Status", y = 'Months', fill = 'Months', title = "Months")
ES8 = ES8 + theme(axis.title.x=element_text(size = 10,face ='bold'),
                  axis.text.x=element_text(size = 10,face ='bold'),
                  axis.ticks.x=element_blank())+
  theme(axis.text.y = element_text( size = 10,face = 'bold'), 
        axis.title.y = element_text(size = 10,face ='bold'))+
  theme(legend.text=element_text(size=10,face ='bold'),legend.title =element_text(size = 10,face ='bold') )
ES8


ES9 <-ggplot(aEmp_status, aes(x=churn_status, y=Count,fill=Year))
ES9 = ES9 +geom_bar(stat = "identity")
ES9 = ES9 + labs(x = "Employee Status", y = 'Year', fill = 'Year', title = "Year")
ES9 = ES9 + theme(axis.title.x=element_text(size = 10,face ='bold'),
                  axis.text.x=element_text(size = 10,face ='bold'),
                  axis.ticks.x=element_blank())+
  theme(axis.text.y = element_text( size = 10,face = 'bold'), 
        axis.title.y = element_text(size = 10,face ='bold'))+
  theme(legend.text=element_text(size=10,face ='bold'),legend.title =element_text(size = 10,face ='bold') )
ES9

grid.arrange(ES1,ES2,ES3,ES4,nrow = 2)
grid.arrange(ES5,ES6,ES7,ES8,ES9,nrow = 3)
#---------------------------------------------------------------------
#Testing and data set creation 
#2018 data has been identified as test data ~ 20% of the data
#2012 to 2017 has been identified as training data ~ 80 of the data


atrain<-achurn5 %>%
  filter(Year!="2018")
atest<-achurn5 %>%
  filter(Year=="2018")

str(atrain)
str(atest)

atrain$Year=as.integer(atrain$Year)

atest$Year=as.integer(atest$Year)

dim(atrain)
dim(atest)

str(atrain)
str(atest)

#-------------------------------------------------------------
#back up point 
achurn6<-achurn5

#---------------------------------------------------------------------
#Logistic model will all vriables

aLogit <-glm(churn_status~Month+Year+overall_ratings+
               work_balance_stars+culture_values_stars+
               Carrer_opp_stars+comp_ben_stars+senior_mgmt_stars+
               company,data=atrain,family='binomial')

summary(aLogit)

#Model creation with predictor variable in sequence from higher priorty to low figured from aLogit
aLogit_sig_variables<-glm(churn_status~company+overall_ratings+
                            Year+senior_mgmt_stars+work_balance_stars+
                            culture_values_stars+Month+comp_ben_stars+
                            Carrer_opp_stars,data=atrain,family='binomial')


summary(aLogit_sig_variables)

#Performing analysis of vairance

anova(aLogit_sig_variables, test="Chisq")


#Final model with the top variables selected from the annova test
aFinal_model<-glm(churn_status~company+overall_ratings+
                    Year+senior_mgmt_stars+work_balance_stars+
                    culture_values_stars,data=atrain,family='binomial')
summary(aFinal_model)


# Checking the model and finding the new threshhold

ap1<- predict(aFinal_model,atrain, type='response')
ap1
apred1 <-ifelse(ap1>.5,"Current Employee ","Former Employee ")
str(apred1)
apred1<-as.factor(apred1)
aconf_matrix1 = confusionMatrix(apred1,atrain$churn_status)
aconf_matrix1
#grid.arrange(a,nrow = 1)
library(ROCR)
aROCRPred = prediction(ap1,atrain$churn_status)
aROCRPref <- performance(aROCRPred,"tpr","fpr")
plot(aROCRPref,colorize=TRUE,print.cutoffs.at=seq(.1,by=0.1))

apred3 <-ifelse(ap1>.25,"Current Employee ","Former Employee ")
str(apred3)
apred3<-as.factor(apred3)
aconf_matrix3 = confusionMatrix(apred3,atrain$churn_status)
aconf_matrix3

#------------------------------------------------------------
#Implementing the model on test data and compare

s1<- predict(aFinal_model,atest, type='response')
s1
s_pred1 <-ifelse(s1>.25,"Current Employee ","Former Employee ")
s_pred1
str(s_pred1)
s_pred1<-as.factor(s_pred1)
s_conf_matrix1 = confusionMatrix(s_pred1,atest$churn_status)
s_conf_matrix1
ROCRPred_s = prediction(s1,atest$churn_status)
ROCRPref_s <- performance(ROCRPred_s,"tpr","fpr")
#plot(ROCRPref_s,colorize=TRUE,print.cutoffs.at=seq(.1,by=0.1))


# ROC area under the curve
auc.tmp <- performance(ROCRPred_s,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc


#------------------------------------------------------------------
#Randomforest 
#------------------------------------------------------------------
library(randomForest)
Randomforestm1<- randomForest(churn_status~company+overall_ratings+
                                Year+senior_mgmt_stars+work_balance_stars+
                                culture_values_stars+Month+comp_ben_stars+
                                Carrer_opp_stars,
                              method="class", data = atrain, ntree=500)

Auc_for_initial_model = predict(Randomforestm1, newdata = atrain, type = "prob")
Auc_for_initial_model_auc <- prediction(Auc_for_initial_model[,1], atrain$churn_status)


auc_first_model <- performance(Auc_for_initial_model_auc,"auc")
auc_fm<- as.numeric(auc_first_model@y.values)
auc_fm


#--------------------------------------
#Error Out of the bound error rate changes with the number of trees

Randomforestm1# view results 

oob.error.data<-data.frame(
  Trees=rep(1:nrow(Randomforestm1$err.rate), times=3),
  Type=rep(c("OOB", "Current Employee ", "Former Employee "),each=nrow(Randomforestm1$err.rate)),
  Error=c(Randomforestm1$err.rate[,"OOB"],
          Randomforestm1$err.rate[,"Current Employee "],
          Randomforestm1$err.rate[,"Former Employee "]))
ggplot(data=oob.error.data,aes(x=Trees,y=Error))+
  geom_line(aes(color=Type))

importance(Randomforestm1) # importance of each predictor




#--------------------------------------------------------------------------
# Developing different models for error rate comparison

Randomforestm1_2<- randomForest(churn_status~company+overall_ratings+
                                  Year+senior_mgmt_stars+work_balance_stars+
                                  culture_values_stars+Month+comp_ben_stars+
                                  Carrer_opp_stars,
                                method="class", data = atrain,mtry=2, ntree=150)



Randomforestm1_3<- randomForest(churn_status~company+overall_ratings+
                                  Year+senior_mgmt_stars+work_balance_stars+
                                  culture_values_stars+Month+comp_ben_stars+
                                  Carrer_opp_stars,
                                method="class", data = atrain,mtry=3, ntree=150)

Randomforestm1_4<- randomForest(churn_status~company+overall_ratings+
                                  Year+senior_mgmt_stars+work_balance_stars+
                                  culture_values_stars+Month+comp_ben_stars+
                                  Carrer_opp_stars,
                                method="class", data = atrain,mtry=4, ntree=150)

Randomforestm1_5<- randomForest(churn_status~company+overall_ratings+
                                  Year+senior_mgmt_stars+work_balance_stars+
                                  culture_values_stars+Month+comp_ben_stars+
                                  Carrer_opp_stars,
                                method="class", data = atrain,mtry=5, ntree=150)
Randomforestm1_6<- randomForest(churn_status~company+overall_ratings+
                                  Year+senior_mgmt_stars+work_balance_stars+
                                  culture_values_stars+Month+comp_ben_stars+
                                  Carrer_opp_stars,
                                method="class", data = atrain,mtry=6, ntree=150)


#for(i in 1:6){Randomforestm1_i$err.rate[150,1]}

#for(i in 1:6){print(i)}

print(Randomforestm1_2$err.rate[150,1])
print(Randomforestm1_3$err.rate[150,1])
print(Randomforestm1_4$err.rate[150,1])
print(Randomforestm1_5$err.rate[150,1])
print(Randomforestm1_6$err.rate[150,1])
#------------------------------------------------------------------------------------


#Creating and testing the selected model with test data

Randomforestm1_2_less_train<- randomForest(churn_status~Year+work_balance_stars+
                                             +Month+comp_ben_stars+
                                             Carrer_opp_stars,
                                           method="class", data = atrain,mtry=2, ntree=150)

Randomforestm1_2_less_train

Randomforestm1_2_less<- randomForest(churn_status~Year+work_balance_stars+
                                       +Month+comp_ben_stars+
                                       Carrer_opp_stars,
                                     method="class", data = atest,mtry=2, ntree=150)
Randomforestm1_2_less



Auc_for_initial_mode_less = predict(Randomforestm1_2_less, newdata = atest, type = "prob")
Auc_for_initial_model_auc_less <- prediction(Auc_for_initial_mode_less[,2], atest$churn_status)


auc_first_model_less <- performance(Auc_for_initial_model_auc_less,"auc")
auc_fm_less<- as.numeric(auc_first_model_less@y.values)
auc_fm_less




#plotting roc

Direction.2006 = atest$churn_status

Direction.2006

z<- Direction.2006
z

check1 = predict(Randomforestm1_2_less, newdata = atest, type = "prob")
check1
model_on_test<- prediction(check1[,2], z)
ROC_forest <- performance(model_on_test, "tpr", "fpr")
plot (ROC_forest,col='blue')
plot(ROCRPref_s,col='green',add = TRUE)
legend(0, 1, legend=c("Logistic Reg", "Random Forest"),
       col=c("green","blue"), lty=1:2, text.font=4,cex=.5)
abline(a=0,b=1,lty=2)


auc_random_11 <- performance(model_on_test,"auc")
auc1 <- as.numeric(auc_random_11@y.values)
auc1

#--------------------------------------------------------------------------

#Checking the class imbalance data after review from professor
par(mfrow=c(2,3))

google <- atrain%>% filter(company=='google')
str(google)


google_az<-sqldf('select churn_status,count(*) as COUNT  from google GROUP BY churn_status')
google_az
google_plot = ggplot(google_az, aes(x=churn_status, y=COUNT)) + geom_bar(stat="identity", width=.5, fill = "#FF6666")
google_plot=google_plot + ggtitle("Google Employee status") + xlab("Employee status")+theme(axis.text.x = element_text(colour = 'black', angle = 45, size = 10,hjust = 0.5, vjust = 0.5,face = 'bold'),axis.title.x=element_blank())+
  theme(axis.text.y = element_text(colour = 'black', size = 10), axis.title.y = element_text(size = 10, hjust = 0.5, vjust = 0.2)) + 
  theme(strip.text.y = element_text(size = 10, hjust = 0.5, vjust = 0.5, face = 'bold'))+
  theme(panel.background = element_rect(fill = 'cornsilk2'))+
  theme(legend.text=element_text(size=15,face ='bold'),legend.title =element_text(size = 15,face ='bold') )  
google_plot 





apple <- atrain%>% filter(company=='apple')
str(apple)

apple_az<-sqldf('select churn_status,count(*) as COUNT  from apple GROUP BY churn_status')
apple_az
apple_plot = ggplot(apple_az, aes(x=churn_status, y=COUNT)) + geom_bar(stat="identity", width=.5, fill = "#FF6666")
apple_plot=apple_plot + ggtitle("Apple Employee status") + xlab("Employee status")+theme(axis.text.x = element_text(colour = 'black', angle = 45, size = 10,hjust = 0.5, vjust = 0.5,face = 'bold'),axis.title.x=element_blank())+
  theme(axis.text.y = element_text(colour = 'black', size = 10), axis.title.y = element_text(size = 10, hjust = 0.5, vjust = 0.2)) + 
  theme(strip.text.y = element_text(size = 10, hjust = 0.5, vjust = 0.5, face = 'bold'))+
  theme(panel.background = element_rect(fill = 'cornsilk2'))+
  theme(legend.text=element_text(size=15,face ='bold'),legend.title =element_text(size = 15,face ='bold') )  
apple_plot 


amazon <- atrain%>% filter(company=='amazon')
str(amazon)

amazon_az<-sqldf('select churn_status,count(*) as COUNT  from amazon GROUP BY churn_status')
amazon_az
Amazon_plot = ggplot(amazon_az, aes(x=churn_status, y=COUNT)) + geom_bar(stat="identity", width=.5, fill = "#FF6666")
Amazon_plot=Amazon_plot + ggtitle("Amazon Employee status") + xlab("Employee status")+theme(axis.text.x = element_text(colour = 'black', angle = 45, size = 10,hjust = 0.5, vjust = 0.5,face = 'bold'),axis.title.x=element_blank())+
  theme(axis.text.y = element_text(colour = 'black', size = 10), axis.title.y = element_text(size = 10, hjust = 0.5, vjust = 0.2)) + 
  theme(strip.text.y = element_text(size = 10, hjust = 0.5, vjust = 0.5, face = 'bold'))+
  theme(panel.background = element_rect(fill = 'cornsilk2'))+
  theme(legend.text=element_text(size=15,face ='bold'),legend.title =element_text(size = 15,face ='bold') )  
Amazon_plot 


microsoft <- atrain%>% filter(company=='microsoft')
str(microsoft)

microsoft_az<-sqldf('select churn_status,count(*) as COUNT  from microsoft GROUP BY churn_status')
microsoft_az
Microsoft_plot = ggplot(microsoft_az, aes(x=churn_status, y=COUNT)) + geom_bar(stat="identity", width=.5, fill = "#FF6666")
Microsoft_plot=Microsoft_plot + ggtitle("Microsoft Employee status") + xlab("Employee status")+theme(axis.text.x = element_text(colour = 'black', angle = 45, size = 10,hjust = 0.5, vjust = 0.5,face = 'bold'),axis.title.x=element_blank())+
  theme(axis.text.y = element_text(colour = 'black', size = 10), axis.title.y = element_text(size = 10, hjust = 0.5, vjust = 0.2)) + 
  theme(strip.text.y = element_text(size = 10, hjust = 0.5, vjust = 0.5, face = 'bold'))+
  theme(panel.background = element_rect(fill = 'cornsilk2'))+
  theme(legend.text=element_text(size=15,face ='bold'),legend.title =element_text(size = 15,face ='bold') )  
Microsoft_plot 




facebook <- atrain%>% filter(company=='facebook')
str(facebook)

Facebook_az<-sqldf('select churn_status,count(*) as COUNT  from facebook GROUP BY churn_status')
Facebook_az
Facebook_plot = ggplot(Facebook_az, aes(x=churn_status, y=COUNT)) + geom_bar(stat="identity", width=.5, fill = "#FF6666")
Facebook_plot=Facebook_plot + ggtitle("Facebook Employee status") + xlab("Employee status")+theme(axis.text.x = element_text(colour = 'black', angle = 45, size = 10,hjust = 0.5, vjust = 0.5,face = 'bold'),axis.title.x=element_blank())+
  theme(axis.text.y = element_text(colour = 'black', size = 10), axis.title.y = element_text(size = 10, hjust = 0.5, vjust = 0.2)) + 
  theme(strip.text.y = element_text(size = 10, hjust = 0.5, vjust = 0.5, face = 'bold'))+
  theme(panel.background = element_rect(fill = 'cornsilk2'))+
  theme(legend.text=element_text(size=15,face ='bold'),legend.title =element_text(size = 15,face ='bold') )  
Facebook_plot 



netflix <- atrain%>% filter(company=='netflix')
str(netflix)

Netflix_az<-sqldf('select churn_status,count(*) as COUNT  from netflix GROUP BY churn_status')
Netflix_az
Netflix_plot = ggplot(Netflix_az, aes(x=churn_status, y=COUNT)) + geom_bar(stat="identity", width=.5, fill = "#FF6666")
Netflix_plot=Netflix_plot + ggtitle("Netflix Employee status") + xlab("Employee status")+theme(axis.text.x = element_text(colour = 'black', angle = 45, size = 10,hjust = 0.5, vjust = 0.5,face = 'bold'),axis.title.x=element_blank())+
  theme(axis.text.y = element_text(colour = 'black', size = 10), axis.title.y = element_text(size = 10, hjust = 0.5, vjust = 0.2)) + 
  theme(strip.text.y = element_text(size = 10, hjust = 0.5, vjust = 0.5, face = 'bold'))+
  theme(panel.background = element_rect(fill = 'cornsilk2'))+
  theme(legend.text=element_text(size=15,face ='bold'),legend.title =element_text(size = 15,face ='bold') )  
Netflix_plot


grid.arrange(google_plot,apple_plot,Amazon_plot,Microsoft_plot,Facebook_plot,Netflix_plot, nrow = 2)
