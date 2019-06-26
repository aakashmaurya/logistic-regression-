################### logistic regression case study################


sessionInfo()  
getwd()
setwd("F:\\r\\logistic")

### packages required 

install.packages('dplyr',dependencies = T)
install.packages("psych", dependencies = TRUE)


# loading required packages

require(sqldf)
require(dplyr)
require(psych)
require(tables)


# importing Proactive Attrition Management-Logistic Regression Case Study CSV file

givendata<-read.csv("Proactive Attrition Management-Logistic Regression Case Study.csv")


# ___ decriptive statistics_______

mystats=function(x){
  if(class(x)=="numeric"){
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    mean<-mean(x,na.rm=T)
    std<-sd(x,na.rm=T)
    var<-var(x,na.rm=T)
    min<-min(x,na.rm=T)
    p1<-quantile(x,0.01,na.rm=T)
    p5<-quantile(x,0.05,na.rm=T)
    p10<-quantile(x,0.1,na.rm=T)
    q1<-quantile(x,0.25,na.rm=T)
    q2<-quantile(x,0.5,na.rm=T)
    q3<-quantile(x,0.75,na.rm=T)
    p90<-quantile(x,0.9,na.rm=T)
    p95<-quantile(x,0.95,na.rm=T)
    p99<-quantile(x,0.99,na.rm=T)
    max<-max(x,na.rm=T)
    UC1=mean(x,na.rm=T)+3*sd(x,na.rm=T)
    LC1=mean(x,na.rm=T)-3*sd(x,na.rm=T)
    UC2=quantile(x,0.99,na.rm=T)
    LC2=quantile(x,0.01,na.rm=T)
    
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,mean=mean,std=std,var=var,min=min,p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max,UC1=UC1,LC1=LC1,UC2=UC2,LC2=LC2))
  }
  else{
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    fre<-table(x)
    prop<-prop.table(table(x))
    
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,freq=fre,proportion=prop))
  }
}

num_var= sapply(givendata,is.numeric)
Other_var= !sapply(givendata,is.numeric)

num_diag_stats<-t(data.frame(apply(givendata[num_var], 2, FUN=mystats)))
othr_diag_stats<-data.frame(t(apply(givendata[Other_var], 2, FUN=mystats)))

View(num_diag_stats)
View(othr_diag_stats)
#missing value imputation ##

givendata$AGE1[is.na(givendata$AGE1)] <- 24 
givendata$AGE2[is.na(givendata$AGE2)] <- 24
givendata$PHONES[is.na(givendata$PHONES)] <- 0 
givendata$MODELS[is.na(givendata$MODELS)] <- 0 
givendata$EQPDAYS[is.na(givendata$EQPDAYS)] <- 0
givendata$REVENUE[is.na(givendata$REVENUE)] <- 191.584800836578
givendata$MOU[is.na(givendata$MOU)] <-2116.131170205
givendata$RECCHRGE[is.na(givendata$RECCHRGE)] <- 118.621800685609
givendata$DIRECTAS[is.na(givendata$DIRECTAS)] <- 7.48824548187283
givendata$OVERAGE[is.na(givendata$OVERAGE)] <- 329.136669520041 
givendata$ROAM[is.na(givendata$ROAM)] <- 28.4651146572613
givendata$CHANGEM[is.na(givendata$CHANGEM)] <- 755.09648501527
givendata$CHANGER[is.na(givendata$CHANGER)] <- 115.106160506814

####outlier treatment #####

givendata$ REVENUE  [givendata$ REVENUE >235.8284138]<-235.8284138
givendata$ MOU  [givendata$ MOU >2646.26543]<-2646.26543
givendata$ RECCHRGE  [givendata$ RECCHRGEed >142.5369036]<-142.5369036
givendata$ DIRECTAS  [givendata$	 DIRECTAS >9.6860603]<-9.6860603
givendata$ OVERAGE  [givendata$	OVERAGE >425.4837726]<-425.4837726
givendata$		ROAM	 [givendata$	ROAM	>	37.546311] <-	37.546311
givendata$ CHANGEM	 [givendata$	CHANGEM	>	1010.410801		] <-	1010.410801
givendata$ CHANGER	 [givendata$	CHANGER	>	153.876856		] <-	153.876856
givendata$ 	DROPVCE [givendata$	DROPVCE	>	42.0346255		] <-	42.0346255
givendata$ BLCKVCE [givendata$	BLCKVCE	>	46.7509856			] <-	46.7509856
givendata$ UNANSVCE [givendata$	UNANSVCE	>	183.9728302			] <-	183.9728302
givendata$ CUSTCARE [givendata$	CUSTCARE	>	22.5090096			] <-	22.5090096
givendata$ THREEWAY [givendata$	THREEWAY	>	4.946347			] <-	4.946347
givendata$ 	MOUREC [givendata$	MOUREC	>	780.1581898		] <-	780.1581898
givendata$ OUTCALLS [givendata$	OUTCALLS	>	165.9866005			] <-	165.9866005
givendata$ INCALLS [givendata$	INCALLS	>	74.2531438			] <-	74.2531438
givendata$ PEAKVCE [givendata$	PEAKVCE	>	510.2404537			] <-	510.2404537
givendata$ OPEAKVCE [givendata$	OPEAKVCE	>	441.1343809			] <-	441.1343809
givendata$ DROPBLK	 [givendata$	DROPBLK	>	71.9921546		] <-	71.9921546
givendata$ CALLFWDV [givendata$	CALLFWDV	>	2.260606			] <-	2.260606
givendata$ CALLWAIT [givendata$	CALLWAIT	>	24.0779135			] <-	24.07791
givendata$ MONTHS [givendata$	MONTHS	>	57.9011009			] <-	57.9011009
givendata$ 	UNIQSUBS [givendata$	UNIQSUBS	>	6.0566469		] <-	6.0566469
givendata$ 	ACTVSUBS	 [givendata$	ACTVSUBS	>	3.9918517	] <-	3.9918517
givendata$ 	PHONES [givendata$	PHONES	>	7.1530982		] <-	7.1530982
givendata$ 	MODELS [givendata$	MODELS	>	5.194913		] <-	5.194913



num_var= sapply(givendata,is.numeric)
Other_var= !sapply(givendata,is.numeric)

num_diag_stats<-t(data.frame(apply(givendata[num_var], 2, FUN=mystats)))
othr_diag_stats<-data.frame(t(apply(givendata[Other_var], 2, FUN=mystats)))

View(num_diag_stats)
View(othr_diag_stats)


########################## factor analysis ###########################3
reqdata<-select_if(givendata, is.numeric)
reqdata<-reqdata[ , -which(names(reqdata) %in% c("CHURNDEP"))]
## FACTOR ANALYSIS 
                               
 corrm<-cor(reqdata)  ### CORRELATION MATRIX
View(corrm)
corrm<-cor.smooth(corrm)

### DECIDING NUMBER OF FACTORS USING SCREE PLOT & KAISER TEST(NUMBER OF EIGEN VALUES OVER 1)

scree(corrm, factors=T, pc=T, main="scree plot", hline=NULL, add=FALSE) ### SCREE PLOT

eigen(corrm)$values                                                     ### EIGEN VALUES


eigen_values <- mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum_eigen=cumsum(eigen.corrm..values)
                       , pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values))  # CALCULATING VARIANCE, CUMULATIVE VARIANCE etc... 

write.csv(eigen_values, "F:\\r\\logistic\\EIGENVALUES.CSV")

FA<-fa(r=corrm, 27, rotate="varimax",fm="ml")               ### CONDUCTING FACTOR ANALYSIS
print(FA)                                                    ### PRINT THE RESULTS
FA_SORT<-fa.sort(FA)                                         ### SORTING THE LOADINGS
ls(FA_SORT)                                                  ### LISTING OUT THE OBJECTS
FA_SORT$loadings

Loadings<-data.frame(FA_SORT$loadings[1:ncol(reqdata),]) ### CAPTURING ONLY LOADINGS INTO DATA FRAME

write.csv(Loadings, "F:\\r\\logistic\\loading.csv") ### SAVING THE FILE


#Splitting data into Training, Validaton and Testing Dataset
training<-givendata[!is.na(givendata$CHURNDEP),]
testing<-givendata[is.na(givendata$CHURNDEP),]


#Building Models for training dataset

fit<-glm(CHURNDEP~INCALLS+MOU+REVENUE+OVERAGE+ROAM+MONTHS+EQPDAYS+RETCALLS+AGE1+CHANGEM+NEWCELLY
         +MAILORD+CREDITDE+CHANGER+DROPBLK+PHONES+ACTVSUBS,data = training,
         family = binomial(logit))

#Output of Logistic Regression
summary(fit)
ls(fit)
fit$model

coeff<-fit$coef #Coefficients of model
write.csv(coeff, "coeff.csv")

#Checking for concordance 
source("Concordance.R")
Concordance(fit)  


#Stepwise regression
step1=step(fit)

#Final Model
fit2<-glm(CHURNDEP~INCALLS+MOU+REVENUE+OVERAGE+ROAM+MONTHS+EQPDAYS+RETCALLS+AGE1+CHANGEM+NEWCELLY
          +MAILORD+CREDITDE+CHANGER+DROPBLK+PHONES,data = training,
          family = binomial(logit))
summary(fit2)
source("Concordance.R")
Concordance(fit2)



################################VALIDATION ##############################
#Decile Scoring for 
##Training dataset
train1<- cbind(training, Prob=predict(fit2, type="response")) 
View(train1)

##Creating Deciles
decLocations <- quantile(train1$Prob, probs = seq(0.1,0.9,by=0.1))
train1$decile <- findInterval(train1$Prob,c(-Inf,decLocations, Inf))
View(train1)




training_DA <- sqldf("select decile, count(decile) as counts,min(Prob) as min_prob,   
              max(Prob) as max_prob ,sum(CHURNDEP) as churn_cnt ,
                count(decile)-sum(CHURNDEP) as non_churn_cnt
               from train1
               group by decile
               order by decile desc")

write.csv(training_DA,"fit_train_DA1.csv",row.names = F)
#Decile Analysis Reports

fit_train_DA <- sqldf("select decile, min(Prob) as Min_prob
                      , max(Prob) as max_prob
                      , sum(default) as default_Count
                      , (count(decile)-sum(default)) as Non_default_Count 
                      from train1
                      group by decile
                      order by decile desc")

write.csv(fit_train_DA,"fit_train_DA1.csv",row.names = F)

##Testing dataset
test1<- cbind(testing, Prob=predict(fit2,testing, type="response")) 
View(test1)

##Creating Deciles
decLocations <- quantile(test1$Prob, probs = seq(0.1,0.9,by=0.1))
test1$decile <- findInterval(test1$Prob,c(-Inf,decLocations, Inf))
test1$newchurn<-ifelse(test1$Prob>0.40, 1,0)
View(test1)
#Decile Analysis Reports

fit_test_DA <- sqldf("select decile, count(decile) as count, min(Prob) as Min_prob
                     , max(Prob) as max_prob
                      from test1
                     group by decile
                     order by decile desc")
View(fit_test_DA)
write.csv(fit_test_DA,"fit_test_DA1.csv",row.names = F)


#################### end of case study####################