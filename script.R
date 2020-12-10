############################# First we need to import the data from csv.file

data<-read.csv("creditcard.csv", sep=";",h=T)

######################## To explore the data to understand better their properties


## data dimensions
dim(data)

## data structure 
str(data)

## data head
head(data)

## final rows of the data
tail(data)

## variables names (columns)
names(data)

## let's to see the frequency of the response value (class)
table(data$class)


### let's to see summary information about variables, such as mean, sd, and whether there are NAs
summary(data)


######## let's to see the variability of each variable using boxplot
## for this i will change the data format from wide to thin using the dplyr
# the final plot was made using ggplot2

### load the packages
library(dplyr)
library(tidyr)
library(ggplot2)


## change data format from wide to thin
data_thin<-data %>%
  gather(., value=value,key=var,1:8)

#build the plot
ggplot(data_thin) +
  aes(y = value) +
  geom_boxplot(fill = "#0c4c8a") +
  facet_wrap(~var,scales = "free",ncol = 4)+
  theme_bw()+
  theme(axis.text.x=element_text(colour="black",size=25))+
  theme(axis.text.y= element_text(colour="black",size = 25))+
  theme(axis.title.y= element_text(colour="black",size = 25))+
  theme(axis.title.x = element_text(colour="black",size = 25))+
  theme(strip.text.x = element_text(color="black",size=25))+
  theme(strip.background =element_rect(fill="#CCCCCC",colour="black"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



######### data management

# Let's to scale the non-binary variables (amount and max value)
data$amount<-c(scale(data$amount))
data$max_value<-c(scale(data$max_value))



##### Split data into train and test
set.seed(2452)
data$r<-runif(nrow(data),min=0,max=1)

train<- data[data$r>=0.8,]
test<-data[data$r<0.2,]

dim(train)
dim(test)

table(train$class)

table(test$class)


############ evaluate correlation between variables
corr<-cor(data[,1:7], method = c("pearson"))
corr


library(corrplot)
corrplot(corr, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

### We have correlated variables (correct_fill and charge back), bu in this case this is not a great problem, since we
# are interested mainly in the outcome, not in determining each variable importance
# I recognize correlated variables may imply a problem in estimation, but here I will desconsider this in order to only do the full process
# of build a model and deploying

######### Adjusting the logistic regression

### First I will obtain a global model with the full variables

log_mod<- glm(class ~correct_fill+cpf_died+cpf_dirty+days_last+max_value+charge_back+amount,train,family = binomial,"na.action"="na.fail")


### Let's to do a dredge to obtain the several submodels with no correlation
library(MuMIn)

dredge1=dredge(log_mod,trace=2,subset=abs(corr) < 0.6) 

dredge1

res<-summary(model.avg(dredge1, subset = delta <= 2,fit=T))
res


res$residuals

## lets evaluate somethings in global model
#overdispersion
log_mod$deviance/log_mod$df.residual

### since it is lower than 1, there is no overdispersion

## compare the model deviance to the null deviance (deviance under a null model)
## since model deviance is lower, our model has a good fit


## let's to see the select variables
res

## let's create a model with them
mx<-glm(class ~correct_fill+cpf_died+cpf_dirty+days_last+max_value+charge_back+amount,train,family = binomial,"na.action"="na.fail")
summary(mx)

## and them change the coefficients using the full model values obtained by multimodel
mx$coefficients<-res$coefmat.full[,1]
summary(mx)


### let's do the prediction
train$pred<-predict(mx,newdata = train,type="response")
test$pred<-predict(mx,newdata = test,type="response")


### let's do a double plot to see how the prediction are distributed.
## the ideal is to have "1" in the rigth side of plot
library(WVPlots)
DoubleDensityPlot(train, 
                  xvar = "pred",
                  truthVar = "class",
                  title = "Distribution of scores for fraud filter")


###### one important thing is to determine the limit of the prediction values to differentiate classes
### one option for this is to see the relationship between enrichment and recall
##3 I do this using the training data

### enrichment is the rate of precision to the average rate of positives

help("PRTPlot")
plt <- PRTPlot(train, "pred", "class", 1,    	# Note: 1 
               plotvars = c("enrichment", "recall"),
               thresholdrange = c(0,1),                                       
               title = "Enrichment/recall vs. threshold for fraud model") 
plt
plt + geom_vline(xintercept = 0.1, color="red", linetype = 2)


## base the results of image i will adopt the threshold od 0.1

### ROC plot: greater the model, better the fitting

ROCPlot(test,                                       
        xvar = "pred", 
        truthVar = "class", 
        truthTarget = "1", 
        title = 'Fraud filter test performance')


### let's to do a confusion matrix
### but it is important to remember confusion matrix is not a goo tool for situations we have unbalanced classes
# in this situations the null vs model residues comparison is enough

# but let's doing it to see the results
## here

confmat <- table(truth = test$class,
                       prediction = ifelse(test$pred > 0.1, 
                                           "1", "0"))
print(confmat)


##there are a series of measures to evaluate in the confusion matrix

# Accuracy: how much I got it right within the whole. It is the result of the relationship between the correct predictions
# and the total of predictions. The correct predictions are diagonal (true negative and true positive)

(confmat[1,1]+confmat[2,2])/sum(confmat)


#I used this measure here, but remember that accuracy is not a very good measure for unbalanced classes or
#with rare events, considering that in these situations the null model already tends to be good

# precision and recall

#precision: relationship between what was predicted correct (true positive) and what was predicted as positive
# he answers the question "if the model says it is fraud, what is the chance of it really being?"

confmat[2,2] / (confmat[2,2]+ confmat[1,2])

#recall: relationship between what was correctly predicted (TP) and what is in fact fraud (FN + TP)
# this measure answers the question "of all the fraud in my dataset, how many were classified
#correctly as such?

confmat[2,2] / (confmat[2,1]+ confmat[2,2])


#the F1 score measure is a good measure to use as a comparison metric between classifiers
#this is better and easier to look at one measurement, than two
# This measure quantifies a trade-off between precision and recall, and is defined as the harmonic average
# of precision and recall

precision <- confmat[2,2] / (confmat[2,2]+ confmat[1,2])
recall <- confmat[2,2] / (confmat[2,1]+ confmat[2,2])

(F1 <- 2 * precision * recall / (precision + recall) )

# F1 is 1 when the classifier has perfect accuracy and recall


#specificity and sensitivity

#sensitivity equals recall
#specificity is the rate of true negatives and answers the question: "What fraction of what is not spam was actually considered
# how not spam?

confmat[1,1] / (confmat[1,1] + confmat[1,2])

# 1-specificity is equal to false positive rate, which answers the question: What fraction of non-fraud will be classified
#as fraud by the classifier?


### Since we finally our model by evaluating its quality, now we will save it to use latter.



gdata::keep(mx,sure=T)
save.image(".RData")
