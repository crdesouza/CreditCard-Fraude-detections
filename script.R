############################# First we need to import the data from csv.file

data<-read.csv("creditcard.csv", sep=";",h=T)
data<-as.data.frame(data)

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
data$amount<-scale(data$amount)
data$max_value<-scale(data$max_value)


###### Split data into train and test
set.seed(2452)
data$r<-runif(nrow(data),min=0,max=1)

train<- data[data$r>=0.8,]
test<-data[data$r<0.2,]

dim(train)
dim(test)

table(train$class)

table(test$class)


######### Adjusting the logistic regression

log_mod<- glm(class ~.,train,family = binomial)

## model
summary(log_mod)

## lets evaluate somethings in model
#overdispersion
log_mod$deviance/log_mod$df.residual

### since it is lower than 1, there is no overdispersion


## compare the model deviance to the null deviance (deviance under a null model)
## since model deviance is lower, our model has a good fit


### let's do the prediction
train$pred<-predict(log_mod,newdata = train,type="response")
test$pred<-predict(log_mod,newdata = test,type="response")

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

plt <- PRTPlot(train, "pred", "class", 1,    	# Note: 1 
               plotvars = c("enrichment", "recall"),
               thresholdrange = c(0,1),                                       
               title = "Enrichment/recall vs. threshold for fraud model") 
plt
plt + geom_vline(xintercept = 0.5, color="red", linetype = 2)




ROCPlot(test,                                       
        xvar = "pred", 
        truthVar = "class", 
        truthTarget = "1", 
        title = 'Fraud filter test performance')


### let's to do a confusion matrix

confmat <- table(truth = test$class,
                       prediction = ifelse(test$pred > 0.5, 
                                           "1", "0"))
print(confmat)



##existem uma série de medidas pra avaliar na matriz de confusão

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


#especificidade e sensibilidade

#sensitivity é igual a recall
#especificidade é a taxa de true negatives e responde a pergunta: "Qual fração do que não é spam de fato foi considerado
#como não spam?

confmat_fisio[1,1] / (confmat_fisio[1,1] + confmat_fisio[1,2])

#1-especificidade é igual a false positive rate, que responde a pergunta: Qual fração do não-spam vai ser classificado
#como spam pelo classificador?







table(test$pred_class,test$class)



gdata::keep(log_mod,sure=T)
save.image(".RData")
