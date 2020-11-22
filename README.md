# CreditCard-Fraude-detections
This is a project based on the project provided By Luis Otávio of the youtube channel Luis Otávio.Pro (https://www.youtube.com/channel/UCC3Vw7R-fKS-uYXYRhJ983A).

The database if from kaggle (https://www.kaggle.com/mlg-ulb/creditcardfraud) and consists of transactions made by credit cards in September 2013 by european cardholders.
This dataset presents transactions that occurred in two days, where we have 492 frauds out of 284,807 transactions.
The dataset is highly unbalanced, the positive class (frauds) account for 0.172% of all transactions.

This data were modified by Luis Otavio and it is not equal to eh original database.
Here I follow the script and aplication from Luis Otavio and made some changes according to my preferences in analysing. In the Luis Otavio channel he presents a video-aula (https://www.youtube.com/watch?v=rSnPkYnaH4E&feature=youtu.be&ab_channel=LuisOtavio) about this example.

  
The data has 8 columms:
#correct_fill: value representing whether the all information were filled correctly
#cpf_died: value representing whether the client used a cpf (brazilian Id number) of a died person
#cpf_dirty: value representing whether the client used a cpf that has some pendencies related for not paying bills.
#max_value: the maxmimum value the person has ever paid in the credit card.
#days_last: the days since the person made has last buying using the credit card.
#charge_back: represent the number of times person has declared the occurrence of charge back in the credit card, that is the situation when you see a buy you not did. 
#amount: the amount of cash spend in this transaction.
#class: the response variable (1 = fraud; 0 = not fraude). 


*** It is important to highlight that the first three values are not in real scales due to data confidence and are presented as PCA axes. This is the original data format.
