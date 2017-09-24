# Imbalanced-Class-Classification

Imbalanced classification is a supervised learning problem where one class outnumbers other class by a large proportion. This problem is faced more frequently in binary classification problems than multi-level classification problems. This imbalanced distribution of classes is quite commonly faced in business problems these days.



The standard statistical learning algorithms like Maximum Likelihood etc. fare quite well in attaining an almost universal accuracy. But such algorithms usually lead to a bugbear called Accuracy Paradox. The accuracy paradox for predictive analytics states that predictive models with a given level of accuracy may have greater predictive power than models with higher accuracy. Thus, a model with higher accuracy makes the analyst to have a false sense of success, but inferences or generalization through that model takes an unwarranted hit. 



I hope to come up with some algorithm or suggest some other way which successfully facilitates the pure classification of data with imbalanced classes.


  
# Dataset 

The Dataset used throughout this paper is the “Kaggle Credit Card Fraud Detection Dataset.” The datasets contains transactions made by credit cards in September 2013 by European cardholders. This dataset presents transactions that occurred in two days, where we have 492 frauds out of 284,807 transactions. The dataset is highly unbalanced, the positive class (frauds) account for 0.172% of all transactions.


It contains only numerical input variables which are the result of a PCA transformation. Unfortunately, due to confidentiality issues, we cannot provide the original features and more background information about the data. Features V1, V2, ... V28 are the principal components obtained with PCA, the only features which have not been transformed with PCA are 'Time' and 'Amount'. 

Feature 'Time' contains the seconds elapsed between each transaction and the first transaction in the dataset. The feature 'Amount' is the transaction Amount, this feature can be used for example-dependant cost-sensitive learning. Feature 'Class' is the response variable and it takes value 1 in case of fraud and 0 otherwise.
