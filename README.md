# Prediction of USA Robberies in 1995

The objective of this report is to identify the most significant variables in order to predict, through a regression
model, the number of robberies per 100.000 habitants in the USA. The dataset that is used for this report’s
purpose is a Communities and Crime Un-normalized dataset. It consists of 100 crime instances that had been
reported from different states across the country and 147 total attributes describing a crime. Specifically, the
dataset contains 4 non-predictive attributes, 125 predictive attributes and 18 crime related attributes which
are potential dependent variables.

According to the Federal Bureau of Investigation (FBI) a violent crime is defined as an offense which
involves force or threat. The FBI’s Uniform Crime Reporting (UCR) program categorizes these offenses into
four categories: murder, forcible rape, robbery and aggravated assault. In this paper, the focus is on the violent
crime of robberies. The FBI’s UCR defines robbery as the taking or attempting to take anything of value
from the care, custody, or control of a person or people by force or threat of force or violence and/or by
putting the victim in fear.

The first hardship that increased the difficulty of the analysis was that the data was over-parameterized. Thus,
the first goal was to reduce the number of attributes that were used as input in the multiple regression model.
Moreover, all variables used in the model needed to be converted to numeric. Last but not least, the
phenomenon of multi-collinearity in which the predictor variables of the multiple regression are highly
correlated hindered the attribute selection process even more.

All in all, the question that has to be answered is the following: “**Are there any characteristics that can
predict the number of robberies per 100.000 habitants of a typical area?**”

## US Map

![image](https://github.com/AthinaSpanou/prediction-robberies-usa-1995/blob/main/R_USMAP.png)

## Dataset

The data set can be found here: 
https://archive.ics.uci.edu/ml/datasets/Communities+and+Crime+Unnormalized
