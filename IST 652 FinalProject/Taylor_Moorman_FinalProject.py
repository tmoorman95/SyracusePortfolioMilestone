# -*- coding: utf-8 -*-
"""

@author: tmoorman

IST 652 Final Projest
"""
#import relevant packages and set the working directory
import os
os.chdir("C:/Users/tmoorman/OneDrive - BeyondTrust Software Inc/Documents/Not Work Related/school/Spring 2019/IST 652/FinalProject")
import numpy as np
import pandas as p
import sklearn

#read in data sets
fights = p.read_excel('fights.xlsx')
fighters = p.read_excel('fighters.xlsx')
fightmets = p.read_excel('fightmetric.xlsx')

#clean data sets to relevant dates and choose relevant columns for analysis
fights = fights[fights.event_date >= '2003-1-1']
fightscol = ['f1fid', 'f2fid', 'f1result', 'f2result', 'ref', 'event_date']

fights = fights[fightscol]

fighterscol = ['fid', 'birth_date', 'height', 'weight', 'association', 'class', 'country' ]

fighters = fighters[fighterscol]

fightmetscol = ['fid', 'Wins', 'Losses', 'Reach', 'Stance', 'SLpM', 'Str. Acc', 'SApM', 'Str. Def', 'TD Avg', 'TD Acc', 'TD Def', 'Sub. Avg']

fightmets = fightmets[fightmetscol]

fightmets = fightmets.dropna(subset=['fid'])

#Merge fighter datasets together
fNEW = fighters.merge(fightmets, on=['fid'])

#seperate wins and losses
fightlosscol = ['f2fid', 'f2result','ref', 'event_date']
fightwincol = ['f1fid', 'f1result','ref', 'event_date']

win = fights[fightwincol]
loss = fights[fightlosscol]

#rename columns in win and loss datasets
win = win.rename(index=str, columns = {'f1fid' : 'fid', 'f1result' : 'result'})
loss = loss.rename(index=str, columns = {'f2fid' : 'fid', 'f2result' : 'result'})

#append win and loss datasets to create new fight dataframe with wins and losses to ultimately create target variable
totalfights = win.append(loss, ignore_index = True)

totalfights['y'] = np.where(totalfights['result']== 'win', 1, 0)

#merge all fight data with fighter data to get data for every fighter in every fight
finalset = totalfights.merge(fNEW, on=['fid'])

#getting rid of columns that are unwanted, creating binaries for a factored feature
finalset = finalset.drop('fid', axis=1)
finalset = finalset.drop('result', axis=1)
finalset = finalset.drop('association', axis=1)
finalset = finalset.drop('class', axis=1)
finalset['stancebin'] = np.where(finalset['Stance']== 'Orthodox', 1, 0)
finalset = finalset.drop('Stance', axis=1)

#Creating 'age' variable for the age of fighter at time of fight
finalset['age'] = finalset['event_date'] - finalset['birth_date']
finalset['age'] = finalset['age'].dt.days
finalset = finalset.drop('event_date', axis=1)
finalset = finalset.drop('birth_date', axis=1)

#dropping any remaining rows with NANs
finalset = finalset.dropna()
###############################################################################################################################3

##Encoding country and ref fields
from sklearn.preprocessing import LabelEncoder
from sklearn.preprocessing import OneHotEncoder
label_encoder = LabelEncoder()
country_encoded = label_encoder.fit_transform(finalset['country'])
ref_encoded = label_encoder.fit_transform(finalset['ref'])
print(country_encoded)

finalset['countrycode'] = country_encoded
finalset['refcode'] = ref_encoded

finalset = finalset.drop('country', axis=1)
finalset = finalset.drop('ref', axis=1)
#making certain datatypes were correct
finalset.dtypes
#If writing to a XLSX document is needed
#writer = p.ExcelWriter('finalset.xlsx', engine='xlsxwriter')
#finalset.to_excel(writer,sheet_name='Sheet1')
#writer.save()


#########################################

x = finalset.drop('y', axis=1)
y = finalset['y']
from sklearn.model_selection import train_test_split

X_train, X_test, y_train, y_test = train_test_split(x, y, test_size=0.03, random_state=0)  

from sklearn.metrics import classification_report, confusion_matrix, accuracy_score
from sklearn.ensemble import RandomForestClassifier
regressor = RandomForestClassifier(n_estimators=3000, random_state=0)
regressor.fit(X_train, y_train)  
y_pred = regressor.predict(X_test) 

print("Confusion Matrix: ", confusion_matrix(y_test,y_pred))  
print("Metrics: ", classification_report(y_test,y_pred))  
print("Model Accuracy: ", accuracy_score(y_test, y_pred))  

feature_imp = p.Series(regressor.feature_importances_,index=X_train.columns).sort_values(ascending=False)

############################################## VISUALIZATIONS
import matplotlib.pyplot as plt
import seaborn as sns

# Creating a bar plot of the classifier
sns.barplot(x=feature_imp, y=feature_imp.index)
plt.xlabel('Feature Importance Score')
plt.ylabel('Features')
plt.title("Important Features")
plt.legend()
plt.show()

#Creating plots of most important features by wins and losses
#Ages by winning
sns.barplot(x=round((finalset['age']/365)), y=finalset['y'])
plt.xlabel('Age at time of Fight')
plt.ylabel('Win/Loss')
plt.title("Win/Loss by Age")
plt.legend()
plt.show()

#Stance
sns.barplot(x=finalset['stancebin'], y=finalset['y'])
plt.xlabel('Stance 1= right handed, 0 = lefthanded')
plt.ylabel('Win/Loss')
plt.title("Win/Loss by Stance")
plt.legend()
plt.show()

#Wins
sns.barplot(x=finalset['Wins'], y=finalset['y'])
plt.xlabel('Wins on Record')
plt.ylabel('Win/Loss')
plt.title("Win/Loss by Record")
plt.legend()
plt.show()


#Histogram of Ages
n, bins, patches = plt.hist(x=(finalset['age']/365), bins='auto', color='#0504aa',
                            alpha=0.7, rwidth=0.85)
plt.xlabel('Ages')
plt.ylabel('Frequency')
plt.title('Age Histogram')


#Tables

bindf = finalset[['y']]
bindf['binheight'] = p.cut(finalset['height'], 5)
bindf['binweight'] = p.cut(finalset['weight'], 5)
bindf['binwins'] = p.cut(finalset['Wins'], 5)
bindf['binreach'] = p.cut(finalset['Reach'], 5)
bindf['binstance'] = finalset['stancebin']
bindf['binage'] = p.cut(round(finalset['age']/365), 5)

bindf.groupby(['binage'])[['y']].count()/5391
bindf.groupby(['binweight'])[['y']].count()/5391
bindf.groupby(['binwins'])[['y']].count()/5391
bindf.groupby(['binreach'])[['y']].count()/5391
bindf.groupby(['binstance'])[['y']].sum()
bindf.groupby(['y'])[['binstance']].count()

finalset.groupby(['stancebin']).count()
719/1320
2069/4071



corr = finalset.corr()
ax = sns.heatmap(
    corr, 
    vmin=-1, vmax=1, center=0,
    cmap=sns.diverging_palette(20, 220, n=200),
    square=True
)
ax.set_xticklabels(
    ax.get_xticklabels(),
    rotation=45,
    horizontalalignment='right'
);




