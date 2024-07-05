#!/usr/bin/env python
# coding: utf-8

# In[4]:


import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns


# In[5]:


data= pd.read_csv('final_dataset_phase2.csv')


# In[6]:


data


# In[7]:


data[data['Outcome'] == 1].describe()


# In[8]:


data[data['Outcome'] == 0].describe()


# In[9]:


from sklearn.svm import SVC
from pandas import set_option
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import train_test_split
from sklearn.model_selection import KFold
from sklearn.model_selection import StratifiedKFold
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import GridSearchCV
from sklearn.metrics import classification_report
from sklearn.metrics import confusion_matrix
from sklearn.metrics import accuracy_score
from sklearn.pipeline import Pipeline
from sklearn.model_selection import RepeatedStratifiedKFold
from sklearn.metrics import classification_report,confusion_matrix
from sklearn.metrics import f1_score, precision_score, recall_score
from sklearn.model_selection import GridSearchCV


# In[10]:


from sklearn.model_selection import train_test_split
X = data.drop('Outcome', axis=1)
y = data['Outcome']
X_train,X_test,Y_train,Y_test = train_test_split(X,y,test_size=0.2,random_state=42)


# ##Support Vector Machine

# In[11]:


svm = SVC()
svm.fit(X_train,Y_train)

svm_acc= accuracy_score(Y_test,svm.predict(X_test))


print("Train Set Accuracy:"+str(accuracy_score(Y_train,svm.predict(X_train))*100))
print("Test Set Accuracy:"+str(accuracy_score(Y_test,svm.predict(X_test))*100))


# Now we will optimize the hyperparameters using Grid Search to improve the accuracy scores

# In[12]:


model = SVC()
kernel = ['poly', 'rbf', 'sigmoid']
C = [50, 10, 1.0, 0.1, 0.01]
gamma = ['scale']


# In[13]:


# define grid search
grid = dict(kernel=kernel,C=C,gamma=gamma)
cv = RepeatedStratifiedKFold(n_splits=10, n_repeats=3, random_state=1)
grid_search = GridSearchCV(estimator=model, param_grid=grid, n_jobs=-1, cv=cv, scoring='f1',error_score=0)


# In[14]:


grid_result = grid_search.fit(X, y)


# In[15]:


svm_pred=grid_result.predict(X_test)


# In[16]:


svm_acc= accuracy_score(Y_test,grid_result.predict(X_test))


print("Train Set Accuracy:"+str(accuracy_score(Y_train,grid_result.predict(X_train))*100))
print("Test Set Accuracy:"+str(accuracy_score(Y_test,grid_result.predict(X_test))*100))


# In[17]:


print("Classification Report is:\n",classification_report(Y_test,svm_pred))
sns.heatmap(confusion_matrix(Y_test,svm_pred))


# Note that the accuracy scores significantly improved after optimizing the hyperparamters for SVC

# In[1]:


import pickle
import os
import sys
import pandas as pd
from sklearn.metrics import accuracy_score
from sklearn.preprocessing import MinMaxScaler


#For training
def train():
    data= pd.read_csv('final_dataset_phase2.csv')
    X = data[['Pregnancies','Glucose','SkinThickness','Insulin','Age','BMIxThickness','BMIxAge']]
    Y = data[['Outcome']]

    #train test split
    from sklearn.model_selection import train_test_split
    #X = data.drop('Outcome', axis=1)
    #y = data['Outcome']
    X_train,X_test,Y_train,Y_test = train_test_split(X,Y,test_size=0.2,random_state=42)

    from sklearn.svm import SVC
    model = SVC()
    kernel = ['poly', 'rbf', 'sigmoid']
    C = [50, 10, 1.0, 0.1, 0.01]
    gamma = ['scale']
    # define grid search
    grid = dict(kernel=kernel,C=C,gamma=gamma)
    cv = RepeatedStratifiedKFold(n_splits=10, n_repeats=3, random_state=1)
    grid_search = GridSearchCV(estimator=model, param_grid=grid, n_jobs=-1, cv=cv, scoring='f1',error_score=0)
    svc = grid_search.fit(X, y)

    #model = SVC(kernel='linear')
    #svc=model.fit(X_train,Y_train)

    #Save Model As Pickle File
    with open('svc.pkl','wb') as m:
        pickle.dump(svc,m)
    test(X_test,Y_test)

#Test accuracy of the model
def test(X_test,Y_test):
    with open('svc.pkl','rb') as mod:
        p=pickle.load(mod)

    pre=p.predict(X_test)
    print (accuracy_score(Y_test,pre)) #Prints the accuracy of the model


def find_data_file(filename):
    if getattr(sys, "frozen", False):
        # The application is frozen.
        datadir = os.path.dirname(sys.executable)
    else:
        # The application is not frozen.
        datadir = os.path.dirname(__file__)

    return os.path.join(datadir, filename)


def check_input(data) -> int:
    df = pd.DataFrame(data=data, index=[0])
    columns = ['Pregnancies', 'Glucose', 'SkinThickness', 'Insulin', 'Age', 'BMIxThickness', 'BMIxAge']

    scaler = MinMaxScaler()  
    df_normalized = scaler.fit_transform(df[columns])
    with open(find_data_file('svc.pkl'), 'rb') as model:
        p = pickle.load(model)
    op = p.predict(df_normalized)
    return op[0]

if __name__=='__main__':
    train()

