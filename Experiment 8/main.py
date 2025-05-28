# %% codecell
# This Python 3 environment comes with many helpful analytics libraries installed
# It is defined by the kaggle/python Docker image: https://github.com/kaggle/docker-python
# For example, here's several helpful packages to load

import numpy as np # linear algebra
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)

from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression


# Input data files are available in the read-only "../input/" directory
# For example, running this (by clicking run or pressing Shift+Enter) will list all files under the input directory

import os
for dirname, _, filenames in os.walk('/kaggle/input'):
    for filename in filenames:
        print(os.path.join(dirname, filename))

# You can write up to 20GB to the current directory (/kaggle/working/) that gets preserved as output when you create a version using "Save & Run All"
# You can also write temporary files to /kaggle/temp/, but they won't be saved outside of the current session
# %% codecell
def wrangle(filepath):
    df = pd.read_csv(filepath)

    # Removing unnecessary columns
    df.drop(columns=["Cabin", "Ticket", "Name", "Embarked"], inplace=True)
    # Change Sex from male to 1 and female to 0
    df["Sex"] = (df["Sex"].str[0]=="m").replace("m", 1).astype(int)
    # forward filling those columns which have NaN values
    df["Age"] = df["Age"].ffill()
    df["Fare"] = df["Fare"].ffill()

    return df

# %% codecell
train = wrangle("/kaggle/input/titanic/train.csv")
print(train.info())
# %% codecell
test = wrangle("/kaggle/input/titanic/test.csv")
test_ids = test["PassengerId"]
test.info()
# %% codecell
target = "Survived"
X = train.drop(columns="Survived")
y= train[target]
print("X shape: ", X.shape)
print("y shape: ", y.shape)
# %% codecell
X_train, X_val, y_train, y_val= train_test_split(X, y, test_size=0.2, random_state=42)
# %% codecell
acc_baseline = y.value_counts(normalize=True).max()
print("Baseline Accuracy", round(acc_baseline, 4))
# %% codecell
model = LogisticRegression(max_iter=1000)
model.fit(X_train, y_train)
# %% codecell
acc_train = model.score(X_train, y_train)
acc_train
# %% codecell
acc_val = model.score(X_val, y_val)
acc_val
# %% codecell
predictions = model.predict(test)
df = pd.DataFrame({"PassengerId":test_ids.values,
                  "Survived":predictions})
df.head()
# %% codecell
df.to_csv("predictions.csv", index=False)
