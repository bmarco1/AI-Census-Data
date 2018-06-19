import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd
import numpy as np

df = pd.read_csv('census_data.csv')
df['Public vs Private Work'] =df['PrivateWork'] / df['PublicWork']
df.head()

to_drop = ['CensusTract', 'Citizen', 'IncomeErr', 'IncomePerCapErr', 'Poverty', 'ChildPoverty']
df.drop(to_drop, inplace=True, axis=1)


df = df.dropna()
df.shape
df = df.groupby('State', as_index=False).sum()

df.shape

df = df.sort_values('TotalPop')
df.head()

fig, ax = plt.subplots(figsize=(14,4))
fig = sns.barplot(x=df['State'], y=df['TotalPop'])
fig.axis(ymin=0, ymax=40000000)
plt.xticks(rotation=90)
plt.show()
