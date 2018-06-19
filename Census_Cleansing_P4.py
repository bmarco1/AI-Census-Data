import pandas as pd
import numpy as np


df = pd.read_csv('census_data.csv')
df['Public vs Private Work'] =df['PrivateWork'] / df['PublicWork']


to_drop = ['CensusTract', 'Citizen', 'IncomeErr', 'IncomePerCapErr', 'Poverty', 'ChildPoverty']
df.drop(to_drop, inplace=True, axis=1)
df.head()

df = df.dropna()

df = df.groupby('State', as_index=False).sum()

df.shape

df = df.sort_values('TotalPop')
df.head()

print(df)
