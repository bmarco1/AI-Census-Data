import pandas as pd
import numpy as np

df = pd.read_csv('census_data.csv')
df['Public vs Private Work'] =df['PrivateWork'] / df['PublicWork']
df.head()

to_drop = ['CensusTract', 'Citizen', 'IncomeErr', 'IncomePerCapErr', 'Poverty', 'ChildPoverty']
df.drop(to_drop, inplace=True, axis=1)


df = df.dropna()
df.shape


print(df.columns)
