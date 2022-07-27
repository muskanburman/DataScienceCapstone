#!/usr/bin/env python
# coding: utf-8

# In[23]:


import csv
import pandas as pd
import numpy as np
from pandas import Series
from pandas import DataFrame

def read_file():
    with open("airbnb_v1.csv",'r') as file_name:
        file_read = pd.read_csv(file_name)

    return file_read


# In[24]:


read_file()


# In[25]:


def create_header():
    airbnb_df = read_file()
    new_header = airbnb_df.iloc[0] 
    airbnb_df = airbnb_df[1:] 
    airbnb_df.columns = new_header 
    return airbnb_df


# In[26]:


create_header()


# In[33]:


def drop_NaN_column():
    airbnb_df = create_header()
    airbnb_df = airbnb_df.dropna(axis=1, how='all')
    return airbnb_df


# In[34]:


drop_NaN_column()


# In[164]:


def rename_column():
    airbnb_df = drop_NaN_column()
    airbnb_df = airbnb_df.rename(columns={'[host_name]': 'host name'})
    return airbnb_df


# In[163]:


rename_column()


# In[85]:


import re
def clean_price():
    airbnb_df = rename_column()
    airbnb_df['price'] = airbnb_df['price'].map(lambda x: x.lstrip('$').rstrip())
    airbnb_df['price'] = airbnb_df['price'].str.replace(r'\D', '')                             
    return airbnb_df


# In[86]:


clean_price()


# In[113]:


def clean_latandlong():
    airbnb_df = clean_price()
    airbnb_df['latitude'] = airbnb_df['latitude'].str.replace('N', '')
    airbnb_df['longitude'] = airbnb_df['longitude'].str.replace('° W', '')
    return airbnb_df


# In[114]:


clean_latandlong()


# In[247]:


def clean_noise():
    airbnb_df = clean_latandlong()
    airbnb_df['noise.dB.'] = airbnb_df['noise.dB.'].str.replace('dB', '')
    return airbnb_df


# In[248]:


clean_noise()


# In[290]:


def clean_id():
    airbnb_df = clean_noise()
    airbnb_df['id'] = airbnb_df['id'].replace('52$$$38', '5238')
    return airbnb_df


# In[291]:


clean_id()


# In[332]:


def clean_hostid():
    airbnb_df = clean_id()
    #print(airbnb_df['host   id'].values[257])
    #airbnb_df = airbnb_df.replace({'host   id':{'2,,,118778':'2118778'}})
    airbnb_df['host   id'] = airbnb_df['host   id'].replace('2,,,118778', '2118778')
    #print(airbnb_df['host   id'].values[257])
    return airbnb_df


# In[333]:


clean_hostid()


# In[334]:


def clean_rooms():
    airbnb_df = clean_hostid()
    airbnb_df = airbnb_df.replace({'room and type':{'Privatè-Room':'Private room', 'Room Type Private':'Private room'}})
    return airbnb_df


# In[335]:


clean_rooms()


# In[336]:


def clean_nights():
    airbnb_df = clean_rooms()
    airbnb_df['minimum nights'] = airbnb_df['minimum nights'].str.replace(r'\D', '')     
    return airbnb_df


# In[337]:


clean_nights()


# In[371]:


def clean_neighbourhoods():
    airbnb_df = clean_nights()
    airbnb_df = airbnb_df.replace({'neighbourhood group':{'brklyn':'Brooklyn', 'Brooklyn - **Borough**':'Brooklyn',
                                   'Brooklyn Borough':'Brooklyn', 'B-r-0-n-x':'Bronx', 'bronx_borugh':'Bronx', 
                                   'broxn':'Bronx'}})
    return airbnb_df


# In[372]:


clean_neighbourhoods()


# In[373]:


def fillNas():
    airbnb_df = clean_neighbourhoods()
    airbnb_df["reviews per month"] = airbnb_df["reviews per month"].astype(float) 
    airbnb_df['reviews per month'].fillna(airbnb_df.groupby('room and type')['reviews per month'].transform(np.mean), inplace = True)

    return airbnb_df


# In[374]:


final = fillNas()
final.to_csv('airbnb_cleaned.csv')


# In[ ]:


u


# In[ ]:




