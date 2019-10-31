#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Apr 29 16:41:23 2019

@author: gopi
"""

import pandas as pd
import numpy as np
import re


##GLOBAL VARIABLES

FILE_NAME="Refined_Phone_Dataset.csv"
FILE_PATH="./DATASET/" + FILE_NAME

df = pd.read_csv(FILE_PATH)
print(df.columns)
for column in df.columns:
    print(column)
    print(pd.Series(df[column]).unique()) 
    

    

