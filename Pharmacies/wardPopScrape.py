import pandas as pd
import numpy as np
from bs4 import BeautifulSoup
import requests 


wardID=range(1,9)

pop10=[]


for num in wardID:
    r = requests.get('http://www.neighborhoodinfodc.org/wards/Nbr_prof_wrd' + str(num) + '.html')
    b = BeautifulSoup(r.text)
    pop10.append(b('table')[2].find_all('tr')[6].find_all('td')[1].text)  

wardPop = pd.DataFrame({'ward':wardID, 'pop10':pop10})

wardPop.to_csv('wardPop.csv')