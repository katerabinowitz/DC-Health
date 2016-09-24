from bs4 import BeautifulSoup
import urllib
import pandas as pd
r = urllib.urlopen('https://www.urgentcarelocations.com/dc/washington-dc-urgent-care').read()
soup = BeautifulSoup(r)
section = soup.find('div', {"class": "generic-block"})

uc=[]
ucList= section.find_next('ul')

for li in ucList.findAll('li'):
	liT=li.getText()
	uc.append(liT)

clinicLoc = pd.DataFrame({'uc':uc})
clinicLoc.to_csv('clinicLoc.csv')
