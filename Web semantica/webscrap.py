import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

from urllib.request import urlopen
from bs4 import BeautifulSoup

url = "https://golem.es/"
html = urlopen(url)

soup = BeautifulSoup(html, 'lxml')
type(soup)

# Get the title
title = soup.title
print(title)