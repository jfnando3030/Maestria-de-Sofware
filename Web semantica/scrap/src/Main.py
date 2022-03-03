from golem import Main as golemClass
from brodway import Main as brodwayClass
from thefuzz import fuzz
from pymongo import MongoClient

import json
import os



program1 = golemClass("https://golem.es/golem/encartel/")
dictonary_golem= program1.scrapHTML()

os.chdir('../')
pwd= os.getcwd()
archivo= pwd+"/data/golem.json"
with open(archivo, 'w', encoding='utf8') as f:
    json.dump(dictonary_golem, f, ensure_ascii=False)

program2 = brodwayClass("http://cbroadway.net/estrenos.html")
dictonary_brodway= program2.scrapHTML()



archivo= pwd+"/data/brodway.json"
with open(archivo, 'w', encoding='utf8') as f:
    json.dump(dictonary_brodway, f, ensure_ascii=False)



for index1 in range(len(dictonary_golem)):
    for key1, value1 in dictonary_golem[index1].items():
        if(key1 == "title"):
            for index2 in range(len(dictonary_brodway)):
                for key2, value2 in dictonary_brodway[index2].items():
                    if(key2 == "title"):
                        similitud= fuzz.partial_ratio(value1, value2)
                        if(similitud >= 85):
                            print("similitud: " + str(similitud) + " title1: " + value1 + " title2: " + value2)
                            if(dictonary_golem[index1]["imagen"] == ""):
                                dictonary_golem[index1]["imagen"] = dictonary_brodway[index2]["imagen"]
                            if(dictonary_golem[index1]["sesiones"] == ""):
                                dictonary_golem[index1]["sesiones"] = dictonary_brodway[index2]["sesiones"]
                            if(dictonary_golem[index1]["anio"] == ""):
                                dictonary_golem[index1]["anio"] = dictonary_brodway[index2]["anio"]
                            if(dictonary_golem[index1]["guion"] == ""):
                                dictonary_golem[index1]["guion"] = dictonary_brodway[index2]["guion"]
                            if(dictonary_golem[index1]["sinopsis"] == ""):
                                dictonary_golem[index1]["sinopsis"] = dictonary_brodway[index2]["sinopsis"]
                            if(dictonary_golem[index1]["director"] == ""):
                                dictonary_golem[index1]["director"] = dictonary_brodway[index2]["director"]
                            if(dictonary_golem[index1]["duracion"] == ""):
                                dictonary_golem[index1]["duracion"] = dictonary_brodway[index2]["duracion"]
                            if(dictonary_golem[index1]["nacionalidad"] == ""):
                                dictonary_golem[index1]["nacionalidad"] = dictonary_brodway[index2]["nacionalidad"]
                            if(dictonary_golem[index1]["reparto"] == ""):
                                dictonary_golem[index1]["reparto"] = dictonary_brodway[index2]["reparto"]
                            if(dictonary_golem[index1]["title_original"] == ""):
                                dictonary_golem[index1]["title_original"] = dictonary_brodway[index2]["title_original"]
                            if(dictonary_golem[index1]["title"] == ""):
                                dictonary_golem[index1]["title"] = dictonary_brodway[index2]["title"]
                        else:
                            dictonary_golem.append(dictonary_brodway[index2])


archivo= pwd+"/data/estrenos.json"
with open(archivo, 'w', encoding='utf8') as f:
    json.dump(dictonary_golem, f, ensure_ascii=False)

client = MongoClient('localhost', 27017)
db = client['estrenos']
collection = db['golem_brodway']
collection.insert_many(dictonary_golem)
                            



                        





