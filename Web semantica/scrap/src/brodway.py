
# -*- coding: utf-8 -*-
import bs4
import requests 



class Main:
    def __init__(self, url):
        self.url= url
    
    def scrapHTML(self):
        pageHTML = requests.get(self.url).text
        object_soup = bs4.BeautifulSoup(pageHTML, "lxml")
        pelis = object_soup.find_all('a', attrs={"class": "imCssLink inline-block"})
        myMovies = []
        for movie in pelis:
            v = movie.get('href').strip()
            myMovies.append(self.scrapMovie(v))
        return myMovies

    def scrapMovie(self, url):
        url= "http://cbroadway.net/" + url 
        pages=requests.get(url)
        pages.encoding= "utf-8"
        diccionario={'imagen':'','sesiones': '','anio':'','guion':'','title_original': '','title':'','sinopsis':'','director':'', 'duracion':'', 'nacionalidad':'', 'reparto':''}
        object_soup= bs4.BeautifulSoup(pages.text, "lxml")
        title= object_soup.find('span', attrs={"class": "fs40lh1-5 ff1"}).text.lower()
        diccionario['title'] = title
        print("brodway: " + title)
        sessiones= object_soup.find_all('span', attrs={"class": "fs20lh1-5 ff1"})
        try:
            imagen="http://cbroadway.net/"+object_soup.find('img', attrs={"class": "image-1"})['src'].strip()
        except:
            try:
                imagen="http://cbroadway.net/"+object_soup.find('img', attrs={"class": "image-2"})['src'].strip()
            except:
                imagen="http://cbroadway.net/"+object_soup.find('img', attrs={"class": "image-3"})['src'].strip()
        diccionario['imagen'] = imagen
        nacionalidad_anio_duracion_director_guion_reparto_sinopsis= object_soup.find_all('span', attrs={"class": "fs16lh1-5 ff1"})
        for session in sessiones:
            if "Sesiones:" in session.text:
                session_response = session.text.replace("Sesiones:", "").strip()
                diccionario['sesiones'] = session_response
            if "Estreno" in session.text:
                estreno = session.text.replace("Estreno", "").strip()
                diccionario['sesiones'] = estreno
            if "Sesiones:" in session.text or "Estreno" in session.text:
                try:
                    title_original= session.find_previous('span', attrs={"class": "fs20lh1-5 ff1"}).text.lower()
                except:
                    title_original=""

        for record in nacionalidad_anio_duracion_director_guion_reparto_sinopsis:
            if "Sinópsis:" in record.text:
                sinopsis = record.find_next('span', attrs={"class": "fs16lh1-5 ff1"}).text
                sinopsis = sinopsis.replace("\r", " ")
                sinopsis = sinopsis.replace("\n", " ")
                sinopsis = sinopsis.replace("\t", " ")
                sinopsis= sinopsis.strip()
                diccionario['sinopsis'] = sinopsis
            if "País:" in record.text:
                pais = record.text.replace("País:", "").strip()
                diccionario['nacionalidad'] = pais
            if "Año:" in record.text:
                anio = record.text.replace("Año:", "").strip()
                diccionario['anio'] = anio
            if "Duración:" in record.text:
                duracion = record.text.replace("Duración:", "").strip()
                duracion = duracion.replace(".", "").strip()
                diccionario['duracion'] = duracion
            if "Director:" in record.text:
                split= record.text.split("Guión:")
                director= split[0]
                director = director.replace("Director:", "")
                director = director.replace("\r", " ")
                director = director.replace("\n", " ")
                director = director.replace("\t", " ")
                guion=split[1]
                guion = guion.replace("\r", " ")
                guion = guion.replace("\n", " ")
                guion = guion.replace("\t", " ")
                guion= guion.strip()
                guion = guion.split(",")
                diccionario['director'] = director.strip()
                diccionario['guion'] = guion
            if "Repato:" in record.text:
                reparto = record.text.replace("Repato:", "").strip()
                reparto = reparto.replace(".", "").strip()
                reparto= reparto.strip()
                reparto = reparto.replace("\r", " ")
                reparto = reparto.replace("\n", " ")
                reparto = reparto.replace("\t", " ")
                reparto = reparto.split(",")
                diccionario['reparto'] = reparto

        return diccionario


            