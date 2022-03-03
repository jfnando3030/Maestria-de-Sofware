
# -*- coding: utf-8 -*-
import bs4
import requests 


class Main:
    def __init__(self, url):
        self.url= url
    
    def scrapHTML(self):
        pageHTML = requests.get(self.url).text
        object_soup = bs4.BeautifulSoup(pageHTML, "lxml")
        pelis = object_soup.find_all('a', attrs={"class": "m5 txtNegXL"})
        myMovies = []
        for movie in pelis:
            v = movie.get('href').strip()
            if(v == "/golem/pelicula/LOMASDESEADODELMENDIFILM2021"):
                print("da error")
            else:
                if(v == "/golem/pelicula/DArtacanylostresMosqueperros"):
                    print("da error")
                else:
                    myMovies.append(self.scrapMovie(v))
            
        return myMovies

    def scrapMovie(self, url):
        url= "https://golem.es" + url 
        pages=requests.get(url)
        pages.encoding= "utf-8"
        object_soup= bs4.BeautifulSoup(pages.text, "lxml")
        title_original=object_soup.find_all('em', attrs={"class": "txtNegXXXL"})[1].text.strip()
        print(title_original)
        imagen="https://golem.es/"+object_soup.find('img', attrs={"width":"250"})['src']
        sinopsis=object_soup.find('td', attrs={"class": "txtNegLJust"}).text.strip()
        fichaTecnica= self.getFichaTecnica(imagen,sinopsis,title_original, object_soup.find('td', attrs={"class": "txtNegL"}))
        return fichaTecnica

    def getFichaTecnica(self,imagen,sinopsis,title_original, data):
        sinopsis = sinopsis.replace("\r", "")
        sinopsis = sinopsis.replace("\n", "")
        sinopsis = sinopsis.replace("-------------------", "")
        rows= data.find_all('tr')
        title=title_original.lower()
        diccionario={'imagen':imagen,'sesiones': '','anio':'','guion':'','title_original':'', 'title': title,'sinopsis':sinopsis, 'director': '', 'duracion': '', 'nacionalidad': '', 'reparto':''}
        rows.pop(0)
        for row in rows:
            if "Título original" in row.text:
                titulo= row.find_all('td')[1].text.strip().lower()
                diccionario['title_original'] = titulo.lower()
            if "Dirigida por" in row.text:
                direccion=row.find_all('td')[1].text.strip()
                diccionario['director'] =direccion
            if "Duración" in row.text:
                duracion=row.find_all('td')[1].text.strip()
                diccionario['duracion'] =duracion
            if "Nacionalidad" in row.text:
                nacionalidad=row.find_all('td')[1].text.strip()
                diccionario['nacionalidad'] =nacionalidad
            if "Ficha Artística:" in row.text:
                reparto = row.find_next('td', attrs={"class": "txtLectura"}).text
                reparto = reparto.replace("\r", " ")
                reparto = reparto.replace("\n", " ")
                reparto = reparto.replace("\t", " ")
                reparto = reparto.strip()
                reparto = reparto.split(",")
                diccionario['reparto'] =reparto
        return diccionario


            