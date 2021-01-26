#!/usr/bin/env python
# coding: utf-8

# In[3]:


import requests
import json
import base64
import pandas as pd
import matplotlib as mt
from random import sample

#Requesting access to the spotify web API
client_id = '16fd6188f83242288e8af5e299bc66dd'
client_secret = '6abc3f8f1a714265a17ad6423066ec43'

auth_header_b64 = base64.b64encode(f'{client_id}:{client_secret}'.encode('ascii'))
auth_header = auth_header_b64.decode('ascii')

req_header =  {'Authorization' : f'Basic {auth_header}'}
payload = {"grant_type": "client_credentials"}
url = "https://accounts.spotify.com/api/token"
resp = requests.post(url, headers = req_header, data = payload)
token = resp.json()

token['access_token']

#Defining header with access token
header = {
"Accept": "application/json",
"Content-Type": "application/json",
"Authorization": f"Bearer {token['access_token']}"
}


# In[2]:


#Get 2 ablbums from artist and return data frame with list of album ids and names
def albums(artist):
    oi = requests.get(f"https://api.spotify.com/v1/artists/{artist}/albums?limit=5", headers = header)
    oi = pd.DataFrame(oi.json()['items'])
    oi = oi[oi['album_group'] == 'album'] #exclude single tracks
    oi['artist_name'] = oi['artists'][0][0]['name'] #Fetching artist's name
    oi = oi[['id', 'name', 'release_date', 'artist_name']]
    return oi

#Function to get all tracks from one album
def tracks_albums(album_id):
    varb = requests.get(f"https://api.spotify.com/v1/albums/{album_id}/tracks?limit=50", headers = header)
    raw = pd.DataFrame(varb.json()['items'])
    varb = raw[['track_number']]
    varb['track_id'] = raw['id']
    #Guardando o nome do album:
    varb['album_id'] = album_id
    return varb

#Função para pegar lista de musicas de cada album
def album_to_track(id_artista):
    album_dos_artistas = albums(id_artista)
    tracks = map(tracks_albums, album_dos_artistas['id'])
    oi = map(lambda dt : dt.merge(album_dos_artistas, left_on = 'album_id', right_on = 'id'), tracks)
    return pd.concat(oi).drop(columns = ['id'])

#Getting global audio analysis for a single track
def audio_analysis(track):
    oi = requests.get(f"https://api.spotify.com/v1/audio-features/{track}", headers = header).json()
    return oi

#Function receives an artist and returns all of his albuns with global analysis for each track within each album.
def describing_tracks(artist):
    tracks_artists = album_to_track(artist)
    tracks = map(audio_analysis, tracks_artists['track_id'])
    tracks = pd.DataFrame(tracks)
    oi = tracks.merge(tracks_artists, left_on = "id", right_on = 'track_id').drop(columns = ['id'])
    return oi.drop(columns = ['analysis_url', 'track_href', 'uri'])

#Define query
def query_genre(genero):
    oi = requests.get(f"https://api.spotify.com/v1/search?q=genre:{genero}&type=artist&limit=10", headers = header).json()
    oi = pd.DataFrame(oi['artists']['items'])
    oi = oi.sample(5).reset_index()
    return oi

def get_albums_from_query(genero):
    query = query_genre(genero) #Complete query result
    artists = list(query['id']) #List of artist's ids
    concat_albums = [] #empty list which will receive complete audio analysis for each artist's albums
    for i in range(len(artists)):
        oi = describing_tracks(artists[i])
        generos = [query['genres'][i]]
        oi['list_of_genres'] = str(generos)
        concat_albums.append(oi)
    final = pd.concat(concat_albums)
    final['genre_defiened_by_query'] = genero #appending genres defined by query
    print("Finished genre")
    return final


# In[2]:


album_id = pd.read_csv("/home/pasoneto/Documents/Ciência/PhD/data/novo.csv")['album_id']
names = [requests.get(f"https://api.spotify.com/v1/albums/{i}/tracks?limit=50", headers = header).json()['items'][0]['name'] for i in album_id]

final = pd.DataFrame({'album_id': album_id, 'names': names})


# In[1]:


#####################################
########### Starting sample #########
#####################################

#genres = ["bebop", "bossa", "edm", "axe", "rock", "pop", "reggae", "country", "jazz", "cool", "folk"]
#genre2 = ["indie", "mpb", "forro", "sertanejo", "tango", "free", "swing"]
#genre3 = ["neurofunk", "american", "latin", "fusion", "ragtime", "bolero", "zouk"]
#genre4 = ["mandopop", "polo", "Morlam"]
#genre5 = ["celtic", "ska", "fado", "timba"] 
#genre6 = ["grunge", "sludge", "mariachi"]

final_sample = []
for i in genre6:
    final_sample.append(get_albums_from_query(i))

