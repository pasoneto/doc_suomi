#!/usr/bin/env python
# coding: utf-8

# In[1]:


from keras.models import Sequential
from keras.layers import Dense, LSTM, Dropout
from sklearn import preprocessing
from sklearn.preprocessing import MinMaxScaler, OneHotEncoder
import numpy as np
import keras
import tensorflow as tf
import keras.backend as kb
from PIL import Image
import itertools
import matplotlib.pyplot as plt
import pandas as pd
from sklearn.metrics import mean_squared_error, accuracy_score, roc_auc_score, roc_curve
from keras.callbacks import EarlyStopping, ReduceLROnPlateau, ModelCheckpoint


# In[2]:


def sampleMaker_entry(sample, input_size, output_size):
    n_slides = len(sample.index) - (output_size + input_size)+1
    entrada = np.array([np.array(sample[entrada_var].iloc[i:i+input_size]) for i in range(0, n_slides, input_size)])
    return entrada

def sampleMaker_out(sample, input_size, output_size):
    n_slides = len(sample.index) - (output_size + input_size)+1
    saida = [sample[saida_var].iloc[i+input_size:i+input_size+output_size] for i in range(0, n_slides, input_size)]
    return saida

def splitter(data, group):
    data = list(data.groupby(group))
    data = [data[i][1] for i in range(len(data))]
    return data


# In[34]:


base = pd.read_csv('/home/pasoneto/Documents/github/doc_suomi/data/lstm/lstm.csv')

le = preprocessing.LabelEncoder()
base['valence_cat'] = le.fit_transform(base['valence_cat'])
base['energy_cat'] = le.fit_transform(base['energy_cat'])
base['loudness_cat'] = le.fit_transform(base['loudness_cat'])
base['tempo_cat'] = le.fit_transform(base['tempo_cat'])


# In[5]:





# In[6]:


base = splitter(base, "album_id")
for i in base:
    i.reset_index(drop = True, inplace = True)


# In[7]:


treino = base[0:int(len(base)*0.8)]
teste = base[len(treino):len(base)]


# In[8]:


entrada_var   = ['danceability', 'energy', 'loudness_overall', 'mode_confidence','speechiness', 'acousticness', 'instrumentalness', 'liveness', 'valence', 'tempo_overall', 'duration_ms', 'time_signature_confidence', 'loudness_continuous', 'tempo_continuous', 'tempo_confidence', 'key_confidence', 'danceability_cum', 'energy_cum','loudness_overall_cum', 'speechiness_cum', 'acousticness_cum','instrumentalness_cum', 'liveness_cum', 'valence_cum','tempo_overall_cum', 'duration_ms_cum', 'time_signature_cum','loudness_continuous_cum', 'tempo_continuous_cum','tempo_confidence_cum', 'key_confidence_cum', 'mode_confidence_cum','time_signature_confidence_cum']
saida_var     = ['tempo_cat'] #'energy_cat', 'loudness_cat', 'tempo_cat', 'album_id']

entrada_treino = list(map(lambda x : sampleMaker_entry(x, 5, 1), treino))
saida_treino   = list(map(lambda x : sampleMaker_out(x, 5, 1), treino))

entrada_teste = list(map(lambda x : sampleMaker_entry(x, 5, 1), teste))
saida_teste   = list(map(lambda x : sampleMaker_out(x, 5, 1), teste))


# In[9]:


#import itertools
entrada_treino = np.array(list(itertools.chain.from_iterable(entrada_treino)))
saida_treino   = np.array(list(itertools.chain.from_iterable(saida_treino)))

entrada_teste = np.array(list(itertools.chain.from_iterable(entrada_teste)))
saida_teste   = np.array(list(itertools.chain.from_iterable(saida_teste)))


# In[ ]:


# Definindo modelo
regressor = Sequential()

regressor.add(LSTM(units = 60, return_sequences = True, activation = 'relu', input_shape = (entrada_treino.shape[1], 33)))
regressor.add(Dropout(0.2))

regressor.add(LSTM(units = 120, activation = 'sigmoid'))
regressor.add(Dropout(0.5))

regressor.add(Dense(units = 240, activation = 'sigmoid'))

regressor.add(Dense(units = 1, activation = 'sigmoid'))

regressor.compile(optimizer = 'sgd', loss = "binary_crossentropy", 
                  metrics = ['accuracy'])


# In[ ]:


# Fitando modelo
regressor.fit(entrada_treino, saida_treino, epochs = 1500, batch_size = 120)


# In[ ]:


from sklearn.metrics import confusion_matrix
previsores = list(itertools.chain.from_iterable(regressor.predict_classes(entrada_teste)))
real = list(itertools.chain.from_iterable(list(itertools.chain.from_iterable(saida_teste))))
probs = list(itertools.chain.from_iterable(regressor.predict_proba(entrada_teste)))

print("Model: ", accuracy_score(previsores, real))


# In[ ]:


# 0 greater
# 1 smaller
for i in range(len(previsores)):
    for k in range(len(previsores[i])):
        if previsores[i][k] == 0:
            previsores[i][k] = np.mean(real[i])+np.random.uniform(0.08, 0.1)
        if previsores[i][k] == 1:
            previsores[i][k] = np.mean(real[i])-np.random.uniform(0.08, 0.1)
    plt.plot(previsores[i], 'ro', color = 'red', label = 'Valencia prevista')
    plt.plot(real[i], 'ro', color = 'blue', label = 'Valencia real')
    plt.plot(previsores[i], color = 'red')
    plt.plot(real[i], color = 'blue')
    plt.show()


# # Saving model - Energy

# In[ ]:


# serialize model to JSON
model_json = regressor.to_json()
with open("model_tempo.json", "w") as json_file:
    json_file.write(model_json)
# serialize weights to HDF5
regressor.save_weights("model_tempo.h5")
print("Saved model to disk")


# # Loading model
# 
# Loading model's weights.

# In[ ]:


# load json and create model
from keras.models import model_from_json
json_file = open('model_energy.json', 'r')
loaded_model_json = json_file.read()
json_file.close()
loaded_model = model_from_json(loaded_model_json)
# load weights into new model
loaded_model.load_weights("model_energy.h5")
print("Loaded model from disk")


# In[ ]:


# evaluate loaded model on test data
loaded_model.compile(optimizer = 'adam', loss = 'binary_crossentropy', 
                  metrics = ['accuracy'])ww


# In[ ]:


from sklearn.metrics import confusion_matrix
previsores = list(itertools.chain.from_iterable(loaded_model.predict_classes(entrada_teste)))
real = list(itertools.chain.from_iterable(list(itertools.chain.from_iterable(saida_teste))))

print("Model: ", accuracy_score(previsores, real))


# In[ ]:





# In[ ]:




