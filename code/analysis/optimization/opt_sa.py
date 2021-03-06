#%%
import matplotlib.pyplot as plt
import numpy as np
from itertools import permutations
import random
import math
import pandas as pd
#%%
# Swap function 
# def swap(S, T):
#     velho = S
#     comeco = S.loc[0:T-1, :]
#     shuf = S.loc[T:, :].sample(frac = 1, replace = False)
#     novo = pd.concat([comeco, shuf], ignore_index=True, axis=0)
#     return novo, velho

def swap(S, T):
    velho = S.copy()
    ind = random.sample(list(range(1, len(S.index))), T)
    res = S.loc[ind].sample(frac = 1).reset_index(drop = True)
    for i, x in zip(ind, list(range(len(S.index)))):
        S.loc[i] = res.loc[x].values
    return S, velho

def splitter(data, group):
    data = list(data.groupby(group))
    data = [data[i][1].reset_index(drop = True) for i in range(len(data))]
    return data
    
def transProb(x, tm):
    if x[0] == "greater" and x[1] == "greater":
        return tm[0]
    if x[0] == "greater" and x[1] == "smaller":
        return tm[1]
    if x[0] == "smaller" and x[1] == "greater":
        return tm[2]
    if x[0] == "smaller" and x[1] == 'smaller':
        return tm[3]
    if x[0] == "start"   and x[1] == 'greater':
        return tm[4]
    if x[0] == "start"   and x[1] == 'smaller':
        return tm[5]

def likelyhood(S, tm):
    valence  = np.array([transProb(np.array(S["valence"])[i:], tm[0])  for i in range(len(S["valence"])-1)])
    energy   = np.array([transProb(np.array(S["energy"])[i:], tm[1])   for i in range(len(S["energy"])-1)])
    loudness = np.array([transProb(np.array(S["loudness"])[i:], tm[2])  for i in range(len(S["loudness"])-1)])
    tempo    = np.array([transProb(np.array(S["tempo"])[i:], tm[3])     for i in range(len(S["tempo"])-1)])
    final = (valence*1.2)*energy*loudness*(tempo*1.2)
    final = np.mean([math.log2(i) for i in final])
    return final

# cf:   cost function
# sf:   random shuffler
# T:    number of items to shuffle
# S:    Sequence of items
# n_it: Number of iterations before updating T
def opt(S, likelyhood, tm, swap, Temperature, outer = 1000, cooling = 0.6, n = 1000):
    history = []
    historia_maior = []
    h_temp = []
    d = []
    T_max = len(S.index)-1
    T = T_max
    original = S.copy()
    likelyhood_original = likelyhood(original, tm)
    for j in range(outer):
        for i in range(n):
            new, S = swap(S, T)
            l_n = likelyhood(new, tm)
            l_o = likelyhood(S, tm)
            d_energy = l_n - l_o
            if d_energy > 0:
                S = new.copy()
                best = l_o
                history.append(best)
                h_temp.append(Temperature)
                p = np.exp(-np.abs(d_energy)/Temperature)
                chance = random.random()
                taxa_melhora = (abs(likelyhood_original)-abs(likelyhood(S, tm)))/abs(likelyhood_original)
                if random.uniform(0, 0.5) <= taxa_melhora:
                    T = T-1
            if d_energy <= 0:
                p = np.exp(-np.abs(d_energy)/Temperature)
                chance = random.random()
                if p > chance:
                    S = new.copy()
                    best = l_n
                    history.append(best)
                    h_temp.append(Temperature)
                    d.append(S)
                    if T < T_max:
                        taxa_melhora = (abs(likelyhood_original)-abs(likelyhood(S, tm)))/abs(likelyhood_original)
                        if random.uniform(0, -1) <= taxa_melhora:
                            T = T+1
        if T > 2:
            taxa_melhora = (abs(likelyhood_original)-abs(likelyhood(S, tm)))/abs(likelyhood_original)
            if random.uniform(0, 1) <= taxa_melhora:
                T = T-1
        print("Taxa melhora:", round(taxa_melhora, 2), "| N shuffled tracks:", T, "|Current likelyhood:", round(likelyhood(S, tm), 2), "| Likelyhood original:", round(likelyhood_original, 2))
        Temperature = Temperature*cooling
    return S, history, h_temp, d

#%%
data = pd.read_csv("/home/pa/Documents/github/doc_suomi/data/optimization/data_discrete.csv")
variables = ["album_id", "track_number", "valence", "energy", "loudness", "tempo"]
data = data[variables]
data = splitter(data, "album_id")
#%%
data = [swap(i, 5)[0] for i in data]
#%%
tm_v = [0.3205376, 0.6794624, 0.6677269, 0.3322731, 0.5336611, 0.4663389]
tm_e = [0.3118086, 0.6881914, 0.6616254, 0.3383746, 0.5075643, 0.4924357]
tm_l = [0.3223561, 0.6776439, 0.6515744, 0.3484256, 0.5279879, 0.4720121]
tm_t = [0.3328422, 0.6671578, 0.6619533, 0.3380467, 0.5075643, 0.4924357]
tm = [tm_v, tm_e, tm_l, tm_t]
#%%
import matplotlib.animation as animation

result, history, temp, d = opt(data[50],                       
                             likelyhood = likelyhood,
                             tm = tm,
                             swap = swap,
                             Temperature= 5,
                             outer = 500,
                             cooling = 0.6,
                             n = 10)
plt.plot(history)
plt.plot(history.index(max(history)), max(history), ".", markersize = 10)
plt.show()
plt.plot(temp)
plt.show()
#%%
sur = [d[-1] for i in range(len(history)-len(d))]
d = d+sur

# %%
#%%
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation

x = list(range(len(history)))
y = history

fig, ax = plt.subplots()
line, = ax.plot(x, y, color='k')

def update(num, x, y, line):
    line.set_data(x[:num], y[:num])
    line.axes.axis([0, len(history), min(y)-0.25, max(y)+0.25])
    return line,

ani = animation.FuncAnimation(fig, update, len(x), fargs=[x, y, line],
                              interval=50, blit=True)
ani.save('likelyhood.gif', dpi = 300)
plt.show()



# %%
x = list(range(len(temp)))
y = temp 

fig, ax = plt.subplots()
line, = ax.plot(x, y, color='k')

def update(num, x, y, line):
    line.set_data(x[:num], y[:num])
    line.axes.axis([0, len(history), min(y)-0.25, max(y)+0.25])
    return line,

ani = animation.FuncAnimation(fig, update, len(x), fargs=[x, y, line],
                              interval=50, blit=True)
ani.save('temp.gif', dpi = 300)
plt.show()

# %%
import dataframe_image as dfi
dfi.export(data[50], 'imagem.png')
# %%
for i in range(len(d)):
    dfi.export(d[i], f'./data/imagem{i}.png')
# %%
import imageio
images = []
filenames = [f"./data/imagem{i}.png" for i in range(len(d))]
for filename in filenames:
    images.append(imageio.imread(filename))
imageio.mimsave('movie.gif', images, duration = 0.03048780487804878)
# %%

