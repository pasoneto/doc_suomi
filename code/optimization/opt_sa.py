#%%
import matplotlib.pyplot as plt
import numpy as np
from itertools import permutations
import random
import math
import pandas as pd
#%%
# Swap function 
def swap(S, T):
    velho = S
    shuf = S.loc[1:, :].sample(frac = 1)
    novo = pd.concat([S.iloc[[0]], shuf], ignore_index=True, axis=0)
    return novo, velho

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
def opt(S, T, likelyhood, tm, swap, Temperature, outer = 1000, cooling = 0.6, n = 1000):
    history = []
    h_temp = []
    likelyhood_original = likelyhood(S, tm)
    for j in range(outer):
        for i in range(n):
            new, S = swap(S, T)
            d_energy = likelyhood(new, tm) - likelyhood(S, tm)
            if d_energy > 0:
                S = new
                best = likelyhood(S, tm)
                history.append(best)
                h_temp.append(Temperature)
            if d_energy <= 0:
                p = np.exp(-np.abs(d_energy)/Temperature)
                chance = random.random()
                if p > chance:
                    S = new
                    best = likelyhood(S, tm)                
                    history.append(best)
                    h_temp.append(Temperature)
        Temperature = Temperature*cooling 
        if T == 0:
            return S, history, h_temp
    return S, history, h_temp
