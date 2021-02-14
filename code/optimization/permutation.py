#%%
import numpy as np
from itertools import permutations
import random
import math
#%%
# Swap function 
def swap(S, T):
    indices = list(range(len(S)))
    original = [i for i in S]
    swp_indices = np.random.choice(indices, T, replace = False)
    elements = [S[i] for i in swp_indices]
    np.random.shuffle(elements)
    for i, j in zip(swp_indices, elements):
        S[i] = j
    return S, original

def transProb(x, tm):
    if x[0] == "up" and x[1] == "up":
        return tm[0]
    if x[0] == "up" and x[1] == "down":
        return tm[1]
    if x[0] == "down" and x[1] == "down":
        return tm[2]
    if x[0] == "down" and x[1] == 'up':
        return tm[3]

def likelyhood(S, tm):
    probs = [transProb(S[i:], tm) for i in range(len(S)-1)]
    return np.mean([math.log2(i) for i in probs])

# cf:   cost function
# sf:   random shuffler
# T:    number of items to shuffle
# S:    Sequence of items
# n_it: Number of iterations before updating T
def opt(S, T, likelyhood, tm, swap, Temperature, outer = 1000, cooling = 0.6, n = 1000):
    history = []
    h_temp = []
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
                print(p, chance)
                if p > chance:
                    S = new
                    best = likelyhood(S, tm)                
                    history.append(best)
                    h_temp.append(Temperature)
        Temperature = Temperature*cooling 
        T = round(T-0.01)
        if T == 1:
            return S, history, h_temp
    return S, history, h_temp

import matplotlib.pyplot as plt

S = ["down", "down", "down", "up", "up", "up"]
tm = [0.3, 0.6, 0.3, 0.6]

result, history, temp = opt(S, T = 6, likelyhood = likelyhood, tm = tm, swap = swap, Temperature= 100, outer = 100, cooling = 0.2, n = 10)
plt.plot(history)
plt.show()
plt.plot(temp)
plt.show()

# print(opt(S, 6, likelyhood, tm, swap, 10))
# print(likelyhood(["up", "down"], tm) == likelyhood(['up', "down", "up", "down", "up"], tm))
# print(likelyhood(["up", "up", "up", "down", "down", "down"], tm) < likelyhood(["up", "down", "up", "down", "up", "down"], tm))


# %%
# print([1, 2, 3]*10)