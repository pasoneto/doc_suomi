#%%
from opt_sa import *
import pandas as pd
def splitter(data, group):
    data = list(data.groupby(group))
    data = [data[i][1].reset_index(drop = True) for i in range(len(data))]
    return data

#%% Importing data
variables = ["album_id", "track_number", "valence", "energy", "loudness", "tempo"]
data = pd.read_csv("/home/pa/Documents/github/doc_suomi/data/optimization/data_discrete.csv")
data = data[variables]
data = splitter(data, "album_id")

#%%
data = [swap(i, 1)[0] for i in data]
data = data[round(len(data)*0.8) : len(data)]

#%% #Empirical transition matrices
tm_v = [0.3205376, 0.6794624, 0.6677269, 0.3322731, 0.5336611, 0.4663389]
tm_e = [0.3118086, 0.6881914, 0.6616254, 0.3383746, 0.5075643, 0.4924357]
tm_l = [0.3223561, 0.6776439, 0.6515744, 0.3484256, 0.5279879, 0.4720121]
tm_t = [0.3328422, 0.6671578, 0.6619533, 0.3380467, 0.5075643, 0.4924357]
tm = [tm_v, tm_e, tm_l, tm_t]
# %%
resultados = []
historicos = []
count = 0
for k in data:
    r, h, _, = opt(k,
                   likelyhood = likelyhood,
                   tm = tm,
                   swap = swap,
                   Temperature= 5,
                   outer = 500,
                   cooling = 0.2,
                   n = 10) 
    resultados.append(r)
    # historicos.append([h[0], maior])
    count = count+1
    print("----------------------------------------------------------")
    print("----------------------------------------------------------")
    print("Album ", count, "/", len(data), "| fomos de ", h[0], "para ", likelyhood(r, tm))
    print("----------------------------------------------------------")
    print("----------------------------------------------------------")
# %%
final = pd.concat(resultados)

#%%
final.to_csv("reordered.csv")
















#%%
final.to_csv("reordered.csv")

#%% ########### EVAL
dt = pd.read_csv("/home/pa/Documents/github/doc_suomi/data/optimization/reordered.csv")
gt = pd.read_csv("/home/pa/Documents/github/doc_suomi/data/optimization/data_discrete.csv")

variables = ["album_id", "track_number", "valence", "energy", "loudness", "tempo"]

gt = gt[variables]
gt = splitter(gt, "album_id")
gt = gt[round(len(gt)*0.8) : len(gt)]

gt = pd.concat(gt)

#%%
gt.to_csv("original.csv")






#%%
print(result)
plt.plot(history)
plt.show()
plt.plot(temp)
plt.show()
print(result)

# %%


