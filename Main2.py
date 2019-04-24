import sklearn.datasets as data
from sklearn.cluster import KMeans
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import random as rd

#USE GAUSSIAN MIXTURE MODEL


def makeData(n, std):
    X1 = data.make_blobs(n_samples=n, centers=1, n_features=3, cluster_std=std, center_box=(0, 0))
    df = pd.DataFrame(np.hstack([np.array(X1[0]), np.zeros(np.array(X1[0]).shape)]))
    df['labels'] = np.ones(n)
    X2 = data.make_blobs(n_samples=n, centers=1, n_features=3, cluster_std=std, center_box=(0, 0))
    dftmp = pd.DataFrame(np.hstack([np.zeros(np.array(X1[0]).shape), np.array(X2[0])]))
    dftmp['labels'] = np.ones(n)*2
    return df.append(dftmp, ignore_index=True)

def cluster_KMeans(df, k): #Hanterar dataframe

    labelsArray = []
    oldLabels = list(df['labels'])
    data = df.drop('labels', axis=1)
    data = data.values

    X = StandardScaler().fit_transform(data)
    print('Executing K-Means clustering on ' + str(len(data[:, 0])) + ' points.')
    print('Looking for k=' + str(k) + ' clusters.')
    print()

    # Clustering
    km = KMeans(n_clusters=k, random_state=0, init = 'k-means++').fit(X)
    labels = km.labels_
    n_clusters = len(set(labels))
    print(str(n_clusters) + " clusters found.")

    for i in range(0,len(km.labels_)):
        labelsArray.append('Cluster '+str(km.labels_[i] + 1))

    dfNew = pd.DataFrame(data=data)

    dfNew['labels']=labelsArray

    for i in range(0, n_clusters, 1):
            print('#Points in cluster ' + str(i+1) + ': ' + str(len(dfNew.loc[dfNew['labels'] == 'Cluster '+str(i+1)]))+'.')
    dfNew['oldLabels'] = oldLabels

    return dfNew


def getPCs(df, n_components):

    labels = list(df['labels'])
    data = df.drop('labels', axis=1)
    if 'oldLabels' in df.columns:
        oldLabels = list(df['oldLabels'])
        data = data.drop('oldLabels', axis=1)

    tmp = data.values
    standard = StandardScaler()
    tmpS = standard.fit_transform(tmp)
    data = pd.DataFrame(tmpS)

    pca = PCA(n_components=n_components)
    pca.fit(data)
    columns = ['PC %i' % i for i in range(1,n_components+1)]
    df_pca = pd.DataFrame(pca.transform(data), columns=columns, index=df.index)

    df_pca['labels'] = labels
    if 'oldLabels' in df.columns:
        df_pca['oldLabels'] = oldLabels

    return df_pca


df = makeData(100, 1)
df_cl = cluster_KMeans(df, 2)
df_pca = getPCs(df_cl, 2)

plt.scatter(df_pca['PC 1'], df_pca['PC 2'])

ax1 = plt.subplot(1,2,1)
df_pca[df_pca['oldLabels']==1].plot.scatter(x='PC 1', y='PC 2', color='DarkGreen', marker='o', label='Cluster 1', ax = ax1)
df_pca[df_pca['oldLabels']==2].plot.scatter(x='PC 1', y='PC 2', color='blue', marker='>', label='Cluster 2', ax = ax1)
plt.xlabel('PC 1', fontsize = 14)
plt.ylabel('PC 2', fontsize = 14)
plt.title('True data', fontsize = 14)

ax2 = plt.subplot(1,2,2)
df_pca[df_pca['labels']=='Cluster 1'].plot.scatter(x='PC 1', y='PC 2', color='DarkGreen', marker='o', label='Cluster 1', ax = ax2)
df_pca[df_pca['labels']=='Cluster 2'].plot.scatter(x='PC 1', y='PC 2', color='blue', marker='>', label='Cluster 2', ax = ax2)
plt.xlabel('PC 1', fontsize = 14)
plt.ylabel('PC 2', fontsize = 14)
plt.title('K-Means clustering', fontsize = 14)

plt.show()