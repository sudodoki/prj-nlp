import os
import re
import six
import itertools
import multiprocessing as mp

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

import tokenize_uk as tok
import pymorphy2

from sklearn.model_selection import StratifiedKFold, StratifiedShuffleSplit
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics import f1_score, classification_report, confusion_matrix
from sklearn.pipeline import Pipeline
from sklearn.ensemble import (RandomForestClassifier, AdaBoostClassifier,
                              GradientBoostingClassifier, ExtraTreesClassifier)
from sklearn.linear_model import SGDClassifier

from sklearn.utils import resample

import lightgbm


def plot_confusion_matrix(cm, classes,
                          title='Confusion matrix',
                          cmap=plt.cm.Blues):
    print(cm)

    plt.imshow(cm, interpolation='nearest', cmap=cmap)
    plt.title(title)
    plt.colorbar()
    tick_marks = np.arange(len(classes))
    plt.xticks(tick_marks, classes, rotation=45)
    plt.yticks(tick_marks, classes)

    fmt = 'd'
    thresh = cm.max() / 2.
    for i, j in itertools.product(range(cm.shape[0]), range(cm.shape[1])):
        plt.text(j, i, format(cm[i, j], fmt),
                 horizontalalignment="center",
                 color="white" if cm[i, j] > thresh else "black")

    plt.tight_layout()
    plt.ylabel('True label')
    plt.xlabel('Predicted label')


def parallelize(df, func):
    cpu_count = mp.cpu_count()
    pool = mp.Pool(cpu_count)
    results = pool.map(func, np.array_split(df, cpu_count))
    pool.close()
    pool.join()

    res = pd.concat(results)
    return res


def downsample(df):
    df_1 = df[df['stars'] == 1]
    df_2 = df[df['stars'] == 2]
    df_3 = df[df['stars'] == 3]
    df_4 = df[df['stars'] == 4]
    df_5 = df[df['stars'] == 5]

    df_3_downsampled = resample(df_3, replace=False, n_samples=len(df_1), random_state=1234)
    df_4_downsampled = resample(df_4, replace=False, n_samples=len(df_1), random_state=1234)
    df_5_downsampled = resample(df_5, replace=False, n_samples=len(df_1), random_state=1234)

    return pd.concat([df_1, df_2, df_3_downsampled, df_4_downsampled, df_5_downsampled])


def upsample(df):
    df_1 = df[df['stars'] == 1]
    df_2 = df[df['stars'] == 2]
    df_3 = df[df['stars'] == 3]
    df_4 = df[df['stars'] == 4]
    df_5 = df[df['stars'] == 5]

    df_1_upsampled = resample(df_1, replace=True, n_samples=len(df_5), random_state=1234)
    df_2_upsampled = resample(df_2, replace=True, n_samples=len(df_5), random_state=1234)
    df_3_upsampled = resample(df_3, replace=True, n_samples=len(df_5), random_state=1234)
    df_4_upsampled = resample(df_4, replace=True, n_samples=len(df_5), random_state=1234)

    return pd.concat([df_1_upsampled, df_2_upsampled, df_3_upsampled, df_4_upsampled, df_5])


# staking related code

rf_params = {
    'n_jobs': -1,
    'n_estimators': 500,
    'warm_start': True,
    'max_depth': 6,
    'min_samples_leaf': 2,
    'max_features' : 'sqrt',
    'verbose': 0,
    'class_weight': 'balanced'
}

et_params = {
    'n_jobs': -1,
    'n_estimators': 500,
    'max_depth': 8,
    'min_samples_leaf': 2,
    'verbose': 0,
    'warm_start': True,
    'class_weight': 'balanced'
}

ada_params = {
    'n_estimators': 500,
    'learning_rate' : 0.75
}

gb_params = {
    'n_estimators': 500,
    'max_depth': 5,
    'min_samples_leaf': 2,
    'warm_start': True,
    'verbose': 0
}
sgd_params = {
    'loss': 'log',
    'penalty': 'elasticnet',
    'max_iter': 10,
    'n_jobs': -1,
    'learning_rate': 'optimal',
    'class_weight': 'balanced'
}


def get_out_of_fold(clf, x_train, y_train, x_test,
                    ntrain, ntest, nfolds=5):
    """ Generates out of fold predictions for train and test dataset,
    which then used for training of the second level model

    Parameters
    ----------
    clf : obj
        Classifier to train (firts level model)

    """
    oof_train = np.zeros((ntrain,))
    oof_test = np.zeros((ntest,))
    oof_test_skf = np.empty((nfolds, ntest))

    skf = StratifiedKFold(n_splits=nfolds, shuffle=True, random_state=1234)

    for i, (train_index, test_index) in enumerate(skf.split(x_train, y_train)):
        x_tr = x_train[train_index]
        y_tr = y_train[train_index]
        x_te = x_train[test_index]

        clf.fit(x_tr, y_tr)

        oof_train[test_index] = clf.predict(x_te)
        oof_test_skf[i, :] = clf.predict(x_test)

    oof_test[:] = oof_test_skf.mean(axis=0)
    return oof_train.reshape(-1, 1), oof_test.reshape(-1, 1)


def train_stacked_oof(X, X_val, y, y_val):
    ntrain = X.shape[0]
    ntest = X_val.shape[0]

    rf_g = RandomForestClassifier(criterion='gini', **rf_params)
    rf_e = RandomForestClassifier(criterion='entropy', **rf_params)
    et_g = ExtraTreesClassifier(criterion='gini', **et_params)
    et_e = ExtraTreesClassifier(criterion='entropy', **et_params)
    ada = AdaBoostClassifier(**ada_params)
    gb = GradientBoostingClassifier(**gb_params)
    sgd = SGDClassifier(**sgd_params)

    cpu_count = mp.cpu_count()
    pool = mp.Pool(cpu_count)

    classifiers_args = [
        (rf_g, X, y, X_val, ntrain, ntest, 5),
        (rf_e, X, y, X_val, ntrain, ntest, 5),
        (et_g, X, y, X_val, ntrain, ntest, 5),
        (et_e, X, y, X_val, ntrain, ntest, 5),
        (ada, X, y, X_val, ntrain, ntest, 5),
        (gb, X, y, X_val, ntrain, ntest, 5),
        (sgd, X, y, X_val, ntrain, ntest, 5)
    ]

    results = [pool.apply_async(get_out_of_fold, args=x) for x in classifiers_args]
    output = [res.get() for res in results]

    pool.close()
    pool.join()

    rf_g_oof_train, rf_g_oof_test = output[0]
    rf_e_oof_train, rf_e_oof_test = output[1]
    et_g_oof_train, et_g_oof_test = output[2]
    et_e_oof_train, et_e_oof_test = output[3]
    ada_oof_train, ada_oof_test = output[4]
    gb_oof_train, gb_oof_test = output[5]
    sgd_oof_train, sgd_oof_test = output[6]

    x_train_ = np.concatenate([rf_g_oof_train, rf_e_oof_train,
                               et_g_oof_train, et_e_oof_train,
                               ada_oof_train, gb_oof_train,
                               sgd_oof_train
                               ], axis=1)
    x_test_ = np.concatenate([rf_g_oof_test, rf_e_oof_test,
                              et_g_oof_test, et_e_oof_test,
                              ada_oof_test, gb_oof_test,
                              sgd_oof_test
                              ], axis=1)
    return x_train_, x_test_
