from sklearn import metrics
import pandas as pd
import numpy as np

def calc_metrics(y_test, pred, proba=None, labels=None, print_=True, mode="binary"):
    output = {}
    if proba is not None:
        roc_auc = metrics.roc_auc_score(y_test, proba)
        output["AUC"] = roc_auc
    output["Recall"] = metrics.recall_score(y_test, pred, average=mode)
    output["Precision"] = metrics.precision_score(y_test, pred, average=mode)
    output["F1"] = metrics.f1_score(y_test, pred, average=mode)
    output["Accuracy"] = metrics.accuracy_score(y_test, pred)
    if labels is not None:
        index = labels
        columns = ["pred_" + str(el) for el in index]
    else:
        columns = None
        index = None
    if not all(el in labels for el in y_test):
        labels = np.unique(y_test)
    conf_matrix = pd.DataFrame(metrics.confusion_matrix(y_test, pred, labels=labels),
                               columns=columns, index=index)
    report = metrics.classification_report(y_true=y_test, y_pred=pred, labels=labels)
    if print_:
        for key, value in output.items():
            print(f"{key}: {value:0.3f}")
        print("\nConfusion matrix:")
        print(conf_matrix)
        print("\nReport:")
        print(report)
    return output, report, conf_matrix

def importance(classes, coefs, names, n=15):
    for k, class_ in enumerate(classes):
        print(f"{class_}\n")
        data = sorted(zip(names, coefs[k]), key=lambda x: np.abs(x[-1]), reverse=True)[:n]
        for name, coef in data:
            print(f"{name}: {coef:0.3f}")
        print()