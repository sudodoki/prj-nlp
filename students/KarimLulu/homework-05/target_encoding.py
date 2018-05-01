import pandas as pd
from pandas.api.types import CategoricalDtype
from itertools import zip_longest
import string
import random

class Converter(object):

    def __init__(self, dtype=CategoricalDtype(), dtypes=list(), columns=None,
                 exclude_cols=list()):
        self.columns = columns
        self.exclude_cols = exclude_cols
        self.dtype = dtype
        self.dtypes = dtypes

    @property
    def dtypes(self):
        return self._dtypes

    @dtypes.setter
    def dtypes(self, types):
        if types:
            self._dtypes = types
        else:
            n = len(self.columns) if self.columns else 1
            self._dtypes = [self.dtype] * n

    def __call__(self, df):
        if isinstance(df, pd.Series):
            df = df.to_frame(df.name)
        columns = self.columns if self.columns is not None else df.columns
        for col, dtype in zip_longest(columns, self.dtypes, fillvalue=self.dtype):
            if col not in self.exclude_cols:
                df[col] = df[col].astype(dtype)
        return df


class NAFiller(object):

    def __init__(self, na_value="-999", columns=None):
        self.columns = columns
        self.na_value = na_value

    def __call__(self, df):
        if isinstance(df, pd.Series):
            df = df.to_frame(df.name)
        columns = self.columns if self.columns is not None else df.columns
        for col in columns:
            while True:
                try:
                    df[col] = df[col].cat.add_categories(self.na_value).fillna(self.na_value)
                    break
                except ValueError:
                    self.na_value += random.choice(alphabet)
        return df


class TargetEncoder(object):

    def __init__(self, type_="expanding", na_as_category=True, na_value="-999",
                 columns=None, feature_names=None):
        self.na_as_category = na_as_category
        self.na_value = na_value
        self.columns = columns
        self.feature_names = feature_names
        self.mapping = None

    def fit(self, X, y):
        assert X.shape[0] == y.shape[0]
        self._train = True
        self._fitted = False
        _, mapping = self.target_encode(X, y, mapping=self.mapping, cols=self.columns,
                                        na_as_category=self.na_as_category, na_value=self.na_value)
        self.mapping = mapping
        self._fitted = True
        return self

    def transform(self, X, y=None):
        if not self._fitted:
            raise ValueError('Must train encoder before it can be used to transform data.')
        assert (y is None or X.shape[0] == y.shape[0])
        X, mapping = self.target_encode(X, y, mapping=self.mapping, cols=self.columns,
                                        na_as_category=self.na_as_category,
                                        na_value=self.na_value)
        return X

    def target_encode(self, X_in, y, mapping=None, cols=None, na_as_category=None, na_value="-999"):
        X = X_in.copy(deep=True)
        if cols is None:
            cols = X.columns.values
        if na_as_category:
            converter = Converter(dtype=CategoricalDtype(), columns=cols)
            nafiller = NAFiller(na_value=na_value, columns=cols)
            X = nafiller(converter(X))
        if mapping is not None and self._fitted:
            mapping_out = mapping
            index = X.index
            for el in self.mapping:
                if self._train:
                    grouper = el.pop("grouper")
                    cumsum = grouper.cumsum() - y
                    cumcnt = grouper.cumcount()
                    new_col = self.feature_names.get(el["col"], el["col"]+"_tmp")
                    X[new_col] = cumsum * 1.0 / cumcnt
                    X[new_col] = X[new_col].fillna(self._mean)
                    del X[el["col"]]
                    #X = X.rename(columns={new_col: el["col"]})
                else:
                    means = el["mean"]
                    new_col = self.feature_names.get(el["col"], el["col"]+"_tmp")
                    X[new_col] = (pd.merge(X.loc[:, el["col"]].reset_index(),
                                           means.reset_index(),
                                           how="left",
                                           on=el["col"])
                                  .set_index("index")["mean"].rename(new_col))
                    X[new_col] = X[new_col].fillna(self._mean)
                    del X[el["col"]]
                    #X = X.rename(columns={new_col: el["col"]})
            if self._train:
                self._train = False
        else:
            self._mean = y.mean()
            mapping_out = []
            for col in cols:
                grouper = y.groupby(X[col])
                means = grouper.agg(["mean"])
                mapping_out.append({"col": col, "grouper": grouper, "mean": means})
        return X, mapping_out

    def fit_transform(self, X, y):
        return self.fit(X, y).transform(X, y)