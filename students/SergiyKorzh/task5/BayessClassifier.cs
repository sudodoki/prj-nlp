using System;
using System.Collections.Generic;
using System.Linq;

namespace NLPCourse.Task5
{
    public class BayessClassifier {
        private IDictionary<string, double[]> _bag = new Dictionary<string, double[]>();

        private double[] _tokenTotals;
        private double[] _textTotals;

        private int _featureNum = 0;


        public BayessClassifier(int featureNum) {
            _featureNum = featureNum;
            _tokenTotals = new double[featureNum];
            _textTotals = new double[featureNum];
        }

        public void TrainReset() {
            for (int i = 0; i < _featureNum; i++) {
                _tokenTotals[i] = 0;
                _textTotals[i] = 0;
            }
        }

        public void TrainText(string[] tokens, params double[] features) {
            if (features.Length > _featureNum) {
                throw new BayessClassifierError($"Incorrect number of features. Expected: {_featureNum}, passed: {features.Length}");
            }

            foreach (var token in tokens) {
                TrainToken(token, features);
            }

            for (int i = 0; i < features.Length; i++) {
                _textTotals[i] += features[i];
            }
        }

        public void TrainToken(string token, params double[] features) {
            if (!_bag.TryGetValue(token, out double[] tokFeatures)) {
                tokFeatures = new double[_featureNum];
                _bag[token] = tokFeatures;
            }

            for (int i = 0; i < features.Length; i++) {
                tokFeatures[i] += features[i];
                _tokenTotals[i] += features[i];
            }
        }


        public double[] Predict(string[] tokens) {
            var predictVect = _textTotals.Select(d => 1d).ToArray();

            double total = _textTotals.Sum();
            var bias = _textTotals.Select(d => d / total).ToArray();

            foreach (var token in tokens) {
                if (_bag.TryGetValue(token, out double[] features)) {
                    //we just ignore the words not found in the bag

                    for (int i = 0; i < _featureNum; i++) {
                        predictVect[i] *= features[i]/_tokenTotals[i];
                    }
                }
            }

            for (int i = 0; i < _featureNum; i++) {
                predictVect[i] *= bias[i];
            }

            return predictVect;
        }

    }

    public class BayessClassifierError : Exception {
        public BayessClassifierError(string message) : base(message) { }
    }


}
