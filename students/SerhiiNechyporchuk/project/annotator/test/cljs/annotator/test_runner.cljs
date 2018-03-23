(ns annotator.test-runner
  (:require
   [doo.runner :refer-macros [doo-tests]]
   [annotator.core-test]
   [annotator.common-test]))

(enable-console-print!)

(doo-tests 'annotator.core-test
           'annotator.common-test)
