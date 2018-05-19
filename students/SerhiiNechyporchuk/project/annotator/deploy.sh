lein uberjar
scp target/annotator-$1.jar nlp-proj:./annotator/
ssh nlp-proj ln -s ./annotator/annotator-$1.jar ./annotator/latest.jar