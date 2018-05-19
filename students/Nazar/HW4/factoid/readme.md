#Fact checking system

##Sourse:
there was questionas(factoid) and correct answers(patterns of correct answers) open domain ~800 pairs
i do some simple(it was not transforms well in all the cases) transformation from q to statement

##Base fact checking
1. Extract entities by using wikification, if there is less than 3 entities - add aditional using spacy
2. Load abstract text from wikipedia of each entity, check if there is oyher entities inside. it's a bit stupid - cose i didnot check dependensies in that case, in this aproach i get ~ 87% of recall
3. build identifier-atribute-value based on rules, as an input was statement
4. repeat step 2. it gets additional ~5%

##next steps
1. increase quality of rules
2. use sparql and DBPedia
3. use sinonimus
4. add postprocessing for the triplets
5. check not only if entity is inside of text, but olso dependensy
6. There is some logical statement like 1+1=2, this is out of scope