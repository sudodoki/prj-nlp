import spacy

def normalization(infilePath, outfilePath):
	nlp = spacy.load('en', disable=['parser', 'ner'])

	infile = open(infilePath, 'r')
	outfile = open(outfilePath, 'w')
	for line in infile:
		doc = nlp(line)
		normalized = []
		firstIndex = 1 if (doc[0].pos_ in ["PUNCT", "PART"]) else 0
		lastIndex = len(doc) - 3 if (doc[len(doc) - 2].pos_ in ["PUNCT", "PART"]) else len(doc) - 2
		for idx, tok in enumerate(doc):
			string = tok.string
			pos = tok.pos_
			if (idx == firstIndex or idx == lastIndex or pos in ["NOUN", "PROPN", "ADJ", "VERB", "ADV", "SCONJ"]):
				if (string.strip() != "n't"):
					string = string[:1].upper() + string[1:]
			elif (pos in ["CCONJ", "ADP", "INTJ", "PART", "Art"]):
				string = string.lower()

			if (not pos in ["SPACE"]):		
				normalized.append(string)

		print(u''.join(normalized), file=outfile)
	return;


normalization(infilePath='/Users/gift/Grammarly/examiner-headlines.txt', outfilePath='/Users/gift/Grammarly/output.txt')