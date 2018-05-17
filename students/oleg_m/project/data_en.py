import pandas as pd


DATA_DIR = '/Users/admin/edu/NLP/practical_NLP_course/data/'
BLOGS_EN_FILE = 'blog-authorship-corpus.zip'

df = pd.read_csv(DATA_DIR+BLOGS_EN_FILE, compression='zip')
print(df.shape)
print(df.head())
df.drop_duplicates(subset="text", inplace=True)
print(df.text.str.len().describe())
