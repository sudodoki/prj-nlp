from pathlib import Path

log_fmt='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
date_fmt="%Y-%m-%d %H:%M:%S"

root_dir = Path(__file__).resolve().parent
posts_dir = root_dir / "posts"

wiki_filename = "bgwiktionary-20180301-pages-articles-multistream.xml.bz2"
wiki_output = "wiktionary.txt"

def init_dir(folder):
    Path.mkdir(folder, parents=True, exist_ok=True)