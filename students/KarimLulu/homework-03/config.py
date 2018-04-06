from pathlib import Path

log_fmt='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
date_fmt="%Y-%m-%d %H:%M:%S"

root_dir = Path(__file__).resolve().parent
data_dir = root_dir / "data"
posts_dir = data_dir / "posts"
persons_dir = data_dir / "persons"

wiki_filename = "bgwiktionary-20180301-pages-articles-multistream.xml.bz2"
wiki_output = "synonyms.txt"
hosts_output = "hosts.txt"
servers_output = "servers.txt"
annotated_data = "conll14st-test-data.tar.gz"
annotations_output = "annotations.json"
agreement_output = "agreement.txt"

def init_dir(folder):
    Path.mkdir(folder, parents=True, exist_ok=True)
