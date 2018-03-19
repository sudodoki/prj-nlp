from pathlib import Path

log_format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'

root_dir = Path(__file__).resolve().parent
posts_dir = root_dir / "posts"

def init_dir(folder):
    Path.mkdir(folder, parents=True, exist_ok=True)