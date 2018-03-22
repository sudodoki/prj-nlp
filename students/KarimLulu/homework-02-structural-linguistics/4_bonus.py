PREFIX = "shm"
VOWELS =  ('a', 'e', 'i', 'o', 'u')

def shmificate(word, prefix=PREFIX):
    is_capitalized = word[0].isupper()
    word = word.lower()
    if word.startswith(PREFIX):
        return word
    if "sh" in word:
        prefix = "sm"
    i = 0
    for letter in word:
        if letter not in VOWELS:
            i += 1
        else:
            break
    prefix = prefix.capitalize() if is_capitalized else prefix
    output = prefix + word[i:]
    return output

if __name__ == "__main__":
    words = ["table", "apple", "shmaltz", "Ashmont", "truncate"]
    for word in words:
        print(f"{word} - {shmificate(word)}")