import argparse
import itertools


vowels = {'a', 'e', 'i', 'o', 'u'}


def shmificate(word):
    """ Shmificates a single word.
    Applies following heuristic:
        - if word starts with vowel
            - if first consonant cluster after the first vowel is 'shm' or 'sh',
              prepend 'sm'
            - else, prepend 'shm'
        - else
            - if first consonant is 'shm', leave word as is
            - else, replace first consonant cluster with 'shm'
    Also keeps track of the first letter capitalization.

    Parameters
    ----------
    word : str
        input word

    Returns
    -------
    result : str
        word after shmification
    """
    first = word[0]
    if first.lower() in vowels:
        consonant = itertools.takewhile(lambda x: x not in vowels, word[1:])
        consonant = ''.join(consonant).lower()
        if consonant == 'shm' or consonant == 'sh':
            new_word = 'sm' + word
        else:
            new_word = 'shm' + word
    else:
        consonant = itertools.takewhile(lambda x: x not in vowels, word)
        consonant = ''.join(consonant).lower()
        if consonant == 'shm':
            new_word = word
        else:
            new_word = 'shm' + word[len(consonant):]
    return new_word.capitalize() if first.isupper() else new_word


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Task 2.4 Shmification')
    parser.add_argument('-i', dest='input', help='Input word or words to shmificate')

    args = parser.parse_args()

    # handle multiple words case, shmificate only last one
    words = args.input.split()
    if len(words) > 2:
        raise ValueError("Can't shmificate phrases with more then two words :(")

    words[-1] = shmificate(words[-1])
    print('Result: ' + ' '.join(words))