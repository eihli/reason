from itertools import cycle

"""
SENTENCE    := NOUN_PHRASE VERB
NOUN_PHRASE := PROPER_NOUN | ARTICLE NOUN | PRONOUN
ARTICLE     := the NOUN | a NOUN
PROPER_NOUN := Alice | Bob
NOUN        := dog | cat
PRONOUN     := he | she | they | it
VERB        := walk | run
"""

def sentence_generator():
    for noun_phrase in noun_phrase_generator():
        for verb in verb_generator():
            yield f'{noun_phrase} {verb}'

def noun_phrase_generator():
    for proper_noun in proper_noun_generator():
        yield proper_noun
    for article in article_generator():
        yield article
    for pronoun in pronoun_generator():
        yield pronoun

def article_generator():
    for article in ['the', 'a']:
        for noun in noun_generator():
            yield f'{article} {noun}'

def proper_noun_generator():
    for proper_noun in ['Alice', 'Bob']:
        yield proper_noun

def noun_generator():
    for noun in ['dog', 'cat']:
        yield noun

def pronoun_generator():
    for pronoun in ['he', 'she', 'they', 'it']:
        yield pronoun

def verb_generator():
    for verb in ['walk', 'run']:
        yield verb

generate_sentence = sentence_generator()
for sentence in generate_sentence:
    print(sentence)
