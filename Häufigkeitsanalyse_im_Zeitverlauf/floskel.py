# sebsch 26.12.16

import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import re
import os.path

raw_data = pd.read_csv("./share/Data/SPON_complete", delimiter=",", skipinitialspace=True)
floskel_data = pd.read_csv("./share/floskelwolke_basisdaten.csv", delimiter=",", skipinitialspace=True)
floskel_data = floskel_data[floskel_data['Floskel/Phrase'].map( lambda x: len(x.split())) < 2]
datestrings = [".{:02d}.20{:02d}".format(m,y)  for y in range(1, 17) for m in range(1, 13) ]

# Stopwords are downloaded and defined here

with open("german_stopwords_full.txt") as f:
    STOPWORDS = [line.strip() for line in f if not line.startswith(";")] 

dynamic_stopwords = ["dass", "", " ", "worden", "jahren", "jahre", "jahr", 
                     "heißt", "heißen", "müsse", "prozent"]

STOPWORDS += dynamic_stopwords

def count_words(source):
    """ Counting the words of the column article of a given Dataframe.
    
    It is possible to define a word, so only this word will be counted.
    """
    
    #split column Message to new df, create Serie by stack
    s = (source.article.str.split(expand=True).stack().str.lower()   )
    #remove multiindex
    s.index = s.index.droplevel(-1)
    s.name= 'words'
    #join Serie s to df source
    df = (source.join(s))
    
    # Cleaning the strings to be only alphanumeric
    df['words'] = df['words'].map(lambda x: re.sub(r'\W+', '', str(x), re.U))
    
    df = df[~df['words'].isin(
            STOPWORDS)].groupby(
            ['words']).size().reset_index(
            name='count'
    ).sort_values(by='count')
    
    return df

def wordcounter(source, word):
    df = count_words(source)
    return df[df['words'].str.contains(word)]

count_superstrings = lambda d, w : wordcounter(raw_data
                                           [raw_data.day.str.contains(d, na=False)],word=w)

def sumjoin(a, b):
    m = pd.concat([a, b],axis=1)
    m["count"] = m.pop("count").sum(axis=1)
    m['words'] = m['words'].fillna('').sum(axis=1)
    return m.T.groupby(level=0).first().T

def count_words_with_superstringtables_into_csv(word):
    
    count_file = "./share/DATA/Spon_count_floskelwolke_{}.csv".format(word)
    words_file = "./share/DATA/Spon_words_floskelwolke_{}.csv".format(word)
    
    if os.path.exists(count_file) and os.path.exists(words_file):
        return 
    
    
    print("Init: {}".format(word))
    m = None
    absolute_counts = []
    for date in datestrings:
        print("\t{} ... ".format(date))
        if m is None :
            m = count_superstrings(date, word)
            absolute_counts.append(m['count'].sum())
            print("\t\t{}x".format(absolute_counts[-1]))
            continue
        b = count_superstrings(date, word)
        absolute_counts.append(b['count'].sum())
        m = sumjoin(m, b) 
        print("\t\t{}x".format(absolute_counts[-1]))
     
    # Collecting data for absolute and relative counts
    acdf = pd.DataFrame(absolute_counts)
    acdf.columns = [word]
    acdf[word + '_relativ'] = [v/acdf[word].sum() for v in absolute_counts]
    print(acdf)
    acdf.index = datestrings
    
    # Writing Data into csv
    acdf.to_csv(count_file , sep=',')
    m.to_csv(words_file , sep=',')
    
    print("Done: {}".format(word))


def main():
    import html
    html.unescape('Suzy &amp; John')
    words = floskel_data['Floskel/Phrase']
    for word in words:
        count_words_with_superstringtables_into_csv(u'{}'.format(html.unescape(word).lower()))
        
if __name__ == '__main__': main()
        

