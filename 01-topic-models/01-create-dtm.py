'''
# 01-create-dtm.py

Date: 6/6/2015
Author: Pablo Barberá

Takes txt files with tweets grouped by chamber and party and prepares
a sparse matrix with unigram + bigram counts per document.

Then applies same tokenizer to user and media corpus

NOTE: running this script requires access to the original corpus of
tweets, which we cannot share publicly in order to comply with Twitter's
terms of service. For advice on how to re-run this script, please
contact the corresponding author.

All other scripts can be run without running this one first, since we
are providing the files required to replicate the document-feature matrices
we use in the analysis.

'''

from twokenize import tokenizeRawTweetText

##################
# reading documents
##################

import os
import re
from unidecode import unidecode

fls = os.listdir('data/tweets/text')

f = open('data/dfm/fls-list.txt', 'w')
for item in fls:
  f.write("%s\n" % item.replace('.txt', ''))

f.close()

docs = []

for f in fls:
	print f
	f = 'data/tweets/text/' + f
	lines = open(f).readlines()
	doc = " ".join(lines).replace("\n", "")
	#text = re.sub(r'http\S+|RT|&amp;|!|"|\?|\"|--|:|\.|(|)|;|\,|\+', "", doc)
	text = re.sub(r'http\S+|RT|&amp;|,|\.|\xe2\x80\xa6|-', "", doc)
	text = unidecode(text.decode('utf-8'))
	#text = re.sub(r'http\S+|RT|&amp;|!|"|\?|\"|--|:|\.|(|)|;|\,|,|\+', "", text)
	docs.append(text)


####################################
# creating matrix for congress
####################################

print 'Members of Congress'

from sklearn.feature_extraction.text import CountVectorizer
import numpy as np


def tokenizer(text):
	doc = tokenizeRawTweetText(text)
	doc = [d for d in doc if len(d)>2]
	return(doc)

vectorizer = CountVectorizer(stop_words='english', tokenizer=tokenizer,
    max_features=75000, ngram_range=(1,2), 
    min_df=10, max_df=.90)

X = vectorizer.fit_transform(docs)

# checking words
words = vectorizer.get_feature_names()

####################################
# exporting DFM matrix
####################################

np.savetxt('data/dfm/congress-dtm-indices.txt', X.indices, fmt='%.0f')
np.savetxt('data/dfm/congress-dtm-pointers.txt', X.indptr, fmt='%.0f')
np.savetxt('data/dfm/congress-dtm-values.txt', X.data, fmt='%.0f')

## words
words = vectorizer.get_feature_names()
f = open('congress-words.txt', 'w')
for item in words:
  f.write("%s\n" % item)

f.close()

####################################
# new text: Members of Congress
####################################

print 'Members of Congress, one by one'

# reading text
fls = os.listdir('data/tweets/congress_text')
docs = []

# parsing text
for f in fls:
	print f
	f = 'data/tweets/congress_text/' + f
	lines = open(f).readlines()
	doc = " ".join(lines).replace("\n", "")
	text = re.sub(r'http\S+|RT|&amp;|,|\.|\xe2\x80\xa6|-', "", doc)
	text = unidecode(text.decode('utf-8'))
	docs.append(text)

# feeding to vectorizer
Xnew = vectorizer.transform(docs)

# exporting

np.savetxt('data/dfm/mcs-dtm-indices.txt', Xnew.indices, fmt='%.0f')
np.savetxt('data/dfm/mcs-dtm-pointers.txt', Xnew.indptr, fmt='%.0f')
np.savetxt('data/dfm/mcs-dtm-values.txt', Xnew.data, fmt='%.0f')

f = open('data/dfm/mcs-list.txt', 'w')
for item in fls:
  f.write("%s\n" % item.replace('.txt', ''))

f.close()

####################################
# new text: random sample of tweets
####################################

print 'Random sample of tweets'

# reading from file
import csv
f = open('data/tweets/tweets-random-sample.csv', 'r')
reader = csv.reader(f)
reader.next()
docs = []
for row in reader:
	txt = row[3].replace('\n','')
	txt = re.sub(r'http\S+|RT|&amp;|,|\.|\xe2\x80\xa6|-', "", txt)
	txt = unidecode(txt.decode('utf-8'))
	docs.append(txt)

# feeding to vectorizer
Xnew = vectorizer.transform(docs)

# exporting
np.savetxt('data/dfm/rs-dtm-indices.txt', Xnew.indices, fmt='%.0f')
np.savetxt('data/dfm/rs-dtm-pointers.txt', Xnew.indptr, fmt='%.0f')
np.savetxt('data/dfm/rs-dtm-values.txt', Xnew.data, fmt='%.0f')


####################################
# new text: media tweets
####################################

print 'Media tweets'

# reading text
fls = os.listdir('data/tweets/media_text')
docs = []

# parsing text
for f in fls:
	print f
	f = 'data/tweets/media_text/' + f
	lines = open(f).readlines()
	doc = " ".join(lines).replace("\n", "")
	text = re.sub(r'http\S+|RT|&amp;|,|\.|\xe2\x80\xa6|-', "", doc)
	text = unidecode(text.decode('utf-8'))
	docs.append(text)

# feeding to vectorizer
Xnew = vectorizer.transform(docs)

# exporting

np.savetxt('data/dfm/media-tweets-dtm-indices.txt', Xnew.indices, fmt='%.0f')
np.savetxt('data/dfm/media-tweets-dtm-pointers.txt', Xnew.indptr, fmt='%.0f')
np.savetxt('data/dfm/media-tweets-dtm-values.txt', Xnew.data, fmt='%.0f')

f = open('data/dfm/media-tweets-list.txt', 'w')
for item in fls:
  f.write("%s\n" % item.replace('.txt', ''))

f.close()


###########################################
# new text: random sample of media tweets
###########################################

print 'Random sample of media tweets'

# reading from file
import csv
f = open('data/tweets/media-tweets-random-sample.csv', 'r')
reader = csv.reader(f)
reader.next()
docs = []
for row in reader:
	txt = row[3].replace('\n','')
	txt = re.sub(r'http\S+|RT|&amp;|,|\.|\xe2\x80\xa6|-', "", txt)
	txt = unidecode(txt.decode('utf-8'))
	docs.append(txt)

# feeding to vectorizer
Xnew = vectorizer.transform(docs)

# exporting
np.savetxt('data/dfm/media-rs-dtm-indices.txt', Xnew.indices, fmt='%.0f')
np.savetxt('data/dfm/media-rs-dtm-pointers.txt', Xnew.indptr, fmt='%.0f')
np.savetxt('data/dfm/media-rs-dtm-values.txt', Xnew.data, fmt='%.0f')


####################################
# new text: sample of users
####################################

print 'User samples'

# reading text
fls = os.listdir('data/tweets/users')
docs = []

# parsing text
for f in fls:
	print f
	f = 'data/tweets/users/' + f
	lines = open(f).readlines()
	doc = " ".join(lines).replace("\n", "")
	text = re.sub(r'http\S+|RT|&amp;|,|\.|\xe2\x80\xa6|-', "", doc)
	text = unidecode(text.decode('utf-8'))
	docs.append(text)

# feeding to vectorizer
Xnew = vectorizer.transform(docs)

# exporting

np.savetxt('data/dfm/users-dtm-indices.txt', Xnew.indices, fmt='%.0f')
np.savetxt('data/dfm/users-dtm-pointers.txt', Xnew.indptr, fmt='%.0f')
np.savetxt('data/dfm/users-dtm-values.txt', Xnew.data, fmt='%.0f')

f = open('data/dfm/users-list.txt', 'w')
for item in fls:
  f.write("%s\n" % item.replace('.txt', ''))

f.close()


####################################
# new text: random sample of users
####################################

print 'Random sample of users'

# reading text
fls = os.listdir('data/tweets/random_tweets')
docs = []

# parsing text
for f in fls:
	print f
	f = 'data/tweets/random_tweets/' + f
	lines = open(f).readlines()
	doc = " ".join(lines).replace("\n", "")
	text = re.sub(r'http\S+|RT|&amp;|,|\.|\xe2\x80\xa6|-', "", doc)
	text = unidecode(text.decode('utf-8'))
	docs.append(text)

# feeding to vectorizer
Xnew = vectorizer.transform(docs)

# exporting

np.savetxt('data/dfm/random-users-dtm-indices.txt', Xnew.indices, fmt='%.0f')
np.savetxt('data/dfm/random-users-dtm-pointers.txt', Xnew.indptr, fmt='%.0f')
np.savetxt('data/dfm/random-users-dtm-values.txt', Xnew.data, fmt='%.0f')

f = open('data/dfm/random-users-list.txt', 'w')
for item in fls:
  f.write("%s\n" % item.replace('.txt', ''))

f.close()

