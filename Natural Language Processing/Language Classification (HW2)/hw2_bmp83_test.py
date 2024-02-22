import pandas as pd
import numpy as np
from sklearn.feature_extraction.text import CountVectorizer, TfidfVectorizer
from sklearn.model_selection import GridSearchCV
import nltk
from sklearn.linear_model import LogisticRegression
from sklearn.neural_network import MLPClassifier
from sklearn.metrics import accuracy_score, make_scorer, precision_score, recall_score, f1_score
from sklearn.model_selection import cross_validate
from sklearn.pipeline import Pipeline
from sklearn.metrics import confusion_matrix
from sklearn import metrics
import matplotlib.pyplot as plt
import seaborn as sns
from gensim.models import Word2Vec
import sys

def read_csv(filename):
    if filename:
        return(pd.read_csv(filename))
    if not filename:
        print("Something went wrong with feeding in a new data set, you can modify the script manually at line 27 with the new dataset name")


# Call the function with the CSV file name
print('reading new file')
new_data = read_csv(sys.argv[1])

nltk.download('punkt') # this is needed to use NLTK's `word_tokenize` function
nltk.download('stopwords')

print('reading politeness')
polite = pd.read_csv('politeness_data.csv')


print('-'*5 + ' Training Logistic Regression ' + 5*'-')
x=polite.text
y=polite.polite

scoring = {
    'accuracy': make_scorer(accuracy_score),
    'precision': make_scorer(precision_score),
    'recall': make_scorer(recall_score),
    'f1_score': make_scorer(f1_score)
}

pipe_tfidf = Pipeline([('vec',TfidfVectorizer()), 
                       ('lr', LogisticRegression(C = 1, penalty='l2',solver='saga',max_iter=1000))])

pipe_tfidf.fit(x, y)

new_x=new_data.text
print('predicting with LR')
new_pred_y = pipe_tfidf.predict(new_x)

pred_truth_diff = 1 - abs(new_pred_y - new_data['polite'])
n = len(pred_truth_diff)

print(f'The accuracy of the Logistic Regression Model is: {pred_truth_diff.sum() / n} (aka not very good)')

print('-'*5 + ' Training Neural Network ' + 5*'-')

print('preprocessing for NN')
sentences = [nltk.word_tokenize(sentence) for sentence in x]
model = Word2Vec(sentences)
def sentence_to_avg_vector(sentence):
    words = sentence.split()
    word_vectors = [model.wv[word] for word in words if word in model.wv.key_to_index]
    if not word_vectors:
        return np.zeros(model.vector_size)
    return np.mean(word_vectors, axis=0)

polite['text_avg_embedding'] = polite['text'].apply(sentence_to_avg_vector)

print('training NN (for me is < 10 seconds)')

best_params = {'activation': 'relu', 'alpha': 0.0001, 'hidden_layer_sizes': (50, 50, 50), 'learning_rate': 'constant', 'solver': 'adam'}
nn = MLPClassifier(**best_params, max_iter=1000, random_state=42)

nn_x = np.stack(polite['text_avg_embedding'].values)
nn_y = polite['polite'].values

nn.fit(nn_x, nn_y)  # replace X and y with your data

print('predicting with nn')

nn_new_pred_y = pipe_tfidf.predict(new_x)

nn_pred_truth_diff = 1 - abs(nn_new_pred_y - new_data['polite'])
nn_n = len(nn_pred_truth_diff)

print(f'The accuracy of the Neural Network Model is: {nn_pred_truth_diff.sum() / nn_n} (aka not very good)')
