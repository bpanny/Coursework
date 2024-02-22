import os
import subprocess
import csv
import re
import random
from scipy.sparse import lil_matrix
import pandas as pd
import numpy as np
import scipy

def read_in_shakespeare():
    """Reads in the Shakespeare dataset and processes it into a list of tuples.
       Also reads in the vocab and play name lists from files.

    Each tuple consists of
    tuple[0]: The name of the play
    tuple[1] A line from the play as a list of tokenized words.

    Returns:
      tuples: A list of tuples in the above format.
      document_names: A list of the plays present in the corpus.
      vocab: A list of all tokens in the vocabulary.
    """

    tuples = []

    with open("shakespeare_plays.csv") as f:
        csv_reader = csv.reader(f, delimiter=";")
        for row in csv_reader:
            play_name = row[1]
            line = row[5]
            line_tokens = re.sub(r"[^a-zA-Z0-9\s]", " ", line).split()
            line_tokens = [token.lower() for token in line_tokens]

            tuples.append((play_name, line_tokens))

    with open("vocab.txt") as f:
        vocab = [line.strip() for line in f]

    with open("play_names.txt") as f:
        document_names = [line.strip() for line in f]

    return tuples, document_names, vocab


def get_row_vector(matrix, row_id):
    """A convenience function to get a particular row vector from a numpy matrix

    Inputs:
      matrix: a 2-dimensional numpy array
      row_id: an integer row_index for the desired row vector

    Returns:
      1-dimensional numpy array of the row vector
    """
    return matrix[row_id, :]


def get_column_vector(matrix, col_id):
    """A convenience function to get a particular column vector from a numpy matrix

    Inputs:
      matrix: a 2-dimensional numpy array
      col_id: an integer col_index for the desired row vector

    Returns:
      1-dimensional numpy array of the column vector
    """
    return matrix[:, col_id]


def create_term_document_matrix(line_tuples, document_names, vocab):
    """Returns a numpy array containing the term document matrix for the input lines.

    Inputs:
      line_tuples: A list of tuples, containing the name of the document and
      a tokenized line from that document.
      document_names: A list of the document names
      vocab: A list of the tokens in the vocabulary

    Let m = len(vocab) and n = len(document_names).

    Returns:
      td_matrix: A mxn numpy array where the number of rows is the number of words
          and each column corresponds to a document. A_ij contains the
          frequency with which word i occurs in document j.
    """
    # YOUR CODE HERE
    m = len(vocab)
    n = len(document_names)
    term_doc = np.zeros(m * n).reshape(m, n)
    term_doc = pd.DataFrame(term_doc, columns=document_names, index=vocab)
    for line in line_tuples:
        document_name = line[0]
        words = line[1]
        for word in words:
            if word in vocab:
              term_doc[document_name][word] += 1

    # smooth and 'scale'
    term_doc = np.log10(term_doc + 1)
    term_doc = term_doc.to_numpy()

    return term_doc

def create_term_context_matrix(line_tuples, vocab, context_window_size=1):
    """Returns a numpy array containing the term context matrix for the input lines.

    Inputs:
      line_tuples: A list of tuples, containing the name of the document and
      a tokenized line from that document.
      vocab: A list of the tokens in the vocabulary

    # NOTE: THIS DOCSTRING WAS UPDATED ON JAN 24, 12:39 PM.

    Let n = len(vocab).

    Returns:
      tc_matrix: A nxn numpy array where A_ij contains the frequency with which
          word j was found within context_window_size to the left or right of
          word i in any sentence in the tuples.
    """
    # YOUR CODE HERE
    context_window_size=1
    n = len(vocab)
    tc_matrix = lil_matrix((n, n), dtype=int)
    vocab_index = dict(zip(vocab, range(0, len(vocab))))


    # inverse_vocab_index = {index: word for word, index in vocab_index.items()}

    def unnest_and_merge(nested_list):
        merged_list = []
        for item in nested_list:
            if isinstance(item, list):
                merged_list.extend(unnest_and_merge(item))
            else:
                merged_list.append(item)
        return merged_list
    
    # make document dictionary
    doc_dict = {}

    for line in tuples:
        doc_name = line[0]
        words = line[1]
        if doc_name in doc_dict:
            doc_dict[doc_name].extend(words)  
        else:
            doc_dict[doc_name] = words 

    for doc in doc_dict.keys():
        doc_dict[doc] = unnest_and_merge(doc_dict[doc])
        doc_length = len(doc_dict[doc])
        context_bag = []
        for word in set(doc_dict[doc]):
            if word not in vocab:
                continue
            word_indexes = [i for i, doc_words in enumerate(doc) if doc_words == word]

            for idx in word_indexes:
                # Check for if word is close to start or end of document, given context_window_size
                if  context_window_size <= idx <= (doc_length - 3):
                    context_bag = doc_dict[doc][(idx - context_window_size):(idx + context_window_size + 1)]
                elif context_window_size > idx: 
                    l_context = context_window_size - (context_window_size - idx)
                    context_bag = doc_dict[doc][(idx - l_context):(idx + context_window_size + 1)]
                elif idx > (doc_length - 1 - context_window_size):
                    r_context = doc_length - idx
                    context_bag = doc_dict[doc][(idx - context_window_size):(idx + r_context + 1)]
                for context_word in context_bag:
                    if context_word not in vocab:
                        continue
                    tc_matrix[vocab_index[word], vocab_index[context_word]] += 1
            
            tc_matrix[vocab_index[word], vocab_index[word]] += -len(word_indexes)
            
    return(tc_matrix.toarray())

def create_tf_idf_matrix(term_document_matrix):
    """Given the term document matrix, output a tf-idf weighted version.

    See section 6.5 in the textbook.

    Hint: Use numpy matrix and vector operations to speed up implementation.

    Input:
      term_document_matrix: Numpy array where each column represents a document
      and each row, the frequency of a word in that document.

    Returns:
      A numpy array with the same dimension as term_document_matrix, where
      A_ij is weighted by the inverse document frequency of document h.
    """


    # YOUR CODE HERE
    tf_idf_matrix = term_document_matrix.copy()
    for word in range(0, td_matrix.shape[0]): 
        tf = get_row_vector(term_document_matrix, word)
        df = sum(tf > 0)
        idf = np.log10(len(document_names) / df)
        tf_idf_matrix[word,:] = tf * idf
    
    return tf_idf_matrix

def create_ppmi_matrix(term_context_matrix):
    """Given the term context matrix, output a PPMI weighted version.

    See section 6.6 in the textbook.

    Hint: Use numpy matrix and vector operations to speed up implementation.

    Input:
      term_context_matrix: Numpy array where each column represents a context word
      and each row, the frequency of a word that occurs with that context word.

    Returns:
      A numpy array with the same dimension as term_context_matrix, where
      A_ij is weighted by PPMI.
    """

    # YOUR CODE HERE
    total_word_count = np.sum(term_context_matrix)
    word_counts = np.sum(term_context_matrix, axis=1) / total_word_count
    context_counts = np.sum(term_context_matrix, axis=0) / total_word_count

    ppmi_matrix = lil_matrix((term_context_matrix.shape[0], term_context_matrix.shape[1]))

    for word in range(term_context_matrix.shape[0]):
        nonzero_contexts = np.nonzero(get_row_vector(term_context_matrix, word))
        for context in nonzero_contexts[0]:
            word_context_p = term_context_matrix[word, context] / total_word_count
            outer_word_context_p = word_counts[word] * context_counts[context]
            if word_context_p == 0.0:
                ppmi_matrix[word, context] = 0.0
            else:
                ppmi_matrix[word, context] = max(np.log2(word_context_p / outer_word_context_p), 0.0)
                
    return(ppmi_matrix)

def compute_cosine_similarity(vector1, vector2):
    """Computes the cosine similarity of the two input vectors.

    Inputs:
      vector1: A nx1 numpy array
      vector2: A nx1 numpy array

    Returns:
      A scalar similarity value.
    """
    # Check for 0 vectors
    if not np.any(vector1) or not np.any(vector2):
        sim = 0

    else:
        sim = 1 - scipy.spatial.distance.cosine(vector1, vector2)

    return sim


def rank_words(target_word_index, matrix):
    """Ranks the similarity of all of the words to the target word using compute_cosine_similarity.

    Inputs:
      target_word_index: The index of the word we want to compare all others against.
      matrix: Numpy matrix where the ith row represents a vector embedding of the ith word.

    Returns:
      A length-n list of integer word indices, ordered by decreasing similarity to the
      target word indexed by word_index
      A length-n list of similarity scores, ordered by decreasing similarity to the
      target word indexed by word_index
    """
    # YOUR CODE HERE
    similarity = []
    if isinstance(matrix, lil_matrix):
      target_vector = matrix[target_word_index].toarray()[0]
      for i in range(matrix.shape[0]): 
          if i == target_word_index:
            continue
          
          # Convert only the row of the matrix that is currently evaluated with i to a numpy array
          tmp_comparison_vector = matrix[i].toarray()[0]
          
          similarity.append(compute_cosine_similarity(target_vector, tmp_comparison_vector))
    else:
      target_vector = matrix[target_word_index, :]
      for i in range(matrix.shape[0]): 
        if i == target_word_index:
          continue
      
        similarity.append(compute_cosine_similarity(target_vector, matrix[i]))

    word_and_sim = list(zip([i for i in range(0, matrix.shape[0]) if i != target_word_index], similarity))

    sorted_word_and_sim = sorted(word_and_sim, key=lambda x: x[1], reverse = True)

    sorted_words = [sorted[0] for sorted in sorted_word_and_sim]
    sorted_sims = [sorted[1] for sorted in sorted_word_and_sim]

    return sorted_words, sorted_sims

if __name__ == "__main__":
    tuples, document_names, vocab = read_in_shakespeare()

    print("Computing term document matrix...")
    td_matrix = create_term_document_matrix(tuples, document_names, vocab)

    print("Computing tf-idf matrix...")
    tf_idf_matrix = create_tf_idf_matrix(td_matrix)


    print("Computing term context matrix...")
    tc_matrix = create_term_context_matrix(tuples, vocab, context_window_size=4)

    print("Computing PPMI matrix...")
    ppmi_matrix = create_ppmi_matrix(tc_matrix)

    # random_idx = random.randint(0, len(document_names) - 1)

    word = "juliet"
    vocab_to_index = dict(zip(vocab, range(0, len(vocab))))

    print(
        '\nThe 10 most similar words to "%s" using cosine-similarity on term-document frequency matrix are:'
        % (word)
    )
    ranks, scores = rank_words(vocab_to_index[word], td_matrix)
    for idx in range(0,10):
        word_id = ranks[idx]
        print("%d: %s; %s" %(idx+1, vocab[word_id], scores[idx]))

    print(
        '\nThe 10 most similar words to "%s" using cosine-similarity on term-context frequency matrix are:'
        % (word)
    )
    ranks, scores = rank_words(vocab_to_index[word], tc_matrix)
    for idx in range(0,10):
        word_id = ranks[idx]
        print("%d: %s; %s" %(idx+1, vocab[word_id], scores[idx]))


    print(
        '\nThe 10 most similar words to "%s" using cosine-similarity on tf-idf matrix are:'
        % (word)
    )
    ranks, scores = rank_words(vocab_to_index[word], tf_idf_matrix)
    for idx in range(0,10):
        word_id = ranks[idx]
        print("%d: %s; %s" %(idx+1, vocab[word_id], scores[idx]))

    print(
        '\nThe 10 most similar words to "%s" using cosine-similarity on PPMI matrix are:'
        % (word)
    )
    ranks, scores = rank_words(vocab_to_index[word], ppmi_matrix)
    for idx in range(0,10):
        word_id = ranks[idx]
        print("%d: %s; %s" %(idx+1, vocab[word_id], scores[idx]))


