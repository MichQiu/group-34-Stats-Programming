#Practical_1

setwd(".")
a <- scan("1581-0.txt",what="character",skip=156)
n <- length(a)
a <- a[-((n-2909):n)] ## strip license

# Q4
split_punct = function(words, punctuation) {
	## searches words (a vector) for each word containing the punctuation marks
	## removes it from the word and adds the mark as a new entry in
	## words after the word it came from
  
  punc <- paste(punctuation, collapse = "") # combine the vector of punctuation in to a string
  punc_reg <- paste("[", punc, "]", sep="", collapse = "") # put punctuation into regex
	locations <- grep(punc_reg, words) # indices of words containing punctuation
  locations_add <- locations+1:length(locations) # add 1 positions for every punctuation that will be inserted into the text
	no_punc <- gsub(punc_reg, "", words) # removes punctuation from any words
	new_words <- rep("", length(words)+length(locations_add)) # create an empty vector with the combined length of words and punctuation
	
	# get the last character(punctuation) from each word with the punctuation and add them at their respective positions
	new_words[locations_add] <- substr(words[locations], nchar(words[locations]), nchar(words[locations])) 
	new_words[-locations_add] <- no_punc # add the the words without the punctuation in non-punctuation indices
	return (new_words)
}

# Q5
punct <- c(",",".",";","!",":","?")
a <- split_punct(a, punct) # separating punctuation in a

# Q6
a <- tolower(a)
unique_words <- unique(a) # searches for unique words, ignores punctuation marks
iu <- match(a, unique_words) # indices of unique words in vector of words of a
count <- tabulate(iu) # count the number of times the unique words appear in the text
threshold <- min(sort(count, decreasing = TRUE)[1:1000]) # find the counts of the top 1000 most commons words and set the minimum as the threshold
b <- rep(0, 1000)
i <- 1
j <- 1
for (word in count){
  if (i == 1000) break # break loop after 1000 iterations
  else if (word >= threshold){ # insert the word to the b vector if its count is larger than the threshold
    b[i] <- unique_words[j] # j is the index of the most common word identified
    i = i + 1 # the index for vector b only increase when there a word is inserted
  }
  j = j + 1 # j increases for every iteration in the for loop to correctly index the word in unique_words and counts in count
}

# Q7
ib <- match(a, b) # get the indices of the most common words that each of the main text corresponds to
if ((length(ib) %% 2) != 0) ib <- head(ib, -1) # check if indices vector is odd, remove last element if condition is satisfied
## create a two column vector, column 1 consist of a sequence of every odd element of the indices vector
## column 2 consist of a sequence of every even element, thus creating word pairs in subsequent order
word_pairs <- cbind(ib[seq(1, length(ib), 2)], ib[seq(2, length(ib), 2)])
wpsum <- rowSums(word_pairs) # get the numerical sums of each row in the word_pairs matrix
check_na <- is.na(wpsum) # return a boolean matrix of NA values in wpsum
non_na_rows <- which(check_na %in% FALSE) # get the indices of all the FALSE(not NA) values
word_pairs <- word_pairs[non_na_rows, 1:2] # redefine word_pairs by getting only the rows with no NA values
A <- array(0, c(1000, 1000))   # initialize a 1000x1000 matrix
# use 'i' as a row index to loop through the word pairs
for (i in c(1:length(word_pairs)/2)){
  # Assign each common words index pair from the two columns on each row to A and add 1 when they occur
  A[word_pairs[i,1], word_pairs[i,2]] <- A[word_pairs[i,1], word_pairs[i,2]] + 1
}
standardize <- rowSums(A) # get the sums of the word pair counts each row
for (i in c(1:1000)){
  A[i,] <- A[i,]/standardize[i] # standardize each row in A into probabilities
}
