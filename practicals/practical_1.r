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

	new_words <- gsub(punc_reg, "", words) 
	# removes punctuation from any words
	
	i = 0
	for (location in locations) {
		# loop: append the punctuation in words after the word in which it appeared
		# i is needed as the position of the following words will be shifted
		# each time a new element is appended
	  last_char <- nchar(words[location]) # get the index of the last character of the word which is the punctuation
	  punc_insert <- substr(words[location], last_char, last_char) # get the punctuation 
		new_words <- append(new_words, punc_insert, after=location+i)
		i = i + 1
	}
	
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
ib <- match(a, b)
