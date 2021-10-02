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
b <- split_punct(a, punct) # separating punctuation in a

# Q6
a <- tolower(a)
b <- unique(a,incomparables=punct) # searches for unique words, ignores punctuation marks
ib <- match(a, b) # indices of unique words in vector of words of a

