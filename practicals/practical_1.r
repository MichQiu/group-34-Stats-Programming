#Practical_1

setwd("/home/dickon/Documents/WORK/Edinburgh/Statistical_Programming/group-34-Stats-Programming/practicals")
a = scan("1581-0.txt",what="character",skip=156)
n = length(a)
a = a[-((n-2909):n)] ## strip license

# Q4
split_punct = function(words, punctuation) {
	## searches words (a vector) for each word containing the punctuation mark
	## removes it from the word and adds the mark as a new entry in
	## words after the word it came from
	locations = grep(punctuation, words, fixed=TRUE) # indices of words containing punctuation

	words = gsub(punctuation, "", words[locations], fixed=TRUE) 
	# removes punctuation from any words
	
	i = 0
	for (location in locations) {
		# loop: append the punctuation in words after the word in which it appeared
		# i is needed as the position of the following words will be shifted
		# each time a new element is appended
		words = append(words, punctuation, after=location+i)
		i = i + 1
	}
	
	return (words)
}

# Q5
punct = c(",",".",";","!",":","?")
for (p in punct) {
	a = split_punct(a, p) # seperating punctuation in a
}

# Q6
a = tolower(a)
b = unique(a,incomparables=punct) # searches for unique words, ignores punctuation marks
ib = match(a, b) # indices of unique words in vector of words of a

