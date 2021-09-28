#Practical_1

setwd("/home/dickon/Documents/WORK/Edinburgh/Statistical_Programming/group-34-Stats-Programming/practicals")
a = scan("1581-0.txt",what="character",skip=156)
n = length(a)
a = a[-((n-2909):n)] ## strip license

split_punct = function(words, punctuation) {
	## searches words (a vector) for each word containing the punctuation mark
	## removes it from the word and adds the mark as a new entry in
	## words after the word it came from

}
