#EXERCISE -- #
##################

# Create a long sentence of words [assume NO punctuation]
# Put the words into a list (hint, how are the words separated?)
#        (separating the words can be done before the list comprehension)
# Use a list comprehension to return the word along with the length of it
# Use this -->  (word, len(word))   in your list comprehension
# [finally, print out the words along with its length ] - at the end
mysentence = "Hello you shit wad"
my_strip_sent = mysentence.split(" ")
print([(x, len(x)) for x in my_strip_sent])



print("===========================================")
