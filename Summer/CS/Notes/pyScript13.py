# File: pyScript13.py
# CS 5010
# Learning Python (Python version: 3)
# Topics:
#   - List comprehensions

## List comprehensions
# Are essentially short-cuts for for-loops

# Let's say I have a list of numbers (grades) and I want to curve those grades
# Traditional way to do it would be to use a for-loop as follows:

grades = [71, 81, 77, 84]
print(grades)
print("Using a for-loop:")
for i in range(len(grades)):  # range(4) means indicies 0 through 3
   grades[i] = grades[i] + 5  # add 5 to each grade
print(grades)

print("--------------")

# The idea of list comprehensions is to take the two-line for-loop and
# shorten/simplify it - using ideas of for-loops and iterators
# (Almost doing a for-loop backwards)
grades = [71, 81, 77, 84]
print(grades)
print("Using list comprehension:")
grades = [grade + 5 for grade in grades]
#           -2-         -1-

# 1: for each grade in grades
# 2: add 5 to it  [the action on each element]
# Once done, take the result of doing this for all elements and
# assign it back to the variable (list) grades
print(grades)

print("===========================================")

# -- List comprehension using a list of words
words = ["CONVERTING", "TO", "LOWERCASE", "LETTERS"]
print(words)
words = [word.lower() for word in words]  # using lower() function
print(words)  # after processing, should be all lower case

print("===========================================")

# -- List comprehensions with files

# This example demonstrates how list comprehensions can save a lot of time
# The list of grades are stored as strings (since stored in txt file)
# Each grade has a new line character attached to it 
#  ['90\n', '77\n', '85\n', '65\n', '100\n']
# Need to strip the new line characters off

# Traditional way:
print("Traditional way:")
file = open('w2filegrades.txt')
grades = file.readlines()
print(grades)
for i in range(len(grades)):
   grades[i] = grades[i].rstrip()
print(grades)

print("--------------")

print("Using list comprehensions:")
grades = [grade.rstrip() for grade in open('w2filegrades.txt')]
print(grades)

print("===========================================")

# -- Practical examples of list comprehensions
# Find all the even numbers in a range
print("Finding all even numbers:")
N = range(1,101)
EN = [x for x in N if x % 2 == 0]  # Can use an if-statement
print(EN)

# pull out x, for x in N, if x mod 2 == 0 (even)
# Can use the if-statement what will return x if this condition is met

print("===========================================")

##################
# -- EXERCISE -- #
##################

# Create a long sentence of words [assume NO punctuation]
# Put the words into a list (hint, how are the words separated?)
#        (separating the words can be done before the list comprehension)
# Use a list comprehension to return the word along with the length of it
# Use this -->  (word, len(word))   in your list comprehension
# [finally, print out the words along with its length ] - at the end

print("===========================================")
  