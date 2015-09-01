# -*- coding: utf-8 -*-
"""
File: reduce.py

Works in conjunction with: mapper.py (it must be run FIRST)

Input file: wiseMapperOUT.txt (the output of mapper.py)

@author: Nada Basit
"""

import sys
 
# maps words to their counts
word2count = {}
 
# input comes from file
 #--- get all lines from file ---
with open('wiseMapperOUT.txt', 'r') as wiseTxt2: 
    for line in wiseTxt2:
        # --- Remove leading and trailing whitespace
        line = line.strip()        
    
        # --- Parse the input received from mapper.py
        word, count = line.split('\t', 1)
        
        # --- Convert count (currently a string) to int
        try:
            count = int(count)
        except ValueError:
            continue
        
        # --- Do the counting
        try:
            word2count[word] = word2count[word]+count
        except:
            word2count[word] = count
 
# Note: they are unsorted (no shuffle and sorting done in this example)
# --- Redefine stdout (by default it goes to the screen)
sys.stdout = open('reduceOUT.txt', 'a') # open file for appending
for word in word2count.keys():
    print('%s\t%s'% ( word, word2count[word] ))
    