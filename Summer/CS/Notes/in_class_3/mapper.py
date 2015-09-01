# -*- coding: utf-8 -*-
"""
File: mapper.py

Works in conjunction with: reduce.py (it must be run AFTER mapper.py)

Input file: wise.txt
Content of file:
too wise you are too wise you be i see you are too wise for me
Output file: wiseMapperOUT.txt  (becomes the input to reduce.py)

@author: Nada Basit
"""

import sys
    
#--- get all lines from file ---
with open('wise.txt', 'r') as wiseTxt: 
    for line in wiseTxt:
        # --- Remove leading and trailing whitespace
        line = line.strip()
        
        # --- Split the line into words
        words = line.split()
        
        # --- Redefine stdout (by default it goes to the screen)
        sys.stdout = open('wiseMapperOUT.txt', 'a') # open file for appending
        
        # --- Output tuples [word, 1] in tab-delimited format
        for word in words:
            print('%s\t%s' % (word, "1"))
