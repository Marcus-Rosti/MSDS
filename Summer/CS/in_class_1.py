# Marcus Rosti & Katherine Schinkel 
# mer3ef & kms6bn 
# 15 July 2015
# Python inclass assignment


# import statements
import sys

## 1
# collect names
first_name = input("First Name : ")
last_name  = input("Last Name  : ")
age        = input("Age        : ")

## 2
# clean input
first_name_clean = first_name.strip()
last_name_clean  = last_name.strip()
age_clean        = age.strip()


## 3
# concat names
last_and_first = last_name_clean + "-" + first_name_clean
print(last_and_first)

## 4
# create a dictionary
name_list = {last_and_first:age}

#5
# Function to take names
def take_person(dictionary):
    first       = input("First Name : ").strip()
    last        = input("Last Name  : ").strip()
    the_age     = input("Age        : ").strip()

    last_and_first_name = last + "-" + first

    dictionary[last_and_first_name] = the_age
    return

names = {}
for i in range(0,4):
    take_person(names)

print(names)

#6
# write to file
f = open('myOutput.txt', 'w')
f.write(str(names))

