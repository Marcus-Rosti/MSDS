# File: pyScript21.py
# CS 5010
# Learning Python (Python version: 3)
# Topics:
#   - Adding to the Student class (see pyScript19.py)

class Student:
   #fields - name, id, grades(list)
   grades = []
   def __init__(self, name, id):
      self.name = name
      self.id = id

   def addGrade(self, grade):
      self.grades.append(grade)

   def showGrades(self):
      grds = ''
      for grade in self.grades:
         grds += str(grade) + ' '
      return grds

   def __str__(self):  # Adding the to-string method
       # printing as student will print the following three lines
      return "Name: " + self.name + "\n" + \
             "Id: " + self.id + "\n" + \
             "Grades: " + self.showGrades()

   def average(self):  # Adding an average method to compute the avg grade
      total = 0
      for grade in self.grades:  # remember to use  self.grades
         total += grade
      return total / len(self.grades)
