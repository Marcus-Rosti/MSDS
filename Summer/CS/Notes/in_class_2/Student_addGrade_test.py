# kms6bn Katherine Schinkel
# mer3ef Marcus Rosti
# in class assignment 2

import unittest
from pyScript21 import *

class AddGradeTestCase(unittest.TestCase):
    def test_add_one_grade(self):
        #set up
        test_stud = Student("Marcus",'5010')
        test_stud.addGrade(90)
        test_ans = [90]
        self.assertEqual(test_stud.grades,test_ans)


    def test_add_two_grades(self):
        test_stud_2 = Student("Rusty",'5011')
        test_stud_2.grades=[]
        test_stud_2.addGrade(90)
        test_stud_2.addGrade(80)
        test_ans_2 = [90,80]
        self.assertEqual(test_stud_2.grades,test_ans_2)

if __name__ == '__main__':
    unittest.main()
