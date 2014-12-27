from sets import Set
import string
import math

# A helper function for finding letters in an equation
def find_variables(equation):
    variables = Set()
    for character in equation:
        if character in string.ascii_letters: variables.add(character)
    return sorted(variables)

# Try the first 100 positive integers
def bruteforce(equation, finish = 100, start = 1, epsilon = 0.000001):
    variables = find_variables(equation)
    assignment = [start for variable in variables]
    original_left, original_right = equation.split('==')
    cont = True
    while cont:
        left = original_left
        right = original_right
        for i in range(len(variables)):
            left = string.replace(left, variables[i], str(assignment[i]))
            right = string.replace(right, variables[i], str(assignment[i]))
        numeric_left = eval(left)
        numeric_right = eval(right)
        if numeric_left >= numeric_right - epsilon and numeric_left <= numeric_right + epsilon:
            print variables, '=', assignment
        cont = False
        for i in range(len(assignment) - 1, -1, -1):
            if assignment[i] < finish:
                assignment[i] += 1
                for j in range(len(assignment) - 1, i, -1): assignment[j] = start
                cont = True
                break

# Find a modulus in which the equation doesn't have solutions
def modulus(equation, limit = 100):
    variables = find_variables(equation)
    for modulus in range(2, limit):
        assignment = [0 for _ in variables]
        original_left, original_right = equation.split('==')
        solution = False
        while not solution and not all(x == modulus - 1 for x in assignment):
            left = original_left
            right = original_right
            for i in range(len(variables)):
                left = string.replace(left, variables[i], str(assignment[i]))
                right = string.replace(right, variables[i], str(assignment[i]))
            numeric_left = eval(left)
            numeric_right = eval(right)
            if numeric_left % modulus == numeric_right % modulus:
                solution = True
                break
            for i in range(len(assignment) - 1, -1, -1):
                if assignment[i] < modulus - 1:
                    assignment[i] += 1
                    for j in range(len(assignment) - 1, i, -1):
                        assignment[j] = 0
                    break
        if not solution:
            print modulus
            return


def common_divisor(equation, limit = 100):
    variables = find_variables(equation)
    for modulus in range(2, limit):
        assignment = [0 for _ in variables]
        original_left, original_right = equation.split('==')
        solution = False
        found = False
        while not all(x == modulus - 1 for x in assignment):
            left = original_left
            right = original_right
            for i in range(len(variables)):
                left = string.replace(left, variables[i], str(assignment[i]))
                right = string.replace(right, variables[i], str(assignment[i]))
            numeric_left = eval(left)
            numeric_right = eval(right)
            if numeric_left % modulus == numeric_right % modulus:
                if all(x == 0 for x in assignment):
                    found = True
                else:
                    solution = True
            for i in range(len(assignment) - 1, -1, -1):
                if assignment[i] < modulus - 1:
                    assignment[i] += 1
                    for j in range(len(assignment) - 1, i, -1):
                        assignment[j] = 0
                    break
        if found and not solution:
            print modulus
            return
