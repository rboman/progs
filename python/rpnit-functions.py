#! /usr/bin/env python

"""
RPNit is a simple calculator based upon reverse polish notation.

(C)2001 Romain Guy
romain.guy@jext.org
www.jext.org
GNU/GPL Licensed

Although this program does not offer any interesting function, beginners should give it a look.
This calculator makes use of a list of classes, which all inherits from Operator, to perform
computations.

This program was my very first attempt of Python programming :-)
"""

##
## IMPORTS STATEMENTS
##
# sin() and cos()
import math
import re
import sys

def compute_cos(ops):
    if len(ops) == 1:
        return math.cos(ops[0])
    else:
        return None

def compute_division(ops):
    if len(ops) == 2:
        if ops[0] == 0:
            return None
        else:
            return ops[1] / ops[0]
    else:
        return None

def compute_minus(ops):
    if len(ops) == 2:
        return ops[1] - ops[0]
    else:
        return None

def compute_multiply(ops):
    if len(ops) == 2:
        return ops[1] * ops[0]
    else:
        return None

def compute_plus(ops):
    if len(ops) == 2:
        return ops[1] + ops[0]
    else:
        return None

def compute_sin(ops):
    if len(ops) == 1:
        return math.sin(ops[0])
    else:
        return None

# grammar
_nb_ = "[-]?\d+[\.]?\d*"
_op_ = {"cos" : (1, compute_cos),   "/"   : (2, compute_division), \
        "-"   : (2, compute_minus), "*"   : (2, compute_multiply), \
        "+"   : (2, compute_plus),  "sin" : (1, compute_sin)}

# performs the computation
def compute(line):
    # the stack
    stack = []
    # tokens
    tokens = line.split()
    # state
    error = 0

    # stack pushes
    for token in tokens:
        # if the token is a number
        if re.match(_nb_, token) != None:
            try:
                stack.append(float(token))
            except:
                # unknown operator
                print "Invalid number:", token
                error = 1
        else:
            # we check if the token is an operator
            try:
                op, count = _op_[token][1], _op_[token][0]
                # it is a valid operator
                opList = []

                # we check if the stack contains enough elements
                if len(stack) >= count:
                    # we pops the requested amount of operands
                    for i in range(count):
                        opList.append(stack.pop())
                    # compute
                    result = op(opList)
                else:
                    result = None
                    print count - len(stack), "operand(s) is(are) missing"

                # check result
                if result:
                    stack.append(result)
                else:
                    # error during calcul
                    print "Following operation caused an error:", token
                    error = 1
            except:
                # unknown operator
                print "Unknown keyword:", token
                error = 1

        if error:
            break
    else:
        print "Result:", stack.pop()

# main
def main():
    print "\n", sys.argv[0], "is an RPN computation program."
    print """
(C)2001 Romain Guy
Programmed using Python 2.1/IDLE GUI/Jext 2.12 from python.org and jext.org
"""
    # RPNit can perform calculs from an argument or
    # from the standard input pipe
    if len(sys.argv) < 2:
        sys.stdout.write("> ")
        compute(sys.stdin.readline())
    else:
        compute(sys.argv[1])

##
## MAIN ENTRY POINT
##
if __name__ == '__main__':
    main()

# End of rpnit.py
