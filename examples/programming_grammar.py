"""
EXPRESSION := OPERATION ( NUMBER NUMBER )
OPERATION := add | sub | mul | div
NUMBER := 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
"""

def expression_generator():
    for expression in operation_generator():
        for number1 in number_generator():
            for number2 in number_generator():
                yield f'{expression} ( {number1} {number2} )'

def operation_generator():
    for operation in ['add', 'sub', 'mul', 'div']:
        yield operation

def number_generator():
    for number in '0123456789':
        yield number

for expression in expression_generator():
    print(expression)
