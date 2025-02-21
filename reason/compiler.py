import re
import sys
import torch.nn as nn

sample_architectures = """
(((IN 784) (Linear 784 10) (OUT 10))
 ((IN 784) (Linear 784 8)  (Linear 8 10)      (OUT 10))
 ((IN 784) (Linear 784 9)  (Linear 9 10)      (OUT 10))
 ((IN 784) (Linear 784 8)  (Linear 8 8)       (Linear 8 10) (OUT 10))
 ((IN 784) (Linear 784 8)  (Relu 8 8)         (Linear 8 10) (OUT 10))
 ((IN 784) (Linear 784 9)  (Linear 9 8)       (Linear 8 10) (OUT 10))
 ((IN 784) (Linear 784 10) (Linear 10 10)     (OUT 10))
 ((IN 784) (Linear 784 8)  (Dropout 8 8 99)   (Linear 8 10) (OUT 10))
 ((IN 784) (Linear 784 10) (Relu 10 10)       (OUT 10))
 ((IN 784) (Linear 784 10) (Dropout 10 10 99) (OUT 10))
 ((IN 784) (Linear 784 10) (Dropout 10 10 0)  (OUT 10))
 ((IN 784) (Linear 784 10) (Dropout 10 10 1)  (OUT 10))
 ((IN 784) (Linear 784 8)  (Linear 8 8)       (Linear 8 8)  (Linear 8 10) (OUT 10))
 ((IN 784) (Linear 784 10) (Dropout 10 10 98) (OUT 10))
 ((IN 784) (Linear 784 8)  (Dropout 8 8 0)    (Linear 8 10) (OUT 10))
 ((IN 784) (Linear 784 10) (Linear 10 8)      (Linear 8 10) (OUT 10))
 ((IN 784) (Linear 784 10) (Dropout 10 10 3)  (OUT 10))
 ((IN 784) (Linear 784 10) (Dropout 10 10 2)  (OUT 10))
 ((IN 784) (Linear 784 9)  (Linear 9 9)       (Linear 9 10) (OUT 10))
 ((IN 784) (Linear 784 11) (Linear 11 10)     (OUT 10)))
"""

def tokenize(text):
    text = re.sub(r"\s+", " ", text, flags=re.MULTILINE).strip()
    tokens = re.findall(r"\(|\)|\w+", text)
    return tokens

L_PAREN = "("
R_PAREN = ")"
LAYER_IN = "IN"
LAYER_OUT = "OUT"
LAYER_LINEAR = "Linear"
LAYER_DROPOUT = "Dropout"
LAYER_RELU = "Relu"

def expect(tokens, i, expected):
    if i > len(tokens):
        raise Exception(f"Expected {i} to be less than length of tokens, {len(tokens)} at {tokens[-3:]}")
    if tokens[i] != expected:
        raise Exception(f"Expected {expected} but got {tokens[i]} at {i}")

def expance(tokens, i, expected):
    expect(tokens, i, expected)
    return i + 1

def check(tokens, i, expected):
    try:
        expect(tokens, i, expected)
    except:
        return False
    return True

def parse_architectures(tokens, i):
    i = expance(tokens, i, L_PAREN)
    architectures = []
    while i < len(tokens) and check(tokens, i, L_PAREN):
        architecture, i = parse_architecture(tokens, i)
        architectures.append(architecture)
    i = expance(tokens, i, R_PAREN)
    return architectures

def parse_architecture(tokens, i):
    layers = []
    i = expance(tokens, i, L_PAREN)
    while check(tokens, i, L_PAREN):
        layer, i = parse_layer(tokens, i)
        layers.append(layer)
    i = expance(tokens, i, R_PAREN)
    return nn.Sequential(*layers), i

def parse_layer(tokens, i):
    check(tokens, i, L_PAREN)
    if check(tokens, i+1, LAYER_IN):
        layer, i = parse_in(tokens, i)
    elif check(tokens, i+1, LAYER_OUT):
        layer, i = parse_out(tokens, i)
    elif check(tokens, i+1, LAYER_LINEAR):
        layer, i = parse_linear(tokens, i)
    elif check(tokens, i+1, LAYER_DROPOUT):
        layer, i = parse_dropout(tokens, i)
    elif check(tokens, i+1, LAYER_RELU):
        layer, i = parse_relu(tokens, i)
    else:
        raise Exception(f"Expected layer at {i} but got {tokens[i:i+5]}")
    return layer, i

def parse_in(tokens, i):
    i = expance(tokens, i, L_PAREN)
    i = expance(tokens, i, LAYER_IN)
    dims, i = parse_dims(tokens, i)
    i = expance(tokens, i, R_PAREN)
    return nn.Identity(dims=dims), i

def parse_dims(tokens, i):
    try:
        int(tokens[i])
    except:
        raise Exception(f"Expected dims to be an int, but got {tokens[i]}")
    return int(tokens[i]), i + 1

def parse_out(tokens, i):
    i = expance(tokens, i, L_PAREN)
    i = expance(tokens, i, LAYER_OUT)
    dims, i = parse_dims(tokens, i)
    i = expance(tokens, i, R_PAREN)
    return nn.Identity(dims=dims), i

def parse_linear(tokens, i):
    i = expance(tokens, i, L_PAREN)
    i = expance(tokens, i, LAYER_LINEAR)
    in_dims, i = parse_dims(tokens, i)
    out_dims, i = parse_dims(tokens, i)
    i = expance(tokens, i, R_PAREN)
    return nn.Linear(in_dims, out_dims), i

def parse_dropout(tokens, i):
    i = expance(tokens, i, L_PAREN)
    i = expance(tokens, i, LAYER_DROPOUT)
    in_dims, i = parse_dims(tokens, i)
    out_dims, i = parse_dims(tokens, i)
    percent, i = parse_percent(tokens, i)
    i = expance(tokens, i, R_PAREN)
    return nn.Dropout(percent / 100), i

def parse_percent(tokens, i):
    try:
        int(tokens[i])
    except:
        raise Exception(f"Expected dims to be an int, but got {tokens[i]}")
    return int(tokens[i]), i + 1

def parse_relu(tokens, i):
    i = expance(tokens, i, L_PAREN)
    i = expance(tokens, i, LAYER_RELU)
    in_dims, i = parse_dims(tokens, i)
    out_dims, i = parse_dims(tokens, i)
    i = expance(tokens, i, R_PAREN)
    return nn.ReLU(), i


#### Choices
def parse_choices(tokens, i):
    choices = []
    i = expance(tokens, i, L_PAREN)
    while i < len(tokens) and check(tokens, i, L_PAREN):
        choice, i = parse_choice(tokens, i)
        choices.append(choice)
    i = expance(tokens, i, R_PAREN)
    return choices

def parse_choice(tokens, i):
    i = expance(tokens, i, L_PAREN)
    while tokens[i] != R_PAREN:
        i += 1
    i = expance(tokens, i, R_PAREN)
    return "some_choice", i

def test_parse_choices():
    message = "((Linear 1 2) (Linear 2 1))"
    tokens = tokenize(message)
    assert len(parse_choices(tokens, 0)) == 2

def demo():
    print(parse_architectures(tokenize(sample_architectures), 0))

if __name__ == "__main__":
    print(parse_architectures(tokenize(sys.stdin.read()), 0))
