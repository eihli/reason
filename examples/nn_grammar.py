"""
NETWORK := IN LAYERS OUT
IN := nn.Embedding(n_input, n_embed)
OUT := nn.Linear(n_hidden, n_output)
LAYERS := LAYER LAYERS | LAYER ϵ
LAYER := nn.Linear | nn.Dropout | nn.ReLU | nn.Conv2d
"""

def network_o(n_input, n_embed, n_hidden, n_output):
    in_layer = in_o(n_input, n_embed)
    layers = layers_o()
    out_layer = out_o()
    while True:
        result = [
            in_layer,
            next(layers)
            out_layer
        ]

def in_o(n_input, n_embed):
    yield f'nn.Embedding({n_input}, {n_embed})'

def layers_o(n_embed, n_hidden, n_dropout):
        yield next(linear_o(n_embed, n_hidden))
        yield next(dropout_o(n_dropout))
        yield next()


# If this layer is immediately after the embedding layer,
# then n_in will be n_embed. Otherwise, n_in and n_out
# will both be n_hidden.
def linear_o(n_in, n_out):
    yield f'nn.Linear({n_in}, {n_out})'

def dropout_o(n_dropout):
    yield f'nn.Dropout({n_dropout})'

def out_o(n_hidden, n_output):
    yield f'nn.Linear({n_hidden}, {n_output})'
