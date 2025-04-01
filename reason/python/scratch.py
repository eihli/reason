import torch.nn as nn

class MyModel(nn.Module):
    def __init__(self, hidden_dims):
        flatten = nn.Flatten()
        hidden_layer = nn.Linear(768, hidden_dims)
        output_layer = nn.Linear(hidden_dims, 10)

        self.layers = nn.Sequential(
            flatten,
            hidden_layer,
            output_layer
        )

    def forward(self, x):
        return self.layers(x)
