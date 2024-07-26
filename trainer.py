import random
import struct
import torch
import torch.nn as nn
import torch.nn.functional as F
from torch.utils.data.dataloader import DataLoader
from torchvision import datasets
import torchvision.transforms.v2 as transforms
import zmq


from compiler import parse_architectures, tokenize, sample_architectures, parse_choices

mnist = datasets.MNIST(
    "~/.data",
    transform=transforms.Compose(
        [transforms.PILToTensor(), transforms.ToDtype(torch.float32, scale=True)]
    ),
    download=True,
)
mnist_dl = DataLoader(mnist, batch_size=10)


def train(model):
    losses = []
    dl = iter(mnist_dl)
    for _ in range(100):
        x, y = next(dl)
        prediction = model(x)
        loss = torch.gather(-F.log_softmax(prediction, dim=1), 1, y.unsqueeze(1)).mean()
        losses.append(loss)
        loss.backward()
        with torch.no_grad():
            for param in model.parameters():
                param -= 0.001 * param.grad
    return losses


def run(architectures):
    results = []
    for arch in architectures:
        model = nn.Sequential(nn.Flatten(), *arch)
        result = train(model)
        results.append(result)


architectures = parse_architectures(tokenize(sample_architectures), 0)
model = nn.Sequential(nn.Flatten(), *architectures[0])


def make_decision(choices):
    return random.randint(0, len(choices) - 1)

# https://learning-0mq-with-pyzmq.readthedocs.io/en/latest/pyzmq/patterns/pair.html
def run_server():
    context = zmq.Context()
    socket = context.socket(zmq.PAIR)
    print("Binding to port 5555")
    socket.bind("tcp://127.0.0.5:5555")
    while True:
        message = socket.recv()
        socket.send(struct.pack("!i", random.randint(0, 1)))

run_server()
