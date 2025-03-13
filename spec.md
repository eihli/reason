# Neural Guided Constraint Logic Programming: Implementation Specification

## Project Overview

This project aims to implement a neural network-guided constraint logic programming system for program synthesis, inspired by the paper "Neural Guided Constraint Logic Programming for Program Synthesis." The implementation will focus on simplicity rather than performance, creating a flexible framework that allows experimentation with different neural network architectures and reinforcement learning approaches.

## Architecture

The system follows a client-server architecture:

1. **Racket Server**: Implements first-order miniKanren and handles constraint logic programming 
2. **Python Client**: Implements neural network models and reinforcement learning to guide the search

Communication between the components uses ZeroMQ for message passing.

## Component Specifications

### 1. Racket Server (first-order miniKanren)

#### Functionality
- Expose miniKanren's search process via a ZeroMQ REP socket
- Parse queries received from the Python client
- Expand search choices as directed by the client
- Serialize and return search state in JSON format

#### Interface
- **Endpoint**: `tcp://127.0.0.1:5555`
- **Input Message Format**: JSON with either:
  - `{"query": "(query (a b) (appendo a b '(1 2 3 4)))"}` for initializing a search
  - `{"choice": n}` where n is the index of the choice to expand
- **Output Message Format**: JSON with:
  - `{"results": {...}}` containing found solutions (may be empty)
  - `{"choices": [...]}` containing available choices for expansion

#### Error Handling
- Gracefully handle malformed queries and invalid choice indices
- Implement timeouts to prevent hanging
- Provide clear error messages in responses

### 2. Python Client (Neural Guide)

#### Functionality
- Communicate with the Racket server to guide the search process
- Implement neural network models for scoring choices
- Implement reinforcement learning algorithms to improve the guide
- Track and report experimental metrics

#### Neural Network Architecture
- **Input**: Tokenized constraint representations
- **Embedding Layer**: Map tokens to vector representations
- **Encoder**: Sequence model (LSTM/GRU) to process the constraints
- **Output Layer**: Score for each choice

#### Reinforcement Learning Components
- **State**: Current search state with available choices
- **Action**: Selection of choice to expand
- **Reward**: 
  - Positive when finding solutions that match criteria (e.g., equal length lists)
  - Small negative rewards for each search step to encourage efficiency
  - Larger negative rewards for search branches that fail
- **Algorithm**: Proximal Policy Optimization (PPO) or similar

#### Interface
- **Connection**: ZeroMQ REQ socket to Racket server
- **Training Loop**: Process for iteratively improving the neural guide
- **Evaluation**: Methods to test the guide on unseen problems

## Implementation Plan

### Phase 1: Basic Framework
1. Clean up and simplify the existing code
2. Ensure stable communication between Python and Racket
3. Implement a basic neural network model
4. Create a simple reinforcement learning loop

### Phase 2: Experimentation Framework
1. Add support for different neural network architectures
2. Implement metrics tracking and visualization
3. Create configuration system for experiments
4. Add support for saving/loading models

### Phase 3: Evaluation
1. Implement generalization testing (training on lists of length 4-6, testing on 7-9)
2. Add cross-domain generalization testing
3. Implement comparison with baseline search strategies

## Testing Strategy

### Unit Tests
- Test serialization/deserialization of messages
- Test neural network components in isolation
- Test reinforcement learning components with mock environments

### Integration Tests
- Test communication between Python and Racket
- Test full search process with simple queries
- Test reward calculation with different solution types

### Experiment Tests
- Test generalization to longer lists
- Test performance against non-guided search
- Test learning curves across training iterations

## Dependencies

### Racket
- Racket core libraries
- ZeroMQ bindings for Racket
- First-order miniKanren implementation

### Python
- PyTorch for neural networks
- PyZMQ for ZeroMQ communication
- Tiktoken or similar for tokenization
- NumPy and other standard data science libraries

## Metrics and Evaluation

- Success rate (percentage of problems solved)
- Average number of steps to solution
- Generalization performance (success on unseen problem sizes)
- Learning curve (improvement over training iterations)

## Future Extensions

- Support for more complex program synthesis domains
- Implementation of more sophisticated neural architectures
- Addition of active learning components
- Integration with code generation systems

This specification provides a clear roadmap for implementing a simple but flexible neural guided constraint logic programming system, with a focus on experimentation and extensibility rather than optimized performance.
