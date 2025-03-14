"""Neural Network Guided Search for Constraint Logic Programming.

This module implements a neural network-based scoring system to guid the search
process in a constraint logic programming system (like miniKanren). The system
works by evaluating and scoring possible search paths (candidate constraints)
and using these scores to guide the exploration of the search space.

Core Functionality:
-----------------
1. Parse search states from a text-based representation containing:
   - Results: Previously found solutions (if any)
   - Choices: Candidate constraints to explore next

2. For each choice in the search state:
   - Tokenize the constraint representation
   - Pass the tokens through a neural network model to get a score
   - Normalize scores across all choices using softmax

3. Select a choice to explore using either:
   - Greedy selection (highest scoring choice)
   - Weighted random sampling based on softmax probabilities

4. Continue the search process by expanding the selected choice
   - When a solution is found (appears in results), calculate reward
   - Use this reward signal to update the neural network via reinforcement learning

Implementation Details:
---------------------
- Tokenization uses the tiktoken library with the cl100k_base encoding
- The search state is represented as S-expressions in a Lisp-like format
- The neural model should be trained to recognize promising constraint patterns
- Reinforcement learning is used to improve model performance over time

Example Input (partial candidates):
---------------------
'(search-state
  (results)
  (choices
   (choice (state (vars (a 1 2) (b 3 4))) (goal (conj (== () ()) (== (3 4) (3 4)))))
   (choice
    (state (vars (a 1 2 . #s(var b 20)) (b . #s(var b 15))))
    (goal
     (conj
      (conj (relate (conso #s(var a 22) #s(var b 23) #s(var b 20))) (relate (conso #s(var a 22) #s(var bc 24) (3 4))))
      (relate (appendo #s(var b 23) #s(var b 15) #s(var bc 24))))))))

Example Input (result):
---------------------
'(search-state (results (state (vars (a 1) (b 2 3 4)))) (choices))

Example Input (dead end):
---------------------
'(search-state (results) (choices))

Description of Inputs
---------------------

Each choice in the lsit of partial candidates (choices) will get tokenized and
scored separately.

    import tiktoken
    enc = tiktoken.get_encoding("cl100k_base")
    tokens = [enc.tokenize(choice) for choice in choices]

Then, we'll softmax over the scores and use weighted random sampling to select a
choice to explore/expand.

When we get a result, we'll pass the result to a scoring function. That reward
(or penalty) will get propagated back to the model,
reinforcement-learning-style.

Reinforcement Learning Implementation Details:
---------------------
1. Policy Network:
   - Implement a neural network that takes tokenized constraint representations as input
   - The network outputs a score for each choice representing the likelihood it leads to a solution
   - The policy is defined by the softmax over these scores

2. Reward Structure:
   - Positive reward when a solution is found (proportional to solution quality or inverse of search depth)
   - Small negative reward for each step without finding a solution (encourages efficiency)
   - Larger negative reward for search branches that fail completely

3. Training Algorithm:
   - Use Proximal Policy Optimization (PPO) or Advantage Actor-Critic (A2C)
   - Maintain an experience buffer storing (state, action, reward, next_state) tuples
   - Periodically update the policy network using batches from this buffer
   - Employ entropy regularization to encourage exploration of diverse search paths

4. Optimization Strategy:
   - Use mini-batch gradient descent with Adam optimizer
   - Implement learning rate scheduling to stabilize training
   - Add value function estimation to reduce variance in policy updates
   - Employ gradient clipping to prevent exploding gradients

5. Evaluation Metrics:
   - Track average steps to solution
   - Measure success rate across different problem instances
   - Compare performance against baseline search strategies (random, heuristic-based)
   - Monitor entropy of policy distributions to ensure adequate exploration

"""
from typing import Optional

from collections import deque
import logging
import random
import struct

import numpy as np
import tiktoken
import torch
import torch.nn.functional as F
from torch import Tensor, nn
from torch.types import Number
import zmq

logger = logging.getLogger(__name__)

class ConstraintScoringNetwork(nn.Module):
    def __init__(self, n_vocab, n_embed, n_hidden):
        super().__init__()
        self.embedding = nn.Embedding(n_vocab, n_embed)
        self.gru = nn.GRU(n_embed, n_hidden, batch_first=True)
        self.fc = nn.Linear(n_hidden, 1)

    def forward(self, x: Tensor, lengths=None):
        embedded = self.embedding(x)
        if lengths is not None:
            # Pack padded sequence for more efficient processing
            packed = nn.utils.rnn.pack_padded_sequence(
                embedded, lengths.cpu(), batch_first=True, enforce_sorted=False
            )
            _, hidden = self.gru(packed)
        else:
            _, hidden = self.gru(embedded)
        score = self.fc(hidden.squeeze(0))
        return score

def test_gru():
    enc = tiktoken.get_encoding("cl100k_base")
    n_embed = 128
    n_hidden = 256
    model = ConstraintScoringNetwork(enc.n_vocab, n_embed, n_hidden)

    tokens = enc.encode("(choice (state (vars (a 1 2) (b 3 4))) (goal (conj (== () ()) (== (3 4) (3 4)))))")
    tokens = torch.tensor(tokens).unsqueeze(0)
    model(tokens)
    return model

class ExperienceBuffer:
    def __init__(self, buffer_size=10000):
        self.buffer = deque(maxlen=buffer_size)

    def add(self, state, action, reward, next_state, done):
        self.buffer.append((state, action, reward, next_state, done))

    def sample(self, batch_size):
        return random.sample(self.buffer, min(len(self.buffer), batch_size))

    def __len__(self):
        return len(self.buffer)

def preprocess_constraints(tokenizer: tiktoken.Encoding, choices: list[str]) -> tuple[Tensor, Tensor]:
    tokenized = [tokenizer.encode(choice) for choice in choices]
    lengths  = [len(tokens) for tokens in tokenized]
    padded = [tokens + [0] * (max(lengths) - len(tokens)) for tokens in tokenized]
    return torch.tensor(padded), torch.tensor(lengths)

def select_action(model: nn.Module, tokenizer: tiktoken.Encoding, choices: list[str], ε=0.1) -> int:
    if not choices:
        return -1  # No choices available
        
    tokens, lengths = preprocess_constraints(tokenizer, choices)

    if random.random() < ε:
        return random.randint(0, len(choices) - 1)

    with torch.no_grad():
        scores = model(tokens, lengths)

    probs = F.softmax(scores, dim=0)
    action = torch.multinomial(probs.squeeze(1), num_samples=1).to(torch.int).item()
    return int(action)

def train_model(model, tokenizer, opt, exp_buf, bs=64, epochs=10):
    if len(exp_buf) < bs:
        return 0
    total_loss = 0
    # For each epoch:
    #   Sample states, actions, rewards, next_states, and dones from the experience replay buffer
    #
    for _ in range(epochs):
        batch = exp_buf.sample(bs)
        states, actions, rewards, next_states, dones = zip(*batch)
        batch_loss = torch.tensor(0, dtype=torch.float)
        for i in range(len(states)):
            choices = states[i]
            action = actions[i]
            reward = rewards[i]
            tokens, lengths = preprocess_constraints(tokenizer, choices)
            scores = model(tokens, lengths)
            probs = F.softmax(scores, dim=0).squeeze(1)
            action_prob = probs[action]
            log_prob = torch.log(action_prob + 1e-10)
            loss = -log_prob * reward
            batch_loss += loss
        batch_loss = batch_loss / len(states)
        opt.zero_grad()
        batch_loss.backward()
        opt.step()
        total_loss += batch_loss.item()
    return total_loss / epochs

def calc_reward(result, steps_taken, max_steps=200):
    if result:
        efficiency_modifier = (max_steps - steps_taken) / max_steps
        mean_val_lengths = sum([len(x) for x in result.items()]) / len(result)
        diffs = [(len(x) - mean_val_lengths)**2 for x in result.items()]
        len_modifier = 1 / (sum(diffs) + 0.5)
        base_reward = 10.0
        return (base_reward / efficiency_modifier) * len_modifier
    if steps_taken >= max_steps:
        return -5.0
    else:
        return -0.1 * steps_taken

def run_guided_search(model, tokenizer, env, num_episodes=100, max_steps=200, lr=1e-3, γ=0.99, ε_start=1.0, ε_end=0.1):
    exp_buf = ExperienceBuffer()
    opt = torch.optim.AdamW(model.parameters(), lr=lr)

    metrics = {
        'success_rate': [],
        'avg_steps': [],
        'rewards': [],
        'losses': [],
    }

    ε_decay = (ε_start - ε_end) / (num_episodes * 0.7)
    ε = ε_start

    for episode in range(num_episodes):
        state = env.reset()
        ep_reward = 0
        result = None
        
        for step in range(max_steps):
            choices = state.get('choices', [])
            if not choices:
                break
                
            action = select_action(model, tokenizer, choices, ε)
            next_state, result, done = env.step(action)
            reward = calc_reward(result, step+1, max_steps)
            ep_reward += reward
            exp_buf.add(choices, action, reward, next_state.get('choices', []), done)
            state = next_state
            if done:
                break
                
        ε = max(ε_end, ε - ε_decay)
        if len(exp_buf) >= 128:
            loss = train_model(model, tokenizer, opt, exp_buf, bs=64)
            metrics['losses'].append(loss)

        success = result is not None
        metrics['success_rate'].append(1 if success else 0)
        metrics['avg_steps'].append(step+1)
        metrics['rewards'].append(ep_reward)

        if (episode + 1) % 10 == 0:
            avg_success = np.mean(metrics['success_rate'][-10:])
            avg_steps = np.mean(metrics['avg_steps'][-10:])
            avg_reward = np.mean(metrics['rewards'][-10:])
            print(f'Episode {episode+1}/{num_episodes} | Success: {avg_success:.2f}'
                  f' | Steps: {avg_steps:.2f} | Reward: {avg_reward:.2f} | ε: {ε:.3f}')
    return model, metrics

class ConstraintLogicEnvironment:
    def __init__(self, addr, port):
        self.addr = addr
        self.port = port
        self.ctx = zmq.Context()
        self.sock = self.ctx.socket(zmq.REQ)
        logger.debug(f'Connecting to {self.addr}:{self.port}')
        self.sock.connect(f'tcp://{self.addr}:{self.port}')
        logger.debug(f'Connected to {self.addr}:{self.port}')
        self.steps_taken = 0

    def __del__(self):
        if hasattr(self, 'sock') and self.sock:
            self.sock.close()
        if hasattr(self, 'ctx') and self.ctx:
            self.ctx.term()

    def reset(self):
        data = {'query': "(query (a b) (appendo a b '(1 2 3 4)))"}
        logger.debug(f'Sending `{data}` to {self.addr}:{self.port}')
        self.sock.send_json(data)
        initial_state = self.sock.recv_json()
        logger.debug(f'Received {initial_state} from {self.addr}:{self.port}')
        self.steps_taken = 0
        return initial_state

    def step(self, action):
        assert action < 256, f"Didn't plan for enough actions: {action}."
        data = {'choice': action}
        logger.debug(f'Sending `{data}` to {self.addr}:{self.port}')
        self.sock.send_json(data)
        self.steps_taken += 1
        
        response = self.sock.recv_json()
        logger.debug(f'Received response: {response}')
        
        # Handle error responses from the Racket server
        if isinstance(response, dict) and 'error' in response:
            logger.error(f"Server error: {response['error']}")
            return {'choices': []}, None, True
            
        result = None
        done = False
        
        if 'results' in response and response['results']:
            result = response['results']
            done = True
        elif not response.get('choices', []):
            done = True
            
        return response, result, done

def main():
    logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')
    
    tokenizer = tiktoken.get_encoding("cl100k_base")
    n_vocab = tokenizer.n_vocab
    n_embed = 128
    n_hidden = 256
    model = ConstraintScoringNetwork(n_vocab, n_embed, n_hidden)
    
    try:
        env = ConstraintLogicEnvironment("127.0.0.1", 5555)
        logger.info("Starting training with 20 episodes to verify system works")
        trained_model, metrics = run_guided_search(model, tokenizer, env, num_episodes=100)
        torch.save(trained_model.state_dict(), "constraint_scorer_len.pt")
        logger.info("Training completed and model saved")
        return trained_model, metrics
    except Exception as e:
        logger.error(f"Error during training: {e}")
        raise

def test():
    env = ConstraintLogicEnvironment("127.0.0.1", 5555)
    rep = env.reset()
    action = select_action(trained_model, tokenizer, choices, 0)
    rep = env.step(action)
    action = select_action(trained_model, tokenizer, choices, 0)
    rep = env.step(action)

    env.step(1)

if __name__ == "__main__":
    main()
