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
import os
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
logger.setLevel(logging.DEBUG)


class ConstraintScoringNetwork(nn.Module):
    def __init__(self, n_vocab, n_embed, n_hidden):
        super().__init__()
        self.embedding = nn.Embedding(n_vocab, n_embed)
        self.gru = nn.GRU(n_embed, n_hidden, batch_first=True, bidirectional=True)
        self.attention = nn.Linear(n_hidden * 2, 1)
        self.fc = nn.Linear(n_hidden * 2, 1)

    def forward(self, x, lengths=None):
        # x shape: [batch_size, seq_len]
        embedded = self.embedding(x)  # [batch_size, seq_len, embed_dim]

        if lengths is not None:
            # Pack padded sequence for more efficient processing
            packed = nn.utils.rnn.pack_padded_sequence(
                embedded, lengths.cpu(), batch_first=True, enforce_sorted=False
            )
            outputs, hidden = self.gru(packed)
            outputs, _ = nn.utils.rnn.pad_packed_sequence(outputs, batch_first=True)
        else:
            outputs, hidden = self.gru(embedded)

        # Simple attention mechanism
        attention_weights = F.softmax(self.attention(outputs), dim=1)
        context = torch.sum(attention_weights * outputs, dim=1)

        # Final score
        score = self.fc(context)
        return score


class ExperienceBuffer:
    def __init__(self, buffer_size=10000):
        self.buffer = deque(maxlen=buffer_size)

    def add(self, state, action, reward, next_state, done):
        self.buffer.append((state, action, reward, next_state, done))

    def sample(self, batch_size):
        return random.sample(self.buffer, min(len(self.buffer), batch_size))

    def __len__(self):
        return len(self.buffer)


def preprocess_constraints(
    tokenizer: tiktoken.Encoding, choices: list[str]
) -> tuple[Tensor, Tensor]:
    tokenized = [tokenizer.encode(choice) for choice in choices]
    lengths = [len(tokens) for tokens in tokenized]
    padded = [tokens + [0] * (max(lengths) - len(tokens)) for tokens in tokenized]
    return torch.tensor(padded), torch.tensor(lengths)


def select_action(
    model: nn.Module, tokenizer: tiktoken.Encoding, choices: list[str], ε=0.1
) -> int:
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


def train_model(model, tokenizer, opt, exp_buf, bs=64, epochs=10, clip_grad=1.0):
    if len(exp_buf) < bs:
        return 0

    total_loss = 0
    for _ in range(epochs):
        batch = exp_buf.sample(bs)
        states, actions, rewards, next_states, dones = zip(*batch)

        # Process each state-action pair
        batch_loss = 0
        valid_samples = 0

        # Debug rewards
        if rewards:
            print(
                f"Batch rewards: min={min(rewards):.2f}, max={max(rewards):.2f}, mean={np.mean(rewards):.2f}"
            )

        for state, action, reward in zip(states, actions, rewards):
            if not state:  # Skip empty states
                continue

            tokens, lengths = preprocess_constraints(tokenizer, state)
            scores = model(tokens, lengths)

            probs = F.softmax(scores, dim=0).squeeze(1)
            if action < len(probs):
                action_prob = probs[action]
                log_prob = torch.log(action_prob + 1e-10)
                # For policy gradient, we want to maximize reward * log_prob
                # So our loss is negative of that
                sample_loss = -log_prob * reward
                batch_loss += sample_loss
                valid_samples += 1

        if valid_samples > 0:
            batch_loss = batch_loss / valid_samples
            print(f"Batch loss: {batch_loss.item():.4f}")

            opt.zero_grad()
            batch_loss.backward()

            # Debug gradients
            for name, param in model.named_parameters():
                if param.requires_grad and param.grad is not None:
                    print(f"{name}: grad norm = {param.grad.norm().item():.4f}")

            torch.nn.utils.clip_grad_norm_(model.parameters(), clip_grad)
            opt.step()
            total_loss += batch_loss.item()

    return total_loss / epochs if epochs > 0 else 0


def calc_reward(result, steps_taken, max_steps=200):
    """Calculate reward based on result and efficiency"""
    if result:
        # Solution found - base reward
        base_reward = 2.0

        # Efficiency bonus - more steps = less bonus
        efficiency_bonus = max(0, (max_steps - steps_taken) / max_steps * 5.0)

        # Check if solution has balanced list lengths
        if len(result) >= 2:
            lengths = [len(x) for x in result.values()]
            length_variance = np.var(lengths) if lengths else 0
            length_bonus = 5.0 / (
                1.0 + length_variance
            )  # Higher variance = lower bonus
        else:
            length_bonus = 0

        return base_reward + efficiency_bonus + length_bonus
    elif steps_taken >= max_steps:
        # Ran out of steps without solution
        return -2.0
    else:
        # No solution yet, small penalty to encourage efficiency
        return -0.1


def test_calc_reward():
    result_1 = {"a": [], "b": [1, 2, 3, 4]}
    result_2 = {"a": [1, 2], "b": [1, 2]}
    steps_taken = 10
    calc_reward(result_1, steps_taken), calc_reward(result_2, steps_taken)


def run_guided_search(
    model,
    tokenizer,
    env,
    num_episodes=40,
    max_steps=200,
    lr=5e-5,
    γ=0.95,
    ε_start=0.5,
    ε_end=0.05,
):
    exp_buf = ExperienceBuffer()
    opt = torch.optim.AdamW(model.parameters(), lr=lr)
    bs = 16  # Batch size

    metrics = {
        "success_rate": [],
        "avg_steps": [],
        "rewards": [],
        "losses": [],
    }

    ε_decay = (ε_start - ε_end) / (num_episodes * 0.7)
    ε = ε_start

    # Add a flag to track if training was interrupted
    interrupted = False

    try:
        for episode in range(num_episodes):
            state = env.reset()

            # Store episode history
            episode_states = []
            episode_actions = []
            episode_rewards = []

            # Check if reset failed
            if "error" in state:
                logger.error(f"Failed to reset environment: {state['error']}")
                continue

            ep_reward = 0
            result = None

            for step in range(max_steps):
                choices = state.get("choices", [])
                if not choices:
                    break

                action = select_action(model, tokenizer, choices, ε)
                next_state, result, done = env.step(action)

                # Check if step failed
                if isinstance(next_state, dict) and "error" in next_state:
                    logger.error(f"Failed step: {next_state['error']}")
                    break

                reward = calc_reward(result, step + 1, max_steps)

                # Store step information
                episode_states.append(choices)
                episode_actions.append(action)
                episode_rewards.append(reward)

                ep_reward += reward
                logger.debug(f"reward: {reward:.2f} | ep_reward: {ep_reward:.2f}")
                state = next_state
                if done:
                    break

            # Process episode for training
            if episode_states:
                # Calculate discounted rewards
                discounted_rewards = []
                R = 0
                for r in reversed(episode_rewards):
                    R = r + γ * R
                    discounted_rewards.insert(0, R)

                # Normalize rewards for stability
                discounted_rewards = torch.tensor(discounted_rewards)
                if len(discounted_rewards) > 1:
                    discounted_rewards = (
                        discounted_rewards - discounted_rewards.mean()
                    ) / (discounted_rewards.std() + 1e-9)

                # Add to experience buffer
                for i in range(len(episode_states)):
                    exp_buf.add(
                        episode_states[i],
                        episode_actions[i],
                        discounted_rewards[i].item(),
                        [],  # next_state not needed for REINFORCE
                        i == len(episode_states) - 1,  # done flag
                    )

            # Train model
            if len(exp_buf) >= bs and (episode + 1) % 10 == 0:
                debug_scores(model, tokenizer, env)
                loss = train_model(model, tokenizer, opt, exp_buf, bs=bs)
                debug_scores(model, tokenizer, env)
                metrics["losses"].append(loss)

            # Update exploration rate
            ε = max(ε_end, ε - ε_decay)

            # Track metrics
            success = result is not None
            metrics["success_rate"].append(1 if success else 0)
            metrics["avg_steps"].append(step + 1)
            metrics["rewards"].append(ep_reward)

            if (episode + 1) % 10 == 0:
                avg_success = np.mean(metrics["success_rate"][-10:])
                avg_steps = np.mean(metrics["avg_steps"][-10:])
                avg_reward = np.mean(metrics["rewards"][-10:])
                logger.info(
                    f"Episode {episode+1}/{num_episodes} | Success: {avg_success:.2f}"
                    f" | Steps: {avg_steps:.2f} | Reward: {avg_reward:.2f} | ε: {ε:.3f}"
                )

    except KeyboardInterrupt:
        print("\nTraining interrupted by user. Saving current model state...")
        # Save the model before exiting
        torch.save(model.state_dict(), "constraint_scorer_interrupted.pt")
        interrupted = True
        print("Model saved as 'constraint_scorer_interrupted.pt'")
    except Exception as e:
        print(f"\nError during training: {e}")
        # Optionally save model on error too
        torch.save(model.state_dict(), "constraint_scorer_error.pt")
        print("Model saved as 'constraint_scorer_error.pt'")
        raise
    finally:
        # Always report training status
        if interrupted:
            print(
                f"Training was interrupted after {len(metrics['success_rate'])} episodes"
            )
        else:
            print(f"Training completed for all {num_episodes} episodes")

    return model, metrics


class ConstraintLogicEnvironment:
    def __init__(self, addr, port, timeout=1000):  # timeout in milliseconds
        self.addr = addr
        self.port = port
        self.timeout = timeout
        self.ctx = zmq.Context()
        self.sock = self.ctx.socket(zmq.REQ)
        logger.debug(f"Connecting to {self.addr}:{self.port}")
        self.sock.connect(f"tcp://{self.addr}:{self.port}")
        logger.debug(f"Connected to {self.addr}:{self.port}")
        self.steps_taken = 0
        # Create a poller for non-blocking operations
        self.poller = zmq.Poller()
        self.poller.register(self.sock, zmq.POLLIN)

    def __del__(self):
        if hasattr(self, "sock") and self.sock:
            self.poller.unregister(self.sock)
            self.sock.close()
        if hasattr(self, "ctx") and self.ctx:
            self.ctx.term()

    def reset(self):
        data = {"query": "(query (a b) (appendo a b '(1 2 3 4)))"}
        logger.debug(f"Sending `{data}` to {self.addr}:{self.port}")
        self.sock.send_json(data)

        # Non-blocking receive with timeout
        socks = dict(self.poller.poll(self.timeout))
        if self.sock in socks and socks[self.sock] == zmq.POLLIN:
            initial_state = self.sock.recv_json()
            logger.debug(f"Received {initial_state} from {self.addr}:{self.port}")
            self.steps_taken = 0
            return initial_state
        else:
            logger.warning(f"Timeout waiting for response from {self.addr}:{self.port}")
            return {"choices": [], "error": "Timeout waiting for server response"}

    def step(self, action):
        assert action < 256, f"Didn't plan for enough actions: {action}."
        data = {"choice": action}
        logger.debug(f"Sending `{data}` to {self.addr}:{self.port}")
        self.sock.send_json(data)
        self.steps_taken += 1

        # Non-blocking receive with timeout
        socks = dict(self.poller.poll(self.timeout))
        if self.sock in socks and socks[self.sock] == zmq.POLLIN:
            response = self.sock.recv_json()
            logger.debug(f"Received response: {response}")
        else:
            logger.warning(f"Timeout waiting for response from {self.addr}:{self.port}")
            return {"choices": []}, None, True

        # Handle error responses from the Racket server
        if isinstance(response, dict) and "error" in response:
            logger.error(f"Server error: {response['error']}")
            return {"choices": []}, None, True

        result = None
        done = False

        if "results" in response and response["results"]:
            result = response["results"]
            done = True
        elif not response.get("choices", []):
            done = True

        return response, result, done


def interactive_mode():
    """Interactive mode for development and debugging"""
    logging.basicConfig(
        level=logging.INFO,
        format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
    )

    tokenizer = tiktoken.get_encoding("cl100k_base")
    n_vocab = tokenizer.n_vocab
    n_embed = 64
    n_hidden = 128
    model = ConstraintScoringNetwork(n_vocab, n_embed, n_hidden)

    # Try to load the improved model first, fall back to original if not found
    model_path = "./constraint_scorer_improved.pt"
    if not os.path.exists(model_path):
        model_path = "./constraint_scorer.pt"

    if os.path.exists(model_path):
        model = load_model(model_path, model)
        logger.info(f"Loaded pre-trained model from {model_path}")

    env = ConstraintLogicEnvironment("127.0.0.1", 5555, timeout=5000)
    state = env.reset()

    print("\nInteractive Mode - Press Ctrl+C to exit")
    print(
        "Commands: 'r' to reset, 'q' to quit, or enter a number to select that choice"
    )

    try:
        while True:
            choices = state.get("choices", [])
            if not choices:
                print("\nNo choices available. Type 'r' to reset or 'q' to quit.")
            else:
                print("\nAvailable choices:")
                for i, choice in enumerate(choices):
                    print(
                        f"{i}: {choice[:100]}..."
                        if len(choice) > 100
                        else f"{i}: {choice}"
                    )

                # Get model's recommendation
                with torch.no_grad():
                    tokens, lengths = preprocess_constraints(tokenizer, choices)
                    scores = model(tokens, lengths)
                    probs = F.softmax(scores, dim=0).squeeze(1)
                    recommended = torch.argmax(probs).item()
                    print(
                        f"\nModel recommends choice {recommended} with confidence {probs[recommended].item():.4f}"
                    )

            cmd = input("\nEnter command: ")

            if cmd.lower() == "q":
                break
            elif cmd.lower() == "r":
                state = env.reset()
                continue

            try:
                action = int(cmd)
                if action < 0 or action >= len(choices):
                    print(
                        f"Invalid choice. Please enter a number between 0 and {len(choices)-1}"
                    )
                    continue

                next_state, result, done = env.step(action)
                state = next_state

                if result:
                    print(f"\nSolution found: {result}")
                    state = env.reset()
                elif done:
                    print("\nDead end reached. Resetting...")
                    state = env.reset()

            except ValueError:
                print("Invalid command. Please enter 'r', 'q', or a choice number.")

    except KeyboardInterrupt:
        print("\nExiting interactive mode...")

    return model


def main():
    logging.basicConfig(
        level=logging.DEBUG,
        format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
    )

    tokenizer = tiktoken.get_encoding("cl100k_base")
    n_vocab = tokenizer.n_vocab
    n_embed = 64
    n_hidden = 128
    model = ConstraintScoringNetwork(n_vocab, n_embed, n_hidden)

    model_path = "./constraint_scorer.pt"
    if os.path.exists(model_path):
        model = load_model(model_path, model)
        logger.info(f"Loaded pre-trained model from {model_path}")

    try:
        env = ConstraintLogicEnvironment("127.0.0.1", 5555, timeout=2000)
        logger.info("Starting training with 100 episodes")
        trained_model, metrics = run_guided_search(
            model, tokenizer, env, num_episodes=40
        )
        torch.save(trained_model.state_dict(), model_path)
        logger.info("Training completed and model saved")
        return trained_model, metrics
    except Exception as e:
        logger.error(f"Error during training: {e}")
        raise


def load_model(model_path, model):
    """Load saved model weights from a file."""
    try:
        model.load_state_dict(torch.load(model_path))
        logger.info(f"Successfully loaded model from {model_path}")
        return model
    except Exception as e:
        logger.error(f"Error loading model from {model_path}: {e}")
        raise


def debug_scores(model, tokenizer, env):
    print("DEBUGGGING")
    rep = env.reset()
    choices = rep["choices"]
    tokens, lengths = preprocess_constraints(tokenizer, choices)
    scores = model(tokens, lengths)
    print(scores)
    action = select_action(model, tokenizer, choices, 0)
    rep, res, done = env.step(action)
    choices = rep["choices"]
    tokens, lengths = preprocess_constraints(tokenizer, choices)
    scores = model(tokens, lengths)
    print(scores)
    action = select_action(model, tokenizer, choices, 0)
    rep, res, done = env.step(action)
    choices = rep["choices"]
    scores = model(tokens, lengths)
    print(scores)


def test():
    tokenizer = tiktoken.get_encoding("cl100k_base")
    n_vocab = tokenizer.n_vocab
    n_embed = 64
    n_hidden = 128
    model = ConstraintScoringNetwork(n_vocab, n_embed, n_hidden)
    model = load_model("./constraint_scorer_len.pt", model)
    model = trained_model
    env = ConstraintLogicEnvironment("127.0.0.1", 5555)
    rep = env.reset()
    choices = rep["choices"]
    tokens, lengths = preprocess_constraints(tokenizer, choices)
    scores = model(tokens, lengths)
    print(scores)
    action = select_action(model, tokenizer, choices, 0)
    rep, res, done = env.step(action)
    choices = rep["choices"]
    tokens, lengths = preprocess_constraints(tokenizer, choices)
    scores = model(tokens, lengths)
    print(scores)
    action = select_action(model, tokenizer, choices, 0)
    rep, res, done = env.step(action)
    choices = rep["choices"]
    scores = model(tokens, lengths)
    print(scores)
    action = select_action(model, tokenizer, choices, 0)
    tokens, lengths = preprocess_constraints(tokenizer, choices)
    model(tokens, lengths)
    action = select_action(model, tokenizer, choices, 0)
    env.step(action)


if __name__ == "__main__":
    # Add a command-line argument for interactive mode
    import argparse

    parser = argparse.ArgumentParser(
        description="Neural Guided Constraint Logic Programming"
    )
    parser.add_argument(
        "--interactive", "-i", action="store_true", help="Run in interactive mode"
    )
    args = parser.parse_args()

    if args.interactive:
        interactive_mode()
    else:
        main()
