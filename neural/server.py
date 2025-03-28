"""Neural Network Guided Search for Constraint Logic Programming.

This module implements a neural network-based scoring system to guide the search
process in a constraint logic programming system (miniKanren). The system works by
evaluating and scoring possible search paths and using these scores to guide the
exploration of the search space.

Core Functionality:
-----------------
1. Communication with a Racket server running miniKanren via ZeroMQ
   - Sends queries to initialize search
   - Receives search states containing results and choices
   - Sends choice selections to expand the search

2. Neural Network Scoring
   - Tokenizes constraint representations using tiktoken
   - Processes tokens through a GRU-based neural network with attention
   - Outputs scores for each available choice
   - Uses softmax to convert scores to probabilities

3. Reinforcement Learning
   - Implements an experience buffer to store state-action-reward transitions
   - Calculates rewards based on solution quality and search efficiency
   - Uses policy gradient methods to update the neural network
   - Supports exploration via epsilon-greedy strategy

4. Interactive and Training Modes
   - Provides an interactive mode for manual exploration
   - Implements automated training with configurable parameters
   - Includes debugging tools to analyze model behavior

5. Problem Abstraction
   - Supports multiple constraint problem types
   - Implements curriculum learning for progressive difficulty
   - Customizes rewards based on problem-specific criteria

Implementation Details:
---------------------
- The ConstraintScoringNetwork uses embeddings, GRU, batch normalization, and attention
- Experience buffer stores transitions for batch training
- Rewards are calculated based on solution quality, efficiency, and balanced list lengths
- Training includes gradient clipping, learning rate scheduling, and reward normalization
- Problem abstraction allows for flexible training across different problem domains

Example Usage:
---------------------
- Run in training mode: `python neural/server.py`
- Run in interactive mode: `python neural/server.py --interactive`
"""

import pdb
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

from neural.problems import (
    AppendoProblem, 
    ProblemRegistry, 
    ProblemCurriculum,
    default_registry
)

logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)


class ConstraintScoringNetwork(nn.Module):
    def __init__(self, n_vocab, n_embed, n_hidden):
        super().__init__()
        self.embedding = nn.Embedding(n_vocab, n_embed)
        
        # Add layer normalization after embedding
        self.embed_norm = nn.LayerNorm(n_embed)
        
        # Add dropout after embedding
        self.embed_dropout = nn.Dropout(0.2)

        # Use a single-direction GRU with larger hidden size
        self.gru = nn.GRU(n_embed, n_hidden * 2, batch_first=True, bidirectional=False)

        # Add batch normalization
        self.bn = nn.BatchNorm1d(n_hidden * 2)

        self.attention = nn.Linear(n_hidden * 2, 1)

        # Add a hidden layer before final output
        self.hidden = nn.Linear(n_hidden * 2, n_hidden)
        self.hidden_norm = nn.LayerNorm(n_hidden)  # Add normalization here
        self.relu = nn.ReLU()
        self.fc = nn.Linear(n_hidden, 1)

        # Initialize weights with a different strategy
        self._init_weights()

    def _init_weights(self):
        for name, param in self.named_parameters():
            if "weight" in name:
                if "embedding" in name:
                    # Use normal initialization for embeddings
                    nn.init.normal_(param, mean=0.0, std=0.02)
                elif len(param.shape) >= 2:  # Only apply Xavier to weights with 2+ dimensions
                    # Use a smaller gain for Xavier initialization
                    nn.init.xavier_uniform_(param, gain=0.5)
                else:
                    # Use smaller uniform init for 1D weights
                    nn.init.uniform_(param, -0.05, 0.05)
            elif "bias" in name:
                nn.init.constant_(param, 0.0)

    def forward(self, x, lengths=None):
        # x shape: [batch_size, seq_len]
        embedded = self.embedding(x)  # [batch_size, seq_len, embed_dim]
        embedded = self.embed_norm(embedded)  # Apply layer norm
        embedded = self.embed_dropout(embedded)

        if lengths is not None:
            # Pack padded sequence for more efficient processing
            packed = nn.utils.rnn.pack_padded_sequence(
                embedded, lengths.cpu(), batch_first=True, enforce_sorted=False
            )
            outputs, hidden = self.gru(packed)
            outputs, _ = nn.utils.rnn.pad_packed_sequence(outputs, batch_first=True)
        else:
            outputs, hidden = self.gru(embedded)

        # Apply batch normalization
        # Reshape for batch norm (which expects [N, C, ...])
        batch_size, seq_len, hidden_size = outputs.shape
        outputs_reshaped = outputs.transpose(1, 2)  # [batch_size, hidden_size, seq_len]
        outputs_reshaped = self.bn(outputs_reshaped)
        outputs = outputs_reshaped.transpose(1, 2)  # [batch_size, seq_len, hidden_size]

        # Simple attention mechanism
        attention_weights = F.softmax(self.attention(outputs), dim=1)
        context = torch.sum(attention_weights * outputs, dim=1)

        # Add a hidden layer with ReLU activation
        hidden_out = self.relu(self.hidden(context))
        hidden_out = self.hidden_norm(hidden_out)  # Apply layer norm

        # Final score
        score = self.fc(hidden_out)
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


def train_model(
    model, tokenizer, opt, exp_buf, bs=8, epochs=5, clip_grad=1.0, accum_steps=4
):  # Reduced clip_grad, added accum_steps
    if len(exp_buf) < bs:
        return 0

    total_loss = 0
    for _ in range(epochs):
        # Use smaller batches but accumulate gradients
        actual_bs = max(1, bs // accum_steps)
        accumulated_loss = 0
        
        opt.zero_grad()  # Zero gradients at the beginning of accumulation
        
        for accum_idx in range(accum_steps):
            if len(exp_buf) < actual_bs:
                continue
                
            batch = exp_buf.sample(actual_bs)
            states, actions, rewards, next_states, dones = zip(*batch)
            
            # Normalize rewards for stability
            rewards = torch.tensor(rewards)
            if len(rewards) > 1 and rewards.std() > 0:
                rewards = (rewards - rewards.mean()) / (rewards.std() + 1e-8)
            rewards = rewards.tolist()
            
            # Debug rewards
            if rewards:
                logger.debug(
                    f"Batch rewards: min={min(rewards):.2f}, max={max(rewards):.2f}, mean={np.mean(rewards):.2f}"
                )
            
            # Process each state-action pair
            batch_loss = 0
            valid_samples = 0

            for state, action, reward in zip(states, actions, rewards):
                if not state:  # Skip empty states
                    continue

                tokens, lengths = preprocess_constraints(tokenizer, state)
                scores = model(tokens, lengths)

                probs = F.softmax(scores, dim=0).squeeze(1)
                assert action < len(probs)
                action_prob = probs[action]
                log_prob = torch.log(action_prob + 1e-10)
                # For policy gradient, we want to maximize reward * log_prob
                # So our loss is negative of that
                sample_loss = -log_prob * reward
                batch_loss += sample_loss
                valid_samples += 1

            if valid_samples > 0:
                batch_loss = batch_loss / valid_samples
                # Scale the loss by the number of accumulation steps
                batch_loss = batch_loss / accum_steps
                logger.debug(f"Batch loss: {batch_loss.item():.4f}")
                batch_loss.backward()  # Accumulate gradients without zeroing
                accumulated_loss += batch_loss.item()
        
        # Apply gradients after accumulation
        if accumulated_loss > 0:
            # Debug gradients
            for name, param in model.named_parameters():
                if param.requires_grad and param.grad is not None:
                    grad_norm = param.grad.norm().item()
                    logger.debug(f"{name}: grad norm = {grad_norm:.4f}")

                    # Add gradient noise to help escape local minima
                    if grad_norm < 0.001:
                        noise = torch.randn_like(param.grad) * 0.01
                        param.grad = param.grad + noise
                        
            torch.nn.utils.clip_grad_norm_(model.parameters(), clip_grad)
            opt.step()
            total_loss += accumulated_loss

    return total_loss / epochs if epochs > 0 else 0


def calc_reward(result, steps_taken, max_steps=200, problem=None):
    """Calculate reward based on result and efficiency using the problem's reward function if provided."""
    if problem:
        return problem.calculate_reward(result, steps_taken, max_steps)
    
    # Fallback to original implementation if no problem is provided
    if result:
        # Solution found - increased base reward
        base_reward = 20.0  # Doubled from 10.0

        # Efficiency bonus - more steps = less bonus
        efficiency_bonus = max(
            0, (max_steps - steps_taken) / max_steps * 15.0
        )  # Increased from 10.0

        # Check if solution has balanced list lengths
        print(result)
        lengths = [len(x) for x in result.values()]
        length_variance = np.var(lengths) if lengths else 0
        length_bonus = 15.0 / (
            1.0 + length_variance
        )  # Higher variance = lower bonus, increased from 10.0

        return base_reward + efficiency_bonus + length_bonus
    elif steps_taken >= max_steps:
        # Ran out of steps without solution - increased penalty
        return 0  # Doubled from -5.0
    else:
        # No solution yet, small penalty to encourage efficiency
        return 1  # Slightly increased from -0.2


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
    lr=5e-3,  # Increased from 1e-3
    γ=0.95,
    ε_start=0.9,
    ε_end=0.05,
    curriculum=None,
):
    # Run debug training to verify model is learning correctly
    debug_training(model, tokenizer, env)

    exp_buf = ExperienceBuffer()
    # Use a different optimizer with weight decay
    opt = torch.optim.AdamW(model.parameters(), lr=lr, weight_decay=1e-4)

    # Add a learning rate scheduler
    scheduler = torch.optim.lr_scheduler.ReduceLROnPlateau(
        opt, mode="max", factor=0.5, patience=5, verbose=True
    )

    bs = 8  # Batch size

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
            # Update curriculum if provided
            if curriculum and curriculum.update():
                env.set_problem(curriculum.get_current_problem())
                logger.info(f"Curriculum updated to: {env.problem.get_name()}")
                
            state = env.reset()

            # Store episode history
            episode_states = []
            episode_actions = []
            episode_rewards = []
            episode_next_states = []
            episode_dones = []

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

                # Calculate immediate reward using the environment's problem
                immediate_reward = env.get_reward(result, step + 1, max_steps)

                # Store step information
                episode_states.append(choices)
                episode_actions.append(action)
                episode_rewards.append(immediate_reward)
                episode_next_states.append(next_state)
                episode_dones.append(done)

                ep_reward += immediate_reward
                logger.debug(f"reward: {immediate_reward:.2f} | ep_reward: {ep_reward:.2f}")
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

                # Add to experience buffer with discounted returns
                for i in range(len(episode_states)):
                    exp_buf.add(
                        episode_states[i],
                        episode_actions[i],
                        discounted_rewards[i].item(),  # Use discounted return instead of immediate reward
                        episode_next_states[i],  # Store next_state for potential future use
                        i == len(episode_states) - 1,  # done flag
                    )

            # Train model after every episode
            if len(exp_buf) >= bs:
                loss = train_model(model, tokenizer, opt, exp_buf, bs=bs)
                metrics["losses"].append(loss)

                # Update learning rate scheduler based on average reward
                avg_reward = (
                    np.mean(metrics["rewards"][-10:])
                    if len(metrics["rewards"]) >= 10
                    else 0
                )
                scheduler.step(avg_reward)

                # Debug scores every 5 episodes
                if (episode + 1) % 5 == 0:
                    debug_scores(model, tokenizer, env)

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
    def __init__(self, addr, port, problem=None, timeout=1000):  # timeout in milliseconds
        self.addr = addr
        self.port = port
        self.timeout = timeout
        self.problem = problem or AppendoProblem()  # Default to AppendoProblem
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
            
    def set_problem(self, problem):
        """Change the current problem."""
        self.problem = problem
        return self

    def reset(self):
        query = self.problem.generate_query()
        data = {"query": query}
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
        
    def get_reward(self, result, steps_taken=None, max_steps=200):
        """Calculate reward using the current problem's reward function."""
        if steps_taken is None:
            steps_taken = self.steps_taken
        return self.problem.calculate_reward(result, steps_taken, max_steps)


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

    # Create environment with default problem
    env = ConstraintLogicEnvironment("127.0.0.1", 5555, timeout=5000)
    
    # Show available problems
    print("\nAvailable problem types:")
    for i, name in enumerate(default_registry.get_all_names()):
        print(f"{i}: {name}")
    
    try:
        problem_idx = int(input("\nSelect a problem type (number): "))
        problem_name = default_registry.get_all_names()[problem_idx]
        env.set_problem(default_registry.create(problem_name))
        print(f"Selected problem: {env.problem.get_name()}")
    except (ValueError, IndexError):
        print("Invalid selection, using default problem.")
    
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
        level=logging.INFO,
        format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
    )

    tokenizer = tiktoken.get_encoding("cl100k_base")
    n_vocab = tokenizer.n_vocab
    n_embed = 64
    n_hidden = 128
    model = ConstraintScoringNetwork(n_vocab, n_embed, n_hidden)

    model_path = "./constraint_scorer_error.pt"
    if os.path.exists(model_path):
        model = load_model(model_path, model)
        logger.info(f"Loaded pre-trained model from {model_path}")

    try:
        env = ConstraintLogicEnvironment("127.0.0.1", 5555, timeout=2000)
        
        # Create a curriculum for training
        curriculum = ProblemCurriculum(default_registry)
        curriculum.add_stage("appendo-small", episodes=10)
        curriculum.add_stage("appendo-medium", episodes=15)
        curriculum.add_stage("appendo-large", episodes=15)
        
        logger.info(f"Starting training with curriculum: {[stage[0] for stage in curriculum.stages]}")
        env.set_problem(curriculum.get_current_problem())
        
        trained_model, metrics = run_guided_search(
            model, tokenizer, env, num_episodes=40, curriculum=curriculum
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
    logger.debug("DEBUGGGING")
    rep = env.reset()
    choices = rep["choices"]
    tokens, lengths = preprocess_constraints(tokenizer, choices)
    scores = model(tokens, lengths)
    action = select_action(model, tokenizer, choices, 0)
    logger.debug(f"{action}, {scores}")
    rep, res, done = env.step(action)
    choices = rep["choices"]
    tokens, lengths = preprocess_constraints(tokenizer, choices)
    scores = model(tokens, lengths)
    action = select_action(model, tokenizer, choices, 0)
    logger.debug(f"{action}, {scores}")
    rep, res, done = env.step(action)
    choices = rep["choices"]
    if choices:  # Check if there are choices available
        tokens, lengths = preprocess_constraints(tokenizer, choices)
        scores = model(tokens, lengths)
        action = select_action(model, tokenizer, choices, 0)
        logger.debug(f"{action}, {scores}")
    rep, res, done = env.step(action)
    choices = rep["choices"]
    if choices:  # Check if there are choices available
        tokens, lengths = preprocess_constraints(tokenizer, choices)
        scores = model(tokens, lengths)
        action = select_action(model, tokenizer, choices, 0)
        logger.debug(f"{action}, {scores}")



def test():
    tokenizer = tiktoken.get_encoding("cl100k_base")
    n_vocab = tokenizer.n_vocab
    n_embed = 64
    n_hidden = 128
    model = ConstraintScoringNetwork(n_vocab, n_embed, n_hidden)
    model = load_model("./constraint_scorer_len.pt", model)
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
    context = zmq.Context()
    responder = context.socket(zmq.REP)
    responder.bind("tcp://*:5555")
    while True:
        message = responder.recv()
        print(f"Received request: {message}")
        responder.send(b"World")

