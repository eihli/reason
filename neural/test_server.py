import unittest
import torch
from neural.server import (
    ConstraintScoringNetwork,
    ConstraintLogicEnvironment,
    preprocess_constraints,
    select_action,
    ExperienceBuffer,
    train_model,
    calc_reward
)
import tiktoken

def train_model_for_test(model, tokenizer, env, num_episodes=25):
    """Train the model for a small number of episodes for testing purposes"""
    exp_buf = ExperienceBuffer()
    
    # Start with a very low learning rate
    initial_lr = 1e-4
    max_lr = 1e-2
    opt = torch.optim.AdamW(model.parameters(), lr=initial_lr, weight_decay=1e-4)
    
    # Early stopping parameters
    patience = 5
    best_loss = float('inf')
    patience_counter = 0
    
    # Train for a few episodes
    for episode in range(num_episodes):
        # Implement learning rate warmup for first 10 episodes
        if episode < 5:
            # Linear warmup
            lr = initial_lr + (max_lr - initial_lr) * (episode / 5)
            for param_group in opt.param_groups:
                param_group['lr'] = lr
        state = env.reset()
        
        # Store episode history
        episode_states = []
        episode_actions = []
        episode_rewards = []
        episode_next_states = []
        episode_dones = []
        
        # Follow the expected path [0, 1, 1, 0]
        expected_path = [0, 1, 1, 0]
        
        for step, action in enumerate(expected_path):
            choices = state.get("choices", [])
            if not choices:
                break
                
            next_state, result, done = env.step(action)
            
            # Calculate immediate reward
            immediate_reward = calc_reward(result, step + 1, max_steps=200)
            
            # Store step information
            episode_states.append(choices)
            episode_actions.append(action)
            episode_rewards.append(immediate_reward)
            episode_next_states.append(next_state)
            episode_dones.append(done)
            
            state = next_state
            if done:
                break
        
        # Calculate discounted rewards
        discounted_rewards = []
        R = 0
        gamma = 0.95
        for r in reversed(episode_rewards):
            R = r + gamma * R
            discounted_rewards.insert(0, R)
        
        # Add to experience buffer with discounted returns
        for i in range(len(episode_states)):
            exp_buf.add(
                episode_states[i],
                episode_actions[i],
                discounted_rewards[i],
                episode_next_states[i],
                i == len(episode_states) - 1
            )
        
        # Train model after every episode
        if len(exp_buf) >= 4:  # Use a smaller batch size for testing
            # More epochs for better convergence
            loss = train_model(model, tokenizer, opt, exp_buf, bs=4, epochs=5)
            print(f"Episode {episode+1}/{num_episodes} | Loss: {loss:.4f}")
            
            # Early stopping check
            if loss < best_loss * 0.95:  # 5% improvement threshold
                best_loss = loss
                patience_counter = 0
            else:
                patience_counter += 1
                
            if patience_counter >= patience and loss < 0.1:
                print(f"Early stopping at episode {episode+1} with loss {loss:.4f}")
                break
    
    return model

class TestAppendoPathLearning(unittest.TestCase):
    def setUp(self):
        # Initialize tokenizer
        self.tokenizer = tiktoken.get_encoding("cl100k_base")
        
        # Initialize model with same parameters as in main()
        n_vocab = self.tokenizer.n_vocab
        n_embed = 64
        n_hidden = 128
        self.model = ConstraintScoringNetwork(n_vocab, n_embed, n_hidden)
        
        # Connect to Racket server
        self.env = ConstraintLogicEnvironment("127.0.0.1", 5555, timeout=2000)
        
        # Expected path for appendo solution
        self.expected_path = [0, 1, 1, 0]
        
        # Train the model instead of loading it
        print("Training model for tests...")
        # Increase number of episodes for better training
        self.model = train_model_for_test(self.model, self.tokenizer, self.env, num_episodes=25)
        print("Model training complete")

    def test_model_follows_expected_path(self):
        """Test that the trained model follows the expected path [0, 1, 1, 0]"""
        
        # Reset the environment
        state = self.env.reset()
        
        # Follow the path using the model's predictions
        actual_path = []
        
        # Step 1
        choices = state.get("choices", [])
        if not choices:
            self.fail("No choices available after reset")
            
        # Get model's prediction with no exploration (ε=0)
        action = select_action(self.model, self.tokenizer, choices, ε=0)
        actual_path.append(action)
        
        # Take the action
        state, result, done = self.env.step(action)
        
        # Step 2
        choices = state.get("choices", [])
        if not choices:
            self.fail("No choices available after first step")
            
        action = select_action(self.model, self.tokenizer, choices, ε=0)
        actual_path.append(action)
        
        # Take the action
        state, result, done = self.env.step(action)
        
        # Step 3
        choices = state.get("choices", [])
        if not choices:
            self.fail("No choices available after second step")
            
        action = select_action(self.model, self.tokenizer, choices, ε=0)
        actual_path.append(action)
        
        # Take the action
        state, result, done = self.env.step(action)
        
        # Step 4
        choices = state.get("choices", [])
        if not choices:
            # If we're at a solution already, that's unexpected
            if result:
                self.fail("Found solution too early")
            else:
                self.fail("No choices available and no solution found")
                
        action = select_action(self.model, self.tokenizer, choices, ε=0)
        actual_path.append(action)
        
        # Take the action
        state, result, done = self.env.step(action)
        
        # Check if we found a solution
        self.assertIsNotNone(result, "Should find a solution after following the path")
        
        # Check if the path matches the expected path
        self.assertEqual(actual_path, self.expected_path, 
                         f"Model followed path {actual_path} but expected {self.expected_path}")

    def test_model_scores_favor_expected_path(self):
        """Test that the model's scores favor the expected path choices"""
        
        # Reset the environment
        state = self.env.reset()
        
        # For each step in the expected path
        for step, expected_action in enumerate(self.expected_path):
            choices = state.get("choices", [])
            if not choices:
                self.fail(f"No choices available at step {step}")
                
            # Get raw scores from the model
            tokens, lengths = preprocess_constraints(self.tokenizer, choices)
            with torch.no_grad():
                scores = self.model(tokens, lengths).squeeze(1)
                
            # Check that the expected action has the highest score
            max_score_idx = torch.argmax(scores).item()
            self.assertEqual(max_score_idx, expected_action, 
                            f"At step {step}, model favors action {max_score_idx} but expected {expected_action}")
            
            # Take the expected action to move to the next state
            state, result, done = self.env.step(expected_action)
            
            # If we've reached a solution, break
            if result:
                break

if __name__ == "__main__":
    unittest.main()
