"""Constraint logic problems for neural guided search.

This module defines the abstract base class and concrete implementations
for different constraint logic programming problems that can be used with
the neural guided search system.

Core Components:
--------------
1. ConstraintProblem - Abstract base class defining the interface for all problems
2. Concrete problem implementations (AppendoProblem, ReverseoProblem, etc.)
3. ProblemRegistry - Factory for creating and managing problem instances
4. ProblemCurriculum - Support for curriculum learning with increasing difficulty

Each problem type implements:
- Query generation for the Racket server
- Custom reward calculation based on problem-specific criteria
- Human-readable name and description
"""

import random
import numpy as np
from abc import ABC, abstractmethod

class ConstraintProblem(ABC):
    """Abstract base class for constraint logic problems."""
    
    @abstractmethod
    def generate_query(self) -> str:
        """Generate a miniKanren query string."""
        pass
        
    @abstractmethod
    def calculate_reward(self, result, steps_taken, max_steps=200) -> float:
        """Calculate reward based on the result and efficiency."""
        pass
        
    @abstractmethod
    def get_name(self) -> str:
        """Return a human-readable name for this problem type."""
        pass

class AppendoProblem(ConstraintProblem):
    """The appendo relation problem with configurable list lengths."""
    
    def __init__(self, min_length=1, max_length=5):
        self.min_length = min_length
        self.max_length = max_length
    
    def generate_query(self) -> str:
        # Generate a random list length between min and max
        length = random.randint(self.min_length, self.max_length)
        elements = [str(i+1) for i in range(length)]
        list_str = f"'({' '.join(elements)})"
        return f"(query (a b) (appendo a b {list_str}))"
    
    def calculate_reward(self, result, steps_taken, max_steps=200) -> float:
        if result:
            # Solution found - base reward
            base_reward = 20.0
            
            # Efficiency bonus - more steps = less bonus
            efficiency_bonus = max(0, (max_steps - steps_taken) / max_steps * 15.0)
            
            # Check if solution has balanced list lengths
            lengths = [len(x) for x in result.values()]
            length_variance = np.var(lengths) if lengths else 0
            length_bonus = 15.0 / (1.0 + length_variance)
            return base_reward + efficiency_bonus + length_bonus
        elif steps_taken >= max_steps:
            # Ran out of steps without solution
            return 0
        else:
            # No solution yet, small reward to encourage exploration
            return 1
    
    def get_name(self) -> str:
        return f"Appendo(len={self.min_length}-{self.max_length})"

class EvaloProblem(ConstraintProblem):
    def __init__(self):
        self.problems = [
            "(query (a b) (appendo a b '(1 2 3 4)))",
            "(query (a b) (conso a b '(1 2)))",
            "(query (expr) (eval-expo expr '() 'a))",
            # "(query (p) (== p (make-num 3)))",
            # "(query (p) (== p (make-num 4)))",
            # "(query (p) (== p `(lambda)))",
            # "(query (p) (== p `(lambda ,(make-num 1))))",
            # "(query (p) (== p `(lambda ,(make-num 2))))",
            # "(query (p) (== p `(app (lambda ,(make-num 2)))))",
        ]

    def generate_query(self) -> str:
        return random.choice(self.problems)

    def calculate_reward(self, result, steps_taken, max_steps=200) -> float:
        if result:
            base_reward = 20.0
            efficiency_bonus = max(0, (max_steps - steps_taken) / max_steps * 15.0)
            return base_reward + efficiency_bonus
        elif steps_taken >= max_steps:
            return 0
        else:
            return 1

    def get_name(self) -> str:
        return f"(eval-expo ...)"

class ReverseoProblem(ConstraintProblem):
    """The reverseo relation problem with configurable list lengths."""
    
    def __init__(self, min_length=1, max_length=5):
        self.min_length = min_length
        self.max_length = max_length
    
    def generate_query(self) -> str:
        # Generate a random list length between min and max
        length = random.randint(self.min_length, self.max_length)
        elements = [str(i+1) for i in range(length)]
        list_str = f"'({' '.join(elements)})"
        return f"(query (r) (reverseo {list_str} r))"
    
    def calculate_reward(self, result, steps_taken, max_steps=200) -> float:
        if result:
            # Solution found - base reward
            base_reward = 20.0
            
            # Efficiency bonus - more steps = less bonus
            efficiency_bonus = max(0, (max_steps - steps_taken) / max_steps * 15.0)
            
            return base_reward + efficiency_bonus
        elif steps_taken >= max_steps:
            # Ran out of steps without solution
            return 0
        else:
            # No solution yet, small reward to encourage exploration
            return 1
    
    def get_name(self) -> str:
        return f"Reverseo(len={self.min_length}-{self.max_length})"

class ProblemRegistry:
    """Registry of available constraint problems for training."""
    
    def __init__(self):
        self.problems = {}
        
    def register(self, name, problem_class, **kwargs):
        """Register a problem type with optional default parameters."""
        self.problems[name] = (problem_class, kwargs)
        
    def create(self, name, **kwargs):
        """Create an instance of a registered problem."""
        if name not in self.problems:
            raise ValueError(f"Unknown problem type: {name}")
        
        problem_class, default_kwargs = self.problems[name]
        # Override defaults with provided kwargs
        params = {**default_kwargs, **kwargs}
        return problem_class(**params)
        
    def get_all_names(self):
        """Get names of all registered problems."""
        return list(self.problems.keys())

class ProblemCurriculum:
    """Manages a sequence of problems with increasing difficulty."""
    
    def __init__(self, registry):
        self.registry = registry
        self.stages = []  # List of (problem_name, kwargs, episodes)
        self.current_stage = 0
        self.episodes_in_stage = 0
        
    def add_stage(self, problem_name, episodes, **kwargs):
        """Add a stage to the curriculum."""
        self.stages.append((problem_name, kwargs, episodes))
        
    def get_current_problem(self):
        """Get the problem for the current curriculum stage."""
        if self.current_stage >= len(self.stages):
            # Return the last stage if we've completed the curriculum
            name, kwargs, _ = self.stages[-1]
        else:
            name, kwargs, _ = self.stages[self.current_stage]
        
        return self.registry.create(name, **kwargs)
        
    def update(self, episode_completed=1):
        """Update curriculum stage based on completed episodes."""
        if self.current_stage >= len(self.stages):
            return False  # No change
            
        self.episodes_in_stage += episode_completed
        _, _, required_episodes = self.stages[self.current_stage]
        
        if self.episodes_in_stage >= required_episodes:
            self.current_stage += 1
            self.episodes_in_stage = 0
            return True  # Stage changed
            
        return False  # No change

# Create a default registry with common problems
default_registry = ProblemRegistry()
default_registry.register("appendo-small", AppendoProblem, min_length=1, max_length=3)
default_registry.register("appendo-medium", AppendoProblem, min_length=3, max_length=5)
default_registry.register("appendo-large", AppendoProblem, min_length=5, max_length=8)
default_registry.register("eval-expo", EvaloProblem)
