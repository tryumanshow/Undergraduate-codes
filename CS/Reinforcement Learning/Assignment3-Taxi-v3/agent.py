import numpy as np
from collections import defaultdict

class Agent:

    def __init__(self, Q, mode="test_mode", alpha=0.01, gamma=0.99, eps=1e-1):
        self.Q = Q
        self.mode = mode
        self.alpha = alpha
        self.gamma = gamma
        self.eps = eps
        self.n_actions = 6

        self.trajectory = []
        self.G = defaultdict(lambda: np.zeros(self.n_actions))
        self.N = defaultdict(lambda: np.zeros(self.n_actions))

    def select_action(self, state):
        """
        Params
        ======
        - state: the current state of the environment
        Returns
        =======
        - action: an integer, compatible with the task's action space
        """
        if np.random.rand() > self.eps:
            action = np.argmax(self.Q[state])
        else:
            action = np.random.choice(self.n_actions)

        return action

    def step(self, state, action, reward, next_state, done):

        # agent.step(state, action, reward, next_state, done)
        """
        Params
        ======
        - state: the previous state of the environment
        - action: the agent's previous choice of action
        - reward: last reward received
        - next_state: the current state of the environment
        - done: whether the episode is complete (True or False)
        """

        # I don't know why I can't increase average reward over 0.
        if self.mode == 'mc_control':
            self.trajectory.append((state, action, reward))
            self.N[state][action] += 1 # Every-step MC

            if done:
                states, actions, rewards = zip(*self.trajectory)
                discounts = np.array([self.gamma**i for i in range(len(rewards))])

                for i, s in enumerate(states):
                    self.G[s][actions[i]] += sum(discounts[:len(states)-i] * rewards[i:])

                for _, s in enumerate(self.G.keys()):
                    for a in range(len(self.G[s])):
                        if self.N[s][a] != 0:
                            self.Q[s][a] = self.Q[s][a] + 1/self.N[s][a] * (self.G[s][a] - self.Q[s][a])

                self.trajectory = []

        if self.mode == 'q_learning':
            self.Q[state][action] = \
                self.Q[state][action] + self.alpha * (reward + self.gamma * self.Q[next_state][np.argmax(self.Q[next_state])] -
                                                      self.Q[state][action])