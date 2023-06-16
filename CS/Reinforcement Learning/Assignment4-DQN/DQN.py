import gym
import torch
import torch.nn as nn
import torch.nn.functional as F
import torch.optim as optimizer
import random
import numpy as np
import matplotlib.pyplot as plt
from collections import namedtuple

BATCH_SIZE = 32
NUM_STATES = 4
NUM_HIDDEN = 32
NUM_ACTIONS = 2
NUM_EPISODES = 100
TIME_INDEX = 0
GAMMA = 0.9
UPDATE_STANDARD = 10
LR = 0.001

Transition = namedtuple('Transition', ('state', 'action', 'next_state', 'reward'))

# Replay Memory Buffer
class replay_buffer:
    def __init__(self, capacity):
        self.capacity = capacity
        self.memory = []
        self.index = 0

    def push(self, *args):
        if len(self.memory) < self.capacity:
            self.memory.append(None)
        self.memory[self.index] = Transition(*args)
        self.index = (self.index + 1) % self.capacity

    def sample(self, BATCH_SIZE):
        return random.sample(self.memory, BATCH_SIZE)

    def __len__(self):
        return len(self.memory)

# Class
class DQN(nn.Module):
    def __init__(self, num_states, num_actions):
        super(DQN, self).__init__()
        self.fc1 = nn.Linear(num_states, NUM_HIDDEN)
        self.fc2 = nn.Linear(NUM_HIDDEN, NUM_HIDDEN)
        self.fc3 = nn.Linear(NUM_HIDDEN, num_actions)

    def forward(self, x):
        x = F.relu(self.fc1(x))
        x = F.relu(self.fc2(x))
        x = self.fc3(x)
        return x

Target_net = DQN(NUM_STATES, NUM_ACTIONS)
Pred_net = DQN(NUM_STATES, NUM_ACTIONS)
optimizer = optimizer.Adam(Pred_net.parameters(), lr=LR)

Target_net.load_state_dict(Pred_net.state_dict())
# Target_net.train()

def plot(episode_list):
    plt.figure()
    plt.title('Mean Duration time of 10 episodes')
    plt.xlabel('index')
    plt.ylabel('Mean Duration time')
    plt.plot(episode_list)
    plt.show()

def select_action(state_input, time_idx):
    epsilon = 1. / ((time_idx // 100) + 1)
    if np.random.randn() < epsilon:
        action = np.random.choice(NUM_ACTIONS)
    else:
        with torch.no_grad():
            state_input = torch.FloatTensor(state_input)
            action = torch.argmax(Pred_net(state_input)).numpy()

    return action

env = gym.make('CartPole-v0')
memory = replay_buffer(1000)
duration_list = []
mean_epi_duration = torch.zeros(NUM_EPISODES)

# Train
for i_episode in range(NUM_EPISODES):

    env.render()
    state = env.reset()
    done = False
    episode_duration = 0

    while not done:
        action = select_action(state, TIME_INDEX)
        next_state, reward, done, _ = env.step(action)

        if done:
            next_state = None

        memory.push(state, action, next_state, reward)

        if len(memory) > BATCH_SIZE:
            transitions = memory.sample(BATCH_SIZE)
            batch = Transition(*zip(*transitions))

            non_final_mask = torch.BoolTensor(tuple(map(lambda s: s is not None,
                                                    batch.next_state)))

            non_final_next_states = torch.Tensor(np.vstack([s for s in batch.next_state if s is not None]))
            state_batch = torch.Tensor(np.vstack(batch.state))
            action_batch = torch.LongTensor(np.hstack(batch.action))
            reward_batch = torch.Tensor(np.hstack(batch.reward))

            q_values = Pred_net(state_batch).gather(1, action_batch.unsqueeze(1))
            target_q = torch.zeros(BATCH_SIZE)
            target_q[non_final_mask] = Target_net(non_final_next_states).max(1)[0].detach()

            expected_q_values = (target_q * GAMMA) + reward_batch

            loss = F.mse_loss(expected_q_values.unsqueeze(1), q_values)

            optimizer.zero_grad()
            loss.backward()
            for param in Pred_net.parameters():
                param.grad.data.clamp_(-1,1)
            optimizer.step()

        TIME_INDEX += 1
        state = next_state

        if i_episode % UPDATE_STANDARD == 0:
            Target_net.load_state_dict(Pred_net.state_dict())


        if done:
            print("%d th episode is finished after %d steps:, Duration: %d" %(i_episode+1, TIME_INDEX, episode_duration))
            duration_list.append(episode_duration)

            if i_episode == NUM_EPISODES-1:
                mean_epi_duration = torch.cat((mean_epi_duration, torch.Tensor(duration_list)))
                mean_epi_duration = mean_epi_duration.unfold(0, NUM_EPISODES, 1).mean(1).view(-1)

            break

        episode_duration += 1

plot(mean_epi_duration.numpy())
print('Finished')
