import numpy as np
import copy

def policy_evaluation(env, V, policy, gamma=0.99, theta=1e-8):
    while True:
        V_current = copy.deepcopy(V)
        delta = 0
        for i in range(env.nS):
            expected_return = 0
            for j in range(env.nA):
                prob, next_state, reward = env.MDP[i][j][0]
                expected_return += policy[i,j] * (prob * (reward + gamma * V_current[next_state]))
            V[i] = expected_return
        delta = max(delta, max(np.abs(V_current - V)))
        V_current = V

        if delta < theta:
            break

    return V_current

def policy_improvement(env, V, gamma=0.99):
    new_policy = np.zeros([env.nS, env.nA])
    for i in range(env.nS):
        max_value = -np.float('inf')
        max_action = 0
        for j in range(env.nA):
            prob, next_state, reward = env.MDP[i][j][0]
            result = prob * (reward + gamma * V[next_state])
            if result > max_value:
                max_action = j
                max_value = result
            if j == env.nA-1:
                new_policy[i][max_action] = 1

    return new_policy


def policy_iteration(env, gamma=0.99, theta=1e-8):
    V = np.zeros(env.nS)
    old_policy = np.ones([env.nS, env.nA]) / env.nA
    policy_stable = False
    while policy_stable != True:
        new_V = policy_evaluation(env, V, old_policy, gamma, theta)
        policy = policy_improvement(env, new_V, gamma)
        if np.array_equal(old_policy, policy):
            old_policy = policy
            break
        else:
            old_policy = policy
            V = new_V

    return old_policy, V

def value_iteration(env, gamma=0.99, theta=1e-8):
    V = np.zeros(env.nS)
    policy = np.zeros([env.nS, env.nA])
    value_stable = False


    while value_stable != True:
        delta = 0
        V_current = copy.deepcopy(V)
        for i in range(env.nS):
            max_value = -np.float('inf')
            for j in range(env.nA):
                result = 0
                for k in range(len(env.MDP[i][j])):
                    prob, next_state, reward = env.MDP[i][j][k]
                    result += prob * (reward + gamma * V_current[next_state])
                if result > max_value:
                    max_value = result
            V[i] = max_value
        delta = max(delta, max(np.abs(V - V_current)))
        V_current = V
        if delta < theta:
            break

    for i in range(env.nS):
        max_value = -np.float('inf')
        max_action = 0
        for j in range(env.nA):
            prob, next_state, reward = env.MDP[i][j][0]
            result = prob * (reward + gamma * V_current[next_state])
            if result > max_value:
                max_value = result
                max_action = j

        policy[i, max_action] = 1

    return policy, V