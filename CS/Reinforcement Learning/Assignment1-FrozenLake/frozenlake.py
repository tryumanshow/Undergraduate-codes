import numpy as np

LEFT = 0
DOWN = 1
RIGHT = 2
UP = 3

MAP = [
        "SFFF",
        "FHFH",
        "FFFH",
        "HFFG"
    ]

class FrozenLakeEnv():

    """
    Winter is here. You and your friends were tossing around a frisbee at the park
    when you made a wild throw that left the frisbee out in the middle of the lake.
    The water is mostly frozen, but there are a few holes where the ice has melted.
    If you step into one of those holes, you'll fall into the freezing water.
    The surface is described using a grid like the following

        SFFF
        FHFH
        FFFH
        HFFG

    S : starting point, safe
    F : frozen surface, safe
    H : hole, fall to your doom
    G : goal, where the frisbee is located
    The episode ends when you reach the goal or fall in a hole.
    You receive a reward of 1 if you reach the goal, and zero otherwise.
    """

    def __init__(self, is_slippery):

        self.map = np.asarray(MAP, dtype='c')
        nrow, ncol = 4, 4
        self.nA = 4
        self.nS = nrow * ncol

        # 4x4 정사각형 내에서 state #를 mapping (0~15)
        def to_s(row, col):
            return row * ncol + col

        def move(row, col, a):
            if a == 0:  # left
                col = max(col - 1, 0)
            elif a == 1:  # down
                row = min(row + 1, nrow - 1)
            elif a == 2:  # right
                col = min(col + 1, ncol - 1)
            elif a == 3:  # up
                row = max(row - 1, 0)
            return (row, col)

        mdp = list( )
        for i in range(self.nS):
            mdp.append([[], [], [], []]) #  각 state에 대한 4개의 action을 의미. (dim: 16 x 4 먼저 만들어놓고 시작)

        # grid world 생각
        for row in range(nrow):
            for col in range(ncol):
                s = to_s(row, col)
                for a in range(4):
                    letter = self.map[row, col]
                    if letter in b'GH': # 이미 episode가 종료된 경우: 해당 state에서 더이상 움직이지 않으며, reward도 당연히 0
                        mdp[s][a].append([1.0, s, 0])
                    else:
                        if is_slippery:
                            for b in [(a - 1) % 4, a, (a + 1) % 4]: # 상황 정의에 따른 것 (a: action)
                                newrow, newcol = move(row, col, b)
                                newstate = to_s(newrow, newcol)
                                newletter = self.map[newrow, newcol]
                                rew = float(newletter == b'G')
                                mdp[s][a].append((1.0 / 3.0, newstate, rew)) # (Defining MDP) 1/3의 확률로 new'state'로 가서, rew만큼의 reward를 얻는다.
                        else:
                            newrow, newcol = move(row, col, a)
                            newstate = to_s(newrow, newcol)
                            newletter = self.map[newrow][newcol]
                            rew = float(newletter == b'G')
                            mdp[s][a].append([1.0, newstate, rew])

        self.MDP = mdp