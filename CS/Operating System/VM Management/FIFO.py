import numpy as np
from until_full import until_full
from df_format import df_format

class FIFO:

    def __init__(self, n, m, w, k, prs):
        self.n = int(n)
        self.m = int(m)
        self.w = int(w)
        self.k = int(k)
        self.prs= prs

        self.memory_state = np.array([-1 for _ in range(0, self.m*self.k)]).reshape(self.m, self.k)
        self.page_fault = [0]*self.k

    def _time(self):
        self.time = list(range(1, self.k+1))
        return self.time

    def _ref_string(self):
        self.ref_string = self.prs
        return self.ref_string

    # 모든 page frame이 다 차기 전까지의 memory state 관리
    def _mem_state_before_full(self):
        self.threshold = until_full(self.m, self.prs).operation()

        row = 0
        for col in range(0, self.threshold+1):
            if col == 0:
                self.memory_state[row,0] = self.prs[0]
                row = row+1
            else:
                if ((self.prs[col] not in self.prs[0:col-1]) and row < self.threshold):
                    self.memory_state[row,col] = self.prs[col]
                    row = row+1
                else: self.memory_state[row,self.threshold] = self.prs[self.threshold]

        for i in range(0, self.m):
            for j in range(0, self.threshold+1):
                if self.memory_state[i,j] != -1:
                    self.memory_state[i, j:self.threshold+1] = self.memory_state[i, j]
                    if (self.memory_state[:, j] != self.memory_state[:, j - 1]).any(): self.page_fault[j] = 1
                    break

        return self.memory_state

    # 모든 page frame이 다 차고난 후의 memory state 관리
    def _mem_state_after_full(self):

        backward = [0]*self.m

        for i in range(self.threshold+1, self.k): # 오른쪽으로 나아가면서 Reference String을 훑는다.
            for j in range(len(backward)): # 각 page frame 순으로 왼쪽으로 나아가면서 page frame의 최초 진입 시점을 훑는다.
                temp_cnt = 1
                for col in range(i-1, -1, -1): # 왼쪽으로 나아가면서 page frame 마다의 최초 진입 시점을 훑는다.
                    if self.memory_state[j, col]  == self.memory_state[j, col-1]:
                        temp_cnt += 1
                    else:
                        backward[j] = temp_cnt
                        break

            biggest = backward.index(max(backward)) # 그 값이 가장 큰 page frame이 들어온지 가장 오래된 page를 담고 있을 것.

            if self.ref_string[i] in self.memory_state[:, i-1]:
                self.memory_state[:, i] = self.memory_state[:, i-1]
            else:
                self.memory_state[biggest, i] = self.ref_string[i]
                rest = set(list(range(self.m))) - {biggest}
                for _, l in enumerate(list(rest)): self.memory_state[l, i] = self.memory_state[l, i-1]

            if (self.memory_state[:, i] != self.memory_state[:, i-1]).any():
                self.page_fault[i] = 1

        return self.memory_state

    def _page_fault(self):
        return self.page_fault

if __name__ == '__main__':
    n, m, w, k = input('N, M, W, K 값을 입력하세요: ').split()
    pfr = list(int(num) for num in input("Page Reference String 값을 입력하세요: ").strip().split())

    a = FIFO(n, m, w, k, pfr)
    a1 = a._time()
    a2 = a._ref_string()
    a3 = a._mem_state_before_full()
    a4 = a._mem_state_after_full()
    a5 = a._page_fault()

    print('F I F O a l g o r i t h m')
    print(df_format(a1, a2, a3, a4, a5).result())
    print('총 page fault 발생 횟수: %d번' %df_format(a1, a2, a3, a4, a5).num_of_pf())