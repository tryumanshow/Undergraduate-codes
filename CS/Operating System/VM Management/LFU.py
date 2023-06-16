import numpy as np
from until_full import until_full
from df_format import df_format

class LRU:

    def __init__(self, n, m, w, k, prs):
        self.n = int(n)
        self.m = int(m)
        self.w = int(w)
        self.k = int(k)
        self.prs= prs

        self.memory_state = np.array([-1 for _ in range(0, self.m*self.k)]).reshape(self.m, self.k)
        self.page_fault = [0]*self.k
        self.prs_ref_cnt = dict()

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
                self.prs_ref_cnt[self.prs[0]] = 1
            else:
                if ((self.prs[col] not in self.prs[0:col-1])):
                    self.memory_state[row,col] = self.prs[col]
                    row = row+1
                    self.prs_ref_cnt[self.prs[col]] = 1
                else:
                    self.memory_state[row, self.threshold] = self.prs[self.threshold]
                    self.prs_ref_cnt[self.prs[col]] += 1

        for i in range(0, self.m):
            for j in range(0, self.threshold+1):
                if self.memory_state[i,j] != -1:
                    self.memory_state[i, j:self.threshold+1] = self.memory_state[i, j]
                    if (self.memory_state[:, j] != self.memory_state[:, j - 1]).any(): self.page_fault[j] = 1
                    break

        return self.memory_state


    def _mem_state_after_full(self):

        for i in range(self.threshold+1, self.k):
            # i번째 ref_string의 값이 i-1번째 column의 memory state 안에 있다면?
            # => reference count만 update 시켜주면 될 뿐, 다른 것들은 건드릴 필요 없음.
            if self.prs[i] in self.memory_state[:, i-1]:
                self.memory_state[:, i] = self.memory_state[:, i-1]
                self.prs_ref_cnt[self.prs[i]] += 1

            # 없다면? => Page Fault 발생
            # 현재 시점의 Ref.string이 바로 전 시점의 Memory State에 없는 Case
                # 1. 그렇다면 직전 시점의 Memory State 내 모든 Ref String들의 Reference Count를 참조
                    # 2. 중복되는 Count가 없다면 최소 count를 갖는 Page Frame을 교체
                    # 3. 중복되는 Count가 있다면 그 중에서도 Tie-Breaking Rule로서 가장 최소의 것 찾기.
            else:
                temp_row_index = []

                newDict = dict()
                for (key, value) in self.prs_ref_cnt.items():
                    if key in self.memory_state[:, i-1]:
                        newDict[key] = value

                temp_min = min(newDict, key=newDict.get)

                for k1, k2 in enumerate(self.memory_state[:,i-1]):
                    if self.prs_ref_cnt[k2] == self.prs_ref_cnt[temp_min]:
                        temp_row_index.append(k1)

                # 있는데, 최소 # of referred 를 가진 페이지가 하나인 경우
                if len(temp_row_index) == 1:
                    self.memory_state[temp_row_index[0], i] = self.prs[i]

                    # 현재 Time 시점에서의 Ref.string이 이전에 참조된 적이 있었는지.
                    if self.prs[i] in list(self.prs_ref_cnt.keys()):
                        self.prs_ref_cnt[self.prs[i]] += 1
                    else: self.prs_ref_cnt[self.prs[i]] = 1

                    rest = set(list(range(self.m))) - {temp_row_index[0]}
                    for _, l in enumerate(list(rest)): self.memory_state[l, i] = self.memory_state[l, i-1]

                elif len(temp_row_index) > 1:
                    temp_cnt = [0] * len(temp_row_index)

                    for t1, t2 in enumerate(temp_row_index):
                        for d1 in range(i-1, -1, -1):
                            if self.prs[d1] != self.memory_state[t2, i-1]:
                                temp_cnt[t1] += 1
                            else: break

                    to_change = temp_row_index[temp_cnt.index(max(temp_cnt))]
                    self.memory_state[to_change, i] = self.prs[i]

                    rest = set(list(range(self.m))) - {to_change}

                    for _, l in enumerate(list(rest)): self.memory_state[l, i] = self.memory_state[l, i-1]

                    if self.prs[i] in list(self.prs_ref_cnt.keys()):
                        self.prs_ref_cnt[self.prs[i]] += 1
                    else: self.prs_ref_cnt[self.prs[i]] = 1

            if (self.memory_state[:, i] != self.memory_state[:, i-1]).any(): self.page_fault[i] = 1

        return self.memory_state

    def _page_fault(self):
        return self.page_fault


if __name__ == '__main__':

    n, m, w, k = input('N, M, W, K 값을 입력하세요: ').split()
    pfr = list(int(num) for num in input("Page Reference String 값을 입력하세요: ").strip().split())

    a = LRU(n, m, w, k, pfr)
    a1 = a._time()
    a2 = a._ref_string()
    a3 = a._mem_state_before_full()
    a4 = a._mem_state_after_full()
    a5 = a._page_fault()

    print('L F U a l g o r i t h m')
    print(df_format(a1, a2, a3, a4, a5).result())
    print('총 page fault 발생 횟수: %d번' % df_format(a1, a2, a3, a4, a5).num_of_pf())