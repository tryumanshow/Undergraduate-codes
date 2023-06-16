import numpy as np
import pandas as pd
from df_format import df_format

class WS:

    def __init__(self, n, m, w, k, prs):
        self.n = int(n)
        self.m = int(m)
        self.w = int(w)
        self.k = int(k)
        self.prs= prs

        self.memory_state = np.array(['-' for _ in range(0, self.n*self.k)]).reshape(self.n, self.k)
        self.page_fault = [0] * self.k

        # self.ref_set과 self.row_index를 갖고, self.memory_state의 indexing 후 값 filling에 사용할 것.

        self.row_ws_matching = dict() # page번호와 self.memory_state 간의 순서를 매칭하기 위한 dict

        self.ref_set = sorted(list(set(self.prs))) # sorting 시켜놓음.
        self.row_index = list(range(len(self.ref_set))) # 실제로 매칭을 진행
        for i, j in enumerate(self.ref_set):
            self.row_ws_matching[j] = i

        self.page_in = ['-'] * self.k
        self.page_out = ['-'] * self.k


    def _time(self):
        self.time = list(range(1, self.k + 1))
        return self.time

    def _ref_string(self):
        self.ref_string = self.prs
        return self.ref_string

    def _mem_state(self):

        # '매 timestamp마다 차례로 훑는다'는 생각.
        self.memory_in_role = [] # working set 안에 들어오는 page들

        for i in range(len(self.ref_string)): # 0은 먼저 채워주고 시작.

            if i == 0:
                self.memory_state[self.row_ws_matching[self.ref_string[i]], i] = self.ref_string[i]
                self.memory_in_role.append(self.ref_string[i])
                self.page_fault[i] = 1
                self.page_in[i] = self.ref_string[i]

            else:
                if i <= self.w:
                    self.memory_state[:, i] = self.memory_state[:, i-1]
                    self.memory_state[self.row_ws_matching[self.ref_string[i]], i] = self.ref_string[i]
                    self.memory_in_role.append(self.ref_string[i])

                    if self.ref_string[i] not in self.memory_in_role[:-1]:
                        self.page_fault[i] = 1
                        self.page_in[i] = self.ref_string[i]

                else:
                    self.memory_in_role = self.memory_in_role[-3:]
                    for l in self.memory_in_role:
                        self.memory_state[self.row_ws_matching[l], i] = self.memory_state[self.row_ws_matching[l], i-1]
                    self.memory_state[self.row_ws_matching[self.ref_string[i]], i] = self.ref_string[i]
                    self.memory_in_role.append(self.ref_string[i])

                    if self.ref_string[i] not in self.memory_in_role[-4:-1]:
                        self.page_fault[i] = 1
                        self.page_in[i] = self.ref_string[i]

                    if len(list(set(self.memory_state[:,i-1]) - set(self.memory_state[:,i]))) > 0:
                        self.page_out[i] = int(list(set(self.memory_state[:,i-1]) - set(self.memory_state[:,i]))[0])

        return self.memory_state

    def _page_fault_with_io(self):

        self.page_fault_with_io = pd.concat([pd.DataFrame(self.page_fault).T, pd.DataFrame(self.page_in).T,
                                            pd.DataFrame(self.page_out).T], axis=0)

        return self.page_fault_with_io


if __name__ == '__main__':
    n, m, w, k = input('N, M, W, K 값을 입력하세요: ').split()
    pfr = list(int(num) for num in input("Page Reference String 값을 입력하세요: ").strip().split())

    a = WS(n, m, w, k, pfr)
    a1 = a._time()
    a2 = a._ref_string()
    a3 = a._mem_state()
    a4 = a._page_fault_with_io()

    print('W S a l g o r i t h m')
    print(df_format(a1, a2, None, a3, a4.T).result())
    print('총 page fault 발생 횟수: %d번' % sum(a4.iloc[0,:]))
