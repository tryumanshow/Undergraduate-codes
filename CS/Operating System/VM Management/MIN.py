import numpy as np
from until_full import until_full
from df_format import df_format

class MIN:

    # 초기화
    def __init__(self, n, m, w, k, prs):
        self.n = int(n)
        self.m = int(m)
        self.w = int(w)
        self.k = int(k)
        self.prs= prs

        self.memory_state = np.array([-1 for _ in range(0, self.m*self.k)]).reshape(self.m, self.k) # 메모리 상의 상태변화를 2차원 array에 표현
        self.page_fault = [0]*self.k # Pagefault의 발생 양상을 리스트 내에 표현

    # 시간 출력
    def _time(self):
        self.time = list(range(1, self.k+1))
        return self.time

    # 입력받은 Reference String 출력
    def _ref_string(self):
        self.ref_string = self.prs
        return self.ref_string

    # 모든 Pageframe이 메모리 상에 다 차기 이전까지의 memory state 출력
    def _mem_state_before_full(self):

        # self.threshold의 시점이면, 메모리는 FULL
        self.threshold = until_full(self.m, self.prs).operation()

        row = 0 # row: pageframe을 채우면 다음 pageframe으로 이동. 순서대로 하나씩 순회하기 위한 목적.
        for col in range(0, self.threshold+1):
            if col == 0: # 일단 맨 첫 시점은 메모리의 첫번째 공간을 채우고 시작
                self.memory_state[row,0] = self.prs[0]
                row = row+1
            else:
                # 현 시점에 들어온 page가, 이전 시점에 memory 상에 존재했던 page 내에 없을 경우 :빈 page frame에 채운다.
                if ((self.prs[col] not in self.prs[0:col-1]) and row < self.threshold):
                    self.memory_state[row,col] = self.prs[col]
                    row = row+1
                # 현 시점에 들어온 page가, 이전 시점에 memory 상에 존재했던 page 내에 있을 경우. : 이전 시점의 pageframe 할당 상태를 그대로 가져온다.
                else: self.memory_state[row, self.threshold] = self.prs[self.threshold]

        # 현재, 매 timestamp 마다 하나씩의 pageframe만 할당되어 있는 상태.
        # 각 memory 공간들을 차례대로 순회하면서, 처음으로 pageframe이 할당되어 있는 timestamp를 만나면,
        # 그 시점부터 메모리 공간이 다 차는 시점까지 동일한 pageframe으로 모두 채운다.

        # 현시점과 이전시점의 pageframe 분포 중 단 하나라도 다른 것이 있다면 pagefault 발생.
        for i in range(0, self.m):
            for j in range(0, self.threshold+1):
                if self.memory_state[i,j] != -1:
                    self.memory_state[i, j:self.threshold+1] = self.memory_state[i, j]
                    if (self.memory_state[:, j] != self.memory_state[:, j - 1]).any(): self.page_fault[j] = 1
                    break

        return self.memory_state

    # 모든 page frame이 다 차고난 후의 memory state 관리
    def _mem_state_after_full(self):

        self.left_col_index = list(range(self.threshold+1, self.k)) # 2차원 array 상의 column (시간)
        self.all_row_index = list(range(self.m)) # 2차원 array 상의 row (pageframe)

        for col in self.left_col_index:

            appearance = [0] * self.m
            row_temp =list(range(self.m))

            # Tie-break 적용 여부 판단을 위해, 현 시점의 각 pageframe들에 대해 Forward Distance가 Inf라면 999를 할당
            for row in self.all_row_index:
                try:
                    temp = self.ref_string[col:].index(self.memory_state[row, col-1]) + 1
                    appearance[row] = temp
                except ValueError:
                    appearance[row] = 999

            # 만약 Forward Distance가 Inf인 경우가 존재한다면?
            if 999 in appearance:
                inf_idx = [i for i in range(len(appearance)) if appearance[i]==999] # 현재 시점의 memory state에 대해 ref.string 존재하지 않는 page의 row index
                inf = self.memory_state[inf_idx, col-1]
                not_inf = self.memory_state[list(set(row_temp) - set(inf_idx)), col-1] # 현재 시점의 memory state에 대해 ref.string이 존재하는 page의 number

                # 999가 존재하지만, 현재 page frame 내에 다음 시점의 reference string이 있다면?: 이전 시점의 page frame 정보들로 채우면 됨.
                if self.ref_string[col] in not_inf:
                    self.memory_state[:, col] = self.memory_state[:, col-1]

                # 999가 존재하고, 현재 page frame 내에 다음 시점의 reference string도 없다면?
                else:
                    self.memory_state[row_temp.pop(inf_idx[0]), col] = self.ref_string[col] # 가장 빠른 차례의 page frame을 비우고 현재의 page로 교체.
                    for _, j in enumerate(row_temp): self.memory_state[j, col] = self.memory_state[j, col - 1]  # 나머지 page frame들은 그대로 hold

            # 만약 Forward Distance가 Inf인 경우가 존재하지 않는다면?
            else:
                if self.ref_string[col] not in self.memory_state[:, col-1]:
                    to_change = appearance.index(max(appearance)) # 가장 오랫동안 참조되지 않을 page를 선택
                    self.memory_state[to_change, col] = self.ref_string[col]; row_temp.pop(to_change) # 해당 page 교체
                    for _, j in enumerate(row_temp): self.memory_state[j, col] = self.memory_state[j, col-1] # 교체되지 않은 page frame 그대로 hold
                else:
                    self.memory_state[:, col] = self.memory_state[:, col-1]

            if (self.memory_state[:, col] != self.memory_state[:, col-1]).any():
                self.page_fault[col] = 1

        return self.memory_state

    def _page_fault(self):
        return self.page_fault


if __name__ == '__main__':
    n, m, w, k = input('N, M, W, K 값을 입력하세요: ').split()
    pfr = list(int(num) for num in input("Page Reference String 값을 입력하세요: ").strip().split())

    a = MIN(n, m, w, k, pfr)
    a1 = a._time()
    a2 = a._ref_string()
    a3 = a._mem_state_before_full()
    a4 = a._mem_state_after_full()
    a5 = a._page_fault()

    print('M I N a l g o r i t h m')
    print(df_format(a1, a2, a3, a4, a5).result())
    print('총 page fault 발생 횟수: %d번' %df_format(a1, a2, a3, a4, a5).num_of_pf())