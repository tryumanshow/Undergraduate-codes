class until_full:

    def __init__(self, n_pageframe, prs):
        # n_pageframe: 할당 페이지 프레임의 개수
        # prs: 입력받은 page reference string

        assert n_pageframe >= 2
        self.n_pageframe = n_pageframe
        self.prs = prs

    def operation(self):

        # 할당되는 unique한 page frame의 개수는 반드시 두 개 이상이어야 한다.
        threshold = 2 # pageframe이 꽉 차게 될 때의 time index

        while(1):
            prs_set = list(set(self.prs[0:threshold]))

            if len(prs_set) == self.n_pageframe: break
            else: threshold = threshold + 1

        return threshold-1