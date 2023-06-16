import pandas as pd

class df_format:

    def __init__(self, obj1, obj2, obj3, obj4, obj5):
        self.obj1 = obj1
        self.obj2 = obj2
        self.obj4 = obj4
        self.obj5 = obj5

    # 총 page fault, 메모리 상태 변화 과정 등 관심있는 정보들을 모두 모아 하나의 데이터 프레임에 표현
    def result(self):
        pd1 = pd.DataFrame(self.obj1).T
        pd2 = pd.DataFrame(self.obj2).T
        pd4 = pd.DataFrame(self.obj4)
        pd5 = pd.DataFrame(self.obj5).T

        df = pd.concat([pd1, pd2, pd4, pd5], axis=0)
        df.reset_index(drop=True, inplace=True)

        return df

    # Pagefault의 총 발생 횟수를 출력
    def num_of_pf(self):
        num = sum(self.obj5)
        return num