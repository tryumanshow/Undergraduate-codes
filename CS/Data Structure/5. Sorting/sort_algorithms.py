class sort_algo:

    def __init__(self, items):
        self.items = items

    # 선택정렬
    def selection_sort(self):
        for i in range(0, len(self.items)-1):
            minimum = i
            for j in range(i, len(self.items)):
                if self.items[minimum] > self.items[j]:
                    minimum = j
            self.items[i], self.items[minimum] = self.items[minimum], self.items[i]

        return self.items

    # 삽입정렬
    def insertion_sort(self):
        for i in range(1, len(self.items)):
            for j in range(i, 0, -1):
                if self.items[j-1] > self.items[j]:
                    self.items[j], self.items[j-1] = self.items[j-1], self.items[j]
                else:
                    break

        return self.items

    # 쉘 정렬
    def shell_sort(self):
        h = len(self.items) // 2
        while h >= 1:
            for i in range(h, len(self.items)):
                j = i
                while j >= h and self.items[j] < self.items[j-h]:
                    self.items[j], self.items[j-h] = self.items[j-h], self.items[j]
                    j -= h
            print("{}-정렬 결과: ".format(h), self.items)
            h //= 2

        return self.items

class heap_sort:
    def __init__(self, items):
        self.items = items

    def downheap(self, i, size):
        while 2*i + 1 <= size:
            k = 2*i + 1
            if k < size-1 and self.items[k] < self.items[k+1]:
                k+=1
            if self.items[i] >= self.items[k]:
                break
            self.items[i], self.items[k] = self.items[k], self.items[i]
            i=k

    def heapify(self):
        hsize = len(self.items)
        for i in range(hsize//2-1, -1, -1):
            self.downheap(i, hsize)
        return self.items

    def heap_sort(self):
        N = len(self.items)
        for i in range(N):
            self.items[0], self.items[N-1] = self.items[N-1], self.items[0]
            self.downheap(0, N-2)
            N -= 1

        return self.items




if __name__ == '__main__':
    items = [40, 70, 60, 30, 10, 50]
    print('정렬 전: ', end='')
    print(items)
    print('선택정렬 후: ', end='')
    print(sort_algo(items).selection_sort())
    print('삽입정렬 후: ', end='')
    print(sort_algo(items).insertion_sort())

    items2 = [39, 23, 15, 47, 11, 56, 61, 16, 12, 19, 21, 41]
    print('정렬 전: ', end='')
    print(items2)
    print('셸정렬 후: ', end='\n')
    print(sort_algo(items2).shell_sort())

    items2 = [39, 23, 15, 47, 11, 56, 61, 16, 12, 19, 21, 41]
    print('정렬 전: ', end='')
    print(items2)
    a = heap_sort(items2)
    print('최대 힙: ', end='')
    print(a.heapify())
    print('힙정렬 후: ', end='\n')
    print(a.heap_sort())
