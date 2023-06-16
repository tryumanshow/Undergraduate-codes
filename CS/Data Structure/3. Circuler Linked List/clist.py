from node import Node

class CList:

    # 빈 연결 리스트 생성
    def __init__(self):
        self.head = None # CList의 마지막 노드

    # 빈 리스트인지 확인
    def is_empty(self):
        return self.head == None

    # 연결 리스트의 첫 위치에 item 삽입
    def add(self, item):
        temp = Node(item)
        if self.is_empty():
            temp.set_next(temp)
            self.head = temp
        else:
            temp.set_next(self.head.get_next())
            self.head.set_next(temp)

    # 연결 리스트의 마지막 위치에 item 삽입
    def append(self, item):
        temp = Node(item)
        if self.is_empty():
            temp.set_next(temp)
            self.head = temp
        else:
            temp.set_next(self.head.get_next())
            self.head.set_next(temp)
            self.head = temp

    # 연결 리스트의 첫 노드를 삭제
    def pop_first(self):
        if self.head == None:
            print("List is empty.")
        else:
            temp = self.head.get_next()
            if temp == temp.get_next():
                self.head = None
            else:
                self.head.set_next(temp.get_next())

    # 연결 리스트 내에 찾고자하는 item이 있으면 True를 반환
    def search(self, item):
        if self.head == None:
            print("List is empty.")
        else:
            temp = self.head.get_next()
            # if self.head == temp: # 노드 한 개로 구성된 리스트라면?
            #     if self.head.get_item() == item:
            #         return True
            #     else:
            #         return False
            if temp == temp.get_next():
                if temp.get_item() == item:
                    return True
                else:
                    return False
            found = False
            current = temp
            while True:
                if current.get_item() == item:
                    found = True
                else:
                    current = current.get_next()
                if current != temp and not found:
                    continue
                else:
                    break

            return found