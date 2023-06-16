from dnode import Node
class DList:

    # 초기화
    def __init__(self):
        self.head = None

    # 빈 리스트인지 확인
    def is_empty(self):
        return self.head == None

    # 리스트 내 노드의 개수 반환
    def size(self):
        current = self.head
        count = 0
        while current != None:
            count += 1
            current = current.get_next()
        return count

    # 연결리스트의 첫 위치에 item을 저장하는 새로운 노드 삽입
    def add(self, item):
        if self.head == None:
            self.head = Node(item)
        else:
            temp = Node(item)
            temp.set_next(self.head)
            self.head.set_previous(temp)
            self.head=temp

    # 연결리스트의 마지막 위치에 item을 저장하는 새로운 노드를 삽입
    def append(self, item):
        if self.head == None:
            self.head = Node(item)
        else:
            current = self.head
            while current.get_next() != None:
                current = current.get_next()
            temp = Node(item)
            current.set_next(temp)
            temp.set_previous(current)

    # 특정 항목을 저장하고 있는 노드의 다음 위치에 item을 저장하는 새로운 노드 삽입
    def insert_after(self, item, x):
        if self.head == None:
            print("List is empty.")
        else:
            current = self.head
            found = False
            while current != None and not found:
                if current.get_next() == x:
                    found = True
                else:
                    current = current.get_next()

            if found == False:
                print("Item is not in the list.")
            else:
                temp = Node(item)
                temp.set_next(current.get_next())
                temp.set_previous(current)
                if current.get_next() != None:
                    current.get_next().get_previous(temp)
                current.set_next(temp)

    # 특정 항목을 저장하고 있는 노드의 이전 위치에 item을 저장하는 새로운 노드 삽입
    def insert_before(self, item, x):
        if self.head == None:
            print("List is empty.")

        else:
            current = self.head
            found = False
            while current != None and not found:
                if current.get_item() == x:
                    found = True;
                else:
                    current = current.get_next()

            if found == False:
                print("Item is not in the list.")
            else:
                temp = Node(item)
                temp.set_next(current)
                temp.set_previous(current.get_previous())
                if current.get_previous() != None:
                    current.get_previous().set_next(temp)
                else:
                    self.head = temp
                current.set_previous(temp)

    # 연결 리스트에 찾고자 하는 item을 저장하고 있는 노드가 존재하면 True를 반환
    def search(self, item):
        current = self.head
        found = False
        while current != None and not found:
            if current.get_item() == item:
                found = True
            else:
                current = current.get_next()
        return found

    # 연결리스트의 첫 노드를 삭제
    def pop_first(self):
        if self.head == None:
            print("List is empty.")
        else:
            if self.head.get_next() == None:
                self.head = None
            else:
                self.head = self.head.get_next()
                self.head.set_previous(None)

    # 연결 노드의 마지막 노드를 삭제
    def pop_last(self):
        if self.head == None:
            print("List is empty.")
        else:
            if self.head.get_next() == None:
                self.head = None
            else:
                current = self.head
                while current.get_next() != None:
                    current = current.get_next()
                current.get_previous().set_next(None)

    # 연결 리스트에서 특정 item을 저장하고 있는 기존 노드 삭제 (반드시 해당 item을 저장하고 있는 노드가 존재함을 가정)
    def delete(self, item):
        if self.head.get_item() == item:
            self.head = self.head.get_next()
            self.head.set_previous(None)
        else:
            current = self.head
            found = False
            while not found:
                if current.get_next() == item:
                    found = True
                else:
                    current = current.get_next()

            if current.get_next() != None:
                current.get_previous().set_next(current.get_next())
                current.get_next().set_previous(current.get_previous())
            else:
                current.get_previous().set_next(None)


if __name__=='__main__':
    d = DList()
    d.add(30)
    d.add(20)
    d.add(15)
    d.append(35)
    d.append(90)
    d.pop_last()
    d.pop_first()
    d.delete(20)
    current = d.head
    while current:
        if current.get_next() != None:
            print(current.get_item(), '<=> ', end='')
        else:
            print(current.get_item())
        current = current.get_next()
