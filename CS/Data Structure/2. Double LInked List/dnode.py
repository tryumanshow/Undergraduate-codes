class Node:
    def __init__(self, item):
        self.item = item
        self.previous = None
        self.next = None

    def get_item(self):
        return self.item

    def get_previous(self):
        return self.previous

    def get_next(self):
        return self.next

    def set_item(self, new_item):
        self.item = new_item

    def set_previous(self, new_previous):
        self.previous = new_previous

    def set_next(self, new_next):
        self.next = new_next

if __name__=='__main__':
    a = Node(10)
    b = a.set_next(11)
    print(a.get_item(), a.get_previous(), a.get_next())
