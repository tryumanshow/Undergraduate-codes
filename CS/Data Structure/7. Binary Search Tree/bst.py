from btnode import Node

class BST:
    def __init__(self):
        self.root = None

    def search(self, k):
        return self._search(self.root, k)
    def _search(self, n, k):
        if n == None or n.get_key() == k:
            return n!= None
        elif n.get_key() > k:
            return self._search(n.get_left(), k)
        else:
            return self._search(n.get_right(), k)

    def insert(self, key):
        self.root = self._insert(self.root, key)
    def _insert(self, n, key):
        if n == None:
            return Node(key)
        if n.get_key() > key:
            n.set_left(self._insert(n.get_left(), key))
        elif n.get_key() < key:
            n.set_right(self._insert(n.get_right(), key))
        return n

    def find_min(self):
        if self.root == None:
            return None
        return self._find_min(self.root)
    def _find_min(self, n):
        if n.get_left() == None:
            return n
        return self._find_min(n.get_left())

    def delete_min(self):
        if self.root == None:
            print("Tree is empty.")
        self.root = self._delete_min(self.root)
    def _delete_min(self, n):
        if n.get_left() == None:
            return n.get_right()
        n.set_left(self._delete_min(n.get_left()))
        return n

    def delete(self, key):
        self.root = self._delete(self.root, key)
    def _delete(self, n, key):
        if n == None:
            return None
        if n.get_key() > key:
            n.set_left(self._delete(n.get_left(), key))
        elif n.get_key() < key:
            n.set_right(self._delete(n.get_right(), key))
        else:
            if n.get_left() == None and n.get_right() == None:
                return None
            if n.get_left() == None or n.get_right() == None:
                if n.get_left() == None:
                    return n.get_right()
                else:
                    return n.get_left()
            target = n
            n = self._find_min(target.get_right())
            n.set_right(self._delete_min(target.get_right()))
            n.set_left(target.get_left())
        return n



    def inorder(self, n):
        if n != None:
            if n.left:
                self.inorder(n.left)
            print(str(n.key), ' ', end='')
            if n.right:
                self.inorder(n.right)



if __name__ == '__main__':
    a = BST()
    a.insert(7)
    a.insert(2)
    a.insert(5)
    a.insert(9)
    print(a.inorder(a.root))
    a.delete(2)
    print(a.inorder(a.root))