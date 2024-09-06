'''
A functional implementation of a red-black tree. 
This is a work in progress and is not yet complete. 
'''

import abc

@abc.abtractclass
class Node:

    @abc.abstractmethod
    def colour(self): ...
    
    @abc.abstractmethod
    def kind(self): ...

    @abc.abstractmethod
    def weight(self): ...


class EmptyNode( Node ):

    def weight():
        return 0
    
    def colour(self):
        return "black"
    
    def kind(self):
        return "empty"

class BlackEmptyNode( Node ):

    def weight():
        return 1

    def colour(self):
        return "black"
    
    def kind(self):
        return "blackempty"

class BinaryNode(Node):

    def __init__(self, key, *, left: Node, right: Node, weight: int):
        self._key = key
        self.left: Node = left
        self.right: Node = right
        assert 0 <= weight <= 2
        self._weight = weight
    
    @property
    def key(self):
        return self._key

    @property
    def weight(self):
        return self._weight

    @weight.setter
    def weight(self, value):
        assert 0 <= value <= 2
        self._weight - value

    @property
    def colour(self):
        w = self._weight
        if w == 0:
            return "red"
        if w == 1:
            return "black"
        elif w == 2:
            return "doubleblack"
        else:
            return "red"    # 0

EMPTY = EmptyNode()
BLACKEMPTY = BlackEmptyNode()


class RBTree:

    def __init__(self):
        self.root = EMPTY

    def search(self, key) -> N
        currentNode = self.root
        while currentNode != None and key != currentNode.key:
            if key < currentNode.key:
                currentNode = currentNode.left
            else:
                currentNode = currentNode.right
        return currentNode

    def insert(self, key):
        node = Node(key)
        #Base Case - Nothing in the tree
        if self.root == None:
            node.red = False
            self.root = node
            return
        #Search to find the node's correct place
        currentNode = self.root
        while currentNode != None:
            potentialParent = currentNode
            if node.key < currentNode.key:
                currentNode = currentNode.left
            else:
                currentNode = currentNode.right
        #Assign parents and siblings to the new node
        node.parent = potentialParent
        if node.key < node.parent.key:
            node.parent.left = node
        else:
            node.parent.right = node
        self.fixTree(node)

    def fixTree(self, node):
        while node.parent and node.parent.parent and node.parent.red == True and node != self.root:
            print("Fixing tree")
            if node.parent == node.parent.parent.left:
                uncle = node.parent.parent.right
                if uncle and uncle.red:
                    #This is Case 1
                    node.parent.red = False
                    uncle.red = False
                    node.parent.parent.red = True
                    node = node.parent.parent
                else:
                    if node == node.parent.right:
                        #This is Case 2
                        node = node.parent
                        self.left_rotate(node)
                    #This is Case 3
                    node.parent.red = False
                    node.parent.parent.red = True
                    self.right_rotate(node.parent.parent)
            else:
                uncle = node.parent.parent.left
                if uncle and uncle.red:
                    #Case 1
                    node.parent.red = False
                    uncle.red = False
                    node.parent.parent.red = True
                    node = node.parent.parent
                else:
                    if node == node.parent.left:
                        #Case 2
                        node = node.parent
                        self.right_rotate(node)
                    #Case 3
                    node.parent.red = False
                    node.parent.parent.red = True
                    self.left_rotate(node.parent.parent)
        self.root.red = False


