'''
A functional implementation of a red-black tree. 
This is a work in progress and is not yet complete. 
'''

class Node:

    def __init__(self, key, *, left=None, right=None):
        self.key = key
        self.left = left or EMPTY
        self.right = right or EMPTY
        self.bits = 0 if key is None else 1
        
    def is_empty(self):
        return (self.bits & 0b1) == 0
    
    @property
    def weight(self):
        self.bits >> 1 

    @weight.setter
    def weight(self, value):
        assert -1 <= value <= 2
        self.bits = (self.bits & 0b1) | (value << 1)

    @property
    def colour(self):
        e = self.is_empty()
        w = self.weight
        if w == 0:
            return "black" if e else "red"
        elif w == 1:
            return "black"
        elif w == 2:
            return "black"
        else:
            return "red"

EMPTY = Node(None)
EMPTY.bits = 0
EMPTY.left = EMPTY
EMPTY.right = EMPTY

BLACK_EMPTY = Node(None)
BLACK_EMPTY.weight = 1
BLACK_EMPTY.left = EMPTY
BLACK_EMPTY.right = EMPTY




class RBTree:
    def __init__(self):
        self.root = None

    def search(self, key):
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


