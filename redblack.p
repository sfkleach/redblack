;;; Node structure for the Red-Black Tree
defclass node {
    node_key,
    node_value,
    node_weight: 8,
    node_left,
    node_right
};

define node_colour( node );
    lvars w = node.node_weight;

enddefine;

define newNode(key, value, colour);
    consNode( key, value, colour, false, false, false )
enddefine;

procedure( node );
    pr( '<node ' );
    pr( node.node_key );
    pr( ': ' );
    pr( node.node_value );
    pr( '>' )
endprocedure -> class_print( Node_key );

defclass RedBlack {
    redblack_root,
    redblack_cmp
};

define newRedBlack();
    consRedBlack(
        false,
        procedure( a, b );
            if a == b then 0 else alphabefore(a, b) and -1 or 1 endif
        endprocedure
    )
enddefine;


define show(tree);
    define lconstant procedure show_node( level, node );
        repeat level times pr(' ') endrepeat;
        if node then
            pr( node.node_key );
            pr( ': ' );
            pr( node.node_value );
            pr( ', ' );
            pr( node.color );
            nl( 1 );
            show_node( level+1, node.left );
            show_node( level+1, node.right );
        else
            npr( 'nil' )
        endif
    enddefine;
    npr( 'RedBlack' );
    show_node( 1, tree.redblack_root );
    npr( 'endRedBlack' );
enddefine;


define searchTree(key, self);
    lvars currentNode = self.redblack_root;
    lvars procedure cmp = redblack_cmp(self);
    lvars N;
    while currentNode do
        lvars N = fi_check(cmp(key, currentNode.node_key), -1, 1);
        returnif( N == 0 )( currentNode );
        lvars procedure LR = if N == -1 do left else right endif;
        LR( currentNode ) -> currentNode
    endwhile;
    false
enddefine;

define rot(x, tree, L, R);
    lvars y = x.R;
    y.left -> x.R;
    if y.L then
        x -> y.L.parent;
    endif;
    x.parent -> y.parent;
    unless x.parent do
        y -> tree.redblack_root
    elseif x == x.parent.L then
        y -> x.parent.L
    else
        y -> x.parent.R
    endunless;
    x -> y.L;
    y -> x.parent;
enddefine;

;;; Utility function to perform left rotation.
define leftRotate( x, tree );
    rot(x, tree, left, right)
enddefine;

;;; Utility function to perform right rotation
define rightRotate( x, tree );
    rot(x, tree, right, left)
enddefine;

;;; Function to fix Red-Black Tree properties after
;;; insertion.
define fixTree(node, tree);
    lvars root_node = tree.redblack_root;
    returnunless( root_node );
    lvars P = node and node.parent;
    lvars G = P and P.parent;
    while G and P.color == "RED" do
        [fix1] =>
        lvars procedure L; ;;; = left;
        lvars procedure R; ;;; = right;
        if P == G.left then
            left, right
        else
            right, left
        endif -> (L, R);
        [fix2] =>
        lvars u = G.R; ;;; uncle
        if u and u.color == "RED" then
            [fix ^u] =>
            "BLACK" -> P.color;
            "BLACK" -> u.color;
            "RED" -> G.color;
            G -> node;
        else
            [fix ^node]  =>
            if node == P.R then
                P -> node;
                node and node.parent -> P;
                P and P.parent -> G;
                rot(node, tree, L, R);
            endif;
            "BLACK" -> P.color;
            "RED" -> G.color;
            rot(G, tree, R, L);
        endif
    endwhile;
    "BLACK" -> tree.redblack_root.color;
enddefine;

define update(value, key, tree);
    ;;; Synthesize the new node.
    lvars node = newNode(key, value, "RED");

    ;;; Base Case - Nothing in the tree
    returnunless(tree.redblack_root)(
        "BLACK" -> node.color,
        node -> tree.redblack_root
    );

    ;;; Search to find the node's correct place.
    lvars procedure cmp = tree.redblack_cmp;
    lvars currentNode = tree.redblack_root;
    while currentNode do
        lvars potentialParent = currentNode;
        lvars N = fi_check(cmp(node.node_key, currentNode.node_key), -1, 1);
        if N == -1 then
            currentNode.left -> currentNode
        elseif N == 0 then
            return(
                value -> currentNode.node_value
            )
        else
            currentNode.right -> currentNode
        endif
    endwhile;

    ;;; Assign parents and siblings to the new node.
    potentialParent -> node.parent;
    lvars N = fi_check(cmp(node.node_key, node.parent.node_key), -1, 1);
    lvars procedure LR = if N == -1 then left else right endif;
    node -> potentialParent.LR;
    fixTree(node, tree)
enddefine;

define appredblack(procedure p, tree);

    define lconstant procedure appnode(procedure p, node);
        if node.left then appnode(p, node.left) endif;
        p(node.node_key, node.node_value);
        if node.right then appnode(p, node.right) endif;
    enddefine;

    appnode(p, tree.redblack_root)
enddefine;
