Control.Print.printDepth := 100;
Control.Print.printLength := 100;
fun say s = TextIO.output(TextIO.stdOut,s);
fun sayln s =(say s;say "\n");
fun sayi s = say(Int.toString(s));
fun sayiln s= (sayi s;say "\n");

signature BTREE =
sig
    type item;
    type 'a Tree;
    val initTree : 'a Tree;
    val insert : 'a Tree * item -> 'a Tree;
    val member : 'a Tree * item -> bool;
    val print : 'a Tree -> unit;
end;

signature ITEM =
sig
    type item;
    val ls : item * item -> bool;
    val eq : item * item -> bool;
    val out : item -> unit;
end;

functor BTree(Item : ITEM):BTREE =
struct type item = Item.item;
       datatype 'a tree = Empty
                        | Node of item * 'a tree * 'a tree;
       type 'a Tree = 'a tree;

       fun ls (p: item,q: item) = Item.ls(p,q);
       fun eq (p: item,q: item) = Item.eq(p,q);
       fun out (p:item) = Item.out(p);

       val initTree = Empty;

       fun insert (Empty,x) = Node(x,Empty,Empty)
         | insert (Node(Val,lchild,rchild),x) =
           if ls(x,Val) then (Node(Val,insert(lchild,x),rchild))
           else (Node(Val,lchild,insert(rchild,x)));

       fun member (Empty,x) = false
         | member (Node(Val,lchild,rchild),x) =
           if eq(x,Val) then true
           else
               if ls(x,Val) then member(lchild,x)
               else member(rchild,x);

       fun print (Empty) = say("")
         | print (Node(Val,lchild,rchild)) =
           (print(lchild);out(Val);print(rchild));

end;

structure MyIntItem : ITEM =
struct type item = int;
       fun ls (p:item,q:item):bool = p<q;
       fun eq (p:item,q:item):bool = p=q;
       fun out (p:item) = sayiln p;
end;

structure IntBTree = BTree(MyIntItem);

val root0 = IntBTree.initTree;

val root1 = IntBTree.insert(root0,3);
val root2 = IntBTree.insert(root1,7);
val root3 = IntBTree.insert(root2,8);
val root4 = IntBTree.insert(root3,4);
val root5 = IntBTree.insert(root4,6);
val root6 = IntBTree.insert(root5,5);
val root7 = IntBTree.insert(root6,1);
val root8 = IntBTree.insert(root7,0);
val root9 = IntBTree.insert(root8,2);
val root10 = IntBTree.insert(root9,9);

val find1 = IntBTree.member(root5,5);
val find1 = IntBTree.member(root6,5);

val test = IntBTree.print(root10);

structure MyStrItem : ITEM =
struct type item = string;
       fun ls (p:item,q:item):bool = p<q;
       fun eq (p:item,q:item):bool = p=q;
       fun out (p:item) = sayln p;
end;

structure StrBTree = BTree(MyStrItem);

val root0 = StrBTree.initTree;

val root1 = StrBTree.insert(root0,"kang");
val root2 = StrBTree.insert(root1,"xi");
val root3 = StrBTree.insert(root2,"shi");
val root4 = StrBTree.insert(root3,"qian");
val root5 = StrBTree.insert(root4,"long");
val root6 = StrBTree.insert(root5,"de");
val root7 = StrBTree.insert(root6,"baba");
val root8 = StrBTree.insert(root7,"yeye");
val root9 = StrBTree.insert(root8,"gege");
val root10 = StrBTree.insert(root9,"jiejie");

val find1 = StrBTree.member(root5,"de");
val find1 = StrBTree.member(root6,"de");

val test = StrBTree.print(root10);
