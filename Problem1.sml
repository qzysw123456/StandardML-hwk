Control.Print.printDepth := 100;
Control.Print.printLength := 100;
fun less (x,y) = x<y;
fun lessStr (x :string,y :string):bool = x<y;
fun equal (x,y) = x=y;

datatype 'a tree = empty
              | Node of 'a * 'a tree * 'a tree;
fun insert (empty,x,ls) = Node(x,empty,empty)
  | insert (Node(ItemVal,left,right),x,ls) =
    if ls(x,ItemVal)
    then Node(ItemVal,insert(left,x,ls),right)
    else Node(ItemVal,left,insert(right,x,ls));
fun member (empty,x,ls,eq) = false
  | member (Node(ItemVal,left,right),x,ls,eq) =
    if(eq(ItemVal,x))
    then true
    else if(ls(x,ItemVal))
    then member(left,x,ls,eq)
    else member(right,x,ls,eq);

fun say s = TextIO.output(TextIO.stdOut,s);
fun sayi s = say(Int.toString(s));
fun sayiln s= (sayi s;say "\n");
fun sayln s =(say s;say "\n");
fun printtree (empty,prt) = say("")
  | printtree (Node(item,left,right),prt) =
    (printtree(left,prt);prt(item);printtree(right,prt))


val root1 = empty;
val root2 = insert(root1,5,less);
val root3 = insert(root2,3,less);
val root4 = insert(root3,8,less);
val root5 = insert(root4,2,less);
val root6 = insert(root5,4,less);
val root7 = insert(root6,6,less);
val root8 = insert(root7,7,less);
val root9 = insert(root8,1,less);
val root10 = insert(root9,9,less);
member(root7,7,less,equal);
member(root8,7,less,equal);
printtree(root10,sayiln);

val root1 = empty;
val root2 = insert(root1,"wo",lessStr);
val root3 = insert(root2,"zhen",lessStr);
val root4 = insert(root3,"de",lessStr);
val root5 = insert(root4,"fei",lessStr);
val root6 = insert(root5,"chang",lessStr);
val root7 = insert(root6,"sheng",lessStr);
val root8 = insert(root7,"qi",lessStr);
val root9 = insert(root8,"le",lessStr);
val root10 = insert(root9,"a",lessStr);

member(root7,"qi",lessStr,equal);
member(root8,"qi",lessStr,equal);
printtree(root10,sayln);
