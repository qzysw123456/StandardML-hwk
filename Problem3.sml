datatype exp = Name of string
       | And of exp * exp
       | Or of exp * exp
       | Not of exp;

datatype dict = Item of string * bool;
fun find (x,Item(name,value)::t) =
  if(x=name)
  then value
  else find(x,t);

val Expression = And(Or(Name "p",Name "q"),Not(Name "p"));
val Dictionary = [Item("p",true),Item("q",true)];

fun eval (expr,dic)=
  case expr of
      Name(s) => find(s,dic)
    | And(p1,p2) => eval(p1,dic) andalso eval(p2,dic)
    | Or(p1,p2) => eval(p1,dic) orelse eval(p2,dic)
    | Not(p1) => not(eval(p1,dic));

eval(Expression,Dictionary);

fun mymember (s,nil) = false
  | mymember (s,h::t) = if (s=h) then true else mymember(s,t)
fun varsinExp (expr,lis)=
  case expr of
      Name(s) => if(mymember(s,lis)) then lis else s::lis
    | And(p1,p2) => varsinExp(p1,(varsinExp(p2,lis)))
    | Or(p1,p2) => varsinExp(p1,(varsinExp(p2,lis)))
    | Not(p1) => varsinExp(p1,lis);

varsinExp(Expression,nil);

exception Goback;
fun isTaut (nil,dic,expr) =
  if(eval(expr,dic)) then
      raise Goback
  else
      false
  | isTaut ((var::varlist),dic,expr) =
    isTaut (varlist,Item(var,true)::dic,expr)
    handle Goback => isTaut (varlist,Item(var,false)::dic,expr)
                     handle Goback => true;

val exp1 = Or(Name "p",Not(Name "p"));
val res1 = isTaut(varsinExp(exp1,nil),nil,exp1);

val exp2 = And(Name "p",Not(Name "p"));
val res2 = isTaut(varsinExp(exp2,nil),nil,exp2);
