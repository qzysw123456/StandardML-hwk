fun nop x = x;
fun seq stmt1 stmt2 = (fn x => (stmt2(stmt1 x)));
fun ifstat test stmt1 stmt2 =
  (fn x => if(test x) then (stmt1 x) else (stmt2 x));
fun whilestat test stmt =
  (fn x=> if(test x) then (seq stmt (whilestat test stmt) x) else (nop x));
fun repeatstat test stmt =
  (fn x => if(not(test(stmt x))) then(seq stmt (repeatstat test stmt) x) else (stmt x));

fun Bmorethanzero (a,b,r) = b>0;
fun assignRwithB (a,b,r) = (a,b,b);
fun assignBwithAmodB (a,b,r) = (a,a mod b,r);
fun assignAwithR (a,b,r) = (r,b,r);
fun gcd state =
  whilestat Bmorethanzero (seq assignRwithB (seq assignBwithAmodB assignAwithR)) state;
(*
int gcd(int a,int b)
{
    while(b>0){
        int r = b; //assignRwithB
        b = a % b; //assignBwithAmodB
        a = r;     //assignAwithR
    }
    return a;
}
 *)
 val test1 = gcd (36,144,1111);  (* (36,0,36) a = 36 which is gcd(36,144) correctg*)
fun odd_b (a,b,res) = if(b mod 2 = 1) then true else false;
fun assignRes1 (a,b,_) = (a,b,1);
fun assignResResTimesA (a,b,res) = (a,b,res*a);
fun assignAsqrA (a,b,res) = (a*a,b,res);
fun assignBdiv2 (a,b,res) = (a,b div 2,res);
fun exponential state =
  seq assignRes1 (whilestat Bmorethanzero (seq (ifstat odd_b assignResResTimesA nop)
                                               (seq assignAsqrA assignBdiv2))) state;
(*
int exponential(int a,int b)
{
    int res=1;                 //assignRes1
    while(b>0){                //whilestat Bmorethanzero
        if(b&1){               //ifstat odd_b
          res = res * a;       //assignResResTimesA
        }                      //nop
        a = a * a;             //assignAsqrA
        b = b / 2;             //assignBdiv2
    }
    return res;
}
 *)
 val test2 = exponential (3,7,123123); (* (6561,0,2187), res = 2187 which is 3^7 correct *)
