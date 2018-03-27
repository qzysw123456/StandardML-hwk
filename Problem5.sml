fun fib 1 c = (c 1)
  | fib 2 c = (c 1)
  | fib n c = fib (n-1) (fn x1 => fib (n-2) (fn x2 => c (x1+x2)));
val test = fib 11 (fn x=>x);
