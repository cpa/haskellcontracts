data List = Nil 0 
          | Cons 2;;

data Int = Zero 0
         | Succ 1;;

length x = case x of
  | Nil -> Zero
  | Cons a b -> Succ (length b);;
    
zero x = case x of
  | Nil -> True
  | Cons a b -> False;;
    
f x = case x of 
    | Nil -> Nil
    | Cons a b -> Nil;;
    
f ::: a:{x : True} -> {y : zero (length y)};;