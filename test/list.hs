data List = Nil 0 
          | Cons 2;;

data Int = Zero 0
         | Succ 1;;

data Bool = True 0
          | False 0;;

length x = case x of
  | Nil -> Zero
  | Cons a b -> Succ (length b);;
    
notZero x = case x of
  | Nil -> False
  | Cons a b -> True;;
    
f x = case x of 
    | Nil -> Nil
    | Cons a b -> Nil;;
    
f ::: a:{x : True} -> {y : notZero (length y)};;