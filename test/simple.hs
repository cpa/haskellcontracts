data List = Nil 0 
          | Cons 2;;

data Bool = True 0
          | False 0;;

head x = case x of
  | Nil -> BAD
  | Cons a b -> a;;
    
notNil x = case x of
  | Nil -> False
  | Cons a b -> True;;

head ::: a:{x : notNil x} -> {foo : True};;