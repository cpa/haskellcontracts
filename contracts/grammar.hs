{-# OPTIONS_GHC -w #-}
{-# OPTIONS -cpp #-}
module Main where
import Data.Char
import Haskell
import qualified Data.Array as Happy_Data_Array
import qualified System.IO as Happy_System_IO
import qualified System.IO.Unsafe as Happy_System_IO_Unsafe
import qualified Debug.Trace as Happy_Debug_Trace

-- parser produced by Happy Version 1.18.6

data HappyAbsSyn t4 t5 t6 t7 t8 t9
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9

happyActOffsets :: Happy_Data_Array.Array Int Int
happyActOffsets = Happy_Data_Array.listArray (0,38) ([1,1,41,38,37,40,35,33,1,-4,34,34,39,32,36,0,31,30,0,-4,27,29,0,28,28,0,0,26,25,16,-4,-3,4,24,22,0,0,0,0
	])

happyGotoOffsets :: Happy_Data_Array.Array Int Int
happyGotoOffsets = Happy_Data_Array.listArray (0,38) ([12,23,0,0,0,0,0,0,8,21,20,19,0,18,0,0,0,0,0,15,0,17,0,14,13,0,0,0,11,0,9,6,0,6,-1,0,0,0,0
	])

happyDefActions :: Happy_Data_Array.Array Int Int
happyDefActions = Happy_Data_Array.listArray (0,38) ([-4,0,0,0,0,0,0,-3,-4,0,-12,-12,0,-12,0,-6,0,0,-2,0,0,-10,-11,0,-5,-13,-7,0,0,0,0,0,-9,-14,-10,-15,-16,-8
	])

happyCheck :: Happy_Data_Array.Array Int Int
happyCheck = Happy_Data_Array.listArray (0,55) ([-1,2,1,7,3,8,10,10,0,1,4,10,0,1,5,4,12,4,4,2,5,3,3,3,1,9,5,-1,-1,-1,-1,-1,10,6,10,10,6,11,10,10,4,10,10,4,10,5,13,10,10,14,-1,-1,-1,-1,13,-1
	])

happyTable :: Happy_Data_Array.Array Int Int
happyTable = Happy_Data_Array.listArray (0,55) ([0,37,4,17,5,36,18,26,18,7,33,6,6,7,36,31,35,33,24,26,29,22,12,14,2,31,15,0,0,0,0,0,28,29,26,26,20,33,26,28,22,21,14,24,14,10,9,11,12,-1,0,0,0,0,9,0
	])

happyReduceArr = Happy_Data_Array.array (1, 15) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15)
	]

happy_n_terms = 15 :: Int
happy_n_nonterms = 6 :: Int

happyReduce_1 = happySpecReduce_3  0 happyReduction_1
happyReduction_1 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1:happy_var_3
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  0 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 ([happy_var_1]
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_0  0 happyReduction_3
happyReduction_3  =  HappyAbsSyn4
		 ([]
	)

happyReduce_4 = happyReduce 5 1 happyReduction_4
happyReduction_4 ((HappyAbsSyn8  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Def $ Let happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_3  1 happyReduction_5
happyReduction_5 (HappyAbsSyn9  happy_var_3)
	_
	(HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn5
		 (ContSat $ Satisfies happy_var_1 happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happyReduce 5 1 happyReduction_6
happyReduction_6 ((HappyAbsSyn6  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (DataType $ Data happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 4 2 happyReduction_7
happyReduction_7 ((HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenInt happy_var_2)) `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 ((happy_var_1,happy_var_2):happy_var_4
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_2  2 happyReduction_8
happyReduction_8 (HappyTerminal (TokenInt happy_var_2))
	(HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn6
		 ([(happy_var_1,happy_var_2)]
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_0  2 happyReduction_9
happyReduction_9  =  HappyAbsSyn6
		 ([]
	)

happyReduce_10 = happySpecReduce_2  3 happyReduction_10
happyReduction_10 (HappyAbsSyn7  happy_var_2)
	(HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn7
		 (happy_var_1:happy_var_2
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_0  3 happyReduction_11
happyReduction_11  =  HappyAbsSyn7
		 ([]
	)

happyReduce_12 = happySpecReduce_1  4 happyReduction_12
happyReduction_12 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn8
		 (if isUpper $ head happy_var_1 then Con happy_var_1 else Var happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_2  4 happyReduction_13
happyReduction_13 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (App happy_var_1 happy_var_2
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happyReduce 5 5 happyReduction_14
happyReduction_14 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Pred happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_15 = happyReduce 5 5 happyReduction_15
happyReduction_15 ((HappyAbsSyn9  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (AppC happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	happyDoAction 14 notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	TokenLet -> cont 1;
	TokenAny -> cont 2;
	TokenData -> cont 3;
	TokenEquals -> cont 4;
	TokenSatisfies -> cont 5;
	TokenColon -> cont 6;
	TokenCurlyO -> cont 7;
	TokenCurlyC -> cont 8;
	TokenArrow -> cont 9;
	TokenVar happy_dollar_dollar -> cont 10;
	TokenInt happy_dollar_dollar -> cont 11;
	TokenPipe -> cont 12;
	TokenSep -> cont 13;
	_ -> happyError' (tk:tks)
	}

happyError_ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . happyError

haskell tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse 0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


happyError :: [Token] -> a
happyError x = error $ "Parse error: " ++ show x

data Token = TokenLet
     	   | TokenData
	   | TokenInt Int
	   | TokenPipe
     	   | TokenAny
	   | TokenSep
     	   | TokenEquals
	   | TokenSatisfies
	   | TokenVar String
	   | TokenArrow
	   | TokenColon
	   | TokenCurlyO
	   | TokenCurlyC
	   deriving (Eq,Show)

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
      | isSpace c = lexer cs
      | isAlpha c = lexVar (c:cs)
      | isDigit c = lexInt (c:cs)
lexer ('=':cs) = TokenEquals : lexer cs
lexer (':':':':':':cs) = TokenSatisfies : lexer cs
lexer (':':cs) = TokenColon : lexer cs
lexer ('-':'>':cs) = TokenArrow : lexer cs
lexer ('{':cs) = TokenCurlyO : lexer cs
lexer ('}':cs) = TokenCurlyC : lexer cs
lexer (';':';':cs) = TokenSep : lexer cs
lexer ('|':cs) = TokenPipe : lexer cs

lexInt cs = TokenInt (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexVar cs = case span isAlpha cs of
       ("let",rest) -> TokenLet : lexer rest
       ("any",rest) -> TokenAny : lexer rest
       ("data",rest) -> TokenData : lexer rest
       (var,rest)   -> TokenVar var : lexer rest
main = getContents >>= print . haskell . lexer
{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates\\GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Int Happy_IntList





{-# LINE 51 "templates\\GenericTemplate.hs" #-}

{-# LINE 61 "templates\\GenericTemplate.hs" #-}



happyTrace string expr = Happy_System_IO_Unsafe.unsafePerformIO $ do
    Happy_System_IO.hPutStr Happy_System_IO.stderr string
    return expr




infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (0), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (0) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
	= (happyTrace ("state: " ++ show (st) ++  		      ",\ttoken: " ++ show (i) ++ 		      ",\taction: ")) $


	  case action of
		(0)		  -> (happyTrace ("fail.\n")) $
				     happyFail i tk st
		(-1) 	  -> (happyTrace ("accept.\n")) $
				     happyAccept i tk st
		n | (n < ((0) :: Int)) -> (happyTrace ("reduce (rule " ++ show rule 						 ++ ")")) $

				     (happyReduceArr Happy_Data_Array.! rule) i tk st
				     where rule = ((negate ((n + ((1) :: Int)))))
		n		  -> (happyTrace ("shift, enter state " 						 ++ show (new_state) 						 ++ "\n")) $


				     happyShift new_state i tk st
				     where (new_state) = (n - ((1) :: Int))
   where (off)    = indexShortOffAddr happyActOffsets st
         (off_i)  = (off + i)
	 check  = if (off_i >= ((0) :: Int))
			then (indexShortOffAddr happyCheck off_i ==  i)
			else False
         (action)
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st

{-# LINE 130 "templates\\GenericTemplate.hs" #-}

{-# LINE 140 "templates\\GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off








-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 163 "templates\\GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (0) tk st sts stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (0) tk st sts stk
     = happyFail (0) tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (0) tk st sts stk
     = happyFail (0) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (0) tk st sts stk
     = happyFail (0) tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (0) tk st sts stk
     = happyFail (0) tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (0) tk st sts stk
     = happyFail (0) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@((HappyCons (st1@(action)) (_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn (0) tk st sts stk
     = happyFail (0) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@((HappyCons (st1@(action)) (_)))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (0) tk st sts stk
     = happyFail (0) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@((HappyCons (st1@(action)) (_)))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

             (off) = indexShortOffAddr happyGotoOffsets st1
             (off_i) = (off + nt)
             (new_state) = indexShortOffAddr happyTable off_i




happyDrop (0) l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   (happyTrace (", goto state " ++ show (new_state) ++ "\n")) $
   happyDoAction j tk new_state
   where (off) = indexShortOffAddr happyGotoOffsets st
         (off_i) = (off + nt)
         (new_state) = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery ((0) is the error token)

-- parse error if we are in recovery and we fail again
happyFail  (0) tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (0) tk old_st (HappyCons ((action)) (sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	happyDoAction (0) tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
	happyDoAction (0) tk action sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
