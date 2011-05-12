{-# OPTIONS_GHC -w #-}
module Parser where
import Data.Char
import Haskell

-- parser produced by Happy Version 1.18.6

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12

action_0 (14) = happyShift action_3
action_0 (21) = happyShift action_4
action_0 (4) = happyGoto action_5
action_0 (6) = happyGoto action_6
action_0 _ = happyReduce_3

action_1 (14) = happyShift action_3
action_1 (21) = happyShift action_4
action_1 (6) = happyGoto action_2
action_1 _ = happyFail

action_2 (24) = happyShift action_7
action_2 _ = happyFail

action_3 (21) = happyShift action_11
action_3 _ = happyFail

action_4 (16) = happyShift action_9
action_4 (21) = happyShift action_10
action_4 (10) = happyGoto action_8
action_4 _ = happyReduce_27

action_5 (29) = happyAccept
action_5 _ = happyFail

action_6 (24) = happyShift action_7
action_6 _ = happyReduce_2

action_7 (14) = happyShift action_3
action_7 (21) = happyShift action_4
action_7 (4) = happyGoto action_19
action_7 (6) = happyGoto action_6
action_7 _ = happyReduce_3

action_8 (15) = happyShift action_18
action_8 _ = happyFail

action_9 (18) = happyShift action_15
action_9 (21) = happyShift action_16
action_9 (27) = happyShift action_17
action_9 (12) = happyGoto action_14
action_9 _ = happyFail

action_10 (21) = happyShift action_10
action_10 (10) = happyGoto action_13
action_10 _ = happyReduce_27

action_11 (21) = happyShift action_10
action_11 (10) = happyGoto action_12
action_11 _ = happyReduce_27

action_12 (15) = happyShift action_27
action_12 _ = happyFail

action_13 _ = happyReduce_26

action_14 _ = happyReduce_9

action_15 (21) = happyShift action_26
action_15 _ = happyFail

action_16 (17) = happyShift action_25
action_16 _ = happyFail

action_17 (21) = happyShift action_24
action_17 _ = happyFail

action_18 (21) = happyShift action_21
action_18 (25) = happyShift action_22
action_18 (27) = happyShift action_23
action_18 (11) = happyGoto action_20
action_18 _ = happyFail

action_19 _ = happyReduce_1

action_20 (21) = happyShift action_21
action_20 (27) = happyShift action_23
action_20 (11) = happyGoto action_36
action_20 _ = happyReduce_7

action_21 _ = happyReduce_28

action_22 (21) = happyShift action_21
action_22 (27) = happyShift action_23
action_22 (11) = happyGoto action_35
action_22 _ = happyFail

action_23 (21) = happyShift action_21
action_23 (27) = happyShift action_23
action_23 (11) = happyGoto action_34
action_23 _ = happyFail

action_24 (17) = happyShift action_33
action_24 _ = happyFail

action_25 (18) = happyShift action_15
action_25 (21) = happyShift action_16
action_25 (27) = happyShift action_17
action_25 (12) = happyGoto action_32
action_25 _ = happyFail

action_26 (17) = happyShift action_31
action_26 _ = happyFail

action_27 (21) = happyShift action_29
action_27 (27) = happyShift action_30
action_27 (9) = happyGoto action_28
action_27 _ = happyReduce_25

action_28 _ = happyReduce_10

action_29 (22) = happyShift action_43
action_29 _ = happyFail

action_30 (21) = happyShift action_42
action_30 _ = happyFail

action_31 (21) = happyShift action_21
action_31 (27) = happyShift action_23
action_31 (11) = happyGoto action_41
action_31 _ = happyFail

action_32 (20) = happyShift action_40
action_32 _ = happyFail

action_33 (18) = happyShift action_15
action_33 (21) = happyShift action_16
action_33 (27) = happyShift action_17
action_33 (12) = happyGoto action_39
action_33 _ = happyFail

action_34 (21) = happyShift action_21
action_34 (27) = happyShift action_23
action_34 (11) = happyGoto action_38
action_34 _ = happyFail

action_35 (21) = happyShift action_21
action_35 (26) = happyShift action_37
action_35 (27) = happyShift action_23
action_35 (11) = happyGoto action_36
action_35 _ = happyFail

action_36 (21) = happyShift action_21
action_36 (27) = happyShift action_23
action_36 (11) = happyGoto action_36
action_36 _ = happyReduce_30

action_37 (23) = happyShift action_51
action_37 (7) = happyGoto action_50
action_37 _ = happyReduce_15

action_38 (21) = happyShift action_21
action_38 (27) = happyShift action_23
action_38 (28) = happyShift action_49
action_38 (11) = happyGoto action_36
action_38 _ = happyFail

action_39 (20) = happyShift action_48
action_39 _ = happyFail

action_40 (18) = happyShift action_15
action_40 (21) = happyShift action_16
action_40 (27) = happyShift action_17
action_40 (12) = happyGoto action_47
action_40 _ = happyFail

action_41 (19) = happyShift action_46
action_41 (21) = happyShift action_21
action_41 (27) = happyShift action_23
action_41 (11) = happyGoto action_36
action_41 _ = happyFail

action_42 (22) = happyShift action_45
action_42 _ = happyFail

action_43 (23) = happyShift action_44
action_43 _ = happyReduce_23

action_44 (21) = happyShift action_29
action_44 (27) = happyShift action_30
action_44 (9) = happyGoto action_57
action_44 _ = happyReduce_25

action_45 (28) = happyShift action_56
action_45 _ = happyFail

action_46 _ = happyReduce_31

action_47 _ = happyReduce_32

action_48 (18) = happyShift action_15
action_48 (21) = happyShift action_16
action_48 (27) = happyShift action_17
action_48 (12) = happyGoto action_55
action_48 _ = happyFail

action_49 _ = happyReduce_29

action_50 _ = happyReduce_8

action_51 (21) = happyShift action_53
action_51 (27) = happyShift action_54
action_51 (8) = happyGoto action_52
action_51 _ = happyReduce_20

action_52 (20) = happyShift action_63
action_52 _ = happyFail

action_53 (20) = happyReduce_20
action_53 (21) = happyShift action_53
action_53 (27) = happyShift action_62
action_53 (28) = happyReduce_20
action_53 (8) = happyGoto action_61
action_53 _ = happyReduce_20

action_54 (21) = happyShift action_60
action_54 _ = happyFail

action_55 (28) = happyShift action_59
action_55 _ = happyFail

action_56 (23) = happyShift action_58
action_56 _ = happyReduce_24

action_57 _ = happyReduce_21

action_58 (21) = happyShift action_29
action_58 (27) = happyShift action_30
action_58 (9) = happyGoto action_69
action_58 _ = happyReduce_25

action_59 _ = happyReduce_33

action_60 (28) = happyShift action_68
action_60 _ = happyFail

action_61 _ = happyReduce_16

action_62 (21) = happyShift action_67
action_62 (27) = happyShift action_54
action_62 (8) = happyGoto action_66
action_62 _ = happyReduce_20

action_63 (21) = happyShift action_21
action_63 (27) = happyShift action_65
action_63 (11) = happyGoto action_64
action_63 _ = happyFail

action_64 (21) = happyShift action_21
action_64 (23) = happyShift action_51
action_64 (24) = happyReduce_15
action_64 (27) = happyShift action_23
action_64 (29) = happyReduce_15
action_64 (7) = happyGoto action_72
action_64 (11) = happyGoto action_36
action_64 _ = happyReduce_15

action_65 (21) = happyShift action_21
action_65 (27) = happyShift action_23
action_65 (11) = happyGoto action_71
action_65 _ = happyFail

action_66 (28) = happyShift action_70
action_66 _ = happyFail

action_67 (21) = happyShift action_53
action_67 (27) = happyShift action_62
action_67 (28) = happyShift action_68
action_67 (8) = happyGoto action_61
action_67 _ = happyFail

action_68 _ = happyReduce_19

action_69 _ = happyReduce_22

action_70 _ = happyReduce_17

action_71 (21) = happyShift action_21
action_71 (27) = happyShift action_23
action_71 (28) = happyShift action_73
action_71 (11) = happyGoto action_38
action_71 _ = happyFail

action_72 _ = happyReduce_11

action_73 (23) = happyShift action_51
action_73 (24) = happyReduce_15
action_73 (29) = happyReduce_15
action_73 (7) = happyGoto action_74
action_73 _ = happyReduce_15

action_74 _ = happyReduce_12

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1:happy_var_3
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn4
		 ([happy_var_1]
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_0  4 happyReduction_3
happyReduction_3  =  HappyAbsSyn4
		 ([]
	)

happyReduce_4 = happySpecReduce_2  5 happyReduction_4
happyReduction_4 _
	_
	 =  HappyAbsSyn5
		 (
	)

happyReduce_5 = happySpecReduce_1  5 happyReduction_5
happyReduction_5 _
	 =  HappyAbsSyn5
		 (
	)

happyReduce_6 = happySpecReduce_0  5 happyReduction_6
happyReduction_6  =  HappyAbsSyn5
		 (
	)

happyReduce_7 = happyReduce 4 6 happyReduction_7
happyReduction_7 ((HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Def $ Let (map toLower happy_var_1) happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_8 = happyReduce 7 6 happyReduction_8
happyReduction_8 ((HappyAbsSyn7  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Def $ LetCase (map toLower happy_var_1) happy_var_2 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_3  6 happyReduction_9
happyReduction_9 (HappyAbsSyn12  happy_var_3)
	_
	(HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn6
		 (ContSat $ Satisfies (map toLower happy_var_1) happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happyReduce 5 6 happyReduction_10
happyReduction_10 ((HappyAbsSyn9  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (DataType $ Data (map toLower happy_var_2) happy_var_5
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 5 7 happyReduction_11
happyReduction_11 ((HappyAbsSyn7  happy_var_5) `HappyStk`
	(HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 ((happy_var_2,happy_var_4):happy_var_5
	) `HappyStk` happyRest

happyReduce_12 = happyReduce 7 7 happyReduction_12
happyReduction_12 ((HappyAbsSyn7  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 ((happy_var_2,happy_var_5):happy_var_7
	) `HappyStk` happyRest

happyReduce_13 = happyReduce 4 7 happyReduction_13
happyReduction_13 ((HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 ([(happy_var_2,happy_var_4)]
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 6 7 happyReduction_14
happyReduction_14 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 ([(happy_var_2,happy_var_5)]
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_0  7 happyReduction_15
happyReduction_15  =  HappyAbsSyn7
		 ([]
	)

happyReduce_16 = happySpecReduce_2  8 happyReduction_16
happyReduction_16 (HappyAbsSyn8  happy_var_2)
	(HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn8
		 ((map toLower happy_var_1):happy_var_2
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happyReduce 4 8 happyReduction_17
happyReduction_17 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 ((map toLower happy_var_1):happy_var_3
	) `HappyStk` happyRest

happyReduce_18 = happySpecReduce_1  8 happyReduction_18
happyReduction_18 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn8
		 ([map toLower happy_var_1]
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  8 happyReduction_19
happyReduction_19 _
	(HappyTerminal (TokenVar happy_var_2))
	_
	 =  HappyAbsSyn8
		 ([map toLower happy_var_2]
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_0  8 happyReduction_20
happyReduction_20  =  HappyAbsSyn8
		 ([]
	)

happyReduce_21 = happyReduce 4 9 happyReduction_21
happyReduction_21 ((HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenInt happy_var_2)) `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 ((map toLower happy_var_1,happy_var_2):happy_var_4
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 6 9 happyReduction_22
happyReduction_22 ((HappyAbsSyn9  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenInt happy_var_3)) `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 ((map toLower happy_var_2,happy_var_3):happy_var_6
	) `HappyStk` happyRest

happyReduce_23 = happySpecReduce_2  9 happyReduction_23
happyReduction_23 (HappyTerminal (TokenInt happy_var_2))
	(HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn9
		 ([(map toLower happy_var_1,happy_var_2)]
	)
happyReduction_23 _ _  = notHappyAtAll 

happyReduce_24 = happyReduce 4 9 happyReduction_24
happyReduction_24 (_ `HappyStk`
	(HappyTerminal (TokenInt happy_var_3)) `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 ([(map toLower happy_var_2,happy_var_3)]
	) `HappyStk` happyRest

happyReduce_25 = happySpecReduce_0  9 happyReduction_25
happyReduction_25  =  HappyAbsSyn9
		 ([]
	)

happyReduce_26 = happySpecReduce_2  10 happyReduction_26
happyReduction_26 (HappyAbsSyn10  happy_var_2)
	(HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn10
		 ((map toLower happy_var_1):happy_var_2
	)
happyReduction_26 _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_0  10 happyReduction_27
happyReduction_27  =  HappyAbsSyn10
		 ([]
	)

happyReduce_28 = happySpecReduce_1  11 happyReduction_28
happyReduction_28 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn11
		 (if isUpper $ head happy_var_1 then Con (map toLower happy_var_1) else Var happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happyReduce 4 11 happyReduction_29
happyReduction_29 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (App happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_30 = happySpecReduce_2  11 happyReduction_30
happyReduction_30 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (App happy_var_1 happy_var_2
	)
happyReduction_30 _ _  = notHappyAtAll 

happyReduce_31 = happyReduce 5 12 happyReduction_31
happyReduction_31 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (Pred (map toLower happy_var_2) happy_var_4
	) `HappyStk` happyRest

happyReduce_32 = happyReduce 5 12 happyReduction_32
happyReduction_32 ((HappyAbsSyn12  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (AppC (map toLower happy_var_1) happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_33 = happyReduce 7 12 happyReduction_33
happyReduction_33 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (AppC (map toLower happy_var_2) happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 29 29 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenAny -> cont 13;
	TokenData -> cont 14;
	TokenEquals -> cont 15;
	TokenSatisfies -> cont 16;
	TokenColon -> cont 17;
	TokenCurlyO -> cont 18;
	TokenCurlyC -> cont 19;
	TokenArrow -> cont 20;
	TokenVar happy_dollar_dollar -> cont 21;
	TokenInt happy_dollar_dollar -> cont 22;
	TokenPipe -> cont 23;
	TokenSep -> cont 24;
	TokenCase -> cont 25;
	TokenOf -> cont 26;
	TokenParenO -> cont 27;
	TokenParenC -> cont 28;
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
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


happyError :: [Token] -> a
happyError x = error $ "Parse error: " ++ show x

data Token = TokenCase
     	   | TokenExpr
	   | TokenOf
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
	   | TokenParenO
	   | TokenParenC
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
lexer ('(':cs) = TokenParenO : lexer cs
lexer (')':cs) = TokenParenC : lexer cs
lexer (';':';':cs) = TokenSep : lexer cs
lexer ('|':cs) = TokenPipe : lexer cs

lexInt cs = TokenInt (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexVar cs = case span isAlpha cs of
       ("any",rest) -> TokenAny : lexer rest
       ("data",rest) -> TokenData : lexer rest
       ("case",rest) -> TokenCase : lexer rest
       ("of",rest) -> TokenOf : lexer rest       		     		    	  
       (var,rest)   -> TokenVar var : lexer rest
main = getContents >>= print . haskell . lexer
{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates\\GenericTemplate.hs" #-}








{-# LINE 51 "templates\\GenericTemplate.hs" #-}

{-# LINE 61 "templates\\GenericTemplate.hs" #-}

{-# LINE 70 "templates\\GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 148 "templates\\GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates\\GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail  (1) tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

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

{-# LINE 311 "templates\\GenericTemplate.hs" #-}
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
