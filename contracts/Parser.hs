{-# OPTIONS_GHC -w #-}
module Parser where
import Data.Char
import Haskell

-- parser produced by Happy Version 1.18.6

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24
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
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23
	| HappyAbsSyn24 t24

action_0 (25) = happyShift action_5
action_0 (33) = happyShift action_6
action_0 (4) = happyGoto action_7
action_0 (7) = happyGoto action_2
action_0 (17) = happyGoto action_3
action_0 (19) = happyGoto action_4
action_0 _ = happyReduce_30

action_1 (25) = happyShift action_5
action_1 (33) = happyShift action_6
action_1 (7) = happyGoto action_2
action_1 (17) = happyGoto action_3
action_1 (19) = happyGoto action_4
action_1 _ = happyFail

action_2 (36) = happyShift action_14
action_2 _ = happyFail

action_3 (25) = happyShift action_5
action_3 (33) = happyShift action_6
action_3 (7) = happyGoto action_2
action_3 (17) = happyGoto action_3
action_3 (19) = happyGoto action_13
action_3 _ = happyReduce_30

action_4 _ = happyReduce_1

action_5 (32) = happyShift action_12
action_5 _ = happyFail

action_6 (27) = happyShift action_10
action_6 (33) = happyShift action_11
action_6 (5) = happyGoto action_8
action_6 (20) = happyGoto action_9
action_6 _ = happyReduce_32

action_7 (44) = happyAccept
action_7 _ = happyFail

action_8 (26) = happyShift action_23
action_8 _ = happyFail

action_9 _ = happyReduce_2

action_10 (29) = happyShift action_19
action_10 (33) = happyShift action_20
action_10 (39) = happyShift action_21
action_10 (41) = happyShift action_22
action_10 (15) = happyGoto action_17
action_10 (16) = happyGoto action_18
action_10 _ = happyFail

action_11 (33) = happyShift action_11
action_11 (20) = happyGoto action_16
action_11 _ = happyReduce_32

action_12 (33) = happyShift action_11
action_12 (5) = happyGoto action_15
action_12 (20) = happyGoto action_9
action_12 _ = happyReduce_32

action_13 _ = happyReduce_29

action_14 _ = happyReduce_26

action_15 (26) = happyShift action_37
action_15 _ = happyFail

action_16 _ = happyReduce_31

action_17 (31) = happyShift action_34
action_17 (42) = happyShift action_35
action_17 (43) = happyShift action_36
action_17 _ = happyReduce_25

action_18 _ = happyReduce_7

action_19 (33) = happyShift action_33
action_19 _ = happyFail

action_20 (28) = happyShift action_32
action_20 _ = happyFail

action_21 (29) = happyShift action_19
action_21 (33) = happyShift action_20
action_21 (39) = happyShift action_21
action_21 (41) = happyShift action_22
action_21 (15) = happyGoto action_17
action_21 (16) = happyGoto action_31
action_21 _ = happyFail

action_22 _ = happyReduce_19

action_23 (32) = happyShift action_27
action_23 (33) = happyShift action_28
action_23 (37) = happyShift action_29
action_23 (39) = happyShift action_30
action_23 (6) = happyGoto action_24
action_23 (13) = happyGoto action_25
action_23 (14) = happyGoto action_26
action_23 _ = happyFail

action_24 _ = happyReduce_14

action_25 _ = happyReduce_17

action_26 (32) = happyShift action_27
action_26 (33) = happyShift action_28
action_26 (39) = happyShift action_30
action_26 (6) = happyGoto action_24
action_26 (13) = happyGoto action_51
action_26 _ = happyReduce_5

action_27 _ = happyReduce_3

action_28 _ = happyReduce_4

action_29 (32) = happyShift action_27
action_29 (33) = happyShift action_28
action_29 (39) = happyShift action_30
action_29 (6) = happyGoto action_24
action_29 (13) = happyGoto action_25
action_29 (14) = happyGoto action_50
action_29 _ = happyFail

action_30 (32) = happyShift action_27
action_30 (33) = happyShift action_28
action_30 (39) = happyShift action_30
action_30 (6) = happyGoto action_24
action_30 (13) = happyGoto action_25
action_30 (14) = happyGoto action_49
action_30 _ = happyFail

action_31 (40) = happyShift action_48
action_31 _ = happyFail

action_32 (29) = happyShift action_19
action_32 (39) = happyShift action_21
action_32 (41) = happyShift action_22
action_32 (15) = happyGoto action_47
action_32 _ = happyFail

action_33 (28) = happyShift action_46
action_33 _ = happyFail

action_34 (29) = happyShift action_19
action_34 (33) = happyShift action_20
action_34 (39) = happyShift action_21
action_34 (41) = happyShift action_22
action_34 (15) = happyGoto action_17
action_34 (16) = happyGoto action_45
action_34 _ = happyFail

action_35 (29) = happyShift action_19
action_35 (39) = happyShift action_21
action_35 (41) = happyShift action_22
action_35 (15) = happyGoto action_44
action_35 _ = happyFail

action_36 (29) = happyShift action_19
action_36 (39) = happyShift action_21
action_36 (41) = happyShift action_22
action_36 (15) = happyGoto action_43
action_36 _ = happyFail

action_37 (32) = happyShift action_42
action_37 (11) = happyGoto action_38
action_37 (12) = happyGoto action_39
action_37 (21) = happyGoto action_40
action_37 (22) = happyGoto action_41
action_37 _ = happyReduce_34

action_38 (35) = happyShift action_59
action_38 (23) = happyGoto action_57
action_38 (24) = happyGoto action_58
action_38 _ = happyReduce_37

action_39 _ = happyReduce_8

action_40 _ = happyReduce_13

action_41 _ = happyReduce_33

action_42 (34) = happyShift action_56
action_42 _ = happyFail

action_43 _ = happyReduce_21

action_44 _ = happyReduce_22

action_45 _ = happyReduce_24

action_46 (32) = happyShift action_27
action_46 (33) = happyShift action_28
action_46 (39) = happyShift action_30
action_46 (6) = happyGoto action_24
action_46 (13) = happyGoto action_25
action_46 (14) = happyGoto action_55
action_46 _ = happyFail

action_47 (31) = happyShift action_54
action_47 _ = happyFail

action_48 _ = happyReduce_20

action_49 (32) = happyShift action_27
action_49 (33) = happyShift action_28
action_49 (39) = happyShift action_30
action_49 (40) = happyShift action_53
action_49 (6) = happyGoto action_24
action_49 (13) = happyGoto action_51
action_49 _ = happyFail

action_50 (32) = happyShift action_27
action_50 (33) = happyShift action_28
action_50 (38) = happyShift action_52
action_50 (39) = happyShift action_30
action_50 (6) = happyGoto action_24
action_50 (13) = happyGoto action_51
action_50 _ = happyFail

action_51 _ = happyReduce_16

action_52 (35) = happyShift action_67
action_52 (9) = happyGoto action_64
action_52 (10) = happyGoto action_65
action_52 (18) = happyGoto action_66
action_52 _ = happyReduce_28

action_53 _ = happyReduce_15

action_54 (29) = happyShift action_19
action_54 (33) = happyShift action_20
action_54 (39) = happyShift action_21
action_54 (41) = happyShift action_22
action_54 (15) = happyGoto action_17
action_54 (16) = happyGoto action_63
action_54 _ = happyFail

action_55 (30) = happyShift action_62
action_55 (32) = happyShift action_27
action_55 (33) = happyShift action_28
action_55 (39) = happyShift action_30
action_55 (6) = happyGoto action_24
action_55 (13) = happyGoto action_51
action_55 _ = happyFail

action_56 _ = happyReduce_12

action_57 _ = happyReduce_35

action_58 (35) = happyShift action_59
action_58 (23) = happyGoto action_61
action_58 (24) = happyGoto action_58
action_58 _ = happyReduce_37

action_59 (32) = happyShift action_42
action_59 (11) = happyGoto action_60
action_59 _ = happyFail

action_60 _ = happyReduce_38

action_61 _ = happyReduce_36

action_62 _ = happyReduce_18

action_63 _ = happyReduce_23

action_64 (35) = happyShift action_67
action_64 (9) = happyGoto action_64
action_64 (18) = happyGoto action_70
action_64 _ = happyReduce_28

action_65 _ = happyReduce_6

action_66 _ = happyReduce_11

action_67 (32) = happyShift action_69
action_67 (8) = happyGoto action_68
action_67 _ = happyFail

action_68 (31) = happyShift action_72
action_68 _ = happyFail

action_69 (33) = happyShift action_11
action_69 (5) = happyGoto action_71
action_69 (20) = happyGoto action_9
action_69 _ = happyReduce_32

action_70 _ = happyReduce_27

action_71 _ = happyReduce_9

action_72 (32) = happyShift action_27
action_72 (33) = happyShift action_28
action_72 (39) = happyShift action_30
action_72 (6) = happyGoto action_24
action_72 (13) = happyGoto action_25
action_72 (14) = happyGoto action_73
action_72 _ = happyFail

action_73 (32) = happyShift action_27
action_73 (33) = happyShift action_28
action_73 (39) = happyShift action_30
action_73 (6) = happyGoto action_24
action_73 (13) = happyGoto action_51
action_73 _ = happyReduce_10

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyTerminal (TokenCon happy_var_1))
	 =  HappyAbsSyn6
		 (Con happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  6 happyReduction_4
happyReduction_4 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn6
		 (Var happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happyReduce 4 7 happyReduction_5
happyReduction_5 ((HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Def $ Let happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 7 7 happyReduction_6
happyReduction_6 ((HappyAbsSyn10  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Def $ LetCase happy_var_1 happy_var_2 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_3  7 happyReduction_7
happyReduction_7 (HappyAbsSyn16  happy_var_3)
	_
	(HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn7
		 (ContSat $ Satisfies happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happyReduce 5 7 happyReduction_8
happyReduction_8 ((HappyAbsSyn12  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenCon happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (DataType $ Data happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_2  8 happyReduction_9
happyReduction_9 (HappyAbsSyn5  happy_var_2)
	(HappyTerminal (TokenCon happy_var_1))
	 =  HappyAbsSyn8
		 ((happy_var_1,happy_var_2)
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happyReduce 4 9 happyReduction_10
happyReduction_10 ((HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 ((happy_var_2,happy_var_4)
	) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_1  10 happyReduction_11
happyReduction_11 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  11 happyReduction_12
happyReduction_12 (HappyTerminal (TokenInt happy_var_2))
	(HappyTerminal (TokenCon happy_var_1))
	 =  HappyAbsSyn11
		 ((happy_var_1,happy_var_2,error "Parser.y: ConDecl: constructor contracts aren't supported.")
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  12 happyReduction_13
happyReduction_13 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  13 happyReduction_14
happyReduction_14 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn13
		 (Named happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  13 happyReduction_15
happyReduction_15 _
	(HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (happy_var_2
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_2  14 happyReduction_16
happyReduction_16 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1 :@: happy_var_2
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  14 happyReduction_17
happyReduction_17 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happyReduce 5 15 happyReduction_18
happyReduction_18 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (Pred happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_1  15 happyReduction_19
happyReduction_19 _
	 =  HappyAbsSyn15
		 (CF
	)

happyReduce_20 = happySpecReduce_3  15 happyReduction_20
happyReduction_20 _
	(HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (happy_var_2
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  16 happyReduction_21
happyReduction_21 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn16
		 (And happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  16 happyReduction_22
happyReduction_22 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn16
		 (Or  happy_var_1 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happyReduce 5 16 happyReduction_23
happyReduction_23 ((HappyAbsSyn16  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (Arr happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_3  16 happyReduction_24
happyReduction_24 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn16
		 (Arr "" happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  16 happyReduction_25
happyReduction_25 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_2  17 happyReduction_26
happyReduction_26 _
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_26 _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_2  18 happyReduction_27
happyReduction_27 (HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1:happy_var_2
	)
happyReduction_27 _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_0  18 happyReduction_28
happyReduction_28  =  HappyAbsSyn18
		 ([]
	)

happyReduce_29 = happySpecReduce_2  19 happyReduction_29
happyReduction_29 (HappyAbsSyn19  happy_var_2)
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1:happy_var_2
	)
happyReduction_29 _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_0  19 happyReduction_30
happyReduction_30  =  HappyAbsSyn19
		 ([]
	)

happyReduce_31 = happySpecReduce_2  20 happyReduction_31
happyReduction_31 (HappyAbsSyn20  happy_var_2)
	(HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn20
		 (happy_var_1:happy_var_2
	)
happyReduction_31 _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_0  20 happyReduction_32
happyReduction_32  =  HappyAbsSyn20
		 ([]
	)

happyReduce_33 = happySpecReduce_1  21 happyReduction_33
happyReduction_33 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_0  21 happyReduction_34
happyReduction_34  =  HappyAbsSyn21
		 ([]
	)

happyReduce_35 = happySpecReduce_2  22 happyReduction_35
happyReduction_35 (HappyAbsSyn23  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1:happy_var_2
	)
happyReduction_35 _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_2  23 happyReduction_36
happyReduction_36 (HappyAbsSyn23  happy_var_2)
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1:happy_var_2
	)
happyReduction_36 _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_0  23 happyReduction_37
happyReduction_37  =  HappyAbsSyn23
		 ([]
	)

happyReduce_38 = happySpecReduce_2  24 happyReduction_38
happyReduction_38 (HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn24
		 (happy_var_2
	)
happyReduction_38 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 44 44 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenData -> cont 25;
	TokenEquals -> cont 26;
	TokenSatisfies -> cont 27;
	TokenColon -> cont 28;
	TokenCurlyO -> cont 29;
	TokenCurlyC -> cont 30;
	TokenArrow -> cont 31;
	TokenCon happy_dollar_dollar -> cont 32;
	TokenVar happy_dollar_dollar -> cont 33;
	TokenInt happy_dollar_dollar -> cont 34;
	TokenPipe -> cont 35;
	TokenSep -> cont 36;
	TokenCase -> cont 37;
	TokenOf -> cont 38;
	TokenParenO -> cont 39;
	TokenParenC -> cont 40;
	TokenCF -> cont 41;
	TokenOr -> cont 42;
	TokenAnd -> cont 43;
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
           | TokenCF
           | TokenAny
           | TokenSep
           | TokenEquals
           | TokenSatisfies
           | TokenCon String -- Upper case var
           | TokenVar String -- Lower case var
           | TokenArrow
           | TokenColon
           | TokenParenO
           | TokenParenC
           | TokenCurlyO
           | TokenCurlyC
           | TokenComma
           | TokenOr
           | TokenAnd
           deriving (Eq,Show)

lexer :: String -> [Token]
lexer [] = []
lexer ('=':cs) = TokenEquals : lexer cs
lexer (':':':':':':cs) = TokenSatisfies : lexer cs
lexer (':':cs) = TokenColon : lexer cs
lexer ('|':'|':cs) = TokenOr : lexer cs
lexer ('&':'&':cs) = TokenAnd : lexer cs
lexer ('-':'>':cs) = TokenArrow : lexer cs
lexer ('{':cs) = TokenCurlyO : lexer cs
lexer ('}':cs) = TokenCurlyC : lexer cs
lexer ('(':cs) = TokenParenO : lexer cs
lexer (')':cs) = TokenParenC : lexer cs
lexer (';':';':cs) = TokenSep : lexer cs
lexer ('|':cs) = TokenPipe : lexer cs
lexer (',':cs) = TokenComma : lexer cs
lexer ('-':'-':cs) = lexer $ dropWhile (/= '\n') cs
lexer (c:cs) 
      | isSpace c = lexer cs
      | isAlpha c = lexVar (c:cs)
      | isDigit c = lexInt (c:cs)

lexInt cs = TokenInt (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexVar (c:cs) = token : lexer rest where
  (var,rest) = span (\x -> isAlpha x || x == '_' || isDigit x) (c:cs)
  token = case var of
    "data" -> TokenData
    "case" -> TokenCase
    "of"   -> TokenOf
--    "Any"  -> TokenAny
    "CF"   -> TokenCF
    _      -> (if isUpper c then TokenCon else TokenVar) var

main = getContents >>= print . haskell . lexer
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

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

{-# LINE 148 "templates/GenericTemplate.hs" #-}

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

{-# LINE 246 "templates/GenericTemplate.hs" #-}
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

{-# LINE 311 "templates/GenericTemplate.hs" #-}
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
