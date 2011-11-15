{-# OPTIONS_GHC -w #-}
module Parser where
import Data.Char
import Haskell

-- parser produced by Happy Version 1.18.6

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25
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
	| HappyAbsSyn25 t25

action_0 (26) = happyShift action_5
action_0 (27) = happyShift action_6
action_0 (36) = happyShift action_7
action_0 (4) = happyGoto action_8
action_0 (7) = happyGoto action_2
action_0 (17) = happyGoto action_3
action_0 (20) = happyGoto action_4
action_0 _ = happyReduce_33

action_1 (26) = happyShift action_5
action_1 (27) = happyShift action_6
action_1 (36) = happyShift action_7
action_1 (7) = happyGoto action_2
action_1 (17) = happyGoto action_3
action_1 (20) = happyGoto action_4
action_1 _ = happyFail

action_2 (39) = happyShift action_16
action_2 _ = happyFail

action_3 (26) = happyShift action_5
action_3 (27) = happyShift action_6
action_3 (36) = happyShift action_7
action_3 (7) = happyGoto action_2
action_3 (17) = happyGoto action_3
action_3 (20) = happyGoto action_15
action_3 _ = happyReduce_33

action_4 _ = happyReduce_1

action_5 (34) = happyShift action_14
action_5 _ = happyFail

action_6 (35) = happyShift action_13
action_6 _ = happyFail

action_7 (29) = happyShift action_11
action_7 (36) = happyShift action_12
action_7 (5) = happyGoto action_9
action_7 (21) = happyGoto action_10
action_7 _ = happyReduce_35

action_8 (48) = happyAccept
action_8 _ = happyFail

action_9 (28) = happyShift action_25
action_9 _ = happyFail

action_10 _ = happyReduce_2

action_11 (31) = happyShift action_21
action_11 (36) = happyShift action_22
action_11 (43) = happyShift action_23
action_11 (45) = happyShift action_24
action_11 (15) = happyGoto action_19
action_11 (16) = happyGoto action_20
action_11 _ = happyFail

action_12 (36) = happyShift action_12
action_12 (21) = happyGoto action_18
action_12 _ = happyReduce_35

action_13 (36) = happyShift action_12
action_13 (5) = happyGoto action_17
action_13 (21) = happyGoto action_10
action_13 _ = happyReduce_35

action_14 _ = happyReduce_9

action_15 _ = happyReduce_32

action_16 _ = happyReduce_27

action_17 (28) = happyShift action_39
action_17 _ = happyFail

action_18 _ = happyReduce_34

action_19 (33) = happyShift action_36
action_19 (46) = happyShift action_37
action_19 (47) = happyShift action_38
action_19 _ = happyReduce_26

action_20 _ = happyReduce_7

action_21 (36) = happyShift action_35
action_21 _ = happyFail

action_22 (30) = happyShift action_34
action_22 _ = happyFail

action_23 (31) = happyShift action_21
action_23 (36) = happyShift action_22
action_23 (43) = happyShift action_23
action_23 (45) = happyShift action_24
action_23 (15) = happyGoto action_19
action_23 (16) = happyGoto action_33
action_23 _ = happyFail

action_24 _ = happyReduce_20

action_25 (35) = happyShift action_29
action_25 (36) = happyShift action_30
action_25 (41) = happyShift action_31
action_25 (43) = happyShift action_32
action_25 (6) = happyGoto action_26
action_25 (13) = happyGoto action_27
action_25 (14) = happyGoto action_28
action_25 _ = happyFail

action_26 _ = happyReduce_15

action_27 _ = happyReduce_18

action_28 (35) = happyShift action_29
action_28 (36) = happyShift action_30
action_28 (43) = happyShift action_32
action_28 (6) = happyGoto action_26
action_28 (13) = happyGoto action_53
action_28 _ = happyReduce_5

action_29 _ = happyReduce_3

action_30 _ = happyReduce_4

action_31 (35) = happyShift action_29
action_31 (36) = happyShift action_30
action_31 (43) = happyShift action_32
action_31 (6) = happyGoto action_26
action_31 (13) = happyGoto action_27
action_31 (14) = happyGoto action_52
action_31 _ = happyFail

action_32 (35) = happyShift action_29
action_32 (36) = happyShift action_30
action_32 (43) = happyShift action_32
action_32 (6) = happyGoto action_26
action_32 (13) = happyGoto action_27
action_32 (14) = happyGoto action_51
action_32 _ = happyFail

action_33 (44) = happyShift action_50
action_33 _ = happyFail

action_34 (31) = happyShift action_21
action_34 (43) = happyShift action_23
action_34 (45) = happyShift action_24
action_34 (15) = happyGoto action_49
action_34 _ = happyFail

action_35 (30) = happyShift action_48
action_35 _ = happyFail

action_36 (31) = happyShift action_21
action_36 (36) = happyShift action_22
action_36 (43) = happyShift action_23
action_36 (45) = happyShift action_24
action_36 (15) = happyGoto action_19
action_36 (16) = happyGoto action_47
action_36 _ = happyFail

action_37 (31) = happyShift action_21
action_37 (43) = happyShift action_23
action_37 (45) = happyShift action_24
action_37 (15) = happyGoto action_46
action_37 _ = happyFail

action_38 (31) = happyShift action_21
action_38 (43) = happyShift action_23
action_38 (45) = happyShift action_24
action_38 (15) = happyGoto action_45
action_38 _ = happyFail

action_39 (35) = happyShift action_44
action_39 (11) = happyGoto action_40
action_39 (12) = happyGoto action_41
action_39 (22) = happyGoto action_42
action_39 (23) = happyGoto action_43
action_39 _ = happyReduce_37

action_40 (38) = happyShift action_62
action_40 (24) = happyGoto action_60
action_40 (25) = happyGoto action_61
action_40 _ = happyReduce_40

action_41 _ = happyReduce_8

action_42 _ = happyReduce_14

action_43 _ = happyReduce_36

action_44 (35) = happyShift action_29
action_44 (36) = happyShift action_30
action_44 (43) = happyShift action_32
action_44 (6) = happyGoto action_26
action_44 (13) = happyGoto action_58
action_44 (18) = happyGoto action_59
action_44 _ = happyReduce_29

action_45 _ = happyReduce_22

action_46 _ = happyReduce_23

action_47 _ = happyReduce_25

action_48 (35) = happyShift action_29
action_48 (36) = happyShift action_30
action_48 (43) = happyShift action_32
action_48 (6) = happyGoto action_26
action_48 (13) = happyGoto action_27
action_48 (14) = happyGoto action_57
action_48 _ = happyFail

action_49 (33) = happyShift action_56
action_49 _ = happyFail

action_50 _ = happyReduce_21

action_51 (35) = happyShift action_29
action_51 (36) = happyShift action_30
action_51 (43) = happyShift action_32
action_51 (44) = happyShift action_55
action_51 (6) = happyGoto action_26
action_51 (13) = happyGoto action_53
action_51 _ = happyFail

action_52 (35) = happyShift action_29
action_52 (36) = happyShift action_30
action_52 (42) = happyShift action_54
action_52 (43) = happyShift action_32
action_52 (6) = happyGoto action_26
action_52 (13) = happyGoto action_53
action_52 _ = happyFail

action_53 _ = happyReduce_17

action_54 (40) = happyShift action_71
action_54 (9) = happyGoto action_68
action_54 (10) = happyGoto action_69
action_54 (19) = happyGoto action_70
action_54 _ = happyReduce_31

action_55 _ = happyReduce_16

action_56 (31) = happyShift action_21
action_56 (36) = happyShift action_22
action_56 (43) = happyShift action_23
action_56 (45) = happyShift action_24
action_56 (15) = happyGoto action_19
action_56 (16) = happyGoto action_67
action_56 _ = happyFail

action_57 (32) = happyShift action_66
action_57 (35) = happyShift action_29
action_57 (36) = happyShift action_30
action_57 (43) = happyShift action_32
action_57 (6) = happyGoto action_26
action_57 (13) = happyGoto action_53
action_57 _ = happyFail

action_58 (35) = happyShift action_29
action_58 (36) = happyShift action_30
action_58 (43) = happyShift action_32
action_58 (6) = happyGoto action_26
action_58 (13) = happyGoto action_58
action_58 (18) = happyGoto action_65
action_58 _ = happyReduce_29

action_59 _ = happyReduce_13

action_60 _ = happyReduce_38

action_61 (38) = happyShift action_62
action_61 (24) = happyGoto action_64
action_61 (25) = happyGoto action_61
action_61 _ = happyReduce_40

action_62 (35) = happyShift action_44
action_62 (11) = happyGoto action_63
action_62 _ = happyFail

action_63 _ = happyReduce_41

action_64 _ = happyReduce_39

action_65 _ = happyReduce_28

action_66 _ = happyReduce_19

action_67 _ = happyReduce_24

action_68 (40) = happyShift action_71
action_68 (9) = happyGoto action_68
action_68 (19) = happyGoto action_74
action_68 _ = happyReduce_31

action_69 _ = happyReduce_6

action_70 _ = happyReduce_12

action_71 (35) = happyShift action_73
action_71 (8) = happyGoto action_72
action_71 _ = happyFail

action_72 (33) = happyShift action_76
action_72 _ = happyFail

action_73 (36) = happyShift action_12
action_73 (5) = happyGoto action_75
action_73 (21) = happyGoto action_10
action_73 _ = happyReduce_35

action_74 _ = happyReduce_30

action_75 _ = happyReduce_10

action_76 (35) = happyShift action_29
action_76 (36) = happyShift action_30
action_76 (43) = happyShift action_32
action_76 (6) = happyGoto action_26
action_76 (13) = happyGoto action_27
action_76 (14) = happyGoto action_77
action_76 _ = happyFail

action_77 (35) = happyShift action_29
action_77 (36) = happyShift action_30
action_77 (43) = happyShift action_32
action_77 (6) = happyGoto action_26
action_77 (13) = happyGoto action_53
action_77 _ = happyReduce_11

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyAbsSyn21  happy_var_1)
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

happyReduce_9 = happySpecReduce_2  7 happyReduction_9
happyReduction_9 (HappyTerminal (TokenPath happy_var_2))
	_
	 =  HappyAbsSyn7
		 (Import happy_var_2
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_2  8 happyReduction_10
happyReduction_10 (HappyAbsSyn5  happy_var_2)
	(HappyTerminal (TokenCon happy_var_1))
	 =  HappyAbsSyn8
		 ((happy_var_1,happy_var_2)
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happyReduce 4 9 happyReduction_11
happyReduction_11 ((HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 ((happy_var_2,happy_var_4)
	) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_1  10 happyReduction_12
happyReduction_12 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_2  11 happyReduction_13
happyReduction_13 (HappyAbsSyn18  happy_var_2)
	(HappyTerminal (TokenCon happy_var_1))
	 =  HappyAbsSyn11
		 ((happy_var_1,length happy_var_2,error "Parser.y: ConDecl: constructor contracts aren't supported.")
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  12 happyReduction_14
happyReduction_14 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  13 happyReduction_15
happyReduction_15 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn13
		 (Named happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  13 happyReduction_16
happyReduction_16 _
	(HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (happy_var_2
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_2  14 happyReduction_17
happyReduction_17 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1 :@: happy_var_2
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  14 happyReduction_18
happyReduction_18 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happyReduce 5 15 happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (Pred happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_20 = happySpecReduce_1  15 happyReduction_20
happyReduction_20 _
	 =  HappyAbsSyn15
		 (CF
	)

happyReduce_21 = happySpecReduce_3  15 happyReduction_21
happyReduction_21 _
	(HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (happy_var_2
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  16 happyReduction_22
happyReduction_22 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn16
		 (And happy_var_1 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  16 happyReduction_23
happyReduction_23 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn16
		 (Or  happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happyReduce 5 16 happyReduction_24
happyReduction_24 ((HappyAbsSyn16  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (Arr (Just happy_var_1) happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_25 = happySpecReduce_3  16 happyReduction_25
happyReduction_25 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn16
		 (Arr Nothing happy_var_1 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  16 happyReduction_26
happyReduction_26 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_2  17 happyReduction_27
happyReduction_27 _
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_27 _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_2  18 happyReduction_28
happyReduction_28 (HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1:happy_var_2
	)
happyReduction_28 _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_0  18 happyReduction_29
happyReduction_29  =  HappyAbsSyn18
		 ([]
	)

happyReduce_30 = happySpecReduce_2  19 happyReduction_30
happyReduction_30 (HappyAbsSyn19  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1:happy_var_2
	)
happyReduction_30 _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_0  19 happyReduction_31
happyReduction_31  =  HappyAbsSyn19
		 ([]
	)

happyReduce_32 = happySpecReduce_2  20 happyReduction_32
happyReduction_32 (HappyAbsSyn20  happy_var_2)
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1:happy_var_2
	)
happyReduction_32 _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_0  20 happyReduction_33
happyReduction_33  =  HappyAbsSyn20
		 ([]
	)

happyReduce_34 = happySpecReduce_2  21 happyReduction_34
happyReduction_34 (HappyAbsSyn21  happy_var_2)
	(HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn21
		 (happy_var_1:happy_var_2
	)
happyReduction_34 _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_0  21 happyReduction_35
happyReduction_35  =  HappyAbsSyn21
		 ([]
	)

happyReduce_36 = happySpecReduce_1  22 happyReduction_36
happyReduction_36 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_0  22 happyReduction_37
happyReduction_37  =  HappyAbsSyn22
		 ([]
	)

happyReduce_38 = happySpecReduce_2  23 happyReduction_38
happyReduction_38 (HappyAbsSyn24  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1:happy_var_2
	)
happyReduction_38 _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_2  24 happyReduction_39
happyReduction_39 (HappyAbsSyn24  happy_var_2)
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1:happy_var_2
	)
happyReduction_39 _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_0  24 happyReduction_40
happyReduction_40  =  HappyAbsSyn24
		 ([]
	)

happyReduce_41 = happySpecReduce_2  25 happyReduction_41
happyReduction_41 (HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn25
		 (happy_var_2
	)
happyReduction_41 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 48 48 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenImport -> cont 26;
	TokenData -> cont 27;
	TokenEquals -> cont 28;
	TokenSatisfies -> cont 29;
	TokenColon -> cont 30;
	TokenCurlyO -> cont 31;
	TokenCurlyC -> cont 32;
	TokenArrow -> cont 33;
	TokenPath happy_dollar_dollar -> cont 34;
	TokenCon happy_dollar_dollar -> cont 35;
	TokenVar happy_dollar_dollar -> cont 36;
	TokenInt happy_dollar_dollar -> cont 37;
	TokenPipe -> cont 38;
	TokenDoubleSep -> cont 39;
	TokenSingleSep -> cont 40;
	TokenCase -> cont 41;
	TokenOf -> cont 42;
	TokenParenO -> cont 43;
	TokenParenC -> cont 44;
	TokenCF -> cont 45;
	TokenOr -> cont 46;
	TokenAnd -> cont 47;
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
           | TokenImport
           | TokenInt Int
           | TokenPipe
           | TokenCF
           | TokenAny
           | TokenDoubleSep
           | TokenSingleSep
           | TokenEquals
           | TokenSatisfies
           | TokenCon String -- Upper case var
           | TokenVar String -- Lower case var
           | TokenPath FilePath
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
-- drop module decls because they're for GHC
lexer ('m':'o':'d':'u':'l':'e':cs) = lexer $ dropWhile (/= '\n') cs 
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
lexer (';':';':cs) = TokenDoubleSep : lexer cs
lexer (';':cs) = TokenSingleSep : lexer cs
lexer ('|':cs) = TokenPipe : lexer cs
lexer (',':cs) = TokenComma : lexer cs
lexer ('"':cs) = lexPath cs
lexer ('\'':_) = error "Single quotes (\"'\") are not allowed in source files :P"
-- Discard comments.
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
    "data"   -> TokenData
    "case"   -> TokenCase
    "of"     -> TokenOf
--    "Any"  -> TokenAny
    "CF"     -> TokenCF
    "import" -> TokenImport
    _        -> (if isUpper c then TokenCon else TokenVar) var

-- A path is a double-quoted string, without nested double quotes.
lexPath cs = TokenPath p : lexer rest
  where (p,'"':rest) = span (/='"') cs


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
