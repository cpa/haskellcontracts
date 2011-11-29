{-# OPTIONS_GHC -w #-}
module Parser where
import Data.Char
import Haskell
import Prelude hiding (lex)

-- parser produced by Happy Version 1.18.6

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31
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
	| HappyAbsSyn26 t26
	| HappyAbsSyn27 t27
	| HappyAbsSyn28 t28
	| HappyAbsSyn29 t29
	| HappyAbsSyn30 t30
	| HappyAbsSyn31 t31

action_0 (32) = happyShift action_5
action_0 (33) = happyShift action_6
action_0 (41) = happyShift action_7
action_0 (54) = happyShift action_8
action_0 (4) = happyGoto action_9
action_0 (8) = happyGoto action_2
action_0 (19) = happyGoto action_3
action_0 (21) = happyGoto action_4
action_0 _ = happyReduce_35

action_1 (32) = happyShift action_5
action_1 (33) = happyShift action_6
action_1 (41) = happyShift action_7
action_1 (54) = happyShift action_8
action_1 (8) = happyGoto action_2
action_1 (19) = happyGoto action_3
action_1 (21) = happyGoto action_4
action_1 _ = happyFail

action_2 (43) = happyShift action_20
action_2 _ = happyFail

action_3 (32) = happyShift action_5
action_3 (33) = happyShift action_6
action_3 (41) = happyShift action_7
action_3 (54) = happyShift action_8
action_3 (8) = happyGoto action_2
action_3 (19) = happyGoto action_3
action_3 (21) = happyGoto action_19
action_3 _ = happyReduce_35

action_4 _ = happyReduce_1

action_5 (40) = happyShift action_18
action_5 (7) = happyGoto action_16
action_5 (25) = happyGoto action_17
action_5 _ = happyFail

action_6 (40) = happyShift action_15
action_6 _ = happyFail

action_7 (41) = happyShift action_13
action_7 (50) = happyShift action_14
action_7 (5) = happyGoto action_11
action_7 (23) = happyGoto action_12
action_7 _ = happyReduce_39

action_8 (41) = happyShift action_10
action_8 _ = happyFail

action_9 (56) = happyAccept
action_9 _ = happyFail

action_10 (35) = happyShift action_28
action_10 _ = happyFail

action_11 (34) = happyShift action_27
action_11 _ = happyFail

action_12 _ = happyReduce_2

action_13 (41) = happyShift action_13
action_13 (23) = happyGoto action_26
action_13 _ = happyReduce_39

action_14 (41) = happyShift action_25
action_14 _ = happyFail

action_15 (41) = happyShift action_13
action_15 (5) = happyGoto action_24
action_15 (23) = happyGoto action_12
action_15 _ = happyReduce_39

action_16 _ = happyReduce_10

action_17 _ = happyReduce_5

action_18 (45) = happyShift action_23
action_18 (27) = happyGoto action_21
action_18 (29) = happyGoto action_22
action_18 _ = happyReduce_45

action_19 _ = happyReduce_34

action_20 _ = happyReduce_31

action_21 _ = happyReduce_42

action_22 (45) = happyShift action_23
action_22 (27) = happyGoto action_47
action_22 (29) = happyGoto action_22
action_22 _ = happyReduce_45

action_23 (40) = happyShift action_46
action_23 _ = happyFail

action_24 (34) = happyShift action_45
action_24 _ = happyFail

action_25 (50) = happyShift action_44
action_25 _ = happyFail

action_26 _ = happyReduce_38

action_27 (40) = happyShift action_40
action_27 (41) = happyShift action_41
action_27 (46) = happyShift action_42
action_27 (48) = happyShift action_43
action_27 (6) = happyGoto action_35
action_27 (11) = happyGoto action_36
action_27 (14) = happyGoto action_37
action_27 (15) = happyGoto action_38
action_27 (16) = happyGoto action_39
action_27 _ = happyFail

action_28 (37) = happyShift action_31
action_28 (41) = happyShift action_32
action_28 (48) = happyShift action_33
action_28 (51) = happyShift action_34
action_28 (17) = happyGoto action_29
action_28 (18) = happyGoto action_30
action_28 _ = happyFail

action_29 (39) = happyShift action_62
action_29 (52) = happyShift action_63
action_29 (53) = happyShift action_64
action_29 _ = happyReduce_30

action_30 (55) = happyShift action_61
action_30 _ = happyFail

action_31 (41) = happyShift action_60
action_31 _ = happyFail

action_32 (36) = happyShift action_59
action_32 _ = happyFail

action_33 (37) = happyShift action_31
action_33 (41) = happyShift action_32
action_33 (48) = happyShift action_33
action_33 (51) = happyShift action_34
action_33 (17) = happyGoto action_29
action_33 (18) = happyGoto action_58
action_33 _ = happyFail

action_34 _ = happyReduce_24

action_35 _ = happyReduce_17

action_36 _ = happyReduce_6

action_37 _ = happyReduce_20

action_38 (40) = happyShift action_40
action_38 (41) = happyShift action_41
action_38 (48) = happyShift action_43
action_38 (50) = happyShift action_57
action_38 (6) = happyGoto action_35
action_38 (14) = happyGoto action_56
action_38 _ = happyReduce_22

action_39 _ = happyReduce_13

action_40 _ = happyReduce_3

action_41 _ = happyReduce_4

action_42 (40) = happyShift action_40
action_42 (41) = happyShift action_41
action_42 (48) = happyShift action_43
action_42 (6) = happyGoto action_35
action_42 (14) = happyGoto action_37
action_42 (15) = happyGoto action_38
action_42 (16) = happyGoto action_55
action_42 _ = happyFail

action_43 (40) = happyShift action_40
action_43 (41) = happyShift action_41
action_43 (48) = happyShift action_43
action_43 (6) = happyGoto action_35
action_43 (14) = happyGoto action_37
action_43 (15) = happyGoto action_38
action_43 (16) = happyGoto action_54
action_43 _ = happyFail

action_44 (41) = happyShift action_13
action_44 (5) = happyGoto action_53
action_44 (23) = happyGoto action_12
action_44 _ = happyReduce_39

action_45 (40) = happyShift action_52
action_45 (12) = happyGoto action_48
action_45 (13) = happyGoto action_49
action_45 (24) = happyGoto action_50
action_45 (28) = happyGoto action_51
action_45 _ = happyReduce_41

action_46 _ = happyReduce_47

action_47 _ = happyReduce_44

action_48 (42) = happyShift action_79
action_48 (30) = happyGoto action_77
action_48 (31) = happyGoto action_78
action_48 _ = happyReduce_49

action_49 _ = happyReduce_9

action_50 _ = happyReduce_16

action_51 _ = happyReduce_40

action_52 (40) = happyShift action_40
action_52 (41) = happyShift action_41
action_52 (48) = happyShift action_43
action_52 (6) = happyGoto action_35
action_52 (14) = happyGoto action_75
action_52 (20) = happyGoto action_76
action_52 _ = happyReduce_33

action_53 (34) = happyShift action_74
action_53 _ = happyFail

action_54 (49) = happyShift action_73
action_54 _ = happyFail

action_55 (47) = happyShift action_72
action_55 _ = happyFail

action_56 _ = happyReduce_19

action_57 (40) = happyShift action_40
action_57 (41) = happyShift action_41
action_57 (6) = happyGoto action_71
action_57 _ = happyFail

action_58 (49) = happyShift action_70
action_58 _ = happyFail

action_59 (37) = happyShift action_31
action_59 (48) = happyShift action_33
action_59 (51) = happyShift action_34
action_59 (17) = happyGoto action_69
action_59 _ = happyFail

action_60 (36) = happyShift action_68
action_60 _ = happyFail

action_61 _ = happyReduce_8

action_62 (37) = happyShift action_31
action_62 (41) = happyShift action_32
action_62 (48) = happyShift action_33
action_62 (51) = happyShift action_34
action_62 (17) = happyGoto action_29
action_62 (18) = happyGoto action_67
action_62 _ = happyFail

action_63 (37) = happyShift action_31
action_63 (48) = happyShift action_33
action_63 (51) = happyShift action_34
action_63 (17) = happyGoto action_66
action_63 _ = happyFail

action_64 (37) = happyShift action_31
action_64 (48) = happyShift action_33
action_64 (51) = happyShift action_34
action_64 (17) = happyGoto action_65
action_64 _ = happyFail

action_65 _ = happyReduce_26

action_66 _ = happyReduce_27

action_67 _ = happyReduce_29

action_68 (40) = happyShift action_40
action_68 (41) = happyShift action_41
action_68 (48) = happyShift action_43
action_68 (6) = happyGoto action_35
action_68 (14) = happyGoto action_37
action_68 (15) = happyGoto action_38
action_68 (16) = happyGoto action_87
action_68 _ = happyFail

action_69 (39) = happyShift action_86
action_69 _ = happyFail

action_70 _ = happyReduce_25

action_71 (50) = happyShift action_85
action_71 _ = happyFail

action_72 (37) = happyShift action_84
action_72 _ = happyFail

action_73 _ = happyReduce_18

action_74 (40) = happyShift action_40
action_74 (41) = happyShift action_41
action_74 (46) = happyShift action_42
action_74 (48) = happyShift action_43
action_74 (6) = happyGoto action_35
action_74 (11) = happyGoto action_83
action_74 (14) = happyGoto action_37
action_74 (15) = happyGoto action_38
action_74 (16) = happyGoto action_39
action_74 _ = happyFail

action_75 (40) = happyShift action_40
action_75 (41) = happyShift action_41
action_75 (48) = happyShift action_43
action_75 (6) = happyGoto action_35
action_75 (14) = happyGoto action_75
action_75 (20) = happyGoto action_82
action_75 _ = happyReduce_33

action_76 _ = happyReduce_15

action_77 _ = happyReduce_46

action_78 (42) = happyShift action_79
action_78 (30) = happyGoto action_81
action_78 (31) = happyGoto action_78
action_78 _ = happyReduce_49

action_79 (40) = happyShift action_52
action_79 (12) = happyGoto action_80
action_79 _ = happyFail

action_80 _ = happyReduce_50

action_81 _ = happyReduce_48

action_82 _ = happyReduce_32

action_83 _ = happyReduce_7

action_84 (44) = happyShift action_93
action_84 (22) = happyGoto action_91
action_84 (26) = happyGoto action_92
action_84 _ = happyReduce_37

action_85 (40) = happyShift action_40
action_85 (41) = happyShift action_41
action_85 (48) = happyShift action_43
action_85 (6) = happyGoto action_35
action_85 (14) = happyGoto action_37
action_85 (15) = happyGoto action_90
action_85 _ = happyFail

action_86 (37) = happyShift action_31
action_86 (41) = happyShift action_32
action_86 (48) = happyShift action_33
action_86 (51) = happyShift action_34
action_86 (17) = happyGoto action_29
action_86 (18) = happyGoto action_89
action_86 _ = happyFail

action_87 (38) = happyShift action_88
action_87 _ = happyFail

action_88 _ = happyReduce_23

action_89 _ = happyReduce_28

action_90 (40) = happyShift action_40
action_90 (41) = happyShift action_41
action_90 (48) = happyShift action_43
action_90 (6) = happyGoto action_35
action_90 (14) = happyGoto action_56
action_90 _ = happyReduce_21

action_91 (38) = happyShift action_98
action_91 _ = happyFail

action_92 (44) = happyShift action_93
action_92 (22) = happyGoto action_97
action_92 (26) = happyGoto action_92
action_92 _ = happyReduce_37

action_93 (40) = happyShift action_96
action_93 (9) = happyGoto action_94
action_93 (10) = happyGoto action_95
action_93 _ = happyFail

action_94 (39) = happyShift action_100
action_94 _ = happyFail

action_95 _ = happyReduce_43

action_96 (41) = happyShift action_13
action_96 (5) = happyGoto action_99
action_96 (23) = happyGoto action_12
action_96 _ = happyReduce_39

action_97 _ = happyReduce_36

action_98 _ = happyReduce_14

action_99 _ = happyReduce_11

action_100 (40) = happyShift action_40
action_100 (41) = happyShift action_41
action_100 (46) = happyShift action_42
action_100 (48) = happyShift action_43
action_100 (6) = happyGoto action_35
action_100 (11) = happyGoto action_101
action_100 (14) = happyGoto action_37
action_100 (15) = happyGoto action_38
action_100 (16) = happyGoto action_39
action_100 _ = happyFail

action_101 _ = happyReduce_12

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyAbsSyn23  happy_var_1)
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

happyReduce_5 = happySpecReduce_1  7 happyReduction_5
happyReduction_5 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happyReduce 4 8 happyReduction_6
happyReduction_6 ((HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (Def $ Let happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 7 8 happyReduction_7
happyReduction_7 ((HappyAbsSyn11  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (Def $ Let happy_var_3 (happy_var_1:happy_var_5) happy_var_7
	) `HappyStk` happyRest

happyReduce_8 = happyReduce 5 8 happyReduction_8
happyReduction_8 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (ContSat $ Satisfies happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_9 = happyReduce 5 8 happyReduction_9
happyReduction_9 ((HappyAbsSyn13  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenCon happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (DataType $ Data happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_2  8 happyReduction_10
happyReduction_10 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (Import happy_var_2
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  9 happyReduction_11
happyReduction_11 (HappyAbsSyn5  happy_var_2)
	(HappyTerminal (TokenCon happy_var_1))
	 =  HappyAbsSyn9
		 ((happy_var_1, happy_var_2)
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  10 happyReduction_12
happyReduction_12 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn10
		 ((happy_var_1, happy_var_3)
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  11 happyReduction_13
happyReduction_13 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn11
		 (Base happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happyReduce 6 11 happyReduction_14
happyReduction_14 (_ `HappyStk`
	(HappyAbsSyn22  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (Case happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_2  12 happyReduction_15
happyReduction_15 (HappyAbsSyn20  happy_var_2)
	(HappyTerminal (TokenCon happy_var_1))
	 =  HappyAbsSyn12
		 ((happy_var_1,length happy_var_2,Any)
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  13 happyReduction_16
happyReduction_16 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  14 happyReduction_17
happyReduction_17 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn14
		 (Named happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  14 happyReduction_18
happyReduction_18 _
	(HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (happy_var_2
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_2  15 happyReduction_19
happyReduction_19 (HappyAbsSyn14  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1 :@: happy_var_2
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  15 happyReduction_20
happyReduction_20 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happyReduce 5 16 happyReduction_21
happyReduction_21 ((HappyAbsSyn15  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 ((Named happy_var_3 :@: happy_var_1) :@: happy_var_5
	) `HappyStk` happyRest

happyReduce_22 = happySpecReduce_1  16 happyReduction_22
happyReduction_22 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happyReduce 5 17 happyReduction_23
happyReduction_23 (_ `HappyStk`
	(HappyAbsSyn16  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (Pred happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_1  17 happyReduction_24
happyReduction_24 _
	 =  HappyAbsSyn17
		 (CF
	)

happyReduce_25 = happySpecReduce_3  17 happyReduction_25
happyReduction_25 _
	(HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn17
		 (happy_var_2
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  18 happyReduction_26
happyReduction_26 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn18
		 (And happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  18 happyReduction_27
happyReduction_27 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn18
		 (Or  happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happyReduce 5 18 happyReduction_28
happyReduction_28 ((HappyAbsSyn18  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (Arr (Just happy_var_1) happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_29 = happySpecReduce_3  18 happyReduction_29
happyReduction_29 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn18
		 (Arr Nothing happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  18 happyReduction_30
happyReduction_30 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_2  19 happyReduction_31
happyReduction_31 _
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_31 _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_2  20 happyReduction_32
happyReduction_32 (HappyAbsSyn20  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
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
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1:happy_var_2
	)
happyReduction_34 _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_0  21 happyReduction_35
happyReduction_35  =  HappyAbsSyn21
		 ([]
	)

happyReduce_36 = happySpecReduce_2  22 happyReduction_36
happyReduction_36 (HappyAbsSyn22  happy_var_2)
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1:happy_var_2
	)
happyReduction_36 _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_0  22 happyReduction_37
happyReduction_37  =  HappyAbsSyn22
		 ([]
	)

happyReduce_38 = happySpecReduce_2  23 happyReduction_38
happyReduction_38 (HappyAbsSyn23  happy_var_2)
	(HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn23
		 (happy_var_1:happy_var_2
	)
happyReduction_38 _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_0  23 happyReduction_39
happyReduction_39  =  HappyAbsSyn23
		 ([]
	)

happyReduce_40 = happySpecReduce_1  24 happyReduction_40
happyReduction_40 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_0  24 happyReduction_41
happyReduction_41  =  HappyAbsSyn24
		 ([]
	)

happyReduce_42 = happySpecReduce_2  25 happyReduction_42
happyReduction_42 (HappyAbsSyn27  happy_var_2)
	(HappyTerminal (TokenCon happy_var_1))
	 =  HappyAbsSyn25
		 (happy_var_1:happy_var_2
	)
happyReduction_42 _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_2  26 happyReduction_43
happyReduction_43 (HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (happy_var_2
	)
happyReduction_43 _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_2  27 happyReduction_44
happyReduction_44 (HappyAbsSyn27  happy_var_2)
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1:happy_var_2
	)
happyReduction_44 _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_0  27 happyReduction_45
happyReduction_45  =  HappyAbsSyn27
		 ([]
	)

happyReduce_46 = happySpecReduce_2  28 happyReduction_46
happyReduction_46 (HappyAbsSyn30  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn28
		 (happy_var_1:happy_var_2
	)
happyReduction_46 _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_2  29 happyReduction_47
happyReduction_47 (HappyTerminal (TokenCon happy_var_2))
	_
	 =  HappyAbsSyn29
		 (happy_var_2
	)
happyReduction_47 _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_2  30 happyReduction_48
happyReduction_48 (HappyAbsSyn30  happy_var_2)
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1:happy_var_2
	)
happyReduction_48 _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_0  30 happyReduction_49
happyReduction_49  =  HappyAbsSyn30
		 ([]
	)

happyReduce_50 = happySpecReduce_2  31 happyReduction_50
happyReduction_50 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn31
		 (happy_var_2
	)
happyReduction_50 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 56 56 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenImport -> cont 32;
	TokenData -> cont 33;
	TokenEquals -> cont 34;
	TokenSatisfies -> cont 35;
	TokenColon -> cont 36;
	TokenCurlyO -> cont 37;
	TokenCurlyC -> cont 38;
	TokenArrow -> cont 39;
	TokenCon happy_dollar_dollar -> cont 40;
	TokenVar happy_dollar_dollar -> cont 41;
	TokenPipe -> cont 42;
	TokenDoubleSep -> cont 43;
	TokenSingleSep -> cont 44;
	TokenDot -> cont 45;
	TokenCase -> cont 46;
	TokenOf -> cont 47;
	TokenParenO -> cont 48;
	TokenParenC -> cont 49;
	TokenBackTick -> cont 50;
	TokenCF -> cont 51;
	TokenOr -> cont 52;
	TokenAnd -> cont 53;
	TokenContractPragmaO -> cont 54;
	TokenPragmaC -> cont 55;
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
           | TokenPipe
           | TokenCF
           | TokenAny
           | TokenDoubleSep
           | TokenSingleSep
           | TokenEquals
           | TokenSatisfies
           | TokenCon String -- Upper case var
           | TokenVar String -- Lower case var
           | TokenDot
           | TokenArrow
           | TokenColon
           | TokenParenO -- 'O' = 'open'
           | TokenParenC -- 'C' = 'close'
           | TokenCurlyO
           | TokenCurlyC
           | TokenComma
           | TokenBackTick
           | TokenOr
           | TokenAnd
           | TokenContractPragmaO
           | TokenPragmaC
           deriving (Eq,Show)

lex :: String -> [Token]
lex [] = []
-- Skip lines we don't care about.
--
-- XXX: this is pretty ad-hoc.  Maybe better to use CPP?  Use
-- '-fno-warn-unrecognised-pragmas' to avoid GHC's whining.
lex ('{':'-':'#':' ':'S':'K':'I':'P':' ':'#':'-':'}':cs) = lex . skip $ skip cs
lex ('=':cs) = TokenEquals : lex cs
lex (':':':':':':cs) = TokenSatisfies : lex cs
lex (':':cs) = TokenColon : lex cs
lex ('|':'|':cs) = TokenOr : lex cs
lex ('&':'&':cs) = TokenAnd : lex cs
lex ('-':'>':cs) = TokenArrow : lex cs
-- The POPL 09 syntax.
lex ('{':'-':'#':' ':'C':'O':'N':'T':'R':'A':'C':'T':cs)
  = TokenContractPragmaO : lex cs
lex ('#':'-':'}':cs) = TokenPragmaC : lex cs
lex ('{':cs) = TokenCurlyO : lex cs
lex ('}':cs) = TokenCurlyC : lex cs
lex ('(':cs) = TokenParenO : lex cs
lex (')':cs) = TokenParenC : lex cs
lex (';':';':cs) = TokenDoubleSep : lex cs
lex (';':cs) = TokenSingleSep : lex cs
lex ('|':cs) = TokenPipe : lex cs
lex (',':cs) = TokenComma : lex cs
lex ('.':cs) = TokenDot : lex cs
lex ('`':cs) = TokenBackTick : lex cs
lex ('\'':_) = error "Single quotes (\"'\") are not allowed in source files :P"
-- Discard comments.
lex ('-':'-':cs) = lex $ skip cs
lex (c:cs) 
      | isSpace c = lex cs
      | isAlpha c = lexVar (c:cs)
      | isDigit c = error "We don't lex numbers currently!" -- lexInt (c:cs)
{-
lexInt cs = TokenInt (read num) : lex rest
      where (num,rest) = span isDigit cs
-}
lex cs = error $ "Don't know how to lex: "++(head . lines $ cs)

skip cs = let cs' = dropWhile (/= '\n') cs
          in if not (null cs')
             then tail cs' -- Drop the newline, if any.
             else ""

lexVar (c:cs) = token : lex rest where
  (var,rest) = span (\x -> isAlpha x || x == '_' || isDigit x) (c:cs)
  token = case var of
    "data"   -> TokenData
    "case"   -> TokenCase
    "of"     -> TokenOf
--    "Any"  -> TokenAny
    "CF"     -> TokenCF
    "import" -> TokenImport
    _        -> (if isUpper c then TokenCon else TokenVar) var

parse = haskell . lex

main = getContents >>= print . parse
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
