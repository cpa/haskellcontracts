{-# OPTIONS_GHC -w #-}
module Parser where
import Data.Char
import Haskell

-- parser produced by Happy Version 1.18.6

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13
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

action_0 (15) = happyShift action_3
action_0 (23) = happyShift action_4
action_0 (4) = happyGoto action_5
action_0 (6) = happyGoto action_6
action_0 _ = happyReduce_3

action_1 (15) = happyShift action_3
action_1 (23) = happyShift action_4
action_1 (6) = happyGoto action_2
action_1 _ = happyFail

action_2 (26) = happyShift action_7
action_2 _ = happyFail

action_3 (23) = happyShift action_11
action_3 _ = happyFail

action_4 (18) = happyShift action_9
action_4 (23) = happyShift action_10
action_4 (10) = happyGoto action_8
action_4 _ = happyReduce_31

action_5 (31) = happyAccept
action_5 _ = happyFail

action_6 (26) = happyShift action_7
action_6 _ = happyReduce_2

action_7 (15) = happyShift action_3
action_7 (23) = happyShift action_4
action_7 (4) = happyGoto action_19
action_7 (6) = happyGoto action_6
action_7 _ = happyReduce_3

action_8 (16) = happyShift action_18
action_8 _ = happyFail

action_9 (20) = happyShift action_15
action_9 (23) = happyShift action_16
action_9 (29) = happyShift action_17
action_9 (13) = happyGoto action_14
action_9 _ = happyFail

action_10 (23) = happyShift action_10
action_10 (10) = happyGoto action_13
action_10 _ = happyReduce_31

action_11 (23) = happyShift action_10
action_11 (10) = happyGoto action_12
action_11 _ = happyReduce_31

action_12 (16) = happyShift action_29
action_12 _ = happyFail

action_13 _ = happyReduce_30

action_14 _ = happyReduce_9

action_15 (23) = happyShift action_28
action_15 _ = happyFail

action_16 (19) = happyShift action_27
action_16 _ = happyFail

action_17 (23) = happyShift action_26
action_17 _ = happyFail

action_18 (17) = happyShift action_22
action_18 (23) = happyShift action_23
action_18 (27) = happyShift action_24
action_18 (29) = happyShift action_25
action_18 (11) = happyGoto action_20
action_18 (12) = happyGoto action_21
action_18 _ = happyFail

action_19 _ = happyReduce_1

action_20 _ = happyReduce_39

action_21 (23) = happyShift action_23
action_21 (29) = happyShift action_41
action_21 (11) = happyGoto action_40
action_21 _ = happyReduce_7

action_22 (17) = happyShift action_22
action_22 (23) = happyShift action_23
action_22 (29) = happyShift action_25
action_22 (11) = happyGoto action_20
action_22 (12) = happyGoto action_39
action_22 _ = happyReduce_36

action_23 _ = happyReduce_32

action_24 (17) = happyShift action_22
action_24 (23) = happyShift action_23
action_24 (29) = happyShift action_25
action_24 (11) = happyGoto action_20
action_24 (12) = happyGoto action_38
action_24 _ = happyFail

action_25 (17) = happyShift action_37
action_25 (23) = happyShift action_23
action_25 (29) = happyShift action_25
action_25 (11) = happyGoto action_20
action_25 (12) = happyGoto action_36
action_25 _ = happyFail

action_26 (19) = happyShift action_35
action_26 _ = happyFail

action_27 (20) = happyShift action_15
action_27 (23) = happyShift action_16
action_27 (29) = happyShift action_17
action_27 (13) = happyGoto action_34
action_27 _ = happyFail

action_28 (19) = happyShift action_33
action_28 _ = happyFail

action_29 (23) = happyShift action_31
action_29 (29) = happyShift action_32
action_29 (9) = happyGoto action_30
action_29 _ = happyReduce_29

action_30 _ = happyReduce_10

action_31 (24) = happyShift action_51
action_31 _ = happyFail

action_32 (23) = happyShift action_50
action_32 _ = happyFail

action_33 (17) = happyShift action_22
action_33 (23) = happyShift action_23
action_33 (29) = happyShift action_25
action_33 (11) = happyGoto action_20
action_33 (12) = happyGoto action_49
action_33 _ = happyFail

action_34 (22) = happyShift action_48
action_34 _ = happyFail

action_35 (20) = happyShift action_15
action_35 (23) = happyShift action_16
action_35 (29) = happyShift action_17
action_35 (13) = happyGoto action_47
action_35 _ = happyFail

action_36 (23) = happyShift action_23
action_36 (29) = happyShift action_41
action_36 (30) = happyShift action_46
action_36 (11) = happyGoto action_45
action_36 _ = happyFail

action_37 (17) = happyShift action_22
action_37 (23) = happyShift action_23
action_37 (29) = happyShift action_25
action_37 (11) = happyGoto action_20
action_37 (12) = happyGoto action_44
action_37 _ = happyReduce_36

action_38 (23) = happyShift action_23
action_38 (28) = happyShift action_43
action_38 (29) = happyShift action_41
action_38 (11) = happyGoto action_40
action_38 _ = happyFail

action_39 (23) = happyShift action_23
action_39 (29) = happyShift action_41
action_39 (11) = happyGoto action_40
action_39 _ = happyReduce_38

action_40 _ = happyReduce_35

action_41 (17) = happyShift action_22
action_41 (23) = happyShift action_23
action_41 (29) = happyShift action_25
action_41 (11) = happyGoto action_20
action_41 (12) = happyGoto action_42
action_41 _ = happyFail

action_42 (23) = happyShift action_23
action_42 (29) = happyShift action_41
action_42 (30) = happyShift action_46
action_42 (11) = happyGoto action_40
action_42 _ = happyFail

action_43 (25) = happyShift action_61
action_43 (7) = happyGoto action_60
action_43 _ = happyReduce_15

action_44 (23) = happyShift action_23
action_44 (29) = happyShift action_41
action_44 (30) = happyShift action_59
action_44 (11) = happyGoto action_40
action_44 _ = happyFail

action_45 (30) = happyShift action_58
action_45 _ = happyReduce_35

action_46 _ = happyReduce_33

action_47 (22) = happyShift action_57
action_47 _ = happyFail

action_48 (20) = happyShift action_15
action_48 (23) = happyShift action_16
action_48 (29) = happyShift action_17
action_48 (13) = happyGoto action_56
action_48 _ = happyFail

action_49 (21) = happyShift action_55
action_49 (23) = happyShift action_23
action_49 (29) = happyShift action_41
action_49 (11) = happyGoto action_40
action_49 _ = happyFail

action_50 (24) = happyShift action_54
action_50 _ = happyFail

action_51 (18) = happyShift action_52
action_51 (25) = happyShift action_53
action_51 _ = happyReduce_23

action_52 (20) = happyShift action_15
action_52 (23) = happyShift action_16
action_52 (29) = happyShift action_17
action_52 (13) = happyGoto action_68
action_52 _ = happyFail

action_53 (23) = happyShift action_31
action_53 (29) = happyShift action_32
action_53 (9) = happyGoto action_67
action_53 _ = happyReduce_29

action_54 (30) = happyShift action_66
action_54 _ = happyFail

action_55 _ = happyReduce_40

action_56 _ = happyReduce_41

action_57 (20) = happyShift action_15
action_57 (23) = happyShift action_16
action_57 (29) = happyShift action_17
action_57 (13) = happyGoto action_65
action_57 _ = happyFail

action_58 _ = happyReduce_34

action_59 _ = happyReduce_37

action_60 _ = happyReduce_8

action_61 (23) = happyShift action_63
action_61 (29) = happyShift action_64
action_61 (8) = happyGoto action_62
action_61 _ = happyReduce_20

action_62 (22) = happyShift action_76
action_62 _ = happyFail

action_63 (22) = happyReduce_20
action_63 (23) = happyShift action_63
action_63 (29) = happyShift action_75
action_63 (30) = happyReduce_20
action_63 (8) = happyGoto action_74
action_63 _ = happyReduce_20

action_64 (23) = happyShift action_73
action_64 _ = happyFail

action_65 (30) = happyShift action_72
action_65 _ = happyFail

action_66 (18) = happyShift action_70
action_66 (25) = happyShift action_71
action_66 _ = happyReduce_24

action_67 _ = happyReduce_21

action_68 (25) = happyShift action_69
action_68 _ = happyReduce_27

action_69 (23) = happyShift action_31
action_69 (29) = happyShift action_32
action_69 (9) = happyGoto action_84
action_69 _ = happyReduce_29

action_70 (20) = happyShift action_15
action_70 (23) = happyShift action_16
action_70 (29) = happyShift action_17
action_70 (13) = happyGoto action_83
action_70 _ = happyFail

action_71 (23) = happyShift action_31
action_71 (29) = happyShift action_32
action_71 (9) = happyGoto action_82
action_71 _ = happyReduce_29

action_72 _ = happyReduce_42

action_73 (30) = happyShift action_81
action_73 _ = happyFail

action_74 _ = happyReduce_16

action_75 (23) = happyShift action_80
action_75 (29) = happyShift action_64
action_75 (8) = happyGoto action_79
action_75 _ = happyReduce_20

action_76 (17) = happyShift action_22
action_76 (23) = happyShift action_23
action_76 (29) = happyShift action_78
action_76 (11) = happyGoto action_20
action_76 (12) = happyGoto action_77
action_76 _ = happyFail

action_77 (23) = happyShift action_23
action_77 (25) = happyShift action_61
action_77 (26) = happyReduce_15
action_77 (29) = happyShift action_41
action_77 (31) = happyReduce_15
action_77 (7) = happyGoto action_88
action_77 (11) = happyGoto action_40
action_77 _ = happyReduce_15

action_78 (17) = happyShift action_37
action_78 (23) = happyShift action_23
action_78 (29) = happyShift action_25
action_78 (11) = happyGoto action_20
action_78 (12) = happyGoto action_87
action_78 _ = happyFail

action_79 (30) = happyShift action_86
action_79 _ = happyFail

action_80 (23) = happyShift action_63
action_80 (29) = happyShift action_75
action_80 (30) = happyShift action_81
action_80 (8) = happyGoto action_74
action_80 _ = happyFail

action_81 _ = happyReduce_19

action_82 _ = happyReduce_22

action_83 (25) = happyShift action_85
action_83 _ = happyReduce_28

action_84 _ = happyReduce_25

action_85 (23) = happyShift action_31
action_85 (29) = happyShift action_32
action_85 (9) = happyGoto action_90
action_85 _ = happyReduce_29

action_86 _ = happyReduce_17

action_87 (23) = happyShift action_23
action_87 (29) = happyShift action_41
action_87 (30) = happyShift action_89
action_87 (11) = happyGoto action_45
action_87 _ = happyFail

action_88 _ = happyReduce_11

action_89 (25) = happyShift action_61
action_89 (26) = happyReduce_33
action_89 (31) = happyReduce_33
action_89 (7) = happyGoto action_91
action_89 _ = happyReduce_33

action_90 _ = happyReduce_26

action_91 _ = happyReduce_12

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
happyReduction_7 ((HappyAbsSyn12  happy_var_4) `HappyStk`
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
	(HappyAbsSyn12  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Def $ LetCase (map toLower happy_var_1) happy_var_2 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_3  6 happyReduction_9
happyReduction_9 (HappyAbsSyn13  happy_var_3)
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
	(HappyAbsSyn12  happy_var_4) `HappyStk`
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
	(HappyAbsSyn12  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 ((happy_var_2,happy_var_5):happy_var_7
	) `HappyStk` happyRest

happyReduce_13 = happyReduce 4 7 happyReduction_13
happyReduction_13 ((HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 ([(happy_var_2,happy_var_4)]
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 6 7 happyReduction_14
happyReduction_14 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_5) `HappyStk`
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
		 ((map toLower happy_var_1,happy_var_2,okContract happy_var_2):happy_var_4
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
		 ((map toLower happy_var_2,happy_var_3, okContract happy_var_3):happy_var_6
	) `HappyStk` happyRest

happyReduce_23 = happySpecReduce_2  9 happyReduction_23
happyReduction_23 (HappyTerminal (TokenInt happy_var_2))
	(HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn9
		 ([(map toLower happy_var_1,happy_var_2,okContract happy_var_2)]
	)
happyReduction_23 _ _  = notHappyAtAll 

happyReduce_24 = happyReduce 4 9 happyReduction_24
happyReduction_24 (_ `HappyStk`
	(HappyTerminal (TokenInt happy_var_3)) `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 ([(map toLower happy_var_2,happy_var_3,okContract happy_var_3)]
	) `HappyStk` happyRest

happyReduce_25 = happyReduce 6 9 happyReduction_25
happyReduction_25 ((HappyAbsSyn9  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenInt happy_var_2)) `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 ((map toLower happy_var_1,happy_var_2,happy_var_4):happy_var_6
	) `HappyStk` happyRest

happyReduce_26 = happyReduce 8 9 happyReduction_26
happyReduction_26 ((HappyAbsSyn9  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenInt happy_var_3)) `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 ((map toLower happy_var_2,happy_var_3,happy_var_6):happy_var_8
	) `HappyStk` happyRest

happyReduce_27 = happyReduce 4 9 happyReduction_27
happyReduction_27 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenInt happy_var_2)) `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 ([(map toLower happy_var_1,happy_var_2,happy_var_4)]
	) `HappyStk` happyRest

happyReduce_28 = happyReduce 6 9 happyReduction_28
happyReduction_28 ((HappyAbsSyn13  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenInt happy_var_3)) `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 ([(map toLower happy_var_2,happy_var_3,happy_var_6)]
	) `HappyStk` happyRest

happyReduce_29 = happySpecReduce_0  9 happyReduction_29
happyReduction_29  =  HappyAbsSyn9
		 ([]
	)

happyReduce_30 = happySpecReduce_2  10 happyReduction_30
happyReduction_30 (HappyAbsSyn10  happy_var_2)
	(HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn10
		 ((map toLower happy_var_1):happy_var_2
	)
happyReduction_30 _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_0  10 happyReduction_31
happyReduction_31  =  HappyAbsSyn10
		 ([]
	)

happyReduce_32 = happySpecReduce_1  11 happyReduction_32
happyReduction_32 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn11
		 (if isUpper $ head happy_var_1 then Con (map toLower happy_var_1) else Var (map toLower happy_var_1)
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  11 happyReduction_33
happyReduction_33 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (happy_var_2
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happyReduce 4 12 happyReduction_34
happyReduction_34 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (App happy_var_2 $ happy_var_3
	) `HappyStk` happyRest

happyReduce_35 = happySpecReduce_2  12 happyReduction_35
happyReduction_35 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (App happy_var_1 $ happy_var_2
	)
happyReduction_35 _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  12 happyReduction_36
happyReduction_36 _
	 =  HappyAbsSyn12
		 (BAD
	)

happyReduce_37 = happyReduce 4 12 happyReduction_37
happyReduction_37 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (App BAD happy_var_3
	) `HappyStk` happyRest

happyReduce_38 = happySpecReduce_2  12 happyReduction_38
happyReduction_38 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (App BAD happy_var_2
	)
happyReduction_38 _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  12 happyReduction_39
happyReduction_39 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happyReduce 5 13 happyReduction_40
happyReduction_40 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Pred (map toLower happy_var_2) happy_var_4
	) `HappyStk` happyRest

happyReduce_41 = happyReduce 5 13 happyReduction_41
happyReduction_41 ((HappyAbsSyn13  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (AppC (map toLower happy_var_1) happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_42 = happyReduce 7 13 happyReduction_42
happyReduction_42 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (AppC (map toLower happy_var_2) happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 31 31 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenAny -> cont 14;
	TokenData -> cont 15;
	TokenEquals -> cont 16;
	TokenBad -> cont 17;
	TokenSatisfies -> cont 18;
	TokenColon -> cont 19;
	TokenCurlyO -> cont 20;
	TokenCurlyC -> cont 21;
	TokenArrow -> cont 22;
	TokenVar happy_dollar_dollar -> cont 23;
	TokenInt happy_dollar_dollar -> cont 24;
	TokenPipe -> cont 25;
	TokenSep -> cont 26;
	TokenCase -> cont 27;
	TokenOf -> cont 28;
	TokenParenO -> cont 29;
	TokenParenC -> cont 30;
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
	   | TokenBad
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
       ("bad",rest) -> TokenBad : lexer rest
       ("BAD",rest) -> TokenBad : lexer rest
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
test f = do
  cts <- readFile f
  return $ (haskell . lexer) cts