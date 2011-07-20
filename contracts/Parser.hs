{-# OPTIONS_GHC -w #-}
module Parser where
import Data.Char
import Haskell

-- parser produced by Happy Version 1.18.6

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14
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

action_0 (16) = happyShift action_3
action_0 (24) = happyShift action_4
action_0 (4) = happyGoto action_5
action_0 (6) = happyGoto action_6
action_0 _ = happyReduce_3

action_1 (16) = happyShift action_3
action_1 (24) = happyShift action_4
action_1 (6) = happyGoto action_2
action_1 _ = happyFail

action_2 (27) = happyShift action_7
action_2 _ = happyFail

action_3 (24) = happyShift action_11
action_3 _ = happyFail

action_4 (19) = happyShift action_9
action_4 (24) = happyShift action_10
action_4 (10) = happyGoto action_8
action_4 _ = happyReduce_31

action_5 (36) = happyAccept
action_5 _ = happyFail

action_6 (27) = happyShift action_7
action_6 _ = happyReduce_2

action_7 (16) = happyShift action_3
action_7 (24) = happyShift action_4
action_7 (4) = happyGoto action_20
action_7 (6) = happyGoto action_6
action_7 _ = happyReduce_3

action_8 (17) = happyShift action_19
action_8 _ = happyFail

action_9 (21) = happyShift action_15
action_9 (24) = happyShift action_16
action_9 (30) = happyShift action_17
action_9 (33) = happyShift action_18
action_9 (14) = happyGoto action_14
action_9 _ = happyFail

action_10 (24) = happyShift action_10
action_10 (10) = happyGoto action_13
action_10 _ = happyReduce_31

action_11 (24) = happyShift action_10
action_11 (10) = happyGoto action_12
action_11 _ = happyReduce_31

action_12 (17) = happyShift action_33
action_12 _ = happyFail

action_13 _ = happyReduce_30

action_14 (34) = happyShift action_31
action_14 (35) = happyShift action_32
action_14 _ = happyReduce_9

action_15 (24) = happyShift action_30
action_15 _ = happyFail

action_16 (20) = happyShift action_29
action_16 _ = happyFail

action_17 (21) = happyShift action_15
action_17 (24) = happyShift action_28
action_17 (30) = happyShift action_17
action_17 (33) = happyShift action_18
action_17 (14) = happyGoto action_27
action_17 _ = happyFail

action_18 _ = happyReduce_49

action_19 (18) = happyShift action_23
action_19 (24) = happyShift action_24
action_19 (28) = happyShift action_25
action_19 (30) = happyShift action_26
action_19 (11) = happyGoto action_21
action_19 (12) = happyGoto action_22
action_19 _ = happyFail

action_20 _ = happyReduce_1

action_21 _ = happyReduce_39

action_22 (24) = happyShift action_24
action_22 (30) = happyShift action_48
action_22 (11) = happyGoto action_47
action_22 _ = happyReduce_7

action_23 (18) = happyShift action_23
action_23 (24) = happyShift action_24
action_23 (30) = happyShift action_26
action_23 (11) = happyGoto action_21
action_23 (12) = happyGoto action_46
action_23 _ = happyReduce_36

action_24 _ = happyReduce_32

action_25 (18) = happyShift action_23
action_25 (24) = happyShift action_24
action_25 (30) = happyShift action_26
action_25 (11) = happyGoto action_21
action_25 (12) = happyGoto action_45
action_25 _ = happyFail

action_26 (18) = happyShift action_44
action_26 (24) = happyShift action_24
action_26 (30) = happyShift action_26
action_26 (11) = happyGoto action_21
action_26 (12) = happyGoto action_43
action_26 _ = happyFail

action_27 (31) = happyShift action_42
action_27 (34) = happyShift action_31
action_27 (35) = happyShift action_32
action_27 _ = happyFail

action_28 (20) = happyShift action_41
action_28 _ = happyFail

action_29 (21) = happyShift action_15
action_29 (24) = happyShift action_16
action_29 (30) = happyShift action_17
action_29 (33) = happyShift action_18
action_29 (14) = happyGoto action_40
action_29 _ = happyFail

action_30 (20) = happyShift action_39
action_30 _ = happyFail

action_31 (21) = happyShift action_15
action_31 (24) = happyShift action_16
action_31 (30) = happyShift action_17
action_31 (33) = happyShift action_18
action_31 (14) = happyGoto action_38
action_31 _ = happyFail

action_32 (21) = happyShift action_15
action_32 (24) = happyShift action_16
action_32 (30) = happyShift action_17
action_32 (33) = happyShift action_18
action_32 (14) = happyGoto action_37
action_32 _ = happyFail

action_33 (24) = happyShift action_35
action_33 (30) = happyShift action_36
action_33 (9) = happyGoto action_34
action_33 _ = happyReduce_29

action_34 _ = happyReduce_10

action_35 (25) = happyShift action_58
action_35 _ = happyFail

action_36 (24) = happyShift action_57
action_36 _ = happyFail

action_37 (34) = happyShift action_31
action_37 (35) = happyShift action_32
action_37 _ = happyReduce_45

action_38 (34) = happyShift action_31
action_38 (35) = happyShift action_32
action_38 _ = happyReduce_46

action_39 (18) = happyShift action_23
action_39 (24) = happyShift action_24
action_39 (30) = happyShift action_26
action_39 (11) = happyGoto action_21
action_39 (12) = happyGoto action_56
action_39 _ = happyFail

action_40 (23) = happyShift action_55
action_40 (34) = happyShift action_31
action_40 (35) = happyShift action_32
action_40 _ = happyFail

action_41 (21) = happyShift action_15
action_41 (24) = happyShift action_16
action_41 (30) = happyShift action_17
action_41 (33) = happyShift action_18
action_41 (14) = happyGoto action_54
action_41 _ = happyFail

action_42 _ = happyReduce_44

action_43 (24) = happyShift action_24
action_43 (30) = happyShift action_48
action_43 (31) = happyShift action_53
action_43 (11) = happyGoto action_52
action_43 _ = happyFail

action_44 (18) = happyShift action_23
action_44 (24) = happyShift action_24
action_44 (30) = happyShift action_26
action_44 (11) = happyGoto action_21
action_44 (12) = happyGoto action_51
action_44 _ = happyReduce_36

action_45 (24) = happyShift action_24
action_45 (29) = happyShift action_50
action_45 (30) = happyShift action_48
action_45 (11) = happyGoto action_47
action_45 _ = happyFail

action_46 (24) = happyShift action_24
action_46 (30) = happyShift action_48
action_46 (11) = happyGoto action_47
action_46 _ = happyReduce_38

action_47 _ = happyReduce_35

action_48 (18) = happyShift action_23
action_48 (24) = happyShift action_24
action_48 (30) = happyShift action_26
action_48 (11) = happyGoto action_21
action_48 (12) = happyGoto action_49
action_48 _ = happyFail

action_49 (24) = happyShift action_24
action_49 (30) = happyShift action_48
action_49 (31) = happyShift action_53
action_49 (11) = happyGoto action_47
action_49 _ = happyFail

action_50 (26) = happyShift action_68
action_50 (7) = happyGoto action_67
action_50 _ = happyReduce_15

action_51 (24) = happyShift action_24
action_51 (30) = happyShift action_48
action_51 (31) = happyShift action_66
action_51 (11) = happyGoto action_47
action_51 _ = happyFail

action_52 (31) = happyShift action_65
action_52 _ = happyReduce_35

action_53 _ = happyReduce_33

action_54 (23) = happyShift action_64
action_54 (34) = happyShift action_31
action_54 (35) = happyShift action_32
action_54 _ = happyFail

action_55 (21) = happyShift action_15
action_55 (24) = happyShift action_16
action_55 (30) = happyShift action_17
action_55 (33) = happyShift action_18
action_55 (14) = happyGoto action_63
action_55 _ = happyFail

action_56 (22) = happyShift action_62
action_56 (24) = happyShift action_24
action_56 (30) = happyShift action_48
action_56 (11) = happyGoto action_47
action_56 _ = happyFail

action_57 (25) = happyShift action_61
action_57 _ = happyFail

action_58 (19) = happyShift action_59
action_58 (26) = happyShift action_60
action_58 _ = happyReduce_23

action_59 (21) = happyShift action_15
action_59 (24) = happyShift action_16
action_59 (30) = happyShift action_17
action_59 (33) = happyShift action_18
action_59 (14) = happyGoto action_75
action_59 _ = happyFail

action_60 (24) = happyShift action_35
action_60 (30) = happyShift action_36
action_60 (9) = happyGoto action_74
action_60 _ = happyReduce_29

action_61 (31) = happyShift action_73
action_61 _ = happyFail

action_62 _ = happyReduce_43

action_63 (34) = happyShift action_31
action_63 (35) = happyShift action_32
action_63 _ = happyReduce_47

action_64 (21) = happyShift action_15
action_64 (24) = happyShift action_16
action_64 (30) = happyShift action_17
action_64 (33) = happyShift action_18
action_64 (14) = happyGoto action_72
action_64 _ = happyFail

action_65 _ = happyReduce_34

action_66 _ = happyReduce_37

action_67 _ = happyReduce_8

action_68 (24) = happyShift action_70
action_68 (30) = happyShift action_71
action_68 (8) = happyGoto action_69
action_68 _ = happyReduce_20

action_69 (23) = happyShift action_83
action_69 _ = happyFail

action_70 (23) = happyReduce_20
action_70 (24) = happyShift action_70
action_70 (30) = happyShift action_82
action_70 (31) = happyReduce_20
action_70 (8) = happyGoto action_81
action_70 _ = happyReduce_20

action_71 (24) = happyShift action_80
action_71 _ = happyFail

action_72 (31) = happyShift action_79
action_72 (34) = happyShift action_31
action_72 (35) = happyShift action_32
action_72 _ = happyFail

action_73 (19) = happyShift action_77
action_73 (26) = happyShift action_78
action_73 _ = happyReduce_24

action_74 _ = happyReduce_21

action_75 (26) = happyShift action_76
action_75 (34) = happyShift action_31
action_75 (35) = happyShift action_32
action_75 _ = happyReduce_27

action_76 (24) = happyShift action_35
action_76 (30) = happyShift action_36
action_76 (9) = happyGoto action_91
action_76 _ = happyReduce_29

action_77 (21) = happyShift action_15
action_77 (24) = happyShift action_16
action_77 (30) = happyShift action_17
action_77 (33) = happyShift action_18
action_77 (14) = happyGoto action_90
action_77 _ = happyFail

action_78 (24) = happyShift action_35
action_78 (30) = happyShift action_36
action_78 (9) = happyGoto action_89
action_78 _ = happyReduce_29

action_79 _ = happyReduce_48

action_80 (31) = happyShift action_88
action_80 _ = happyFail

action_81 _ = happyReduce_16

action_82 (24) = happyShift action_87
action_82 (30) = happyShift action_71
action_82 (8) = happyGoto action_86
action_82 _ = happyReduce_20

action_83 (18) = happyShift action_23
action_83 (24) = happyShift action_24
action_83 (30) = happyShift action_85
action_83 (11) = happyGoto action_21
action_83 (12) = happyGoto action_84
action_83 _ = happyFail

action_84 (24) = happyShift action_24
action_84 (26) = happyShift action_68
action_84 (27) = happyReduce_15
action_84 (30) = happyShift action_48
action_84 (36) = happyReduce_15
action_84 (7) = happyGoto action_95
action_84 (11) = happyGoto action_47
action_84 _ = happyReduce_15

action_85 (18) = happyShift action_44
action_85 (24) = happyShift action_24
action_85 (30) = happyShift action_26
action_85 (11) = happyGoto action_21
action_85 (12) = happyGoto action_94
action_85 _ = happyFail

action_86 (31) = happyShift action_93
action_86 _ = happyFail

action_87 (24) = happyShift action_70
action_87 (30) = happyShift action_82
action_87 (31) = happyShift action_88
action_87 (8) = happyGoto action_81
action_87 _ = happyFail

action_88 _ = happyReduce_19

action_89 _ = happyReduce_22

action_90 (26) = happyShift action_92
action_90 (34) = happyShift action_31
action_90 (35) = happyShift action_32
action_90 _ = happyReduce_28

action_91 _ = happyReduce_25

action_92 (24) = happyShift action_35
action_92 (30) = happyShift action_36
action_92 (9) = happyGoto action_97
action_92 _ = happyReduce_29

action_93 _ = happyReduce_17

action_94 (24) = happyShift action_24
action_94 (30) = happyShift action_48
action_94 (31) = happyShift action_96
action_94 (11) = happyGoto action_52
action_94 _ = happyFail

action_95 _ = happyReduce_11

action_96 (26) = happyShift action_68
action_96 (27) = happyReduce_33
action_96 (36) = happyReduce_33
action_96 (7) = happyGoto action_98
action_96 _ = happyReduce_33

action_97 _ = happyReduce_26

action_98 _ = happyReduce_12

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
happyReduction_9 (HappyAbsSyn14  happy_var_3)
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
	(HappyAbsSyn14  happy_var_4) `HappyStk`
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
	(HappyAbsSyn14  happy_var_6) `HappyStk`
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
happyReduction_27 ((HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenInt happy_var_2)) `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 ([(map toLower happy_var_1,happy_var_2,happy_var_4)]
	) `HappyStk` happyRest

happyReduce_28 = happyReduce 6 9 happyReduction_28
happyReduction_28 ((HappyAbsSyn14  happy_var_6) `HappyStk`
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
		 (Var $ map toLower happy_var_1
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

happyReduce_40 = happySpecReduce_3  13 happyReduction_40
happyReduction_40 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1:happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  13 happyReduction_41
happyReduction_41 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn13
		 ([happy_var_1]
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_0  13 happyReduction_42
happyReduction_42  =  HappyAbsSyn13
		 ([]
	)

happyReduce_43 = happyReduce 5 14 happyReduction_43
happyReduction_43 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (Pred (map toLower happy_var_2) happy_var_4
	) `HappyStk` happyRest

happyReduce_44 = happySpecReduce_3  14 happyReduction_44
happyReduction_44 _
	(HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (happy_var_2
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  14 happyReduction_45
happyReduction_45 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (And happy_var_1 happy_var_3
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  14 happyReduction_46
happyReduction_46 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (Or  happy_var_1 happy_var_3
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happyReduce 5 14 happyReduction_47
happyReduction_47 ((HappyAbsSyn14  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (AppC (map toLower happy_var_1) happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_48 = happyReduce 7 14 happyReduction_48
happyReduction_48 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (AppC (map toLower happy_var_2) happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_49 = happySpecReduce_1  14 happyReduction_49
happyReduction_49 _
	 =  HappyAbsSyn14
		 (CF
	)

happyNewToken action sts stk [] =
	action 36 36 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenAny -> cont 15;
	TokenData -> cont 16;
	TokenEquals -> cont 17;
	TokenBad -> cont 18;
	TokenSatisfies -> cont 19;
	TokenColon -> cont 20;
	TokenCurlyO -> cont 21;
	TokenCurlyC -> cont 22;
	TokenArrow -> cont 23;
	TokenVar happy_dollar_dollar -> cont 24;
	TokenInt happy_dollar_dollar -> cont 25;
	TokenPipe -> cont 26;
	TokenSep -> cont 27;
	TokenCase -> cont 28;
	TokenOf -> cont 29;
	TokenParenO -> cont 30;
	TokenParenC -> cont 31;
	TokenComma -> cont 32;
	TokenCF -> cont 33;
	TokenOr -> cont 34;
	TokenAnd -> cont 35;
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
	   | TokenVar String
	   | TokenArrow
	   | TokenColon
	   | TokenParenO
	   | TokenParenC
	   | TokenCurlyO
	   | TokenCurlyC
	   | TokenBad
	   | TokenComma
	   | TokenOr
	   | TokenAnd
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
lexInt cs = TokenInt (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexVar cs = case span (\x -> isAlpha x || x == '_' || isDigit x) cs of
       ("any",rest) -> TokenAny : lexer rest
       ("bad",rest) -> TokenBad : lexer rest
       ("BAD",rest) -> TokenBad : lexer rest
       ("data",rest) -> TokenData : lexer rest
       ("case",rest) -> TokenCase : lexer rest
       ("of",rest) -> TokenOf : lexer rest       		     		    	  
       ("cf",rest) -> TokenCF : lexer rest
       ("CF",rest) -> TokenCF : lexer rest
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
