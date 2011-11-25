{-# OPTIONS_GHC -w #-}
module Parser where
import Data.Char
import Haskell
import Prelude hiding (lex)

-- parser produced by Happy Version 1.18.6

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30
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

action_0 (31) = happyShift action_5
action_0 (32) = happyShift action_6
action_0 (40) = happyShift action_7
action_0 (52) = happyShift action_8
action_0 (4) = happyGoto action_9
action_0 (8) = happyGoto action_2
action_0 (18) = happyGoto action_3
action_0 (20) = happyGoto action_4
action_0 _ = happyReduce_32

action_1 (31) = happyShift action_5
action_1 (32) = happyShift action_6
action_1 (40) = happyShift action_7
action_1 (52) = happyShift action_8
action_1 (8) = happyGoto action_2
action_1 (18) = happyGoto action_3
action_1 (20) = happyGoto action_4
action_1 _ = happyFail

action_2 (42) = happyShift action_19
action_2 _ = happyFail

action_3 (31) = happyShift action_5
action_3 (32) = happyShift action_6
action_3 (40) = happyShift action_7
action_3 (52) = happyShift action_8
action_3 (8) = happyGoto action_2
action_3 (18) = happyGoto action_3
action_3 (20) = happyGoto action_18
action_3 _ = happyReduce_32

action_4 _ = happyReduce_1

action_5 (39) = happyShift action_17
action_5 (7) = happyGoto action_15
action_5 (24) = happyGoto action_16
action_5 _ = happyFail

action_6 (39) = happyShift action_14
action_6 _ = happyFail

action_7 (40) = happyShift action_13
action_7 (5) = happyGoto action_11
action_7 (22) = happyGoto action_12
action_7 _ = happyReduce_36

action_8 (40) = happyShift action_10
action_8 _ = happyFail

action_9 (54) = happyAccept
action_9 _ = happyFail

action_10 (34) = happyShift action_26
action_10 _ = happyFail

action_11 (33) = happyShift action_25
action_11 _ = happyFail

action_12 _ = happyReduce_2

action_13 (40) = happyShift action_13
action_13 (22) = happyGoto action_24
action_13 _ = happyReduce_36

action_14 (40) = happyShift action_13
action_14 (5) = happyGoto action_23
action_14 (22) = happyGoto action_12
action_14 _ = happyReduce_36

action_15 _ = happyReduce_9

action_16 _ = happyReduce_5

action_17 (44) = happyShift action_22
action_17 (26) = happyGoto action_20
action_17 (28) = happyGoto action_21
action_17 _ = happyReduce_42

action_18 _ = happyReduce_31

action_19 _ = happyReduce_28

action_20 _ = happyReduce_39

action_21 (44) = happyShift action_22
action_21 (26) = happyGoto action_43
action_21 (28) = happyGoto action_21
action_21 _ = happyReduce_42

action_22 (39) = happyShift action_42
action_22 _ = happyFail

action_23 (33) = happyShift action_41
action_23 _ = happyFail

action_24 _ = happyReduce_35

action_25 (39) = happyShift action_37
action_25 (40) = happyShift action_38
action_25 (45) = happyShift action_39
action_25 (47) = happyShift action_40
action_25 (6) = happyGoto action_33
action_25 (11) = happyGoto action_34
action_25 (14) = happyGoto action_35
action_25 (15) = happyGoto action_36
action_25 _ = happyFail

action_26 (36) = happyShift action_29
action_26 (40) = happyShift action_30
action_26 (47) = happyShift action_31
action_26 (49) = happyShift action_32
action_26 (16) = happyGoto action_27
action_26 (17) = happyGoto action_28
action_26 _ = happyFail

action_27 (38) = happyShift action_56
action_27 (50) = happyShift action_57
action_27 (51) = happyShift action_58
action_27 _ = happyReduce_27

action_28 (53) = happyShift action_55
action_28 _ = happyFail

action_29 (40) = happyShift action_54
action_29 _ = happyFail

action_30 (35) = happyShift action_53
action_30 _ = happyFail

action_31 (36) = happyShift action_29
action_31 (40) = happyShift action_30
action_31 (47) = happyShift action_31
action_31 (49) = happyShift action_32
action_31 (16) = happyGoto action_27
action_31 (17) = happyGoto action_52
action_31 _ = happyFail

action_32 _ = happyReduce_21

action_33 _ = happyReduce_16

action_34 _ = happyReduce_6

action_35 _ = happyReduce_19

action_36 (39) = happyShift action_37
action_36 (40) = happyShift action_38
action_36 (47) = happyShift action_40
action_36 (6) = happyGoto action_33
action_36 (14) = happyGoto action_51
action_36 _ = happyReduce_12

action_37 _ = happyReduce_3

action_38 _ = happyReduce_4

action_39 (39) = happyShift action_37
action_39 (40) = happyShift action_38
action_39 (47) = happyShift action_40
action_39 (6) = happyGoto action_33
action_39 (14) = happyGoto action_35
action_39 (15) = happyGoto action_50
action_39 _ = happyFail

action_40 (39) = happyShift action_37
action_40 (40) = happyShift action_38
action_40 (47) = happyShift action_40
action_40 (6) = happyGoto action_33
action_40 (14) = happyGoto action_35
action_40 (15) = happyGoto action_49
action_40 _ = happyFail

action_41 (39) = happyShift action_48
action_41 (12) = happyGoto action_44
action_41 (13) = happyGoto action_45
action_41 (23) = happyGoto action_46
action_41 (27) = happyGoto action_47
action_41 _ = happyReduce_38

action_42 _ = happyReduce_44

action_43 _ = happyReduce_41

action_44 (41) = happyShift action_71
action_44 (29) = happyGoto action_69
action_44 (30) = happyGoto action_70
action_44 _ = happyReduce_46

action_45 _ = happyReduce_8

action_46 _ = happyReduce_15

action_47 _ = happyReduce_37

action_48 (39) = happyShift action_37
action_48 (40) = happyShift action_38
action_48 (47) = happyShift action_40
action_48 (6) = happyGoto action_33
action_48 (14) = happyGoto action_67
action_48 (19) = happyGoto action_68
action_48 _ = happyReduce_30

action_49 (39) = happyShift action_37
action_49 (40) = happyShift action_38
action_49 (47) = happyShift action_40
action_49 (48) = happyShift action_66
action_49 (6) = happyGoto action_33
action_49 (14) = happyGoto action_51
action_49 _ = happyFail

action_50 (39) = happyShift action_37
action_50 (40) = happyShift action_38
action_50 (46) = happyShift action_65
action_50 (47) = happyShift action_40
action_50 (6) = happyGoto action_33
action_50 (14) = happyGoto action_51
action_50 _ = happyFail

action_51 _ = happyReduce_18

action_52 (48) = happyShift action_64
action_52 _ = happyFail

action_53 (36) = happyShift action_29
action_53 (47) = happyShift action_31
action_53 (49) = happyShift action_32
action_53 (16) = happyGoto action_63
action_53 _ = happyFail

action_54 (35) = happyShift action_62
action_54 _ = happyFail

action_55 _ = happyReduce_7

action_56 (36) = happyShift action_29
action_56 (40) = happyShift action_30
action_56 (47) = happyShift action_31
action_56 (49) = happyShift action_32
action_56 (16) = happyGoto action_27
action_56 (17) = happyGoto action_61
action_56 _ = happyFail

action_57 (36) = happyShift action_29
action_57 (47) = happyShift action_31
action_57 (49) = happyShift action_32
action_57 (16) = happyGoto action_60
action_57 _ = happyFail

action_58 (36) = happyShift action_29
action_58 (47) = happyShift action_31
action_58 (49) = happyShift action_32
action_58 (16) = happyGoto action_59
action_58 _ = happyFail

action_59 _ = happyReduce_23

action_60 _ = happyReduce_24

action_61 _ = happyReduce_26

action_62 (39) = happyShift action_37
action_62 (40) = happyShift action_38
action_62 (47) = happyShift action_40
action_62 (6) = happyGoto action_33
action_62 (14) = happyGoto action_35
action_62 (15) = happyGoto action_77
action_62 _ = happyFail

action_63 (38) = happyShift action_76
action_63 _ = happyFail

action_64 _ = happyReduce_22

action_65 (36) = happyShift action_75
action_65 _ = happyFail

action_66 _ = happyReduce_17

action_67 (39) = happyShift action_37
action_67 (40) = happyShift action_38
action_67 (47) = happyShift action_40
action_67 (6) = happyGoto action_33
action_67 (14) = happyGoto action_67
action_67 (19) = happyGoto action_74
action_67 _ = happyReduce_30

action_68 _ = happyReduce_14

action_69 _ = happyReduce_43

action_70 (41) = happyShift action_71
action_70 (29) = happyGoto action_73
action_70 (30) = happyGoto action_70
action_70 _ = happyReduce_46

action_71 (39) = happyShift action_48
action_71 (12) = happyGoto action_72
action_71 _ = happyFail

action_72 _ = happyReduce_47

action_73 _ = happyReduce_45

action_74 _ = happyReduce_29

action_75 (43) = happyShift action_82
action_75 (21) = happyGoto action_80
action_75 (25) = happyGoto action_81
action_75 _ = happyReduce_34

action_76 (36) = happyShift action_29
action_76 (40) = happyShift action_30
action_76 (47) = happyShift action_31
action_76 (49) = happyShift action_32
action_76 (16) = happyGoto action_27
action_76 (17) = happyGoto action_79
action_76 _ = happyFail

action_77 (37) = happyShift action_78
action_77 (39) = happyShift action_37
action_77 (40) = happyShift action_38
action_77 (47) = happyShift action_40
action_77 (6) = happyGoto action_33
action_77 (14) = happyGoto action_51
action_77 _ = happyFail

action_78 _ = happyReduce_20

action_79 _ = happyReduce_25

action_80 (37) = happyShift action_87
action_80 _ = happyFail

action_81 (43) = happyShift action_82
action_81 (21) = happyGoto action_86
action_81 (25) = happyGoto action_81
action_81 _ = happyReduce_34

action_82 (39) = happyShift action_85
action_82 (9) = happyGoto action_83
action_82 (10) = happyGoto action_84
action_82 _ = happyFail

action_83 (38) = happyShift action_89
action_83 _ = happyFail

action_84 _ = happyReduce_40

action_85 (40) = happyShift action_13
action_85 (5) = happyGoto action_88
action_85 (22) = happyGoto action_12
action_85 _ = happyReduce_36

action_86 _ = happyReduce_33

action_87 _ = happyReduce_13

action_88 _ = happyReduce_10

action_89 (39) = happyShift action_37
action_89 (40) = happyShift action_38
action_89 (45) = happyShift action_39
action_89 (47) = happyShift action_40
action_89 (6) = happyGoto action_33
action_89 (11) = happyGoto action_90
action_89 (14) = happyGoto action_35
action_89 (15) = happyGoto action_36
action_89 _ = happyFail

action_90 _ = happyReduce_11

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyAbsSyn22  happy_var_1)
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
happyReduction_5 (HappyAbsSyn24  happy_var_1)
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

happyReduce_7 = happyReduce 5 8 happyReduction_7
happyReduction_7 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (ContSat $ Satisfies happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_8 = happyReduce 5 8 happyReduction_8
happyReduction_8 ((HappyAbsSyn13  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenCon happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (DataType $ Data happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_2  8 happyReduction_9
happyReduction_9 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (Import happy_var_2
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_2  9 happyReduction_10
happyReduction_10 (HappyAbsSyn5  happy_var_2)
	(HappyTerminal (TokenCon happy_var_1))
	 =  HappyAbsSyn9
		 ((happy_var_1, happy_var_2)
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  10 happyReduction_11
happyReduction_11 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn10
		 ((happy_var_1, happy_var_3)
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  11 happyReduction_12
happyReduction_12 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn11
		 (Base happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happyReduce 6 11 happyReduction_13
happyReduction_13 (_ `HappyStk`
	(HappyAbsSyn21  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (Case happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_14 = happySpecReduce_2  12 happyReduction_14
happyReduction_14 (HappyAbsSyn19  happy_var_2)
	(HappyTerminal (TokenCon happy_var_1))
	 =  HappyAbsSyn12
		 ((happy_var_1,length happy_var_2,Any)
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  13 happyReduction_15
happyReduction_15 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  14 happyReduction_16
happyReduction_16 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn14
		 (Named happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  14 happyReduction_17
happyReduction_17 _
	(HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (happy_var_2
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_2  15 happyReduction_18
happyReduction_18 (HappyAbsSyn14  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1 :@: happy_var_2
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  15 happyReduction_19
happyReduction_19 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happyReduce 5 16 happyReduction_20
happyReduction_20 (_ `HappyStk`
	(HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (Pred happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_1  16 happyReduction_21
happyReduction_21 _
	 =  HappyAbsSyn16
		 (CF
	)

happyReduce_22 = happySpecReduce_3  16 happyReduction_22
happyReduction_22 _
	(HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (happy_var_2
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  17 happyReduction_23
happyReduction_23 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn17
		 (And happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  17 happyReduction_24
happyReduction_24 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn17
		 (Or  happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happyReduce 5 17 happyReduction_25
happyReduction_25 ((HappyAbsSyn17  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (Arr (Just happy_var_1) happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_26 = happySpecReduce_3  17 happyReduction_26
happyReduction_26 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn17
		 (Arr Nothing happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  17 happyReduction_27
happyReduction_27 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_2  18 happyReduction_28
happyReduction_28 _
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_28 _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_2  19 happyReduction_29
happyReduction_29 (HappyAbsSyn19  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
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
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1:happy_var_2
	)
happyReduction_31 _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_0  20 happyReduction_32
happyReduction_32  =  HappyAbsSyn20
		 ([]
	)

happyReduce_33 = happySpecReduce_2  21 happyReduction_33
happyReduction_33 (HappyAbsSyn21  happy_var_2)
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1:happy_var_2
	)
happyReduction_33 _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_0  21 happyReduction_34
happyReduction_34  =  HappyAbsSyn21
		 ([]
	)

happyReduce_35 = happySpecReduce_2  22 happyReduction_35
happyReduction_35 (HappyAbsSyn22  happy_var_2)
	(HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn22
		 (happy_var_1:happy_var_2
	)
happyReduction_35 _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_0  22 happyReduction_36
happyReduction_36  =  HappyAbsSyn22
		 ([]
	)

happyReduce_37 = happySpecReduce_1  23 happyReduction_37
happyReduction_37 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_0  23 happyReduction_38
happyReduction_38  =  HappyAbsSyn23
		 ([]
	)

happyReduce_39 = happySpecReduce_2  24 happyReduction_39
happyReduction_39 (HappyAbsSyn26  happy_var_2)
	(HappyTerminal (TokenCon happy_var_1))
	 =  HappyAbsSyn24
		 (happy_var_1:happy_var_2
	)
happyReduction_39 _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_2  25 happyReduction_40
happyReduction_40 (HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn25
		 (happy_var_2
	)
happyReduction_40 _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_2  26 happyReduction_41
happyReduction_41 (HappyAbsSyn26  happy_var_2)
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1:happy_var_2
	)
happyReduction_41 _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_0  26 happyReduction_42
happyReduction_42  =  HappyAbsSyn26
		 ([]
	)

happyReduce_43 = happySpecReduce_2  27 happyReduction_43
happyReduction_43 (HappyAbsSyn29  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1:happy_var_2
	)
happyReduction_43 _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_2  28 happyReduction_44
happyReduction_44 (HappyTerminal (TokenCon happy_var_2))
	_
	 =  HappyAbsSyn28
		 (happy_var_2
	)
happyReduction_44 _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_2  29 happyReduction_45
happyReduction_45 (HappyAbsSyn29  happy_var_2)
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn29
		 (happy_var_1:happy_var_2
	)
happyReduction_45 _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_0  29 happyReduction_46
happyReduction_46  =  HappyAbsSyn29
		 ([]
	)

happyReduce_47 = happySpecReduce_2  30 happyReduction_47
happyReduction_47 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn30
		 (happy_var_2
	)
happyReduction_47 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 54 54 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenImport -> cont 31;
	TokenData -> cont 32;
	TokenEquals -> cont 33;
	TokenSatisfies -> cont 34;
	TokenColon -> cont 35;
	TokenCurlyO -> cont 36;
	TokenCurlyC -> cont 37;
	TokenArrow -> cont 38;
	TokenCon happy_dollar_dollar -> cont 39;
	TokenVar happy_dollar_dollar -> cont 40;
	TokenPipe -> cont 41;
	TokenDoubleSep -> cont 42;
	TokenSingleSep -> cont 43;
	TokenDot -> cont 44;
	TokenCase -> cont 45;
	TokenOf -> cont 46;
	TokenParenO -> cont 47;
	TokenParenC -> cont 48;
	TokenCF -> cont 49;
	TokenOr -> cont 50;
	TokenAnd -> cont 51;
	TokenContractPragmaO -> cont 52;
	TokenPragmaC -> cont 53;
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
