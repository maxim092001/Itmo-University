{-# OPTIONS_GHC -w #-}
module Parser where
import Lexer
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
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

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,193) ([0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,16384,0,0,0,16,32,0,0,4096,0,0,0,0,0,0,2044,0,0,0,15360,16384,0,0,0,0,0,0,0,0,0,0,14336,16384,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,512,1024,0,0,0,4,0,0,16158,0,0,0,0,0,0,0,0,0,0,0,16,32,0,0,8,16,0,0,4,8,0,0,2,4,0,0,0,0,0,0,16896,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,63488,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,64,0,0,16,32,0,0,8,16,0,0,4,8,0,0,2,4,0,0,1,2,0,0,1024,0,0,3840,4096,0,0,0,0,0,0,0,0,0,0,480,0,0,0,240,0,0,0,120,0,0,0,60,0,0,0,30,0,0,0,15,0,0,0,0,0,0,0,32768,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","Class","ClassDeclaration","Functions","FunctionList","FunctionDeclaration","Parameters","ParametersList","Parameter","Function","Statements","StatementsList","Statement","AssignStatement","ReturnStatement","IfStatement","BooleanExpression","Expression","Type","Modifiers","Modifier","'class'","'static'","'final'","'private'","'public'","'protected'","'void'","'double'","'float'","'int'","'long'","'byte'","'char'","'short'","'boolean'","'+'","'-'","'*'","'/'","'true'","'false'","cst","'>'","'<'","'<='","'>='","'=='","'!='","'if'","'else'","'('","')'","'{'","'}'","'return'","';'","','","'='","var","%eof"]
        bit_start = st Prelude.* 63
        bit_end = (st Prelude.+ 1) Prelude.* 63
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..62]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (4) = happyGoto action_4
action_0 (5) = happyGoto action_2
action_0 (22) = happyGoto action_3
action_0 _ = happyReduce_53

action_1 (5) = happyGoto action_2
action_1 (22) = happyGoto action_3
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (56) = happyShift action_13
action_2 (6) = happyGoto action_12
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (24) = happyShift action_6
action_3 (25) = happyShift action_7
action_3 (26) = happyShift action_8
action_3 (27) = happyShift action_9
action_3 (28) = happyShift action_10
action_3 (29) = happyShift action_11
action_3 (23) = happyGoto action_5
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (63) = happyAccept
action_4 _ = happyFail (happyExpListPerState 4)

action_5 _ = happyReduce_51

action_6 (62) = happyShift action_18
action_6 _ = happyFail (happyExpListPerState 6)

action_7 _ = happyReduce_58

action_8 _ = happyReduce_57

action_9 _ = happyReduce_55

action_10 _ = happyReduce_54

action_11 _ = happyReduce_56

action_12 _ = happyReduce_1

action_13 (25) = happyReduce_53
action_13 (26) = happyReduce_53
action_13 (27) = happyReduce_53
action_13 (28) = happyReduce_53
action_13 (29) = happyReduce_53
action_13 (30) = happyReduce_53
action_13 (31) = happyReduce_53
action_13 (32) = happyReduce_53
action_13 (33) = happyReduce_53
action_13 (34) = happyReduce_53
action_13 (35) = happyReduce_53
action_13 (36) = happyReduce_53
action_13 (37) = happyReduce_53
action_13 (38) = happyReduce_53
action_13 (7) = happyGoto action_14
action_13 (8) = happyGoto action_15
action_13 (12) = happyGoto action_16
action_13 (22) = happyGoto action_17
action_13 _ = happyReduce_7

action_14 (57) = happyShift action_32
action_14 (63) = happyReduce_3
action_14 (8) = happyGoto action_15
action_14 (12) = happyGoto action_31
action_14 (22) = happyGoto action_17
action_14 _ = happyReduce_53

action_15 (56) = happyShift action_30
action_15 (13) = happyGoto action_29
action_15 _ = happyFail (happyExpListPerState 15)

action_16 _ = happyReduce_6

action_17 (25) = happyShift action_7
action_17 (26) = happyShift action_8
action_17 (27) = happyShift action_9
action_17 (28) = happyShift action_10
action_17 (29) = happyShift action_11
action_17 (30) = happyShift action_20
action_17 (31) = happyShift action_21
action_17 (32) = happyShift action_22
action_17 (33) = happyShift action_23
action_17 (34) = happyShift action_24
action_17 (35) = happyShift action_25
action_17 (36) = happyShift action_26
action_17 (37) = happyShift action_27
action_17 (38) = happyShift action_28
action_17 (21) = happyGoto action_19
action_17 (23) = happyGoto action_5
action_17 _ = happyFail (happyExpListPerState 17)

action_18 _ = happyReduce_2

action_19 (62) = happyShift action_41
action_19 _ = happyFail (happyExpListPerState 19)

action_20 _ = happyReduce_50

action_21 _ = happyReduce_48

action_22 _ = happyReduce_49

action_23 _ = happyReduce_42

action_24 _ = happyReduce_45

action_25 _ = happyReduce_46

action_26 _ = happyReduce_44

action_27 _ = happyReduce_47

action_28 _ = happyReduce_43

action_29 _ = happyReduce_15

action_30 (30) = happyShift action_20
action_30 (31) = happyShift action_21
action_30 (32) = happyShift action_22
action_30 (33) = happyShift action_23
action_30 (34) = happyShift action_24
action_30 (35) = happyShift action_25
action_30 (36) = happyShift action_26
action_30 (37) = happyShift action_27
action_30 (38) = happyShift action_28
action_30 (52) = happyShift action_39
action_30 (58) = happyShift action_40
action_30 (14) = happyGoto action_33
action_30 (15) = happyGoto action_34
action_30 (16) = happyGoto action_35
action_30 (17) = happyGoto action_36
action_30 (18) = happyGoto action_37
action_30 (21) = happyGoto action_38
action_30 _ = happyReduce_20

action_31 _ = happyReduce_4

action_32 _ = happyReduce_5

action_33 (30) = happyShift action_20
action_33 (31) = happyShift action_21
action_33 (32) = happyShift action_22
action_33 (33) = happyShift action_23
action_33 (34) = happyShift action_24
action_33 (35) = happyShift action_25
action_33 (36) = happyShift action_26
action_33 (37) = happyShift action_27
action_33 (38) = happyShift action_28
action_33 (52) = happyShift action_39
action_33 (57) = happyShift action_50
action_33 (58) = happyShift action_40
action_33 (15) = happyGoto action_49
action_33 (16) = happyGoto action_35
action_33 (17) = happyGoto action_36
action_33 (18) = happyGoto action_37
action_33 (21) = happyGoto action_38
action_33 _ = happyReduce_16

action_34 _ = happyReduce_19

action_35 _ = happyReduce_21

action_36 _ = happyReduce_23

action_37 _ = happyReduce_22

action_38 (62) = happyShift action_48
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (54) = happyShift action_47
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (45) = happyShift action_45
action_40 (62) = happyShift action_46
action_40 (20) = happyGoto action_44
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (54) = happyShift action_43
action_41 (9) = happyGoto action_42
action_41 _ = happyFail (happyExpListPerState 41)

action_42 _ = happyReduce_8

action_43 (30) = happyShift action_20
action_43 (31) = happyShift action_21
action_43 (32) = happyShift action_22
action_43 (33) = happyShift action_23
action_43 (34) = happyShift action_24
action_43 (35) = happyShift action_25
action_43 (36) = happyShift action_26
action_43 (37) = happyShift action_27
action_43 (38) = happyShift action_28
action_43 (10) = happyGoto action_61
action_43 (11) = happyGoto action_62
action_43 (21) = happyGoto action_63
action_43 _ = happyReduce_13

action_44 (39) = happyShift action_56
action_44 (40) = happyShift action_57
action_44 (41) = happyShift action_58
action_44 (42) = happyShift action_59
action_44 (59) = happyShift action_60
action_44 _ = happyFail (happyExpListPerState 44)

action_45 _ = happyReduce_36

action_46 _ = happyReduce_37

action_47 (43) = happyShift action_54
action_47 (44) = happyShift action_55
action_47 (45) = happyShift action_45
action_47 (62) = happyShift action_46
action_47 (19) = happyGoto action_52
action_47 (20) = happyGoto action_53
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (61) = happyShift action_51
action_48 _ = happyFail (happyExpListPerState 48)

action_49 _ = happyReduce_17

action_50 _ = happyReduce_18

action_51 (45) = happyShift action_45
action_51 (62) = happyShift action_46
action_51 (20) = happyGoto action_78
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (55) = happyShift action_77
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (39) = happyShift action_56
action_53 (40) = happyShift action_57
action_53 (41) = happyShift action_58
action_53 (42) = happyShift action_59
action_53 (46) = happyShift action_71
action_53 (47) = happyShift action_72
action_53 (48) = happyShift action_73
action_53 (49) = happyShift action_74
action_53 (50) = happyShift action_75
action_53 (51) = happyShift action_76
action_53 _ = happyFail (happyExpListPerState 53)

action_54 _ = happyReduce_28

action_55 _ = happyReduce_29

action_56 (45) = happyShift action_45
action_56 (62) = happyShift action_46
action_56 (20) = happyGoto action_70
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (45) = happyShift action_45
action_57 (62) = happyShift action_46
action_57 (20) = happyGoto action_69
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (45) = happyShift action_45
action_58 (62) = happyShift action_46
action_58 (20) = happyGoto action_68
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (45) = happyShift action_45
action_59 (62) = happyShift action_46
action_59 (20) = happyGoto action_67
action_59 _ = happyFail (happyExpListPerState 59)

action_60 _ = happyReduce_25

action_61 (55) = happyShift action_65
action_61 (60) = happyShift action_66
action_61 _ = happyReduce_9

action_62 _ = happyReduce_12

action_63 (62) = happyShift action_64
action_63 _ = happyFail (happyExpListPerState 63)

action_64 _ = happyReduce_14

action_65 _ = happyReduce_11

action_66 (30) = happyShift action_20
action_66 (31) = happyShift action_21
action_66 (32) = happyShift action_22
action_66 (33) = happyShift action_23
action_66 (34) = happyShift action_24
action_66 (35) = happyShift action_25
action_66 (36) = happyShift action_26
action_66 (37) = happyShift action_27
action_66 (38) = happyShift action_28
action_66 (11) = happyGoto action_87
action_66 (21) = happyGoto action_63
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (39) = happyShift action_56
action_67 (40) = happyShift action_57
action_67 (41) = happyShift action_58
action_67 (42) = happyShift action_59
action_67 _ = happyReduce_41

action_68 (39) = happyShift action_56
action_68 (40) = happyShift action_57
action_68 (41) = happyShift action_58
action_68 (42) = happyShift action_59
action_68 _ = happyReduce_40

action_69 (39) = happyShift action_56
action_69 (40) = happyShift action_57
action_69 (41) = happyShift action_58
action_69 (42) = happyShift action_59
action_69 _ = happyReduce_39

action_70 (39) = happyShift action_56
action_70 (40) = happyShift action_57
action_70 (41) = happyShift action_58
action_70 (42) = happyShift action_59
action_70 _ = happyReduce_38

action_71 (45) = happyShift action_45
action_71 (62) = happyShift action_46
action_71 (20) = happyGoto action_86
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (45) = happyShift action_45
action_72 (62) = happyShift action_46
action_72 (20) = happyGoto action_85
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (45) = happyShift action_45
action_73 (62) = happyShift action_46
action_73 (20) = happyGoto action_84
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (45) = happyShift action_45
action_74 (62) = happyShift action_46
action_74 (20) = happyGoto action_83
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (45) = happyShift action_45
action_75 (62) = happyShift action_46
action_75 (20) = happyGoto action_82
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (45) = happyShift action_45
action_76 (62) = happyShift action_46
action_76 (20) = happyGoto action_81
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (56) = happyShift action_30
action_77 (13) = happyGoto action_80
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (39) = happyShift action_56
action_78 (40) = happyShift action_57
action_78 (41) = happyShift action_58
action_78 (42) = happyShift action_59
action_78 (59) = happyShift action_79
action_78 _ = happyFail (happyExpListPerState 78)

action_79 _ = happyReduce_24

action_80 (53) = happyShift action_88
action_80 _ = happyReduce_26

action_81 (39) = happyShift action_56
action_81 (40) = happyShift action_57
action_81 (41) = happyShift action_58
action_81 (42) = happyShift action_59
action_81 _ = happyReduce_35

action_82 (39) = happyShift action_56
action_82 (40) = happyShift action_57
action_82 (41) = happyShift action_58
action_82 (42) = happyShift action_59
action_82 _ = happyReduce_34

action_83 (39) = happyShift action_56
action_83 (40) = happyShift action_57
action_83 (41) = happyShift action_58
action_83 (42) = happyShift action_59
action_83 _ = happyReduce_31

action_84 (39) = happyShift action_56
action_84 (40) = happyShift action_57
action_84 (41) = happyShift action_58
action_84 (42) = happyShift action_59
action_84 _ = happyReduce_33

action_85 (39) = happyShift action_56
action_85 (40) = happyShift action_57
action_85 (41) = happyShift action_58
action_85 (42) = happyShift action_59
action_85 _ = happyReduce_32

action_86 (39) = happyShift action_56
action_86 (40) = happyShift action_57
action_86 (41) = happyShift action_58
action_86 (42) = happyShift action_59
action_86 _ = happyReduce_30

action_87 _ = happyReduce_10

action_88 (56) = happyShift action_30
action_88 (13) = happyGoto action_89
action_88 _ = happyFail (happyExpListPerState 88)

action_89 _ = happyReduce_27

happyReduce_1 = happySpecReduce_2  4 happyReduction_1
happyReduction_1 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (PClass happy_var_1 happy_var_2
	)
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3  5 happyReduction_2
happyReduction_2 (HappyTerminal (TVar happy_var_3))
	_
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn5
		 (PClassDeclaration happy_var_1 (Var happy_var_3)
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_2  6 happyReduction_3
happyReduction_3 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_2  7 happyReduction_4
happyReduction_4 (HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_2 : happy_var_1
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  7 happyReduction_5
happyReduction_5 _
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  7 happyReduction_6
happyReduction_6 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn7
		 ([happy_var_1]
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_0  7 happyReduction_7
happyReduction_7  =  HappyAbsSyn7
		 ([]
	)

happyReduce_8 = happyReduce 4 8 happyReduction_8
happyReduction_8 ((HappyAbsSyn9  happy_var_4) `HappyStk`
	(HappyTerminal (TVar happy_var_3)) `HappyStk`
	(HappyAbsSyn21  happy_var_2) `HappyStk`
	(HappyAbsSyn22  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (PFunctionDeclaration happy_var_1 happy_var_2 (Var happy_var_3) happy_var_4
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_2  9 happyReduction_9
happyReduction_9 (HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (happy_var_2
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  10 happyReduction_10
happyReduction_10 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_3 : happy_var_1
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  10 happyReduction_11
happyReduction_11 _
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  10 happyReduction_12
happyReduction_12 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 ([happy_var_1]
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_0  10 happyReduction_13
happyReduction_13  =  HappyAbsSyn10
		 ([]
	)

happyReduce_14 = happySpecReduce_2  11 happyReduction_14
happyReduction_14 (HappyTerminal (TVar happy_var_2))
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn11
		 (Parameter happy_var_1 (Var happy_var_2)
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_2  12 happyReduction_15
happyReduction_15 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn12
		 (PFunction happy_var_1 happy_var_2
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_2  13 happyReduction_16
happyReduction_16 (HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (happy_var_2
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_2  14 happyReduction_17
happyReduction_17 (HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_2 : happy_var_1
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_2  14 happyReduction_18
happyReduction_18 _
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  14 happyReduction_19
happyReduction_19 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 ([happy_var_1]
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_0  14 happyReduction_20
happyReduction_20  =  HappyAbsSyn14
		 ([]
	)

happyReduce_21 = happySpecReduce_1  15 happyReduction_21
happyReduction_21 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  15 happyReduction_22
happyReduction_22 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  15 happyReduction_23
happyReduction_23 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happyReduce 5 16 happyReduction_24
happyReduction_24 (_ `HappyStk`
	(HappyAbsSyn20  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TVar happy_var_2)) `HappyStk`
	(HappyAbsSyn21  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (PAssign happy_var_1 (Var happy_var_2) happy_var_4
	) `HappyStk` happyRest

happyReduce_25 = happySpecReduce_3  17 happyReduction_25
happyReduction_25 _
	(HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn17
		 (PReturn happy_var_2
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happyReduce 5 18 happyReduction_26
happyReduction_26 ((HappyAbsSyn13  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (IfExpr happy_var_3 happy_var_5 Nothing
	) `HappyStk` happyRest

happyReduce_27 = happyReduce 7 18 happyReduction_27
happyReduction_27 ((HappyAbsSyn13  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (IfExpr happy_var_3 happy_var_5 (Just happy_var_7)
	) `HappyStk` happyRest

happyReduce_28 = happySpecReduce_1  19 happyReduction_28
happyReduction_28 _
	 =  HappyAbsSyn19
		 (PTrue
	)

happyReduce_29 = happySpecReduce_1  19 happyReduction_29
happyReduction_29 _
	 =  HappyAbsSyn19
		 (PFalse
	)

happyReduce_30 = happySpecReduce_3  19 happyReduction_30
happyReduction_30 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 (PGT  happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  19 happyReduction_31
happyReduction_31 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 (PGQ  happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  19 happyReduction_32
happyReduction_32 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 (PLT  happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  19 happyReduction_33
happyReduction_33 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 (PLQ  happy_var_1 happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  19 happyReduction_34
happyReduction_34 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 (PEQ  happy_var_1 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  19 happyReduction_35
happyReduction_35 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 (PNEQ happy_var_1 happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  20 happyReduction_36
happyReduction_36 (HappyTerminal (TConst happy_var_1))
	 =  HappyAbsSyn20
		 (PConst happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  20 happyReduction_37
happyReduction_37 (HappyTerminal (TVar happy_var_1))
	 =  HappyAbsSyn20
		 (VarExpr (Var happy_var_1)
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  20 happyReduction_38
happyReduction_38 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (Add happy_var_1 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  20 happyReduction_39
happyReduction_39 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (Sub happy_var_1 happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  20 happyReduction_40
happyReduction_40 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (Mul happy_var_1 happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  20 happyReduction_41
happyReduction_41 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (Div happy_var_1 happy_var_3
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  21 happyReduction_42
happyReduction_42 _
	 =  HappyAbsSyn21
		 (PInt
	)

happyReduce_43 = happySpecReduce_1  21 happyReduction_43
happyReduction_43 _
	 =  HappyAbsSyn21
		 (PBoolean
	)

happyReduce_44 = happySpecReduce_1  21 happyReduction_44
happyReduction_44 _
	 =  HappyAbsSyn21
		 (PChar
	)

happyReduce_45 = happySpecReduce_1  21 happyReduction_45
happyReduction_45 _
	 =  HappyAbsSyn21
		 (PLong
	)

happyReduce_46 = happySpecReduce_1  21 happyReduction_46
happyReduction_46 _
	 =  HappyAbsSyn21
		 (PByte
	)

happyReduce_47 = happySpecReduce_1  21 happyReduction_47
happyReduction_47 _
	 =  HappyAbsSyn21
		 (PShort
	)

happyReduce_48 = happySpecReduce_1  21 happyReduction_48
happyReduction_48 _
	 =  HappyAbsSyn21
		 (PDouble
	)

happyReduce_49 = happySpecReduce_1  21 happyReduction_49
happyReduction_49 _
	 =  HappyAbsSyn21
		 (PFloat
	)

happyReduce_50 = happySpecReduce_1  21 happyReduction_50
happyReduction_50 _
	 =  HappyAbsSyn21
		 (PVoid
	)

happyReduce_51 = happySpecReduce_2  22 happyReduction_51
happyReduction_51 (HappyAbsSyn23  happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_2 : happy_var_1
	)
happyReduction_51 _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  22 happyReduction_52
happyReduction_52 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_0  22 happyReduction_53
happyReduction_53  =  HappyAbsSyn22
		 ([]
	)

happyReduce_54 = happySpecReduce_1  23 happyReduction_54
happyReduction_54 _
	 =  HappyAbsSyn23
		 (Public
	)

happyReduce_55 = happySpecReduce_1  23 happyReduction_55
happyReduction_55 _
	 =  HappyAbsSyn23
		 (Private
	)

happyReduce_56 = happySpecReduce_1  23 happyReduction_56
happyReduction_56 _
	 =  HappyAbsSyn23
		 (Protected
	)

happyReduce_57 = happySpecReduce_1  23 happyReduction_57
happyReduction_57 _
	 =  HappyAbsSyn23
		 (Final
	)

happyReduce_58 = happySpecReduce_1  23 happyReduction_58
happyReduction_58 _
	 =  HappyAbsSyn23
		 (Static
	)

happyNewToken action sts stk [] =
	action 63 63 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TClass -> cont 24;
	TStatic -> cont 25;
	TFinal -> cont 26;
	TPrivate -> cont 27;
	TPublic -> cont 28;
	TProtected -> cont 29;
	TVoid -> cont 30;
	TDouble -> cont 31;
	TFloat -> cont 32;
	TInt -> cont 33;
	TLong -> cont 34;
	TByte -> cont 35;
	TChar -> cont 36;
	TShort -> cont 37;
	TBoolean -> cont 38;
	TPlus -> cont 39;
	TSub -> cont 40;
	TMul -> cont 41;
	TDiv -> cont 42;
	TTrue -> cont 43;
	TFalse -> cont 44;
	TConst happy_dollar_dollar -> cont 45;
	TGT -> cont 46;
	TLT -> cont 47;
	TLQ -> cont 48;
	TGQ -> cont 49;
	TEQ -> cont 50;
	TNEQ -> cont 51;
	TIf -> cont 52;
	TElse -> cont 53;
	TRoundOpen -> cont 54;
	TRoundClose -> cont 55;
	TCurlyOpen -> cont 56;
	TCurlyClose -> cont 57;
	TReturn -> cont 58;
	TSemicolon -> cont 59;
	TComa -> cont 60;
	TAssign -> cont 61;
	TVar happy_dollar_dollar -> cont 62;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 63 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parserError tokens)
parse tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parserError :: [Token] -> a
parserError _ = error "Parse error"

data Modifier = Public
              | Private
              | Protected
              | Static
              | Final
               deriving (Eq)

data Type = PInt
          | PBoolean
          | PChar
          | PLong
          | PByte
          | PShort
          | PDouble
          | PFloat
          | PVoid
               deriving (Eq)

data Statement  = PAssign Type Var Expression
                | PReturn Expression
                | IfExpr BooleanExpr [Statement] (Maybe [Statement])               deriving (Eq)

data Var = Var String deriving (Eq)

data Expression = PConst Integer
                | VarExpr Var
                | Add Expression Expression
                | Div Expression Expression
                | Sub Expression Expression
                | Mul Expression Expression
               deriving (Eq)

data BooleanExpr =
    PTrue
  | PFalse
  | PEQ  Expression Expression
  | PNEQ Expression Expression
  | PGT  Expression Expression
  | PGQ  Expression Expression
  | PLT  Expression Expression
  | PLQ  Expression Expression
              deriving (Eq)


data Parameter = Parameter Type Var               deriving (Eq)


data PClassDeclaration = PClassDeclaration [Modifier] Var               deriving (Eq)


data PClass = PClass PClassDeclaration [PFunction]               deriving (Eq)


data PFunctionDeclaration = PFunctionDeclaration [Modifier] Type Var [Parameter]               deriving (Eq)


data PFunction = PFunction PFunctionDeclaration [Statement]               deriving (Eq)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









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
