{-
┌───────────────────────────────────────────────────────────────────╖
│ This file is part of MC².                                         ║
│                                                                   ║
│ MC² is free software: you can redistribute it and/or modify it    ║
│ under the terms of the GNU General Public License as published by ║
│ the Free Software Foundation, either version 3 of the License, or ║
│ (at your option) any later version.                               ║
│                                                                   ║
│ MC² is distributed in the hope that it will be useful, but        ║
│ WITHOUT ANY WARRANTY; without even the implied warranty of        ║
│ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU ║
│ General Public License for more details.                          ║
│                                                                   ║
│ You should have received a copy of the GNU General Public License ║
│ along with MC². If not, see <http://www.gnu.org/licenses/>.       ║
│                                                                   ║
│ Copyright 2018 Luca Padovani                                      ║
╘═══════════════════════════════════════════════════════════════════╝
-}
╘═══════════════════════════════════════════════════════════════════╝
-}


module Parser (parseProcess) where

import Lexer
import Language
import Exceptions

import Data.Either (partitionEithers)
import Control.Exception
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.9

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

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,186) ([0,0,2,0,0,0,2,0,0,0,256,0,0,0,2,0,0,0,2048,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,32768,6304,32,0,0,256,0,0,0,2048,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,24576,0,0,0,8,0,0,0,0,0,0,4096,0,0,0,4096,0,0,0,0,0,0,32768,6304,32,0,0,4,0,0,0,0,64,0,0,0,0,0,0,0,1,0,0,4096,0,0,0,0,68,0,0,32768,0,0,0,0,8192,0,0,0,0,0,0,4096,16,0,0,4120,0,0,0,4096,0,0,32768,6304,32,0,0,8192,0,0,0,4096,0,0,0,0,16,0,0,0,2,0,0,0,0,0,0,32768,0,0,32768,6304,32,0,0,0,4,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,8,0,0,1024,0,0,0,16384,0,0,0,0,16,0,0,0,2,0,0,0,0,0,0,4608,2056,0,0,2048,0,0,0,0,0,0,0,0,8,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,1,0,0,0,0,0,0,0,64,1536,0,0,0,0,0,0,4608,2056,0,0,4608,2056,0,0,4096,0,0,0,0,0,0,32768,6304,32,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,4,0,0,2048,0,0,0,4096,0,0,0,0,0,0,0,0,2,0,0,0,24576,0,0,0,0,0,32768,6304,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1552,0,0,0,32,0,0,4608,2056,0,0,4608,2056,0,32768,6304,32,0,0,0,16,0,0,0,0,0,0,0,2,0,0,2048,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,1536,0,0,4120,0,0,0,0,0,0,0,0,0,0,0,4608,2056,0,0,0,0,0,0,0,0,0,0,0,1536,0,0,0,0,0,0,0,64,0,0,0,4096,0,0,0,0,0,0,4120,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","Program","Process","Names","NameNeList","Guards","Guard","InterfaceList","Interface","MessageList","MessageNeList","Message","Types","TypeList","TypeNeList","ProcessDefList","ProcessDef","Parameters","ParameterList","ParameterNeList","Parameter","Name","InterfaceName","ProcessName","Tag","Type","Capability","Pattern","PatternOpt","DONE","IN","INTERFACE","MESSAGE","FAIL","FREE","CASE","OF","NEW","PROCESS","INT","STRING","CID","LID","'='","'.'","':'","';'","','","'|'","'('","')'","'{'","'}'","'['","']'","'+'","'\183'","'*'","'&'","'?'","'!'","%eof"]
        bit_start = st * 64
        bit_end = (st + 1) * 64
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..63]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (34) = happyShift action_4
action_0 (4) = happyGoto action_5
action_0 (10) = happyGoto action_2
action_0 (11) = happyGoto action_3
action_0 _ = happyReduce_20

action_1 (34) = happyShift action_4
action_1 (10) = happyGoto action_2
action_1 (11) = happyGoto action_3
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (41) = happyShift action_11
action_2 (18) = happyGoto action_9
action_2 (19) = happyGoto action_10
action_2 _ = happyReduce_34

action_3 (34) = happyShift action_4
action_3 (10) = happyGoto action_8
action_3 (11) = happyGoto action_3
action_3 _ = happyReduce_20

action_4 (44) = happyShift action_7
action_4 (25) = happyGoto action_6
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (64) = happyAccept
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (54) = happyShift action_23
action_6 _ = happyFail (happyExpListPerState 6)

action_7 _ = happyReduce_45

action_8 _ = happyReduce_21

action_9 (32) = happyShift action_18
action_9 (38) = happyShift action_19
action_9 (40) = happyShift action_20
action_9 (44) = happyShift action_13
action_9 (45) = happyShift action_21
action_9 (54) = happyShift action_22
action_9 (5) = happyGoto action_15
action_9 (24) = happyGoto action_16
action_9 (26) = happyGoto action_17
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (41) = happyShift action_11
action_10 (18) = happyGoto action_14
action_10 (19) = happyGoto action_10
action_10 _ = happyReduce_34

action_11 (44) = happyShift action_13
action_11 (26) = happyGoto action_12
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (52) = happyShift action_37
action_12 (20) = happyGoto action_36
action_12 _ = happyReduce_37

action_13 _ = happyReduce_46

action_14 _ = happyReduce_35

action_15 (51) = happyShift action_35
action_15 _ = happyReduce_1

action_16 (62) = happyShift action_33
action_16 (63) = happyShift action_34
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (52) = happyShift action_32
action_17 (6) = happyGoto action_31
action_17 _ = happyReduce_10

action_18 _ = happyReduce_2

action_19 (45) = happyShift action_21
action_19 (24) = happyGoto action_30
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (45) = happyShift action_21
action_20 (24) = happyGoto action_29
action_20 _ = happyFail (happyExpListPerState 20)

action_21 _ = happyReduce_44

action_22 (32) = happyShift action_18
action_22 (38) = happyShift action_19
action_22 (40) = happyShift action_20
action_22 (44) = happyShift action_13
action_22 (45) = happyShift action_21
action_22 (54) = happyShift action_22
action_22 (5) = happyGoto action_28
action_22 (24) = happyGoto action_16
action_22 (26) = happyGoto action_17
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (35) = happyShift action_27
action_23 (12) = happyGoto action_24
action_23 (13) = happyGoto action_25
action_23 (14) = happyGoto action_26
action_23 _ = happyReduce_23

action_24 (55) = happyShift action_58
action_24 _ = happyFail (happyExpListPerState 24)

action_25 _ = happyReduce_24

action_26 (49) = happyShift action_57
action_26 _ = happyReduce_25

action_27 (45) = happyShift action_45
action_27 (27) = happyGoto action_56
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (51) = happyShift action_35
action_28 (55) = happyShift action_55
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (48) = happyShift action_54
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (62) = happyShift action_53
action_30 _ = happyFail (happyExpListPerState 30)

action_31 _ = happyReduce_9

action_32 (45) = happyShift action_21
action_32 (53) = happyShift action_52
action_32 (7) = happyGoto action_50
action_32 (24) = happyGoto action_51
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (36) = happyShift action_48
action_33 (37) = happyShift action_49
action_33 (45) = happyShift action_45
action_33 (9) = happyGoto action_46
action_33 (27) = happyGoto action_47
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (45) = happyShift action_45
action_34 (27) = happyGoto action_44
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (32) = happyShift action_18
action_35 (38) = happyShift action_19
action_35 (40) = happyShift action_20
action_35 (44) = happyShift action_13
action_35 (45) = happyShift action_21
action_35 (54) = happyShift action_22
action_35 (5) = happyGoto action_43
action_35 (24) = happyGoto action_16
action_35 (26) = happyGoto action_17
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (46) = happyShift action_42
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (45) = happyShift action_21
action_37 (21) = happyGoto action_38
action_37 (22) = happyGoto action_39
action_37 (23) = happyGoto action_40
action_37 (24) = happyGoto action_41
action_37 _ = happyReduce_39

action_38 (53) = happyShift action_77
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (50) = happyShift action_76
action_39 _ = happyReduce_40

action_40 _ = happyReduce_41

action_41 (48) = happyShift action_75
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (32) = happyShift action_18
action_42 (38) = happyShift action_19
action_42 (40) = happyShift action_20
action_42 (44) = happyShift action_13
action_42 (45) = happyShift action_21
action_42 (54) = happyShift action_22
action_42 (5) = happyGoto action_74
action_42 (24) = happyGoto action_16
action_42 (26) = happyGoto action_17
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (51) = happyShift action_35
action_43 _ = happyReduce_7

action_44 (52) = happyShift action_32
action_44 (6) = happyGoto action_73
action_44 _ = happyReduce_10

action_45 _ = happyReduce_47

action_46 _ = happyReduce_5

action_47 (52) = happyShift action_32
action_47 (6) = happyGoto action_72
action_47 _ = happyReduce_10

action_48 (43) = happyShift action_71
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (47) = happyShift action_70
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (53) = happyShift action_69
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (50) = happyShift action_68
action_51 _ = happyReduce_13

action_52 _ = happyReduce_11

action_53 (42) = happyShift action_65
action_53 (45) = happyShift action_45
action_53 (52) = happyShift action_66
action_53 (60) = happyShift action_67
action_53 (27) = happyGoto action_63
action_53 (30) = happyGoto action_64
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (44) = happyShift action_7
action_54 (25) = happyGoto action_62
action_54 _ = happyFail (happyExpListPerState 54)

action_55 _ = happyReduce_3

action_56 (52) = happyShift action_61
action_56 (15) = happyGoto action_60
action_56 _ = happyReduce_28

action_57 (35) = happyShift action_27
action_57 (13) = happyGoto action_59
action_57 (14) = happyGoto action_26
action_57 _ = happyFail (happyExpListPerState 57)

action_58 _ = happyReduce_22

action_59 _ = happyReduce_26

action_60 _ = happyReduce_27

action_61 (44) = happyShift action_7
action_61 (16) = happyGoto action_90
action_61 (17) = happyGoto action_91
action_61 (25) = happyGoto action_79
action_61 (28) = happyGoto action_92
action_61 _ = happyReduce_30

action_62 (33) = happyShift action_89
action_62 _ = happyFail (happyExpListPerState 62)

action_63 _ = happyReduce_52

action_64 (39) = happyShift action_86
action_64 (58) = happyShift action_87
action_64 (59) = happyShift action_88
action_64 _ = happyFail (happyExpListPerState 64)

action_65 _ = happyReduce_51

action_66 (42) = happyShift action_65
action_66 (45) = happyShift action_45
action_66 (52) = happyShift action_66
action_66 (60) = happyShift action_67
action_66 (27) = happyGoto action_63
action_66 (30) = happyGoto action_85
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (42) = happyShift action_65
action_67 (45) = happyShift action_45
action_67 (52) = happyShift action_66
action_67 (60) = happyShift action_67
action_67 (27) = happyGoto action_63
action_67 (30) = happyGoto action_84
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (45) = happyShift action_21
action_68 (7) = happyGoto action_83
action_68 (24) = happyGoto action_51
action_68 _ = happyFail (happyExpListPerState 68)

action_69 _ = happyReduce_12

action_70 (32) = happyShift action_18
action_70 (38) = happyShift action_19
action_70 (40) = happyShift action_20
action_70 (44) = happyShift action_13
action_70 (45) = happyShift action_21
action_70 (54) = happyShift action_22
action_70 (5) = happyGoto action_82
action_70 (24) = happyGoto action_16
action_70 (26) = happyGoto action_17
action_70 _ = happyFail (happyExpListPerState 70)

action_71 _ = happyReduce_17

action_72 (47) = happyShift action_81
action_72 _ = happyFail (happyExpListPerState 72)

action_73 _ = happyReduce_4

action_74 (51) = happyShift action_35
action_74 _ = happyReduce_36

action_75 (44) = happyShift action_7
action_75 (25) = happyGoto action_79
action_75 (28) = happyGoto action_80
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (45) = happyShift action_21
action_76 (22) = happyGoto action_78
action_76 (23) = happyGoto action_40
action_76 (24) = happyGoto action_41
action_76 _ = happyFail (happyExpListPerState 76)

action_77 _ = happyReduce_38

action_78 (50) = happyShift action_76
action_78 _ = happyReduce_42

action_79 (62) = happyShift action_102
action_79 (63) = happyShift action_103
action_79 (29) = happyGoto action_101
action_79 _ = happyFail (happyExpListPerState 79)

action_80 _ = happyReduce_43

action_81 (32) = happyShift action_18
action_81 (38) = happyShift action_19
action_81 (40) = happyShift action_20
action_81 (44) = happyShift action_13
action_81 (45) = happyShift action_21
action_81 (54) = happyShift action_22
action_81 (5) = happyGoto action_100
action_81 (24) = happyGoto action_16
action_81 (26) = happyGoto action_17
action_81 _ = happyFail (happyExpListPerState 81)

action_82 _ = happyReduce_18

action_83 _ = happyReduce_14

action_84 _ = happyReduce_55

action_85 (53) = happyShift action_99
action_85 (58) = happyShift action_87
action_85 (59) = happyShift action_88
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (54) = happyShift action_98
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (42) = happyShift action_65
action_87 (45) = happyShift action_45
action_87 (52) = happyShift action_66
action_87 (60) = happyShift action_67
action_87 (27) = happyGoto action_63
action_87 (30) = happyGoto action_97
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (42) = happyShift action_65
action_88 (45) = happyShift action_45
action_88 (52) = happyShift action_66
action_88 (60) = happyShift action_67
action_88 (27) = happyGoto action_63
action_88 (30) = happyGoto action_96
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (32) = happyShift action_18
action_89 (38) = happyShift action_19
action_89 (40) = happyShift action_20
action_89 (44) = happyShift action_13
action_89 (45) = happyShift action_21
action_89 (54) = happyShift action_22
action_89 (5) = happyGoto action_95
action_89 (24) = happyGoto action_16
action_89 (26) = happyGoto action_17
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (53) = happyShift action_94
action_90 _ = happyFail (happyExpListPerState 90)

action_91 _ = happyReduce_31

action_92 (50) = happyShift action_93
action_92 _ = happyReduce_32

action_93 (44) = happyShift action_7
action_93 (17) = happyGoto action_108
action_93 (25) = happyGoto action_79
action_93 (28) = happyGoto action_92
action_93 _ = happyFail (happyExpListPerState 93)

action_94 _ = happyReduce_29

action_95 (51) = happyShift action_35
action_95 _ = happyReduce_8

action_96 _ = happyReduce_54

action_97 (58) = happyShift action_87
action_97 (59) = happyShift action_88
action_97 _ = happyReduce_53

action_98 (36) = happyShift action_48
action_98 (37) = happyShift action_49
action_98 (45) = happyShift action_45
action_98 (8) = happyGoto action_106
action_98 (9) = happyGoto action_107
action_98 (27) = happyGoto action_47
action_98 _ = happyFail (happyExpListPerState 98)

action_99 _ = happyReduce_56

action_100 _ = happyReduce_19

action_101 (42) = happyShift action_65
action_101 (45) = happyShift action_45
action_101 (52) = happyShift action_66
action_101 (60) = happyShift action_67
action_101 (27) = happyGoto action_63
action_101 (30) = happyGoto action_104
action_101 (31) = happyGoto action_105
action_101 _ = happyReduce_57

action_102 _ = happyReduce_49

action_103 _ = happyReduce_50

action_104 (58) = happyShift action_87
action_104 (59) = happyShift action_88
action_104 _ = happyReduce_58

action_105 _ = happyReduce_48

action_106 (55) = happyShift action_110
action_106 _ = happyFail (happyExpListPerState 106)

action_107 (61) = happyShift action_109
action_107 _ = happyReduce_15

action_108 _ = happyReduce_33

action_109 (36) = happyShift action_48
action_109 (37) = happyShift action_49
action_109 (45) = happyShift action_45
action_109 (8) = happyGoto action_111
action_109 (9) = happyGoto action_107
action_109 (27) = happyGoto action_47
action_109 _ = happyFail (happyExpListPerState 109)

action_110 _ = happyReduce_6

action_111 _ = happyReduce_16

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_3)
	(HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn4
		 ((happy_var_1, happy_var_2, happy_var_3)
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 _
	 =  HappyAbsSyn5
		 (S_Done
	)

happyReduce_3 = happySpecReduce_3  5 happyReduction_3
happyReduction_3 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happyReduce 4 5 happyReduction_4
happyReduction_4 ((HappyAbsSyn6  happy_var_4) `HappyStk`
	(HappyAbsSyn27  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (S_Output happy_var_1 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_3  5 happyReduction_5
happyReduction_5 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn5
		 (S_Input happy_var_1 Nothing happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happyReduce 8 5 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (S_Input happy_var_2 (Just happy_var_4) happy_var_7
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_3  5 happyReduction_7
happyReduction_7 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (S_Parallel happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happyReduce 6 5 happyReduction_8
happyReduction_8 ((HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (S_New happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_2  5 happyReduction_9
happyReduction_9 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn5
		 (S_Call happy_var_1 happy_var_2
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_0  6 happyReduction_10
happyReduction_10  =  HappyAbsSyn6
		 ([]
	)

happyReduce_11 = happySpecReduce_2  6 happyReduction_11
happyReduction_11 _
	_
	 =  HappyAbsSyn6
		 ([]
	)

happyReduce_12 = happySpecReduce_3  6 happyReduction_12
happyReduction_12 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  7 happyReduction_13
happyReduction_13 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn7
		 ([happy_var_1]
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  7 happyReduction_14
happyReduction_14 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 : happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  8 happyReduction_15
happyReduction_15 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  8 happyReduction_16
happyReduction_16 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (S_Choice happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_2  9 happyReduction_17
happyReduction_17 (HappyTerminal (Token _ (TokenString happy_var_2)))
	_
	 =  HappyAbsSyn9
		 (S_Fail happy_var_2
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  9 happyReduction_18
happyReduction_18 (HappyAbsSyn5  happy_var_3)
	_
	_
	 =  HappyAbsSyn9
		 (S_Free happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happyReduce 4 9 happyReduction_19
happyReduction_19 ((HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	(HappyAbsSyn27  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (S_Message happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_20 = happySpecReduce_0  10 happyReduction_20
happyReduction_20  =  HappyAbsSyn10
		 ([]
	)

happyReduce_21 = happySpecReduce_2  10 happyReduction_21
happyReduction_21 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1 : happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happyReduce 5 11 happyReduction_22
happyReduction_22 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_23 = happySpecReduce_0  12 happyReduction_23
happyReduction_23  =  HappyAbsSyn12
		 ([]
	)

happyReduce_24 = happySpecReduce_1  12 happyReduction_24
happyReduction_24 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  13 happyReduction_25
happyReduction_25 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 ([happy_var_1]
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  13 happyReduction_26
happyReduction_26 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 : happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  14 happyReduction_27
happyReduction_27 (HappyAbsSyn15  happy_var_3)
	(HappyAbsSyn27  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (Message happy_var_2 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_0  15 happyReduction_28
happyReduction_28  =  HappyAbsSyn15
		 ([]
	)

happyReduce_29 = happySpecReduce_3  15 happyReduction_29
happyReduction_29 _
	(HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (happy_var_2
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_0  16 happyReduction_30
happyReduction_30  =  HappyAbsSyn16
		 ([]
	)

happyReduce_31 = happySpecReduce_1  16 happyReduction_31
happyReduction_31 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  17 happyReduction_32
happyReduction_32 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn17
		 ([happy_var_1]
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  17 happyReduction_33
happyReduction_33 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1 : happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_0  18 happyReduction_34
happyReduction_34  =  HappyAbsSyn18
		 ([]
	)

happyReduce_35 = happySpecReduce_2  18 happyReduction_35
happyReduction_35 (HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1 : happy_var_2
	)
happyReduction_35 _ _  = notHappyAtAll 

happyReduce_36 = happyReduce 5 19 happyReduction_36
happyReduction_36 ((HappyAbsSyn5  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	(HappyAbsSyn26  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 ((happy_var_2, happy_var_3, happy_var_5)
	) `HappyStk` happyRest

happyReduce_37 = happySpecReduce_0  20 happyReduction_37
happyReduction_37  =  HappyAbsSyn20
		 ([]
	)

happyReduce_38 = happySpecReduce_3  20 happyReduction_38
happyReduction_38 _
	(HappyAbsSyn21  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (happy_var_2
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_0  21 happyReduction_39
happyReduction_39  =  HappyAbsSyn21
		 ([]
	)

happyReduce_40 = happySpecReduce_1  21 happyReduction_40
happyReduction_40 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  22 happyReduction_41
happyReduction_41 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn22
		 ([happy_var_1]
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  22 happyReduction_42
happyReduction_42 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1 ++ happy_var_3
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  23 happyReduction_43
happyReduction_43 (HappyAbsSyn28  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn23
		 ((happy_var_1, happy_var_3)
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  24 happyReduction_44
happyReduction_44 (HappyTerminal (happy_var_1@(Token _ (TokenLID _))))
	 =  HappyAbsSyn24
		 (getId happy_var_1
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  25 happyReduction_45
happyReduction_45 (HappyTerminal (happy_var_1@(Token _ (TokenCID _))))
	 =  HappyAbsSyn25
		 (getId happy_var_1
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  26 happyReduction_46
happyReduction_46 (HappyTerminal (happy_var_1@(Token _ (TokenCID _))))
	 =  HappyAbsSyn26
		 (getId happy_var_1
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  27 happyReduction_47
happyReduction_47 (HappyTerminal (happy_var_1@(Token _ (TokenLID _))))
	 =  HappyAbsSyn27
		 (Tag (Just $ getPos happy_var_1) (getId happy_var_1)
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3  28 happyReduction_48
happyReduction_48 (HappyAbsSyn31  happy_var_3)
	(HappyAbsSyn29  happy_var_2)
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn28
		 (S_Type happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  29 happyReduction_49
happyReduction_49 _
	 =  HappyAbsSyn29
		 (In
	)

happyReduce_50 = happySpecReduce_1  29 happyReduction_50
happyReduction_50 _
	 =  HappyAbsSyn29
		 (Out
	)

happyReduce_51 = happySpecReduce_1  30 happyReduction_51
happyReduction_51 (HappyTerminal (Token _ (TokenInt happy_var_1)))
	 =  HappyAbsSyn30
		 (if happy_var_1 == 0 then Zero
      else if happy_var_1 == 1 then One
      else throw (ErrorInvalidType happy_var_1)
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  30 happyReduction_52
happyReduction_52 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn30
		 (Atom happy_var_1
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_3  30 happyReduction_53
happyReduction_53 (HappyAbsSyn30  happy_var_3)
	_
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1 :+: happy_var_3
	)
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_3  30 happyReduction_54
happyReduction_54 (HappyAbsSyn30  happy_var_3)
	_
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1 :·: happy_var_3
	)
happyReduction_54 _ _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_2  30 happyReduction_55
happyReduction_55 (HappyAbsSyn30  happy_var_2)
	_
	 =  HappyAbsSyn30
		 (Star happy_var_2
	)
happyReduction_55 _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_3  30 happyReduction_56
happyReduction_56 _
	(HappyAbsSyn30  happy_var_2)
	_
	 =  HappyAbsSyn30
		 (happy_var_2
	)
happyReduction_56 _ _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_0  31 happyReduction_57
happyReduction_57  =  HappyAbsSyn31
		 (Nothing
	)

happyReduce_58 = happySpecReduce_1  31 happyReduction_58
happyReduction_58 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn31
		 (Just happy_var_1
	)
happyReduction_58 _  = notHappyAtAll 

happyNewToken action sts stk
	= lexwrap(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	Token _ TokenEOF -> action 64 64 tk (HappyState action) sts stk;
	Token _ TokenDone -> cont 32;
	Token _ TokenIn -> cont 33;
	Token _ TokenInterface -> cont 34;
	Token _ TokenMessage -> cont 35;
	Token _ TokenFail -> cont 36;
	Token _ TokenFree -> cont 37;
	Token _ TokenCase -> cont 38;
	Token _ TokenOf -> cont 39;
	Token _ TokenNew -> cont 40;
	Token _ TokenProcess -> cont 41;
	Token _ (TokenInt happy_dollar_dollar) -> cont 42;
	Token _ (TokenString happy_dollar_dollar) -> cont 43;
	happy_dollar_dollar@(Token _ (TokenCID _)) -> cont 44;
	happy_dollar_dollar@(Token _ (TokenLID _)) -> cont 45;
	Token _ TokenEQ -> cont 46;
	Token _ TokenDot -> cont 47;
	Token _ TokenColon -> cont 48;
	Token _ TokenSemiColon -> cont 49;
	Token _ TokenComma -> cont 50;
	Token _ TokenBar -> cont 51;
	Token _ TokenLParen -> cont 52;
	Token _ TokenRParen -> cont 53;
	Token _ TokenLBrace -> cont 54;
	Token _ TokenRBrace -> cont 55;
	Token _ TokenLBrack -> cont 56;
	Token _ TokenRBrack -> cont 57;
	Token _ TokenPlus -> cont 58;
	Token _ TokenCDot -> cont 59;
	Token _ TokenStar -> cont 60;
	Token _ TokenAmp -> cont 61;
	Token _ TokenQMark -> cont 62;
	Token _ TokenEMark -> cont 63;
	_ -> happyError' (tk, [])
	})

happyError_ explist 64 tk = happyError' (tk, explist)
happyError_ explist _ tk = happyError' (tk, explist)

happyThen :: () => Alex a -> (a -> Alex b) -> Alex b
happyThen = (>>=)
happyReturn :: () => a -> Alex a
happyReturn = (return)
happyThen1 :: () => Alex a -> (a -> Alex b) -> Alex b
happyThen1 = happyThen
happyReturn1 :: () => a -> Alex a
happyReturn1 = happyReturn
happyError' :: () => ((Token), [String]) -> Alex a
happyError' tk = (\(tokens, _) -> happyError tokens) tk
parse = happySomeParser where
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


getId :: Token -> String
getId (Token _ (TokenLID x)) = x
getId (Token _ (TokenCID x)) = x

getPos :: Token -> (Int, Int)
getPos (Token (AlexPn _ line col) _) = (line, col)

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token p t) = alexError' p ("parse error at token '" ++ show t ++ "'")

parseProcess :: FilePath -> String -> Either String (S_Interfaces, S_ProcessDefinitions, S_Process)
parseProcess = runAlex' parse
{-# LINE 1 "templates/GenericTemplate.hs" #-}





































































































































































































-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 











data Happy_IntList = HappyCons Int Happy_IntList




















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




indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






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
     = case happyDrop (k - ((1) :: Int)) sts of
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





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
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

