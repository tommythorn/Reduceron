{-# OPTIONS_GHC -XFunctionalDependencies -XFlexibleInstances -XMultiParamTypeClasses -XUndecidableInstances -fcontext-stack=1024 #-}

{- |

Modest library for statically-typed sized vectors, inspired by Oleg
Kiselyov's \"Number-Parameterized Types\", The Monad.Reader, Issue 5.
Type synonyms 'N0' to 'N255' exported along with zero ('Z') and
successor ('S').  Values 'n0' to 'n255' with types 'N0' to 'N255' are
also exported.

-}

module Lava.Vector
  ( vempty     -- :: Vec Z a
  , (+>)       -- :: a -> Vec n a -> Vec (S n) a
  , (<+)       -- :: Vec n a -> a -> Vec (S n) a
  , velems     -- :: Vec n a -> [a]
  , vsize      -- :: Vec n a -> n
  , sized      -- :: N n => (Int -> Vec n a) -> Vec n a
  , vec        -- :: N n => [a] -> Vec n a
  , vecOf      -- :: N n => a -> Vec n a
  , ofSize     -- :: Vec n a -> n -> Vec n a
  , sameSize   -- :: Vec n a -> Vec n a -> Vec n a
  , vecn       -- :: N n => n -> [a] -> Vec n a
  , vextend    -- :: N m => a -> Vec n a -> Vec m a
  , vsingle    -- :: a -> Vec N1 a
  , vhead      -- :: Vec (S n) a -> a
  , vtail      -- :: Vec (S n) a -> Vec n a
  , vlast      -- :: Vec (S n) a -> a
  , vinit      -- :: Vec (S n) a -> Vec n a
  , vlength    -- :: Vec n a -> Int
  , vmap       -- :: (a -> b) -> Vec n a -> Vec n b
  , vzip       -- :: Vec n a -> Vec n b -> Vec n (a, b)
  , vzipWith   -- :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
  , vreverse   -- :: Vec n a -> Vec n a
  , vfoldl     -- :: (a -> b -> a) -> a -> Vec n b -> a
  , vfoldr     -- :: (a -> b -> b) -> b -> Vec n a -> b
  , vmapAccumL -- :: (a -> b -> (a, c)) -> a -> Vec n b -> (a, Vec n c)
  , vmapAccumR -- :: (a -> b -> (a, c)) -> a -> Vec n b -> (a, Vec n c)
  , (<++>)     -- :: Add n m o => Vec n a -> Vec m a -> Vec o a
  , vconcat    -- :: Mul n m o => Vec n (Vec m a) -> Vec o a
  , vat        -- :: (N n, Less n m) => Vec m a -> n -> a
  , vtake      -- :: N n => n -> Vec m a -> Vec n a
  , vdrop      -- :: (N n, Add n o m) => n -> Vec m a -> Vec o a
  , vsplitAt   -- :: (N m, Add m n o) => m -> Vec o a -> (Vec m a, Vec n a)
  , vgroup     -- :: (N n, Mul n o m) => n -> Vec m a -> Vec o (Vec n a)
  , vtranspose -- :: Vec n (Vec m a) -> Vec m (Vec n a)
  , vshr       -- :: a -> Vec n a -> Vec n a
  , vshl       -- :: Vec n a -> a -> Vec n a
  , vreplicate -- :: N n => n -> a -> Vec n a
  , vrepeat    -- :: N n => a -> Vec n a
  , vsequence  -- :: Monad m => Vec n (m a) -> m (Vec n a)
  , vrigid     -- :: N n => Vec n a -> Vec n a
  , N0,  N1,  N2,  N3
  , N4,  N5,  N6,  N7
  , N8,  N9,  N10, N11
  , N12, N13, N14, N15
  , N16, N17, N18, N19
  , N20, N21, N22, N23
  , N24, N25, N26, N27
  , N28, N29, N30, N31
  , N32, N33, N34, N35
  , N36, N37, N38, N39
  , N40, N41, N42, N43
  , N44, N45, N46, N47
  , N48, N49, N50, N51
  , N52, N53, N54, N55
  , N56, N57, N58, N59
  , N60, N61, N62, N63
  , N64, N65, N66, N67
  , N68, N69, N70, N71
  , N72, N73, N74, N75
  , N76, N77, N78, N79
  , N80, N81, N82, N83
  , N84, N85, N86, N87
  , N88, N89, N90, N91
  , N92, N93, N94, N95
  , N96, N97, N98, N99
  , N100, N101, N102, N103
  , N104, N105, N106, N107
  , N108, N109, N110, N111
  , N112, N113, N114, N115
  , N116, N117, N118, N119
  , N120, N121, N122, N123
  , N124, N125, N126, N127
  , N128, N129, N130, N131
  , N132, N133, N134, N135
  , N136, N137, N138, N139
  , N140, N141, N142, N143
  , N144, N145, N146, N147
  , N148, N149, N150, N151
  , N152, N153, N154, N155
  , N156, N157, N158, N159
  , N160, N161, N162, N163
  , N164, N165, N166, N167
  , N168, N169, N170, N171
  , N172, N173, N174, N175
  , N176, N177, N178, N179
  , N180, N181, N182, N183
  , N184, N185, N186, N187
  , N188, N189, N190, N191
  , N192, N193, N194, N195
  , N196, N197, N198, N199
  , N200, N201, N202, N203
  , N204, N205, N206, N207
  , N208, N209, N210, N211
  , N212, N213, N214, N215
  , N216, N217, N218, N219
  , N220, N221, N222, N223
  , N224, N225, N226, N227
  , N228, N229, N230, N231
  , N232, N233, N234, N235
  , N236, N237, N238, N239
  , N240, N241, N242, N243
  , N244, N245, N246, N247
  , N248, N249, N250, N251
  , N252, N253, N254, N255
  , n0,  n1,  n2,  n3
  , n4,  n5,  n6,  n7
  , n8,  n9,  n10, n11
  , n12, n13, n14, n15
  , n16, n17, n18, n19
  , n20, n21, n22, n23
  , n24, n25, n26, n27
  , n28, n29, n30, n31
  , n32, n33, n34, n35
  , n36, n37, n38, n39
  , n40, n41, n42, n43
  , n44, n45, n46, n47
  , n48, n49, n50, n51
  , n52, n53, n54, n55
  , n56, n57, n58, n59
  , n60, n61, n62, n63
  , n64, n65, n66, n67
  , n68, n69, n70, n71
  , n72, n73, n74, n75
  , n76, n77, n78, n79
  , n80, n81, n82, n83
  , n84, n85, n86, n87
  , n88, n89, n90, n91
  , n92, n93, n94, n95
  , n96, n97, n98, n99
  , n100, n101, n102, n103
  , n104, n105, n106, n107
  , n108, n109, n110, n111
  , n112, n113, n114, n115
  , n116, n117, n118, n119
  , n120, n121, n122, n123
  , n124, n125, n126, n127
  , n128, n129, n130, n131
  , n132, n133, n134, n135
  , n136, n137, n138, n139
  , n140, n141, n142, n143
  , n144, n145, n146, n147
  , n148, n149, n150, n151
  , n152, n153, n154, n155
  , n156, n157, n158, n159
  , n160, n161, n162, n163
  , n164, n165, n166, n167
  , n168, n169, n170, n171
  , n172, n173, n174, n175
  , n176, n177, n178, n179
  , n180, n181, n182, n183
  , n184, n185, n186, n187
  , n188, n189, n190, n191
  , n192, n193, n194, n195
  , n196, n197, n198, n199
  , n200, n201, n202, n203
  , n204, n205, n206, n207
  , n208, n209, n210, n211
  , n212, n213, n214, n215
  , n216, n217, n218, n219
  , n220, n221, n222, n223
  , n224, n225, n226, n227
  , n228, n229, n230, n231
  , n232, n233, n234, n235
  , n236, n237, n238, n239
  , n240, n241, n242, n243
  , n244, n245, n246, n247
  , n248, n249, n250, n251
  , n252, n253, n254, n255
  , Z(), S()
  , Vec(Vec)
  , N(value)
  , Add
  , Mul
  , Less
  ) where

import Data.List
import Control.Monad

data Z = Z
data S a = S a

type N0 = Z
type N1 = S N0
type N2 = S N1
type N3 = S N2
type N4 = S N3
type N5 = S N4
type N6 = S N5
type N7 = S N6
type N8 = S N7
type N9 = S N8
type N10 = S N9
type N11 = S N10
type N12 = S N11
type N13 = S N12
type N14 = S N13
type N15 = S N14
type N16 = S N15
type N17 = S N16
type N18 = S N17
type N19 = S N18
type N20 = S N19
type N21 = S N20
type N22 = S N21
type N23 = S N22
type N24 = S N23
type N25 = S N24
type N26 = S N25
type N27 = S N26
type N28 = S N27
type N29 = S N28
type N30 = S N29
type N31 = S N30
type N32 = S N31
type N33 = S N32
type N34 = S N33
type N35 = S N34
type N36 = S N35
type N37 = S N36
type N38 = S N37
type N39 = S N38
type N40 = S N39
type N41 = S N40
type N42 = S N41
type N43 = S N42
type N44 = S N43
type N45 = S N44
type N46 = S N45
type N47 = S N46
type N48 = S N47
type N49 = S N48
type N50 = S N49
type N51 = S N50
type N52 = S N51
type N53 = S N52
type N54 = S N53
type N55 = S N54
type N56 = S N55
type N57 = S N56
type N58 = S N57
type N59 = S N58
type N60 = S N59
type N61 = S N60
type N62 = S N61
type N63 = S N62
type N64 = S N63
type N65 = S N64
type N66 = S N65
type N67 = S N66
type N68 = S N67
type N69 = S N68
type N70 = S N69
type N71 = S N70
type N72 = S N71
type N73 = S N72
type N74 = S N73
type N75 = S N74
type N76 = S N75
type N77 = S N76
type N78 = S N77
type N79 = S N78
type N80 = S N79
type N81 = S N80
type N82 = S N81
type N83 = S N82
type N84 = S N83
type N85 = S N84
type N86 = S N85
type N87 = S N86
type N88 = S N87
type N89 = S N88
type N90 = S N89
type N91 = S N90
type N92 = S N91
type N93 = S N92
type N94 = S N93
type N95 = S N94
type N96 = S N95
type N97 = S N96
type N98 = S N97
type N99 = S N98
type N100 = S N99
type N101 = S N100
type N102 = S N101
type N103 = S N102
type N104 = S N103
type N105 = S N104
type N106 = S N105
type N107 = S N106
type N108 = S N107
type N109 = S N108
type N110 = S N109
type N111 = S N110
type N112 = S N111
type N113 = S N112
type N114 = S N113
type N115 = S N114
type N116 = S N115
type N117 = S N116
type N118 = S N117
type N119 = S N118
type N120 = S N119
type N121 = S N120
type N122 = S N121
type N123 = S N122
type N124 = S N123
type N125 = S N124
type N126 = S N125
type N127 = S N126
type N128 = S N127
type N129 = S N128
type N130 = S N129
type N131 = S N130
type N132 = S N131
type N133 = S N132
type N134 = S N133
type N135 = S N134
type N136 = S N135
type N137 = S N136
type N138 = S N137
type N139 = S N138
type N140 = S N139
type N141 = S N140
type N142 = S N141
type N143 = S N142
type N144 = S N143
type N145 = S N144
type N146 = S N145
type N147 = S N146
type N148 = S N147
type N149 = S N148
type N150 = S N149
type N151 = S N150
type N152 = S N151
type N153 = S N152
type N154 = S N153
type N155 = S N154
type N156 = S N155
type N157 = S N156
type N158 = S N157
type N159 = S N158
type N160 = S N159
type N161 = S N160
type N162 = S N161
type N163 = S N162
type N164 = S N163
type N165 = S N164
type N166 = S N165
type N167 = S N166
type N168 = S N167
type N169 = S N168
type N170 = S N169
type N171 = S N170
type N172 = S N171
type N173 = S N172
type N174 = S N173
type N175 = S N174
type N176 = S N175
type N177 = S N176
type N178 = S N177
type N179 = S N178
type N180 = S N179
type N181 = S N180
type N182 = S N181
type N183 = S N182
type N184 = S N183
type N185 = S N184
type N186 = S N185
type N187 = S N186
type N188 = S N187
type N189 = S N188
type N190 = S N189
type N191 = S N190
type N192 = S N191
type N193 = S N192
type N194 = S N193
type N195 = S N194
type N196 = S N195
type N197 = S N196
type N198 = S N197
type N199 = S N198
type N200 = S N199
type N201 = S N200
type N202 = S N201
type N203 = S N202
type N204 = S N203
type N205 = S N204
type N206 = S N205
type N207 = S N206
type N208 = S N207
type N209 = S N208
type N210 = S N209
type N211 = S N210
type N212 = S N211
type N213 = S N212
type N214 = S N213
type N215 = S N214
type N216 = S N215
type N217 = S N216
type N218 = S N217
type N219 = S N218
type N220 = S N219
type N221 = S N220
type N222 = S N221
type N223 = S N222
type N224 = S N223
type N225 = S N224
type N226 = S N225
type N227 = S N226
type N228 = S N227
type N229 = S N228
type N230 = S N229
type N231 = S N230
type N232 = S N231
type N233 = S N232
type N234 = S N233
type N235 = S N234
type N236 = S N235
type N237 = S N236
type N238 = S N237
type N239 = S N238
type N240 = S N239
type N241 = S N240
type N242 = S N241
type N243 = S N242
type N244 = S N243
type N245 = S N244
type N246 = S N245
type N247 = S N246
type N248 = S N247
type N249 = S N248
type N250 = S N249
type N251 = S N250
type N252 = S N251
type N253 = S N252
type N254 = S N253
type N255 = S N254

n0 :: N0
n0 = Z

n1 :: N1
n1 = S n0

n2 :: N2
n2 = S n1

n3 :: N3
n3 = S n2

n4 :: N4
n4 = S n3

n5 :: N5
n5 = S n4

n6 :: N6
n6 = S n5

n7 :: N7
n7 = S n6

n8 :: N8
n8 = S n7

n9 :: N9
n9 = S n8

n10 :: N10
n10 = S n9

n11 :: N11
n11 = S n10

n12 :: N12
n12 = S n11

n13 :: N13
n13 = S n12

n14 :: N14
n14 = S n13

n15 :: N15
n15 = S n14

n16 :: N16
n16 = S n15

n17 :: N17
n17 = S n16

n18 :: N18
n18 = S n17

n19 :: N19
n19 = S n18

n20 :: N20
n20 = S n19

n21 :: N21
n21 = S n20

n22 :: N22
n22 = S n21

n23 :: N23
n23 = S n22

n24 :: N24
n24 = S n23

n25 :: N25
n25 = S n24

n26 :: N26
n26 = S n25

n27 :: N27
n27 = S n26

n28 :: N28
n28 = S n27

n29 :: N29
n29 = S n28

n30 :: N30
n30 = S n29

n31 :: N31
n31 = S n30

n32 :: N32
n32 = S n31

n33 :: N33
n33 = S n32

n34 :: N34
n34 = S n33

n35 :: N35
n35 = S n34

n36 :: N36
n36 = S n35

n37 :: N37
n37 = S n36

n38 :: N38
n38 = S n37

n39 :: N39
n39 = S n38

n40 :: N40
n40 = S n39

n41 :: N41
n41 = S n40

n42 :: N42
n42 = S n41

n43 :: N43
n43 = S n42

n44 :: N44
n44 = S n43

n45 :: N45
n45 = S n44

n46 :: N46
n46 = S n45

n47 :: N47
n47 = S n46

n48 :: N48
n48 = S n47

n49 :: N49
n49 = S n48

n50 :: N50
n50 = S n49

n51 :: N51
n51 = S n50

n52 :: N52
n52 = S n51

n53 :: N53
n53 = S n52

n54 :: N54
n54 = S n53

n55 :: N55
n55 = S n54

n56 :: N56
n56 = S n55

n57 :: N57
n57 = S n56

n58 :: N58
n58 = S n57

n59 :: N59
n59 = S n58

n60 :: N60
n60 = S n59

n61 :: N61
n61 = S n60

n62 :: N62
n62 = S n61

n63 :: N63
n63 = S n62

n64 :: N64
n64 = S n63

n65 :: N65
n65 = S n64

n66 :: N66
n66 = S n65

n67 :: N67
n67 = S n66

n68 :: N68
n68 = S n67

n69 :: N69
n69 = S n68

n70 :: N70
n70 = S n69

n71 :: N71
n71 = S n70

n72 :: N72
n72 = S n71

n73 :: N73
n73 = S n72

n74 :: N74
n74 = S n73

n75 :: N75
n75 = S n74

n76 :: N76
n76 = S n75

n77 :: N77
n77 = S n76

n78 :: N78
n78 = S n77

n79 :: N79
n79 = S n78

n80 :: N80
n80 = S n79

n81 :: N81
n81 = S n80

n82 :: N82
n82 = S n81

n83 :: N83
n83 = S n82

n84 :: N84
n84 = S n83

n85 :: N85
n85 = S n84

n86 :: N86
n86 = S n85

n87 :: N87
n87 = S n86

n88 :: N88
n88 = S n87

n89 :: N89
n89 = S n88

n90 :: N90
n90 = S n89

n91 :: N91
n91 = S n90

n92 :: N92
n92 = S n91

n93 :: N93
n93 = S n92

n94 :: N94
n94 = S n93

n95 :: N95
n95 = S n94

n96 :: N96
n96 = S n95

n97 :: N97
n97 = S n96

n98 :: N98
n98 = S n97

n99 :: N99
n99 = S n98

n100 :: N100
n100 = S n99

n101 :: N101
n101 = S n100

n102 :: N102
n102 = S n101

n103 :: N103
n103 = S n102

n104 :: N104
n104 = S n103

n105 :: N105
n105 = S n104

n106 :: N106
n106 = S n105

n107 :: N107
n107 = S n106

n108 :: N108
n108 = S n107

n109 :: N109
n109 = S n108

n110 :: N110
n110 = S n109

n111 :: N111
n111 = S n110

n112 :: N112
n112 = S n111

n113 :: N113
n113 = S n112

n114 :: N114
n114 = S n113

n115 :: N115
n115 = S n114

n116 :: N116
n116 = S n115

n117 :: N117
n117 = S n116

n118 :: N118
n118 = S n117

n119 :: N119
n119 = S n118

n120 :: N120
n120 = S n119

n121 :: N121
n121 = S n120

n122 :: N122
n122 = S n121

n123 :: N123
n123 = S n122

n124 :: N124
n124 = S n123

n125 :: N125
n125 = S n124

n126 :: N126
n126 = S n125

n127 :: N127
n127 = S n126

n128 :: N128
n128 = S n127

n129 :: N129
n129 = S n128

n130 :: N130
n130 = S n129

n131 :: N131
n131 = S n130

n132 :: N132
n132 = S n131

n133 :: N133
n133 = S n132

n134 :: N134
n134 = S n133

n135 :: N135
n135 = S n134

n136 :: N136
n136 = S n135

n137 :: N137
n137 = S n136

n138 :: N138
n138 = S n137

n139 :: N139
n139 = S n138

n140 :: N140
n140 = S n139

n141 :: N141
n141 = S n140

n142 :: N142
n142 = S n141

n143 :: N143
n143 = S n142

n144 :: N144
n144 = S n143

n145 :: N145
n145 = S n144

n146 :: N146
n146 = S n145

n147 :: N147
n147 = S n146

n148 :: N148
n148 = S n147

n149 :: N149
n149 = S n148

n150 :: N150
n150 = S n149

n151 :: N151
n151 = S n150

n152 :: N152
n152 = S n151

n153 :: N153
n153 = S n152

n154 :: N154
n154 = S n153

n155 :: N155
n155 = S n154

n156 :: N156
n156 = S n155

n157 :: N157
n157 = S n156

n158 :: N158
n158 = S n157

n159 :: N159
n159 = S n158

n160 :: N160
n160 = S n159

n161 :: N161
n161 = S n160

n162 :: N162
n162 = S n161

n163 :: N163
n163 = S n162

n164 :: N164
n164 = S n163

n165 :: N165
n165 = S n164

n166 :: N166
n166 = S n165

n167 :: N167
n167 = S n166

n168 :: N168
n168 = S n167

n169 :: N169
n169 = S n168

n170 :: N170
n170 = S n169

n171 :: N171
n171 = S n170

n172 :: N172
n172 = S n171

n173 :: N173
n173 = S n172

n174 :: N174
n174 = S n173

n175 :: N175
n175 = S n174

n176 :: N176
n176 = S n175

n177 :: N177
n177 = S n176

n178 :: N178
n178 = S n177

n179 :: N179
n179 = S n178

n180 :: N180
n180 = S n179

n181 :: N181
n181 = S n180

n182 :: N182
n182 = S n181

n183 :: N183
n183 = S n182

n184 :: N184
n184 = S n183

n185 :: N185
n185 = S n184

n186 :: N186
n186 = S n185

n187 :: N187
n187 = S n186

n188 :: N188
n188 = S n187

n189 :: N189
n189 = S n188

n190 :: N190
n190 = S n189

n191 :: N191
n191 = S n190

n192 :: N192
n192 = S n191

n193 :: N193
n193 = S n192

n194 :: N194
n194 = S n193

n195 :: N195
n195 = S n194

n196 :: N196
n196 = S n195

n197 :: N197
n197 = S n196

n198 :: N198
n198 = S n197

n199 :: N199
n199 = S n198

n200 :: N200
n200 = S n199

n201 :: N201
n201 = S n200

n202 :: N202
n202 = S n201

n203 :: N203
n203 = S n202

n204 :: N204
n204 = S n203

n205 :: N205
n205 = S n204

n206 :: N206
n206 = S n205

n207 :: N207
n207 = S n206

n208 :: N208
n208 = S n207

n209 :: N209
n209 = S n208

n210 :: N210
n210 = S n209

n211 :: N211
n211 = S n210

n212 :: N212
n212 = S n211

n213 :: N213
n213 = S n212

n214 :: N214
n214 = S n213

n215 :: N215
n215 = S n214

n216 :: N216
n216 = S n215

n217 :: N217
n217 = S n216

n218 :: N218
n218 = S n217

n219 :: N219
n219 = S n218

n220 :: N220
n220 = S n219

n221 :: N221
n221 = S n220

n222 :: N222
n222 = S n221

n223 :: N223
n223 = S n222

n224 :: N224
n224 = S n223

n225 :: N225
n225 = S n224

n226 :: N226
n226 = S n225

n227 :: N227
n227 = S n226

n228 :: N228
n228 = S n227

n229 :: N229
n229 = S n228

n230 :: N230
n230 = S n229

n231 :: N231
n231 = S n230

n232 :: N232
n232 = S n231

n233 :: N233
n233 = S n232

n234 :: N234
n234 = S n233

n235 :: N235
n235 = S n234

n236 :: N236
n236 = S n235

n237 :: N237
n237 = S n236

n238 :: N238
n238 = S n237

n239 :: N239
n239 = S n238

n240 :: N240
n240 = S n239

n241 :: N241
n241 = S n240

n242 :: N242
n242 = S n241

n243 :: N243
n243 = S n242

n244 :: N244
n244 = S n243

n245 :: N245
n245 = S n244

n246 :: N246
n246 = S n245

n247 :: N247
n247 = S n246

n248 :: N248
n248 = S n247

n249 :: N249
n249 = S n248

n250 :: N250
n250 = S n249

n251 :: N251
n251 = S n250

n252 :: N252
n252 = S n251

n253 :: N253
n253 = S n252

n254 :: N254
n254 = S n253

n255 :: N255
n255 = S n254

npred :: S a -> a
npred n = undefined

class N a where
  value :: a -> Int

instance N Z where
  value n = 0

instance N a => N (S a) where
  value n = 1 + value (npred n)

class Less a b
instance Less Z (S a)
instance Less a b => Less (S a) (S b)

class Add a b c | a b -> c
instance Add Z b b
instance Add a b c => Add (S a) b (S c)

class Mul a b c | a b -> c
instance Mul Z b Z
instance (Mul a b x, Add x b c) => Mul (S a) b c

newtype Vec n a = Vec [a]
  deriving Show

vempty :: Vec Z a
vempty = Vec []

infixr 5 +>
(+>) :: a -> Vec n a -> Vec (S n) a
a +> Vec as = Vec (a:as)

infixl 5 <+
(<+) :: Vec n a -> a -> Vec (S n) a
Vec as <+ a = Vec (as++[a])

velems :: Vec n a -> [a]
velems (Vec as) = as

vsize :: Vec n a -> n
vsize = undefined

sized :: N n => (Int -> Vec n a) -> Vec n a
sized f = let v = f (value $ vsize v) in v

vec :: N n => [a] -> Vec n a
vec as = sized $ \n -> Vec $ take n (as ++ repeat err)
  where err = error "Unitialised element in vector"

vecOf :: N n => a -> Vec n a
vecOf a = sized $ \n -> Vec (replicate n a)

ofSize :: Vec n a -> n -> Vec n a
ofSize v n = v

sameSize :: Vec n a -> Vec n a -> Vec n a
sameSize v w = v

vecn :: N n => n -> [a] -> Vec n a
vecn n as = vec as

vextend :: N m => a -> Vec n a -> Vec m a
vextend a (Vec xs) = sized $ \m -> Vec (take m (xs ++ repeat a))

vsingle :: a -> Vec N1 a
vsingle a = Vec [a]

vhead :: Vec (S n) a -> a
vhead (Vec as) = head as

vtail :: Vec (S n) a -> Vec n a
vtail (Vec as) = Vec (tail as)

vlast :: Vec (S n) a -> a
vlast (Vec as) = last as

vinit :: Vec (S n) a -> Vec n a
vinit (Vec as) = Vec (init as)

vlength :: Vec n a -> Int
vlength (Vec as) = length as

vmap :: (a -> b) -> Vec n a -> Vec n b
vmap f (Vec as) = Vec (map f as)

vzip :: Vec n a -> Vec n b -> Vec n (a, b)
vzip (Vec as) (Vec bs) = Vec (zip as bs)

vzipWith :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
vzipWith f (Vec as) (Vec bs) = Vec (zipWith f as bs)

instance Functor (Vec n) where
  fmap = vmap

vreverse :: Vec n a -> Vec n a
vreverse (Vec as) = Vec (reverse as)

vfoldl :: (a -> b -> a) -> a -> Vec n b -> a
vfoldl f z (Vec bs) = foldl f z bs

vfoldr :: (a -> b -> b) -> b -> Vec n a -> b
vfoldr f z (Vec as) = foldr f z as

vmapAccumL :: (a -> b -> (a, c)) -> a -> Vec n b -> (a, Vec n c)
vmapAccumL f acc (Vec bs) = (a, Vec cs)
  where (a, cs) = mapAccumL f acc bs

vmapAccumR :: (a -> b -> (a, c)) -> a -> Vec n b -> (a, Vec n c)
vmapAccumR f acc (Vec bs) = (a, Vec cs)
  where (a, cs) = mapAccumR f acc bs

infixr 5 <++>
(<++>) :: Add n m o => Vec n a -> Vec m a -> Vec o a
Vec as <++> Vec bs = Vec (as ++ bs)

vconcat :: Mul n m o => Vec n (Vec m a) -> Vec o a
vconcat (Vec v) = Vec $ concatMap velems v

vat :: (N n, Less n m) => Vec m a -> n -> a
Vec vs `vat` n = vs !! value n

vtake :: N n => n -> Vec m a -> Vec n a
vtake n (Vec as) = Vec $ take (value n) as

vdrop :: (N n, Add n o m) => n -> Vec m a -> Vec o a
vdrop n (Vec as) = Vec $ drop (value n) as

vsplitAt :: (N m, Add m n o) => m -> Vec o a -> (Vec m a, Vec n a)
vsplitAt m (Vec as) = (Vec bs, Vec cs)
  where (bs, cs) = splitAt (value m) as

vgroup :: (N n, Mul n o m) => n -> Vec m a -> Vec o (Vec n a)
vgroup n (Vec as) = Vec $ map Vec $ groupN (value n) as
  where groupN n [] = []
        groupN n xs = take n xs : groupN n (drop n xs)

vtranspose :: Vec n (Vec m a) -> Vec m (Vec n a)
vtranspose (Vec vs) = Vec $ map Vec $ transpose $ map velems vs

vshr :: a -> Vec n a -> Vec n a
vshr a (Vec as) = Vec (if null as then [] else a:init as)

vshl :: Vec n a -> a -> Vec n a
vshl (Vec as) a = Vec (if null as then [] else tail as ++ [a])

vreplicate :: N n => n -> a -> Vec n a
vreplicate n a = Vec $ replicate (value n) a

vrepeat :: N n => a -> Vec n a
vrepeat a = sized $ \n -> Vec (replicate n a)

vsequence :: Monad m => Vec n (m a) -> m (Vec n a)
vsequence (Vec ms) = liftM Vec (sequence ms)

vrigid :: N n => Vec n a -> Vec n a
vrigid (Vec xs) = sized (\n -> Vec [xs !! i | i <- [0..n-1]])
