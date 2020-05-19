-- | Minimal PDB parser.  <https://www.wwpdb.org/documentation/file-format-content/format33/v3.3.html>
module Sound.SC3.Data.Chemistry.PDB.Parse where

import Control.Monad {- base -}
import Data.Char {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Data.ByteString.Char8 as T {- bytestring -}

import qualified Music.Theory.Directory as T {- hmt -}

import Sound.SC3.Data.Chemistry.PDB.Types {- hsc3-data -}

-- * RECORDS-SE

{- | ATOM/HETATM

COLUMNS        DATA  TYPE    FIELD        DEFINITION
-------------------------------------------------------------------------------------
 1 -  6        Record name   "ATOM  "
 7 - 11        Integer       serial       Atom  serial number.
13 - 16        Atom          name         Atom name.
17             Character     altLoc       Alternate location indicator.
18 - 20        Residue name  resName      Residue name.
22             Character     chainID      Chain identifier.
23 - 26        Integer       resSeq       Residue sequence number.
27             AChar         iCode        Code for insertion of residues.
31 - 38        Real(8.3)     x            Orthogonal coordinates for X in Angstroms.
39 - 46        Real(8.3)     y            Orthogonal coordinates for Y in Angstroms.
47 - 54        Real(8.3)     z            Orthogonal coordinates for Z in Angstroms.
55 - 60        Real(6.2)     occupancy    Occupancy.
61 - 66        Real(6.2)     tempFactor   Temperature  factor.
77 - 78        LString(2)    element      Element symbol, right-justified.
79 - 80        LString(2)    charge       Charge  on the atom.
-}
atom_se :: Num i => ([i],[i])
atom_se =
  ([ 1, 7,13,17,18,22,23,27,31,39,47,55,61,77,79]
  ,[ 6,11,16,17,20,22,26,27,38,46,54,60,66,78,80])

{- | CISPEP

COLUMNS       DATA  TYPE    FIELD         DEFINITION
-------------------------------------------------------------------------
 1 -  6       Record name   "CISPEP"
 8 - 10       Integer       serNum        Record serial number.
12 - 14       LString(3)    pep1          Residue name.
16            Character     chainID1      Chain identifier.
18 - 21       Integer       seqNum1       Residue sequence number.
22            AChar         icode1        Insertion code.
26 - 28       LString(3)    pep2          Residue name.
30            Character     chainID2      Chain identifier.
32 - 35       Integer       seqNum2       Residue sequence number.
36            AChar         icode2        Insertion code.
44 - 46       Integer       modNum        Identifies the specific model.
54 - 59       Real(6.2)     measure       Angle measurement in degrees.
-}
cispep_se :: Num i => ([i],[i])
cispep_se =
  ([ 1, 8,12,16,18,22,26,30,32,36,44,54]
  ,[ 6,10,14,16,21,22,28,30,35,36,46,59])

{- | CONECT

COLUMNS       DATA  TYPE      FIELD        DEFINITION
-------------------------------------------------------------------------
 1 -  6        Record name    "CONECT"
 7 - 11        Integer        serial       Atom  serial number
12 - 16        Integer        serial       Serial number of bonded atom
17 - 21        Integer        serial       Serial  number of bonded atom
22 - 26        Integer        serial       Serial number of bonded atom
27 - 31        Integer        serial       Serial number of bonded atom
-}
conect_se :: Num i => ([i],[i])
conect_se = ([1,7,12,17,22,27],[6,11,16,21,26,31])

{- | CRYST1

COLUMNS       DATA  TYPE    FIELD          DEFINITION
-------------------------------------------------------------
 1 -  6       Record name   "CRYST1"
 7 - 15       Real(9.3)     a              a (Angstroms).
16 - 24       Real(9.3)     b              b (Angstroms).
25 - 33       Real(9.3)     c              c (Angstroms).
34 - 40       Real(7.2)     alpha          alpha (degrees).
41 - 47       Real(7.2)     beta           beta (degrees).
48 - 54       Real(7.2)     gamma          gamma (degrees).
56 - 66       LString       sGroup         Space  group.
67 - 70       Integer       z              Z value.
-}
cryst1_se :: Num i => ([i],[i])
cryst1_se = ([1,7,16,25,34,41,48,56,67],[6,15,24,33,40,47,54,66,70])

{- | END

COLUMNS       DATA  TYPE     FIELD         DEFINITION
-------------------------------------------------------
1 -  6        Record name    "END   "
-}
end_se :: Num i => ([i],[i])
end_se = ([1],[6])

{- | ENDMDL

COLUMNS       DATA  TYPE     FIELD        DEFINITION
------------------------------------------------------------------
1 - 6         Record name   "ENDMDL"
-}
endmdl_se :: Num i => ([i],[i])
endmdl_se = ([1],[6])

{- | FORMUL

COLUMNS        DATA TYPE     FIELD         DEFINITION
-----------------------------------------------------------------------
 1 -  6        Record name   "FORMUL"
 9 - 10        Integer       compNum       Component  number.
13 - 15        LString(3)    hetID         Het identifier.
17 - 18        Integer       continuation  Continuation number.
19             Character     asterisk      "*" for water.
20 - 70        String        text          Chemical formula.
-}
formul_se :: Num i => ([i],[i])
formul_se = ([1,9,13,17,19,20],[6,10,15,18,19,70])

{- | HEADER

COLUMNS       DATA  TYPE     FIELD             DEFINITION
------------------------------------------------------------------------------------
 1 -  6       Record name    "HEADER"
11 - 50       String(40)     classification    Classifies the molecule(s).
51 - 59       Date           depDate           Deposition date. This is the date the
                                               coordinates  were received at the PDT.
63 - 66       IDcode         idCode            This identifier is unique within the PDT.
-}
header_se :: Num i => ([i],[i])
header_se = ([1,11,51,63],[6,50,59,66])

{- | HELIX

COLUMNS        DATA  TYPE     FIELD         DEFINITION
-----------------------------------------------------------------------------------
 1 -  6        Record name    "HELIX "
 8 - 10        Integer        serNum        Serial number of the helix. This starts
                                            at 1  and increases incrementally.
12 - 14        LString(3)     helixID       Helix  identifier. In addition to a serial
                                            number, each helix is given an
                                            alphanumeric character helix identifier.
16 - 18        Residue name   initResName   Name of the initial residue.
20             Character      initChainID   Chain identifier for the chain containing
                                            this  helix.
22 - 25        Integer        initSeqNum    Sequence number of the initial residue.
26             AChar          initICode     Insertion code of the initial residue.
28 - 30        Residue  name  endResName    Name of the terminal residue of the helix.
32             Character      endChainID    Chain identifier for the chain containing
                                            this  helix.
34 - 37        Integer        endSeqNum     Sequence number of the terminal residue.
38             AChar          endICode      Insertion code of the terminal residue.
39 - 40        Integer        helixClass    Helix class (see below).
41 - 70        String         comment       Comment about this helix.
72 - 76        Integer        length        Length of this helix.
-}
helix_se :: Num i => ([i],[i])
helix_se =
  ([1, 8,12,16,20,22,26,28,32,34,38,39,41,72]
  ,[6,10,14,18,20,25,26,30,32,37,38,40,70,76])

{- | HET

COLUMNS       DATA  TYPE     FIELD         DEFINITION
---------------------------------------------------------------------------------
 1 -  6       Record name   "HET   "
 8 - 10       LString(3)    hetID          Het identifier, right-justified.
13            Character     ChainID        Chain  identifier.
14 - 17       Integer       seqNum         Sequence  number.
18            AChar         iCode          Insertion  code.
21 - 25       Integer       numHetAtoms    Number of HETATM records for the group
                                           present in the entry.
31 - 70       String        text           Text describing Het group.
-}
het_se :: Num i => ([i],[i])
het_se = ([1,8,13,14,18,21,31],[6,10,13,17,18,25,70])

{- | LINK

COLUMNS         DATA TYPE      FIELD           DEFINITION
------------------------------------------------------------------------------------
 1 -  6         Record name    "LINK  "
13 - 16         Atom           name1           Atom name.
17              Character      altLoc1         Alternate location indicator.
18 - 20         Residue name   resName1        Residue  name.
22              Character      chainID1        Chain identifier.
23 - 26         Integer        resSeq1         Residue sequence number.
27              AChar          iCode1          Insertion code.
43 - 46         Atom           name2           Atom name.
47              Character      altLoc2         Alternate location indicator.
48 - 50         Residue name   resName2        Residue name.
52              Character      chainID2        Chain identifier.
53 - 56         Integer        resSeq2         Residue sequence number.
57              AChar          iCode2          Insertion code.
60 - 65         SymOP          sym1            Symmetry operator atom 1.
67 - 72         SymOP          sym2            Symmetry operator atom 2.
74 – 78         Real(5.2)      Length          Link distance
-}
link_se :: Num i => ([i],[i])
link_se = ([1,13,17,18,22,23,27,43,47,48,52,53,57,60,67,74]
          ,[6,16,17,20,22,26,27,46,47,50,52,56,57,65,72,78])

{- | MASTER


COLUMNS         DATA TYPE     FIELD          DEFINITION
----------------------------------------------------------------------------------
 1 -  6         Record name   "MASTER"
11 - 15         Integer       numRemark      Number of REMARK records
16 - 20         Integer       "0"
21 - 25         Integer       numHet         Number of HET records
26 - 30         Integer       numHelix       Number of HELIX records
31 - 35         Integer       numSheet       Number of SHEET records
36 - 40         Integer       numTurn        deprecated
41 - 45         Integer       numSite        Number of SITE records
46 - 50         Integer       numXform       Number of coordinate transformation
                                             records  (ORIGX+SCALE+MTRIX)
51 - 55         Integer       numCoord       Number of atomic coordinate records
                                             records (ATOM+HETATM)
56 - 60         Integer       numTer         Number of TER records
61 - 65         Integer       numConect      Number of CONECT records
66 - 70         Integer       numSeq         Number of SEQRES records
-}
master_se :: (Num i,Enum i) => ([i],[i])
master_se = (1 : [11,16 ..66],6 : [15,20 .. 70])

{- | MDLTYP

COLUMNS      DATA TYPE      FIELD         DEFINITION
------------------------------------------------------------------------------------
 1 -  6      Record name    "MDLTYP"
 9 - 10      Continuation   continuation  Allows concatenation of multiple records.
11 - 80      SList          comment       Free Text providing  additional structural
                                          annotation.
-}
mdltyp_se :: Num i => ([i],[i])
mdltyp_se = ([1,9,11],[6,10,80])

{- | MODEL

COLUMNS        DATA  TYPE    FIELD          DEFINITION
---------------------------------------------------------------------------------------
 1 -  6        Record name   "MODEL "
11 - 14        Integer       serial         Model serial number.
-}
model_se :: Num i => ([i],[i])
model_se = ([1,11],[6,14])

{- | MODRES

COLUMNS        DATA TYPE     FIELD       DEFINITION
--------------------------------------------------------------------------------
 1 -  6        Record name   "MODRES"
 8 - 11        IDcode        idCode      ID code of this entry.
13 - 15        Residue name  resName     Residue name used in this entry.
17             Character     chainID     Chain identifier.
19 - 22        Integer       seqNum      Sequence number.
23             AChar         iCode       Insertion code.
25 - 27        Residue name  stdRes      Standard residue name.
30 - 70        String        comment     Description of the residue modification.
-}
modres_se :: Num i => ([i],[i])
modres_se =
  ([ 1, 8,13,17,19,23,25,30]
  ,[ 6,11,15,17,22,23,27,70])

{- | NUMMDL

COLUMNS      DATA TYPE      FIELD         DEFINITION
------------------------------------------------------------------------------------
 1 -  6      Record name    "NUMMDL"
11 - 14      Integer        modelNumber   Number of models.
-}
nummdl_se :: Num i => ([i],[i])
nummdl_se = ([1,11],[6,14])

{- | OBSLTE

COLUMNS       DATA  TYPE     FIELD         DEFINITION
---------------------------------------------------------------------------------------
 1 -  6       Record name   "OBSLTE"
 9 - 10       Continuation  continuation  Allows concatenation of multiple records
12 - 20       Date          repDate       Date that this entry was replaced.
22 - 25       IDcode        idCode        ID code of this entry.
32 - 35       IDcode        rIdCode       ID code of entry that replaced this one.
37 - 40       IDcode        rIdCode       ID code of entry that replaced this one.
42 - 45       IDcode        rIdCode       ID code of entry  that replaced this one.
47 - 50       IDcode        rIdCode       ID code of entry that replaced this one.
52 - 55       IDcode        rIdCode       ID code of entry that replaced this one.
57 - 60       IDcode        rIdCode       ID code of entry that replaced this one.
62 - 65       IDcode        rIdCode       ID code of entry that replaced this one.
67 - 70       IDcode        rIdCode       ID code of entry that replaced this one.
72 - 75       IDcode        rIdCode       ID code of entry that replaced this one.
-}
obslte_se :: Num i => ([i],[i])
obslte_se = ([1,9,12,22,32,37,42,27,52,57,62,67,72],[6,10,20,25,35,40,45,50,55,60,65,70,75])

{- | REMARK

COLUMNS       DATA TYPE     FIELD         DEFINITION
--------------------------------------------------------------------------------------
 1 -  6       Record name   "REMARK"
 8 - 10       Integer       remarkNum     Remark  number. It is not an error for
                                          remark n to exist in an entry when
                                          remark n-1 does not.
12 - 79       LString       empty         Left  as white space in first line
                                          of each  new remark.
-}
remark_se :: Num i => ([i],[i])
remark_se = ([1,8,12],[6,10,79])

{- | SEQRES

COLUMNS        DATA TYPE      FIELD        DEFINITION
-------------------------------------------------------------------------------------
 1 -  6        Record name    "SEQRES"
 8 - 10        Integer        serNum       Serial number of the SEQRES record for  the
                                           current  chain. Starts at 1 and increments
                                           by one  each line. Reset to 1 for each chain.
12             Character      chainID      Chain identifier. This may be any single
                                           legal  character, including a blank which is
                                           is  used if there is only one chain.
14 - 17        Integer        numRes       Number of residues in the chain.
                                           This  value is repeated on every record.
20 - 22        Residue name   resName      Residue name.
24 - 26        Residue name   resName      Residue name.
28 - 30        Residue name   resName      Residue name.
32 - 34        Residue name   resName      Residue name.
36 - 38        Residue name   resName      Residue name.
40 - 42        Residue name   resName      Residue name.
44 - 46        Residue name   resName      Residue name.
48 - 50        Residue name   resName      Residue name.
52 - 54        Residue name   resName      Residue name.
56 - 58        Residue name   resName      Residue name.
60 - 62        Residue name   resName      Residue name.
64 - 66        Residue name   resName      Residue name.
68 - 70        Residue name   resName      Residue name.
-}
seqres_se :: (Num i,Enum i) => ([i],[i])
seqres_se = ([1,8,12,14] ++ [20,24 .. 68],[6,10,12,17] ++ [22,26 .. 70])

{- | SHEET

COLUMNS       DATA  TYPE     FIELD          DEFINITION
-------------------------------------------------------------------------------------
 1 -  6        Record name   "SHEET "
 8 - 10        Integer       strand         Strand  number which starts at 1 for each
                                            strand within a sheet and increases by one.
12 - 14        LString(3)    sheetID        Sheet  identifier.
15 - 16        Integer       numStrands     Number  of strands in sheet.
18 - 20        Residue name  initResName    Residue  name of initial residue.
22             Character     initChainID    Chain identifier of initial residue
                                            in strand.
23 - 26        Integer       initSeqNum     Sequence number of initial residue
                                            in strand.
27             AChar         initICode      Insertion code of initial residue
                                            in  strand.
29 - 31        Residue name  endResName     Residue name of terminal residue.
33             Character     endChainID     Chain identifier of terminal residue.
34 - 37        Integer       endSeqNum      Sequence number of terminal residue.
38             AChar         endICode       Insertion code of terminal residue.
39 - 40        Integer       sense          Sense of strand with respect to previous
                                            strand in the sheet. 0 if first strand,
                                            1 if  parallel,and -1 if anti-parallel.
42 - 45        Atom          curAtom        Registration.  Atom name in current strand.
46 - 48        Residue name  curResName     Registration.  Residue name in current strand
50             Character     curChainId     Registration. Chain identifier in
                                            current strand.
51 - 54        Integer       curResSeq      Registration.  Residue sequence number
                                            in current strand.
55             AChar         curICode       Registration. Insertion code in
                                            current strand.
57 - 60        Atom          prevAtom       Registration.  Atom name in previous strand.
61 - 63        Residue name  prevResName    Registration.  Residue name in
                                            previous strand.
65             Character     prevChainId    Registration.  Chain identifier in
                                            previous  strand.
66 - 69        Integer       prevResSeq     Registration. Residue sequence number
                                            in previous strand.
70             AChar         prevICode      Registration.  Insertion code in
                                            previous strand.
-}
sheet_se :: Num i => ([i],[i])
sheet_se =
  ([1, 8,12,15,18,22,23,27,29,33,34,38,39,42,46,50,51,55,57,61,65,66,70]
  ,[6,10,14,16,20,22,26,27,31,33,37,38,40,45,48,50,54,55,60,63,65,69,70])

{- | SSBOND

COLUMNS        DATA  TYPE     FIELD            DEFINITION
--------------------------------------------------------------------------------
 1 -  6        Record name    "SSBOND"
 8 - 10        Integer        serNum           Serial number.
12 - 14        LString(3)     "CYS"            Residue name.
16             Character      chainID1         Chain identifier.
18 - 21        Integer        seqNum1          Residue sequence number.
22             AChar          icode1           Insertion code.
26 - 28        LString(3)     "CYS"            Residue name.
30             Character      chainID2         Chain identifier.
32 - 35        Integer        seqNum2          Residue sequence number.
36             AChar          icode2           Insertion code.
60 - 65        SymOP          sym1             Symmetry operator for residue 1.
67 - 72        SymOP          sym2             Symmetry operator for residue 2.
74 – 78        Real(5.2)      Length           Disulfide bond distance
-}
ssbond_se :: Num i => ([i],[i])
ssbond_se = ([1, 8,12,16,18,22,26,30,32,36,60,67,74]
            ,[6,10,14,16,21,22,28,30,35,36,65,72,78])

{- | TER

COLUMNS        DATA  TYPE    FIELD           DEFINITION
-------------------------------------------------------------------------
 1 -  6        Record name   "TER   "
 7 - 11        Integer       serial          Serial number.
18 - 20        Residue name  resName         Residue name.
22             Character     chainID         Chain identifier.
23 - 26        Integer       resSeq          Residue sequence number.
27             AChar         iCode           Insertion code.
-}
ter_se :: Num i => ([i],[i])
ter_se = ([1,7,18,22,23,27],[6,11,20,22,26,27])

{- | TITLE

COLUMNS       DATA  TYPE     FIELD         DEFINITION
----------------------------------------------------------------------------------
 1 -  6       Record name    "TITLE "
 9 - 10       Continuation   continuation  Allows concatenation of multiple records.
11 - 80       String         title         Title of the  experiment.
-}
title_se :: Num i => ([i],[i])
title_se = ([1,9,11],[6,10,80])

-- * TXT

-- | Alias for CHAR8-BYTESTRING
type TXT = T.ByteString

-- | ZERO-INDEXED [(START,LENGTH)] -> [SUB-STR]
txt_parts :: TXT -> [(Int, Int)] -> [TXT]
txt_parts s ix = let f (i,j) = T.take j (T.drop i s) in map f ix

txt_parts_spl :: TXT -> [(Int, Int)] -> (TXT, [TXT])
txt_parts_spl = let spl x = (head x,tail x) in fmap spl . txt_parts

-- | Plain text string (ie. as written)
--
-- > txt_pln (txt " a b c ") == " a b c "
txt_pln :: TXT -> String
txt_pln = T.unpack

-- | Unpack and trim TXT.
--
-- > txt_str (txt " a b c ") == "a b c"
txt_str :: TXT -> String
txt_str = T.unpack . fst . T.spanEnd isSpace . T.dropWhile isSpace

-- | 'read' of 'txt_str'
txt_int :: TXT -> Int
txt_int = read . txt_str

-- | 'read' of 'txt_str'
txt_flt :: TXT -> Double
txt_flt = read . txt_str

-- | Unpack single element TXT.
txt_chr :: TXT -> Char
txt_chr x =
  case T.unpack x of
    [c] -> c
    _ -> error "txt_chr?"

-- | Is TXT nil (ie. empty or all whitespace)
txt_nil :: TXT -> Bool
txt_nil = T.all isSpace

-- | Readers for 'Char', 'String', 'Int' and 'Double'.
txt_readers :: [TXT] -> (Int -> Char, Int -> String, Int -> Int, Int -> Double)
txt_readers x = (txt_chr . (x !!),txt_str . (x !!),txt_int . (x !!),txt_flt . (x !!))

-- | Pack TXT.
txt :: String -> TXT
txt = T.pack

-- * REC

-- | (RECORD-TYPE,RECORD-FIELDS)
type REC = (TXT,[TXT])

-- | Record names are the initial six-characters, ie. "HET   " and "ATOM  " and "HETATM"
txt_rec_name :: TXT -> TXT
txt_rec_name = T.take 6

txt_rec_match :: TXT -> TXT -> Bool
txt_rec_match x = (==) x . txt_rec_name

-- * RECORD TABLE

-- | ONE-INDEXED (START,END) to ZERO-INDEXED (START,LENGTH)
se_to_ix :: Num i => ([i],[i]) -> [(i,i)]
se_to_ix (i,j) = zip (map (subtract 1) i) (map (+ 1) (zipWith (-) j i))

-- | Table of (RECORD-TYPE:STRING,INDICES:START-END).
pdb_rec_str_se :: [(String,([Int],[Int]))]
pdb_rec_str_se =
  [("ATOM  ",atom_se)
  ,("CISPEP",cispep_se)
  ,("CONECT",conect_se)
  ,("CRYST1",cryst1_se)
  ,("END   ",end_se)
  ,("ENDMDL",endmdl_se)
  ,("FORMUL",formul_se)
  ,("HEADER",header_se)
  ,("HELIX ",helix_se)
  ,("HET   ",het_se)
  ,("HETATM",atom_se)
  ,("LINK  ",link_se)
  ,("MASTER",master_se)
  ,("MDLTYP",mdltyp_se)
  ,("MODEL ",model_se)
  ,("MODRES",modres_se)
  ,("NUMMDL",nummdl_se)
  ,("OBSLTE",obslte_se)
  ,("REMARK",remark_se)
  ,("SEQRES",seqres_se)
  ,("SHEET ",sheet_se)
  ,("SSBOND",ssbond_se)
  ,("TER   ",ter_se)
  ,("TITLE ",title_se)
  ]

-- | Table of (RECORD-TYPE:TXT,INDICES:START-LENGTH)
pdb_rec_txt_ix :: [(TXT, [(Int, Int)])]
pdb_rec_txt_ix = let f (nm,se) = (txt nm,se_to_ix se) in map f pdb_rec_str_se

-- * UNPACK

atom_unpack :: REC -> ATOM
atom_unpack (r,x) =
  let (c,s,i,f) = txt_readers x
  in (r == txt "HETATM",i 0,s 1,c 2,(s 3,c 4,i 5,c 6),(f 7,f 8,f 9),s 12)

conect_unpack :: REC -> CONECT
conect_unpack (_,x) = zip (repeat (txt_int (x !! 0))) (map txt_int (filter (not . txt_nil) (tail x)))

cryst1_unpack :: REC -> CRYST1
cryst1_unpack (_,x) = let (_,s,i,f) = txt_readers x in ((f 0,f 1,f 2),(f 3,f 4,f 5),s 7,i 8)

header_unpack :: REC -> HEADER
header_unpack (_,x) = let s = txt_str . (x !!) in (s 0,s 1,s 2)

helix_unpack :: REC -> HELIX
helix_unpack (_,x) =
  let (c,s,i,_) = txt_readers x
  in ((i 0,s 1),(s 2,c 3,i 4,c 5),(s 6,c 7,i 8,c 9),i 10,i 12)

het_unpack :: REC -> HET
het_unpack (_,x) = let (c,s,i,_) = txt_readers x in ((s 0,c 1,i 2,c 3),i 4,s 5)

link_unpack :: REC -> LINK
link_unpack (_,x) =
  let (c,s,i,f) = txt_readers x
  in (s 0,c 1,(s 2,c 3,i 4,c 5),s 6,c 7,(s 8,c 9,i 10,c 11),i 12,i 13,f 14)

master_unpack :: REC -> MASTER
master_unpack (_,x) = let i = txt_int . (x !!) in (i 0,i 2,i 3,i 4,i 6,i 7,i 8,i 9,i 10,i 11)

mdltyp_unpack :: REC -> MDLTYP
mdltyp_unpack (_,x) = (txt_pln (x !! 0),txt_str (x !! 1))

-- | SERIAL
model_unpack :: REC -> Int
model_unpack (_,x) = txt_int (x !! 0)

modres_unpack :: REC -> MODRES
modres_unpack (_,x) = let (c,s,i,_) = txt_readers x in (s 0,(s 1,c 2,i 3,c 4),s 5,s 6)

nummdl_unpack :: REC -> Int
nummdl_unpack (_,x) = txt_int (x !! 0)

remark_unpack :: REC -> REMARK
remark_unpack (_,x) = (txt_int (x !! 0),txt_pln (x !! 1))

seqres_unpack :: REC -> SEQRES
seqres_unpack (_,x) = let (c,s,i,_) = txt_readers x in (i 0,c 1,i 2,map s [3 .. 15])

sheet_unpack :: REC -> SHEET
sheet_unpack (_,x) =
  let (c,s,i,_) = txt_readers x
  in (i 0,s 1,i 2,(s 3,c 4,i 5,c 6),(s 7,c 8,i 9,c 10))

ssbond_unpack :: REC -> SSBOND
ssbond_unpack (_,x) =
  let (c,s,i,f) = txt_readers x
      cys n = if s n /= "CYS" then error "ssbond_unpack?" else "CYS"
  in (i 0,(cys 1,c 2,i 3,c 4),(cys 5,c 6,i 7,c 8),i 9,i 10,f 11)

ter_unpack :: REC -> TER
ter_unpack (_,x) = let (c,s,i,_) = txt_readers x in (i 0,(s 1,c 2,i 3,c 4))

title_unpack :: REC -> TITLE
title_unpack (_,x) = (txt_pln (x !! 0),txt_str (x !! 1))

-- * PARSE

parse_txt_ix :: (TXT -> Maybe [(Int, Int)]) -> TXT -> Maybe REC
parse_txt_ix f s = fmap (txt_parts_spl s) (f (txt_rec_name s))

pdb_rec_parse :: TXT -> TXT -> Maybe REC
pdb_rec_parse nm =
  let ix = fromMaybe (error (show ("pdb_rec_parse",nm))) (lookup nm pdb_rec_txt_ix)
  in if T.length nm /= 6
     then error "pdb_rec_parse?"
     else parse_txt_ix (\z -> if z == nm then Just ix else Nothing)

pdb_rec_parse_set :: [TXT] -> TXT -> Maybe REC
pdb_rec_parse_set nm = parse_txt_ix (\z -> if z `elem` nm then lookup z pdb_rec_txt_ix else Nothing)

-- * DAT

-- | PDB DATA
type DAT = [TXT]

-- | Find first instance of /ty/ record.
pdb_dat_rec_1 :: TXT -> DAT -> Maybe REC
pdb_dat_rec_1 ty = join . fmap (pdb_rec_parse ty) . find (txt_rec_match ty)

-- | Collect all instances of /ty/ record.
pdb_dat_rec :: TXT -> DAT -> [REC]
pdb_dat_rec ty = mapMaybe (pdb_rec_parse ty)

-- | Collect all instances of /ty-set/ records.
pdb_dat_rec_set :: [TXT] -> DAT -> [REC]
pdb_dat_rec_set ty_set = mapMaybe (pdb_rec_parse_set ty_set)

-- * RECORDS

-- | ATOM and HETATM records
dat_atom_all :: DAT -> [ATOM]
dat_atom_all = map atom_unpack . pdb_dat_rec_set (map txt ["ATOM  ","HETATM"])

-- | (ATOM,HETATM)
dat_atom :: DAT -> ([ATOM],[ATOM])
dat_atom = partition (not . atom_het) . dat_atom_all

-- | ATOM
dat_atom__ :: DAT -> [ATOM]
dat_atom__ = map atom_unpack . pdb_dat_rec (txt "ATOM  ")

-- | HETATM
dat_hetatm :: DAT -> [ATOM]
dat_hetatm = map atom_unpack . pdb_dat_rec (txt "HETATM")

dat_conect :: DAT -> [CONECT]
dat_conect = map conect_unpack . pdb_dat_rec (txt "CONECT")

dat_cryst1 :: DAT -> CRYST1
dat_cryst1 = cryst1_unpack . fromMaybe (error "dat_cryst1?") . pdb_dat_rec_1 (txt "CRYST1")

dat_header :: DAT -> HEADER
dat_header = header_unpack . fromMaybe (error "dat_header?") . pdb_dat_rec_1 (txt "HEADER")

dat_helix :: DAT -> [HELIX]
dat_helix = map helix_unpack . pdb_dat_rec (txt "HELIX ")

dat_het :: DAT -> [HET]
dat_het = map het_unpack . pdb_dat_rec (txt "HET   ")

dat_link :: DAT -> [LINK]
dat_link = map link_unpack . pdb_dat_rec (txt "LINK  ")

dat_master :: DAT -> Maybe MASTER
dat_master = fmap master_unpack . pdb_dat_rec_1 (txt "MASTER")

dat_modres :: DAT -> [MODRES]
dat_modres = map modres_unpack . pdb_dat_rec (txt "MODRES")

dat_nummdl :: DAT -> Maybe Int
dat_nummdl = fmap nummdl_unpack . pdb_dat_rec_1 (txt "NUMMDL")

dat_remark :: DAT -> [REMARK]
dat_remark = map remark_unpack . pdb_dat_rec (txt "REMARK")

dat_seqres :: DAT -> [SEQRES]
dat_seqres = map seqres_unpack . pdb_dat_rec (txt "SEQRES")

dat_sheet :: DAT -> [SHEET]
dat_sheet = map sheet_unpack . pdb_dat_rec (txt "SHEET ")

dat_ssbond :: DAT -> [SSBOND]
dat_ssbond = map ssbond_unpack . pdb_dat_rec (txt "SSBOND")

dat_ter :: DAT -> [TER]
dat_ter = map ter_unpack . pdb_dat_rec (txt "TER   ")

dat_title :: DAT -> [TITLE]
dat_title = map title_unpack . pdb_dat_rec (txt "TITLE ")

-- * COMPOSITE

dat_parse :: DAT -> PDB
dat_parse x =
  (dat_header x
  ,title_group (dat_title x)
  ,dat_nummdl x
  ,dat_cryst1 x
  ,dat_atom x
  ,conect_group (dat_conect x)
  ,seqres_group (dat_seqres x)
  ,dat_helix x
  ,dat_sheet x
  ,dat_link x
  ,dat_ssbond x)

-- * IO

pdb_load_dat :: FilePath -> IO DAT
pdb_load_dat = fmap T.lines . T.readFile

pdb_load_dat_dir :: FilePath -> IO [DAT]
pdb_load_dat_dir dir = T.dir_subset [".pdb"] dir >>= mapM pdb_load_dat
