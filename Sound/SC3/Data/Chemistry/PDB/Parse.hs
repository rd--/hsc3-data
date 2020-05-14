-- | Minimal PDB parser.  <https://www.wwpdb.org/documentation/file-format-content/format33/v3.3.html>
module Sound.SC3.Data.Chemistry.PDB.Parse where

import Data.Char {- base -}
import Data.Maybe {- base -}

import qualified Data.ByteString.Char8 as T {- bytestring -}

-- * TXT

-- | Alias for U8-BYTESTRING
type TXT = T.ByteString

-- | ZERO-INDEXED [(START,LENGTH)] -> [SUB-STR]
txt_parts :: TXT -> [(Int, Int)] -> [TXT]
txt_parts s ix = let f (i,j) = T.take j (T.drop i s) in map f ix

-- | Unpack and trim TXT.
txt_str :: TXT -> String
txt_str = T.unpack . T.takeWhile (not . isSpace) . T.dropWhile isSpace

-- | 'read' of 'txt_str'
txt_int :: TXT -> Int
txt_int = read . txt_str

-- | Unpack single element TXT.
txt_char :: TXT -> Char
txt_char x =
  case T.unpack x of
    [c] -> c
    _ -> error "txt_char?"

-- | Pack TXT.
txt :: String -> TXT
txt = T.pack

-- * RECORDS

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

{- | TITLE

COLUMNS       DATA  TYPE     FIELD         DEFINITION
----------------------------------------------------------------------------------
 1 -  6       Record name    "TITLE "
 9 - 10       Continuation   continuation  Allows concatenation of multiple records.
11 - 80       String         title         Title of the  experiment.
-}
title_se :: Num i => ([i],[i])
title_se = ([1,9,11],[6,10,80])

-- * RECORD TABLE

-- | ONE-INDEXED (START,END) to ZERO-INDEXED (START,LENGTH)
se_to_ix :: Num i => ([i],[i]) -> [(i,i)]
se_to_ix (i,j) = zip (map (subtract 1) i) (map (+ 1) (zipWith (-) j i))

-- | Table of (RECORD-TYPE:STRING,INDICES:START-END).
pdb_rec_str_se :: [(String,([Int],[Int]))]
pdb_rec_str_se =
  [("ATOM  ",atom_se)
  ,("END   ",end_se)
  ,("ENDMDL",endmdl_se)
  ,("FORMUL",formul_se)
  ,("HEADER",header_se)
  ,("HELIX ",helix_se)
  ,("HETATM",atom_se)
  ,("MASTER",master_se)
  ,("MDLTYP",mdltyp_se)
  ,("MODEL ",model_se)
  ,("MODRES",modres_se)
  ,("NUMMDL",nummdl_se)
  ,("OBSLTE",obslte_se)
  ,("SEQRES",seqres_se)
  ,("SHEET ",sheet_se)
  ,("TITLE ",title_se)
  ]

-- | Table of (RECORD-TYPE:TXT,INDICES:START-LENGTH)
pdb_rec_txt_ix :: [(TXT, [(Int, Int)])]
pdb_rec_txt_ix = let f (nm,se) = (txt nm,se_to_ix se) in map f pdb_rec_str_se

-- * PARSE

-- | (RECORD-TYPE,RECORD-FIELDS)
type REC = (TXT,[TXT])

txt_rec_name :: TXT -> TXT
txt_rec_name = T.take 6

parse_txt_ix :: (TXT -> Maybe [(Int, Int)]) -> TXT -> Maybe REC
parse_txt_ix f s = let spl x = (head x,tail x) in fmap (spl . txt_parts s) (f (txt_rec_name s))

pdb_rec_parse :: TXT -> TXT -> Maybe REC
pdb_rec_parse nm =
  let ix = fromMaybe (error "pdb_rec_parse") (lookup nm pdb_rec_txt_ix)
  in parse_txt_ix (\z -> if z == nm then Just ix else Nothing)

pdb_rec_parse_set :: [TXT] -> TXT -> Maybe REC
pdb_rec_parse_set nm = parse_txt_ix (\z -> if z `elem` nm then lookup z pdb_rec_txt_ix else Nothing)

pdb_dat_rec :: TXT -> [TXT] -> [REC]
pdb_dat_rec ty = mapMaybe (pdb_rec_parse ty)

pdb_dat_rec_set :: [TXT] -> [TXT] -> [REC]
pdb_dat_rec_set ty = mapMaybe (pdb_rec_parse_set ty)

-- * IO

pdb_load_dat :: FilePath -> IO [TXT]
pdb_load_dat = fmap T.lines . T.readFile

-- * SPECIFIC

atom_coord :: REC -> (Double,Double,Double)
atom_coord (_,x) = let f i = read (txt_str (x !! i)) in (f 7,f 8,f 9)

atom_element :: REC -> String
atom_element (_,x) = txt_str (x !! 12)

helix_unpack :: REC -> ((Int,String),(String,Char,Int,Char),(String,Char,Int,Char),Int,Int)
helix_unpack (_,r) =
  case r of
    [h_k,h_id,r1_nm,r1_ch,r1_id,r1_ins,r2_nm,r2_ch,r2_id,r2_ins,h_cl,_,h_n] ->
      ((txt_int h_k,txt_str h_id)
      ,(txt_str r1_nm,txt_char r1_ch,txt_int r1_id,txt_char r1_ins)
      ,(txt_str r2_nm,txt_char r2_ch,txt_int r2_id,txt_char r2_ins)
      ,txt_int h_cl
      ,txt_int h_n)
    _ -> error "helix_unpack?"

nummdl_n :: REC -> Int
nummdl_n r =
  case r of
    (_,[i]) -> read (txt_str i)
    _ -> error "nummdl_n"

model_serial :: REC -> Int
model_serial r =
  case r of
    (_,[i]) -> read (txt_str i)
    _ -> error "model_serial"

-- | (resName,stdRes) fields of MODRES record.
modres_names :: REC -> (String,String)
modres_names (_,x) = (txt_str (x !! 1),txt_str (x !! 5))

-- * DAT-SPECIFIC

dat_nummdl :: [TXT] -> Maybe Int
dat_nummdl d =
  case pdb_dat_rec (txt "NUMMDL") d of
    [] -> Nothing
    [r] -> Just (nummdl_n r)
    _ -> error "dat_nummdl"
