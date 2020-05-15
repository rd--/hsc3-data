-- | Minimal PDB parser.  <https://www.wwpdb.org/documentation/file-format-content/format33/v3.3.html>
module Sound.SC3.Data.Chemistry.PDB.Parse where

import Data.Char {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Data.ByteString.Char8 as T {- bytestring -}

import qualified Music.Theory.List as T {- hmt -}

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

txt_readers :: [TXT] -> (Int -> Char, Int -> String, Int -> Int, Int -> Double)
txt_readers x = (txt_chr . (x !!),txt_str . (x !!),txt_int . (x !!),txt_flt . (x !!))

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

{- | CONECT

COLUMNS       DATA  TYPE      FIELD        DEFINITION
-------------------------------------------------------------------------
 1 -  6        Record name    "CONECT"
 7 - 11       Integer        serial       Atom  serial number
12 - 16        Integer        serial       Serial number of bonded atom
17 - 21        Integer        serial       Serial  number of bonded atom
22 - 26        Integer        serial       Serial number of bonded atom
27 - 31        Integer        serial       Serial number of bonded atom
-}
conect_se :: Num i => ([i],[i])
conect_se = ([1,7,12,17,22,27],[6,11,16,21,26,31])

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

-- * RECORD TABLE

-- | ONE-INDEXED (START,END) to ZERO-INDEXED (START,LENGTH)
se_to_ix :: Num i => ([i],[i]) -> [(i,i)]
se_to_ix (i,j) = zip (map (subtract 1) i) (map (+ 1) (zipWith (-) j i))

-- | Table of (RECORD-TYPE:STRING,INDICES:START-END).
pdb_rec_str_se :: [(String,([Int],[Int]))]
pdb_rec_str_se =
  [("ATOM  ",atom_se)
  ,("CONECT",conect_se)
  ,("END   ",end_se)
  ,("ENDMDL",endmdl_se)
  ,("FORMUL",formul_se)
  ,("HEADER",header_se)
  ,("HELIX ",helix_se)
  ,("HET   ",het_se)
  ,("HETATM",atom_se)
  ,("MASTER",master_se)
  ,("MDLTYP",mdltyp_se)
  ,("MODEL ",model_se)
  ,("MODRES",modres_se)
  ,("NUMMDL",nummdl_se)
  ,("OBSLTE",obslte_se)
  ,("SEQRES",seqres_se)
  ,("SHEET ",sheet_se)
  ,("TER ",ter_se)
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
  in if T.length nm /= 6
     then error "pdb_rec_parse?"
     else parse_txt_ix (\z -> if z == nm then Just ix else Nothing)

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

-- | (NAME,CHAIN,SEQNO,INSCODE)
type RESIDUE_ID = (String,Char,Int,Char)

residue_id_name :: RESIDUE_ID -> String
residue_id_name (nm,_,_,_) = nm

residue_id_chain :: RESIDUE_ID -> Char
residue_id_chain (_,ch,_,_) = ch

residue_id_seqno_inscode :: RESIDUE_ID -> (Int,Char)
residue_id_seqno_inscode (_,_,k,i) = (k,i)

-- | Give (R1,R3) is R2 in range (inclusive).
residue_id_in_range :: (RESIDUE_ID,RESIDUE_ID) -> RESIDUE_ID -> Bool
residue_id_in_range ((_,c1,k1,i1),(_,c3,k3,i3)) (_,c2,k2,i2) =
  if c1 /= c3 || (k1,i1) >= (k3,i3)
  then error "residue_id_in_range?"
  else if c2 /= c1 then False else (k1,i1) <= (k2,i2) && (k2,i2) <= (k3,i3)

-- | (HETATM,SERIAL,NAME,ALT-LOC,RESIDUE-ID,COORDINATE,ELEMENT)
type ATOM = (Bool,Int,String,Char,RESIDUE_ID,(Double,Double,Double),String)

atom_het :: ATOM -> Bool
atom_het (h,_,_,_,_,_,_) = h

atom_unpack :: REC -> ATOM
atom_unpack (r,x) =
  let (c,s,i,f) = txt_readers x
  in (r == txt "HETATM",i 0,s 1,c 2,(s 3,c 4,i 5,c 6),(f 7,f 8,f 9),s 12)

atom_serial :: ATOM -> Int
atom_serial (_,k,_,_,_,_,_) = k

atom_name :: ATOM -> String
atom_name (_,_,nm,_,_,_,_) = nm

atom_altloc :: ATOM -> Char
atom_altloc (_,_,_,alt,_,_,_) = alt

atom_sel_altloc_A :: ATOM -> Bool
atom_sel_altloc_A = flip elem " A" . atom_altloc

atom_residue_id :: ATOM -> RESIDUE_ID
atom_residue_id (_,_,_,_,r,_,_) = r

atom_chain_id :: ATOM -> Char
atom_chain_id = residue_id_chain . atom_residue_id

atom_coord :: ATOM -> (Double,Double,Double)
atom_coord (_,_,_,_,_,c,_) = c

atom_element :: ATOM -> String
atom_element (_,_,_,_,_,_,e) = e

atom_element_or_name :: ATOM -> String
atom_element_or_name (_,_,nm,_,_,_,el) = if null el then nm else el

type CONECT = [(Int,Int)]

conect_unpack :: REC -> CONECT
conect_unpack (_,x) = zip (repeat (txt_int (x !! 0))) (map txt_int (filter (not . txt_nil) (tail x)))

-- | (CLASSIFICATION,DEP-DATE,ID-CODE)
type HEADER = (String,String,String)

header_unpack :: REC -> HEADER
header_unpack (_,x) = let s = txt_str . (x !!) in (s 0,s 1,s 2)

-- | ((SERIAL,ID),INIT-RESIDUE,END-RESIDUE,CLASS,LENGTH)
type HELIX = ((Int,String),RESIDUE_ID,RESIDUE_ID,Int,Int)

helix_serial :: HELIX -> Int
helix_serial ((k,_),_,_,_,_) = k

helix_chain_id :: HELIX -> Char
helix_chain_id (_,(_,c1,_,_),(_,c2,_,_),_,_) = if c1 /= c2 then error "helix_chain_id?" else c1

helix_unpack :: REC -> HELIX
helix_unpack (_,x) =
  let (c,s,i,_) = txt_readers x
  in ((i 0,s 1),(s 2,c 3,i 4,c 5),(s 6,c 7,i 8,c 9),i 10,i 12)

type HET = (RESIDUE_ID,Int,String)

het_id :: HET -> String
het_id ((x,_,_,_),_,_) = x

het_chain :: HET -> Char
het_chain ((_,x,_,_),_,_) = x

het_n_atom :: HET -> Int
het_n_atom (_,x,_) = x

het_unpack :: REC -> HET
het_unpack (_,x) = let (c,s,i,_) = txt_readers x in ((s 0,c 1,i 2,c 3),i 4,s 5)

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

-- | (ID,RESIDUE-ID,STD-RES,COMMENT)
type MODRES = (String,RESIDUE_ID,String,String)

modres_unpack :: REC -> MODRES
modres_unpack (_,x) = let (c,s,i,_) = txt_readers x in (s 0,(s 1,c 2,i 3,c 4),s 5,s 6)

-- | (resName,stdRes) fields of MODRES record.
modres_names :: MODRES -> (String,String)
modres_names (_,(r1,_,_,_),r2,_) = (r1,r2)

-- | (STRAND,ID,NUM-STRANDS,INIT-RESIDUE,END-RESIDUE)
type SHEET = (Int,String,Int,RESIDUE_ID,RESIDUE_ID)

sheet_unpack :: REC -> SHEET
sheet_unpack (_,x) =
  let (c,s,i,_) = txt_readers x
  in (i 0,s 1,i 2,(s 3,c 4,i 5,c 6),(s 7,c 8,i 9,c 10))

sheet_chain_id :: SHEET -> Char
sheet_chain_id (_,_,_,(_,c1,_,_),(_,c2,_,_)) = if c1 /= c2 then error "sheet_chain_id?" else c1

-- | (SERIAL,RESIDUE-ID)
type TER = (Int,RESIDUE_ID)

ter_unpack :: REC -> TER
ter_unpack (_,x) = let (c,s,i,_) = txt_readers x in (i 0,(s 1,c 2,i 3,c 4))

-- * DAT-SPECIFIC

dat_atom :: [TXT] -> [ATOM]
dat_atom = map atom_unpack . pdb_dat_rec_set (map txt ["ATOM  ","HETATM"])

dat_conect :: [TXT] -> [CONECT]
dat_conect = map conect_unpack . pdb_dat_rec (txt "CONECT")

dat_header :: [TXT] -> [HEADER]
dat_header = map header_unpack . pdb_dat_rec (txt "HEADER")

dat_helix :: [TXT] -> [HELIX]
dat_helix = map helix_unpack . pdb_dat_rec (txt "HELIX ")

dat_het :: [TXT] -> [HET]
dat_het = map het_unpack . pdb_dat_rec (txt "HET   ")

dat_modres :: [TXT] -> [MODRES]
dat_modres = map modres_unpack . pdb_dat_rec (txt "MODRES")

dat_nummdl :: [TXT] -> Maybe Int
dat_nummdl d =
  case pdb_dat_rec (txt "NUMMDL") d of
    [] -> Nothing
    [r] -> Just (nummdl_n r)
    _ -> error "dat_nummdl"

dat_sheet :: [TXT] -> [SHEET]
dat_sheet = map sheet_unpack . pdb_dat_rec (txt "SHEET ")

dat_ter :: [TXT] -> [TER]
dat_ter = map ter_unpack . pdb_dat_rec (txt "TER   ")

-- * GROUP

-- | Group atoms by chain and ensure sequence
atom_group :: [ATOM] -> [(Char,[ATOM])]
atom_group = map (fmap (sortOn atom_serial)) . T.collate_on atom_chain_id id

-- | Group helices by chain and ensure sequence
helix_group :: [HELIX] -> [(Char,[HELIX])]
helix_group = map (fmap (sortOn helix_serial)) . T.collate_on helix_chain_id id

-- | Group helices by chain
sheet_group :: [SHEET] -> [(Char,[SHEET])]
sheet_group = T.collate_on sheet_chain_id id
