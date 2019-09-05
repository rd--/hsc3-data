{- | SFZ <http://www.sfzformat.com/legacy/>

<control>
default_path : string : directory-name

<global> | <group> | <region>
volume : float : db : 0 : -144 6
pan : float : linear : 0 : -100 100
sample : string : file-name
key|lokey|hikey|pitch_keycenter : int|string : midi-note-number|iso-pitch-name : 0|127|60 : 0 127
lochan|hichan : int : channel-number : 1|16 : 1 16
tune : int : cents : 0 : -100 100
transpose : int : linear : 0 : -127 127
loop_mode : string : no_loop one_shot loop_continuous loop_sustain
loop_start : int : frame-number : 0 : 0 2^32
loop_end : int : frame-number : 0 : 0 2^32
ampeg_attack : float : seconds : 0 : 0 100
ampeg_decay : float : seconds : 0 : 0 100
ampeg_sustain : float : % : 100 : 0 100
ampeg_release : float : seconds : 0 : 0 100

-}
module Sound.SC3.Data.SFZ where

import Data.Int {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import Data.Word {- base -}
import System.FilePath {- filepath -}

import qualified Data.List.Split as Split {- split -}

import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Pitch as T {- hmt -}
import qualified Music.Theory.Read as T {- hmt -}

import qualified Sound.File.HSndFile as SF {- hsc3-sf-hsndfile -}

-- * TYPES

-- | An opcode is a (key,value) pair.
type SFZ_Opcode = (String,String)

-- | A section is a <header> and a set of opcodes.
type SFZ_Section = (String,[SFZ_Opcode])

-- | The <control> header defines a set of opcodes.
type SFZ_Control = [SFZ_Opcode]

-- | The <global> header defines a set of opcodes.
type SFZ_Global = [SFZ_Opcode]

-- | The <group> header defines a set of opcodes.
type SFZ_Group = [SFZ_Opcode]

-- | The <region> header defines a set of opcodes, and has salient <global> and <group> opcodes.
type SFZ_Region = ([SFZ_Opcode],[SFZ_Opcode])

-- | (control,global,[region])
type SFZ_Data = (SFZ_Control,SFZ_Global,[SFZ_Region])

-- | Does a set of opcodes contain given key?
sfz_region_has_opcode :: String -> SFZ_Region -> Bool
sfz_region_has_opcode k (g,c) = any ((== k) . fst) (g ++ c)

-- | Does a set of opcodes contain any of a given set of key?
sfz_region_has_opcode_in :: [String] -> SFZ_Region -> Bool
sfz_region_has_opcode_in k (g,c) = any ((`elem` k) . fst) (g ++ c)

-- | Delete any opcode with given key.
sfz_opcode_delete :: String -> [SFZ_Opcode] -> [SFZ_Opcode]
sfz_opcode_delete k c = filter ((/= k) . fst) c

-- * PARSE

-- | Lines starting with / are comments.
sfz_is_comment :: String -> Bool
sfz_is_comment ln =
  case ln of
    '/':_ -> True
    _ -> False

-- | Headers are in angle brackets, ie. <group>.
sfz_is_header :: String -> Bool
sfz_is_header s = not (null s) && head s == '<' && last s == '>'

-- | SFZ tokenizer, white space is allowed in the right hand sides of opcodes, ie. in file-names.
--
-- > sfz_tokenize "<region> sample=a.wav <region> sample=b c.wav"
sfz_tokenize :: String -> [String]
sfz_tokenize =
  let recur l = case l of
                  x1:x2:r ->
                    if not (sfz_is_header x2) && '=' `notElem` x2
                    then recur ((unwords [x1,x2]) : r)
                    else x1 : recur (x2 : r)
                  _ -> l
  in recur . words -- INCORRECT IMPLEMENTATION, WORKS FOR NON-CONSECUTIVE SPACES ONLY...

-- | Pitch values, ie. for pitch_keycenter, may be either numbers or strings.
--   Returned as midi-note numbers (ie. 0 - 127)
--
-- > map sfz_parse_pitch ["B3","60","C#4"] == [59,60,61]
sfz_parse_pitch :: String -> Word8
sfz_parse_pitch s =
  case T.read_maybe s of
    Just n -> n
    _ -> T.pitch_to_midi (T.parse_iso_pitch_err s)

-- | An opcode is written key=value.
--
-- > sfz_parse_opcode "pitch_keycenter=C4"
sfz_parse_opcode :: String -> SFZ_Opcode
sfz_parse_opcode s =
  case break (== '=') s of
    (k,'=':v) -> (k,v)
    _ -> error "sfz_parse_opcode?"

-- | Group tokens into sections.
sfz_tokens_group :: [String] -> [SFZ_Section]
sfz_tokens_group =
  map (\(h:c) -> (h,map sfz_parse_opcode c)) .
  filter (not . null) .
  (Split.split . Split.keepDelimsL . Split.whenElt) sfz_is_header

-- | Collate grouped token sequences.
--   <region>s have salient <global> and <group> opcodes.
--   <group> opcodes are reset at each <group> element.
sfz_collate :: SFZ_Global -> [SFZ_Section] -> [SFZ_Region]
sfz_collate gl =
  let recur gr sc =
        case sc of
          [] -> []
          ("<group>",op):sc' -> recur op sc'
          ("<region>",op):sc' -> (gr ++ gl,op) : recur gr sc'
          _ -> error "sfz_collate?"
  in recur []

-- | Collect <control> and <global> opcodes, and collate <region>s.
sfz_get_data :: [SFZ_Section] -> SFZ_Data
sfz_get_data gr =
  let (lhs,rhs) = partition ((`elem` ["<control>","<global>"]) . fst) gr
  in case lhs of
    [] -> ([],[],sfz_collate [] rhs)
    [("<control>",c)] -> (c,[],sfz_collate [] rhs)
    [("<control>",c),("<global>",g)] -> (c,g,sfz_collate g rhs)
    _ -> error "sfz_get_data?"

-- | Check that if region has a key opcode it doesn't have any of the opcodes that implicitly defines.
sfz_region_key_validate :: SFZ_Region -> Bool
sfz_region_key_validate r =
  not (sfz_region_has_opcode "key" r &&
       sfz_region_has_opcode_in ["pitch_keycenter","lokey","hikey"] r)

-- * IO

-- | Read a file, remove comments, parse into sections.
sfz_load_sections :: FilePath -> IO [SFZ_Section]
sfz_load_sections fn = do
  s <- readFile fn
  let l = filter (not . sfz_is_comment) (lines s)
  return (sfz_tokens_group (concatMap sfz_tokenize l))

-- | 'sfz_get_data' of 'sfz_load_sections'
sfz_load_data :: FilePath -> IO SFZ_Data
sfz_load_data = fmap sfz_get_data . sfz_load_sections

-- * LOOKUP

-- | Lookup in region opcodes, then in group if not located.
sfz_region_lookup :: SFZ_Region -> String -> Maybe String
sfz_region_lookup (gr,c) k =
  case lookup k c of
    Just r -> Just r
    Nothing -> lookup k gr

-- | Erroring variant.
sfz_region_lookup_err :: SFZ_Region -> String -> String
sfz_region_lookup_err r = fromMaybe (error "sfz_region_lookup?") . sfz_region_lookup r

-- | Lookup with default value and parser.
sfz_region_lookup_f :: t -> (String -> t) -> SFZ_Region -> String -> t
sfz_region_lookup_f z f r = maybe z f . sfz_region_lookup r

-- | Lookup with default value and read instance.
sfz_region_lookup_read :: Read t => t -> SFZ_Region -> String -> t
sfz_region_lookup_read z = sfz_region_lookup_f z read

-- * NAMED

sfz_region_volume :: SFZ_Region -> Double
sfz_region_volume r = sfz_region_lookup_read 0 r "volume"

sfz_region_pan :: SFZ_Region -> Double
sfz_region_pan r = sfz_region_lookup_read 0 r "pan"

sfz_region_sample :: SFZ_Region -> FilePath
sfz_region_sample r = sfz_region_lookup_err r "sample"

sfz_region_pitch_keycenter :: SFZ_Region -> Word8
sfz_region_pitch_keycenter r = sfz_region_lookup_f 60 sfz_parse_pitch r "pitch_keycenter"

sfz_region_lokey :: SFZ_Region -> Word8
sfz_region_lokey r = sfz_region_lookup_f 0 sfz_parse_pitch r "lokey"

sfz_region_hikey :: SFZ_Region -> Word8
sfz_region_hikey r = sfz_region_lookup_f 127 sfz_parse_pitch r "hikey"

-- | If opcode @key@ is give, it defines the triple (pitch_keycenter,lokey,hikey).
--   Else read these individually.
sfz_region_key :: SFZ_Region -> (Word8,Word8,Word8)
sfz_region_key r =
  case sfz_region_lookup r "key" of
    Just x -> let n = sfz_parse_pitch x in (n,n,n)
    Nothing -> (sfz_region_pitch_keycenter r,sfz_region_lokey r,sfz_region_hikey r)

sfz_region_tune :: SFZ_Region -> Int8
sfz_region_tune r = sfz_region_lookup_read 0 r "tune"

sfz_region_lochan :: SFZ_Region -> Word8
sfz_region_lochan r = sfz_region_lookup_read 1 r "lochan"

sfz_region_hichan :: SFZ_Region -> Word8
sfz_region_hichan r = sfz_region_lookup_read 16 r "hichan"

sfz_region_lovel :: SFZ_Region -> Word8
sfz_region_lovel r = sfz_region_lookup_read 0 r "lovel"

sfz_region_hivel :: SFZ_Region -> Word8
sfz_region_hivel r = sfz_region_lookup_read 127 r "hivel"

sfz_region_loop_mode :: SFZ_Region -> Maybe String
sfz_region_loop_mode r = sfz_region_lookup r "loop_mode"

sfz_loop_mode_sym_tbl :: [(String, Char)]
sfz_loop_mode_sym_tbl = [("no_loop",'N'),("one_shot",'O'),("loop_continuous",'C'),("loop_sustain",'S')]

sfz_loop_mode_sym :: String -> Char
sfz_loop_mode_sym = flip T.lookup_err sfz_loop_mode_sym_tbl

sfz_region_loop_mode_sym :: SFZ_Region -> Maybe Char
sfz_region_loop_mode_sym = fmap sfz_loop_mode_sym . sfz_region_loop_mode

sfz_region_loop_start :: SFZ_Region -> Int
sfz_region_loop_start r = sfz_region_lookup_read 0 r "loop_start"

sfz_region_loop_end :: SFZ_Region -> Int
sfz_region_loop_end r = sfz_region_lookup_read 0 r "loop_end"

{- | If loop start and end points are defined,
     then return them with mode (defaulting to loop_continuous),
     else return Nothing and mode (defaulting to no_loop).
     Does not read loop data from sample file.
-}
sfz_region_loop_data :: SFZ_Region -> (String,Maybe (Int,Int))
sfz_region_loop_data r =
  case (sfz_region_lookup r "loop_start",sfz_region_lookup r "loop_end") of
    (Just st,Just en) -> (sfz_region_lookup_f "loop_continuous" id r "loop_mode"
                         ,Just (read st,read en))
    _ -> (sfz_region_lookup_f "no_loop" id r "loop_mode",Nothing)

sfz_region_ampeg_attack :: SFZ_Region -> Double
sfz_region_ampeg_attack r = sfz_region_lookup_read 0 r "ampeg_attack"

sfz_region_ampeg_decay :: SFZ_Region -> Double
sfz_region_ampeg_decay r = sfz_region_lookup_read 0 r "ampeg_decay"

sfz_region_ampeg_sustain :: SFZ_Region -> Double
sfz_region_ampeg_sustain r = sfz_region_lookup_read 100 r "ampeg_sustain"

sfz_region_ampeg_release :: SFZ_Region -> Double
sfz_region_ampeg_release r = sfz_region_lookup_read 0 r "ampeg_release"

sfz_region_ampeg_adsr :: SFZ_Region -> (Double, Double, Double, Double)
sfz_region_ampeg_adsr r =
  (sfz_region_ampeg_attack r,sfz_region_ampeg_decay r
  ,sfz_region_ampeg_sustain r,sfz_region_ampeg_release r)

-- * QUERY

-- | Resolve sample file-name of <region>.
--   Requires SFZ file name (for directory) and <control> data for default_path.
--
-- > sfz_region_sample_resolve "x/x.sfz" [] ([],[("sample","y.z")]) == "x/y.z"
-- > sfz_region_sample_resolve "x.sfz" [("default_path","x")] ([],[("sample","y.z")]) == "./x/y.z"
--
-- > "x" </> "" </> "y.z" == "x/y.z"
-- > splitFileName "x.sfz" == ("./","x.sfz")
sfz_region_sample_resolve :: FilePath -> SFZ_Control -> SFZ_Region -> FilePath
sfz_region_sample_resolve sfz_fn ctl rgn =
  let (dir,_) = splitFileName sfz_fn
      path = dir </> fromMaybe "" (lookup "default_path" ctl)
  in path </> sfz_region_sample rgn

-- | Get number-of-channels of sample of region, requires reading SF header.
sfz_region_get_nc :: FilePath -> SFZ_Control -> SFZ_Region -> IO Int
sfz_region_get_nc sfz_fn ctl rgn = do
  hdr <- SF.sf_header (sfz_region_sample_resolve sfz_fn ctl rgn)
  return (SF.channelCount hdr)

-- | Run 'sfz_region_get_nc' at each region in sequence.
sfz_data_get_nc :: FilePath -> SFZ_Data -> IO [Int]
sfz_data_get_nc sfz_fn (ctl,_,rgn) = mapM (sfz_region_get_nc sfz_fn ctl) rgn

-- * PP

-- | Print section, nl=new-line
sfz_section_pp :: Bool -> SFZ_Section -> String
sfz_section_pp nl (hdr,op) =
  let tk = hdr : map (\(k,v) -> concat [k,"=",v]) op
  in (if nl then unlines else unwords) tk

-- | Write sections to file.
sfz_write_sections :: Bool -> FilePath -> [SFZ_Section] -> IO ()
sfz_write_sections nl fn sc = writeFile fn (unlines (map (sfz_section_pp nl) sc))

{-

fn = "/home/rohan/rd/j/2019-04-21/FAIRLIGHT/IIX/PLUCKED/koto.sfz"
sc:_ <- sfz_load_sections fn
putStrLn $ sfz_section_pp True sc
(_,_,r:_) <- sfz_load_data fn
map (sfz_region_lookup r) ["sample","volume","pan"]
sfz_region_sample r
sfz_region_volume r
sfz_region_pan r
sfz_region_key r
sfz_region_tune r
sfz_region_loop_mode r
sfz_region_loop_start r
sfz_region_loop_end r
sfz_region_loop_data r
sfz_region_ampeg_attack r
sfz_region_ampeg_release r

fn = "/home/rohan/data/audio/instr/casacota/zell_1737_415_MeanTone5/8_i.sfz"
(_,_,r) <- sfz_load_data fn
length r == 51
map sfz_region_sample r
map sfz_region_key r
map sfz_region_ampeg_attack r
map sfz_region_ampeg_release r

-}
