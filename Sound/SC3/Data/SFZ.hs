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

-- | An opcode is a (key,value) pair.
type SFZ_Opcode = (String,String)

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

-- | Read a file, remove comments, parse into tokens.
sfz_load_tokens :: FilePath -> IO [String]
sfz_load_tokens fn = do
  s <- readFile fn
  let l = filter (not . sfz_is_comment) (lines s)
  return (concatMap sfz_tokenize l)

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
sfz_tokens_group :: [String] -> [[String]]
sfz_tokens_group =
  filter (not . null) .
  (Split.split . Split.keepDelimsL . Split.whenElt) sfz_is_header

-- | Collate grouped token sequences.
--   <region>s have salient <global> and <group> opcodes, which are reset at each <group> element.
sfz_collate :: SFZ_Global -> [[String]] -> [SFZ_Region]
sfz_collate gl =
  let recur gr tk =
        case tk of
          [] -> []
          ("<group>":c):tk' -> recur (map sfz_parse_opcode c) tk'
          ("<region>":c):tk' -> (gr ++ gl,map sfz_parse_opcode c) : recur gr tk'
          _ -> error "sfz_collate?"
  in recur []

-- | Collect <control> and <global> opcodes, and collate <region>s.
sfz_get_data :: [[String]] -> SFZ_Data
sfz_get_data gr =
  let (lhs,rhs) = partition ((`elem` ["<control>","<global>"]) . head) gr
  in case lhs of
    [] -> ([],[],sfz_collate [] rhs)
    ["<control>":c] -> (map sfz_parse_opcode c,[],sfz_collate [] rhs)
    ["<control>":c,"<global>":g] ->
      let gl = map sfz_parse_opcode g
      in (map sfz_parse_opcode c,gl,sfz_collate gl rhs)
    _ -> error "sfz_get_meta?"

-- | Load tokens, group and collate into (<control>,[<region>])
sfz_load :: FilePath -> IO SFZ_Data
sfz_load fn = do
  tk <- sfz_load_tokens fn
  return (sfz_get_data (sfz_tokens_group tk))

-- * RW

-- | Add implict op-codes, ie. if region has @key@ opcode.
sfz_region_key_rewrite :: SFZ_Region -> SFZ_Region
sfz_region_key_rewrite (g,c) =
  case lookup "key" c of
    Nothing -> (g,c)
    Just r -> (g,("pitch_keycenter",r):("lokey",r):("hikey",r):c)

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

sfz_region_key :: SFZ_Region -> Maybe Word8
sfz_region_key r = fmap sfz_parse_pitch (sfz_region_lookup r "key")

sfz_region_pitch_keycenter :: SFZ_Region -> Word8
sfz_region_pitch_keycenter r = sfz_region_lookup_f 60 sfz_parse_pitch r "pitch_keycenter"

sfz_region_lokey :: SFZ_Region -> Word8
sfz_region_lokey r = sfz_region_lookup_f 0 sfz_parse_pitch r "lokey"

sfz_region_hikey :: SFZ_Region -> Word8
sfz_region_hikey r = sfz_region_lookup_f 127 sfz_parse_pitch r "hikey"

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

sfz_region_ampeg_attack :: SFZ_Region -> Double
sfz_region_ampeg_attack r = sfz_region_lookup_read 0 r "ampeg_attack"

sfz_region_ampeg_release :: SFZ_Region -> Double
sfz_region_ampeg_release r = sfz_region_lookup_read 0 r "ampeg_release"

-- * QUERY

-- | Resolve sample file-name of <region>
sfz_resolve :: FilePath -> SFZ_Control -> SFZ_Region -> FilePath
sfz_resolve fn ctl rgn =
  let (dir,_) = splitFileName fn
      path = dir </> fromMaybe "" (lookup "default_path" ctl)
  in path </> sfz_region_sample rgn

-- | Get number-of-channels of sample of region.
sfz_get_nc :: FilePath -> SFZ_Control -> SFZ_Region -> IO Int
sfz_get_nc fn ctl rgn = do
  hdr <- SF.sf_header (sfz_resolve fn ctl rgn)
  return (SF.channelCount hdr)

{-

fn = "/home/rohan/rd/j/2019-04-21/FAIRLIGHT/IIX/PLUCKED/koto.sfz"
r:_ <- sfz_load_regions fn
map (sfz_region_lookup r) ["sample","volume","pan"]
sfz_region_sample r
sfz_region_volume r
sfz_region_pan r
sfz_region_pitch_keycenter r
sfz_region_tune r
sfz_region_loop_mode r
sfz_region_loop_start r
sfz_region_loop_end r
sfz_region_ampeg_attack r
sfz_region_ampeg_release r

fn = "/home/rohan/data/audio/instr/casacota/zell_1737_415_MeanTone5/8_i.sfz"
(c,_,r') <- sfz_load fn
length c == 1
r = map sfz_region_key_rewrite r'
length r == 51
map sfz_region_sample r
map sfz_region_pitch_keycenter r
map sfz_region_lokey r
map sfz_region_hikey r
map sfz_region_ampeg_attack r
map sfz_region_ampeg_release r

-}
