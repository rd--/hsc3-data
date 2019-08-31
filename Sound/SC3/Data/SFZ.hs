{- | SFZ

<http://www.sfzformat.com/legacy/>

<group>
<region>
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

import Data.Maybe {- base -}

import qualified Data.List.Split as Split {- split -}

import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Pitch as T {- hmt -}
import qualified Music.Theory.Read as T {- hmt -}

-- | An Opcode is a (key,value) pair.
type SFZ_Opcode = (String,String)

-- | A group is a set of opcodes.
type SFZ_Group = [SFZ_Opcode]

-- | A region is a salient group, perhaps empty, and a set of opcodes.
type SFZ_Region = (SFZ_Group,[SFZ_Opcode])

-- | Lines starting with / are comments.
sfz_is_comment :: String -> Bool
sfz_is_comment ln =
  case ln of
    '/':_ -> True
    _ -> False

-- | Read a file, remove comments, parse into tokens.
--   Requires file names not include white space, which however they may...
sfz_load_tokens :: FilePath -> IO [String]
sfz_load_tokens fn = do
  s <- readFile fn
  let l = filter (not . sfz_is_comment) (lines s)
  return (concatMap words l)

-- | Pitch values, ie. for pitch_keycenter, may be either numbers or strings.
--
-- > map sfz_parse_pitch ["B3","60","C#4"] == [59,60,61]
sfz_parse_pitch :: String -> Int
sfz_parse_pitch s =
  case T.read_maybe_int s of
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

-- | Section tokens (<group> and <region>) start with <.
sfz_is_section :: String -> Bool
sfz_is_section s =
  case s of
    '<':_ -> True
    _ -> False

-- | Group tokens into sections.
sfz_tokens_group :: [String] -> [[String]]
sfz_tokens_group =
  filter (not . null) .
  (Split.split . Split.keepDelimsL . Split.whenElt) sfz_is_section

-- | Collate grouped token sequence into regions.
--   <group> opcodes are reset at each <group>.
sfz_collate :: [[String]] -> [SFZ_Region]
sfz_collate =
  let recur gr tk =
        case tk of
          [] -> []
          ("<group>":c):tk' -> recur (map sfz_parse_opcode c) tk'
          ("<region>":c):tk' -> (gr,map sfz_parse_opcode c) : recur gr tk'
          _ -> error "sfz_collate?"
  in recur []

-- | Load tokens, group and collate into regions.
sfz_load_regions :: FilePath -> IO [SFZ_Region]
sfz_load_regions fn = do
  tk <- sfz_load_tokens fn
  return (sfz_collate (sfz_tokens_group tk))

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

sfz_region_key :: SFZ_Region -> Maybe Int
sfz_region_key r = fmap sfz_parse_pitch (sfz_region_lookup r "key")

sfz_region_pitch_keycenter :: SFZ_Region -> Int
sfz_region_pitch_keycenter r = sfz_region_lookup_f 60 sfz_parse_pitch r "pitch_keycenter"

sfz_region_lokey :: SFZ_Region -> Int
sfz_region_lokey r = sfz_region_lookup_f 0 sfz_parse_pitch r "lokey"

sfz_region_hikey :: SFZ_Region -> Int
sfz_region_hikey r = sfz_region_lookup_f 127 sfz_parse_pitch r "hikey"

sfz_region_tune :: SFZ_Region -> Int
sfz_region_tune r = sfz_region_lookup_read 0 r "tune"

sfz_region_lochan :: SFZ_Region -> Int
sfz_region_lochan r = sfz_region_lookup_read 1 r "lochan"

sfz_region_hichan :: SFZ_Region -> Int
sfz_region_hichan r = sfz_region_lookup_read 16 r "hichan"

sfz_region_lovel :: SFZ_Region -> Int
sfz_region_lovel r = sfz_region_lookup_read 0 r "lovel"

sfz_region_hivel :: SFZ_Region -> Int
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
r <- fmap (map sfz_region_key_rewrite) (sfz_load_regions fn)
length r == 51
map sfz_region_sample r
map sfz_region_pitch_keycenter r
map sfz_region_lokey r
map sfz_region_hikey r
map sfz_region_ampeg_attack r
map sfz_region_ampeg_release r

-}
