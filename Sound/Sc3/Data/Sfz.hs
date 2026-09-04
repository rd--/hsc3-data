{- | Sfz <http://www.sfzformat.com/> (<http://www.sfzformat.com/legacy/>)

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
module Sound.Sc3.Data.Sfz where

import qualified Data.List {- base -}
import qualified Data.Maybe {- base -}

import qualified System.FilePath {- filepath -}

import qualified Data.List.Split as Split {- split -}

import qualified Music.Theory.List as List {- hmt-base -}
import qualified Music.Theory.Maybe as Maybe {- hmt-base -}
import qualified Music.Theory.Read as Read {- hmt-base -}

import qualified Music.Theory.Pitch as Pitch {- hmt -}

import qualified Sound.File.HSndFile as SndFile {- hsc3-sf-hsndfile -}

import qualified Sound.Midi.Type as Midi {- midi-osc -}

import qualified Sound.Sc3.Data.Math.Types as Math {- hsc3-data -}

-- * Types

-- | An opcode is a (key,value) pair.
type Sfz_Opcode = (String, String)

-- | A <header> is a string.
type Sfz_Header = String

-- | A section is a <header> and a set of opcodes.
type Sfz_Section = (Sfz_Header, [Sfz_Opcode])

-- | The <control> section defines a set of opcodes.
type Sfz_Control = [Sfz_Opcode]

-- | The <global> section defines a set of opcodes.
type Sfz_Global = [Sfz_Opcode]

-- | <group> sections define a set of opcodes.
type Sfz_Group = [Sfz_Opcode]

-- | <region> sections define a set of opcodes, and have salient <global> and <group> opcodes.
type Sfz_Region = ([Sfz_Opcode], [Sfz_Opcode])

-- | (control,global,[region])
type Sfz_Data = (Sfz_Control, Sfz_Global, [Sfz_Region])

-- * Opcodes

-- | Does a set of opcodes contain given key?
sfz_region_has_opcode :: String -> Sfz_Region -> Bool
sfz_region_has_opcode k (g, c) = any ((== k) . fst) (g ++ c)

-- | Does a set of opcodes contain any of a given set of key?
sfz_region_has_opcode_in :: [String] -> Sfz_Region -> Bool
sfz_region_has_opcode_in k (g, c) = any ((`elem` k) . fst) (g ++ c)

-- | Delete any opcode with given key.
sfz_opcode_delete :: String -> [Sfz_Opcode] -> [Sfz_Opcode]
sfz_opcode_delete k = filter ((/= k) . fst)

-- * Parse

-- | Lines starting with / are comments.
sfz_is_comment :: String -> Bool
sfz_is_comment ln =
  case ln of
    '/' : _ -> True
    _ -> False

-- | Headers are in angle brackets, ie. <group>.
sfz_is_header :: Sfz_Header -> Bool
sfz_is_header s = not (null s) && List.head_err s == '<' && last s == '>'

{- | Sfz tokenizer, white space is allowed in the right hand sides of opcodes, ie. in file-names.

>>> sfz_tokenize "<region> sample=a.wav <region> sample=b c.wav"
["<region>","sample=a.wav","<region>","sample=b c.wav"]
-}
sfz_tokenize :: String -> [String]
sfz_tokenize =
  let recur l = case l of
        x1 : x2 : r ->
          if not (sfz_is_header x2) && '=' `notElem` x2
            then recur (unwords [x1, x2] : r)
            else x1 : recur (x2 : r)
        _ -> l
  in recur . words -- NB. Incorrect implementation, works for non-consecutive spaces only...

{- | Pitch values, ie. for pitch_keycenter, may be either numbers or strings.
  Returned as midi-note numbers (ie. 0 - 127)

>>> map sfz_parse_pitch ["B3","60","C#4"]
[59,60,61]
-}
sfz_parse_pitch :: String -> Midi.Key
sfz_parse_pitch s =
  case Read.read_maybe s of
    Just n -> n
    _ -> Pitch.pitch_to_midi (Pitch.parse_iso_pitch_err s)

{- | An opcode is written key=value.

>>> sfz_parse_opcode "pitch_keycenter=C4"
("pitch_keycenter","C4")

>>> sfz_parse_opcode "locc4=64"
("locc4","64")
-}
sfz_parse_opcode :: String -> Sfz_Opcode
sfz_parse_opcode s =
  case break (== '=') s of
    (k, '=' : v) -> (k, v)
    _ -> error "sfz_parse_opcode?"

-- | Group tokens into sections.
sfz_tokens_group :: [String] -> [Sfz_Section]
sfz_tokens_group =
  map (\(h : c) -> (h, map sfz_parse_opcode c))
    . filter (not . null)
    . (Split.split . Split.keepDelimsL . Split.whenElt) sfz_is_header

{- | Collate grouped token sequences.
  <region>s have salient <global> and <group> opcodes.
  <group> opcodes are reset at each <group> element.
-}
sfz_collate :: Sfz_Global -> [Sfz_Section] -> [Sfz_Region]
sfz_collate gl =
  let recur gr sc =
        case sc of
          [] -> []
          ("<group>", op) : sc' -> recur op sc'
          ("<region>", op) : sc' -> (gr ++ gl, op) : recur gr sc'
          _ -> error "sfz_collate?"
  in recur []

-- | Collect <control> and <global> opcodes, and collate <region>s.
sfz_get_data :: [Sfz_Section] -> Sfz_Data
sfz_get_data gr =
  let (lhs, rhs) = Data.List.partition ((`elem` ["<control>", "<global>"]) . fst) gr
  in case lhs of
      [] -> ([], [], sfz_collate [] rhs)
      [("<control>", c)] -> (c, [], sfz_collate [] rhs)
      [("<control>", c), ("<global>", g)] -> (c, g, sfz_collate g rhs)
      _ -> error "sfz_get_data?"

-- | Parse sections
sfz_parse_sections :: String -> [Sfz_Section]
sfz_parse_sections s =
  let l = filter (not . sfz_is_comment) (lines s)
  in sfz_tokens_group (concatMap sfz_tokenize l)

-- | Parse data
sfz_parse_data :: String -> Sfz_Data
sfz_parse_data = sfz_get_data . sfz_parse_sections

-- * Read/Io

-- | Read a file, remove comments, parse into sections.
sfz_load_sections :: FilePath -> IO [Sfz_Section]
sfz_load_sections = fmap sfz_parse_sections . readFile

-- | 'sfz_get_data' of 'sfz_load_sections'
sfz_load_data :: FilePath -> IO Sfz_Data
sfz_load_data = fmap sfz_parse_data . readFile

-- * Lookup

-- | Lookup in region opcodes, then in group if not located.
sfz_region_lookup :: Sfz_Region -> String -> Maybe String
sfz_region_lookup (gr, c) k =
  case lookup k c of
    Just r -> Just r
    Nothing -> lookup k gr

-- | Erroring variant.
sfz_region_lookup_err :: Sfz_Region -> String -> String
sfz_region_lookup_err r = Maybe.from_just "sfz_region_lookup" . sfz_region_lookup r

-- | Lookup with default value and parser.
sfz_region_lookup_f :: t -> (String -> t) -> Sfz_Region -> String -> t
sfz_region_lookup_f z f r = maybe z f . sfz_region_lookup r

-- | Lookup with default value and read instance.
sfz_region_lookup_read :: Read t => t -> Sfz_Region -> String -> t
sfz_region_lookup_read z = sfz_region_lookup_f z read

{- | Lookup in region opcodes, then in group if not located.

NOTE: Does not filter duplicates, region copies should override group and gobal copies.
-}
sfz_region_lookup_n_f :: (String -> t) -> Sfz_Region -> String -> [(Int, t)]
sfz_region_lookup_n_f parse (gr, c) k =
  let find = filter (\(x, _) -> k `Data.List.isPrefixOf` x)
      get_n = read . drop (length k)
      r = map (\(x, y) -> (get_n x, parse y)) (find c)
      g = map (\(x, y) -> (get_n x, parse y)) (find gr)
  in r ++ g

sfz_region_lookup_n :: Sfz_Region -> String -> [(Int, String)]
sfz_region_lookup_n = sfz_region_lookup_n_f id

-- * Named

-- | Volume, in decibels (-144.6 -- 6) <https://sfzformat.com/opcodes/volume/>
sfz_region_volume :: Sfz_Region -> Double
sfz_region_volume r = sfz_region_lookup_read 0 r "volume"

-- | Panoramic position, percentage (-100 -- 100) <https://sfzformat.com/opcodes/pan/>
sfz_region_pan :: Sfz_Region -> Double
sfz_region_pan r = sfz_region_lookup_read 0 r "pan"

-- | Sample <https://sfzformat.com/opcodes/sample/>
sfz_region_sample :: Sfz_Region -> FilePath
sfz_region_sample r = sfz_region_lookup_err r "sample"

-- | Tuning, in cents (-100 -- 100) <https://sfzformat.com/opcodes/tune/>
sfz_region_tune :: Sfz_Region -> Math.I8
sfz_region_tune r = sfz_region_lookup_read 0 r "tune"

-- | Low channel, one-indexed (1 -- 16) <https://sfzformat.com/opcodes/lochan/>
sfz_region_lochan :: Sfz_Region -> Midi.Channel
sfz_region_lochan r = sfz_region_lookup_read 1 r "lochan"

-- | High channel, one-indexed (1 -- 16) <https://sfzformat.com/opcodes/hichan/>
sfz_region_hichan :: Sfz_Region -> Midi.Channel
sfz_region_hichan r = sfz_region_lookup_read 16 r "hichan"

-- | Low and high channel
sfz_region_chan :: Sfz_Region -> (Midi.Velocity, Midi.Velocity)
sfz_region_chan r = (sfz_region_lochan r, sfz_region_hichan r)

-- | Low velocity (1 -- 127) <https://sfzformat.com/opcodes/lovel/>
sfz_region_lovel :: Sfz_Region -> Midi.Velocity
sfz_region_lovel r = sfz_region_lookup_read 0 r "lovel"

-- | High velocity (1 -- 127) <https://sfzformat.com/opcodes/hivel/>
sfz_region_hivel :: Sfz_Region -> Midi.Velocity
sfz_region_hivel r = sfz_region_lookup_read 127 r "hivel"

-- | Low and high velocity
sfz_region_vel :: Sfz_Region -> (Midi.Velocity, Midi.Velocity)
sfz_region_vel r = (sfz_region_lovel r, sfz_region_hivel r)

-- | Low random (0 -- 1) <https://sfzformat.com/opcodes/lorand/>
sfz_region_lorand :: Sfz_Region -> Double
sfz_region_lorand r = sfz_region_lookup_read 0 r "lorand"

-- | High random (0 -- 1) <https://sfzformat.com/opcodes/hirand/>
sfz_region_hirand :: Sfz_Region -> Double
sfz_region_hirand r = sfz_region_lookup_read 1 r "hirand"

-- | Low and high random
sfz_region_rand :: Sfz_Region -> (Double, Double)
sfz_region_rand r = (sfz_region_lorand r, sfz_region_hirand r)

-- | Loop mode <https://sfzformat.com/opcodes/loop_mode/>
sfz_region_loop_mode :: Sfz_Region -> Maybe String
sfz_region_loop_mode r = sfz_region_lookup r "loop_mode"

-- | Loop mode symbol table
sfz_loop_mode_sym_tbl :: [(String, Char)]
sfz_loop_mode_sym_tbl =
  [ ("no_loop", 'N')
  , ("one_shot", 'O')
  , ("loop_continuous", 'C')
  , ("loop_sustain", 'S')
  ]

-- | Loop mode symbol table lookup
sfz_loop_mode_sym :: String -> Char
sfz_loop_mode_sym = flip List.lookup_err sfz_loop_mode_sym_tbl

-- | Loop mode symbol
sfz_region_loop_mode_sym :: Sfz_Region -> Maybe Char
sfz_region_loop_mode_sym = fmap sfz_loop_mode_sym . sfz_region_loop_mode

-- | Loop start <https://sfzformat.com/opcodes/loop_start/>
sfz_region_loop_start :: Sfz_Region -> Math.U32
sfz_region_loop_start r = sfz_region_lookup_read 0 r "loop_start"

-- | Loop end <https://sfzformat.com/opcodes/loop_end/>
sfz_region_loop_end :: Sfz_Region -> Math.U32
sfz_region_loop_end r = sfz_region_lookup_read 0 r "loop_end"

-- | Amplitude envelope attack, in seconds (0 -- 100) <https://sfzformat.com/opcodes/ampeg_attack/>
sfz_region_ampeg_attack :: Sfz_Region -> Double
sfz_region_ampeg_attack r = sfz_region_lookup_read 0 r "ampeg_attack"

-- | Amplitude envelope decay, in seconds (0 -- 100) <https://sfzformat.com/opcodes/ampeg_decay/>
sfz_region_ampeg_decay :: Sfz_Region -> Double
sfz_region_ampeg_decay r = sfz_region_lookup_read 0 r "ampeg_decay"

-- | Amplitude envelope sustain, percentage (0 -- 100) <https://sfzformat.com/opcodes/ampeg_sustain/>
sfz_region_ampeg_sustain :: Sfz_Region -> Double
sfz_region_ampeg_sustain r = sfz_region_lookup_read 100 r "ampeg_sustain"

-- | Amplitude envelope release, in seconds (0.001 - 100) <https://sfzformat.com/opcodes/ampeg_release/>
sfz_region_ampeg_release :: Sfz_Region -> Double
sfz_region_ampeg_release r = sfz_region_lookup_read 0 r "ampeg_release"

-- | Amplitude envelope attack, sustain, decay and release
sfz_region_ampeg_adsr :: Sfz_Region -> (Double, Double, Double, Double)
sfz_region_ampeg_adsr r =
  ( sfz_region_ampeg_attack r
  , sfz_region_ampeg_decay r
  , sfz_region_ampeg_sustain r
  , sfz_region_ampeg_release r
  )

-- | Sequence length <https://sfzformat.com/opcodes/seq_length/>
sfz_region_seq_length :: Sfz_Region -> Int
sfz_region_seq_length r = sfz_region_lookup_read 1 r "seq_length"

-- | Sequence position <https://sfzformat.com/opcodes/seq_position/>
sfz_region_seq_position :: Sfz_Region -> Int
sfz_region_seq_position r = sfz_region_lookup_read 1 r "seq_position"

-- | Trigger (attack, release, first, legato) <https://sfzformat.com/opcodes/trigger/>
sfz_region_trigger :: Sfz_Region -> String
sfz_region_trigger r = sfz_region_lookup_f "attack" id r "trigger"

-- | Low continuous-controller (0 -- 127) <https://sfzformat.com/opcodes/loccN/>
sfz_region_locc :: Sfz_Region -> [(Math.I8, Math.I8)]
sfz_region_locc r = sfz_region_lookup_n_f read r "locc"

-- | High continuous-controller (0 -- 127) <https://sfzformat.com/opcodes/hiccN/>
sfz_region_hicc :: Sfz_Region -> [(Math.I8, Math.I8)]
sfz_region_hicc r = sfz_region_lookup_n_f read r "hicc"

{- | Low and high continuous-controller

>>> let o = sfz_parse_opcode "locc4=64"
>>> sfz_region_cc ([],[o])
Just (4,(64,127))
-}
sfz_region_cc :: Sfz_Region -> Maybe (Math.I8, (Math.I8, Math.I8))
sfz_region_cc r =
  case (sfz_region_locc r, sfz_region_hicc r) of
    ([], []) -> Nothing
    ([(k, i)], []) -> Just (k, (i, 127))
    ([], [(k, j)]) -> Just (k, (0, j))
    ([(k, i)], [(k', j)]) ->
      if k == k'
        then Just (k, (i, j))
        else error "sfz_region_cc"
    _ -> error "sfz_region_cc"

-- | Amplifier velocity tracking (-100 - 100) <https://sfzformat.com/opcodes/amp_veltrack/>
sfz_region_amp_veltrack :: Sfz_Region -> Double
sfz_region_amp_veltrack r = sfz_region_lookup_read 100 r "amp_veltrack"

{- | Amplitude velocity curve <https://sfzformat.com/opcodes/amp_velcurve_N/>

>>> (_,_,[r]) = sfz_parse_data "<region> amp_velcurve_1=0.1 amp_velcurve_63=0.25"
>>> sfz_region_amp_velcurve r
[(1,0.1),(63,0.25)]
-}
sfz_region_amp_velcurve :: Sfz_Region -> [(Math.I8, Double)]
sfz_region_amp_velcurve r = sfz_region_lookup_n_f read r "amp_velcurve_"

-- | Amplifier low-frequency oscillator frequency, in hertz (0 - 20) <https://sfzformat.com/opcodes/amplfo_freq/>
sfz_region_amplfo_freq :: Sfz_Region -> Double
sfz_region_amplfo_freq r = sfz_region_lookup_read 0 r "amplfo_freq"

-- | Amplifier low-frequency oscillator depth, in decibels (-10 - 10) <https://sfzformat.com/opcodes/amplfo_depth/>
sfz_region_amplfo_depth :: Sfz_Region -> Double
sfz_region_amplfo_depth r = sfz_region_lookup_read 0 r "amplfo_depth"

-- * Composite

-- | Check that if region has a key opcode it doesn't have any of the opcodes it implicitly defines.
sfz_region_key_validate :: Sfz_Region -> Bool
sfz_region_key_validate r =
  not
    ( sfz_region_has_opcode "key" r
        && sfz_region_has_opcode_in ["pitch_keycenter", "lokey", "hikey"] r
    )

{- | If opcode @key@ exists it defines the triple (pitch_keycenter,lokey,hikey).
  Else read these opcodes individually, with defaults.
-}
sfz_region_key :: Sfz_Region -> (Midi.Key, Midi.Key, Midi.Key)
sfz_region_key r =
  case sfz_region_lookup r "key" of
    Just x -> let n = sfz_parse_pitch x in (n, n, n)
    Nothing ->
      ( sfz_region_lookup_f 60 sfz_parse_pitch r "pitch_keycenter"
      , sfz_region_lookup_f 0 sfz_parse_pitch r "lokey"
      , sfz_region_lookup_f 127 sfz_parse_pitch r "hikey"
      )

{- | If loop start and end points are defined,
     then return them with mode (defaulting to loop_continuous),
     else return Nothing and mode (defaulting to no_loop).
     Does not read loop data from sample file.
-}
sfz_region_loop_data :: Sfz_Region -> (String, Maybe (Math.U32, Math.U32))
sfz_region_loop_data r =
  case (sfz_region_lookup r "loop_start", sfz_region_lookup r "loop_end") of
    (Just st, Just en) ->
      ( sfz_region_lookup_f "loop_continuous" id r "loop_mode"
      , Just (read st, read en)
      )
    _ -> (sfz_region_lookup_f "no_loop" id r "loop_mode", Nothing)

-- * Query

{- | Resolve sample file-name of <region>.
  Requires Sfz file name (for directory) and <control> data for default_path.

>>> sfz_region_sample_resolve "x/x.sfz" [] ([],[("sample","y.z")])
"x/y.z"

>>> sfz_region_sample_resolve "x.sfz" [("default_path","x")] ([],[("sample","y.z")])
"./x/y.z"

>>> "x" System.FilePath.</> "" System.FilePath.</> "y.z"
"x/y.z"

>>> System.FilePath.splitFileName "x.sfz"
("./","x.sfz")
-}
sfz_region_sample_resolve :: FilePath -> Sfz_Control -> Sfz_Region -> FilePath
sfz_region_sample_resolve sfz_fn ctl rgn =
  let (dir, _) = System.FilePath.splitFileName sfz_fn
      path = dir System.FilePath.</> Data.Maybe.fromMaybe "" (lookup "default_path" ctl)
  in path System.FilePath.</> sfz_region_sample rgn

-- | Get number-of-channels of sample of region, requires reading sound-file header.
sfz_region_get_nc :: FilePath -> Sfz_Control -> Sfz_Region -> IO Int
sfz_region_get_nc sfz_fn ctl rgn = do
  hdr <- SndFile.sf_header (sfz_region_sample_resolve sfz_fn ctl rgn)
  return (SndFile.channelCount hdr)

-- | Run 'sfz_region_get_nc' at each region in sequence.
sfz_data_get_nc :: FilePath -> Sfz_Data -> IO [Int]
sfz_data_get_nc sfz_fn (ctl, _, rgn) = mapM (sfz_region_get_nc sfz_fn ctl) rgn

-- | Sfz note range (lo,hi), inclusive
sfz_data_rng :: Sfz_Data -> (Midi.Key, Midi.Key)
sfz_data_rng (_, _, rgn) =
  let (_, l, r) = unzip3 (map sfz_region_key rgn)
  in (minimum l, maximum r)

-- * Write/Io

-- | Print section, nl=new-line
sfz_section_pp :: Bool -> Sfz_Section -> String
sfz_section_pp nl (hdr, op) =
  let tk = hdr : map (\(k, v) -> concat [k, "=", v]) op
  in (if nl then unlines else unwords) tk

-- | Write sections to file.
sfz_write_sections :: Bool -> FilePath -> [Sfz_Section] -> IO ()
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
z <- sfz_load_data fn
sfz_data_get_nc fn z
sfz_data_rng z == (36,86)
(_,_,r) = z
length r == 51
map sfz_region_sample r
map sfz_region_key r
map sfz_region_ampeg_attack r
map sfz_region_ampeg_release r

fn = "/home/rohan/A-String/000_A-String.sfz"
z <- sfz_load_data fn
sfz_data_get_nc fn z
sfz_data_rng z

-}
