-- | Midi file IO, courtesy HCodecs.
module Sound.Sc3.Data.Midi.File.C where

import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Codec.Midi as C {- HCodecs -}

import qualified Music.Theory.List as T {- hmt-base -}

import qualified Music.Theory.Time.Seq as T {- hmt -}

import qualified Sound.Midi.Common as M {- midi-osc -}
import qualified Sound.Midi.Type as M {- midi-osc -}

-- * MIDI arcana

-- | Translate pulses per minute to micro seconds per quarter note.
--
-- > map ppm_to_mspqn [60,120,240] == [10^6,5*10^5,25*10^4]
ppm_to_mspqn :: Integral t => t -> t
ppm_to_mspqn ppm =
    let microseconds_per_minute = 60000000
    in microseconds_per_minute `div` ppm

-- | Table mapping time signature denominator to MIDI notation.
ts_denominator_tbl :: Num t => [(t,t)]
ts_denominator_tbl = [(1,0),(2,1),(4,2),(8,3),(16,4),(32,5),(64,6)]

mk_denominator :: (Num t,Eq t) => t -> t
mk_denominator d = T.lookup_err d ts_denominator_tbl

-- | Tempo change, given in pulses per minute.
--
-- > mk_tempo_change 60 == C.TempoChange (10^6)
mk_tempo_change :: C.Tempo -> C.Message
mk_tempo_change = C.TempoChange . ppm_to_mspqn

-- | Make time signature with default values for ticks-per-pulse and 1/32-per-1/4.
--
-- > mk_time_signature (4,4) == C.TimeSignature 4 2 24 8
mk_time_signature :: (Int,Int) -> C.Message
mk_time_signature (nn,d) =
    let dd = mk_denominator d
        cc = 24 -- midi ticks per pulse
        bb = 8 -- 1/32 per 1/4
    in C.TimeSignature nn dd cc bb

-- * WRITE

-- | Add 'C.TrackEnd' message.
add_track_end :: T.Tseq t C.Message -> T.Tseq t C.Message
add_track_end tr = tr ++ [(fst (last tr),C.TrackEnd)]

-- | Write FMT-0 midi file.  The time-division is 1024.  Initial
-- tempo-change and time-signature meta data can be written.
c_write_midi0_opt :: Maybe Int -> Maybe (Int,Int) -> FilePath -> [T.Tseq C.Time C.Message] -> IO ()
c_write_midi0_opt m_tc m_ts fn sq =
    let ft = C.SingleTrack
        tf = C.TicksPerBeat 1024
        pre = catMaybes [fmap mk_tempo_change m_tc
                        ,fmap mk_time_signature m_ts]
        m = map (\x -> (0,x)) pre ++ concat sq
        mk_t = C.fromRealTime tf . C.fromAbsTime . add_track_end . sortOn fst
    in C.exportFile fn (C.Midi ft tf [mk_t m])

-- | Erroring variant of 'C.importFile'.
--
-- > fn = "/home/rohan/sw/hsc3-data/data/midi/BWV-1080-1.midi"
-- > m <- c_load_midi fn
c_load_midi :: FilePath -> IO C.Midi
c_load_midi fn = do
  r <- C.importFile fn
  return (either (\err -> error ("c_load_midi: read failed: " ++ show err)) id r)

-- | Load Type-0 or Type-1 MIDI file as 'SEQ' data.  Ignores
-- everything except note on and off messages.
--
-- > sq <- c_read_midi fn
c_read_midi :: FilePath -> IO [T.Tseq C.Time C.Message]
c_read_midi fn = do
  m <- c_load_midi fn
  let ty = C.fileType m
      dv = C.timeDiv m
      f = C.toRealTime dv . C.toAbsTime
      sq = filter (not . null) (map f (C.tracks m))
  if ty /= C.MultiPattern
    then return sq
    else error (show ("read_midi: not type-0 or type-1",ty))

-- * HEADER

-- | MIDI header, (file-type, time-div, track-count).
c_midi_header :: C.Midi -> (Int,Int,Int)
c_midi_header m =
  (c_file_type (C.fileType m)
  ,c_time_div (C.timeDiv m)
  ,length (C.tracks m))

-- * INTEROP

c_file_type :: C.FileType -> Int
c_file_type ty =
    case ty of
      C.SingleTrack -> 0
      C.MultiTrack -> 1
      C.MultiPattern -> 2

c_time_div :: C.TimeDiv -> Int
c_time_div td =
    case td of
      C.TicksPerBeat i -> i
      _ -> error "c_time_div"

-- | Channel Messages
c_parse_channel_message :: C.Message -> Maybe (M.Channel_Voice_Message Int)
c_parse_channel_message c =
  case c of
    C.NoteOff ch mnn vel -> Just (M.Note_Off ch mnn vel)
    C.NoteOn ch mnn vel -> Just (M.Note_On ch mnn vel)
    C.KeyPressure ch d1 d2 -> Just (M.Polyphonic_Key_Pressure ch d1 d2)
    C.ControlChange ch i j -> Just (M.Control_Change ch i j)
    C.ProgramChange ch pc -> Just (M.Program_Change ch pc)
    C.ChannelPressure ch d1 -> Just (M.Channel_Aftertouch ch d1)
    C.PitchWheel ch d ->
      let (d1,d2) = M.bits_14_sep_le d
      in Just (M.Pitch_Bend ch d1 d2)
    _ -> Nothing

-- | Meta Messages
c_parse_meta_message :: C.Message -> Maybe [String]
c_parse_meta_message c =
  case c of
    C.TrackName nm -> Just ["track-name",nm]
    C.TempoChange tm -> Just ["tempo-change",show tm]
    C.TrackEnd -> Just ["track-end"]
    C.TimeSignature b0 b1 b2 b3 -> Just ("time-signature" : map show [b0,b1,b2,b3])
    C.KeySignature b0 b1 -> Just ("key-signature" : map show [b0,b1])
    C.SMPTEOffset b0 b1 b2 b3 b4 -> Just ("smpte-offset" : map show [b0,b1,b2,b3,b4])
    _ -> Nothing

c_parse_message :: C.Message -> Either [String] (M.Channel_Voice_Message Int)
c_parse_message c =
  case c_parse_channel_message c of
    Just m -> Right m
    Nothing ->
      case c_parse_meta_message c of
        Just m -> Left m
        Nothing -> Left ["unrecognised"]
