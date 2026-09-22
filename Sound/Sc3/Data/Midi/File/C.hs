-- | Midi file Io, courtesy HCodecs.
module Sound.Sc3.Data.Midi.File.C where

import qualified Data.List {- base -}
import qualified Data.Maybe {- base -}

import qualified Codec.Midi {- HCodecs -}

import qualified Music.Theory.List as List {- hmt-base -}
import qualified Music.Theory.Time.Seq as Seq {- hmt-base -}

import qualified Sound.Midi.Common as Midi {- midi-osc -}
import qualified Sound.Midi.Type as Midi {- midi-osc -}

-- * Midi arcana

{- | Translate pulses per minute to micro seconds per quarter note.

>>> map ppm_to_mspqn [60,120,240]
[1000000,500000,250000]

>>> [10^6,5*10^5,25*10^4]
[1000000,500000,250000]
-}
ppm_to_mspqn :: Integral t => t -> t
ppm_to_mspqn ppm =
  let microseconds_per_minute = 60000000
  in microseconds_per_minute `div` ppm

-- | Table mapping time signature denominator to Midi notation.
ts_denominator_tbl :: Num t => [(t, t)]
ts_denominator_tbl = [(1, 0), (2, 1), (4, 2), (8, 3), (16, 4), (32, 5), (64, 6)]

mk_denominator :: (Num t, Eq t) => t -> t
mk_denominator d = List.lookup_err d ts_denominator_tbl

{- | Tempo change, given in pulses per minute.

>>> mk_tempo_change 60
TempoChange 1000000
-}
mk_tempo_change :: Codec.Midi.Tempo -> Codec.Midi.Message
mk_tempo_change = Codec.Midi.TempoChange . ppm_to_mspqn

{- | Make time signature with default values for ticks-per-pulse and 1/32-per-1/4.

>>> mk_time_signature (4,4)
TimeSignature 4 2 24 8
-}
mk_time_signature :: (Int, Int) -> Codec.Midi.Message
mk_time_signature (nn, d) =
  let dd = mk_denominator d
      cc = 24 -- midi ticks per pulse
      bb = 8 -- 1/32 per 1/4
  in Codec.Midi.TimeSignature nn dd cc bb

-- * Write

-- | Add 'Codec.Midi.TrackEnd' message.
add_track_end :: Seq.Tseq t Codec.Midi.Message -> Seq.Tseq t Codec.Midi.Message
add_track_end tr = tr ++ [(fst (last tr), Codec.Midi.TrackEnd)]

{- | Write Fmt-0 midi file.  The time-division is 1024.  Initial
tempo-change and time-signature meta data can be written.
-}
c_write_midi0_opt :: Maybe Int -> Maybe (Int, Int) -> FilePath -> [Seq.Tseq Codec.Midi.Time Codec.Midi.Message] -> IO ()
c_write_midi0_opt m_tc m_ts fn sq =
  let ft = Codec.Midi.SingleTrack
      tf = Codec.Midi.TicksPerBeat 1024
      pre =
        Data.Maybe.catMaybes
          [ fmap mk_tempo_change m_tc
          , fmap mk_time_signature m_ts
          ]
      m = map (\x -> (0, x)) pre ++ concat sq
      mk_t =
        Codec.Midi.fromAbsTime
          . Codec.Midi.fromRealTime tf
          . add_track_end
          . Data.List.sortOn fst
  in Codec.Midi.exportFile fn (Codec.Midi.Midi ft tf [mk_t m])

{- | Erroring variant of 'Codec.Midi.importFile'.

> let fn = "/home/rohan/sw/hsc3-data/data/midi/BWV-1080-1.midi"
> m <- c_load_midi fn
-}
c_load_midi :: FilePath -> IO Codec.Midi.Midi
c_load_midi fn = do
  r <- Codec.Midi.importFile fn
  return (either (\err -> error ("c_load_midi: read failed: " ++ show err)) id r)

{- | Load Type-0 or Type-1 Midi file as 'TSeq' data.  Ignores
everything except note on and off messages.

> sq <- c_read_midi fn
-}
c_read_midi :: FilePath -> IO [Seq.Tseq Codec.Midi.Time Codec.Midi.Message]
c_read_midi fn = do
  m <- c_load_midi fn
  let ty = Codec.Midi.fileType m
      dv = Codec.Midi.timeDiv m
      f = Codec.Midi.toRealTime dv . Codec.Midi.toAbsTime
      sq = filter (not . null) (map f (Codec.Midi.tracks m))
  if ty /= Codec.Midi.MultiPattern
    then return sq
    else error (show ("read_midi: not type-0 or type-1", ty))

-- * Header

-- | Midi header, (file-type, time-div, track-count).
c_midi_header :: Codec.Midi.Midi -> (Int, Int, Int)
c_midi_header m =
  ( c_file_type (Codec.Midi.fileType m)
  , c_time_div (Codec.Midi.timeDiv m)
  , length (Codec.Midi.tracks m)
  )

-- * Interop

c_file_type :: Codec.Midi.FileType -> Int
c_file_type ty =
  case ty of
    Codec.Midi.SingleTrack -> 0
    Codec.Midi.MultiTrack -> 1
    Codec.Midi.MultiPattern -> 2

c_time_div :: Codec.Midi.TimeDiv -> Int
c_time_div td =
  case td of
    Codec.Midi.TicksPerBeat i -> i
    _ -> error "c_time_div"

-- | Channel Messages
c_parse_channel_message :: Codec.Midi.Message -> Maybe (Midi.Channel_Voice_Message Int)
c_parse_channel_message c =
  case c of
    Codec.Midi.NoteOff ch mnn vel -> Just (Midi.Note_Off ch mnn vel)
    Codec.Midi.NoteOn ch mnn vel -> Just (Midi.Note_On ch mnn vel)
    Codec.Midi.KeyPressure ch d1 d2 -> Just (Midi.Polyphonic_Key_Pressure ch d1 d2)
    Codec.Midi.ControlChange ch i j -> Just (Midi.Control_Change ch i j)
    Codec.Midi.ProgramChange ch pc -> Just (Midi.Program_Change ch pc)
    Codec.Midi.ChannelPressure ch d1 -> Just (Midi.Channel_Aftertouch ch d1)
    Codec.Midi.PitchWheel ch d ->
      let (d1, d2) = Midi.bits_14_sep_le d
      in Just (Midi.Pitch_Bend ch d1 d2)
    _ -> Nothing

-- | Meta Messages
c_parse_meta_message :: Codec.Midi.Message -> Maybe [String]
c_parse_meta_message c =
  case c of
    Codec.Midi.TrackName nm -> Just ["track-name", nm]
    Codec.Midi.TempoChange tm -> Just ["tempo-change", show tm]
    Codec.Midi.TrackEnd -> Just ["track-end"]
    Codec.Midi.TimeSignature b0 b1 b2 b3 -> Just ("time-signature" : map show [b0, b1, b2, b3])
    Codec.Midi.KeySignature b0 b1 -> Just ("key-signature" : map show [b0, b1])
    Codec.Midi.SMPTEOffset b0 b1 b2 b3 b4 -> Just ("smpte-offset" : map show [b0, b1, b2, b3, b4])
    _ -> Nothing

c_parse_message :: Codec.Midi.Message -> Either [String] (Midi.Channel_Voice_Message Int)
c_parse_message c =
  case c_parse_channel_message c of
    Just m -> Right m
    Nothing ->
      case c_parse_meta_message c of
        Just m -> Left m
        Nothing -> Left ["unrecognised"]
