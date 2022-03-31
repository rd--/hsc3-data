-- | Midi file IO, courtesy zmidi-core.
module Sound.SC3.Data.Midi.File.Z where

import Control.Monad {- base -}
import Data.Maybe {- base -}
import Data.Ratio {- base -}
import Data.Word {- base -}

import qualified Music.Theory.Duration.RQ as T {- hmt -}
import qualified Music.Theory.Time.Seq as T {- hmt -}

import qualified Sound.Midi.Common as M {- midi-osc -}
import qualified Sound.Midi.Type as M {- midi-osc -}

import qualified ZMidi.Core as Z {- zmidi-core -}

-- | 'Z.canonical' of 'Z.readMidi'
z_load_midi :: FilePath -> IO Z.MidiFile
z_load_midi fn = do
  r <- Z.readMidi fn
  return (either (\err -> error ("z_load_midi: read failed: " ++ show err)) Z.canonical r)

-- | 'Z.DeltaTime' to 'Word32'
z_delta_time_to_word32 :: Z.DeltaTime -> Word32
z_delta_time_to_word32 = fromIntegral

-- | 'Z.DeltaTime' to 'Word64'
z_delta_time_to_word64 :: Z.DeltaTime -> Word64
z_delta_time_to_word64 = fromIntegral

-- | 'Z.Word14' to 'Word16'
z_word14_to_int :: Z.Word14 -> Int
z_word14_to_int = fromIntegral . toInteger

-- | Calculate 'T.RQ' for /t/ given 'Z.MidiTimeDivision'.
z_to_rq :: Integral t => Z.MidiTimeDivision -> t -> T.RQ
z_to_rq t_div t =
  case t_div of
    Z.TPB n -> fromIntegral t % fromIntegral n
    _ -> error "non-TPB division"

-- | Type-specialised 'z_to_rq'.
z_delta_time_to_rq :: Z.MidiTimeDivision -> Z.DeltaTime -> T.RQ
z_delta_time_to_rq = z_to_rq

-- * Voice

z_status_ch :: Word8 -> M.Channel
z_status_ch = fromIntegral . M.status_ch

-- | Translate from 'Z.MidiVoiceEvent' to 'M.Channel_Voice_Message'.
z_parse_midi_voice_event :: Z.MidiVoiceEvent -> M.Channel_Voice_Message Int
z_parse_midi_voice_event e =
  case e of
    Z.NoteOff st d1 d2 -> M.Note_Off (z_status_ch st) (fromIntegral d1) (fromIntegral d2)
    Z.NoteOn st d1 d2 -> M.Note_On (z_status_ch st) (fromIntegral d1) (fromIntegral d2)
    Z.NoteAftertouch st d1 d2 -> M.Polyphonic_Key_Pressure (z_status_ch st) (fromIntegral d1) (fromIntegral d2)
    Z.Controller st d1 d2 -> M.Control_Change (z_status_ch st) (fromIntegral d1) (fromIntegral d2)
    Z.ProgramChange st d1 -> M.Program_Change (z_status_ch st) (fromIntegral d1)
    Z.ChanAftertouch st d1 -> M.Channel_Aftertouch (z_status_ch st) (fromIntegral d1)
    Z.PitchBend st d ->
      let (d1,d2) = M.bits_14_sep_le (z_word14_to_int d)
      in M.Pitch_Bend (z_status_ch st) (fromIntegral d1) (fromIntegral d2)

-- | Parse voice message at 'Z.MidiMessage' to 'M.Channel_Voice_Message'.
z_parse_midi_message :: Z.MidiMessage -> Maybe (Z.DeltaTime,M.Channel_Voice_Message Int)
z_parse_midi_message (t,e) =
  case e of
    Z.VoiceEvent _ v -> Just (t,z_parse_midi_voice_event v)
    _ -> Nothing

-- | Parse voice messages at 'Z.MidiTrack'.
z_parse_midi_track :: Z.MidiTrack -> T.Iseq Z.DeltaTime (M.Channel_Voice_Message Int)
z_parse_midi_track = mapMaybe z_parse_midi_message . Z.getTrackMessages

-- | 'z_parse_midi_track' Voice messages per-track.
z_parse_midi_file :: Z.MidiFile -> [T.Iseq Z.DeltaTime (M.Channel_Voice_Message Int)]
z_parse_midi_file = map z_parse_midi_track . Z.mf_tracks

-- | 'T.iseq_to_tseq' of 'z_parse_midi_file'
z_parse_midi_file_abs :: Z.MidiFile -> [T.Tseq Word64 (M.Channel_Voice_Message Int)]
z_parse_midi_file_abs = map (T.iseq_to_tseq 0 . T.seq_tmap z_delta_time_to_word64) . z_parse_midi_file

-- * Meta

-- | Select 'Z.MidiMetaEvent'.
z_meta_event :: Z.MidiEvent -> Maybe Z.MidiMetaEvent
z_meta_event e =
  case e of
    Z.MetaEvent m -> Just m
    _ -> Nothing

-- | Read 'Z.SetTempo'.
z_meta_event_tempo :: Z.MidiMetaEvent -> Maybe Word32
z_meta_event_tempo m =
  case m of
    Z.SetTempo k -> Just k
    _ -> Nothing

-- | 'isJust' of 'meta_event_tempo'
z_is_tempo_event :: Z.MidiMetaEvent -> Bool
z_is_tempo_event = isJust . z_meta_event_tempo

-- | Sequence of 'Z.SetTempo' events.
z_midi_track_tempo :: Z.MidiTrack -> T.Tseq Word64 (Maybe Word32)
z_midi_track_tempo =
  filter (isJust . snd) .
  map (fmap (z_meta_event_tempo <=< z_meta_event)) .
  z_midi_track_to_abs

-- | 'Z.MidiTrack' with delta time-stamps converted to absolute time.
z_midi_track_to_abs :: Z.MidiTrack -> T.Tseq Word64 Z.MidiEvent
z_midi_track_to_abs = T.iseq_to_tseq 0 . T.seq_tmap z_delta_time_to_word64 . Z.getTrackMessages

-- | 'z_midi_track_tempo' of ' Z.mf_tracks'
z_midi_file_tempo_map :: Z.MidiFile -> [T.Tseq Word64 (Maybe Word32)]
z_midi_file_tempo_map = map z_midi_track_tempo . Z.mf_tracks

{-

fn = "/home/rohan/sw/rsc3-midi/help/1080-C01.midi"
m <- z_load_midi fn
sq = z_parse_midi_file_abs m
length sq == 17
length (sq !! 1) == 495
sq !! 1
tm = z_midi_file_tempo_map m
length tm == 17
map length tm == [40,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
tm
-}
