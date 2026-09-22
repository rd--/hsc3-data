-- | Midi file Io, courtesy zmidi-core.
module Sound.Sc3.Data.Midi.File.Z where

import qualified Control.Monad {- base -}
import qualified Data.Maybe {- base -}
import qualified Data.Ratio {- base -}
import qualified Data.Word {- base -}

import qualified Music.Theory.Time.Seq as Seq {- hmt-base -}

import qualified Music.Theory.Duration.Rq as Rq {- hmt -}

import qualified Sound.Midi.Common as Midi {- midi-osc -}
import qualified Sound.Midi.Type as Midi {- midi-osc -}

import qualified ZMidi.Core as ZMidi {- zmidi-core -}

-- | Unsigned 8-bit integer
type U8 = Data.Word.Word8

-- | Unsigned 32-bit integer
type U32 = Data.Word.Word32

-- | Unsigned 64-bit integer
type U64 = Data.Word.Word64

-- | 'ZMidi.canonical' of 'ZMidi.readMidi'
z_load_midi :: FilePath -> IO ZMidi.MidiFile
z_load_midi fn = do
  r <- ZMidi.readMidi fn
  return (either (\err -> error ("z_load_midi: read failed: " ++ show err)) ZMidi.canonical r)

-- | 'ZMidi.DeltaTime' to 'U32'
z_delta_time_to_word32 :: ZMidi.DeltaTime -> U32
z_delta_time_to_word32 = fromIntegral

-- | 'ZMidi.DeltaTime' to 'U64'
z_delta_time_to_word64 :: ZMidi.DeltaTime -> U64
z_delta_time_to_word64 = fromIntegral

-- | 'ZMidi.Word14' to 'Word16'
z_word14_to_int :: ZMidi.Word14 -> Int
z_word14_to_int = fromIntegral . toInteger

-- | Calculate 'Seq.Rq' for /t/ given 'ZMidi.MidiTimeDivision'.
z_to_rq :: Integral t => ZMidi.MidiTimeDivision -> t -> Rq.Rq
z_to_rq t_div t =
  case t_div of
    ZMidi.TPB n -> fromIntegral t Data.Ratio.% fromIntegral n
    _ -> error "non-TPB division"

-- | Type-specialised 'z_to_rq'.
z_delta_time_to_rq :: ZMidi.MidiTimeDivision -> ZMidi.DeltaTime -> Rq.Rq
z_delta_time_to_rq = z_to_rq

-- * Voice

z_status_ch :: U8 -> Midi.Channel
z_status_ch = fromIntegral . Midi.status_ch

-- | Translate from 'ZMidi.MidiVoiceEvent' to 'Midi.Channel_Voice_Message'.
z_parse_midi_voice_event :: ZMidi.MidiVoiceEvent -> Midi.Channel_Voice_Message Int
z_parse_midi_voice_event e =
  case e of
    ZMidi.NoteOff st d1 d2 -> Midi.Note_Off (z_status_ch st) (fromIntegral d1) (fromIntegral d2)
    ZMidi.NoteOn st d1 d2 -> Midi.Note_On (z_status_ch st) (fromIntegral d1) (fromIntegral d2)
    ZMidi.NoteAftertouch st d1 d2 -> Midi.Polyphonic_Key_Pressure (z_status_ch st) (fromIntegral d1) (fromIntegral d2)
    ZMidi.Controller st d1 d2 -> Midi.Control_Change (z_status_ch st) (fromIntegral d1) (fromIntegral d2)
    ZMidi.ProgramChange st d1 -> Midi.Program_Change (z_status_ch st) (fromIntegral d1)
    ZMidi.ChanAftertouch st d1 -> Midi.Channel_Aftertouch (z_status_ch st) (fromIntegral d1)
    ZMidi.PitchBend st d ->
      let (d1, d2) = Midi.bits_14_sep_le (z_word14_to_int d)
      in Midi.Pitch_Bend (z_status_ch st) (fromIntegral d1) (fromIntegral d2)

-- | Parse voice message at 'ZMidi.MidiMessage' to 'Midi.Channel_Voice_Message'.
z_parse_midi_message :: ZMidi.MidiMessage -> Maybe (ZMidi.DeltaTime, Midi.Channel_Voice_Message Int)
z_parse_midi_message (t, e) =
  case e of
    ZMidi.VoiceEvent _ v -> Just (t, z_parse_midi_voice_event v)
    _ -> Nothing

-- | Parse voice messages at 'ZMidi.MidiTrack'.
z_parse_midi_track :: ZMidi.MidiTrack -> Seq.Iseq ZMidi.DeltaTime (Midi.Channel_Voice_Message Int)
z_parse_midi_track = Data.Maybe.mapMaybe z_parse_midi_message . ZMidi.getTrackMessages

-- | 'z_parse_midi_track' Voice messages per-track.
z_parse_midi_file :: ZMidi.MidiFile -> [Seq.Iseq ZMidi.DeltaTime (Midi.Channel_Voice_Message Int)]
z_parse_midi_file = map z_parse_midi_track . ZMidi.mf_tracks

-- | 'Seq.iseq_to_tseq' of 'z_parse_midi_file'
z_parse_midi_file_abs :: ZMidi.MidiFile -> [Seq.Tseq U64 (Midi.Channel_Voice_Message Int)]
z_parse_midi_file_abs = map (Seq.iseq_to_tseq 0 . Seq.seq_tmap z_delta_time_to_word64) . z_parse_midi_file

-- * Meta

-- | Select 'ZMidi.MidiMetaEvent'.
z_meta_event :: ZMidi.MidiEvent -> Maybe ZMidi.MidiMetaEvent
z_meta_event e =
  case e of
    ZMidi.MetaEvent m -> Just m
    _ -> Nothing

-- | Read 'ZMidi.SetTempo'.
z_meta_event_tempo :: ZMidi.MidiMetaEvent -> Maybe U32
z_meta_event_tempo m =
  case m of
    ZMidi.SetTempo k -> Just k
    _ -> Nothing

-- | 'isJust' of 'meta_event_tempo'
z_is_tempo_event :: ZMidi.MidiMetaEvent -> Bool
z_is_tempo_event = Data.Maybe.isJust . z_meta_event_tempo

-- | Sequence of 'ZMidi.SetTempo' events.
z_midi_track_tempo :: ZMidi.MidiTrack -> Seq.Tseq U64 (Maybe U32)
z_midi_track_tempo =
  filter (Data.Maybe.isJust . snd)
    . map (fmap (z_meta_event_tempo Control.Monad.<=< z_meta_event))
    . z_midi_track_to_abs

-- | 'ZMidi.MidiTrack' with delta time-stamps converted to absolute time.
z_midi_track_to_abs :: ZMidi.MidiTrack -> Seq.Tseq U64 ZMidi.MidiEvent
z_midi_track_to_abs = Seq.iseq_to_tseq 0 . Seq.seq_tmap z_delta_time_to_word64 . ZMidi.getTrackMessages

-- | 'z_midi_track_tempo' of ' ZMidi.mf_tracks'
z_midi_file_tempo_map :: ZMidi.MidiFile -> [Seq.Tseq U64 (Maybe U32)]
z_midi_file_tempo_map = map z_midi_track_tempo . ZMidi.mf_tracks

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
