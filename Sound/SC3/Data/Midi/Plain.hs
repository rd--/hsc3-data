-- | Write Type-0 (single track) midi files for plain RQ midi note data.
-- Voices are encoded as channels (Finale can extract these).
-- Simpler than MusicXML transfer for cases where durations are straightforward.
module Sound.SC3.Data.Midi.Plain where

import Data.Maybe {- base -}

import qualified Music.Theory.Array.CSV.Midi.MND as T {- hmt -}
import qualified Music.Theory.Math.Convert as T {- hmt -}
import qualified Music.Theory.Time.Seq as T {- hmt -}

import qualified Codec.Midi as C {- HCodecs -}

import qualified Sound.SC3.Data.Midi.File.C as File.C {- hsc3-data -}

-- * Types

-- | The first value encodes non-note data as negative numbers.
--
--   0-127 = (midi-note-number,midi-velocity,midi-channel)
--   -1 = (-1:time-signature,numerator,denominator)
type Event = (Int,Int,Int)

-- | ((start-time,duration),note-event)
type Note = ((C.Time,C.Time),Event)

-- | 'T.Wseq' of 'Event'.
type SEQ = T.Wseq C.Time Event

-- * Write

-- | Translate 'Note' to on & off messages or time-signature message.
note_to_midi :: Note -> [(C.Time,C.Message)]
note_to_midi ((st,dur),(msg,d1,d2)) =
    let n_on = (st, C.NoteOn d2 msg d1)
        n_off = (st + dur, C.NoteOff d2 msg 0)
        ts = (st,File.C.mk_time_signature (d1,d2))
    in case msg of
         -1 -> [ts]
         _ -> [n_on,n_off]

-- | Add 'C.TrackEnd' message.
add_track_end :: T.Tseq t C.Message -> T.Tseq t C.Message
add_track_end tr = tr ++ [(fst (last tr),C.TrackEnd)]

write_midi0_opt :: Maybe Int -> Maybe (Int,Int) -> FilePath -> [SEQ] -> IO ()
write_midi0_opt m_tc m_ts fn = File.C.c_write_midi0_opt m_tc m_ts fn . map (concatMap note_to_midi)

-- | Write Type-0 midi file, tempo is 60, time signature is 4/4.
write_midi0 :: FilePath -> [SEQ] -> IO ()
write_midi0 = write_midi0_opt (Just 60) (Just (4,4))

-- * Read

track_to_wseq :: [(C.Time,C.Message)] -> SEQ
track_to_wseq =
    let f (tm,msg) = case msg of
                       C.NoteOn ch mnn vel ->
                           let ty = if vel == 0 then T.End else T.Begin
                           in Just (tm,ty (mnn,vel,ch))
                       C.NoteOff ch mnn vel ->
                           Just (tm,T.End (mnn,vel,ch))
                       _ -> Nothing
        event_eq (n0,_,c0) (n1,_,c1) = n0 == n1 && c0 == c1
    in T.tseq_begin_end_to_wseq event_eq . mapMaybe f

-- | Load Type-0 or Type-1 MIDI file as 'SEQ' data.  Ignores
-- everything except note on and off messages.
read_midi :: FilePath -> IO [SEQ]
read_midi fn = do
  m <- File.C.c_read_midi fn
  return (filter (not . null) (map track_to_wseq m))

-- * CSV

seq_merge :: [SEQ] -> SEQ
seq_merge = foldr T.wseq_merge []

-- | Write MND type CSV file.
write_csv_mnd :: FilePath -> [SEQ] -> IO ()
write_csv_mnd fn =
    T.csv_mnd_write_tseq 4 fn .
    T.midi_wseq_to_midi_tseq .
    T.tseq_map (\(mn,vel,ch) -> (mn,vel,T.int_to_word8 ch,[])) .
    seq_merge
