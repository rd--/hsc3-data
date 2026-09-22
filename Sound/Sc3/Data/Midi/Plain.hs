{- | Write Type-0 (single track) midi files for plain RQ midi note data.
Voices are encoded as channels (Finale can extract these).
Simpler than MusicXML transfer for cases where durations are straightforward.
-}
module Sound.Sc3.Data.Midi.Plain where

import qualified Data.Maybe {- base -}

import qualified Music.Theory.Time.Seq as Seq {- hmt-base -}

import qualified Music.Theory.Array.Csv.Midi.Mnd as Mnd {- hmt -}

import qualified Codec.Midi as Codec.Midi {- HCodecs -}

import qualified Sound.Sc3.Data.Midi.File.C as Midi.File {- hsc3-data -}

-- * Types

{- | ((start-time,duration),note-event)

  The Mnd.Event type is re-purposed a little.
  The first value encodes non-note data as negative numbers.

  0-127 = (midi-note-number,midi-velocity,midi-channel)
  -1 = (-1:time-signature,numerator,denominator)
-}
type Note = ((Codec.Midi.Time, Codec.Midi.Time), Mnd.Event Int)

-- | 'Mnd.Wseq' of 'Event'.
type Seq = [Note]

-- * Write

-- | Translate 'Note' to on & off messages or time-signature message.
note_to_midi :: Note -> [(Codec.Midi.Time, Codec.Midi.Message)]
note_to_midi ((st, dur), (msg, d1, d2, _)) =
  let n_on = (st, Codec.Midi.NoteOn d2 msg d1)
      n_off = (st + dur, Codec.Midi.NoteOff d2 msg 0)
      ts = (st, Midi.File.mk_time_signature (d1, d2))
  in case msg of
      -1 -> [ts]
      _ -> [n_on, n_off]

-- | Add 'Codec.Midi.TrackEnd' message.
add_track_end :: Seq.Tseq t Codec.Midi.Message -> Seq.Tseq t Codec.Midi.Message
add_track_end tr = tr ++ [(fst (last tr), Codec.Midi.TrackEnd)]

write_midi0_opt :: Maybe Int -> Maybe (Int, Int) -> FilePath -> [Seq] -> IO ()
write_midi0_opt m_tc m_ts fn =
  Midi.File.c_write_midi0_opt m_tc m_ts fn
    . map (concatMap note_to_midi)

-- | Write Type-0 midi file, tempo is 60, time signature is 4/4.
write_midi0 :: FilePath -> [Seq] -> IO ()
write_midi0 = write_midi0_opt (Just 60) (Just (4, 4))

-- * Translate

-- | 'Mnd.Event' 'Mnd.Wseq' node to 'Note'.
mnd_to_note :: ((Double, Double), Mnd.Event Int) -> Note
mnd_to_note = id -- ((st,du),(mnn,vel,ch,param)) = ((st,du),(mnn,vel,Mnd.word8_to_int ch,param))

-- | Type-specialised
event_fmidi_to_midi :: Integral t => (Double -> t) -> Mnd.Event Double -> Mnd.Event t
event_fmidi_to_midi = Mnd.event_cast

{- | Read either Mnd or Mndd Csv file and write Format-0 midi file.

> let csv_fn = "/home/rohan/sw/hmt/data/csv/mnd/1080-C01.csv"
> cvs_mnd_to_midi0 False 60 (4,4) csv_fn "/tmp/1080-C01.midi"
-}
cvs_mnd_to_midi0 :: Bool -> Int -> (Int, Int) -> FilePath -> FilePath -> IO ()
cvs_mnd_to_midi0 rw tc ts fn1 fn2 = do
  sq <- Mnd.csv_midi_read_wseq fn1
  let f1 = Seq.wseq_map (event_fmidi_to_midi round)
      f2 = if rw then Seq.wseq_remove_overlaps_rw Mnd.event_eq_ol id else id
      sq_n = map mnd_to_note (f2 (f1 sq))
  print ("cvs_mnd_to_midi0", "#", length sq, "o/l", Seq.wseq_has_overlaps Mnd.event_eq_ol sq)
  write_midi0_opt (Just tc) (Just ts) fn2 [sq_n]

-- * Read

track_to_wseq :: [(Codec.Midi.Time, Codec.Midi.Message)] -> Seq
track_to_wseq =
  let f (tm, msg) = case msg of
        Codec.Midi.NoteOn ch mnn vel ->
          let ty = if vel == 0 then Seq.End else Seq.Begin
          in Just (tm, ty (mnn, vel, ch, []))
        Codec.Midi.NoteOff ch mnn vel ->
          Just (tm, Seq.End (mnn, vel, ch, []))
        _ -> Nothing
  in Seq.tseq_begin_end_to_wseq Mnd.event_eq_ol
      . Data.Maybe.mapMaybe f

{- | Load Type-0 or Type-1 MIDI file as 'Seq' data.  Ignores
everything except note on and off messages.
-}
read_midi :: FilePath -> IO [Seq]
read_midi fn = do
  m <- Midi.File.c_read_midi fn
  return (filter (not . null) (map track_to_wseq m))

-- * Csv

seq_merge :: [Seq] -> Seq
seq_merge = foldr Seq.wseq_merge []

-- | Write Mnd type Csv file.
write_csv_mnd :: FilePath -> [Seq] -> IO ()
write_csv_mnd fn =
  Mnd.csv_mnd_write_tseq 4 fn
    . Mnd.midi_wseq_to_midi_tseq
    . seq_merge
