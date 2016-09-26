-- | Write Type-0 (single track) midi files for plain RQ midi note data.
-- Voices are encoded as channels (Finale can extract these).
-- Simpler than MusicXML transfer for cases where durations are straightforward.
module Sound.SC3.Data.Midi.Plain where

import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Music.Theory.Array.CSV.Midi.MND as T {- hmt -}
import qualified Music.Theory.Math.Convert as T {- hmt -}
import qualified Music.Theory.Time.Seq as T {- hmt -}
import qualified Music.Theory.Tuple as T {- hmt -}

import qualified Codec.Midi as M {- HCodecs -}

-- | Time in fractional seconds.
type Time = Double

-- | (midi-note-number,midi-velocity,midi-channel)
type Event = (Int,Int,Int)

-- | ((start-time,duration),event)
type Note = ((Time,Time),Event)

-- | 'T.Wseq' of 'Event'.
type SEQ = T.Wseq Time Event

-- | Translate 'Note' to on & off messages.
note_to_midi :: Note -> T.T2 (Time,M.Message)
note_to_midi ((st,dur),(mn,vel,ch)) =
    let n_on = (st, M.NoteOn ch mn vel)
        n_off = (st + dur, M.NoteOff ch mn 0)
    in (n_on,n_off)

-- | Add 'M.TrackEnd' message.
add_track_end :: T.Tseq t M.Message -> T.Tseq t M.Message
add_track_end tr = tr ++ [(fst (last tr),M.TrackEnd)]

-- | Translate tempo value.
--
-- > map mk_tempo [60,120,240] == [10^6,5*10^5,25*10^4]
mk_tempo :: Int -> Int
mk_tempo bpm =
    let microseconds_per_minute = 60000000
    in microseconds_per_minute `div` bpm

-- | Write Type-0 midi file, tempo is 60, time signature is 4/4.
write_midi0 :: FilePath -> [SEQ] -> IO ()
write_midi0 fn sq =
    let ft = M.SingleTrack
        tf = M.TicksPerBeat 1024
        ts = M.TimeSignature 4 2 24 8
        tc = M.TempoChange (mk_tempo 60)
        ev = map (concat . map T.t2_to_list . map note_to_midi) sq
        m = (0,ts) : (0,tc) : concat ev
        t = M.fromRealTime tf . M.fromAbsTime . add_track_end . sortOn fst $ m
    in M.exportFile fn (M.Midi ft tf [t])

track_to_wseq :: [(Time,M.Message)] -> SEQ
track_to_wseq =
    let f (tm,msg) = case msg of
                       M.NoteOn ch mnn vel ->
                           let ty = if vel == 0 then T.Off else T.On
                           in Just (tm,ty (mnn,vel,ch))
                       M.NoteOff ch mnn vel ->
                           Just (tm,T.Off (mnn,vel,ch))
                       _ -> Nothing
        event_eq (n0,_,c0) (n1,_,c1) = n0 == n1 && c0 == c1
    in T.tseq_on_off_to_wseq event_eq . mapMaybe f

-- | Load Type-0 or Type-1 MIDI file as 'SEQ' data.  Ignores
-- everything except note on and off messages.
read_midi :: FilePath -> IO [SEQ]
read_midi fn = do
  r <- M.importFile fn
  case r of
    Left err -> error err
    Right m -> let ty = M.fileType m
                   dv = M.timeDiv m
                   f = track_to_wseq . M.toRealTime dv . M.toAbsTime
                   sq = filter (not . null) (map f (M.tracks m))
               in if ty /= M.MultiPattern
                  then return sq
                  else error (show ("read_midi: not type-0 or type-1",ty))

-- | Write MND type CSV file.
write_csv_mnd :: FilePath -> [SEQ] -> IO ()
write_csv_mnd fn =
    T.csv_mnd_write_tseq 4 fn .
    T.midi_wseq_to_midi_tseq .
    T.tseq_map (\(mn,vel,ch) -> (mn,vel,T.int_to_word8 ch,[])) .
    seq_merge

seq_merge :: [SEQ] -> SEQ
seq_merge = foldr T.wseq_merge []
