-- | Write Type-0 (single track) midi files for plain RQ midi note data.
-- Voices are encoded as channels (Finale can extract these).
-- Simpler than MusicXML transfer for cases where durations are straightforward.
module Sound.SC3.Data.Midi.Plain where

import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Music.Theory.Array.CSV.Midi.MND as T {- hmt -}
import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Math.Convert as T {- hmt -}
import qualified Music.Theory.Time.Seq as T {- hmt -}

import qualified Codec.Midi as M {- HCodecs -}

-- * Types

-- | Time in fractional seconds.
type Time = Double

-- | The first value encodes non-note data as negative numbers.
--
--   0-127 = (midi-note-number,midi-velocity,midi-channel)
--   -1 = (-1:time-signature,numerator,denominator)
type Event = (Int,Int,Int)

-- | ((start-time,duration),note-event)
type Note = ((Time,Time),Event)

-- | 'T.Wseq' of 'Event'.
type SEQ = T.Wseq Time Event

-- * MIDI arcana

-- | Translate pulses per minute to micro seconds per quarter note.
--
-- > map ppm_to_mspqn [60,120,240] == [10^6,5*10^5,25*10^4]
ppm_to_mspqn :: Int -> Int
ppm_to_mspqn ppm =
    let microseconds_per_minute = 60000000
    in microseconds_per_minute `div` ppm

-- | Tempo change, given in pulses per minute.
--
-- > mk_tempo_change 60 == M.TempoChange (10^6)
mk_tempo_change :: Int -> M.Message
mk_tempo_change = M.TempoChange . ppm_to_mspqn

-- | Table mapping time signature denominator to MIDI notation.
ts_denominator_tbl :: [(Int,Int)]
ts_denominator_tbl = [(1,0),(2,1),(4,2),(8,3),(16,4),(32,5),(64,6)]

mk_denominator :: Int -> Int
mk_denominator d = T.lookup_err d ts_denominator_tbl

-- | Make time signature with default values for ticks-per-pulse and 1/32-per-1/4.
--
-- > mk_time_signature (4,4) == M.TimeSignature 4 2 24 8
mk_time_signature :: (Int,Int) -> M.Message
mk_time_signature (nn,d) =
    let dd = mk_denominator d
        cc = 24 -- midi ticks per pulse
        bb = 8 -- 1/32 per 1/4
    in M.TimeSignature nn dd cc bb

-- * Write

-- | Translate 'Note' to on & off messages or time-signature message.
note_to_midi :: Note -> [(Time,M.Message)]
note_to_midi ((st,dur),(msg,d1,d2)) =
    let n_on = (st, M.NoteOn d2 msg d1)
        n_off = (st + dur, M.NoteOff d2 msg 0)
        ts = (st,mk_time_signature (d1,d2))
    in case msg of
         -1 -> [ts]
         _ -> [n_on,n_off]

-- | Add 'M.TrackEnd' message.
add_track_end :: T.Tseq t M.Message -> T.Tseq t M.Message
add_track_end tr = tr ++ [(fst (last tr),M.TrackEnd)]

write_midi0_opt :: Maybe Int -> Maybe (Int,Int) -> FilePath -> [SEQ] -> IO ()
write_midi0_opt m_tc m_ts fn sq =
    let ft = M.SingleTrack
        tf = M.TicksPerBeat 1024
        pre = catMaybes [fmap mk_tempo_change m_tc
                        ,fmap mk_time_signature m_ts]
        ev = map (concatMap note_to_midi) sq
        m = map (\x -> (0,x)) pre ++ concat ev
        t = M.fromRealTime tf . M.fromAbsTime . add_track_end . sortOn fst $ m
    in M.exportFile fn (M.Midi ft tf [t])

-- | Write Type-0 midi file, tempo is 60, time signature is 4/4.
write_midi0 :: FilePath -> [SEQ] -> IO ()
write_midi0 = write_midi0_opt (Just 60) (Just (4,4))

-- * Read

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
