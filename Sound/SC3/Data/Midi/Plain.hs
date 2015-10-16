-- | Write Type-0 midi files for plain midi note data.
module Sound.SC3.Data.Midi.Plain where

import Data.Function {- base -}
import Data.List {- base -}

import qualified Music.Theory.Array.CSV.Midi.MND as T {- hmt -}
import qualified Music.Theory.Time.Seq as T {- hmt -}
import qualified Music.Theory.Tuple as T {- hmt -}

import qualified Codec.Midi as M {- HCodecs -}

-- | Time in fractional seconds.
type Time = Double

-- | ((start-time,duration),(midi-note-number,midi-channel))
type Note = ((Time,Time),(Int,Int))

-- | 'T.Wseq' of (midi-note-number,midi-channel)
type SEQ = T.Wseq Time (Int,Int)

-- | Translate 'Note' to on & off messages.
note_to_midi :: Note -> T.T2 (Time,M.Message)
note_to_midi ((st,dur),(mn,ch)) =
    let n_on = (st, M.NoteOn ch mn 127)
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
write_midi :: FilePath -> [SEQ] -> IO ()
write_midi fn sq =
    let ft = M.SingleTrack
        tf = M.TicksPerBeat 1024
        ts = M.TimeSignature 4 2 24 8
        tc = M.TempoChange (mk_tempo 60)
        t_cmp = compare `on` fst
        ev = map (concat . map T.t2_list . map note_to_midi) sq
        m = (0,ts) : (0,tc) : concat ev
        t = M.fromRealTime tf . M.fromAbsTime . add_track_end . sortBy t_cmp $ m
    in M.exportFile fn (M.Midi ft tf [t])

-- | Write MND type CSV file.
write_csv :: FilePath -> [SEQ] -> IO ()
write_csv fn =
    T.midi_tseq_write fn .
    T.midi_wseq_to_midi_tseq .
    T.tseq_map (\(mn,ch) -> (mn,127,ch)) .
    foldl1 T.wseq_merge
