-- | Midi file IO, courtesy HCodecs.
module Sound.SC3.Data.Midi.File.C where

import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Time.Seq as T {- hmt -}

import qualified Codec.Midi as C {- HCodecs -}

-- * MIDI arcana

-- | Translate pulses per minute to micro seconds per quarter note.
--
-- > map ppm_to_mspqn [60,120,240] == [10^6,5*10^5,25*10^4]
ppm_to_mspqn :: Int -> Int
ppm_to_mspqn ppm =
    let microseconds_per_minute = 60000000
    in microseconds_per_minute `div` ppm

-- | Table mapping time signature denominator to MIDI notation.
ts_denominator_tbl :: [(Int,Int)]
ts_denominator_tbl = [(1,0),(2,1),(4,2),(8,3),(16,4),(32,5),(64,6)]

mk_denominator :: Int -> Int
mk_denominator d = T.lookup_err d ts_denominator_tbl

-- | Tempo change, given in pulses per minute.
--
-- > mk_tempo_change 60 == C.TempoChange (10^6)
mk_tempo_change :: Int -> C.Message
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

write_midi0_opt :: Maybe Int -> Maybe (Int,Int) -> FilePath -> [T.Tseq C.Time C.Message] -> IO ()
write_midi0_opt m_tc m_ts fn sq =
    let ft = C.SingleTrack
        tf = C.TicksPerBeat 1024
        pre = catMaybes [fmap mk_tempo_change m_tc
                        ,fmap mk_time_signature m_ts]
        m = map (\x -> (0,x)) pre ++ concat sq
        t = C.fromRealTime tf . C.fromAbsTime . add_track_end . sortOn fst $ m
    in C.exportFile fn (C.Midi ft tf [t])


-- | Load Type-0 or Type-1 MIDI file as 'SEQ' data.  Ignores
-- everything except note on and off messages.
read_midi :: FilePath -> IO [T.Tseq C.Time C.Message]
read_midi fn = do
  r <- C.importFile fn
  case r of
    Left err -> error err
    Right m -> let ty = C.fileType m
                   dv = C.timeDiv m
                   f = C.toRealTime dv . C.toAbsTime
                   sq = filter (not . null) (map f (C.tracks m))
               in if ty /= C.MultiPattern
                  then return sq
                  else error (show ("read_midi: not type-0 or type-1",ty))
