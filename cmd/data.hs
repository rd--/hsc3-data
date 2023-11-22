import Data.Function {- base -}
import Data.List {- base -}
import Data.Word {- base -}
import System.Environment {- base -}

import qualified Data.Vector.Storable as Vector {- vector -}

import qualified Text.CSV.Lazy.String as Csv {- lazy-csv -}

import qualified Music.Theory.Byte as Byte {- hmt-base -}
import qualified Music.Theory.List as List {- hmt-base -}
import qualified Music.Theory.Show as Show {- hmt-base -}

import qualified Music.Theory.Array.Csv.Midi.Mnd as Midi.Mnd {- hmt -}
import qualified Music.Theory.Time.Seq as Seq {- hmt -}

import qualified Sound.Sc3 as Sc3 {- hsc3 -}
import qualified Sound.Sc3.Common.Buffer as Sc3.Common.Buffer {- hsc3 -}

import qualified Sound.Sc3.Lang.Math.Statistics as Lang.Math.Statistics {- hsc3-lang -}

import qualified Sound.File.Next as Sf.Au {- hsc3-sf -}
import qualified Sound.File.HSndFile as Sf.SndFile {- hsc3-sf-hsndfile -}

import qualified Sound.Sc3.Data.Ats as Ats {- hsc3-data -}
import qualified Sound.Sc3.Data.Bitmap.Pbm as Pbm {- hsc3-data -}
import qualified Sound.Sc3.Data.Bitmap.Type as Bitmap {- hsc3-data -}
import qualified Sound.Sc3.Data.Image.Pgm as Pgm  {- hsc3-data -}
import qualified Sound.Sc3.Data.Image.Plain as Image.Plain {- hsc3-data -}
import qualified Sound.Sc3.Data.Midi.Plain as Midi.Plain {- hsc3-data -}

-- * Ats

ats_header :: FilePath -> IO ()
ats_header fn = Ats.ats_read fn >>= putStrLn . Ats.ats_header_pp . Ats.ats_header

-- * Au

{- | Au to Pbm

> let fn = "/home/rohan/sw/hsc3-sf/au/mc-4-16.au"
> au_to_pbm fn (fn ++ ".pbm")
> pbm_print_ascii (fn ++ ".pbm")
-}
au_to_pbm :: FilePath -> FilePath -> IO ()
au_to_pbm au_fn pbm_fn = do
  (hdr,vec) <- Sf.Au.au_read_f32_vec au_fn
  let nr = Sf.Au.channelCount hdr
      nc = Sf.Au.frameCount hdr
      dm = (nr,nc)
      f ix = let n = vec Vector.! Bitmap.ix_to_linear_co dm ix in n > 0.5
  Pbm.pbm4_write pbm_fn (Pbm.bitindices_to_pbm (dm,filter f (Bitmap.bm_indices dm)))

{- | Au to Pgm

> let fn = "/home/rohan/sw/hsc3-sf/au/mc-4-16.au"
> au_to_pgm 8 fn (fn ++ ".pgm")
-}
au_to_pgm :: Int -> FilePath -> FilePath -> IO ()
au_to_pgm depth au_fn pgm_fn = do
  (hdr,vec) <- Sf.Au.au_read_f32_vec au_fn
  let dm = (Sf.Au.channelCount hdr,Sf.Au.frameCount hdr)
      img = Pgm.pgmf_from_vec_co dm vec
  Pgm.pgm5_save_0 pgm_fn (Pgm.pgmf_to_pgm depth img)

-- * Csv

load_mnd_wseq :: FilePath -> IO (Seq.Wseq Double (Midi.Mnd.Event Double))
load_mnd_wseq = fmap Midi.Mnd.midi_tseq_to_midi_wseq . Midi.Mnd.csv_mnd_read_tseq

wseq_to_pgm :: (Int,Int) -> Seq.Wseq Double (Midi.Mnd.Event Double) -> Pgm.Pgm
wseq_to_pgm (w,h) sq =
  let (_,et) = Seq.wseq_tspan sq
      tm_incr = et / fromIntegral w
      tm_seq = take w (List.adj2 1 [0,tm_incr ..])
      nd = zip [0..] (map (Seq.wseq_at_window sq) tm_seq)
      to_y mnn = h - 1 - floor (Sc3.linlin_ma Sc3.sc3_mul_add mnn 21 108 0 (fromIntegral h))
      to_grey = floor . (* 255) . (/ 127)
      to_entry (x,(_,(mnn,vel,_,_))) = ((to_y mnn,x),to_grey vel)
      uncollate (k,v) = zip (repeat k) v
      pgm = Pgm.pgm_from_list (h,w) (concatMap (map to_entry . uncollate) nd)
  in Pgm.pgm_invert pgm

{- | Mnd to Png

> let c_fn = "/home/rohan/sw/hmt/csv/mnd/1080-C01.csv"
> let c_fn = "/home/rohan/uc/sp-id/csv/music/ngv/s-gyrostasis.plain.csv"
> csv_mnd_to_pgm (1200,200) c_fn "/tmp/t.pgm"
-}
csv_mnd_to_pgm :: (Int,Int) -> FilePath -> FilePath -> IO ()
csv_mnd_to_pgm (w,h) csv_fn pgm_fn = do
  sq <- load_mnd_wseq csv_fn
  Pgm.pgm5_save_0 pgm_fn (wseq_to_pgm (w,h) sq)

csv_load :: (String -> n) -> FilePath -> IO [[n]]
csv_load f fn = do
  s <- readFile fn
  let t = Csv.fromCSVTable (Csv.csvTable (Csv.parseCSV s))
  return (map (map f) t)

csv_load_double :: FilePath -> IO [[Double]]
csv_load_double = csv_load read

csv_load_double_round :: FilePath -> IO [[Int]]
csv_load_double_round = fmap (map (map round)) . csv_load_double

csv_to_indices :: (Int,Int) -> [[t]] -> [(t, t)]
csv_to_indices (i,j) =
    let f r = (r !! i,r !! j)
    in map f

{- | Csv to Image (Point, Real, Pbm)

> let csv_fn = "/home/rohan/cvs/uc/uc-26/daily-practice/2016-07-06/pt-2-20.csv"
> csv_to_image_point_real_pbm csv_fn (1001,1001) (0,1) "/tmp/pt-2-20.pbm"
-}
csv_to_image_point_real_pbm :: FilePath -> Bitmap.Dimensions -> (Int,Int) -> FilePath -> IO ()
csv_to_image_point_real_pbm csv_fn dm ix pbm_fn = do
  dat <- csv_load_double_round csv_fn
  let ind = csv_to_indices ix dat
  Pbm.write_pbm_bitindices pbm_fn (dm,ind)

-- * Hex

id_w8_seq :: [Word8] -> [Word8]
id_w8_seq = id

-- * Image

{- | Query Unique

> mapM_ (\n -> image_query_unique 'c' ("/home/rohan/rd/j/2017-11-30/G." ++ show n ++ ".png")) [0 .. 7]
-}
image_query_unique :: Char -> FilePath -> IO ()
image_query_unique md fn = do
  i <- Image.Plain.img_load fn
  let f (ix,c) = (ix,Image.Plain.rgb24_unpack c)
  case md of
    'c' -> print (map (Image.Plain.rgb24_unpack . snd) (Image.Plain.img_uniq_colours i))
    'l' -> print (map f (Image.Plain.img_uniq_colours i))
    'g' -> print (map (map f) (Image.Plain.img_uniq_colours_gr i))
    _ -> error "image_query_unique"

img_to_pbm :: (Image.Plain.Rgb24 -> Image.Plain.Bw) -> FilePath -> FilePath -> IO ()
img_to_pbm to_bw img_fn pbm_fn = do
  i <- Image.Plain.img_load img_fn
  Image.Plain.img_bw_write_pbm4 to_bw pbm_fn i

img_to_pgm :: Int -> (Image.Plain.Rgb24 -> Image.Plain.Grey) -> FilePath -> FilePath -> IO ()
img_to_pgm depth to_grey img_fn pgm_fn = do
  i <- Image.Plain.img_load img_fn
  Image.Plain.img_write_pgm5 depth to_grey pgm_fn i

-- * Pbm

pbm_load_indices :: FilePath -> IO Bitmap.Indices
pbm_load_indices pbm_fn = do
  pbm <- Pbm.read_pbm pbm_fn
  let (_,ix) = Pbm.pbm_to_bitindices pbm
  return ix

{- | Pbm Indices (Csv)

> pbm_indices_csv "/home/rohan/sw/hsc3-data/data/pbm/fh.pbm" "/dev/stdout"
-}
pbm_indices_csv :: FilePath -> FilePath -> IO ()
pbm_indices_csv pbm_fn csv_fn = do
  ix <- pbm_load_indices pbm_fn
  let f (r,c) = show r ++ "," ++ show c
  writeFile csv_fn (unlines ("r,c" : map f ix))

{- | Pbm Indices (Json)

> pbm_indices_json "/home/rohan/sw/hsc3-data/data/pbm/fh.pbm" "/dev/stdout"
-}
pbm_indices_json :: FilePath -> FilePath -> IO ()
pbm_indices_json pbm_fn json_fn = do
  ix <- pbm_load_indices pbm_fn
  let to_array l = "[" ++ intercalate "," l ++ "]"
      f (r,c) = to_array [show r,show c]
  writeFile json_fn (to_array (map f ix))

type PbmToCsv =
  ((Double,Double,Maybe FilePath,Bool,Bool) -- midi note number
  ,(Double,Maybe FilePath) -- time
  ,(Double,Maybe FilePath) -- duration
  ,(Double,Maybe FilePath)) -- velocity

-- | Read Table from SoundFile.
sf_tbl_rd :: Maybe FilePath -> IO [Double]
sf_tbl_rd fn =
    case fn of
      Nothing -> return [1]
      Just fn' -> fmap (List.head_err . snd) (Sf.SndFile.read fn')

{- | Pbm to Csv/Mnd

nc = number of columns (width), nr = number of rows (height)
-}
pbm_to_csv_mnd :: PbmToCsv -> FilePath -> FilePath -> IO ()
pbm_to_csv_mnd opt pbm_fn csv_fn = do
  let ((mnn,mnn_incr,mnn_tbl,inv,le),(tm_incr,tm_tbl),(du,du_tbl),(vel,vel_tbl)) = opt
  i <- Pbm.read_pbm pbm_fn
  tm_mod <- sf_tbl_rd tm_tbl
  vel_mod <- sf_tbl_rd vel_tbl
  du_mod <- sf_tbl_rd du_tbl
  mnn_mod <- sf_tbl_rd mnn_tbl
  let ((nr,nc),bi') = let z = Pbm.pbm_to_bitindices i
                      in if le
                         then Bitmap.bitindices_leading_edges Bitmap.Dir_Right z
                         else z
      bi = sortBy (compare `on` snd) bi'
      mnn_sq = let sq = zipWith (*) [mnn,mnn + mnn_incr .. ] (Sc3.Common.Buffer.resamp1 nr mnn_mod)
               in if inv then reverse sq else sq
      tm_sq = List.dx_d 0 (map (* tm_incr) (Sc3.Common.Buffer.resamp1 nc tm_mod))
      vel_sq = map (* vel) (Sc3.Common.Buffer.resamp1 nc vel_mod)
      du_sq = map (* du) (Sc3.Common.Buffer.resamp1 nc du_mod)
      f (y,x) = ((tm_sq !! x,du_sq !! y),(mnn_sq !! y,vel_sq !! x,0,[]))
  putStrLn (unwords . map (Show.double_pp 3) $ mnn_sq)
  Midi.Mnd.csv_mnd_write_tseq 4 csv_fn (Midi.Mnd.midi_wseq_to_midi_tseq (map f bi))

pbm_to_csv_mnd_cli :: [String] -> IO ()
pbm_to_csv_mnd_cli arg =
  case arg of
    [mnn,mnn_incr,mnn_tbl,inv,le,tm_incr,tm_tbl,du,du_tbl,gn,gn_tbl,pbm_fn,csv_fn] ->
        let rd_tbl x = if x == "nil" then Nothing else Just x
            opt = ((read mnn,read mnn_incr,rd_tbl mnn_tbl,inv == "inv",le == "le")
                  ,(read tm_incr,rd_tbl tm_tbl)
                  ,(read du,rd_tbl du_tbl)
                  ,(read gn,rd_tbl gn_tbl))
        in pbm_to_csv_mnd opt pbm_fn csv_fn
    _ -> putStrLn (unlines help)

{- | Pbm to Table

> pbm_to_tbl Lang.Math.Statistics.mean True "/home/rohan/uc/sp-id/eof/pbm/gs/02.pbm" "/tmp/t.au"
-}
pbm_to_tbl :: Num n => ([n] -> Double) -> Bool -> FilePath -> FilePath -> IO ()
pbm_to_tbl avg_f nrm pbm_fn au_fn = do
  i <- Pbm.read_pbm pbm_fn
  let (nc,nr) = Pbm.pbm_dimensions i
      nrm_f = if nrm then Sc3.Common.Buffer.normalise_rng (0,fromIntegral nc - 1) (0,1) else id
  print ("(w/nc,h/nr)",(nc,nr))
  let tbl = nrm_f .
            map snd .
            List.fill_gaps_ascending 0 (0,nc - 1) .
            map (fmap (avg_f . map fromIntegral)) .
            List.collate_on snd fst .
            snd .
            Pbm.pbm_to_bitindices $ i
  Sf.SndFile.write au_fn (Sf.SndFile.Sf_Header 1 nc 1 Sf.SndFile.fmt_au_f32_be) [tbl]
  return ()

-- * Pgm

pgm_to_au :: FilePath -> FilePath -> IO ()
pgm_to_au pgm_fn au_fn = do
  img <- Pgm.pgm_load_0 pgm_fn
  let (nr,nc) = Pgm.pgm_dimensions img
      img' = Pgm.pgm_to_pgmf img
      hdr = Sf.Au.Sf_Header nc Sf.Au.Float 1 nr
      v = Pgm.pgmf_to_vec (nr,nc) img'
  Sf.Au.au_write_f32_vec au_fn (hdr,v)

-- * Main

read_double :: String -> Double
read_double = read

help :: [String]
help =
  ["ats"
  ,"    header file-name"
  ,"    write-au ats-file au-file"
  ,"au"
  ,"    to-pbm au-file pbm-file"
  ,"    to-pgm au-file pgm-file"
  ,"csv"
  ,"    mnd-to-midi remove-overlaps:bool tempo:int time-signature:int/int csv-file midi-file"
  ,"    mnd-to-pgm width:int height:int csv-file pgm-file"
  ,"    to-image point real pbm csv-file height:int width:int y-index x-index pbm-file"
  ,"hex"
  ,"    encode text-file binary-file"
  ,"    decode binary-file text-file"
  ,"image"
  ,"    query unique mode=(c|l|g) img-file"
  ,"    to-pbm convert=(eq|lm/rec.709) threshold?:real img-file pbm-file"
  ,"    to-pgm depth=(8|16) convert=(eq|lm/rec.709) img-file pbm-file"
  ,"pbm"
  ,"    indices csv pbm-file csv-file"
  ,"    indices json pbm-file json-file"
  ,"    to-csv-mnd mnn mnn+ mnn~ inv le tm+ tm~ du du~ gn gn~ pbm-file csv-file"
  ,"    to-tbl mode:(median|mean) normalise:bool image-file au-file"
  ,"pgm"
  ,"    to-au pgm-file au-file"
  ]

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["ats","header",fn] -> ats_header fn
    ["ats","write-au",ats_fn,au_fn] -> Ats.ats_write_au ats_fn au_fn
    ["au","to-pbm",au_fn,pbm_fn] -> au_to_pbm au_fn pbm_fn
    ["au","to-pgm",depth,au_fn,pgm_fn] -> au_to_pgm (read depth) au_fn pgm_fn
    ["csv","mnd-to-midi",rw,tc,ts_n,ts_d,fn1,fn2] -> Midi.Plain.cvs_mnd_to_midi0 (read rw) (read tc) (read ts_n,read ts_d) fn1 fn2
    ["csv","mnd-to-pgm",w,h,csv_fn,pgm_fn] -> csv_mnd_to_pgm (read w,read h) csv_fn pgm_fn
    ["csv","to-image","point","real","pbm",csv_fn,h,w,i,j,pbm_fn] -> csv_to_image_point_real_pbm csv_fn (read h,read w) (read i,read j) pbm_fn
    ["hex","decode",b_fn,t_fn] -> Byte.load_byte_seq b_fn >>= Byte.store_hex_byte_seq t_fn . return . id_w8_seq
    ["hex","encode",t_fn,b_fn] -> Byte.load_hex_byte_seq t_fn >>= Byte.store_byte_seq b_fn . id_w8_seq . List.unlist1_err
    ["image","query","uniq",[mode],fn] -> image_query_unique mode fn
    ["image","to-pbm","eq",img_fn,pbm_fn] -> img_to_pbm Image.Plain.rgb24_to_bw_eq' img_fn pbm_fn
    ["image","to-pbm","lm/rec.709",th,img_fn,pbm_fn] -> let f = (< (read_double th)) . Image.Plain.rgb_to_gs_rec_709 in img_to_pbm f img_fn pbm_fn
    ["image","to-pgm",d,"eq",img_fn,pgm_fn] -> img_to_pgm (read d) Image.Plain.rgb24_to_gs_eq' img_fn pgm_fn
    ["image","to-pgm",d,"lm/rec.709",img_fn,pgm_fn] -> img_to_pgm (read d) Image.Plain.rgb_to_gs_rec_709 img_fn pgm_fn
    ["pbm","indices","csv",pbm_fn,csv_fn] -> pbm_indices_csv pbm_fn csv_fn
    ["pbm","indices","json",pbm_fn,json_fn] -> pbm_indices_json pbm_fn json_fn
    "pbm":"to-csv-mnd":rest -> pbm_to_csv_mnd_cli rest
    ["pbm","to-table",mode,nrm,pbm_fn,au_fn] -> pbm_to_tbl (Lang.Math.Statistics.parse_averaging_f mode) (read nrm) pbm_fn au_fn
    ["pgm","to-au",pgm_fn,au_fn] -> pgm_to_au pgm_fn au_fn
    _ -> putStrLn (unlines help)
