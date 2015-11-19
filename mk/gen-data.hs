import qualified Music.Theory.Array.CSV as T {- hmt -}
import qualified Music.Theory.Math as T {- hmt -}
--import Sound.SC3 {- hsc3 -}

prj_file :: FilePath -> FilePath
prj_file = (++) "/home/rohan/sw/hsc3-data/data/"

csv_xyz_a :: [[Double]]
csv_xyz_a =
    let ph = [-pi,-pi + 0.01 .. pi]
        f0 n d = sin . (+) d . (*) n
        f1 t = [f0 1 (pi/2) t,f0 3 0 t,f0 5 0 t]
    in map f1 ph

csv_trace_b :: [[Double]]
csv_trace_b =
    let r = 24 * pi
        f0 ph = sin ph * (ph / r)
        f1 ph = sin (ph + pi/2) * (ph / r * 2) + (ph * 4 / r)
    in map (\ph -> [ph,f0 ph,f1 ph]) [0,0.025 .. r]

tbl_degree :: [[x]] -> Int
tbl_degree = length . head

write_tbl_csv :: FilePath -> ([String],[[Double]]) -> IO ()
write_tbl_csv fn (hdr,dat) =
    let opt =  (True,',',False,T.CSV_No_Align)
    in T.csv_table_write (T.double_pp 5) opt fn (Just hdr,dat)

write_trace_csv :: FilePath -> [[Double]] -> IO ()
write_trace_csv fn tr =
    let n = tbl_degree tr
        ltr = "xyz" ++ "abcdefghijklmnopqrstuvw"
        hdr = "tm" : take (n - 1) (map return ltr)
    in write_tbl_csv fn (hdr,tr)

xyz_hdr :: [String]
xyz_hdr = map return "xyz"

main :: IO ()
main = do
  write_tbl_csv (prj_file "csv/xyz/a.csv") (xyz_hdr,csv_xyz_a)
  write_trace_csv (prj_file "csv/trace/b.csv") csv_trace_b
