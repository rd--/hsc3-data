import qualified Music.Theory.Array.CSV as T {- hmt -}
import qualified Music.Theory.Math as T {- hmt -}

prj_file :: FilePath -> FilePath
prj_file = (++) "/home/rohan/sw/hsc3-data/data/"

csv_trace_b :: [[Double]]
csv_trace_b =
    let ph = [-pi,-pi + 0.01 .. pi]
        f0 n d = sin . (+) d . (*) n
        f1 t = [f0 1 (pi/2) t,f0 3 0 t,f0 5 0 t]
    in map f1 ph

trace_degree :: [[x]] -> Int
trace_degree = length . head

write_trace_csv :: FilePath -> [[Double]] -> IO ()
write_trace_csv fn tr =
    let n = trace_degree tr
        ltr = "xyz" ++ "abcdefghijklmnopqrstuvw"
        hdr = "tm" : take (n - 1) (map return ltr)
    in T.csv_table_write (T.double_pp 4) (True,',',False,T.CSV_No_Align) fn (Just hdr,tr)

main :: IO ()
main = write_trace_csv (prj_file "csv/trace/b.csv") csv_trace_b
