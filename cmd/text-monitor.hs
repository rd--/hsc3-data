import Control.Exception {- base -}
import System.Environment {- base -}
import System.IO {- base -}
import UI.HSCurses.Curses {- hscurses -}

set_ln :: Window -> Int -> String -> IO ()
set_ln w n s = do
  move n 0
  wClrToEol w
  wAddStr w s
  addLn
  refresh

recv_ln :: Char -> Window -> String -> IO ()
recv_ln c w s = do
  let (n, _) = break (== c) s
  case reads n of
    [(n', _)] -> set_ln w n' s
    _ -> return ()

text_monitor :: Char -> Window -> IO ()
text_monitor c w = do
  s <- getLine
  recv_ln c w s
  text_monitor c w

main :: IO ()
main = do
  a <- getArgs
  let c = case a of
        ["stdin", "space"] -> ' '
        ["stdin", "comma"] -> ','
        _ -> error "text-monitor stdin space|comma"
  hSetBuffering stdin LineBuffering
  initCurses
  w <- initScr
  finally (text_monitor c w) endWin
