import qualified Control.Exception {- base -}
import qualified System.Environment {- base -}
import qualified System.IO {- base -}

import qualified UI.HSCurses.Curses as Curses {- hscurses -}

set_ln :: Curses.Window -> Int -> String -> IO ()
set_ln w n s = do
  Curses.move n 0
  Curses.wClrToEol w
  Curses.wAddStr w s
  Curses.addLn
  Curses.refresh

recv_ln :: Char -> Curses.Window -> String -> IO ()
recv_ln c w s = do
  let (n, _) = break (== c) s
  case reads n of
    [(n', _)] -> set_ln w n' s
    _ -> return ()

text_monitor :: Char -> Curses.Window -> IO ()
text_monitor c w = do
  s <- getLine
  recv_ln c w s
  text_monitor c w

main :: IO ()
main = do
  a <- System.Environment.getArgs
  let c = case a of
        ["stdin", "space"] -> ' '
        ["stdin", "comma"] -> ','
        _ -> error "text-monitor stdin space|comma"
  System.IO.hSetBuffering System.IO.stdin System.IO.LineBuffering
  Curses.initCurses
  w <- Curses.initScr
  Control.Exception.finally (text_monitor c w) Curses.endWin
