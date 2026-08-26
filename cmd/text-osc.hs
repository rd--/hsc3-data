import qualified Control.Exception {- base -}
import qualified Control.Monad {- base -}

import qualified UI.HSCurses.Curses as Curses {- hscurses -}

import qualified Music.Theory.Opt as Opt {- hmt-base -}

import qualified Sound.Osc.Fd as Osc {- hosc -}
import qualified Sound.Osc.Transport.Fd.Udp as Osc.Fd.Udp {- hosc -}

set_ln :: Curses.Window -> Int -> String -> IO ()
set_ln w n s = do
  Curses.move n 0
  Curses.wClrToEol w
  Curses.wAddStr w s
  Curses.addLn
  Curses.refresh

set_str :: Curses.Window -> (Int, Int) -> String -> IO ()
set_str w (x, y) s = do
  Curses.move x y
  Curses.wAddStr w s
  Curses.refresh

proc_msg :: Curses.Window -> Osc.Message -> IO ()
proc_msg w m =
  case m of
    Osc.Message "/set_ln" [Osc.Int32 n, Osc.AsciiString str] ->
      set_ln w (fromIntegral n) (Osc.ascii_to_string str)
    Osc.Message "/set_str" [Osc.Int32 x, Osc.Int32 y, Osc.AsciiString str] ->
      set_str w (fromIntegral x, fromIntegral y) (Osc.ascii_to_string str)
    _ -> return ()

help :: [String]
help = ["text-osc"]

opt_def :: [Opt.OptUsr]
opt_def = [("port", "57350", "int", "Udp port number")]

main :: IO ()
main = do
  (o, _a) <- Opt.opt_get_arg True help opt_def
  Curses.initCurses
  w <- Curses.initScr
  let f fd = Control.Monad.forever (Osc.recvMessage fd >>= maybe (return ()) (proc_msg w))
      t = Osc.Fd.Udp.udpServer "127.0.0.1" (Opt.opt_read o "port")
  Control.Exception.finally (Osc.withTransport t f) Curses.endWin

{-
fd <- Osc.Fd.Udp.openUdp "127.0.0.1" 57350
Osc.sendMessage fd (Osc.Message "/set_ln" [Osc.int32 0,Osc.string ['a'..'z']])
Osc.sendMessage fd (Osc.Message "/set_str" [Osc.int32 0,Osc.int32 2,Osc.string ['C'..'G']])
Osc.close fd
-}
