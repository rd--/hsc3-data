{- base -}
import Control.Exception {- base -}
import Control.Monad

import UI.HSCurses.Curses {- hscurses -}

import qualified Music.Theory.Opt as Opt {- hmt-base -}

import Sound.Osc.Fd {- hosc -}
import qualified Sound.Osc.Transport.Fd.Udp as Fd.Udp {- hosc -}

set_ln :: Window -> Int -> String -> IO ()
set_ln w n s = move n 0 >> wClrToEol w >> wAddStr w s >> addLn >> refresh

set_str :: Window -> (Int, Int) -> String -> IO ()
set_str w (x, y) s = move x y >> wAddStr w s >> refresh

proc_msg :: Window -> Message -> IO ()
proc_msg w m =
  case m of
    Message "/set_ln" [Int32 n, AsciiString str] ->
      set_ln w (fromIntegral n) (ascii_to_string str)
    Message "/set_str" [Int32 x, Int32 y, AsciiString str] ->
      set_str w (fromIntegral x, fromIntegral y) (ascii_to_string str)
    _ -> return ()

help :: [String]
help = ["text-osc"]

opt_def :: [Opt.OptUsr]
opt_def = [("port", "57350", "int", "Udp port number")]

main :: IO ()
main = do
  (o, _a) <- Opt.opt_get_arg True help opt_def
  initCurses
  w <- initScr
  let f fd = forever (recvMessage fd >>= maybe (return ()) (proc_msg w))
      t = Fd.Udp.udpServer "127.0.0.1" (Opt.opt_read o "port")
  finally (withTransport t f) endWin

{-
fd <- openUDP "127.0.0.1" 57350
sendMessage fd (Message "/set_ln" [int32 0,string ['a'..'z']])
sendMessage fd (Message "/set_str" [int32 0,int32 2,string ['C'..'G']])
close fd
-}
