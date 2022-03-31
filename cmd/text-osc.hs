import Control.Monad {- base -}
import Control.Exception {- base -}

import UI.HSCurses.Curses {- hscurses -}

import Music.Theory.Opt {- hmt-base -}

import Sound.Osc.Fd {- hosc -}

set_ln :: Window -> Int -> String -> IO ()
set_ln w n s = move n 0 >> wClrToEol w >> wAddStr w s >> addLn >> refresh

set_str :: Window -> (Int,Int) -> String -> IO ()
set_str w (x,y) s = move x y >> wAddStr w s >> refresh

proc_msg :: Window -> Message -> IO ()
proc_msg w m =
    case m of
      Message "/set_ln" [Int32 n,AsciiString str] ->
          set_ln w (fromIntegral n) (ascii_to_string str)
      Message "/set_str" [Int32 x,Int32 y,AsciiString str] ->
          set_str w (fromIntegral x,fromIntegral y) (ascii_to_string str)
      _ -> return ()

help :: [String]
help = ["text-osc"]

opt_def :: [OptUsr]
opt_def = [("port","57350","int","UDP port number")]

main :: IO ()
main = do
  (o,_a) <- opt_get_arg True help opt_def
  initCurses
  w <- initScr
  let f fd = forever (recvMessage fd >>= maybe (return ()) (proc_msg w))
      t = udpServer "127.0.0.1" (opt_read o "port")
  finally (withTransport t f) endWin

{-
fd <- openUDP "127.0.0.1" 57350
sendMessage fd (Message "/set_ln" [int32 0,string ['a'..'z']])
sendMessage fd (Message "/set_str" [int32 0,int32 2,string ['C'..'G']])
close fd
-}
