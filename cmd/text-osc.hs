import Control.Monad {- base -}
import Control.Exception {- base -}
import Data.Maybe {- base -}
import System.Environment {- base -}
import System.Exit {- base -}

import UI.HSCurses.Curses {- hscurses -}

import Sound.OSC.FD {- hosc -}

set_ln :: Window -> Int -> String -> IO ()
set_ln w n s = move n 0 >> wClrToEol w >> wAddStr w s >> addLn >> refresh

set_str :: Window -> (Int,Int) -> String -> IO ()
set_str w (x,y) s = move x y >> wAddStr w s >> refresh

proc_msg :: Window -> Message -> IO ()
proc_msg w m =
    case m of
      Message "/set_ln" [Int32 n,ASCII_String str] ->
          set_ln w (fromIntegral n) (ascii_to_string str)
      Message "/set_str" [Int32 x,Int32 y,ASCII_String str] ->
          set_str w (fromIntegral x,fromIntegral y) (ascii_to_string str)
      _ -> return ()

usage :: IO ()
usage = putStrLn "text-osc [-p port-number]" >> exitSuccess

main :: IO ()
main = do
  a <- getArgs
  when (has_opt "-h" a) usage
  let p' = fromMaybe 57350 (get_opt_arg_read "-p" a)
  initCurses
  w <- initScr
  let f fd = forever (recvMessage fd >>= maybe (return ()) (proc_msg w))
      t = udpServer "127.0.0.1" p'
  finally (withTransport t f) endWin

-- ...

has_opt :: Eq a => a -> [a] -> Bool
has_opt nm a = nm `elem` a

get_opt_arg :: Eq a => a -> [a] -> Maybe a
get_opt_arg nm a =
    case a of
      [p] -> if p == nm then error "get_opt_arg: no arg" else Nothing
      p:q:a' -> if p == nm then Just q else get_opt_arg nm (q:a')
      _ -> Nothing

get_opt_arg_read :: Read a => String -> [String] -> Maybe a
get_opt_arg_read nm = fmap read . get_opt_arg nm

{-
fd <- openUDP "127.0.0.1" 57350
sendMessage fd (Message "/set_ln" [int32 0,string ['a'..'z']])
sendMessage fd (Message "/set_str" [int32 0,int32 2,string ['C'..'G']])
close fd
-}
