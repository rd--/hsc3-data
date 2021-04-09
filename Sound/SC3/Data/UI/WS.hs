-- | WS = WEBSOCKET
module Sound.SC3.Data.UI.WS where

import Text.Printf {- base -}

import qualified Data.Text as Text {- text -}
import qualified Network.WebSockets as WS {- websockets -}

-- | Stateful variant of 'Control.Monad.forever'.
m_recur :: Monad m => t -> (t -> m t) -> m ()
m_recur st0 f = do
  st <- f st0
  m_recur st f

-- | (ST,(ELEM-IX,VALUE-IX)) -> IO ST
type WS_RECV_F st = (st,(Int,Int)) -> IO st

ws_string_to_ix :: String -> (Int,Int)
ws_string_to_ix str =
  case words str of
    ["P",d1,d2] -> (read d1,read d2)
    _ -> error "ws_string_to_ix?"

-- | Format (d1,d1) as "P d1 d2"
ws_ix_fmt :: (Int,Int) -> Text.Text
ws_ix_fmt (d1,d2) = Text.pack (printf "P %d %d" d1 d2)

-- | Send IX.
ws_ix_send :: WS.Connection -> (Int,Int) -> IO ()
ws_ix_send c ix = WS.sendTextData c (ws_ix_fmt ix)

ws_incoming_f :: st -> WS_RECV_F st -> WS.Connection -> IO ()
ws_incoming_f st0 recv_f c = do
  let recur_f st = WS.receiveData c >>= (\x -> recv_f (st,x)) . ws_string_to_ix . Text.unpack
  m_recur st0 recur_f

ws_server_f :: (WS.Connection -> IO ()) -> WS.ServerApp
ws_server_f f rq = WS.acceptRequest rq >>= f

-- > ws_reader 9160 () (\(_,x) -> print x)
ws_reader :: Int -> st -> WS_RECV_F st -> IO ()
ws_reader p st recv_f = WS.runServer "127.0.0.1" p (ws_server_f (ws_incoming_f st recv_f))
