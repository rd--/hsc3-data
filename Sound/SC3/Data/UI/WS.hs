-- | WS = WEBSOCKET
module Sound.SC3.Data.UI.WS where

import Text.Printf {- base -}

import qualified Data.Text as Text {- text -}
import qualified Network.WebSockets as W {- websockets -}

import qualified Sound.OSC.Type.JSON as J {- hosc-json -}

-- | Stateful variant of 'Control.Monad.forever'.
m_recur :: Monad m => t -> (t -> m t) -> m ()
m_recur st0 f = do
  st <- f st0
  m_recur st f

-- | (ST,(ELEM-IX,VALUE-IX)) -> IO ST
type WS_RECV_F st = (st,(Int,Int)) -> IO st

ws_value_to_ix :: J.Value -> (Int,Int)
ws_value_to_ix v =
  case J.value_to_list_err v of
    [ty,d1,d2] ->
      case J.value_to_string_err ty of
        "/P" -> (J.value_to_int_err d1,J.value_to_int_err d2)
        _ -> error "ws_ix?"
    _ -> error "ws_ix?"

-- | Format (d1,d1) as JSON ["/P",d1,d2]
ws_ix_fmt :: (Int,Int) -> Text.Text
ws_ix_fmt (d1,d2) = Text.pack (printf "[\"/P\",%d,%d]" d1 d2)

-- | Send IX.
ws_ix_send :: W.Connection -> (Int,Int) -> IO ()
ws_ix_send c ix = W.sendTextData c (ws_ix_fmt ix)

ws_incoming_f :: st -> WS_RECV_F st -> W.Connection -> IO ()
ws_incoming_f st0 recv_f c = do
  let recur_f st = W.receiveData c >>= (\x -> recv_f (st,x)) . ws_value_to_ix . J.json_decode_value_err
  m_recur st0 recur_f

ws_server_f :: (W.Connection -> IO ()) -> W.ServerApp
ws_server_f f rq = W.acceptRequest rq >>= f

-- > ws_reader 9160 () (\(_,x) -> print x)
ws_reader :: Int -> st -> WS_RECV_F st -> IO ()
ws_reader p st recv_f = W.runServer "127.0.0.1" p (ws_server_f (ws_incoming_f st recv_f))
