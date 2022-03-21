-- | Yamaha DX7 & SuperCollider
module Sound.SC3.Data.Yamaha.DX7.SC3 where

import Data.Word {- base -}

import Sound.Osc.Core {- hosc -}
import Sound.SC3 {- hsc3 -}

import qualified Sound.SC3.Server.Command.Plain as SC3 {- hsc3 -}

-- | Convert DX7 patch (ie. sequence of 'Word8') sequence data to a
--   sequence of 'OSC' messages.  The OSC messages will load the patch
--   data into the indicated buffer.
dx7_sc3_data_msg_seq :: SC3.Buffer_Id -> [[Word8]] -> [Message]
dx7_sc3_data_msg_seq buf = SC3.b_setn1_segmented 256 buf 0 . map fromIntegral . concat

-- | Allocate buffer and load in DX7 patch data.
dx7_sc3_data_load :: Buffer_Id -> [[Word8]] -> IO ()
dx7_sc3_data_load buf dat = do
  let alloc_msg = SC3.b_alloc buf (sum (map length dat)) 1
  withSC3 (mapM_ maybe_async (alloc_msg : dx7_sc3_data_msg_seq buf dat))
