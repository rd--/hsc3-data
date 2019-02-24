-- | Yamaha TX816
module Sound.SC3.Data.Yamaha.TX816 where

-- | The DX7 does not have patch request SYSEX messages, the TX816 does.
tx816_request_data :: Num t => t -> t -> [t]
tx816_request_data ch cmd = [0xF0,0x43,0x2 + ch,cmd,0xF7]

-- | Table of patch request SYSEX command numbers.
tx816_data_request_tbl :: Num t => [(t,String)]
tx816_data_request_tbl =
  [(0x00,"current voice patch")
  ,(0x01,"current performance bulk data")
  ,(0x02,"64 performance bulk data")
  ,(0x09,"32 voice bulk data")
  ,(0x7D,"acknowledge")]
