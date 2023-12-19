-- | Yamaha Tx816
module Sound.Sc3.Data.Yamaha.Dx7.Tx816 where

-- | The Dx7 does not have patch request SYSEX messages, the Tx816 does.
tx816_request_data :: Num t => t -> t -> [t]
tx816_request_data ch cmd = [0xF0, 0x43, 0x2 + ch, cmd, 0xF7]

-- | Table of patch request SYSEX command numbers.
tx816_data_request_tbl :: Num t => [(t, String)]
tx816_data_request_tbl =
  [ (0x00, "CURRENT VOICE PATCH")
  , (0x01, "CURRENT PERFORMANCE BULK DATA")
  , (0x02, "64 PERFORMANCE BULK DATA")
  , (0x09, "32 VOICE BULK DATA")
  , (0x7D, "ACKNOWLEDGE")
  ]
