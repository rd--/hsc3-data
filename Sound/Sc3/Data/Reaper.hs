-- | Reaper utilities
module Sound.Sc3.Data.Reaper where

import Text.Printf {- base -}

-- * Reaper Channel Map File

-- | One-indexed channel number.
type ChannelNumber = Int

-- | Display name for channel.
type ChannelName = String

-- | Triple of (UserNumber, UserName, SystemNumber)
type ChannelMapping = (ChannelNumber, ChannelName, ChannelNumber)

-- | List of channel mappings.
type ChannelMap = [ChannelMapping]

-- | Print ChannelMapping
channelMappingPp :: ChannelMapping -> [String]
channelMappingPp (user, name, system) =
  [printf "ch%d=%d" (user - 1) (system - 1)
  ,printf "name%d=%s" (user - 1) name]

{- | Print ChannelMap

>>> putStr $ channelMapPp [(1, "Left", 3), (2, "Centre", 2), (3, "Right", 1)]
[reaper_chanmap]
map_hwnch=3
map_size=3
ch0=2
name0=Left
ch1=1
name1=Centre
ch2=0
name2=Right
<BLANKLINE>
-}
channelMapPp :: ChannelMap -> String
channelMapPp l =
  let header = "[reaper_chanmap]"
      meta = printf "map_hwnch=%d\nmap_size=%d" (length l) (length l)
      entries = unlines (concatMap channelMappingPp l)
  in unlines [header, meta, entries]
