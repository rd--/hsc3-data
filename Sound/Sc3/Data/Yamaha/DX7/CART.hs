-- | DX7 CART
module Sound.Sc3.Data.Yamaha.DX7.CART where

-- | DX7 ROM cartridges as (cartridge-number,bank-letter,description)
dx7_rom_tbl :: [(Int,Char,String)]
dx7_rom_tbl =
  [(1,'A',"MASTER")
  ,(1,'B',"KEYBOARD AND PLUCKED SOUNDS")
  ,(2,'A',"ORCHESTRAL & PERCUSSIVE SOUNDS")
  ,(2,'B',"SYNTH, COMPLEX & EFFECTS SOUNDS")
  ,(3,'A',"MASTER")
  ,(3,'B',"KEYBOARD & PLUCKED SOUNDS")
  ,(4,'A',"ORCHESTRAL & PERCUSSIVE SOUNDS")
  ,(4,'B',"COMPLEX SOUND & EFFECTS")]

-- | DX7 ROM cartridge to SYSEX file name.
--
-- > map dx7_rom_syx_name dx7_rom_tbl
dx7_rom_syx_name :: (Int,Char,String) -> String
dx7_rom_syx_name (p,q,_) = "DX7-ROM" ++ show p ++ [q]

-- | DX7 VRC-Series cartridges
dx7_vrc_tbl :: [(Int,String)]
dx7_vrc_tbl =
  [(101,"Keyboard, Plucked & Tuned Percussion Group")
  ,(102,"Wind Instrument Group")
  ,(103,"Sustain Group")
  ,(104,"Percussion Group")
  ,(105,"Sound Effect Group")
  ,(106,"Synthesizer Group")
  ,(107,"Special Selection - David Bristow")
  ,(108,"Special Selection - Gary Leuenberger")
  ,(109,"Studio 64")
  ,(110,"Special Selection - Bo Tomlyn")
  ,(111,"Special Selection - Bo Tomlyn II")
  ,(112,"Live 64")]

-- | DX7 VRC cartridge to SYSEX file name.
--
-- > concatMap dx7_vrc_syx_name dx7_vrc_tbl
dx7_vrc_syx_name :: (Int,String) -> [String]
dx7_vrc_syx_name (p,_) = map (\c -> "VRC-" ++ show p ++ ['-',c]) "AB"

