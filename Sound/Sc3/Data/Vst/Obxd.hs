-- | <https://github.com/2DaT/Obxd>
module Sound.Sc3.Data.Vst.Obxd where

import qualified Control.Monad {- base -}
import qualified Data.List {- base -}
import qualified Data.Word {- base -}

import qualified Data.ByteString as ByteString {- bytestring -}
import qualified Data.List.Split as Split {- split -}
import qualified Text.XML.Light as Xml {- xml -}
import qualified Text.XML.Light.Lexer as Xml.Lexer {- xml -}

import qualified Music.Theory.Byte as Byte {- hmt-base -}

import qualified Sound.Sc3.Common.Math as Sc3 {- hsc3 -}

import qualified Sound.Sc3.Data.Vst as Vst {- hsc3-data -}
import qualified Sound.Sc3.Data.Xml as Xml {- hsc3-data -}

-- * Fxb / Io

-- | Load Obxd Fxb file, returns the number of progams and the Xml data.
obxd_fxb_load_xml :: FilePath -> IO (Data.Word.Word32, String)
obxd_fxb_load_xml fxb_fn = do
  (fx_id, fx_v, fx_sz, dat) <- Vst.fx_load_CcnK_FBCh fxb_fn
  Control.Monad.when
    (Vst.word32_to_str fx_id /= "Obxd")
    (error "obxd_load_fxb: fx-id?")
  Control.Monad.when
    (fx_v /= 100)
    (print ("obxd_load_fxb: fx-version?", fx_v))
  let xml_n = Vst.pack_word32 (reverse (take 4 (drop 4 dat)))
  return (fx_sz, map Byte.word8_to_char (Data.List.genericTake xml_n (drop 8 dat)))

-- * Xml / Io

-- | Scan Fxp or Fxb file for Obxd Xml data, ie. ignore Fxb or Fxp container.
obxd_load_xml_lax :: FilePath -> IO ByteString.ByteString
obxd_load_xml_lax fn = do
  b <- ByteString.readFile fn
  let str_pack :: String -> ByteString.ByteString
      str_pack = ByteString.pack . map (fromIntegral . fromEnum)
      is_ascii_print :: Data.Word.Word8 -> Bool
      is_ascii_print c = c >= 32 && c <= 126
      (_, r) = ByteString.breakSubstring (str_pack "<Datsounds") b
  return (ByteString.takeWhile is_ascii_print r)

-- * Xml / Parse

-- | (Program-Name,Parameter-Data)
type Obxd_Program = (String, [Double])

-- | Parse attributes from program element (for fxb) or Datsounds element (for fxp).
obxd_attr_parse :: Xml.Element -> Obxd_Program
obxd_attr_parse e =
  ( Xml.x_get_attr "programName" e
  , map (\x -> read (Xml.x_get_attr (show x) e)) [0 :: Int .. 70]
  )

-- | Parse Fxp Xml data.
obd_fxp_xml_parse :: Xml.Lexer.XmlSource x => x -> Obxd_Program
obd_fxp_xml_parse = obxd_attr_parse . Xml.xml_parse_err

-- | 'obd_fxp_xml_parse' of 'obxd_load_xml_lax'
obxd_fxp_load_lax :: FilePath -> IO Obxd_Program
obxd_fxp_load_lax = fmap obd_fxp_xml_parse . obxd_load_xml_lax

-- | Get list of program from programs element of Datsounds element.
obxd_fxb_xml_programs :: Xml.Element -> [Xml.Element]
obxd_fxb_xml_programs = Xml.elChildren . Xml.x_get_elem "programs"

-- | Parse Fxb Xml data, run 'obxd_attr_parse'.
obd_fxb_xml_parse :: Xml.Lexer.XmlSource x => x -> [Obxd_Program]
obd_fxb_xml_parse = map obxd_attr_parse . obxd_fxb_xml_programs . Xml.xml_parse_err

-- | 'obd_xml_parse' of 'obxd_load_xml_lax'
obxd_fxb_load_lax :: FilePath -> IO [Obxd_Program]
obxd_fxb_load_lax = fmap obd_fxb_xml_parse . obxd_load_xml_lax

-- * Csv

-- | Encode 'Obxd_Program' as Csv entry, /k/ is the precision to print to.
obxd_program_to_csv :: Int -> Obxd_Program -> String
obxd_program_to_csv k (nm, dat) =
  if ',' `elem` nm
    then error "obxd_program_to_csv: name comma?"
    else Data.List.intercalate "," (nm : map (Sc3.real_pp k) dat)

-- | 'writeFile' of 'obxd_program_to_csv'.
obxd_write_csv :: Int -> FilePath -> [Obxd_Program] -> IO ()
obxd_write_csv k fn = writeFile fn . unlines . map (obxd_program_to_csv k)

-- | Parse Csv entry to 'Obxd_Program'.
obxd_parse_csv :: String -> Obxd_Program
obxd_parse_csv s =
  case Split.splitOn "," s of
    nm : dat ->
      let n = length dat
      in if n < 71 || n > 80
          then error ("obxd_parse_csv: n-param = " ++ show n)
          else (nm, map read dat)
    _ -> error "obxd_parse_csv?"

-- | 'obxd_parse_csv' of 'readFile'.
obxd_load_csv :: FilePath -> IO [Obxd_Program]
obxd_load_csv fn = do
  s <- readFile fn
  return (map obxd_parse_csv (lines s))

-- * Util

-- | 'writeFile' of 'obxd_load_fxb'
obxd_fxb_to_xml :: FilePath -> FilePath -> IO ()
obxd_fxb_to_xml fxb_fn xml_fn = do
  (_, xml_str) <- obxd_fxb_load_xml fxb_fn
  writeFile xml_fn xml_str

-- | 'obd_xml_parse' of 'obxd_load_fxb'
obxd_load_programs :: FilePath -> IO [Obxd_Program]
obxd_load_programs fn = do
  (n, x) <- obxd_fxb_load_xml fn
  let p = obd_fxb_xml_parse x
  Control.Monad.when
    (n /= Data.List.genericLength p)
    (print ("obxd_load_programs?", n, length p))
  return p

{- | Obxd default parameters

>>> length obxd_param_def
80
-}
obxd_param_def :: [Double]
obxd_param_def = [0, 0, 0.5, 1, 0.5, 0.5, 0, 0, 0, 0.6, 0, 0, 0, 0, 0, 0.2, 0.4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0.3, 0.3, 0.3, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0, 1, 0, 0, 0, 0, 0, 0, 0.3, 0]

{- | Obxd parameter names

>>> length obxd_param_nm
80
-}
obxd_param_nm :: [String]
obxd_param_nm =
  [ "UNDEFINED" -- 0
  , "MIDILEARN"
  , "VOLUME"
  , "VOICE_COUNT"
  , "TUNE"
  , "OCTAVE"
  , "BENDRANGE"
  , "BENDOSC2"
  , "LEGATOMODE"
  , "BENDLFORATE"
  , "VFLTENV" -- 10
  , "VAMPENV"
  , "ASPLAYEDALLOCATION"
  , "PORTAMENTO"
  , "UNISON"
  , "UDET"
  , "OSC2_DET"
  , "LFOFREQ"
  , "LFOSINWAVE"
  , "LFOSQUAREWAVE"
  , "LFOSHWAVE" -- 18-20
  , "LFO1AMT"
  , "LFO2AMT"
  , "LFOOSC1"
  , "LFOOSC2"
  , "LFOFILTER"
  , "LFOPW1"
  , "LFOPW2"
  , "OSC2HS"
  , "XMOD"
  , "OSC1P" -- 30
  , "OSC2P"
  , "OSCQuantize"
  , "OSC1Saw"
  , "OSC1Pul"
  , "OSC2Saw"
  , "OSC2Pul"
  , "PW"
  , "BRIGHTNESS"
  , "ENVPITCH"
  , "OSC1MIX" -- 40
  , "OSC2MIX"
  , "NOISEMIX"
  , "FLT_KF"
  , "CUTOFF"
  , "RESONANCE"
  , "MULTIMODE"
  , "FILTER_WARM"
  , "BANDPASS"
  , "FOURPOLE"
  , "ENVELOPE_AMT" -- 50
  , "LATK"
  , "LDEC"
  , "LSUS"
  , "LREL"
  , "FATK"
  , "FDEC"
  , "FSUS"
  , "FREL"
  , "ENVDER"
  , "FILTERDER"
  , "PORTADER" -- 59-61
  , "PAN1"
  , "PAN2"
  , "PAN3"
  , "PAN4"
  , "PAN5"
  , "PAN6"
  , "PAN7"
  , "PAN8"
  , "UNLEARN" -- 70
  , "ECONOMY_MODE" -- 2014-07-06
  , "LFO_SYNC" -- 2014-07-18
  , "PW_ENV" -- 2014-07-21
  , "PW_ENV_BOTH"
  , "ENV_PITCH_BOTH"
  , "FENV_INVERT"
  , "PW_OSC2_OFS"
  , "LEVEL_DIF"
  , "SELF_OSC_PUSH"
  ]
