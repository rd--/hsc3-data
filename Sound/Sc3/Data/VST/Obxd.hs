-- | <https://github.com/2DaT/Obxd>
module Sound.Sc3.Data.VST.Obxd where

import Control.Monad {- base -}
import Data.List {- base -}
import Data.Word {- base -}

import qualified Data.ByteString as B {- bytestring -}
import qualified Data.List.Split as Split {- split -}
import qualified Text.XML.Light as X {- xml -}
import qualified Text.XML.Light.Lexer as X {- xml -}

import qualified Sound.Sc3.Common.Math as Sc3 {- hsc3 -}

import qualified Sound.Sc3.Data.VST as VST {- hsc3-data -}
import qualified Sound.Sc3.Data.XML as XML {- hsc3-data -}

-- * FXB / IO

-- | Load Obxd FXB file, returns the number of progams and the XML data.
obxd_fxb_load_xml :: FilePath -> IO (Word32,String)
obxd_fxb_load_xml fxb_fn = do
  (fx_id,fx_v,fx_sz,dat) <- VST.fx_load_CcnK_FBCh fxb_fn
  when (VST.word32_to_str fx_id /= "Obxd") (error "obxd_load_fxb: fx-id?")
  when (fx_v /= 100) (print ("obxd_load_fxb: fx-version?",fx_v))
  let xml_n = VST.pack_word32 (reverse (take 4 (drop 4 dat)))
  return (fx_sz,map VST.word8_to_char (genericTake xml_n (drop 8 dat)))

-- * XML / IO

-- | Scan FXP or FXB file for Obxd XML data, ie. ignore FXB or FXP container.
obxd_load_xml_lax :: FilePath -> IO B.ByteString
obxd_load_xml_lax fn = do
  b <- B.readFile fn
  let str_pack :: String -> B.ByteString
      str_pack = B.pack . map (fromIntegral . fromEnum)
      is_ascii_print :: Word8 -> Bool
      is_ascii_print c = c >= 32 && c <= 126
      (_,r) = B.breakSubstring (str_pack "<Datsounds") b
  return (B.takeWhile is_ascii_print r)

-- * XML / PARSE

-- | (PROGRAM-NAME,PARAMETER-DATA)
type OBXD_Program = (String,[Double])

-- | Parse attributes from program element (for fxb) or Datsounds element (for fxp).
obxd_attr_parse :: X.Element -> OBXD_Program
obxd_attr_parse e =
  (XML.x_get_attr "programName" e
  ,map (\x -> read (XML.x_get_attr (show x) e)) [0::Int .. 70])

-- | Parse FXP XML data.
obd_fxp_xml_parse :: X.XmlSource x => x -> OBXD_Program
obd_fxp_xml_parse = obxd_attr_parse . XML.xml_parse_err

-- | 'obd_fxp_xml_parse' of 'obxd_load_xml_lax'
obxd_fxp_load_lax :: FilePath -> IO OBXD_Program
obxd_fxp_load_lax = fmap obd_fxp_xml_parse . obxd_load_xml_lax

-- | Get list of program from programs element of Datsounds element.
obxd_fxb_xml_programs :: X.Element -> [X.Element]
obxd_fxb_xml_programs = X.elChildren . XML.x_get_elem "programs"

-- | Parse FXB XML data, run 'obxd_attr_parse'.
obd_fxb_xml_parse :: X.XmlSource x => x -> [OBXD_Program]
obd_fxb_xml_parse = map obxd_attr_parse . obxd_fxb_xml_programs . XML.xml_parse_err

-- | 'obd_xml_parse' of 'obxd_load_xml_lax'
obxd_fxb_load_lax :: FilePath -> IO [OBXD_Program]
obxd_fxb_load_lax = fmap obd_fxb_xml_parse . obxd_load_xml_lax

-- * CSV

-- | Encode 'OBXD_Program' as CSV entry, /k/ is the precision to print to.
obxd_program_to_csv :: Int -> OBXD_Program -> String
obxd_program_to_csv k (nm,dat) =
  if ',' `elem` nm
  then error "obxd_program_to_csv: name comma?"
  else intercalate "," (nm : map (Sc3.real_pp k) dat)

-- | 'writeFile' of 'obxd_program_to_csv'.
obxd_write_csv :: Int -> FilePath -> [OBXD_Program] -> IO ()
obxd_write_csv k fn = writeFile fn . unlines . map (obxd_program_to_csv k)

-- | Parse CSV entry to 'OBXD_Program'.
obxd_parse_csv :: String -> OBXD_Program
obxd_parse_csv s =
  case Split.splitOn "," s of
    nm:dat -> let n = length dat
              in if n < 71 || n > 80
                 then error ("obxd_parse_csv: n-param = " ++ show n)
                 else (nm,map read dat)
    _ -> error "obxd_parse_csv?"

-- | 'obxd_parse_csv' of 'readFile'.
obxd_load_csv :: FilePath -> IO [OBXD_Program]
obxd_load_csv fn = do
  s <- readFile fn
  return (map obxd_parse_csv (lines s))

-- * Util

-- | 'writeFile' of 'obxd_load_fxb'
obxd_fxb_to_xml :: FilePath -> FilePath -> IO ()
obxd_fxb_to_xml fxb_fn xml_fn = do
  (_,xml_str) <- obxd_fxb_load_xml fxb_fn
  writeFile xml_fn xml_str

-- | 'obd_xml_parse' of 'obxd_load_fxb'
obxd_load_programs :: FilePath -> IO [OBXD_Program]
obxd_load_programs fn = do
  (n,x) <- obxd_fxb_load_xml fn
  let p = obd_fxb_xml_parse x
  when (n /= genericLength p) (print ("obxd_load_programs?",n,length p))
  return p

-- > length obxd_def == 80
obxd_param_def :: [Double]
obxd_param_def = [0,0,0.5,1,0.5,0.5,0,0,0,0.6,0,0,0,0,0,0.2,0.4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,1,0,1,1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0.3,0.3,0.3,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0,1,0,0,0,0,0,0,0.3,0]

-- > length obxd_param_nm == 80
obxd_param_nm :: [String]
obxd_param_nm =
  ["UNDEFINED" -- 0
  ,"MIDILEARN"
  ,"VOLUME"
  ,"VOICE_COUNT"
  ,"TUNE"
  ,"OCTAVE"
  ,"BENDRANGE"
  ,"BENDOSC2"
  ,"LEGATOMODE"
  ,"BENDLFORATE"
  ,"VFLTENV" -- 10
  ,"VAMPENV"
  ,"ASPLAYEDALLOCATION"
  ,"PORTAMENTO"
  ,"UNISON"
  ,"UDET"
  ,"OSC2_DET"
  ,"LFOFREQ"
  ,"LFOSINWAVE","LFOSQUAREWAVE","LFOSHWAVE" -- 18-20
  ,"LFO1AMT","LFO2AMT"
  ,"LFOOSC1","LFOOSC2","LFOFILTER","LFOPW1","LFOPW2"
  ,"OSC2HS"
  ,"XMOD"
  ,"OSC1P" -- 30
  ,"OSC2P"
  ,"OSCQuantize"
  ,"OSC1Saw"
  ,"OSC1Pul"
  ,"OSC2Saw"
  ,"OSC2Pul"
  ,"PW"
  ,"BRIGHTNESS"
  ,"ENVPITCH"
  ,"OSC1MIX" -- 40
  ,"OSC2MIX"
  ,"NOISEMIX"
  ,"FLT_KF"
  ,"CUTOFF"
  ,"RESONANCE"
  ,"MULTIMODE"
  ,"FILTER_WARM"
  ,"BANDPASS"
  ,"FOURPOLE"
  ,"ENVELOPE_AMT" -- 50
  ,"LATK"
  ,"LDEC"
  ,"LSUS"
  ,"LREL"
  ,"FATK"
  ,"FDEC"
  ,"FSUS"
  ,"FREL"
  ,"ENVDER","FILTERDER","PORTADER" -- 59-61
  ,"PAN1","PAN2","PAN3","PAN4","PAN5","PAN6","PAN7","PAN8"
  ,"UNLEARN" -- 70
  ,"ECONOMY_MODE" -- 2014-07-06
  ,"LFO_SYNC" -- 2014-07-18
  ,"PW_ENV" -- 2014-07-21
  ,"PW_ENV_BOTH"
  ,"ENV_PITCH_BOTH"
  ,"FENV_INVERT"
  ,"PW_OSC2_OFS"
  ,"LEVEL_DIF"
  ,"SELF_OSC_PUSH"]
