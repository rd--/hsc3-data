-- <https://github.com/2DaT/Obxd>
module Sound.SC3.Data.VST.Obxd where

import Control.Monad {- base -}
import Data.List {- base -}
import Data.Word {- base -}

import qualified Text.XML.Light as X {- xml -}

import qualified Sound.SC3.Common.Math as SC3 {- hsc3 -}

import qualified Sound.SC3.Data.VST as VST {- hsc3-data -}
import qualified Sound.SC3.Data.XML as XML {- hsc3-data -}

-- | Load Obxd FXB file, returns the number of progams and the XML data.
obxd_load_fxb :: FilePath -> IO (Word32,String)
obxd_load_fxb fxb_fn = do
  (fx_id,fx_v,fx_sz,dat) <- VST.fx_load_CcnK_FBCh fxb_fn
  when (VST.word32_to_str fx_id /= "Obxd") (error "obxd_load_fxb: fx-id?")
  when (fx_v /= 100) (error "obxd_load_fxb: fx-version?")
  let xml_n = VST.pack_word32 (reverse (take 4 (drop 4 dat)))
  return (fx_sz,map VST.word8_to_char (genericTake xml_n (drop 8 dat)))

-- | 'writeFile' of 'obxd_load_fxb'
obxd_fxb_to_xml :: FilePath -> FilePath -> IO ()
obxd_fxb_to_xml fxb_fn xml_fn = do
  (_,xml_str) <- obxd_load_fxb fxb_fn
  writeFile xml_fn xml_str

-- | Get program elements from root element.
obxd_get_programs :: X.Element -> [X.Element]
obxd_get_programs = X.elChildren . XML.x_get_elem "programs"

-- | (PROGRAM-NAME,PARAMETER-DATA)
type OBXD_Program = (String,[Double])

-- | Parse program element.
obxd_program_parse :: X.Element -> OBXD_Program
obxd_program_parse e =
  (XML.x_get_attr "programName" e
  ,map (\x -> read (XML.x_get_attr (show x) e)) [0::Int .. 70])

-- | Load Obxd FXB file, parse XML data, run 'obxd_program_parse'.
obxd_load_programs :: FilePath -> IO [OBXD_Program]
obxd_load_programs fn = do
  (n,x) <- obxd_load_fxb fn
  let p = obxd_get_programs (XML.xml_parse_err x)
  when (n /= genericLength p) (error "obxd_load_programs?")
  return (map obxd_program_parse p)

obxd_program_to_csv :: Int -> OBXD_Program -> String
obxd_program_to_csv k (nm,dat) = intercalate "," (nm : map (SC3.real_pp k) dat)

obxd_write_csv :: Int -> FilePath -> [OBXD_Program] -> IO ()
obxd_write_csv k fn = writeFile fn . unlines . map (obxd_program_to_csv k)
