-- | Printers and parsers for timestamps.
module Sound.Sc3.Data.Timestamp where

import qualified Data.Time as T {- time -}

-- * GEN

-- | Generate an ISO8601 (basic or extended) format timestamp for the current local time.
--
-- > mapM gen_iso8601_time_stamp [True,False]
gen_iso8601_time_stamp :: Bool -> IO String
gen_iso8601_time_stamp ext =
  fmap
  (T.formatTime T.defaultTimeLocale (if ext then  "%Y-%m-%dT%H:%M:%S" else "%Y%m%dT%H%M%S"))
  T.getZonedTime

-- | Variant useful for forming portable file-names, with @:@ re-written as @-@.
gen_iso8601_time_stamp_fn :: IO String
gen_iso8601_time_stamp_fn =
  let rw = map (\c -> if c == ':' then '-' else c)
  in fmap rw (gen_iso8601_time_stamp True)

-- * PARSE

-- | 'T.parseTimeOrError' of 'T.defaultTimeLocale'.
parse_time_fmt :: String -> String -> T.LocalTime
parse_time_fmt = T.parseTimeOrError True T.defaultTimeLocale

-- | Strict RFC822 format.
--
-- > parse_time_fmt rfc822_fmt "Sat, 26 Mar 2016 19:12:51 +1100"
rfc822_fmt :: String
rfc822_fmt = "%a, %d %b %Y %H:%M:%S %Z"

-- | Allow single-digit day and hour.
--
-- > parse_time_fmt rfc822_fmt_lenient "Mon, 4 Sep 2017 9:07:09 +1000"
rfc822_fmt_lenient :: String
rfc822_fmt_lenient = "%a, %_d %b %Y %_H:%M:%S %Z"

-- | Timezone name as prefix to offset.
--
-- > parse_time_fmt rfc822_fmt_infix_named_tz "Sat, 12 Dec 09 16:05:55 GMT-0700"
rfc822_fmt_infix_named_tz :: String
rfc822_fmt_infix_named_tz = "%a, %_d %b %Y %_H:%M:%S %Z%z"

-- | Timezone name as parenthesised postfix of offset.
--
-- > parse_time_fmt rfc822_fmt_postfix_named_tz "Fri, 11 Nov 2016 12:39:09 +0100 (CET)"
rfc822_fmt_postfix_named_tz :: String
rfc822_fmt_postfix_named_tz = "%a, %_d %b %Y %_H:%M:%S %z (%Z)"

-- | Without day of week.
--
-- > parse_time_fmt rfc822_fmt_no_day_of_week "11 Aug 2008 21:29:30 -0700"
rfc822_fmt_no_day_of_week :: String
rfc822_fmt_no_day_of_week = "%_d %b %Y %_H:%M:%S %Z"

-- | Without timezone.
--
-- > parse_time_fmt rfc822_fmt_no_tz "Tue, 16 Oct 2007 10:21:54"
rfc822_fmt_no_tz :: String
rfc822_fmt_no_tz = "%a, %_d %b %Y %_H:%M:%S"

-- | ISO8601 basic.
--
-- > parse_time_fmt iso8601_fmt_basic "20141118T194544"
iso8601_fmt_basic :: String
iso8601_fmt_basic = "%Y%m%dT%H%M%S"

-- | ISO8601 extended.
--
-- > parse_time_fmt iso8601_fmt_ext "2014-11-18T19:45:44"
iso8601_fmt_ext :: String
iso8601_fmt_ext = "%FT%H:%M:%S"

-- | ISO8601 extended with time-zone.
--
-- > parse_time_fmt iso8601_fmt_ext_tz "2014-11-18T19:45:44+11:00"
iso8601_fmt_ext_tz :: String
iso8601_fmt_ext_tz = "%FT%H:%M:%S%z"

-- | Apply /f/ at /l/ until if provides a value, or 'Nothing' if it never does.
locate :: (t -> Maybe u) -> [t] -> Maybe u
locate f l =
    case l of
      [] -> Nothing
      e:l' -> case f e of
                Just r -> Just r
                Nothing -> locate f l'

-- | Attempt to parse time-stamp using a sequence of format strings.
parse_timestamp_fmt_seq :: T.ParseTime t => [String] -> String -> Maybe t
parse_timestamp_fmt_seq fmt s =
    let ptm = T.parseTimeM True T.defaultTimeLocale
        try x = ptm x s
    in locate try fmt

{- | Somewhat flexible time-stamp parser.

> let ts = ["Sat, 26 Mar 2016 19:12:51 +1100"
>          ,"Sat, 20 Sep 2008 9:18:14 +1000"
>          ,"Sat, 12 Dec 09 16:05:55 GMT-0700"
>          ,"Fri, 11 Nov 2016 12:39:09 +0100 (CET)"
>          ,"11 Aug 2008 21:29:30 -0700"
>          ,"Tue, 16 Oct 2007 10:21:54"
>          ,"20141118T194544"
>          ,"2014-11-18T19:45:44"
>          ,"2014-11-18T19:45:44+11:00"
>          ,"Wed, 22 Feb 2006 09:39:30 +1100 (AUS Eastern Standard Time)"]
> in map parse_timestamp ts

-}
parse_timestamp :: String -> Maybe T.LocalTime
parse_timestamp =
    parse_timestamp_fmt_seq
    [rfc822_fmt_lenient
    ,rfc822_fmt_infix_named_tz
    ,rfc822_fmt_postfix_named_tz
    ,rfc822_fmt_no_day_of_week
    ,rfc822_fmt_no_tz
    ,iso8601_fmt_basic
    ,iso8601_fmt_ext
    ,iso8601_fmt_ext_tz]
