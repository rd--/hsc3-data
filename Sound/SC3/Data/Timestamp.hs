module Sound.SC3.Data.Timestamp where

import qualified Data.Time as T {- time -}

parse_time_fmt :: String -> String -> T.LocalTime
parse_time_fmt fmt = T.parseTimeOrError True T.defaultTimeLocale fmt

-- | Allow single-digit hour.
--
-- > parse_time_fmt rfc822_fmt "Sat, 26 Mar 2016 19:12:51 +1100"
-- > parse_time_fmt rfc822_fmt "Sat, 20 Sep 2008 9:18:14 +1000"
rfc822_fmt :: String
rfc822_fmt = "%a, %_d %b %Y %_H:%M:%S %Z"

-- > parse_time_fmt rfc822_fmt_infix_named_tz "Sat, 12 Dec 09 16:05:55 GMT-0700"
rfc822_fmt_infix_named_tz :: String
rfc822_fmt_infix_named_tz = "%a, %_d %b %Y %_H:%M:%S %Z%z"

-- > parse_time_fmt rfc822_fmt_postfix_named_tz "Fri, 11 Nov 2016 12:39:09 +0100 (CET)"
rfc822_fmt_postfix_named_tz :: String
rfc822_fmt_postfix_named_tz = "%a, %_d %b %Y %_H:%M:%S %z (%Z)"

-- > parse_time_fmt rfc822_fmt_no_day "11 Aug 2008 21:29:30 -0700"
rfc822_fmt_no_day :: String
rfc822_fmt_no_day = "%_d %b %Y %_H:%M:%S %Z"

-- > parse_time_fmt rfc822_fmt_no_tz "Tue, 16 Oct 2007 10:21:54"
rfc822_fmt_no_tz :: String
rfc822_fmt_no_tz = "%a, %_d %b %Y %_H:%M:%S"

-- > parse_time_fmt iso8601_tz "2014-11-18T19:45:44+11:00"
iso8601_tz :: String
iso8601_tz = "%FT%H:%M:%S%z"

locate :: (t -> Maybe u) -> [t] -> Maybe u
locate f l =
    case l of
      [] -> Nothing
      e:l' -> case f e of
                Just r -> Just r
                Nothing -> locate f l'

{- | Somewhat flexible time-stamp parser.

> let ts = ["Sat, 26 Mar 2016 19:12:51 +1100"
>          ,"Fri, 11 Nov 2016 12:39:09 +0100 (CET)"
>          ,"11 Aug 2008 21:29:30 -0700"
>          ,"Sat, 12 Dec 09 16:05:55 GMT-0700"
>          ,"2014-11-18T19:45:44+11:00"
>          ,"Sat, 20 Sep 2008 9:18:14 +1000"
>          ,"Tue, 16 Oct 2007 10:21:54"
>          ,"Wed, 22 Feb 2006 09:39:30 +1100 (AUS Eastern Standard Time)"]
> in map parse_time ts

-}
parse_timestamp :: String -> T.LocalTime
parse_timestamp s =
    let fmt = [rfc822_fmt
              ,rfc822_fmt_infix_named_tz
              ,rfc822_fmt_postfix_named_tz
              ,rfc822_fmt_no_day
              ,rfc822_fmt_no_tz
              ,iso8601_tz]
        ptm = T.parseTimeM True T.defaultTimeLocale
        try x = ptm x s
    in case locate try fmt of
         Just t -> t
         Nothing ->
             case ptm rfc822_fmt (take 31 s) of
               Just t -> t
               _ -> error (show ("parse_time",s))
