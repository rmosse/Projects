module Getdate

	where

import System.IO
import System.Time
import System.Locale
import System.Environment
 
main s = do
    time <- getClockTime >>= toCalendarTime
	a = $ formatCalendarTime defaultTimeLocale s time


