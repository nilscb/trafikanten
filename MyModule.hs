module MyModule where

import Network.HTTP
import System.IO
import Data.Maybe
import Data.DateTime
import Data.Time
import Data.List
import Data.Maybe
import Network.URI
import Text.XML.HaXml
import Text.XML.HaXml.Parse
import Text.Printf
import Control.Monad 
import Stations 
 
 
 
trafikantenUrl = "http://www.sis.trafikanten.no:8088/xmlrtpi/dis/request?DISID=SN$"
timeparsestring = "%Y-%m-%dT%H:%M:%S%Q+02:00"

--type = (Station, Arrival)
type TripId = String

data Arrival = Arrival { tripID::TripId,  arrivesSeconds::Integer  }             
data StationArrival = StationArrival { line::Line, arrival::Arrival }  
  
instance Show Arrival where
   show a = (printf "%02d:%02d:%02d" hours minutes sec) ++ " tripid: " ++
            show (tripID a) ++ "\n" 
            where  totseconds = arrivesSeconds a
                   hours   = totseconds `div` 3600
                   minutes = totseconds `div` 60
                   sec     = totseconds `mod` 60    
                                    
                                                     
instance Show StationArrival where
   show a = show (line a) ++ "  " ++ show (arrival a) 



tripsOnLine::Line -> IO [(Station, Arrival)]
tripsOnLine line = do 
                     trips <- getLineTrips line
                     x <- sequence $ (map (findTripId line) trips)
                     return (map fromJust (filter isJust x))
 

-- which station is tripid about to enter
findTripId::Line -> TripId -> IO (Maybe (Station, Arrival))
findTripId line tripid = myfunc (stations line)
                         where myfunc (stat:stats) = do 
                                 x <- isTripPresent line tripid stat
                                 case x of 
                                   Just arr -> return (Just (stat,arr))
                                   Nothing  -> myfunc stats
                               myfunc [] = return Nothing


-- returns an unique list of the tripid's on the given line
getLineTrips::Line -> IO [TripId] 
getLineTrips line = let a = liftM concat (sequence $ map (getArrivals line) (stations line))  -- a::IO [Arrival]
                    in  a >>= return . (map tripID)  >>= return . nub



--checks if a given tripID is present in a list of arrivals and returns 
-- the corresponing Arrival
isTripPresent:: Line -> TripId -> Station -> IO (Maybe Arrival)
isTripPresent line tripid station 
    = do x <- (getArrivals line station)
         y <- return (filter (\yy -> tripID yy == tripid) x)
         case y of
           [] -> return Nothing
           _  -> return (Just (head y))
       
-------------------------------------------------

getArrivals:: Line -> Station -> IO [Arrival]
getArrivals line station 
   = getAllArrivals station >>= (\x -> return (filter (mycompare line) x)) -- IO [StationArrival] here
     >>= (\x -> return (map arrival x)) 

mycompare::Line -> StationArrival -> Bool
mycompare aline statarr | line statarr == aline  = True
                        | otherwise              = False


getAllArrivals::Station -> IO [StationArrival]
getAllArrivals station = stationXmlString station >>= parse 
                         >>= (\x -> return (sortBy compareStationArrivals x))


compareStationArrivals::StationArrival -> StationArrival -> Ordering
compareStationArrivals a b | arrivesSeconds (arrival a) < arrivesSeconds (arrival b) = LT
                           | arrivesSeconds (arrival a) > arrivesSeconds (arrival b) = GT
                           | otherwise                                               = EQ


stationXmlString::Station -> IO String
stationXmlString station = let xmlstring = trafikantenUrl ++ stationId station 
                           in  simpleHTTP (getRequest xmlstring) >>= getResponseBody

 
parse::String -> IO [StationArrival]
parse xmlText = let (Document _ _ root _) = xmlParse "dummyfilename" xmlText
                    topContent = CElem root
                    contentList = multi (keep /> tag "DISDeviation") $ topContent  -- multi / deep
                in  return (map makeStationArrival contentList)

makeStationArrival::Content -> StationArrival
makeStationArrival c = 
       let lineIdString = contentToString $ (keep /> tag "LineID" /> txt) $ c
           lineId       = read lineIdString::Integer
           dirString    = contentToString $ (keep /> tag "DirectionID" /> txt) $ c
           dir          = read dirString::Integer
           dest         = contentToString $ (keep /> tag "DestinationStop" /> txt) $ c
           tripid       = contentToString $ (keep /> tag "TripID" /> txt) $ c
           nowtime      = contentToString $ showattr "TimeStamp" c
           arrtime      = contentToString $ (keep /> tag "ExpectedDISArrivalTime" /> txt) $ c
           Just arr1    = parseDateTime timeparsestring arrtime
           Just arr2    = parseDateTime timeparsestring nowtime
           difftime     = diffSeconds  arr1 arr2
        in  StationArrival { line=Line {lineID = lineId, direction = dir},
                             arrival=Arrival{tripID = tripid, arrivesSeconds = difftime} 
                           }


-- tatt fra real world haskell bok
contentToString :: [Content] -> String
contentToString =
    concatMap procContent
    where procContent x =
              verbatim $ keep /> txt $ CElem (unesc (fakeElem x))

          fakeElem :: Content -> Element
          fakeElem x = Elem "fake" [] [x]

          unesc :: Element -> Element
          unesc = xmlUnEscape stdXmlEscaper

--------------------------------------------------------------------------------

-- haxml doc:  http://www.cs.york.ac.uk/fp/HaXml/icfp99.html#examplescript
-- http://timjstewart.blogspot.com/2009/03/exploring-xml-using-haskell.html

--url for station search api
--station_url = "http://www5.trafikanten.no/txml/?type=1&stopname=Storo [T-bane]"

-- # url for non-subway realtime data
--    "http://www.sis.trafikanten.no/xmlrtpi/dis/request?DISID=SN$"

-- # url for subway realtime data
--    "http://www.sis.trafikanten.no:8088/xmlrtpi/dis/request?DISID=SN$"


--timestring      = "2009-04-16T13:01:03.000+02:00"
--timeparsestring = "%Y-%m-%dT%H:%M:%S%Q%z"
--parseDateTime parsestring timestring
--parseDateTime :: String -> String -> Maybe DateTime
--diffSeconds :: DateTime -> DateTime -> Integer



--xmlText::String
--xmlText ="<order><part>Hammer</part></order>"
--xmlText =  "<xml><DISDeviation><LineID>5</LineID><ArrivalTime>hei</ArrivalTime></DISDeviation><DISDeviation><LineID>5</LineID><ArrivalTime>hei2</ArrivalTime></DISDeviation></xml>"

-- xmlText = "<DataSupplyAnswer><Acknowledge TimeStamp=\"2009-04-13T22:10:41.905+02:00\" Result=\"ok\" ErrorNumber=\"0\"/><DISMessage SubscriptionID=\"1\"><DISDeviation TimeStamp=\"2009-04-13T22:10:41.905+02:00\"><TripID>502:8H3S04:8H3S04:38</TripID><DISID>SN$3011930</DISID><StopSeqCount>0</StopSeqCount><LineID>5</LineID><DirectionID>2</DirectionID><LineText>5</LineText><DirectionText>2</DirectionText><DestinationStop>Storo</DestinationStop><TripStatus>Real</TripStatus><ScheduledDISArrivalTime>2009-04-13T22:55:20.000+02:00</ScheduledDISArrivalTime><ExpectedDISArrivalTime>2009-04-13T22:55:20.000+02:00</ExpectedDISArrivalTime><ScheduledDISDepartureTime>2009-04-13T22:55:20.000+02:00</ScheduledDISDepartureTime><ExpectedDISDepartureTime>2009-04-13T22:55:20.000+02:00</ExpectedDISDepartureTime><StopPosition>2</StopPosition><TripInfo><BlockNumber>173</BlockNumber></TripInfo></DISDeviation></DISMessage></DataSupplyAnswer>"

-- <DataSupplyAnswer>
--    <Acknowledge TimeStamp="2009-04-28T21:52:30.194+02:00" Result="ok" ErrorNumber="0"/>
--    <DISMessage SubscriptionID="1">
--        <DISDeviation TimeStamp="2009-04-28T21:52:30.179+02:00">
--        <TripID>502:8H3H04:8H3H04:57</TripID>
--        <DISID>SN$03012130</DISID>
--        <StopSeqCount>0</StopSeqCount>
--        <LineID>5</LineID>
--        <DirectionID>1</DirectionID>
--        <LineText>5</LineText>
--        <DirectionText>1</DirectionText>
--        <DestinationStop>Ryen</DestinationStop>
--        <TripStatus>Real</TripStatus>
--        <ScheduledDISArrivalTime>2009-04-28T21:53:48.000+02:00</ScheduledDISArrivalTime>
--        <ExpectedDISArrivalTime>2009-04-28T21:53:48.000+02:00</ExpectedDISArrivalTime>
--        <ScheduledDISDepartureTime>2009-04-28T21:53:48.000+02:00</ScheduledDISDepartureTime>
--        <ExpectedDISDepartureTime>2009-04-28T21:53:48.000+02:00</ExpectedDISDepartureTime>
--        <StopPosition>1</StopPosition>
--        <TripInfo>
--        <BlockNumber>46</BlockNumber>
--        </TripInfo>
--        </DISDeviation>
--    </DISMessage>
-- </DataSupplyAnswer>




