module Stations where

import Text.Printf

 
--  http://www5.trafikanten.no/txml/?type=1&stopname=

line_5_vestli = Line { lineID = 5, direction = 1}
     
data Line = Line { lineID::Integer, direction::Integer }     

instance Show Line where
  show a = "Line " ++ show (lineID a) ++ " retning " ++ show (direction a) --printf "%-15s" (destination a)
 
instance Eq Line where
   a==b  = (lineID a == lineID b) && (direction a == direction b)
     
     
stations::Line -> [Station]
stations line | line == line_5_vestli = [STORO, 
                                         NYDALEN,    
                                         ULLEVAALSTADION, 
                                         FORSKNINGSPARKEN,
                                         BLINDERN,       
                                         MAJORSTUEN,      
                                         NATIONALTHEATRET,
                                         STORTINGET,      
                                         JERNBANETORGET,  
                                         GROENLAND,       
                                         TOEYEN,          
                                         CARLBERNERSPLASS,
                                         HASLE,           
                                         OEKERN,          
                                         RISLOEKKA,       
                                         VOLLEBEKK,       
                                         LINDERUD,        
                                         VEITVET,         
                                         ROEDTVET,        
                                         KALBAKKEN,       
                                         AMMERUD,         
                                         GRORUD,          
                                         ROMSAAS,         
                                         ROMMEN,          
                                         STOVNER,         
                                         VESTLI]


data Station =   STORO
               | NYDALEN
               | ULLEVAALSTADION
               | FORSKNINGSPARKEN
               | BLINDERN         
               | MAJORSTUEN       
               | NATIONALTHEATRET 
               | STORTINGET       
               | JERNBANETORGET   
               | GROENLAND        
               | TOEYEN           
               | CARLBERNERSPLASS 
               | HASLE            
               | OEKERN           
               | RISLOEKKA        
               | VOLLEBEKK        
               | LINDERUD         
               | VEITVET          
               | ROEDTVET         
               | KALBAKKEN        
               | AMMERUD          
               | GRORUD           
               | ROMSAAS          
               | ROMMEN           
               | STOVNER          
               | VESTLI     
 
instance Show Station where
   --show a = printf "%s" (stationName a)
   show a = show (stationName a)
  
  
stationId::Station -> String  
stationId STORO            = "03012120"
stationId NYDALEN          = "03012130"
stationId ULLEVAALSTADION  = "03012210"
stationId FORSKNINGSPARKEN = "03010370"
stationId BLINDERN         = "03010360"
stationId MAJORSTUEN       = "03010200"
stationId NATIONALTHEATRET = "03010031"
stationId STORTINGET       = "03010020"
stationId JERNBANETORGET   = "03010011"
stationId GROENLAND        = "03010610"
stationId TOEYEN           = "03010600"
stationId CARLBERNERSPLASS = "03011400"
stationId HASLE            = "03011410"
stationId OEKERN           = "03012000"
stationId RISLOEKKA        = "03012010"
stationId VOLLEBEKK        = "03012020"
stationId LINDERUD         = "03012030"
stationId VEITVET          = "03012040"
stationId ROEDTVET         = "03011910"
stationId KALBAKKEN        = "03011920"
stationId AMMERUD          = "03011930"
stationId GRORUD           = "03011940"
stationId ROMSAAS          = "03011810"
stationId ROMMEN           = "03011710"
stationId STOVNER          = "03011720"
stationId VESTLI           = "03011730"

stationName::Station -> String  
stationName STORO            = "STORO"
stationName NYDALEN          = "NYDALEN"
stationName ULLEVAALSTADION  = "ULLEVAALSTADION"
stationName FORSKNINGSPARKEN = "FORSKNINGSPARKEN"
stationName BLINDERN         = "BLINDERN"
stationName MAJORSTUEN       = "MAJORSTUEN"
stationName NATIONALTHEATRET = "NATIONALTHEATRET"
stationName STORTINGET       = "STORTINGET"
stationName JERNBANETORGET   = "JERNBANETORGET"
stationName GROENLAND        = "GROENLAND"
stationName TOEYEN           = "TOEYEN"
stationName CARLBERNERSPLASS = "CARLBERNERSPLASS"
stationName HASLE            = "HASLE"
stationName OEKERN           = "OEKERN"
stationName RISLOEKKA        = "RISLOEKKA"
stationName VOLLEBEKK        = "VOLLEBEKK"
stationName LINDERUD         = "LINDERUD "
stationName VEITVET          = "VEITVET"
stationName ROEDTVET         = "ROEDTVET"
stationName KALBAKKEN        = "KALBAKKEN"
stationName AMMERUD          = "AMMERUD"
stationName GRORUD           = "GRORUD"
stationName ROMSAAS          = "ROMSAAS"
stationName ROMMEN           = "ROMMEN"
stationName STOVNER          = "STOVNER"
stationName VESTLI           = "VESTLI"
