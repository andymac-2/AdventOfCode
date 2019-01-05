import Data.List (sort, maximumBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing, compare)

import Data.Foldable (foldl')

import qualified Data.Map.Strict as M

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Text.Megaparsec
import Text.Megaparsec.Char

main :: IO ()
main = do
    T.interact (\input -> let
        eventsList =  sort . parseEvents $ input
        initState =  (beginEvent, -1, newGuardSchedules)
        (_, _, totalSchedule) = foldl' (flip addEvent) initState eventsList
        (guardid, guard) = mostAsleepGuard totalSchedule
        minute = mostAsleepMinute guard
        in T.pack $ show guardid ++ show minute)
    putStrLn ""

data EventType = Asleep | Awake | BeginShift Int deriving (Eq, Ord, Show)
-- Year Month Day Hour Minute
data TimeStamp = TimeStamp Int Int Int Int Int deriving (Eq, Ord, Show)
getMinute :: TimeStamp -> Int
getMinute (TimeStamp _ _ _ _ m) = m

data Event = Event TimeStamp EventType deriving (Eq, Ord, Show)
beginEvent :: Event
beginEvent = (Event (TimeStamp 0 0 0 0 0) Awake)

-- parse input
type Parser = Parsec () T.Text

parseEvents :: T.Text -> [Event]
parseEvents str = case parse eventListP "" str of
    Left _ -> error "Invalid Parse"
    Right events -> events

eventListP :: Parser [Event]
eventListP = sepEndBy eventP newline

eventP :: Parser Event
eventP = Event <$> timestampP <* space <*> eventTypeP

timestampP :: Parser TimeStamp
timestampP = TimeStamp
    <$ char '['
    <*> integerP
    <* char '-'
    <*> integerP
    <* char '-'
    <*> integerP
    <* space
    <*> integerP
    <* char ':'
    <*> integerP
    <* char ']'

eventTypeP :: Parser EventType
eventTypeP = choice
    [ Awake <$ string (T.pack "wakes up")
    , Asleep <$ string (T.pack "falls asleep")
    , BeginShift 
        <$ string (T.pack "Guard #") 
        <*> integerP 
        <* string (T.pack " begins shift")
    ]

integerP :: Parser Int
integerP = read <$> some digitChar

-- Schedule creation
addEvent :: Event -> (Event, Int, GuardSchedules) 
    -> (Event, Int, GuardSchedules)
addEvent e@(Event _ Asleep) (_, guardid, sched) 
    = (e, guardid, sched)
addEvent e@(Event _ (BeginShift guardid)) (Event _ _, _, sched)
    = (e, guardid, sched)
addEvent
    e@(Event end Awake) (Event start Asleep, guardid, sched)
    = let
        sm = getMinute start
        em = getMinute end
    in (e, guardid, guardFallAsleep guardid sm em sched)
addEvent e s = error $ "Inconsistent guard patterns:\n" ++ show e ++ "\n" ++ show s

-- combined Schedules
newtype GuardSchedules = GuardSchedules (M.Map Int GuardSchedule) deriving (Show)

newGuardSchedules :: GuardSchedules
newGuardSchedules = GuardSchedules (M.empty)

guardFallAsleep :: Int -> Int -> Int -> GuardSchedules -> GuardSchedules
guardFallAsleep guardId start end (GuardSchedules s) = let
    go sched = Just . fallAsleep start end $ fromMaybe newGuardSchedule sched
    in GuardSchedules . M.alter go guardId $ s

mostAsleepGuard :: GuardSchedules -> (Int, GuardSchedule)
mostAsleepGuard (GuardSchedules s) = maximumBy (comparing snd) . M.toList $ s

-- individual Guard schedules
newtype GuardSchedule = GuardSchedule (M.Map Int Int) deriving (Show)
instance Eq GuardSchedule where
    a == b = asleepTime a == asleepTime b
instance Ord GuardSchedule where
    compare = comparing asleepTime

newGuardSchedule :: GuardSchedule
newGuardSchedule = GuardSchedule M.empty

fallAsleep :: Int -> Int -> GuardSchedule -> GuardSchedule
fallAsleep start end (GuardSchedule schedule) = let
    asleepMinute sched time = M.insertWith (+) time 1 sched
    in GuardSchedule $ foldl' asleepMinute schedule [start .. end - 1]

asleepTime :: GuardSchedule -> Int
-- part 1
-- asleepTime (GuardSchedule x) = sum x
-- part 2
asleepTime (GuardSchedule s) = maximum s

mostAsleepMinute :: GuardSchedule -> (Int, Int)
mostAsleepMinute (GuardSchedule s) = maximumBy (comparing snd) . M.toList $ s
