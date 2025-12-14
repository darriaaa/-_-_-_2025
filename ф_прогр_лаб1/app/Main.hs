{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}



module Main (main) where

import           Control.Monad          (forM_, forever, unless, void)
import           Data.Char              (isSpace)
import           Data.Int               (Int64)
import qualified Data.ByteString.Char8  as BS
import qualified Data.Text              as T
import           Data.Text              (Text)
import           Data.Proxy             (Proxy (..))
import           Data.String            (fromString)
import           System.Environment     (lookupEnv)
import           System.IO              (hFlush, stdout)
import           Database.MySQL.Simple
    ( Connection
    , ConnectInfo (..)
    , close
    , connect
    , defaultConnectInfo
    , execute
    , execute_
    , query
    , query_
    )
import           Database.MySQL.Simple.Types (Only(..))

data Room = Room
  { roomId    :: Int
  , roomName  :: Text
  , roomBuilding :: Text
  , roomFloor :: Int
  , roomCapacity :: Int
  } deriving (Show)

data Workstation = Workstation
  { wsId     :: Int
  , wsRoomId :: Int
  , wsLabel  :: Text
  , wsStatus :: Text
  } deriving (Show)

data AppUser = AppUser
  { userId   :: Int
  , userName :: Text
  , userRole :: Text
  } deriving (Show)


data Teacher = Teacher
  { teacherId   :: Int
  , teacherName :: Text
  , teacherDept :: Text
  } deriving (Show)

lastInsertId :: Connection -> IO Int64
lastInsertId conn = do
  [Only i] <- query_ conn "SELECT LAST_INSERT_ID()"
  pure i


data Course = Course
  { courseId   :: Int
  , courseName :: Text
  } deriving (Show)


data Lesson = Lesson
  { lessonId       :: Int
  , lessonCourseId :: Int
  , lessonTeacherId :: Int
  , lessonRoomId   :: Int
  , lessonStart    :: Text -- ^ start time as ISO datetime string
  , lessonEnd      :: Text -- ^ end time as ISO datetime string
  } deriving (Show)


data FreeAccess = FreeAccess
  { accessId    :: Int
  , accessRoomId :: Int
  , accessStart :: Text
  , accessEnd   :: Text
  } deriving (Show)


data Booking = Booking
  { bookingId    :: Int
  , bookingWsId  :: Int
  , bookingUserId :: Int
  , bookingStart :: Text
  , bookingEnd   :: Text
  } deriving (Show)


class Crud a where
  tableName  :: Proxy a -> String
  listAll    :: Connection -> IO [a]
  insertOne  :: Connection -> a -> IO Int
  updateOne  :: Connection -> a -> IO ()
  deleteById :: Proxy a -> Connection -> Int -> IO ()



instance Crud Room where
  tableName _ = "rooms"
  listAll conn = do
    rows <- query_ conn "SELECT room_id, name, building, floor_no, capacity FROM rooms ORDER BY room_id" :: IO [(Int, Text, Text, Int, Int)]
    pure [ Room i n b f c | (i,n,b,f,c) <- rows ]
  insertOne conn (Room _ n b f c) = do
    void $ execute conn
      "INSERT INTO rooms (name, building, floor_no, capacity) VALUES (?,?,?,?)"
      (n, b, f, c)
    fromIntegral <$> lastInsertId conn
  updateOne conn (Room i n b f c) = do
    void $ execute conn
      "UPDATE rooms SET name=?, building=?, floor_no=?, capacity=? WHERE room_id=?"
      (n, b, f, c, i)
  deleteById _ conn ident = do
    void $ execute conn "DELETE FROM rooms WHERE room_id=?" (Only ident)

instance Crud Workstation where
  tableName _ = "workstations"
  listAll conn = do
    rows <- query_ conn "SELECT ws_id, room_id, label, status FROM workstations ORDER BY ws_id" :: IO [(Int, Int, Text, Text)]
    pure [ Workstation i r l s | (i,r,l,s) <- rows ]
  insertOne conn (Workstation _ rid lbl st) = do
    void $ execute conn
      "INSERT INTO workstations (room_id, label, status) VALUES (?,?,?)"
      (rid, lbl, st)
    fromIntegral <$> lastInsertId conn
  updateOne conn (Workstation i rid lbl st) = do
    void $ execute conn
      "UPDATE workstations SET room_id=?, label=?, status=? WHERE ws_id=?"
      (rid, lbl, st, i)
  deleteById _ conn ident = do
    void $ execute conn "DELETE FROM workstations WHERE ws_id=?" (Only ident)

instance Crud AppUser where
  tableName _ = "users"
  listAll conn = do
    rows <- query_ conn "SELECT user_id, full_name, role FROM users ORDER BY user_id" :: IO [(Int, Text, Text)]
    pure [ AppUser i n r | (i,n,r) <- rows ]
  insertOne conn (AppUser _ n r) = do
    void $ execute conn
      "INSERT INTO users (full_name, role) VALUES (?,?)"
      (n, r)
    fromIntegral <$> lastInsertId conn
  updateOne conn (AppUser i n r) = do
    void $ execute conn
      "UPDATE users SET full_name=?, role=? WHERE user_id=?"
      (n, r, i)
  deleteById _ conn ident = do
    void $ execute conn "DELETE FROM users WHERE user_id=?" (Only ident)

instance Crud Teacher where
  tableName _ = "teachers"
  listAll conn = do
    rows <- query_ conn "SELECT teacher_id, full_name, department FROM teachers ORDER BY teacher_id" :: IO [(Int, Text, Text)]
    pure [ Teacher i n d | (i,n,d) <- rows ]
  insertOne conn (Teacher _ n d) = do
    void $ execute conn
      "INSERT INTO teachers (full_name, department) VALUES (?,?)"
      (n, d)
    fromIntegral <$> lastInsertId conn
  updateOne conn (Teacher i n d) = do
    void $ execute conn
      "UPDATE teachers SET full_name=?, department=? WHERE teacher_id=?"
      (n, d, i)
  deleteById _ conn ident = do
    void $ execute conn "DELETE FROM teachers WHERE teacher_id=?" (Only ident)

instance Crud Course where
  tableName _ = "courses"
  listAll conn = do
    rows <- query_ conn "SELECT course_id, name FROM courses ORDER BY course_id" :: IO [(Int, Text)]
    pure [ Course i n | (i,n) <- rows ]
  insertOne conn (Course _ n) = do
    void $ execute conn "INSERT INTO courses (name) VALUES (?)" (Only n)
    fromIntegral <$> lastInsertId conn
  updateOne conn (Course i n) = do
    void $ execute conn "UPDATE courses SET name=? WHERE course_id=?" (n, i)
  deleteById _ conn ident = do
    void $ execute conn "DELETE FROM courses WHERE course_id=?" (Only ident)

instance Crud Lesson where
  tableName _ = "lessons"
  listAll conn = do
    rows <- query_ conn "SELECT lesson_id, course_id, teacher_id, room_id, start_time, end_time FROM lessons ORDER BY lesson_id" :: IO [(Int, Int, Int, Int, Text, Text)]
    pure [ Lesson i c t r s e | (i,c,t,r,s,e) <- rows ]
  insertOne conn (Lesson _ c t r s e) = do
    void $ execute conn
      "INSERT INTO lessons (course_id, teacher_id, room_id, start_time, end_time) VALUES (?,?,?,?,?)"
      (c, t, r, s, e)
    fromIntegral <$> lastInsertId conn
  updateOne conn (Lesson i c t r s e) = do
    void $ execute conn
      "UPDATE lessons SET course_id=?, teacher_id=?, room_id=?, start_time=?, end_time=? WHERE lesson_id=?"
      (c, t, r, s, e, i)
  deleteById _ conn ident = do
    void $ execute conn "DELETE FROM lessons WHERE lesson_id=?" (Only ident)

instance Crud FreeAccess where
  tableName _ = "free_access"
  listAll conn = do
    rows <- query_ conn "SELECT access_id, room_id, start_time, end_time FROM free_access ORDER BY access_id" :: IO [(Int, Int, Text, Text)]
    pure [ FreeAccess i r s e | (i,r,s,e) <- rows ]
  insertOne conn (FreeAccess _ r s e) = do
    void $ execute conn
      "INSERT INTO free_access (room_id, start_time, end_time) VALUES (?,?,?)"
      (r, s, e)
    fromIntegral <$> lastInsertId conn
  updateOne conn (FreeAccess i r s e) = do
    void $ execute conn
      "UPDATE free_access SET room_id=?, start_time=?, end_time=? WHERE access_id=?"
      (r, s, e, i)
  deleteById _ conn ident = do
    void $ execute conn "DELETE FROM free_access WHERE access_id=?" (Only ident)

instance Crud Booking where
  tableName _ = "bookings"
  listAll conn = do
    rows <- query_ conn "SELECT booking_id, ws_id, user_id, start_time, end_time FROM bookings ORDER BY booking_id" :: IO [(Int, Int, Int, Text, Text)]
    pure [ Booking i w u s e | (i,w,u,s,e) <- rows ]
  insertOne conn (Booking _ w u s e) = do
    void $ execute conn
      "INSERT INTO bookings (ws_id, user_id, start_time, end_time) VALUES (?,?,?,?)"
      (w, u, s, e)
    fromIntegral <$> lastInsertId conn
  updateOne conn (Booking i w u s e) = do
    void $ execute conn
      "UPDATE bookings SET ws_id=?, user_id=?, start_time=?, end_time=? WHERE booking_id=?"
      (w, u, s, e, i)
  deleteById _ conn ident = do
    void $ execute conn "DELETE FROM bookings WHERE booking_id=?" (Only ident)


prompt :: String -> IO String
prompt label = do
  putStr (label ++ ": ")
  hFlush stdout
  getLine


promptInt :: String -> IO Int
promptInt label = do
  s <- prompt label
  case reads s of
    [(i, "")] -> return i
    _          -> putStrLn "Invalid number, please try again." >> promptInt label

promptText :: String -> IO Text
promptText label = do
  s <- prompt label
  return (T.pack (trim s))


trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace


initSchema :: Connection -> IO ()
initSchema conn = do
  contents <- readFile "schema.sql"
  let statements = filter (not . null) . map trim $ splitSemicolons contents
  forM_ statements $ \stmt -> do
    execute_ conn (fromString stmt)
  putStrLn "Schema initialisation complete."


splitSemicolons :: String -> [String]
splitSemicolons str = go str []
  where
    go [] acc = [reverse acc]
    go (c:cs) acc
      | c == ';'  = reverse acc : go cs []
      | otherwise = go cs (c:acc)


connectMySQL :: IO Connection
connectMySQL = do
  host <- getEnvOr "DB_HOST" "127.0.0.1"
  portStr <- getEnvOr "DB_PORT" "3306"
  user <- getEnvOr "DB_USER" "root"
  pass <- getEnvOr "DB_PASS" ""
  db   <- getEnvOr "DB_NAME" "display_schedule"
  let portNum = read portStr :: Int
  let info = defaultConnectInfo
        { connectHost     = host
        , connectPort     = fromIntegral portNum
        , connectUser     = user
        , connectPassword = pass
        , connectDatabase = db
        }
  putStrLn $ "Connecting to MySQL at " ++ host ++ ":" ++ show portNum ++ " ..."
  connect info


getEnvOr :: String -> String -> IO String
getEnvOr key def = do
  mval <- lookupEnv key
  return (maybe def id mval)


main :: IO ()
main = do
  conn <- connectMySQL
  putStrLn "Welcome to the Display Schedule system."
  mainLoop conn
  close conn


mainLoop :: Connection -> IO ()
mainLoop conn = loop
  where
    loop = do
      putStrLn ""
      putStrLn "===== Main Menu ====="
      putStrLn "0) Initialise schema"
      putStrLn "1) Manage rooms"
      putStrLn "2) Manage workstations"
      putStrLn "3) Manage users"
      putStrLn "4) Manage teachers"
      putStrLn "5) Manage courses"
      putStrLn "6) Manage lessons"
      putStrLn "7) Manage free access windows"
      putStrLn "8) Manage bookings"
      putStrLn "9) Quit"
      choice <- prompt "Choose an option"
      case choice of
        "0" -> initSchema conn >> loop
        "1" -> crudMenu (Proxy :: Proxy Room) conn >> loop
        "2" -> crudMenu (Proxy :: Proxy Workstation) conn >> loop
        "3" -> crudMenu (Proxy :: Proxy AppUser) conn >> loop
        "4" -> crudMenu (Proxy :: Proxy Teacher) conn >> loop
        "5" -> crudMenu (Proxy :: Proxy Course) conn >> loop
        "6" -> crudMenu (Proxy :: Proxy Lesson) conn >> loop
        "7" -> crudMenu (Proxy :: Proxy FreeAccess) conn >> loop
        "8" -> crudMenu (Proxy :: Proxy Booking) conn >> loop
        "9" -> putStrLn "Goodbye!" >> return ()
        _   -> putStrLn "Invalid option, please try again." >> loop


crudMenu :: forall a. (Crud a, Show a) => Proxy a -> Connection -> IO ()
crudMenu proxy conn = loop
  where
    name = tableName proxy
    loop = do
      putStrLn ""
      putStrLn $ "--- Manage " ++ name ++ " ---"
      putStrLn "1) List all"
      putStrLn "2) Add new"
      putStrLn "3) Update existing"
      putStrLn "4) Delete"
      putStrLn "5) Back to main menu"
      c <- prompt "Choose an option"
      case c of
        "1" -> do
          xs <- listAll conn :: IO [a]
          mapM_ print xs
          loop
        "2" -> do
          insertEntity proxy conn
          loop
        "3" -> do
          updateEntity proxy conn
          loop
        "4" -> do
          ident <- promptInt "Enter identifier to delete"
          deleteById proxy conn ident
          putStrLn "Deleted (if it existed)."
          loop
        "5" -> return ()
        _   -> do
          putStrLn "Invalid option."
          loop


insertEntity :: forall a. Crud a => Proxy a -> Connection -> IO ()
insertEntity proxy conn = case proxy of
  _ | tableName proxy == "rooms" -> do
        n  <- promptText "Name"
        b  <- promptText "Building"
        f  <- promptInt "Floor"
        c  <- promptInt "Capacity"
        let r = Room 0 n b f c
        i <- insertOne conn r
        putStrLn $ "Inserted room with id " ++ show i
    | tableName proxy == "workstations" -> do
        rid <- promptInt "Room ID"
        lbl <- promptText "Label"
        st  <- promptText "Status (available/reserved/broken)"
        let w = Workstation 0 rid lbl st
        i <- insertOne conn w
        putStrLn $ "Inserted workstation with id " ++ show i
    | tableName proxy == "users" -> do
        n  <- promptText "Full name"
        r  <- promptText "Role (student/admin)"
        let u = AppUser 0 n r
        i <- insertOne conn u
        putStrLn $ "Inserted user with id " ++ show i
    | tableName proxy == "teachers" -> do
        n  <- promptText "Full name"
        d  <- promptText "Department"
        let t = Teacher 0 n d
        i <- insertOne conn t
        putStrLn $ "Inserted teacher with id " ++ show i
    | tableName proxy == "courses" -> do
        n  <- promptText "Course name"
        let c' = Course 0 n
        i <- insertOne conn c'
        putStrLn $ "Inserted course with id " ++ show i
    | tableName proxy == "lessons" -> do
        cid <- promptInt "Course ID"
        tid <- promptInt "Teacher ID"
        rid <- promptInt "Room ID"
        st  <- promptText "Start time (YYYY-MM-DD HH:MM:SS)"
        en  <- promptText "End time (YYYY-MM-DD HH:MM:SS)"
        let l = Lesson 0 cid tid rid st en
        i <- insertOne conn l
        putStrLn $ "Inserted lesson with id " ++ show i
    | tableName proxy == "free_access" -> do
        rid <- promptInt "Room ID"
        st  <- promptText "Start time (YYYY-MM-DD HH:MM:SS)"
        en  <- promptText "End time (YYYY-MM-DD HH:MM:SS)"
        let fa = FreeAccess 0 rid st en
        i <- insertOne conn fa
        putStrLn $ "Inserted free access entry with id " ++ show i
    | tableName proxy == "bookings" -> do
        wid <- promptInt "Workstation ID"
        uid <- promptInt "User ID"
        st  <- promptText "Start time (YYYY-MM-DD HH:MM:SS)"
        en  <- promptText "End time (YYYY-MM-DD HH:MM:SS)"
        let bkg = Booking 0 wid uid st en
        i <- insertOne conn bkg
        putStrLn $ "Inserted booking with id " ++ show i
    | otherwise -> putStrLn "Unknown entity."


updateEntity :: forall a. Crud a => Proxy a -> Connection -> IO ()
updateEntity proxy conn = case proxy of
  _ | tableName proxy == "rooms" -> do
        ident <- promptInt "Room ID to update"
        n  <- promptText "Name"
        b  <- promptText "Building"
        f  <- promptInt "Floor"
        c  <- promptInt "Capacity"
        let r = Room ident n b f c
        updateOne conn r
        putStrLn "Room updated."
    | tableName proxy == "workstations" -> do
        ident <- promptInt "Workstation ID to update"
        rid <- promptInt "Room ID"
        lbl <- promptText "Label"
        st  <- promptText "Status (available/reserved/broken)"
        let w = Workstation ident rid lbl st
        updateOne conn w
        putStrLn "Workstation updated."
    | tableName proxy == "users" -> do
        ident <- promptInt "User ID to update"
        n  <- promptText "Full name"
        r  <- promptText "Role"
        let u = AppUser ident n r
        updateOne conn u
        putStrLn "User updated."
    | tableName proxy == "teachers" -> do
        ident <- promptInt "Teacher ID to update"
        n  <- promptText "Full name"
        d  <- promptText "Department"
        let t = Teacher ident n d
        updateOne conn t
        putStrLn "Teacher updated."
    | tableName proxy == "courses" -> do
        ident <- promptInt "Course ID to update"
        n  <- promptText "Course name"
        let c' = Course ident n
        updateOne conn c'
        putStrLn "Course updated."
    | tableName proxy == "lessons" -> do
        ident <- promptInt "Lesson ID to update"
        cid <- promptInt "Course ID"
        tid <- promptInt "Teacher ID"
        rid <- promptInt "Room ID"
        st  <- promptText "Start time (YYYY-MM-DD HH:MM:SS)"
        en  <- promptText "End time (YYYY-MM-DD HH:MM:SS)"
        let l = Lesson ident cid tid rid st en
        updateOne conn l
        putStrLn "Lesson updated."
    | tableName proxy == "free_access" -> do
        ident <- promptInt "Free access ID to update"
        rid <- promptInt "Room ID"
        st  <- promptText "Start time (YYYY-MM-DD HH:MM:SS)"
        en  <- promptText "End time (YYYY-MM-DD HH:MM:SS)"
        let fa = FreeAccess ident rid st en
        updateOne conn fa
        putStrLn "Free access entry updated."
    | tableName proxy == "bookings" -> do
        ident <- promptInt "Booking ID to update"
        wid <- promptInt "Workstation ID"
        uid <- promptInt "User ID"
        st  <- promptText "Start time (YYYY-MM-DD HH:MM:SS)"
        en  <- promptText "End time (YYYY-MM-DD HH:MM:SS)"
        let bkg = Booking ident wid uid st en
        updateOne conn bkg
        putStrLn "Booking updated."
    | otherwise -> putStrLn "Unknown entity."


onlyDummy :: Only Int
onlyDummy = Only 0
