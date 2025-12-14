-- MySQL schema for the display schedule application.
--
-- This script creates eight tables used by the Haskell
-- application.  To initialise your database, run this file
-- against an empty schema (e.g. `mysql -u root -p < schema.sql`).

CREATE TABLE IF NOT EXISTS rooms (
  room_id   INT AUTO_INCREMENT PRIMARY KEY,
  name      VARCHAR(100) NOT NULL,
  building  VARCHAR(100) NOT NULL,
  floor_no  INT NOT NULL,
  capacity  INT NOT NULL
);

CREATE TABLE IF NOT EXISTS workstations (
  ws_id     INT AUTO_INCREMENT PRIMARY KEY,
  room_id   INT NOT NULL,
  label     VARCHAR(100) NOT NULL,
  status    VARCHAR(20) NOT NULL DEFAULT 'available',
  FOREIGN KEY (room_id) REFERENCES rooms(room_id)
    ON DELETE CASCADE ON UPDATE CASCADE
);

CREATE TABLE IF NOT EXISTS users (
  user_id   INT AUTO_INCREMENT PRIMARY KEY,
  full_name VARCHAR(200) NOT NULL,
  role      VARCHAR(50) NOT NULL
);

CREATE TABLE IF NOT EXISTS teachers (
  teacher_id INT AUTO_INCREMENT PRIMARY KEY,
  full_name  VARCHAR(200) NOT NULL,
  department VARCHAR(200) NOT NULL
);

CREATE TABLE IF NOT EXISTS courses (
  course_id INT AUTO_INCREMENT PRIMARY KEY,
  name      VARCHAR(200) NOT NULL
);

CREATE TABLE IF NOT EXISTS lessons (
  lesson_id  INT AUTO_INCREMENT PRIMARY KEY,
  course_id  INT NOT NULL,
  teacher_id INT NOT NULL,
  room_id    INT NOT NULL,
  start_time DATETIME NOT NULL,
  end_time   DATETIME NOT NULL,
  FOREIGN KEY (course_id)  REFERENCES courses(course_id)
    ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (teacher_id) REFERENCES teachers(teacher_id)
    ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (room_id)    REFERENCES rooms(room_id)
    ON DELETE CASCADE ON UPDATE CASCADE
);

CREATE TABLE IF NOT EXISTS free_access (
  access_id  INT AUTO_INCREMENT PRIMARY KEY,
  room_id    INT NOT NULL,
  start_time DATETIME NOT NULL,
  end_time   DATETIME NOT NULL,
  FOREIGN KEY (room_id) REFERENCES rooms(room_id)
    ON DELETE CASCADE ON UPDATE CASCADE
);

CREATE TABLE IF NOT EXISTS bookings (
  booking_id INT AUTO_INCREMENT PRIMARY KEY,
  ws_id      INT NOT NULL,
  user_id    INT NOT NULL,
  start_time DATETIME NOT NULL,
  end_time   DATETIME NOT NULL,
  FOREIGN KEY (ws_id)   REFERENCES workstations(ws_id)
    ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (user_id) REFERENCES users(user_id)
    ON DELETE CASCADE ON UPDATE CASCADE
);