-- entertainment.sql




DROP TABLE IF EXISTS survey;
DROP TABLE IF EXISTS movies;

DROP TABLE IF EXISTS certificate;
DROP TABLE IF EXISTS genre;

-- TABLE GENRE

CREATE TABLE genre 
(
  genreid varchar(20) NOT NULL,
  genretype varchar(200) NOT NULL,
  PRIMARY KEY (genreid)
);

LOAD DATA INFILE 'F:/data/entertainment/genre.csv' 
INTO TABLE genre
FIELDS TERMINATED BY ',' 
ENCLOSED BY '"'
LINES TERMINATED BY '\r\n'
(genreid, genretype)
;


-- TABLE CERTIFICATE

CREATE TABLE certificate 
(
  certificateid varchar(20) NOT NULL,
  certificatedesc varchar(200) NOT NULL,
  PRIMARY KEY (certificateid)
);

LOAD DATA INFILE 'F:/data/entertainment/certificate.csv' 
INTO TABLE certificate
FIELDS TERMINATED BY ',' 
ENCLOSED BY '"'
LINES TERMINATED BY '\r\n'
(certificateid, certificatedesc)
;


-- TABLE MOVIES

CREATE TABLE movies 
(
  movieid varchar(200) NOT NULL,
  moviename varchar(100) NOT NULL,
  genre varchar(20) NOT NULL,
  yearreleased int NOT NULL,
  leadcast varchar(300) NOT NULL,
  dirrectedby varchar(300) NOT NULL,
  countryreleased varchar(100) NOT NULL,
  language varchar(50) NOT NULL,
  watchtimemin int NOT NULL,
  certificate varchar(10) NOT NULL,
  awards varchar(50) NULL,
   info varchar(1000) NOT NULL,
   PRIMARY KEY (movieid),
   CONSTRAINT FK_genre FOREIGN KEY (genre) 
    REFERENCES genre(genreid)
    ON DELETE CASCADE ON UPDATE CASCADE,
   CONSTRAINT FK_certificate FOREIGN KEY (certificate) 
    REFERENCES certificate(certificateid)
    ON DELETE CASCADE ON UPDATE CASCADE
    
);

LOAD DATA INFILE 'F:/data/entertainment/movies.csv' 
INTO TABLE movies
FIELDS TERMINATED BY ',' 
ENCLOSED BY '"'
LINES TERMINATED BY '\r\n'
(movieid, moviename, genre, yearreleased, leadcast, dirrectedby, countryreleased, language, watchtimemin, certificate, awards, info)
;


-- TABLE SURVEY

CREATE TABLE survey 
(
  respondentid int NOT NULL,
  movieid varchar(100) NOT NULL,
  rating int NULL,
  responsedt varchar(50) NOT NULL,
  PRIMARY KEY(respondentid, movieid),
  CONSTRAINT FK_movieid FOREIGN KEY (movieid) 
    REFERENCES movies(movieid)
    ON DELETE CASCADE ON UPDATE CASCADE
);

LOAD DATA INFILE 'F:/data/entertainment/survey.csv' 
INTO TABLE survey
FIELDS TERMINATED BY ',' 
ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(respondentid, movieid, @rating, responsedt)
SET
rating = nullif(@rating,-1)
;
UPDATE survey SET responsedt=STR_TO_DATE(responsedt, "%m-%d-%Y");



SELECT * FROM survey;
SELECT COUNT(*) FROM survey;
SELECT * FROM movies;
SELECT COUNT(*) FROM movies;
SELECT * FROM certificate;
SELECT COUNT(*) FROM certificate;
SELECT * FROM genre;
SELECT COUNT(*) FROM genre;

