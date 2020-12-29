-- Databricks notebook source
-- MAGIC %md
-- MAGIC # SQL CAPSTONE PROJECT
-- MAGIC Lobbyists4America is a company that seeks to provide insights to their customers (who aim to affect legislation within the US).  They want you to analyze the 2008-2017 congressional tweets in order to understand key topics, members, and relationships within Congress.  These insights will help them focus and strengthen their lobbying efforts.

-- COMMAND ----------

-- MAGIC %md
-- MAGIC The database is created for tweet and user datasets.

-- COMMAND ----------

CREATE DATABASE IF NOT EXISTS Twitter

-- COMMAND ----------

-- MAGIC %md
-- MAGIC Two datasets are retrieved from DBFS FileStore.

-- COMMAND ----------

SET spark.sql.shuffle.partitions=50

-- COMMAND ----------

USE Twitter;

CREATE TABLE IF NOT EXISTS tweet
USING org.apache.spark.sql.json
OPTIONS (
  path 
  "dbfs:/FileStore/tables/tweets.json"
);

CREATE TABLE IF NOT EXISTS user
USING org.apache.spark.sql.json
OPTIONS (
  path "dbfs:/FileStore/tables/users.json"
);

CACHE TABLE tweet;
CACHE TABLE user

-- COMMAND ----------

DESCRIBE tweet

-- COMMAND ----------

DESCRIBE user

-- COMMAND ----------

SELECT name
  , SUM(climate_mentioning)    AS climate_mentioning
FROM  
  (SELECT name
    , CASE
        WHEN text LIKE '%climate change%'
        OR text LIKE '%global warming%'
        OR text LIKE '%changing climate%'
        OR text LIKE '%climate crisis%'
        THEN 1
        ELSE 0
      END                        AS climate_mentioning
  FROM
    (SELECT user.id              AS id
      , tweet.user_id
      , user.created_at
      , name
      , user.screen_name         AS screen_name
      , LOWER(text)              AS text
      , retweeted
      , in_reply_to_screen_name
      , in_reply_to_status_id
      , in_reply_to_user_id
    FROM user INNER JOIN tweet
     ON user.screen_name = tweet.screen_name))
GROUP BY name
ORDER BY climate_mentioning DESC

-- COMMAND ----------

-- MAGIC %md
-- MAGIC Since the climate change holds significant role on the political platform, the data showing certain politicians, choose not to tweet about this are skeptical. By scrutinizing the record of politicians that data does not have their tweet information, the problem is revealed. Elijah E Cummings who is in this record [had already tweeted about this topics](https://twitter.com/repcummings/status/1123998380303298561?lang=en) before passing away. There are some restrictions existing in both datasets so that both may not answer all questions.

-- COMMAND ----------

SELECT name
  , SUM(climate_mentioning)    AS total_climate_mentioning
FROM  
  (SELECT name
    , CASE
        WHEN text LIKE '%climate change%'
        OR text LIKE '%global warming%'
        OR text LIKE '%changing climate%'
        OR text LIKE '%climate crisis%'
        THEN 1
        ELSE 0
      END                        AS climate_mentioning
  FROM
    (SELECT user.id              AS id
      , tweet.user_id
      , user.created_at
      , name
      , user.screen_name         AS screen_name
      , LOWER(text)              AS text
      , retweeted
      , in_reply_to_screen_name
      , in_reply_to_status_id
      , in_reply_to_user_id
    FROM user INNER JOIN tweet
     ON user.screen_name = tweet.screen_name))
GROUP BY name
HAVING SUM(climate_mentioning) = 0

-- COMMAND ----------

SELECT COUNT(replied)            AS Responses
FROM
  (SELECT name
    , CASE
        WHEN in_reply_to_screen_name IS NULL
        THEN 0
        ELSE 1
      END                        AS replied          
    FROM
    (SELECT user.id              AS id
      , tweet.user_id
      , user.created_at
      , name
      , user.screen_name         AS screen_name
      , LOWER(text)              AS text
      , retweeted
      , in_reply_to_screen_name
      , in_reply_to_status_id
      , in_reply_to_user_id
      FROM user INNER JOIN tweet
       ON user.screen_name = tweet.screen_name
     WHERE text LIKE '%climate change%'
            OR text LIKE '%global warming%'
            OR text LIKE '%changing climate%'
            OR text LIKE '%climate crisis%'))
GROUP BY replied

-- COMMAND ----------

SELECT Responses
  , name
FROM  
  (SELECT SUM(replied)             AS Responses
    , name
  FROM
    (SELECT name
      , CASE
          WHEN in_reply_to_screen_name IS NULL
          THEN 0
          ELSE 1
        END                        AS replied          
      FROM
      (SELECT user.id              AS id
        , tweet.user_id
        , user.created_at
        , name
        , user.screen_name         AS screen_name
        , LOWER(text)              AS text
        , retweeted
        , in_reply_to_screen_name
        , in_reply_to_status_id
        , in_reply_to_user_id
        FROM user INNER JOIN tweet
         ON user.screen_name = tweet.screen_name
       WHERE text LIKE '%climate change%'
              OR text LIKE '%global warming%'
              OR text LIKE '%changing climate%'
              OR text LIKE '%climate crisis%'))
  GROUP BY name
  HAVING SUM(replied) > 0)
  ORDER BY Responses DESC

-- COMMAND ----------

SELECT id -- Dataset that selects primary key and text columns exports to R Studio.
  , text
FROM tweet
LIMIT 1000000

-- COMMAND ----------

-- MAGIC %md
-- MAGIC This new dataset produced by R Studio is imported to Spark.

-- COMMAND ----------

USE Twitter;

CREATE TABLE IF NOT EXISTS sentiment
USING CSV
OPTIONS(
path
"dbfs:/FileStore/tables/tweetsentiment.csv",
HEADER = TRUE,
INFERSCHEMA = TRUE);

CACHE TABLE sentiment

-- COMMAND ----------

SELECT AVG(CASE
    WHEN scoring > 0
    THEN 1
    ELSE -1
    END)                    AS Average
  , STD(CASE
    WHEN scoring > 0
    THEN 1
    ELSE -1
    END)                    AS Standard_Deviation
  , SUM(CASE
    WHEN scoring IS NULL
    THEN 0
    ELSE 1
    END)                    AS Sample_Size
  FROM tweet INNER JOIN user
   ON tweet.screen_name = user.screen_name
   LEFT JOIN sentiment
   ON tweet.id = sentiment.id
  WHERE text LIKE '%climate change%'
    OR text LIKE '%global warming%'
    OR text LIKE '%changing climate%'
UNION
SELECT AVG(CASE
    WHEN scoring > 0
    THEN 1
    ELSE -1
    END)                    AS Average
  , STD(CASE
    WHEN scoring > 0
    THEN 1
    ELSE -1
    END)                    AS Standard_Deviation
  , SUM(CASE
    WHEN scoring IS NULL
    THEN 0
    ELSE 1
    END)                    AS Sample_Size
  FROM tweet INNER JOIN user
   ON tweet.screen_name = user.screen_name
   LEFT JOIN sentiment
   ON tweet.id = sentiment.id
  WHERE text LIKE '%combat climate change%'
    OR text LIKE '%climate action%'
    OR text LIKE '%climate crisis%'

-- COMMAND ----------

SELECT name
  , screen_name
  , COUNT(scoring)           AS Number_of_Tweets
  , AVG(scoring)             AS Aggregated_Scoring
FROM
  (SELECT name
    , tweet.screen_name      AS screen_name
    , CASE
      WHEN scoring > 0
      THEN 1
      ELSE -1
      END                    AS scoring
    FROM tweet INNER JOIN user
     ON tweet.screen_name = user.screen_name
     LEFT JOIN sentiment
     ON tweet.id = sentiment.id
    WHERE text LIKE '%climate change%'
      OR text LIKE '%global warming%'
      OR text LIKE '%changing climate%')
GROUP BY name
  , screen_name
HAVING Number_of_Tweets > 2
ORDER BY Aggregated_Scoring DESC

-- COMMAND ----------

-- MAGIC %md
-- MAGIC Export the dataset to R programming language.

-- COMMAND ----------

SELECT tweet.id              AS id
    , name
    , tweet.screen_name      AS screen_name
    , text
    , CASE
      WHEN scoring > 0
      THEN 1
      ELSE -1
      END                    AS scoring
    FROM tweet INNER JOIN user
     ON tweet.screen_name = user.screen_name
     LEFT JOIN sentiment
     ON tweet.id = sentiment.id
    WHERE text LIKE '%climate change%'
      OR text LIKE '%global warming%'
      OR text LIKE '%changing climate%'

-- COMMAND ----------

-- MAGIC %md
-- MAGIC As mentioned earlier, the R script shows that climate denial detection is being built to systematically identify the text in dataset. Now, this dataset is imported to here in order to further analyze who the politicians may be climate deniers.

-- COMMAND ----------

USE Twitter;

CREATE TABLE IF NOT EXISTS denial3
USING csv
OPTIONS (
  path
  "dbfs:/FileStore/tables/climatedenial-3.csv",
  HEADER = "TRUE",
  INFERSCHEMA = "TRUE"
);

CACHE TABLE denial3

-- COMMAND ----------

SELECT name,
  ROUND(SUM(Potential_Climate_Denial)/COUNT(Potential_Climate_Denial),2)  AS Potential_Climate_Denial
FROM denial3
GROUP BY name
HAVING COUNT(id) > 5
  AND ROUND(SUM(Potential_Climate_Denial)/COUNT(Potential_Climate_Denial),2) > 0
ORDER BY Potential_Climate_Denial DESC

-- COMMAND ----------

SELECT Name
 , ROUND(SUM(Potential_Climate_Denial)/COUNT(Potential_Climate_Denial),2) AS Potential_Climate_Denial
 , ROUND(AVG(scoring),2)
FROM
 (SELECT denial3.name           AS Name
       , tweet.screen_name      AS screen_name
       , Potential_Climate_Denial
       , CASE
           WHEN scoring > 0
           THEN 1
           ELSE -1
         END                    AS scoring
       FROM tweet INNER JOIN user
        ON tweet.screen_name = user.screen_name
        LEFT JOIN sentiment
        ON tweet.id = sentiment.id
        INNER JOIN denial3
        ON user.name = denial3.name
       WHERE text LIKE '%climate change%'
         OR text LIKE '%global warming%'
         OR text LIKE '%changing climate%')
 GROUP BY name
 HAVING ROUND(SUM(Potential_Climate_Denial)/COUNT(Potential_Climate_Denial),2) > 0
   AND ROUND(SUM(Potential_Climate_Denial)/COUNT(Potential_Climate_Denial),2) < 1
 ORDER BY Potential_Climate_Denial DESC
