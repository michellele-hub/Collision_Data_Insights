-- Project task 4: Group 9 

-- 1. Which months have the highest number of collisions? 

SELECT strftime( '%m', collision_date) AS MONTH --Extract month from collision date and name the column as MONTH
, COUNT (case_id) AS total_collisions --Counts the total number of collisions for each month
FROM collisions c
GROUP BY MONTH --Group by month to show total number of collisions
ORDER BY total_collisions DESC; --Order the output in descending order

-- 2. How many drivers over 16 were killed in traffic collisions?

SELECT victim_age, COUNT(*) -- selects victim_age and counts total number of victims 
AS num_killed --renames column
FROM victims v 
WHERE victim_role = 'driver' -- filter for only the drivers
AND victim_age >= 16 -- filters for victims at leas 16 years old
GROUP BY victim_age -- groups by the age of victims
ORDER BY num_killed DESC;  -- orders output in descending order

-- 3. Is there a connection bewteen time of day and collision?

--Selecting lighting, adding all fatalities
SELECT lighting, SUM(killed_victims) 
--Renaming the column
AS total_fatalities
FROM collisions c 
-- Omitting values equal to 0 and filtering by daylight only
WHERE killed_victims != '0' AND lighting = 'daylight'
-- Grouping lighting for sum function
GROUP BY lighting;

--Selecting lighting, adding all fatalities
SELECT lighting, SUM(killed_victims)
-- Renaming the column
AS 'total fatalities'
FROM collisions c
-- Omitting values equal to 0 and filtering by two nighttime settings
WHERE killed_victims != '0' AND lighting = 'dark with street lights' OR lighting = 'dark with no street lights'
-- Grouping lighting for sum function
GROUP BY lighting;


-- 4. What are the most common factors in fatal collisions?

SELECT collision_severity, pcf_violation_category -- selecting collision severity and pcf_violation category
AS violation, -- renaming pcf_violation category as violation
COUNT(pcf_violation_category) AS total_collisions, -- seleecting the total number OF collisions and renaming AS total_collisions
ROUND(COUNT(pcf_violation_category) * 100.0 / SUM(COUNT(pcf_violation_category)) OVER (),2) -- creates and rounds a percentage
AS percent_total -- renames collumn
FROM collisions c 
WHERE pcf_violation_category NOT LIKE 'unknown' AND pcf_violation_category IS NOT NULL -- filters out all 'unknown' and NULL responses
AND collision_severity = 'fatal' -- filters for only fatal collisions 
GROUP BY violation, collision_severity -- groups to show violation AND collision_severity 
ORDER BY total_collisions DESC -- orders output IN descending order
LIMIT 10; -- limits to 10 entries

-- 5. What is the distribution of the severity of collisions?

SELECT collision_severity, COUNT(collision_severity) -- selects collision_severity and total number of collisions
AS total, -- renames column
ROUND (COUNT (collision_severity) * 100.0 / SUM(COUNT(collision_severity)) OVER (),2) AS percent_total -- creates and rounds a percentage over total 
FROM collisions c
WHERE collision_severity NOT LIKE 'unknown' AND collision_severity IS NOT NULL -- filter out all 'unknown' and NULL responses
GROUP BY collision_severity --group to show each level of severity
ORDER BY total DESC -- orders the output in descending order
LIMIT 5; -- shows top five levels of collision severity