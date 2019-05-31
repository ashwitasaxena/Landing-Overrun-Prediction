Project details:

1. Data: Landing data (landing distance and other parameters) from 950 commercial flights 
      (not real data set but simulated from statistical models). 

   See two Excel files ‘FAA-1.xls’ (800 flights) and ‘FAA-2.xls’ (150 flights).

2. Code : i. Please refer to 'Project 1 code.R' first
          ii. Please refer to 'Project2 code.R' second
          iii. Please refer to 'Project3 code.R' third

3. Reports: The detailed reports (PDF) are named 'Ashwita Saxena Project Part 1', 'Ashwita Saxena Project part 2'
            and 'Ashwita Saxena Project Part 3' respectively for all the three projects.


Variable dictionary:

Aircraft: The make of an aircraft (Boeing or Airbus).
Duration (in minutes): Flight duration between taking off and landing. 
         The duration of a normal flight should always be greater than 40min.
No_pasg: The number of passengers in a flight.
Speed_ground (in miles per hour): The ground speed of an aircraft when passing 
         over the threshold of the runway. If its value is less than 30MPH or greater than 140MPH, then the landing would be considered as abnormal.
Speed_air (in miles per hour): The air speed of an aircraft when passing over 
         the threshold of the runway. If its value is less than 30MPH or greater than 140MPH, then the landing would be considered as abnormal.
Height (in meters): The height of an aircraft when it is passing over the threshold 
          of the runway. The landing aircraft is required to be at least 6 meters high at the threshold of the runway.
Pitch (in degrees): Pitch angle of an aircraft when it is passing over the threshold 
          of the runway.

Distance (in feet): The landing distance of an aircraft. More specifically, 
          it refers to the distance between the threshold of the runway and the point where the aircraft can be fully stopped. The length of the airport runway is typically less than 6000 feet.