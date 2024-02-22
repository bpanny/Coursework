# Personal-Healthcare-System-Software

**setup_tables.py** organizes tables in a database

**app.py** organizes files in ./template/ into a useable web app for staff login and reporting

**stream_shot.py** reads a datastream of screen shots from a video livestream, classifies the existence of hands in the frame, and sends messages to the uploader component. If the message is "help", it includes the emergency manager super-component in the loop

**hand.xml** is an xml file specifying a cascade classifier for hand detection

**uploader.py** uploads messages to a database and provides a function for connecting to a database server

**emergency_manager.py** handles "help" messages and enumerates an emergency solution through parallel and serial computation. Namely, the emergency manager obtains the location of the senior, a list of available staff, and the info of any staff already assigned to a senior in parallel, then it will determine which staff is closest to the senior (redundant if one already assigned) and message the one who is closest. It does this through the uploader component, which contains the database connection method so that the emergency manager can obtain staff location, availability, assignments, and senior location. After selecting a healthcare staff based on availability and closest distance, it will notify the staff and log this information in the system database, storing which staff was contacted through e-mail for this particular emergency. If a second “help” gesture is received, the emergency manager sends a message to the healthcare staff already assigned to the senior, logging the relevant information accordingly in the database. 
