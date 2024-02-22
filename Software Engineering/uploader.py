import mysql.connector

# Set up the MySQL connection
def connect_to_db():
    connection = mysql.connector.connect(
        host="localhost",
        user="bpanny",
        password="software",
        database="phs"
    )
    return connection

# Define the uploader function
def uploader(message_data):
    connection = connect_to_db()
    
    cursor = connection.cursor()
    query = "INSERT INTO events (pt_id, message, timestamp) VALUES (%s, %s, %s)"
    values = (message_data["pt_id"], message_data["message"], message_data["timestamp"])
    
    cursor.execute(query, values)
    connection.commit()
    cursor.close()
    connection.close()
