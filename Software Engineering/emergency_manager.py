import mysql.connector
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime
from uploader import connect_to_db
import smtplib
from email.message import EmailMessage

def get_patient_details(pt_id):
    connection = connect_to_db()
    cursor = connection.cursor()
    cursor.execute("SELECT pt_first_name, pt_last_name, pt_address, pt_phone FROM patients WHERE pt_id = %s", (pt_id,))
    result = cursor.fetchone()
    connection.close()
    return result


def get_patient_location(pt_id):
    connection = connect_to_db()
    cursor = connection.cursor()
    cursor.execute("SELECT pt_location FROM patients WHERE pt_id = %s", (pt_id,))
    result = cursor.fetchone()
    connection.close()
    return result[0] if result else None

def get_assigned_staff(pt_id):
    connection = connect_to_db()
    cursor = connection.cursor()
    cursor.execute("SELECT staff_id FROM emergency_log WHERE pt_id = %s", (pt_id,))
    result = cursor.fetchone()
    connection.close()
    return result[0] if result else None

def get_available_staff_locations():
    connection = connect_to_db()
    cursor = connection.cursor()
    cursor.execute("SELECT staff_id, staff_location FROM staff WHERE staff_id NOT IN (SELECT staff_id FROM emergency_log)")
    result = cursor.fetchall()
    connection.close()
    return result

def send_message_to_closest_staff(pt_id, pt_location, available_staff_locations):
    closest_staff_id = None
    closest_distance = float("inf")
    for staff_id, staff_location in available_staff_locations:
        distance = abs(int(pt_location) - int(staff_location))
        if distance < closest_distance:
            closest_distance = distance
            closest_staff_id = staff_id

    print(f"Sending message to staff {closest_staff_id}")

    return closest_staff_id

def send_email_to_staff(staff_email, subject, body):
    # Define the email message
    message = EmailMessage()
    message.set_content(body)

    message["Subject"] = subject
    message["From"] = "benjaminpannysw@gmail.com" 
    message["To"] = staff_email

    # Send the email using smtplib
    try: 
        with smtplib.SMTP_SSL("smtp.gmail.com", 465) as server:
            server.login("benjaminpannysw@gmail.com", "pw") 
            server.send_message(message)
        print(f"Email sent to {staff_email}")
    except Exception as e:
        print(f"Error sending email: {e}")
#
def get_emergency_info(pt_id):
    with ThreadPoolExecutor() as executor:
        pt_location_future = executor.submit(get_patient_location, pt_id)
        assigned_staff_future = executor.submit(get_assigned_staff, pt_id)
        available_staff_locations_future = executor.submit(get_available_staff_locations)

        pt_location = pt_location_future.result()
        assigned_staff = assigned_staff_future.result()
        available_staff_locations = available_staff_locations_future.result()

    return pt_location, assigned_staff, available_staff_locations

def log_emergency_status(pt_id, staff_id, message_count):
    connection = connect_to_db()
    cursor = connection.cursor()
    timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    cursor.execute("""
        INSERT INTO emergency_log (pt_id, staff_id, message_count, timestamp)
        VALUES (%s, %s, %s, %s)
        """,
        (pt_id, staff_id, message_count, timestamp)
    )
    connection.commit()
    connection.close()


def emergency_manager(pt_id):
    pt_location, assigned_staff, available_staff_locations = get_emergency_info(pt_id)

    # Get the patient details
    pt_first_name, pt_last_name, pt_address, pt_phone = get_patient_details(pt_id)

    if assigned_staff:
        # Fetch the assigned staff's email
        connection = connect_to_db()
        cursor = connection.cursor()
        cursor.execute("SELECT staff_email FROM staff WHERE staff_id = %s", (assigned_staff,))
        staff_email = cursor.fetchone()[0]
        connection.close()

        # Send an email to the assigned staff
        print(f"Sending message to assigned staff {assigned_staff}")
        staff_id = assigned_staff
    else:
        staff_id = send_message_to_closest_staff(pt_id, pt_location, available_staff_locations)
        # Fetch the closest staff's email
        connection = connect_to_db()
        cursor = connection.cursor()
        cursor.execute("SELECT staff_email FROM staff WHERE staff_id = %s", (staff_id,))
        staff_email = cursor.fetchone()[0]
        connection.close()

    # Send an email to the selected staff member
    subject = f"Emergency request for Patient ID: {pt_id}"
    body = f"Patient Details:\nName: {pt_first_name} {pt_last_name}\nID: {pt_id}\nPhone: {pt_phone}\nAddress: {pt_address}"
    send_email_to_staff(staff_email, subject, body)

    # Log the emergency status as "open"
    log_emergency_status(pt_id, staff_id, 1)

