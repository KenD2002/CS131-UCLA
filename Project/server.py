import asyncio
import argparse
import datetime
import logging
import setting
import aiohttp
import json
from pathlib import Path

class Server:
    def __init__(self, server):
        # initialization for the server. set server name and port number
        self.port = setting.PORT_NUM[server]
        self.server = server

        self.client_info = {}

        # create client log directory
        logs_path = Path('logs')
        if not logs_path.exists():
            logs_path.mkdir(parents=True)
        
        # get the current time
        time_current = datetime.datetime.now()

        # format the current time as a string in the format 'YYYY-MM-DDTHH:MM:SS'
        time_string = time_current.strftime('%Y-%m-%dT%H:%M:%S')

        # Configure the logging settings
        logging.basicConfig(
            # set the log file name to 'client-<current_time>.txt',
            # creating a unique log file for each run
            filename = f'logs/{server}-{time_string}.txt', 
            # set the encoding of the log file to UTF-8
            encoding = 'utf-8', 
            # set the log message format to include the timestamp,
            # log level, and message
            format = '%(asctime)s %(levelname)-8s %(message)s', 
            # set the logging level to DEBUG to capture all levels of log messages
            level = logging.DEBUG
        )
        
        # Log an initial message indicating that the client connection has been initiated
        logging.info('Session for server {server} started\n')


    # Asynchronous method to process input from a client
    async def process_input(self, reader, writer):
        # Continuously read from the reader until the end of the stream is reached
        while not reader.at_eof():
            # Read a line of input data from the reader
            data = await reader.readline()
            
            # Decode the received data from bytes to a string
            msg = data.decode()
            
            # Get the address of the client
            addr = writer.get_extra_info('peername')
            
            # Log the received message along with the client's address
            self.normal_log(f'Received from {addr}: {msg}')

            # Parse the received message and get the response
            resp = await self.msg_parser(msg)
            
            # If there is a response to send back to the client
            if resp:
                # Encode the response to bytes and write it to the writer
                writer.write(resp.encode())
                
                # Log the response sent to the client
                self.normal_log(f'Response sent to client {addr}: {resp}')

            # Close the connection to the client
            writer.close()
            
            # Log the closure of the connection
            self.normal_log(f'Session with {addr} terminated')

    
    # Asynchronous method to parse messages
    async def msg_parser(self, msg):
        # Dictionary mapping commands to their respective handling methods
        commands = {
            'IAMAT': self.process_IAMAT,
            'WHATSAT': self.process_WHATSAT,
            'AT': self.process_AT
        }

        # Strip the message of leading and trailing whitespace, split it by spaces, and filter out any empty strings
        msg_list = [message for message in msg.strip().split() if len(message)]

        # If the message list is empty, log an error and return an invalid message response
        if len(msg_list) == 0:
            self.error_log(f'Invalid message: {msg}')
            return f'? {msg}'
        
        # If the first element of the message list is not a valid command, log an error and return an invalid message response
        if not msg_list[0] in commands:
            self.error_log(f'Invalid message: {msg}')
            return f'? {msg}'
        
        # Dictionary specifying the required number of arguments for each command
        number_of_arg = {
            'IAMAT': 4,
            'WHATSAT': 4,
            'AT': 6
        }

        # If the length of the message list does not match the required number of arguments for the command, log an error and return an invalid message response
        if len(msg_list) != number_of_arg[msg_list[0]]:
            self.error_log(f'Invalid number of args for {msg_list[0]}: {len(msg_list)}')
            return f'? {msg}'

        # Retrieve the appropriate command handler method
        cmd = commands.get(msg_list[0])

        try:
            # Call the command handler method with the message arguments and await its response
            response = await cmd(*msg_list[1:])
            return response
        except Exception as e:
            # If an error occurs while handling the command, log the error and return an invalid message response
            self.error_log(f'Error while handling command: {e}')
            return f'? {msg}'
        

    # Asynchronous method to process the IAMAT command
    async def process_IAMAT(self, client, coord, timestamp):
        # Split the coordinate string into latitude and longitude, handling both positive and negative values
        coords = [s for s in coord.replace('-', '+').split('+') if s != '']

        # Validate the coordinates: there should be exactly two values, and both should be numeric
        if len(coords) != 2 or not self.is_numeric(coords[0]) or not self.is_numeric(coords[1]):
            raise Exception(f'invalid IAMAT message coord: {coord}\n')

        # Validate the timestamp: it should be numeric
        if not self.is_numeric(timestamp):
            raise Exception(f'invalid IAMAT message timestamp: {timestamp}\n')

        # Calculate the time difference between the current time and the provided timestamp
        time_diff = datetime.datetime.now().timestamp() - float(timestamp)

        # Format the time difference as a string with a leading '+' for positive values
        time_str = '+' + str(time_diff) if time_diff > 0 else str(time_diff)

        # Construct the response string in the AT message format
        response_str = f'AT {self.server} {time_str} {client} {coord} {timestamp}'
        
        # Store the client information with the timestamp and response string
        self.client_info[client] = {'timestamp': timestamp, 'msg': response_str}

        # Propagate the response to other servers asynchronously
        await self.propagate(response_str)

        # Return the response string
        return response_str


    # Asynchronous method to process the WHATSAT command
    async def process_WHATSAT(self, client, radius, numplaces):
        # Check if the client is in the client_info dictionary
        if not client in self.client_info:
            raise Exception(f'invalid WHATSAT message client field: {client}\n')

        # Validate the radius: it should be numeric, non-negative, and not exceed 50
        if not self.is_numeric(radius) or float(radius) < 0 or float(radius) > 50:
            raise Exception(f'invalid WHATSAT message radius field: {radius}\n')

        # Validate the number of places: it should be numeric, non-negative, and not exceed 20
        if not self.is_numeric(numplaces) or float(numplaces) < 0 or float(numplaces) > 20:
            raise Exception(f'invalid WHATSAT message numplaces field: {numplaces}\n')

        # Retrieve the AT message string for the given client
        at_str = self.client_info[client]['msg']

        # Extract the coordinates from the AT message
        coord = at_str.split()[4]

        # Get the place information using the coordinates, radius, and number of places
        places_str = (await self.get_place(coord, radius, numplaces)).rstrip('\n')

        # Construct and return the final response string
        return f'{at_str}\n{places_str}\n\n'

    # Asynchronous method to process the AT command
    async def process_AT(self, server, time_diff, client, coord, timestamp):
        # Check if the client is not in the client_info dictionary or the provided timestamp is newer than the stored timestamp
        if not client in self.client_info or timestamp > self.client_info[client]['timestamp']:
            # Construct the AT message string with the provided parameters
            at_str = f'AT {server} {time_diff} {client} {coord} {timestamp}'
            
            # Update the client_info dictionary with the new timestamp and message string
            self.client_info[client] = {'timestamp': timestamp, 'msg':at_str}
            
            # Log that new propagated information about the client's location has been received
            self.normal_log(f'Received new propagated information about location of {client}\n')
            
            # Propagate the AT message to other servers asynchronously
            await self.propagate(at_str)
        else:
            # Log that redundant information for the client has been received and will be ignored
            self.normal_log(f'Received redundant propagated info for client {client}, ignoring.\n')
    
    # Asynchronous method to retrieve places from Google Places API
    async def get_place(self, coord, radius, numplaces):
        # Create an asynchronous HTTP client session
        async with aiohttp.ClientSession() as session:
            # Construct the URL for the Google Places API request
            url = f'https://maps.googleapis.com/maps/api/place/nearbysearch/json?location={self.parse_coord(coord)}&radius={radius}&key={setting.APIKEY}'
            
            # Log the attempt to make a GET request to the constructed URL
            self.normal_log(f'Attempting to GET from {url}')
            
            # Make an asynchronous GET request to the constructed URL
            async with session.get(url) as response:
                # Parse the JSON response from the Google Places API
                places = await response.json(loads=json.loads)

            # Log the number of places retrieved from the Google Places API
            self.normal_log(f'Retrieved {len(places['results'])} places from Google Places API.')
            
            # If the number of retrieved places exceeds the specified number of places, truncate the list
            if len(places['results']) > int(numplaces):
                places['results'] = places['results'][:int(numplaces)]
                
            # Convert the places data to a JSON-formatted string with indentation
            return str(json.dumps(places, indent=4))


    # Asynchronous method to propagate messages to neighboring servers
    async def propagate(self, message):
        # Iterate over each neighbor server in the relationship dictionary
        for neighbor in setting.RELATIONSHIP[self.server]:
            try:
                # Attempt to open a connection to the neighbor server
                _, writer = await asyncio.open_connection(setting.LOCALHOST, setting.PORT_NUM[neighbor])
                
                # Send the message to the neighbor server
                writer.write(message.encode())
                self.normal_log(f'Propagating message to {neighbor}: {message}')

                # Close the connection to the neighbor server
                writer.close()
                await writer.wait_closed()
                self.normal_log(f'Closing connection with {neighbor}')
            
            except:
                # Log an error message if unable to connect to the neighbor server
                self.normal_log(f'Error connecting to server {neighbor}')


    # Asynchronous method to start the server and handle incoming connections
    async def serve(self):
        # Start the server to listen for incoming connections
        server = await asyncio.start_server(self.handle_input, setting.LOCALHOST, self.port)
        
        # Get the server's address
        addr = server.sockets[0].getsockname()
        self.normal_log(f'{self.server} serving on {addr}')
        
        # Ensure the server runs indefinitely
        async with server:
            await server.serve_forever()


    # Method to run the server
    def run(self):
        try:
            # Run the asynchronous serve method using asyncio.run
            asyncio.run(self.serve())
        except KeyboardInterrupt:
            # Log a message when the server is closed due to a keyboard interrupt (e.g., Ctrl+C)
            self.normal_log(f'Server {self.server} closed.')

    # Method to parse coordinates
    def parse_coord(self, coord):
        # Find the position of the last '+' or '-' character in the coordinate string
        split = max(coord.rfind('+'), coord.rfind('-'))
        # Return the coordinate in the format 'latitude,longitude'
        return f'{coord[:split]},{coord[split:]}'

    # Method to check if a string is numeric
    def is_numeric(self, string):
        try:
            # Try to convert the string to a float
            float(string)
            return True
        except ValueError:
            # If a ValueError occurs, the string is not numeric
            return False

    # define logging methods
    def normal_log(self, msg):
        print(msg)
        logging.info(msg)

    def error_log(self, msg):
        print(msg)
        logging.error(msg)

if __name__ == '__main__':
    # Create an argument parser for command line arguments
    parser = argparse.ArgumentParser(description='Proxy herd server')
    
    # Add a positional argument 'server' of type string to the parser
    parser.add_argument('server', type=str)
    
    # Parse the command line arguments
    args = parser.parse_args()
    
    # Check if the provided server argument is in the setting.PORT_NUM dictionary
    if not args.server in setting.PORT_NUM:
        # Print an error message and exit the program if the server is invalid
        print(f'Invalid server: {args.server}')
        exit()
    
    # Create an instance of the Server class with the provided server argument
    s = Server(args.server)
    
    # Run the server instance
    s.run()