import asyncio
import argparse
import datetime
import logging
import setting
from pathlib import Path

class Client:
    def _init_(self, server):
        # initialization for the client. set server name and port number
        self.port = setting.PORT_NUM[server]
        self.server = server

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
            filename = f'logs/client-{time_string}.txt', 
            # set the encoding of the log file to UTF-8
            encoding = 'utf-8', 
            # set the log message format to include the timestamp,
            # log level, and message
            format = '%(asctime)s %(levelname)-8s %(message)s', 
            # set the logging level to DEBUG to capture all levels of log messages
            level = logging.DEBUG
        )
        
        # Log an initial message indicating that the client connection has been initiated
        logging.info('Client connection is initiated\n')


    # define logging methods
    def normal_log(self, msg):
        print(msg)
        logging.info(msg)

    def error_log(self, msg):
        print(msg)
        logging.error(msg)


    # Asynchronous method to communicate with the server
    async def server_tcp_communication(self, msg):
        try:
            # Attempt to open a connection to the server at the specified host and port
            reader, writer = await asyncio.open_connection(setting.LOCALHOST, self.port)
        except:
            # If the connection fails, log an error message and exit the program
            self.error_log(f'unable to connect to server: {self.server}')
            exit()

        # Send the message to the server
        writer.write((msg + '\n').encode())
        self.normal_log(f'Message delivered to {self.server}: {msg}')

        # Wait for a response from the server
        data = await reader.read()
        self.normal_log(f'Data received from {self.server}: {data.decode()}\n')

        # Close the connection to the server
        writer.close()
        self.normal_log(f'Ended session with {self.server}\n')
    

    # read user input and communicate with the server
    def read_user_input(self):
        while True:
            try: 
                # prompt the user to input a message
                text = input('Input you message: ')
                # run the asynchronous method server_tcp_communication using the input text
                asyncio.run(self.server_tcp_communication(text))
            except KeyboardInterrupt:
                # if the user interrupts the process (e.g., by pressing Ctrl+C),
                # log a message and break the loop
                self.normal_log('Session terminated.')
                break


if __name__ == '__main__':
    # command line argument parsing
    input_parser = argparse.ArgumentParser(description='Proxy Herd Client')
    input_parser.add_argument('server', type=str)
    args = input_parser.parse_args() #parse the inputs

    # check if the server name is valid
    if not args.server in setting.PORT_NUM:
        print(f'Invaid Server: {args.server}')
        exit()

    # create client instance and start input reader
    client = Client(args.server)
    client.read_user_input()