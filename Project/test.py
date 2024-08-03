import subprocess
import asyncio
import time
import os
import logging

#Not used
logging.basicConfig(
    level=logging.INFO, 
    format='%(asctime)s - %(levelname)s - %(message)s', 
    handlers=[
        logging.FileHandler("server.log"),
        logging.StreamHandler()
    ]
)

# Using subprocess to start the servers (was using the shell cmd)
def start_servers():
    servers = ['Bailey', 'Bona', 'Campbell', 'Clark', 'Jaquez']
    processes = []
    for server in servers:
        process = subprocess.Popen(['python3', 'server.py', server])
        processes.append(process)
    return processes

# Testing the commands
async def test_servers(server, port, message):
    try:
        reader, writer = await asyncio.open_connection('127.0.0.1', port)
        #logging.info(f'Send: {message}')
        writer.write(message.encode())
        await writer.drain()
        
        data = await reader.read(100)
        #logging.info(f'Received: {data.decode()}')

        #logging.info('Close the connection')
        writer.close()
        await writer.wait_closed()
    except Exception as e:
        #logging.error(f"Error testing server {server} on port {port}: {e}")
        return

# Test cases
async def run_tests():
    commands = [
        ('Clark', 20563, 'IAMAT kiwi.cs.ucla.edu +34.068930-118.445127 1621464827.959498503'),
        ('Jaquez', 20564, 'WHATSAT kiwi.cs.ucla.edu 10 5'),
        ('Clark', 20563, 'IAMAT kiwi.cs.ucla.edu +34.068930-118.445127 1621464827.959498503')
    ]
    for cmd in commands:
        await test_servers(*cmd)
        await asyncio.sleep(1)

if __name__ == '__main__':
    try:
        processes = start_servers()
        #logging.info("Starting all servers for test.")
        time.sleep(2)  # Allow some time for the servers to start
        asyncio.run(run_tests())
    finally:
        # Terminate servers after testing
        for process in processes:
            process.terminate()
        subprocess.run(['pkill', '-f', 'server.py'])
        #logging.info("All servers terminated.")