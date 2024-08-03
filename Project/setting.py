# this file is storing the settings

# local host ip:
LOCALHOST = '127.0.0.1'

# API KEY:
API = 'AIzaSyDSk5Rw6Xxfln80jjDad0EBnkl3SLuic-A'

# server relationship among 'Bailey', 'Bona', 'Campbell', 'Clark', 'Jaquez':
RELATIONSHIP = {
    'Bailey': ['Bona', 'Campbell'],
    'Bona': ['Bailey', 'Campbell', 'Clark'],
    'Campbell': ['Bailey', 'Bona', 'Jaquez'],
    'Clark': ['Bona', 'Jaquez'],
    'Jaquez': ['Campbell', 'Clark']
}

# ports:
PORT_NUM = {'Bailey': 20936, 'Bona': 20937, 'Campbell': 20938, 'Clark': 20939, 'Jaquez': 20940}