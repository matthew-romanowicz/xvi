import curses, math, sys, struct

class Action:

    def __init__(self, index, deleted_data, inserted_length, inserted_data, record=False):
        self.index = index
        self.deleted_data = deleted_data
        self.inserted_length = inserted_length
        if record:
            self.inserted_data = inserted_data
        else:
            self.inserted_data = None

    def undo(self, editor):
        editor.remove_bytes(self.index, self.inserted_length, record=False)
        editor.insert_data_block(self.index, self.deleted_data, record=False)

    def redo(self, editor):
        return

class UndoRedoStack:

    def __init__(self):
        self.index = 0
        self.stack = 0

    def addAction(self, index, deleted_data, inserted_length, inserted_data, record=False):
        self.stack.append(Action(index, deleted_data, inserted_length, inserted_data))

    def undo(self, editor):
        action = self.stack[self.index]
        self.index -= 1
        action.undo()
        
        return self.stack[index].undo()

    def redo(self, editor):
        return 
        

KEY_ENTER = 10
KEY_ESCAPE = 27
KEY_BACKSPACE = 8
KEY_DELETE = 330

NO_EDIT = 0
HEX_EDIT = 2
ASCII_EDIT = 4
COMMAND = 8

OVERWRITE = 16
INSERT = 32

class HexEditor:

    def __init__(self, file, word_length, read_file=True):
        self.file = file
        self.word_length = word_length
        self.read_file = read_file
        self.command_history = []
        self.command_index = 0
        self.command_accumulator = ''
        self.action_stack = []

    def run(self):

        # Initialize screen
        stdscr = curses.initscr()
        curses.noecho()
        stdscr.keypad(True)
        curses.cbreak()

        # Determine screen size
        self.n_rows, self.n_cols = stdscr.getmaxyx()

        # Initialize data for display
        self.current_line = 0
        if self.read_file:
            self.data = list(self.file.read())
            self.display_data = self.data[:self.n_rows*self.word_length]
        else:
            self.data = None
            self.display_data = list(self.file.read(self.n_rows*self.word_length))
        self.current_command = ''

        # Determine number of digits to reserve for line numbers
        self.n_digits = math.ceil(math.log(self.file_length(), 10))

        # Initialize windows
        self.line_number_pad = curses.newwin(self.n_rows - 1, self.n_digits, 0, 0)
        self.hex_window = curses.newwin(self.n_rows - 1, 3*self.word_length - 1, 0, self.n_digits + 2)
        self.n_cols = self.n_digits + 2 + 4*self.word_length + 3
        self.ascii_window = curses.newwin(self.n_rows - 1, self.word_length, 0, self.n_cols - self.word_length)
        self.cmd_window = curses.newwin(1, self.n_cols, self.n_rows - 1, 0)

        self.edit_mode = NO_EDIT
        self.rv_cursor = 0

        # Populate windows
        self.fill_hex_window()
        self.hex_window.refresh()
        self.fill_ascii_window()
        self.ascii_window.refresh()
        self.fill_line_numbers()
        self.line_number_pad.refresh()

        #self.hex_window.keypad(True)
        #self.ascii_window.keypad(True)
        #self.cmd_window.keypad(True)


        self.running = True
        while self.running:
            if self.edit_mode == NO_EDIT:
                c = self.hex_window.getch()
                if c == 105: # i
                    self.edit_mode = HEX_EDIT | INSERT
                    self.hex_window.keypad(True)
                elif c == 111: # o
                    self.edit_mode = HEX_EDIT | OVERWRITE
                    self.hex_window.keypad(True)
                elif c == 73: # I
                    self.edit_mode = ASCII_EDIT | INSERT
                    self.ascii_window.keypad(True)
                elif c == 79: # O
                    self.edit_mode = ASCII_EDIT | OVERWRITE
                    self.ascii_window.keypad(True)
                elif c == 120: # x
                    index, nibble = self.hex_cursor_index()
                    self.remove_bytes(index, 1)
                elif 48 <= c <= 57: #0-9
                    self.command_accumulator += chr(c)
                elif c in [43, 45]: # -, +
                    self.command_accumulator += chr(c)
                elif c == 97: # a
                    if self.command_accumulator and self.command_accumulator.isnumeric():
                        index, nibble = self.hex_cursor_index()
                        self.insert_bytes(index, int(self.command_accumulator))
                        self.move_hex_cursor(index, nibble)
                        self.command_accumulator = ''
                elif c == 100: # d
                    if self.command_accumulator and self.command_accumulator.isnumeric():
                        index, nibble = self.hex_cursor_index()
                        self.remove_bytes(index, int(self.command_accumulator))
                        self.move_hex_cursor(index, nibble)
                        self.command_accumulator = ''
                elif c == 103: # g
                    if self.command_accumulator:
                        index, nibble = self.hex_cursor_index()
                        if self.command_accumulator.endswith('+'):
                            if self.command_accumulator[:-1].isnumeric():
                                self.move_hex_cursor(index + int(self.command_accumulator[:-1]), 'left')
                                self.hex_window.refresh()
                        elif self.command_accumulator.endswith('-'):
                            if self.command_accumulator[:-1].isnumeric():
                                self.move_hex_cursor(index - int(self.command_accumulator[:-1]), 'left')
                                self.hex_window.refresh()
                        elif self.command_accumulator.isnumeric():
                            self.move_hex_cursor(int(self.command_accumulator), 'left')
                            self.hex_window.refresh()
                        self.command_accumulator = ''
                    else:
                        self.move_hex_cursor(0, 'left')
                        self.hex_window.refresh()
                elif c == 71: # G
                    self.move_hex_cursor(len(self.data) - 1, 'left')
                    self.hex_window.refresh()
                elif c == 117: # u
                    action = self.action_stack.pop(-1)
                    action.undo(self)
                    self.move_hex_cursor(action.index, "left")
                    
                elif c in [47, 58, 60, 62]: # /, :, <, >
                    self.edit_mode = COMMAND
                    self.cmd_window.keypad(True)
                    curses.ungetch(c)

                self.update_status()

            elif self.edit_mode & COMMAND:
                c = self.cmd_window.getch()
                if c == KEY_ESCAPE:
                    self.escape_edit()
                else:
                    self.cmd_edit_input(c)
                    
            elif self.edit_mode & HEX_EDIT:
                c = self.hex_window.getch()
                if c == KEY_ESCAPE:
                    self.escape_edit()
                else:
                    self.hex_edit_input(c)

            elif self.edit_mode & ASCII_EDIT:
                c = self.ascii_window.getch()
                if c == KEY_ESCAPE:
                    self.escape_edit()
                else:
                    self.ascii_edit_input(c)                

        curses.nocbreak()
        stdscr.keypad(False)
        curses.echo()
        curses.endwin()

    def escape_edit(self):
        self.hex_window.keypad(True)
        self.ascii_window.keypad(True)
        self.cmd_window.keypad(True)
        self.edit_mode = NO_EDIT
        self.update_status()

    def file_length(self):
        if self.read_file:
            return len(self.data)
        else:
            self.file.seek(0, 2)
            return self.file.tell()

    def cmd_edit_input(self, c):
        y, x = self.cmd_window.getyx()
        if c == curses.KEY_LEFT:
            self.cmd_window.move(y, x-1)
        elif c == curses.KEY_RIGHT:
            self.cmd_window.move(y, x+1)
        elif c == KEY_ENTER:
            self.cmd_window.clear()
            self.execute_command()
        else:
            self.cmd_window.clear()
            if c == curses.KEY_UP:
                self.command_index += 1
                self.current_command = self.command_history[-self.command_index]
                x = 0
            elif c == curses.KEY_DOWN:
                self.command_index -= 1
                self.current_command = self.command_history[-self.command_index]
                x = 0
            elif c == KEY_BACKSPACE:
                self.current_command = self.current_command[:x-1] + self.current_command[x:]
                x -= 1
            elif c == KEY_DELETE:
                self.current_command = self.current_command[:x] + self.current_command[x+1:]
            else:
                self.current_command = self.current_command[:x] + chr(c) + self.current_command[x:]
                x += 1

            self.cmd_window.addstr(0, 0, self.current_command)
            self.cmd_window.move(y, x)

        self.cmd_window.refresh()

    def ascii_edit_input(self, c):
        index = self.ascii_cursor_index()
        if c == curses.KEY_UP:
            self.move_ascii_cursor(index - self.word_length)
        elif c == curses.KEY_DOWN:
            self.move_ascii_cursor(index + self.word_length)
        elif c == curses.KEY_LEFT:
            self.move_ascii_cursor(index - 1)
        elif c == curses.KEY_RIGHT:
            self.move_ascii_cursor(index + 1)
        elif c == curses.KEY_END:
            self.move_ascii_cursor(self.file_length()-1)
        elif c == curses.KEY_HOME:
            self.move_ascii_cursor(0)
        elif c == curses.KEY_PPAGE:
            y, x = self.ascii_window.getyx()
            self.rv_cursor -= (self.n_rows - 1)*self.word_length
            self.scroll_to_line(self.current_line - (self.n_rows - 1))
            self.ascii_window.move(y, x)
        elif c == curses.KEY_NPAGE:
            y, x = self.ascii_window.getyx()
            self.rv_cursor += (self.n_rows - 1)*self.word_length
            self.scroll_to_line(self.current_line + (self.n_rows - 1))
            self.ascii_window.move(y, x)
        elif self.edit_mode & OVERWRITE:
            if 0 <= c <= 255:
                self.overwrite_data(index, c, 'none')
                self.move_ascii_cursor(index + 1)
        elif self.edit_mode & INSERT:
            if c == KEY_BACKSPACE:
                self.remove_bytes(index, 1)
                self.move_ascii_cursor(index-1)
            elif 0 <= c <= 255:
                self.insert_bytes(index, 1, c)
                self.move_ascii_cursor(index + 1)

        self.hex_window.refresh()
        self.ascii_window.refresh()

    def hex_edit_input(self, c):
        index, nibble = self.hex_cursor_index()
        if c == curses.KEY_UP:
            self.move_hex_cursor(index - self.word_length, nibble)
        elif c == curses.KEY_DOWN:
            self.move_hex_cursor(index + self.word_length, nibble)
        elif c == curses.KEY_LEFT:
            if nibble == 'left':
                self.move_hex_cursor(index - 1, 'right')
            else:
                self.move_hex_cursor(index, 'left')
        elif c == curses.KEY_RIGHT:
            if nibble == 'left':
                self.move_hex_cursor(index, 'right')
            else:
                self.move_hex_cursor(index + 1, 'left')
        elif c == curses.KEY_END:
            self.move_hex_cursor(self.file_length()-1, 'right')
        elif c == curses.KEY_HOME:
            self.move_hex_cursor(0, 'left')
        elif c == curses.KEY_PPAGE:
            y, x = self.hex_window.getyx()
            self.rv_cursor -= (self.n_rows - 1)*self.word_length
            self.scroll_to_line(self.current_line - (self.n_rows - 1))
            self.hex_window.move(y, x)
        elif c == curses.KEY_NPAGE:
            y, x = self.hex_window.getyx()
            self.rv_cursor += (self.n_rows - 1)*self.word_length
            self.scroll_to_line(self.current_line + (self.n_rows - 1))
            self.hex_window.move(y, x)
        elif self.edit_mode & OVERWRITE:
            if 97 <= c <= 102 or 48 <= c <= 57 or 65 <= c <= 70:
                y, x = self.hex_window.getyx()
                index = (self.current_line + y)*self.word_length + x//3
                nibble = ['left', 'right'][x%3]
                n = '0123456789ABCDEF'.index(chr(c).upper())
                self.overwrite_data(index, n, nibble)

                if nibble == 'left':
                    self.move_hex_cursor(index, 'right')
                else:
                    self.move_hex_cursor(index + 1, 'left')
        elif self.edit_mode & INSERT:
            if c == KEY_BACKSPACE:
                index, nibble = self.hex_cursor_index()
                if nibble == 'left':
                    self.remove_bytes(index, 1)
                    self.move_hex_cursor(index-1, 'right')
                else:
                    self.overwrite_data(index, 0, nibble)
                    self.move_hex_cursor(index, 'left')
            elif 97 <= c <= 102 or 48 <= c <= 57 or 65 <= c <= 70:
                y, x = self.hex_window.getyx()
                index = (self.current_line + y)*self.word_length + x//3
                nibble = ['left', 'right'][x%3]
                n = '0123456789ABCDEF'.index(chr(c).upper())

                if nibble == 'left':
                    self.insert_bytes(index, 1, n << 4)
                    self.move_hex_cursor(index, 'right')
                else:
                    self.overwrite_data(index, n, nibble)
                    self.move_hex_cursor(index + 1, 'left')
                #hex_window.addch(y, x, chr(c).upper())
##                if x + 1 != self.hex_window.getmaxyx()[1]:
##                    if x % 3 == 1:
##                        self.hex_window.move(y, x+2)
##                    else:
##                        self.hex_window.move(y, x+1)
##                elif y != self.n_rows:
##                    self.hex_window.move(y+1, 0)          

        self.hex_window.refresh()
        self.ascii_window.refresh()

    def execute_command(self):
        self.command_index = 0
        self.command_history.append(self.current_command)
        if self.current_command[0] == ':':
            cmd = self.current_command[1:]
            if cmd == 'q!':
                self.running = False
            elif cmd.isnumeric():
                self.move_hex_cursor(int(cmd), 'left')
                self.hex_window.refresh()
                self.edit_mode = HEX_EDIT | OVERWRITE
            elif cmd[0] in '+-' and cmd[1:].isnumeric():
                index, nibble = self.hex_cursor_index()
                if cmd[0] == '+':
                    index += int(cmd[1:])
                else:
                    index -= int(cmd[1:])
                self.move_hex_cursor(index, nibble)
                self.hex_window.refresh()
                self.edit_mode = HEX_EDIT | OVERWRITE
            elif cmd[0] in 'io':
                index, nibble = self.hex_cursor_index()
                fmt, value = cmd[2:].split(' ')
                if fmt[0] in '<>':
                    t = fmt[1]
                else:
                    t = fmt
                if t in 'bBhHiIlLqQ':
                    data = struct.pack(fmt, int(value))
                elif t in 'efd':
                    data = struct.pack(fmt, float(value))
                else:
                    raise SomeError
                if cmd[0] == 'i':
                    self.insert_data_block(index, data)
                elif cmd[0] == 'o':
                    self.overwrite_data_block(index, data)

                self.move_hex_cursor(index + len(data), nibble)
                self.hex_window.refresh()
                    
        elif self.current_command[0] == '>':
            index, nibble = self.hex_cursor_index()
            if self.current_command[1:].isnumeric():
                self.insert_bits(index, int(self.current_command[1:]))
            elif self.current_command[1] == '>' and self.current_command[2:].isnumeric():
                self.insert_bytes(index, int(self.current_command[2:]))
                self.move_hex_cursor(index, nibble)
        elif self.current_command[0] == '<':
            index, nibble = self.hex_cursor_index()
            if self.current_command[1:].isnumeric():
                pass
            if self.current_command[1] == '<' and self.current_command[2:].isnumeric():
                self.remove_bytes(index, int(self.current_command[2:]))
                self.move_hex_cursor(index, nibble)
        elif self.current_command[0] == '/':
            if self.current_command[1] == '&':
                pass
            elif self.current_command[1] == '|':
                pass
            else:
                index, nibble = self.hex_cursor_index()
                data = bytearray.fromhex(self.current_command[1:])
                self.search_binary(index, data)
                self.hex_window.refresh()
                self.edit_mode = HEX_EDIT | OVERWRITE

        self.current_command = ''

    def update_status(self):
        if self.edit_mode & HEX_EDIT:
            index, nibble = self.hex_cursor_index()
        elif self.edit_mode & ASCII_EDIT:
            index = self.ascii_cursor_index()
        else:
            self.cmd_window.clear()
            self.cmd_window.refresh()
            return            
        if self.edit_mode & INSERT:
            s = 'INS'
        elif self.edit_mode & OVERWRITE:
            s = 'OVR'
        else:
            self.cmd_window.clear()
            self.cmd_window.refresh()
            return

        try:
            self.cmd_window.addstr(0, self.n_cols-3, s, curses.A_REVERSE)
        except curses.error:
            pass

        i = str(index)
        self.cmd_window.addstr(0, self.n_cols-4 - len(i), i)

        

        self.cmd_window.refresh()
        

    def insert_bytes(self, index, n, value=0, record=True):
        if self.read_file:
            for i in range(n):
                self.data.insert(index, value)
            self.display_data = self.data[self.current_line*self.word_length:(self.current_line+self.n_rows)*self.word_length]

        if record:
            self.action_stack.append(Action(index, b'', n, bytes([value]*n), False))
        
        self.fill_hex_window()
        self.hex_window.refresh()
        self.fill_ascii_window()
        self.ascii_window.refresh()

    def remove_bytes(self, index, n, record=True):
        deleted_data = []
        if self.read_file:
            for i in range(n):
                deleted_data.append(self.data.pop(index))
            self.display_data = self.data[self.current_line*self.word_length:(self.current_line+self.n_rows)*self.word_length]

        if record:
            self.action_stack.append(Action(index, bytes(deleted_data), 0, b'', False))
            
        self.fill_hex_window()
        self.hex_window.refresh()
        self.fill_ascii_window()
        self.ascii_window.refresh()

    def insert_bits(self, index, n, value=0):
        if self.read_file:
            for i in range(index, index+n//8):
                self.data.insert(index, 0xff if value else 0x00)

            offset = n%8
            carry = ((0xff if value else 0x00) >> n) << n
            for i in range(index+n//8, len(self.data)):
                temp = (self.data[i] << carry) & 0xff
                self.data[i] = self.data[i] >> n + carry
                carry = temp
            self.display_data = self.data[self.current_line*self.word_length:(self.current_line+self.n_rows)*self.word_length]
            
        self.fill_hex_window()
        self.hex_window.refresh()
        self.fill_ascii_window()
        self.ascii_window.refresh()

    def insert_data_block(self, index, data, record=True):
        if self.read_file:
            for c in reversed(data):
                self.data.insert(index, int(c))
            self.display_data = self.data[self.current_line*self.word_length:(self.current_line+self.n_rows)*self.word_length]
            
        self.fill_hex_window()
        self.hex_window.refresh()
        self.fill_ascii_window()
        self.ascii_window.refresh()

    def overwrite_data_block(self, index, data):
        if self.read_file:
            for i, c in enumerate(data):
                self.data[index + i] = int(c)
            self.display_data = self.data[self.current_line*self.word_length:(self.current_line+self.n_rows)*self.word_length]
            
        self.fill_hex_window()
        self.hex_window.refresh()
        self.fill_ascii_window()
        self.ascii_window.refresh()        

    def search_binary(self, index, data):
        n = len(data)
        for i, c in enumerate(self.data):
            if c == data[0]:
                for j in range(1, n):
                    if self.data[i+j] != data[j]:
                        break
                else:
                    self.move_hex_cursor(i, "left")
                    return

    def hex_cursor_index(self):
        y, x = self.hex_window.getyx()
        index = (self.current_line + y)*self.word_length + x//3
        nibble = ['left', 'right'][x%3]
        return index, nibble

    def ascii_cursor_index(self):
        y, x = self.ascii_window.getyx()
        index = (self.current_line + y)*self.word_length + x
        return index

    def set_ascii_cursor_attr(self, attr):
        y = self.rv_cursor//self.word_length
        if self.current_line <= y < self.current_line + self.n_rows - 1:
            x = (self.rv_cursor % self.word_length)
            self.ascii_window.chgat(y - self.current_line, x, 1, attr)        

    def set_hex_cursor_attr(self, attr):
        y = self.rv_cursor//self.word_length
        if self.current_line <= y < self.current_line + self.n_rows - 1:
            x = (self.rv_cursor % self.word_length)*3
            self.hex_window.chgat(y - self.current_line, x, 2, attr)

    def move_ascii_rv_cursor(self, index):
        self.set_ascii_cursor_attr(0)
        self.rv_cursor = index
        self.set_ascii_cursor_attr(curses.A_REVERSE)
        self.ascii_window.refresh()

    def move_hex_rv_cursor(self, index):
        self.set_hex_cursor_attr(0)
        self.rv_cursor = index
        self.set_hex_cursor_attr(curses.A_REVERSE)
        self.hex_window.refresh()
        
            

    def move_hex_cursor(self, index, nibble):
        index = max(index, 0)
        line = index//self.word_length
        x = (index % self.word_length)*3
        if nibble == 'right':
            x += 1
            
        if self.current_line <= line:
            
            if line < self.current_line + self.n_rows - 1 :
                # Index is inside current view
                y = line - self.current_line

            else:
                # Index is below current view

                self.scroll_to_line(line - (self.n_rows - 1) + 1)
                y = self.n_rows - 2
        else:
            self.scroll_to_line(line)
            y = 0

        self.move_ascii_rv_cursor(index)
        try:
            self.hex_window.move(y, x)
        except curses.error:
            print(y, x, self.n_rows)
            raise

        self.update_status()

    def move_ascii_cursor(self, index):
        index = max(index, 0)
        line = index//self.word_length
        x = (index % self.word_length)

        if self.current_line <= line:
            
            if line < self.current_line + self.n_rows - 1 :
                # Index is inside current view
                y = line - self.current_line

            else:
                # Index is below current view

                self.scroll_to_line(line - (self.n_rows - 1) + 1)
                y = self.n_rows - 2
        else:
            self.scroll_to_line(line)
            y = 0

        self.move_hex_rv_cursor(index)
        try:
            self.ascii_window.move(y, x)
        except curses.error:
            print(y, x, self.n_rows)
            raise

        self.update_status()
            

    def scroll_to_line(self, line_number):
        #print(line_number)
        line_number = max(0, line_number)
        if self.read_file:
            self.display_data = self.data[line_number*self.word_length:(line_number+self.n_rows)*self.word_length]
        else:
            self.file.seek(line_number*self.word_length)
            self.display_data = list(self.file.read(self.n_rows*self.word_length))
        self.current_line = line_number
        self.fill_hex_window()
        self.hex_window.refresh()
        self.fill_ascii_window()
        self.ascii_window.refresh()
        self.fill_line_numbers()
        self.line_number_pad.refresh()        

    def fill_line_numbers(self):
        for y in range(self.n_rows - 1):
            line_number = ('{0:0%sd}' % self.n_digits).format((y + self.current_line)*self.word_length)
            try:
                self.line_number_pad.addstr(y, 0, line_number, curses.A_DIM)
            except curses.error:
                pass        

    def fill_hex_window(self):
        for y in range(self.n_rows - 1):
            b = self.display_data[y*self.word_length:(y+1)*self.word_length]
            line = ' '.join(['{0:02x}'.format(c).upper() for c in b] + ['~~']*(self.word_length - len(b)))
            try:
                self.hex_window.addstr(y, 0, line)
            except:
                pass

        self.set_hex_cursor_attr(curses.A_REVERSE)

    def fill_ascii_window(self):
        for y in range(self.n_rows - 1):
            b = self.display_data[y*self.word_length:(y+1)*self.word_length]
            line = ''.join([chr(c) if 0x20 <= c <= 0x7E else '.' for c in b] + [' ']*(self.word_length - len(b)))
            try:
                self.ascii_window.addstr(y, 0, line)
            except curses.error:
                pass

        self.set_ascii_cursor_attr(curses.A_REVERSE)

    def overwrite_data(self, index, c, nibble='none'):

        display_index = index - self.word_length*self.current_line

        if display_index >= len(self.display_data):
            self.display_data += [0]*(display_index - len(self.display_data) + 1)
            current_data = 0
        else:
            current_data = self.display_data[display_index]

        if nibble == 'left':
            c = (current_data & 0x0F) | (c << 4)
        elif nibble == 'right':
            c = (current_data & 0xF0) | c
        
        self.display_data[display_index] = c
        if self.read_file:
            if index >= len(self.data):
                self.data += [0]*(index - len(self.data) + 1)
            self.data[index] = c

        y = display_index//self.word_length
        x = (display_index % self.word_length)

        try:
            self.hex_window.addstr(y, x*3, '{0:02x}'.format(c).upper())
        except curses.error:
            pass
        try:
            self.ascii_window.addstr(y, x, chr(c) if 0x20 <= c <= 0x7E else '.')
        except curses.error:
            pass

        self.hex_window.refresh()
        self.ascii_window.refresh()

    def scroll_down(self):

        y, x = self.hex_window.getyx()
        if self.read_file:
            self.current_line += 1
            self.display_data = self.data[self.current_line*self.word_length:(self.current_line+self.n_rows)*self.word_length]
        else:
            self.file.seek((self.current_line + self.n_rows - 1)*self.word_length)
            self.display_data = self.display_data[self.word_length:] + list(self.file.read(self.word_length))
            self.current_line += 1
        self.fill_hex_window()
        self.hex_window.move(y, x)
        self.hex_window.refresh()
        self.fill_ascii_window()
        self.ascii_window.refresh()
        self.fill_line_numbers()
        self.line_number_pad.refresh()

    def scroll_up(self):

        y, x = self.hex_window.getyx()
        if self.read_file:
            self.current_line -= 1
            self.display_data = self.data[self.current_line*self.word_length:(self.current_line+self.n_rows)*self.word_length]
        else:
            self.file.seek((self.current_line - 1)*self.word_length)
            self.display_data = list(self.file.read(self.word_length)) + self.display_data[:-self.word_length]
            self.current_line -= 1
        self.fill_hex_window()
        self.hex_window.move(y, x)
        self.hex_window.refresh()
        self.fill_ascii_window()
        self.ascii_window.refresh()
        self.fill_line_numbers()
        self.line_number_pad.refresh()

HELP_STRING = """
ARGUMENTS:
    -w: Specify number of bytes per word
    -h: Print this text
SHORTCUTS:
    Esc:\tEnter command mode
    I:\tEnter hex insert mode
    i:\tEnter ascii insert mode
    O:\tEnter hex overwrite mode
    o:\tEnter ascii overwrite mode

    u:\t[TODO] Undo last change
    x:\tDelete single byte at cursor
    g:\tMove cursor to beginning of file
    G:\tMove cursor to end of file
    ###g:\tMove cursor to byte ###
    ###+g:\tMove cursor right ### bytes
    ###-g:\tMove cursor left ### bytes
    ###a:\tInsert ### zero-bytes at cursor
    ###d:\tDelete ### bytes to the right of the cursor
COMMANDS:
    :q!:\tQuit without saving
    :w:\t[TODO] Save and continue editing
    :wq\t[TODO] Save and exit
    :###:\tMove cursor to byte ###
    :+###:\tMove cursor right ### bytes
    :-###:\tMove cursor left ### bytes
    >###:\tInsert ### zero-bits at cursor
    >>###:\tInsert ### zero-bytes at cursor
    <###:\tDelete ### bits to the right of the cursor
    <<###:\tDelete ### bytes to the right of the cursor
    /###:\tSearch for occurrence of ### from cursor
    :i XXX YYY:\t Insert YYY encoded as XXX at cursor
    :o XXX YYY:\t Overwrite YYY encoded as XXX at cursor
    
"""

if __name__ == '__main__':
    filename = r"C:\Users\Matthew\Documents\Python\Projects\Code Editor\app\__pycache__\app.cpython-38.pyc"

    args = sys.argv[1:]

    if "-h" in args:
        print(HELP_STRING)
    else:
        if "-w" in args:
            word_length = int(args[args.index("-w")+1])
        else:
            word_length = 16
        with open(filename, 'rb') as infile:
            editor = HexEditor(infile, word_length)
            editor.run()

    
