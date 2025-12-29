#!/usr/bin/env python3
"""
IBM System/360 COBOL F Language Server

A Language Server Protocol implementation for IBM System/360 Operating System
COBOL F (1968), based on documentation C28-6516-8 and GC28-6380-3.

This server provides:
- Code completion for reserved words, divisions, sections
- Hover information with documentation
- Go to definition for paragraphs and data items
- Find references
- Document symbols/outline
"""

import re
import sys
import json
import logging
from typing import Dict, List, Optional, Tuple, Any
from dataclasses import dataclass, field

# COBOL F 1968 Reserved Words (from C28-6516-8 Appendix A, page 139)
# Verified against actual IBM documentation
RESERVED_WORDS = {
    # Division and Section Keywords
    'IDENTIFICATION', 'ENVIRONMENT', 'DATA', 'PROCEDURE', 'DIVISION', 'SECTION',
    'CONFIGURATION', 'INPUT-OUTPUT', 'FILE', 'WORKING-STORAGE', 'LINKAGE',
    'FILE-CONTROL', 'I-O-CONTROL', 'SPECIAL-NAMES', 'REPORT', 'DECLARATIVES',

    # Identification Division Paragraphs
    'PROGRAM-ID', 'AUTHOR', 'INSTALLATION', 'DATE-WRITTEN', 'DATE-COMPILED',
    'SECURITY', 'REMARKS',

    # Environment Division
    'SOURCE-COMPUTER', 'OBJECT-COMPUTER', 'MEMORY', 'SIZE', 'WORDS',
    'MODULES', 'SEGMENT-LIMIT', 'CURRENCY', 'SIGN', 'DECIMAL-POINT',
    'CONSOLE', 'SWITCH', 'ON', 'OFF',

    # File Control
    'SELECT', 'ASSIGN', 'RESERVE', 'ORGANIZATION', 'SEQUENTIAL', 'INDEXED',
    'RELATIVE', 'DIRECT', 'DIRECT-ACCESS', 'ACCESS', 'RANDOM', 'SYMBOLIC', 'KEY',
    'ACTUAL', 'RECORD', 'TRACK-AREA', 'TRACKS', 'FILE-LIMIT', 'ALTERNATE',
    'PROCESSING', 'MODE',

    # I-O Control
    'SAME', 'AREA', 'AREAS', 'RERUN', 'EVERY', 'RECORDS', 'END', 'OF', 'REEL',
    'UNIT', 'UNITS', 'UNIT-RECORD', 'APPLY', 'WRITE-ONLY',

    # Data Division
    'FD', 'SD', 'RD', 'BLOCK', 'CONTAINS', 'CHARACTERS', 'LABEL', 'STANDARD',
    'OMITTED', 'RECORDING', 'VALUE', 'DATA', 'RECORD', 'RECORDS', 'LINES',
    'WITH', 'FOOTING', 'AT', 'REPORT', 'REPORTS', 'CODE',

    # Record Description
    'BLANK', 'WHEN', 'ZERO', 'ZEROES', 'ZEROS', 'JUSTIFIED', 'RIGHT',
    'OCCURS', 'TIMES', 'TO', 'DEPENDING', 'ASCENDING', 'DESCENDING', 'INDEXED',
    'PICTURE', 'PIC', 'REDEFINES', 'SIGN', 'IS', 'LEADING', 'FILLER',
    'USAGE', 'DISPLAY', 'DISPLAY-ST', 'COMPUTATIONAL', 'COMPUTATIONAL-1',
    'COMPUTATIONAL-2', 'COMPUTATIONAL-3', 'COMP', 'COMP-1', 'COMP-2', 'COMP-3',
    'INDEX',

    # Condition Names
    'THROUGH', 'THRU',

    # Procedure Division Verbs
    'ACCEPT', 'ADD', 'ALTER', 'CALL', 'CLOSE', 'COMPUTE', 'DISPLAY', 'DIVIDE',
    'ENTER', 'EXAMINE', 'EXHIBIT', 'EXIT', 'GO', 'IF', 'MOVE', 'MULTIPLY',
    'NOTE', 'OPEN', 'PERFORM', 'READ', 'RELEASE', 'RETURN', 'SEARCH', 'SET',
    'SORT', 'STOP', 'SUBTRACT', 'TERMINATE', 'TRANSFORM', 'USE', 'WRITE',

    # Procedure Division Keywords
    'GIVING', 'CORRESPONDING', 'ROUNDED', 'SIZE', 'ERROR', 'ON',
    'NOT', 'INTO', 'BY', 'FROM', 'TALLYING', 'REPLACING', 'ALL', 'LEADING',
    'FIRST', 'BEFORE', 'AFTER', 'WITH', 'THEN', 'ELSE', 'VARYING', 'UNTIL',
    'WHEN', 'OTHERWISE', 'TIMES', 'INVALID', 'USING', 'ADVANCING', 'PAGE',
    'LINE', 'LINES', 'INPUT', 'OUTPUT', 'I-O', 'REVERSED', 'NO', 'REWIND',
    'LOCK', 'HOLD',

    # Conditional Keywords
    'EQUAL', 'GREATER', 'LESS', 'THAN', 'POSITIVE', 'NEGATIVE',
    'NUMERIC', 'ALPHABETIC', 'AND', 'OR', 'NOT',

    # Figurative Constants
    'SPACE', 'SPACES', 'ZERO', 'ZEROS', 'ZEROES', 'HIGH-VALUE', 'HIGH-VALUES',
    'LOW-VALUE', 'LOW-VALUES', 'QUOTE', 'QUOTES', 'ALL',

    # Special Registers (1968)
    'TALLY', 'LINE-COUNTER', 'PAGE-COUNTER',

    # Report Writer
    'CONTROL', 'CONTROLS', 'PAGE', 'LIMIT', 'LIMITS', 'HEADING', 'FIRST',
    'LAST', 'DETAIL', 'DE', 'FOOTING', 'LINE', 'COLUMN', 'SOURCE', 'SUM',
    'UPON', 'RESET', 'GROUP', 'INDICATE', 'NEXT', 'TYPE', 'REPORTING',
    'CF', 'CH', 'PF', 'PH', 'RF', 'RH', 'FINAL', 'GENERATE', 'INITIATE',

    # Sort Feature
    'SORT', 'ASCENDING', 'DESCENDING', 'INPUT', 'OUTPUT', 'PROCEDURE', 'USING',

    # IBM-360 Specific (from C28-6516-8)
    'IBM-360', 'COPY', 'SYSIN', 'SYSOUT', 'SYSPUNCH', 'CONSOLE', 'DISPLAY-ST',
    'TRACE', 'READY', 'PRINT-SWITCH', 'FORM-OVERFLOW',

    # Miscellaneous
    'RUN', 'PROGRAM', 'SEGMENT', 'END', 'FILLER', 'COBOL', 'ID', 'SENTENCE',
    'CHANGED', 'NAMED', 'PLUS', 'PROCEED', 'PROCESS', 'RESTRICTED',
    'TRY', 'UTILITY', 'INCLUDE', 'FOR', 'ARE', 'COMMA'
}

# Documentation for reserved words (from C28-6516-8, 1968 Language Reference)
RESERVED_WORD_DOCS = {
    'IDENTIFICATION': 'The first of four divisions. Contains program identification.',
    'ENVIRONMENT': 'The second division. Specifies computer equipment and file assignments.',
    'DATA': 'The third division. Describes data structures and working storage.',
    'PROCEDURE': 'The fourth division. Contains executable statements.',
    'DIVISION': 'Marks the beginning of a major program section.',
    'SECTION': 'A subdivision of a division containing related paragraphs.',

    'PROGRAM-ID': 'Specifies the program name (up to 8 characters for OS/360).',
    'AUTHOR': 'Optional documentation of program author.',
    'DATE-WRITTEN': 'Optional documentation of when program was written.',
    'DATE-COMPILED': 'Automatically filled in by compiler at compile time.',

    'SELECT': 'Associates a file-name with an external data set.',
    'ASSIGN': 'Specifies the device and external name for a file.',
    'ORGANIZATION': 'Specifies file organization: SEQUENTIAL, INDEXED, RELATIVE, or DIRECT.',
    'ACCESS': 'Specifies access mode: SEQUENTIAL or RANDOM.',

    'FD': 'File Description. Describes characteristics of a data file.',
    'SD': 'Sort Description. Describes a sort work file.',
    'RD': 'Report Description. Describes a report file.',

    'PICTURE': 'Describes the format of an elementary data item. Abbreviated as PIC.',
    'PIC': 'Abbreviation for PICTURE.',
    'USAGE': 'Specifies internal representation: DISPLAY, COMPUTATIONAL, etc.',
    'COMPUTATIONAL': 'Binary representation. Abbreviated as COMP.',
    'COMPUTATIONAL-1': 'Short floating-point (4 bytes on System/360).',
    'COMPUTATIONAL-2': 'Long floating-point (8 bytes on System/360).',
    'COMPUTATIONAL-3': 'Packed decimal representation. Abbreviated as COMP-3.',

    'WORKING-STORAGE': 'Section containing internal data items not part of files.',
    'LINKAGE': 'Section containing data passed from calling program.',

    'MOVE': 'Transfers data from one item to another.',
    'COMPUTE': 'Performs arithmetic using expression notation.',
    'ADD': 'Adds numeric values.',
    'SUBTRACT': 'Subtracts numeric values.',
    'MULTIPLY': 'Multiplies numeric values.',
    'DIVIDE': 'Divides numeric values.',

    'IF': 'Conditional execution based on test condition.',
    'PERFORM': 'Executes a paragraph or section, optionally with iteration.',
    'GO': 'Unconditional branch to a procedure name. Usually written GO TO.',
    'CALL': 'Invokes a separately compiled program.',
    'STOP': 'Terminates program execution. Usually STOP RUN.',

    'OPEN': 'Opens a file for processing.',
    'CLOSE': 'Closes a file after processing.',
    'READ': 'Retrieves a record from a file.',
    'WRITE': 'Outputs a record to a file.',

    'COPY': 'Includes text from a library member at compile time.',
    'TALLY': 'Special register used with EXAMINE statement.',
    'EXAMINE': 'Counts or replaces characters in a data item (use TALLYING or REPLACING).',
    'TRANSFORM': 'Converts characters according to translation table.',

    'IBM-360': 'Reserved word identifying IBM System/360 computer.',
    'DISPLAY-ST': 'IBM extension for displaying status information.',
    'TRACE': 'IBM extension for procedure tracing.',
    'EXHIBIT': 'IBM extension for displaying data item values during execution.'
}


@dataclass
class DataItem:
    """Represents a COBOL data item."""
    name: str
    level: int
    line: int
    column: int
    picture: Optional[str] = None
    usage: Optional[str] = None
    occurs: Optional[int] = None
    value: Optional[str] = None
    redefines: Optional[str] = None
    parent: Optional[str] = None
    children: List[str] = field(default_factory=list)


@dataclass
class Paragraph:
    """Represents a COBOL paragraph or section."""
    name: str
    line: int
    column: int
    section: Optional[str] = None
    is_section: bool = False


@dataclass
class FileDescriptor:
    """Represents a COBOL file description."""
    name: str
    line: int
    column: int
    organization: Optional[str] = None
    access_mode: Optional[str] = None
    record_name: Optional[str] = None


class COBOL360Parser:
    """Parser for IBM System/360 COBOL F programs."""

    def __init__(self):
        self.data_items: Dict[str, DataItem] = {}
        self.paragraphs: Dict[str, Paragraph] = {}
        self.files: Dict[str, FileDescriptor] = {}
        self.current_section: Optional[str] = None
        self.current_division: Optional[str] = None

        # Patterns for parsing
        self.division_pattern = re.compile(
            r'^\s*(IDENTIFICATION|ENVIRONMENT|DATA|PROCEDURE)\s+DIVISION',
            re.IGNORECASE
        )
        self.section_pattern = re.compile(
            r'^\s*([A-Z0-9][-A-Z0-9]*)\s+SECTION\s*\.?',
            re.IGNORECASE
        )
        self.paragraph_pattern = re.compile(
            r'^.{6}\s{1,4}([A-Z][A-Z0-9-]*)\s*\.',
            re.IGNORECASE
        )
        self.fd_pattern = re.compile(
            r'^\s*FD\s+([A-Z][A-Z0-9-]*)',
            re.IGNORECASE
        )
        self.data_item_pattern = re.compile(
            r'^\s*(0[1-9]|[1-4][0-9]|66|77|88)\s+([A-Z][A-Z0-9-]*)',
            re.IGNORECASE
        )
        self.picture_pattern = re.compile(
            r'\bPIC(?:TURE)?\s+(?:IS\s+)?([9XAVSPZBCRDBabcrdb$,.+\-/*()0-9]+)',
            re.IGNORECASE
        )
        self.usage_pattern = re.compile(
            r'\bUSAGE\s+(?:IS\s+)?(DISPLAY|DISPLAY-ST|COMP(?:UTATIONAL)?(?:-[123])?|INDEX)',
            re.IGNORECASE
        )
        self.value_pattern = re.compile(
            r'\bVALUE\s+(?:IS\s+)?(["\'][^"\']*["\']|\d+|ZERO|ZEROS|ZEROES|SPACE|SPACES|HIGH-VALUE|LOW-VALUE)',
            re.IGNORECASE
        )

    def parse(self, text: str) -> None:
        """Parse a COBOL program."""
        self.data_items.clear()
        self.paragraphs.clear()
        self.files.clear()
        self.current_section = None
        self.current_division = None

        lines = text.split('\n')
        current_level_stack: List[Tuple[int, str]] = []

        for line_num, line in enumerate(lines):
            # Skip comment lines (column 7 = '*')
            if len(line) > 6 and line[6] == '*':
                continue

            # Skip sequence numbers, work with columns 7-72
            source_line = line[6:72] if len(line) > 6 else ''

            # Check for division
            div_match = self.division_pattern.search(source_line)
            if div_match:
                self.current_division = div_match.group(1).upper()
                continue

            # Check for section (within PROCEDURE DIVISION)
            if self.current_division == 'PROCEDURE':
                sec_match = self.section_pattern.search(source_line)
                if sec_match:
                    name = sec_match.group(1).upper()
                    self.current_section = name
                    self.paragraphs[name] = Paragraph(
                        name=name,
                        line=line_num,
                        column=source_line.find(name),
                        is_section=True
                    )
                    continue

                # Check for paragraph (starts in margin A, columns 8-11)
                para_match = self.paragraph_pattern.match(line)
                if para_match:
                    name = para_match.group(1).upper()
                    if name not in RESERVED_WORDS:
                        self.paragraphs[name] = Paragraph(
                            name=name,
                            line=line_num,
                            column=para_match.start(1),
                            section=self.current_section
                        )

            # Check for FD entry
            fd_match = self.fd_pattern.search(source_line)
            if fd_match:
                name = fd_match.group(1).upper()
                self.files[name] = FileDescriptor(
                    name=name,
                    line=line_num,
                    column=fd_match.start(1)
                )
                continue

            # Check for data item
            if self.current_division == 'DATA':
                data_match = self.data_item_pattern.search(source_line)
                if data_match:
                    level = int(data_match.group(1))
                    name = data_match.group(2).upper()

                    if name != 'FILLER':
                        # Get PICTURE if present
                        pic_match = self.picture_pattern.search(source_line)
                        picture = pic_match.group(1) if pic_match else None

                        # Get USAGE if present
                        usage_match = self.usage_pattern.search(source_line)
                        usage = usage_match.group(1) if usage_match else None

                        # Get VALUE if present
                        value_match = self.value_pattern.search(source_line)
                        value = value_match.group(1) if value_match else None

                        # Determine parent
                        parent = None
                        while current_level_stack and current_level_stack[-1][0] >= level:
                            current_level_stack.pop()
                        if current_level_stack:
                            parent = current_level_stack[-1][1]
                            if parent in self.data_items:
                                self.data_items[parent].children.append(name)

                        self.data_items[name] = DataItem(
                            name=name,
                            level=level,
                            line=line_num,
                            column=data_match.start(2),
                            picture=picture,
                            usage=usage,
                            value=value,
                            parent=parent
                        )

                        current_level_stack.append((level, name))

    def get_completions(self, prefix: str = '') -> List[Dict[str, Any]]:
        """Get completion items."""
        completions = []
        prefix_upper = prefix.upper()

        # Reserved words
        for word in RESERVED_WORDS:
            if word.startswith(prefix_upper):
                doc = RESERVED_WORD_DOCS.get(word, f'COBOL F reserved word')
                completions.append({
                    'label': word,
                    'kind': 14,  # Keyword
                    'detail': 'Reserved Word',
                    'documentation': doc
                })

        # Data items
        for name, item in self.data_items.items():
            if name.startswith(prefix_upper):
                detail = f'Level {item.level:02d}'
                if item.picture:
                    detail += f' PIC {item.picture}'
                completions.append({
                    'label': name,
                    'kind': 6,  # Variable
                    'detail': detail
                })

        # Paragraphs
        for name, para in self.paragraphs.items():
            if name.startswith(prefix_upper):
                kind = 'Section' if para.is_section else 'Paragraph'
                completions.append({
                    'label': name,
                    'kind': 3,  # Function
                    'detail': kind
                })

        return completions

    def get_hover(self, word: str) -> Optional[str]:
        """Get hover information for a word."""
        word_upper = word.upper()

        # Check reserved words
        if word_upper in RESERVED_WORD_DOCS:
            return f"**{word_upper}**\n\n{RESERVED_WORD_DOCS[word_upper]}"

        # Check data items
        if word_upper in self.data_items:
            item = self.data_items[word_upper]
            lines = [f"**{item.name}**", f"Level: {item.level:02d}"]
            if item.picture:
                lines.append(f"Picture: {item.picture}")
            if item.usage:
                lines.append(f"Usage: {item.usage}")
            if item.value:
                lines.append(f"Value: {item.value}")
            if item.parent:
                lines.append(f"Parent: {item.parent}")
            return '\n\n'.join(lines)

        # Check paragraphs
        if word_upper in self.paragraphs:
            para = self.paragraphs[word_upper]
            kind = 'Section' if para.is_section else 'Paragraph'
            return f"**{para.name}**\n\n{kind} at line {para.line + 1}"

        return None

    def get_definition(self, word: str) -> Optional[Tuple[int, int]]:
        """Get definition location (line, column)."""
        word_upper = word.upper()

        if word_upper in self.data_items:
            item = self.data_items[word_upper]
            return (item.line, item.column)

        if word_upper in self.paragraphs:
            para = self.paragraphs[word_upper]
            return (para.line, para.column)

        return None

    def get_references(self, word: str, text: str) -> List[Tuple[int, int]]:
        """Find all references to a word."""
        word_upper = word.upper()
        references = []

        pattern = re.compile(rf'\b{re.escape(word_upper)}\b', re.IGNORECASE)

        for line_num, line in enumerate(text.split('\n')):
            # Skip comments
            if len(line) > 6 and line[6] == '*':
                continue

            for match in pattern.finditer(line):
                references.append((line_num, match.start()))

        return references

    def get_document_symbols(self) -> List[Dict[str, Any]]:
        """Get document symbols for outline view."""
        symbols = []

        # Divisions would be at the top level
        # For now, list paragraphs and data items

        for name, para in self.paragraphs.items():
            kind = 12 if para.is_section else 13  # Class for section, Method for paragraph
            symbols.append({
                'name': name,
                'kind': kind,
                'range': {
                    'start': {'line': para.line, 'character': para.column},
                    'end': {'line': para.line, 'character': para.column + len(name)}
                },
                'selectionRange': {
                    'start': {'line': para.line, 'character': para.column},
                    'end': {'line': para.line, 'character': para.column + len(name)}
                }
            })

        for name, item in self.data_items.items():
            symbols.append({
                'name': name,
                'kind': 8,  # Field
                'detail': f'Level {item.level:02d}',
                'range': {
                    'start': {'line': item.line, 'character': item.column},
                    'end': {'line': item.line, 'character': item.column + len(name)}
                },
                'selectionRange': {
                    'start': {'line': item.line, 'character': item.column},
                    'end': {'line': item.line, 'character': item.column + len(name)}
                }
            })

        return symbols


class COBOL360LanguageServer:
    """Language Server Protocol implementation for COBOL F."""

    def __init__(self):
        self.parser = COBOL360Parser()
        self.documents: Dict[str, str] = {}
        self.logger = logging.getLogger('cobol360-lsp')

    def handle_message(self, message: Dict[str, Any]) -> Optional[Dict[str, Any]]:
        """Handle an incoming JSON-RPC message."""
        method = message.get('method', '')
        params = message.get('params', {})
        msg_id = message.get('id')

        result = None

        if method == 'initialize':
            result = self.handle_initialize(params)
        elif method == 'initialized':
            pass  # Notification, no response
        elif method == 'shutdown':
            result = None
        elif method == 'exit':
            sys.exit(0)
        elif method == 'textDocument/didOpen':
            self.handle_did_open(params)
        elif method == 'textDocument/didChange':
            self.handle_did_change(params)
        elif method == 'textDocument/didClose':
            self.handle_did_close(params)
        elif method == 'textDocument/completion':
            result = self.handle_completion(params)
        elif method == 'textDocument/hover':
            result = self.handle_hover(params)
        elif method == 'textDocument/definition':
            result = self.handle_definition(params)
        elif method == 'textDocument/references':
            result = self.handle_references(params)
        elif method == 'textDocument/documentSymbol':
            result = self.handle_document_symbol(params)

        if msg_id is not None:
            return {
                'jsonrpc': '2.0',
                'id': msg_id,
                'result': result
            }
        return None

    def handle_initialize(self, params: Dict[str, Any]) -> Dict[str, Any]:
        """Handle initialize request."""
        return {
            'capabilities': {
                'textDocumentSync': {
                    'openClose': True,
                    'change': 1  # Full sync
                },
                'completionProvider': {
                    'triggerCharacters': ['.', '-']
                },
                'hoverProvider': True,
                'definitionProvider': True,
                'referencesProvider': True,
                'documentSymbolProvider': True
            },
            'serverInfo': {
                'name': 'IBM System/360 COBOL F Language Server',
                'version': '1.0.0'
            }
        }

    def handle_did_open(self, params: Dict[str, Any]) -> None:
        """Handle textDocument/didOpen notification."""
        uri = params['textDocument']['uri']
        text = params['textDocument']['text']
        self.documents[uri] = text
        self.parser.parse(text)

    def handle_did_change(self, params: Dict[str, Any]) -> None:
        """Handle textDocument/didChange notification."""
        uri = params['textDocument']['uri']
        changes = params.get('contentChanges', [])
        if changes:
            text = changes[0].get('text', '')
            self.documents[uri] = text
            self.parser.parse(text)

    def handle_did_close(self, params: Dict[str, Any]) -> None:
        """Handle textDocument/didClose notification."""
        uri = params['textDocument']['uri']
        self.documents.pop(uri, None)

    def handle_completion(self, params: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Handle completion request."""
        uri = params['textDocument']['uri']
        position = params['position']

        text = self.documents.get(uri, '')
        lines = text.split('\n')

        if position['line'] < len(lines):
            line = lines[position['line']]
            col = position['character']

            # Find the word being typed
            start = col
            while start > 0 and (line[start-1].isalnum() or line[start-1] == '-'):
                start -= 1

            prefix = line[start:col]
            return self.parser.get_completions(prefix)

        return []

    def handle_hover(self, params: Dict[str, Any]) -> Optional[Dict[str, Any]]:
        """Handle hover request."""
        uri = params['textDocument']['uri']
        position = params['position']

        text = self.documents.get(uri, '')
        word = self._get_word_at_position(text, position)

        if word:
            content = self.parser.get_hover(word)
            if content:
                return {
                    'contents': {
                        'kind': 'markdown',
                        'value': content
                    }
                }

        return None

    def handle_definition(self, params: Dict[str, Any]) -> Optional[Dict[str, Any]]:
        """Handle goto definition request."""
        uri = params['textDocument']['uri']
        position = params['position']

        text = self.documents.get(uri, '')
        word = self._get_word_at_position(text, position)

        if word:
            location = self.parser.get_definition(word)
            if location:
                return {
                    'uri': uri,
                    'range': {
                        'start': {'line': location[0], 'character': location[1]},
                        'end': {'line': location[0], 'character': location[1] + len(word)}
                    }
                }

        return None

    def handle_references(self, params: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Handle find references request."""
        uri = params['textDocument']['uri']
        position = params['position']

        text = self.documents.get(uri, '')
        word = self._get_word_at_position(text, position)

        references = []
        if word:
            for line, col in self.parser.get_references(word, text):
                references.append({
                    'uri': uri,
                    'range': {
                        'start': {'line': line, 'character': col},
                        'end': {'line': line, 'character': col + len(word)}
                    }
                })

        return references

    def handle_document_symbol(self, params: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Handle document symbol request."""
        return self.parser.get_document_symbols()

    def _get_word_at_position(self, text: str, position: Dict[str, int]) -> Optional[str]:
        """Extract the word at the given position."""
        lines = text.split('\n')
        line_num = position['line']
        col = position['character']

        if line_num >= len(lines):
            return None

        line = lines[line_num]
        if col >= len(line):
            return None

        # Find word boundaries
        start = col
        while start > 0 and (line[start-1].isalnum() or line[start-1] == '-'):
            start -= 1

        end = col
        while end < len(line) and (line[end].isalnum() or line[end] == '-'):
            end += 1

        if start == end:
            return None

        return line[start:end]

    def run(self):
        """Run the language server."""
        self.logger.info('IBM System/360 COBOL F Language Server starting...')

        while True:
            try:
                # Read header
                header = {}
                while True:
                    line = sys.stdin.readline()
                    if not line or line == '\r\n' or line == '\n':
                        break
                    if ':' in line:
                        key, value = line.split(':', 1)
                        header[key.strip()] = value.strip()

                if not header:
                    continue

                # Read content
                content_length = int(header.get('Content-Length', 0))
                if content_length == 0:
                    continue

                content = sys.stdin.read(content_length)
                message = json.loads(content)

                # Handle message
                response = self.handle_message(message)

                if response:
                    response_text = json.dumps(response)
                    sys.stdout.write(f'Content-Length: {len(response_text)}\r\n\r\n')
                    sys.stdout.write(response_text)
                    sys.stdout.flush()

            except Exception as e:
                self.logger.error(f'Error: {e}')


def main():
    """Main entry point."""
    logging.basicConfig(
        level=logging.DEBUG if '--debug' in sys.argv else logging.INFO,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
        filename='cobol360_lsp.log'
    )

    server = COBOL360LanguageServer()
    server.run()


if __name__ == '__main__':
    main()
