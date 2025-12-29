# IBM System/360 COBOL F Language Support for Visual Studio Code

Language Server Protocol (LSP) implementation for **IBM System/360 Operating System COBOL F** (1968), the programming language that has been managing your money since before the Moon landing.

## About OS/360 COBOL F

COBOL F was IBM's full implementation of COBOL for the System/360 mainframe, first released in 1966 and reaching Release 17 by 1968. While modern COBOL compilers have moved on to z/OS and fancy things like "object orientation," some institutions are still running the original. Not out of nostalgia. Out of necessity.

This particular dialect powers systems that were installed to handle currency conversions in the 1960s—New Zealand's decimalization (1967), Australia's (1966), and similar monetary upheavals across the Commonwealth. The banks bought IBM 360s, wrote their core ledgers in COBOL F, and then discovered that replacing something which handles actual money is considerably harder than anyone anticipated.

IBM's modern tooling targets z/OS. This LSP targets the systems that predate z/OS by several decades. If you're maintaining code that runs on iron from 1965, this is for you.

## Features

- **Syntax highlighting** for COBOL F constructs
- **Code completion** for reserved words, divisions, sections
- **Hover information** with documentation from the 1968 Language Reference
- **Go to definition** for paragraphs and data items
- **Find references** across the program
- **Document outline** showing program structure
- **Margin awareness** (columns 1-6 sequence, column 7 indicator, Margin A 8-11, Margin B 12-72)
- **Support for all COBOL F constructs:**
  - All four divisions (IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE)
  - Level numbers (01-49, 66, 77, 88)
  - File organizations (Sequential, Indexed, Direct, Relative)
  - Report Writer feature
  - Sort feature
  - COPY and BASIS statements

## Installation

1. Install the extension from the VS Code Marketplace
2. Ensure Python 3.8+ is installed and available in PATH
3. Open any `.cob`, `.cbl`, `.cobol`, or `.cpy` file
4. Your bank's core ledger is now syntax-highlighted

## File Extensions

| Extension | Description |
|-----------|-------------|
| `.cob`    | COBOL source file |
| `.cbl`    | COBOL source file (alternate) |
| `.cobol`  | COBOL source file (for those who type quickly) |
| `.cpy`    | COBOL copybook |

## Language Overview

COBOL F uses a column-sensitive format inherited from punched cards:

```cobol
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. BANKLEDGER.
000300 AUTHOR. SOMEONE WHO RETIRED IN 1987.
000400 DATE-WRITTEN. 12TH JULY 1967.
000500 REMARKS. HANDLES DECIMALIZATION.
000600*    DO NOT MODIFY WITHOUT APPROVAL.
000700
000800 ENVIRONMENT DIVISION.
000900 CONFIGURATION SECTION.
001000 SOURCE-COMPUTER. IBM-360.
001100 OBJECT-COMPUTER. IBM-360.
001200 SPECIAL-NAMES.
001300     CONSOLE IS OPERATOR-CONSOLE.
001400
001500 INPUT-OUTPUT SECTION.
001600 FILE-CONTROL.
001700     SELECT CUSTOMER-FILE
001800         ASSIGN TO SYS010-UR-2540R-S
001900         ORGANIZATION IS SEQUENTIAL
002000         ACCESS IS SEQUENTIAL.
002100
002200 DATA DIVISION.
002300 FILE SECTION.
002400 FD  CUSTOMER-FILE
002500     LABEL RECORDS ARE STANDARD
002600     BLOCK CONTAINS 10 RECORDS
002700     RECORD CONTAINS 80 CHARACTERS
002800     DATA RECORD IS CUSTOMER-RECORD.
002900 01  CUSTOMER-RECORD.
003000     05  CUST-NUMBER        PIC 9(8).
003100     05  CUST-NAME          PIC X(30).
003200     05  CUST-BALANCE       PIC S9(9)V99 COMP-3.
003300     05  FILLER             PIC X(35).
003400
003500 WORKING-STORAGE SECTION.
003600 77  WS-EOF-FLAG            PIC 9 VALUE 0.
003700     88  END-OF-FILE        VALUE 1.
003800 77  WS-TOTAL               PIC S9(11)V99 COMP-3.
003900
004000 PROCEDURE DIVISION.
004100 MAIN-PARA.
004200     OPEN INPUT CUSTOMER-FILE.
004300     PERFORM READ-CUSTOMERS UNTIL END-OF-FILE.
004400     CLOSE CUSTOMER-FILE.
004500     DISPLAY "TOTAL BALANCE: " WS-TOTAL UPON CONSOLE.
004600     STOP RUN.
004700
004800 READ-CUSTOMERS.
004900     READ CUSTOMER-FILE
005000         AT END MOVE 1 TO WS-EOF-FLAG
005100         NOT AT END
005200             ADD CUST-BALANCE TO WS-TOTAL.
```

### Column Layout

| Columns | Name | Purpose |
|---------|------|---------|
| 1-6 | Sequence Number | Card sequence (optional but recommended) |
| 7 | Indicator | `*` comment, `-` continuation, `/` page eject |
| 8-11 | Margin A | Division, section, paragraph names, level numbers |
| 12-72 | Margin B | Statements, clauses, data descriptions |
| 73-80 | Identification | Program identification (often ignored) |

### The Four Divisions

| Division | Purpose |
|----------|---------|
| IDENTIFICATION | Program metadata (who wrote it, when, and why) |
| ENVIRONMENT | Hardware configuration and file assignments |
| DATA | Data structures, record layouts, working storage |
| PROCEDURE | The actual business logic |

### Level Numbers

| Level | Usage |
|-------|-------|
| 01 | Record description |
| 02-49 | Group and elementary items |
| 66 | RENAMES clause |
| 77 | Independent elementary item |
| 88 | Condition name (boolean flag) |

### PICTURE Clause Symbols

| Symbol | Meaning |
|--------|---------|
| 9 | Numeric digit |
| X | Alphanumeric character |
| A | Alphabetic character |
| V | Implied decimal point |
| S | Sign (positive/negative) |
| P | Decimal scaling position |
| Z | Zero suppression |
| $ | Currency symbol |
| , | Comma insertion |
| . | Decimal point insertion |

### USAGE Clause

| USAGE | Description | IBM-360 Storage |
|-------|-------------|-----------------|
| DISPLAY | Character format | 1 byte per character |
| COMPUTATIONAL | Binary | 2 or 4 bytes |
| COMPUTATIONAL-1 | Short floating point | 4 bytes |
| COMPUTATIONAL-2 | Long floating point | 8 bytes |
| COMPUTATIONAL-3 | Packed decimal | Variable |

## Documentation Sources

This extension was developed using official IBM documentation from 1968:

1. **C28-6516-8: IBM System/360 Operating System COBOL Language** (November 1968)
   - The definitive language reference
   - Ninth Edition, describing COBOL E and COBOL F
   - CODASYL heritage with IBM extensions

2. **GC28-6380-3: IBM System/360 Operating System COBOL (F) Programmer's Guide** (1968)
   - Fourth Edition, Release 17
   - JCL integration, compilation, debugging
   - MFT and MVT environments

All documentation preserved from bitsavers.org, that remarkable archive of computing history that exists because some people understand that the future will need to understand the past.

## Configuration

| Setting | Description | Default |
|---------|-------------|---------|
| `cobol360.pythonPath` | Path to Python interpreter | `python` |
| `cobol360.serverPath` | Path to LSP server script | (bundled) |
| `cobol360.trace.server` | Trace level for debugging | `off` |
| `cobol360.marginA` | Column for Margin A | `8` |
| `cobol360.marginB` | Column for Margin B | `12` |

## Requirements

- Visual Studio Code 1.75.0 or later
- Python 3.8 or later
- Optional: an appreciation for code that has been running longer than most programmers have been alive

## Known Limitations

- Report Writer feature parsing is simplified
- Extended Source Program Library (INSERT/DELETE) statements are recognised but not processed
- Sterling currency support (pre-decimalization) is not implemented (thankfully)
- The parser handles OS/360 COBOL F; later dialects (VS COBOL II, Enterprise COBOL) have additional features

## Why Does This Exist?

Somewhere in a machine room in New Zealand, Australia, and Denmark (for some reason) there is an IBM System/360 that has been running since 1965. It handles the core ledger for a major bank. The original programmers have long since retired. The documentation is scattered across filing cabinets and someone's garage. The code works, and nobody wants to be the person who breaks something that handles actual money.

Modern COBOL tools target modern COBOL. VS COBOL II. Enterprise COBOL for z/OS. Languages that have evolved, that support object orientation and XML and other features that would have seemed like science fiction in 1968.

But some systems haven't evolved. They can't. They're too critical. Too embedded. Too risky to change.

This LSP provides tooling for those systems. Not because we're nostalgic. Because those systems are still running, still processing transactions, still doing the job they were built to do fifty-eight years ago.

## A Note on Y2K

Yes, COBOL was famously blamed for the Y2K problem. Yes, the PIC 99 date fields were a poor life choice. But consider: those systems survived Y2K. They were patched, tested, and fixed. The COBOL programmers who understood these systems came out of retirement, made the necessary changes, and went back to their gardens.

The systems are still running. The Y2K patches are still working. The code that was written for two-digit years now handles four-digit years, and will continue to do so until the Y10K problem becomes someone else's responsibility.

## Licence

Copyright 2025 Zane Hambly

Licensed under the Apache Licence, Version 2.0. See [LICENSE](LICENSE) for details.

## Contributing

Contributions welcome. If you've maintained OS/360 COBOL and have insights about dialect variations, edge cases, or implementation quirks, your knowledge would be invaluable.

Pull requests should include:
- Working code
- An understanding that GOTO is not inherently evil, merely often misused
- Respect for the engineers who built these systems under constraints we can barely imagine

## Related Projects

If you've found yourself oddly comfortable with fixed-format source code and column-sensitive parsing, you might appreciate:

- **[JOVIAL J73 LSP](https://github.com/Zaneham/jovial-lsp)** - For US Air Force systems. Same era, different continent, different wars. JOVIAL keeps F-15s flying; COBOL keeps banks solvent. Both are critical. Neither is glamorous.

- **[CMS-2 LSP](https://github.com/Zaneham/cms2-lsp)** - The US Navy's tactical language. Terminates statements with dollar signs, because apparently the Navy thought that was amusing. Powers Aegis cruisers. Your bank account is safer than an aircraft carrier.

- **[CORAL 66 LSP](https://github.com/Zaneham/coral66-lsp)** - The British equivalent. Developed at Malvern, published by Her Majesty's Stationery Office, features Crown Copyright. For Tornado aircraft and Royal Navy vessels. Less paperwork than COBOL, somehow.

- **[HAL/S LSP](https://github.com/Zaneham/hals-lsp)** - NASA's Space Shuttle language. Native vectors and matrices because astronauts shouldn't debug pointer arithmetic during re-entry. More glamorous than banking. Less financially stable.

- **[MUMPS LSP](https://github.com/Zaneham/mumps-lsp)** - The other healthcare language. Also from the 1960s. Also still in production. Also handling things that really shouldn't break. COBOL handles your money; MUMPS handles your medical records. Sleep well.

- **[Minuteman Guidance Computer Emulator](https://github.com/Zaneham/minuteman-emu)** - An emulator for ICBM guidance computers. A different kind of critical infrastructure. Equally resistant to replacement. We don't talk about the overlap between these codebases.

## Contact

Questions? Found an issue? Currently maintaining a COBOL system and questioning your life choices?

zanehambly@gmail.com - Response time faster than a batch job on a lightly loaded 360.

## Acknowledgements

- IBM Corporation (1968)
- CODASYL (Conference on Data Systems Languages)
- The unknown programmers who wrote the systems we're still running
- The maintainers who've kept them running through five decades
- bitsavers.org for preserving the documentation
- My grandad, who worked with machines when they were new
