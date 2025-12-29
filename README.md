# IBM System/360 Languages for Visual Studio Code

Language Server Protocol implementation for **IBM System/360 Operating System COBOL F** (1968) and **PL/I F** (1965), the programming languages that have been quietly running civilisation since before humans walked on the Moon.

## The Machine That Changed Everything

On 7 April 1964, IBM announced the System/360. It was, without exaggeration, the most significant product launch in the history of computing.

Before the 360, every computer was an island. Programs written for one machine wouldn't run on another. Businesses bought a computer, wrote their software, and then faced an agonising choice when they needed more capacity: throw away everything and start again, or limp along with inadequate hardware.

IBM bet the company—literally, $5 billion in 1960s dollars—on a radical idea: a family of compatible computers. The smallest 360 could run the same programs as the largest. A business could start small and grow without rewriting a single line of code.

They were right. Within two years, IBM had orders for over 1,000 systems. Within a decade, the 360 architecture dominated business computing worldwide. Today, sixty years later, the direct descendants of the 360, the z/Series mainframes, still process the majority of the world's corporate data.

The 360 didn't just change computing. It changed what was possible. Banks could offer instant account enquiries. Airlines could confirm reservations in seconds. Governments could process millions of tax returns. Hospitals could track patient records across departments.

The software that made all of this possible was written in two languages: COBOL and PL/I.

## The Languages

### COBOL F (1968)

COBOL—Common Business Oriented Language—was designed in 1959 by a committee that included Grace Hopper, the woman who had previously invented the compiler. The design goal was simple: a programming language that read like English, so that business managers could understand what their programs did.

IBM's COBOL F was the full implementation for the System/360, described in publication C28-6516-8 (November 1968, Ninth Edition). It was called "F" for "Full," as opposed to COBOL E ("Essential"), a subset for smaller systems.

COBOL F features:
- **Column-sensitive format** inherited from punched cards (columns 1-6 for sequence numbers, column 7 for indicators, columns 8-72 for code)
- **Four divisions**: Identification, Environment, Data, Procedure
- **Self-documenting data descriptions** with PICTURE clauses
- **File organisations**: Sequential, Indexed, Direct, Relative
- **Report Writer** for automated report generation
- **Sort feature** for ordering records

Today, COBOL processes an estimated $3 trillion in commerce daily. More than 95% of ATM transactions involve COBOL code. The US Social Security Administration alone has over 60 million lines of COBOL in production.

### PL/I F (1965)

PL/I—Programming Language One—was IBM's attempt to create one language that could do everything. Scientific computing like FORTRAN. Business processing like COBOL. Systems programming like assembly. Real-time control. All in one language.

The first PL/I specification was published in July 1965 as Form C28-6571-1. It was phenomenally ambitious: block structure, dynamic storage allocation, exception handling, concurrent programming, compile-time macros, and more. Features that wouldn't appear in other languages for decades.

PL/I F features:
- **Free-format source** (no column restrictions!)
- **Rich data types**: FIXED, FLOAT, DECIMAL, BINARY, CHARACTER, BIT, PICTURE
- **Structures and arrays** as first-class values
- **Pointer arithmetic** before C existed
- **Exception handling** with ON-conditions
- **Concurrent programming** with TASK and EVENT
- **Compile-time facilities** for metaprogramming

PL/I never achieved COBOL's dominance in business, but it found a permanent home in systems programming. The MVS operating system—the ancestor of today's z/OS—was written largely in PL/S, a systems programming dialect of PL/I. If you've ever used an ATM, your transaction was processed by software written in a language descended from PL/I.

## Why These Languages Still Matter

### The New Zealand Banking Story

In 1966, the New Zealand government announced that the country would convert from pounds, shillings, and pence to decimal currency on 10 July 1967. Every bank account, every price tag, every financial system would need to change.

The banks responded by purchasing IBM System/360 computers. They wrote their core ledger systems in COBOL and PL/I. On D-Day (Decimal Day), the conversion proceeded smoothly. The new systems worked perfectly.

And then... they kept working.

The Bank of New Zealand's Core Ledger system, first deployed on hardware purchased in 1965, continued running. The IBIS modernisation project in the 1980s attempted to replace it; the project failed spectacularly, costing hundreds of millions of dollars. The old system kept running. Most banks have similar stories.

In the 1990s, Y2K remediation consultants examined the code. They made the necessary changes. The system kept running.

In the 2000s, new internet banking frontends were built. They connected to the old system. The old system kept running.

Today, in 2025, variations of these systems are still processing transactions. The original programmers have long since retired. The documentation exists partly in filing cabinets, partly in someone's garage, partly in institutional memory. The code works, and nobody wants to be the person who breaks something that handles actual money.

### The Y2K Revelation

The Year 2000 problem revealed something unexpected: the people who understood these systems were invaluable.

When organisations realised that their two-digit year fields (PIC 99 in COBOL) would roll over from 99 to 00, they faced a choice. They could rewrite everything—a task that had proven impossible or ruinously expensive every time it was attempted. Or they could find the people who understood the existing code.

COBOL programmers came out of retirement. PL/I specialists were tracked down. They made the necessary changes, tested them thoroughly, and went back to their gardens.

The lights stayed on. The banks kept running. The aeroplanes didn't fall from the sky.

And then, in the years that followed, something remarkable happened: nothing. The systems kept working. The Y2K patches became part of the codebase. The century changed and the code adapted.

Those systems are still running now. The Y2K fixes are still working. They'll keep working until Y10K, which is someone else's problem.

## Features

### COBOL F Support
- **Syntax highlighting** for all COBOL F constructs
- **Code completion** for reserved words, divisions, sections
- **Hover information** with documentation from the 1968 Language Reference
- **Go to definition** for paragraphs and data items
- **Find references** across the program
- **Document outline** showing program structure
- **Margin awareness** (columns 1-6 sequence, column 7 indicator, Margin A 8-11, Margin B 12-72)

### PL/I F Support
- **Syntax highlighting** for all PL/I F constructs
- **Code completion** for keywords, built-in functions, attributes
- **Hover information** with documentation from the 1965 Language Specification
- **Go to definition** for procedures and variables
- **Find references** across the program
- **Document outline** showing program structure

## Installation

1. Install the extension from the VS Code Marketplace
2. Ensure Python 3.8+ is installed and available in PATH
3. Open any supported file

## File Extensions

### COBOL F
| Extension | Description |
|-----------|-------------|
| `.cob`    | COBOL source file |
| `.cbl`    | COBOL source file (alternate) |
| `.cobol`  | COBOL source file |
| `.cpy`    | COBOL copybook |

### PL/I F
| Extension | Description |
|-----------|-------------|
| `.pli`    | PL/I source file |
| `.pl1`    | PL/I source file (alternate) |
| `.plinc`  | PL/I include file |
| `.inc`    | Include file |

## Language Examples

### COBOL F
```cobol
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. DECIMALIZE.
000300 AUTHOR. UNKNOWN PROGRAMMER.
000400 DATE-WRITTEN. 10TH JULY 1967.
000500 REMARKS. CONVERTS POUNDS TO DOLLARS.
000600*    NEW ZEALAND DECIMALIZATION SYSTEM.
000700
000800 ENVIRONMENT DIVISION.
000900 CONFIGURATION SECTION.
001000 SOURCE-COMPUTER. IBM-360.
001100 OBJECT-COMPUTER. IBM-360.
001200
001300 DATA DIVISION.
001400 WORKING-STORAGE SECTION.
001500 77  WS-POUNDS        PIC S9(7)V99 COMP-3.
001600 77  WS-DOLLARS       PIC S9(9)V99 COMP-3.
001700 77  WS-RATE          PIC 9V9(6) VALUE 2.000000.
001800*    CONVERSION: 1 NZ POUND = 2 NZ DOLLARS
001900
002000 PROCEDURE DIVISION.
002100 CONVERT-CURRENCY.
002200     COMPUTE WS-DOLLARS ROUNDED = WS-POUNDS * WS-RATE.
```

### PL/I F
```pli
/* PL/I F EXAMPLE - SYSTEM/360 */
/* JULY 1965 LANGUAGE SPECIFICATION */

DECIMALIZE: PROCEDURE OPTIONS(MAIN);

    /* DECLARATIONS */
    DCL POUNDS    FIXED DECIMAL(9,2);
    DCL DOLLARS   FIXED DECIMAL(11,2);
    DCL RATE      FIXED DECIMAL(7,6) INIT(2.000000);
    /* CONVERSION: 1 NZ POUND = 2 NZ DOLLARS */

    DCL CUSTOMER_FILE FILE RECORD INPUT;
    DCL CUSTOMER_REC CHAR(120);

    /* EXCEPTION HANDLING */
    ON ENDFILE(CUSTOMER_FILE)
        GO TO FINISH;

    ON CONVERSION
        BEGIN;
            PUT SKIP LIST('CONVERSION ERROR');
            GO TO NEXT_RECORD;
        END;

    /* MAIN PROCESSING */
    OPEN FILE(CUSTOMER_FILE);

    READ_LOOP:
        READ FILE(CUSTOMER_FILE) INTO(CUSTOMER_REC);
        DOLLARS = POUNDS * RATE;
        PUT SKIP LIST(CUSTOMER_REC, DOLLARS);
    NEXT_RECORD:
        GO TO READ_LOOP;

    FINISH:
        CLOSE FILE(CUSTOMER_FILE);

END DECIMALIZE;
```

## Documentation Sources

This extension was developed using official IBM documentation:

### COBOL F (1968)
| Publication | Description |
|-------------|-------------|
| **C28-6516-8** | IBM System/360 Operating System: COBOL Language (November 1968, Ninth Edition) |
| **GC28-6380-3** | IBM System/360 Operating System: COBOL (F) Programmer's Guide (1968, Fourth Edition) |

### PL/I F (1965)
| Publication | Description |
|-------------|-------------|
| **C28-6571-1** | IBM Operating System/360: PL/I Language Specifications (July 1965) |
| **C28-8201-1** | IBM System/360: PL/I Reference Manual (January 1969) |

All documentation preserved by bitsavers.org, without whom vast portions of computing history would have been lost to bit rot and corporate indifference.

## Configuration

| Setting | Description | Default |
|---------|-------------|---------|
| `system360.pythonPath` | Path to Python interpreter | `python` |
| `system360.serverPath` | Path to LSP server script | (bundled) |
| `system360.trace.server` | Trace level for debugging | `off` |

## Requirements

- Visual Studio Code 1.75.0 or later
- Python 3.8 or later
- Optional: an appreciation for software that has been running since before the Moon landing

## Known Limitations

- COBOL Report Writer parsing is simplified (it was complicated in 1968; it remains complicated now)
- PL/I compile-time facilities (% statements) are not processed (metaprogramming was a mistake and we all know it)
- Some vendor-specific extensions may not be recognised (every mainframe shop had its own ideas)
- The parser handles OS/360 dialects; later dialects (VS COBOL II, Enterprise COBOL, Enterprise PL/I) have additional features
- Cannot actually run your code. That's what the mainframe is for. It's fine. It's been fine since 1965.

## The Programmers

Behind every line of COBOL and PL/I that still runs today, there was a programmer.

They worked with punched cards and paper tape. They debugged by reading hexadecimal dumps. They documented their code in three-ring binders that now live in archives, attics, and landfills.

Many of them are gone now. Others have retired to gardens and grandchildren. A few still consult, called upon when decades-old systems develop unexpected behaviours and nobody else remembers how they work.

These programmers built the infrastructure of modern finance, healthcare, government, and commerce. They did it with tools that would seem primitive today, under constraints that would seem impossible. They got it right, so right that their code is still running.

This extension is, in a small way, for them. The unknown programmers who wrote the code that still runs the world.

## Licence

Copyright 2025 Zane Hambly

Licensed under the Apache Licence, Version 2.0. See [LICENSE](LICENSE) for details.

## Contributing

Contributions welcome. If you've maintained System/360 code and have insights about dialect variations, edge cases, or implementation quirks, your knowledge would be invaluable.

Pull requests should include:
- Working code
- An understanding that GOTO is not inherently evil, merely often misused
- Respect for the engineers who built these systems under constraints we can barely imagine

## Related Projects

If you've found yourself oddly moved by the thought of code that has been running since 1965, you might appreciate:

- **[JOVIAL J73 LSP](https://github.com/Zaneham/jovial-lsp)** - For US Air Force systems. Jules' Own Version of the International Algebraic Language has been keeping fighter jets in the air since 1959. Same era, different mission. JOVIAL keeps F-15s flying; COBOL keeps banks solvent. Both are essential. Neither is glamorous. Both are still running.

- **[CMS-2 LSP](https://github.com/Zaneham/cms2-lsp)** - The US Navy's Compiler Monitor System 2. Terminates statements with dollar signs, which tells you everything you need to know about military procurement. Powers Aegis cruisers and submarines. Has been defending shipping lanes since 1968.

- **[CORAL 66 LSP](https://github.com/Zaneham/coral66-lsp)** - The British Ministry of Defence standard. Computer On-line Real-time Applications Language, developed at the Royal Radar Establishment, Malvern. Powers Tornado aircraft and Royal Navy vessels. Features Crown Copyright and presumably runs perfectly well in drizzle.

- **[HAL/S LSP](https://github.com/Zaneham/hals-lsp)** - NASA's High-order Assembly Language/Shuttle. Native vectors and matrices because astronauts shouldn't debug pointer arithmetic during re-entry. Powered 85% of the Space Shuttle's avionics. More glamorous than banking. Less financially stable.

- **[MUMPS LSP](https://github.com/Zaneham/mumps-lsp)** - Massachusetts General Hospital Utility Multi-Programming System. The other 1960s language still in production. COBOL handles your money; MUMPS handles your medical records. Together they know more about you than you do.

- **[Minuteman Guidance Computer Emulator](https://github.com/Zaneham/minuteman-emu)** - An emulator for the D17B/D37C computers in Minuteman ICBMs. A different kind of critical infrastructure. We preserve these systems so that future generations don't have to rediscover how they work in a crisis.

## Contact

Questions? Found an issue? Currently maintaining a System/360 codebase and need someone who understands that your production system predates the existence of most programming languages?

zanehambly@gmail.com

Response time faster than a batch job on a lightly loaded 360. Available for consultations and/or a cuppa tea. Will discuss COBOL over biscuits. Can explain why your PL/I code does that thing it does. Speaks fluent hexadecimal dump. Has opinions about GOTO that may differ from yours but will keep them to himself if you're buying.

Timezone: New Zealand (UTC+12/+13), which means I'm already living in your tomorrow. The future looks fine. Your legacy code is still running.

## Acknowledgements

- IBM Corporation (1964-present)
- CODASYL (Conference on Data Systems Languages)
- Grace Hopper, who invented the compiler and then helped invent COBOL
- The PL/I design team, who tried to create one language to rule them all
- The unknown programmers who wrote the code that still runs
- The maintainers who have kept it running through five decades
- bitsavers.org for preserving the documentation
- The Y2K veterans who proved that understanding old code is a superpower
- My grandad, who worked with these machines when they were new
