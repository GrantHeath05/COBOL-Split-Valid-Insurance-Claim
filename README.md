# COBOL Split & Summary Report Program

## The program/assignment is being shared with permission from Durham College

## Overview
This COBOL batch program processes a raw input data file containing mixed insurance claim records.  
It **assumes all input data is valid** and performs **no field‑level validation**.  
The program’s main functions are:

1. **Split** records into two separate output files based on claim type:
   - REPAIR
   - REPLACE

2. **Generate a summary report** that breaks down:
   - Claim type totals
   - Province totals
   - Product code totals
   - Total earned amounts
   - Record counts and percentages

---

## Input Assumptions
- All records in the input file are correctly formatted.
- All fields contain valid values (claim type, province, product code, amounts, etc.).
- No validation or error‑handling logic is required.
- The program processes each record exactly as provided.

---

## Processing Flow

### 1. Read Input File
The program reads each record sequentially from the raw data file.

### 2. Split Records
Based on the claim type field:
- **REPAIR** records → written to the REPAIR output file  
- **REPLACE** records → written to the REPLACE output file  

Counters and totals are updated for each category.

### 3. Accumulate Statistics
For every record processed, the program updates:
- Province totals  
- Product code totals  
- Claim type totals  
- Total earned amounts  

Tables/arrays in WORKING‑STORAGE are used to track counts and totals.

### 4. Generate Summary Report
After all records are processed, the program produces a formatted summary report including:
- Total records processed  
- REPAIR vs REPLACE counts  
- Total earned amounts  
- Province breakdown  
- Product code breakdown  
- Percentages of total volume and dollars  

The report is formatted for readability using standard COBOL reporting techniques.

---

## Output Files

### REPAIR Output File
Contains only REPAIR claim records in the same layout as the input.

### REPLACE Output File
Contains only REPLACE claim records.

### Summary Report
A human‑readable report showing:
- Totals  
- Counts  
- Percentages  
- Province and product breakdowns  

---

## Purpose
This program demonstrates:
- Sequential file handling  
- Conditional routing of records  
- Use of tables for statistical accumulation  
- COBOL report generation  
- Clean batch‑style processing with deterministic output  

It is designed as a straightforward processing program assuming valid input data only.
