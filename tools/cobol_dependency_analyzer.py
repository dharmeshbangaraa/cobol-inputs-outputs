#!/usr/bin/env python3
import os
import re
import json
from collections import defaultdict

REPO_ROOT = '.'
COBOL_EXTENSIONS = ['.cob', '.cbl', '.CBL', '.COB']
COPYBOOK_EXTENSIONS = ['.cpy', '.CPY']

# Patterns for COBOL program and call analysis
PROGRAM_ID_PATTERN = re.compile(r'PROGRAM-ID\.\s+([\w-]+)', re.IGNORECASE)
CALL_PATTERN = re.compile(r'CALL\s+[\'"]([\w-]+)[\'"]', re.IGNORECASE)
COPY_PATTERN = re.compile(r'COPY\s+([\w-]+)', re.IGNORECASE)
SECTION_PATTERN = re.compile(r'^(\s*[\w-]+)\s+SECTION\.', re.IGNORECASE | re.MULTILINE)
PARAGRAPH_PATTERN = re.compile(r'^(\s*[\w-]+)\s*\.', re.MULTILINE)

# Business function keywords
BUSINESS_KEYWORDS = [
    'BATCH', 'PROCESS', 'INTEREST', 'FEE', 'STATEMENT', 'GENERATE', 'ACCOUNT', 'UPDATE', 'REPORT',
    'CUSTOMER', 'BALANCE', 'TRANSACTION', 'HISTORY', 'CONTROL', 'TOTAL'
]

DATA_ELEMENTS = [
    'CUSTOMER', 'BALANCE', 'TRANSACTION', 'STATEMENT', 'CONTROL', 'TOTAL', 'ACCOUNT', 'HISTORY'
]

def find_files(root, extensions):
    matches = []
    for dirpath, _, filenames in os.walk(root):
        for filename in filenames:
            if any(filename.endswith(ext) for ext in extensions):
                matches.append(os.path.join(dirpath, filename))
    return matches

def extract_program_id(code):
    m = PROGRAM_ID_PATTERN.search(code)
    return m.group(1) if m else None

def extract_calls(code):
    return CALL_PATTERN.findall(code)

def extract_copies(code):
    return COPY_PATTERN.findall(code)

def extract_sections(code):
    return SECTION_PATTERN.findall(code)

def extract_paragraphs(code):
    # Exclude SECTIONs
    paragraphs = set(PARAGRAPH_PATTERN.findall(code))
    sections = set(extract_sections(code))
    return list(paragraphs - sections)

def find_business_references(code):
    found = set()
    for kw in BUSINESS_KEYWORDS:
        if re.search(r'\b' + re.escape(kw) + r'\b', code, re.IGNORECASE):
            found.add(kw)
    return list(found)

def find_data_references(code):
    found = set()
    for kw in DATA_ELEMENTS:
        if re.search(r'\b' + re.escape(kw) + r'\b', code, re.IGNORECASE):
            found.add(kw)
    return list(found)

def main():
    cobol_files = find_files(REPO_ROOT, COBOL_EXTENSIONS)
    copybooks = find_files(REPO_ROOT, COPYBOOK_EXTENSIONS)
    program_info = {}
    copybook_usage = defaultdict(list)
    call_graph = defaultdict(list)

    # Map program names to file paths
    program_name_to_file = {}

    for cobol_file in cobol_files:
        with open(cobol_file, 'r', encoding='utf-8', errors='ignore') as f:
            code = f.read()
        program_id = extract_program_id(code)
        if not program_id:
            continue
        program_name_to_file[program_id] = cobol_file
        calls = extract_calls(code)
        copies = extract_copies(code)
        sections = extract_sections(code)
        paragraphs = extract_paragraphs(code)
        business_refs = find_business_references(code)
        data_refs = find_data_references(code)
        program_info[program_id] = {
            'file': cobol_file,
            'calls': calls,
            'copies': copies,
            'sections': sections,
            'paragraphs': paragraphs,
            'business_references': business_refs,
            'data_references': data_refs
        }
        for copy in copies:
            copybook_usage[copy].append(program_id)
        for callee in calls:
            call_graph[program_id].append(callee)

    # Indirect call relationships (simple DFS)
    def get_all_callees(prog, visited=None):
        if visited is None:
            visited = set()
        for callee in call_graph.get(prog, []):
            if callee not in visited:
                visited.add(callee)
                get_all_callees(callee, visited)
        return visited

    indirect_calls = {prog: list(get_all_callees(prog)) for prog in program_info}

    # Build dependency map
    dependency_map = {
        'programs': program_info,
        'copybooks': {cb: users for cb, users in copybook_usage.items()},
        'call_graph': dict(call_graph),
        'indirect_calls': indirect_calls
    }
    print(json.dumps(dependency_map, indent=2))

if __name__ == '__main__':
    main()