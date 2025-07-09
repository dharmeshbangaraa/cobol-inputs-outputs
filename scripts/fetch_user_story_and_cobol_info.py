import requests

def fetch_file_content(url):
    response = requests.get(url)
    if response.status_code == 200:
        return response.text
    else:
        return f'Failed to fetch {url}: {response.status_code}'

# URLs to fetch
user_story_url = 'https://raw.githubusercontent.com/dharmeshbangaraa/cobol-inputs-outputs/master/user-story/user_story_COBOL_Legacy_benchmark_suite.txt'
cobol_repo_base = 'https://api.github.com/repos/sentientsergio/COBOL-Legacy-Benchmark-Suite/contents/'

# Fetch user story
user_story = fetch_file_content(user_story_url)
print('USER STORY CONTENT:')
print(user_story)

# Fetch COBOL repo file list
response = requests.get(cobol_repo_base)
if response.status_code == 200:
    files = response.json()
    cobol_programs = []
    copybooks = []
    dependency_maps = []
    for f in files:
        name = f['name'].lower()
        if name.endswith('.cob') or name.endswith('.cbl'):
            cobol_programs.append(f['name'])
        elif 'copy' in name or name.endswith('.cpy'):
            copybooks.append(f['name'])
        elif 'depend' in name or 'map' in name:
            dependency_maps.append(f['name'])
    print('\nCOBOL PROGRAMS:')
    print('\n'.join(cobol_programs))
    print('\nCOPYBOOKS:')
    print('\n'.join(copybooks))
    print('\nDEPENDENCY MAPS:')
    print('\n'.join(dependency_maps))
else:
    print('Failed to fetch COBOL repo file list:', response.status_code)