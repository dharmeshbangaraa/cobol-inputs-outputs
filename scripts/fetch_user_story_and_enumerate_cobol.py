import requests
from bs4 import BeautifulSoup
import os

def fetch_github_raw(url):
    if 'github.com' in url and '/blob/' in url:
        raw_url = url.replace('github.com', 'raw.githubusercontent.com').replace('/blob/', '/')
        resp = requests.get(raw_url)
        resp.raise_for_status()
        return resp.text
    raise ValueError('Not a valid GitHub blob URL')

def list_github_repo_files(repo_url, file_types=None):
    # file_types: list of extensions, e.g. ['.cob', '.cpy']
    api_url = repo_url.replace('github.com/', 'api.github.com/repos/')
    if api_url.endswith('/'):
        api_url = api_url[:-1]
    api_url += '/contents'
    resp = requests.get(api_url)
    resp.raise_for_status()
    files = []
    for entry in resp.json():
        if entry['type'] == 'file':
            if not file_types or any(entry['name'].lower().endswith(ft) for ft in file_types):
                files.append(entry['download_url'])
        elif entry['type'] == 'dir':
            # Recursively list files in subdirectories
            files.extend(list_github_repo_files(entry['html_url'], file_types))
    return files

def main():
    user_story_url = 'https://github.com/dharmeshbangaraa/cobol-inputs-outputs/blob/master/user-story/user_story_COBOL_Legacy_benchmark_suite.txt'
    cobol_repo_url = 'https://github.com/sentientsergio/COBOL-Legacy-Benchmark-Suite'

    # Fetch user story
    user_story = fetch_github_raw(user_story_url)
    print('USER STORY:')
    print(user_story)

    # List COBOL programs and copybooks
    cobol_files = list_github_repo_files(cobol_repo_url, ['.cob', '.cpy'])
    print('\nCOBOL PROGRAMS AND COPYBOOKS:')
    for f in cobol_files:
        print(f)

    # Try to find dependency maps or documentation
    doc_files = list_github_repo_files(cobol_repo_url, ['.md', '.txt'])
    for doc_url in doc_files:
        if 'depend' in doc_url.lower() or 'readme' in doc_url.lower() or 'doc' in doc_url.lower():
            print(f'\nDOCUMENTATION ({doc_url}):')
            print(fetch_github_raw(doc_url))

if __name__ == '__main__':
    main()
