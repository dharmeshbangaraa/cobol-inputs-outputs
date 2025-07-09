import requests
from bs4 import BeautifulSoup

# Fetch the user story from GitHub
url = 'https://github.com/dharmeshbangaraa/cobol-inputs-outputs/blob/master/user-story/user_story_COBOL_Legacy_benchmark_suite.txt'
response = requests.get(url)
soup = BeautifulSoup(response.text, 'html.parser')

# Extract the raw text from the GitHub file view
user_story = ''
for code_block in soup.find_all('td', class_='blob-code blob-code-inner js-file-line'):
    user_story += code_block.text + '\n'

print('USER STORY:')
print(user_story)

# (Further parsing logic would go here to extract business functions, data elements, and processes)
