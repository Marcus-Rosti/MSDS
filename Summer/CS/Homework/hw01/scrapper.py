from bs4 import BeautifulSoup
import urllib.request

webpage = urllib.request.urlopen('https://news.ycombinator.com')
soup = BeautifulSoup(webpage)
for anchor in soup.find_all('span')
    print(anchor.get('points', '/'))
 
