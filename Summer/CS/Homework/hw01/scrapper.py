from bs4 import BeautifulSoup
import urllib.request

webpage = urllib.request.urlopen('https://news.ycombinator.com')
soup = BeautifulSoup(webpage)

class_titles =  soup.find_all("td", {"class": "title"})
class_subtext = soup.find_all("td", {"class": "subtext"})

title_counter = 1
subtext_counter = 0

titles = []
points = []
times = []
links = []
usernames = []
comment_count = []

def get_numbers(string):
    # returns the leading number in a string
    digits = 0
    test_val = 0
    while(string[test_val].isdigit()):
        digits = digits*10 + int(string[test_val])
        test_val+=1
    return digits

def get_points(line_num):
    # returns the point value for link
    try:
        point = class_subtext[line_num].find("span",{"class": "score"}).contents
        point = point[0]
    except Exception as e:
        return 0
    return get_numbers(point)

def get_comments(line_num):
    # returns the comments
    try:
        comment = class_subtext[line_num].find_all("a")
        comment = comment[2].contents
        comment = comment[0]
    except Exception as e:
        return 0
    return get_numbers(comment)

def get_username(line_num):
    # returns the username
    try:
        username = class_subtext[line_num].find_all("a")
        username = username[0].contents
    except Exception as e:
        return ""
    return username[0]

def get_time(line_num):
    # returns the time in minutes... approximately
    try:
        time = class_subtext[line_num].find_all("a")
        time = time[1].contents
        time = time[0]
    except Exception as e:
        return 0
    if "hour" in time:
        return get_numbers(time) * 60
    return get_numbers(time)

while (title_counter<61):
    # extract title
    titles.append((class_titles[title_counter].find("a").contents)[0])
    # extract points
    points.append(get_points(subtext_counter))
    # extract comment count
    comment_count.append(get_comments(subtext_counter))
    # extract username
    usernames.append(get_username(subtext_counter))
    # extract time
    times.append(get_time(subtext_counter))
    subtext_counter += 1
    title_counter += 2

print("title,points,comment_count,username,time")
for i in range(0,30):
    print(titles[i]+","+str(points[i])+","+str(comment_count[i])+ \
        ","+str(usernames[i])+","+str(times[i]))
