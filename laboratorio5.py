#Laboratorio #5 Prediccion de Textos


Blogs = open("en_US.blogs.txt", "r")
News= open("en_US.news.txt", "r")
Twitter= open("en_US.twitter.txt", "r")

while True:
    # read line
    line = Blogs.readline()
    # in python 2, print line
    # in python 3
    print(line)
    # check if line is not empty
    if not line:
        break
Blogs.close()


while True:
    # read line
    line = News.readline()
    # in python 2, print line
    # in python 3
    print(line)
    # check if line is not empty
    if not line:
        break
News.close()


while True:
    # read line
    line = Twitter.readline()
    # in python 2, print line
    # in python 3
    print(line)
    # check if line is not empty
    if not line:
        break
Twitter.close()
