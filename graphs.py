from turtle import *
from math import *
def curve(func1,func2,precision=100,limits=(0,10)):
    interval=limits[1]-limits[0]
    temp1=[func1(limits[0]+i*float(interval)/precision) for i in range(precision)]
    temp2=[func2(limits[0]+i*float(interval)/precision) for i in range(precision)]
    horizontal=(min(temp1),max(temp1))
    vertical=(min(temp2),max(temp2))
    h=horizontal[1]-horizontal[0]
    v=vertical[1]-vertical[0]
    a,b,c,d=-630,630,-350,350
    print temp1
    print temp2
    resize=min(b-a,d-c)
    img=[((m[0]-horizontal[0])*(d-c)/float(h)+a, (m[1]-vertical[0])*(d-c)/float(v)+c) for m in zip(temp1, temp2)]
    up()
    goto(img[0])
    down()
    writing=precision/50
    for i in range(len(img)):
        if i%writing==0:
            dot(5)
            write(round(limits[0]+i*float(interval)/precision,2))#write([round(temp1[i],2),round(temp2[i],2)])
        goto(img[i])
curve(lambda x: sqrt(-1+sqrt(1+4*x))/sqrt(2), lambda x: (-1+sqrt(1+4*x))/2)
