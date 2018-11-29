from functools import reduce
import random

def addDict(d):
    returnDict = dict()
    for key1,key2 in d.items():
        for key in key2:
            value = d[key1][key]
            if key in returnDict.keys():
                    value += returnDict[key] 
            returnDict[key] = value

    return returnDict

def dictAdd(x, y):
    returnDict = dict()
    for key in x.keys():
        xvalue = x[key]
        returnDict[key] = xvalue
    for key in y.keys():
        yvalue = y[key]
        if key in returnDict.keys():
            yvalue += returnDict[key] 

        returnDict[key] = yvalue
    return returnDict


 #write your code here
def testaddDict():
    print("-----addDict:")
    print(addDict({'355':{'Mon':3,'Wed':2,'Sat':2},'360':{'Mon':3,'Tue':2,'Wed':2,'Fri':10},'321':{'Tue':2,'Wed':2,'Thu':3},'322':{'Tue':1,'Thu':5,'Sat':2}}) == {'Mon': 6, 'Wed': 6, 'Sat': 4, 'Tue': 5, 'Fri': 10, 'Thu': 8})

def addDictN(L):
    tempL = list(map(addDict, L))
    return reduce(lambda x,y: dictAdd(x,y), tempL)

def testaddDictN():
    print("-----addDictN:")
    print(addDictN([{'355':{'Mon':3,'Wed':2,'Sat':2},'360':{'Mon':3,'Tue':2,'Wed':2,'Fri':10},'321':{'Tue':2,'Wed':2,'Thu':3},'322':{'Tue':1,'Thu':5,'Sat':2}},{'322':{'Mon':2},'360':{'Thu':2, 'Fri':5},'321':{'Mon':1, 'Sat':3}},{'355':{'Sun':8},'360':{'Fri':5},'321':{'Mon':4},'322':{'Sat':3}}]) == {'Mon': 13, 'Wed': 6, 'Sat': 10, 'Tue': 5, 'Fri': 20, 'Thu': 10, 'Sun': 8})

def lookupVal(L,k):
    for dic in L[::-1]:
        if k in dic.keys():
            return dic[k]

def searchDict(dic, k):
    for key in dic.keys():
        if key == k:
            return dic[key]

def lookupVal2(tL, k):
    cur = tL[-1]
    result = searchDict(cur[1], k)
    while result == None and cur != tL[cur[0]]:
        cur = tL[cur[0]]
        result = searchDict(cur[1], k)
    return result

def numPaths(m,n,blocks): 
    if (m,n) in blocks or m==0 or n==0:
        return 0
    elif (m,n) == (1,1):
        return 1
    else:
        return (numPaths(m-1, n, blocks) + numPaths(m, n-1, blocks))

def palindromes(S):
    result = []
    for i in range(len(S)):
        for j in range(i,len(S)):
            substring = S[i:j+1]
            revstring = substring[::-1]
            if substring == revstring and len(substring) > 1:
                result.append(substring)
    return sorted(result)


class myObj:
    function = 0
    num = 0
    def __init__(self, n, f):
        self.current = f(n)
        self.function = f
        self.num = n
    def __next__(self):
        result = self.current
        self.num += 1
        self.current = self.function(self.num)
        return result
    def __iter__(self):
        return self

def iterApply(n, f):
    return myObj(n,f)

def iMerge(iNumbers1, iNumbers2, N):
    num1 = iNumbers1.__next__()
    num2 = iNumbers2.__next__()
    result = []
    for i in range(N):
        if num1 <= num2:
            result.append(num1)
            num1 = iNumbers1.__next__()
        else:
            result.append(num2)
            num2 = iNumbers2.__next__()
    iNumbers1.num -= 1
    iNumbers2.num -= 2
    return result

class Stream(object):
    def __init__(self, first, compute_rest, empty= False):
        self.first = first
        self._compute_rest = compute_rest
        self.empty = empty
        self._computed = False
        self._rest = None

    @property
    def rest(self):
        assert not self.empty, 'Empty streams have no rest.'
        if not self._computed:
            self._rest = self._compute_rest()
            self._computed = True
        return self._rest

def streamRandoms(k, min , max):
    def compute_rest():
        return streamRandoms(random.randint(min,max), min, max)
    return Stream(k, compute_rest)

def oddStream(strm):
    def compute_rest():
        return oddStream(strm.rest)
    while strm.first % 2 == 0:
        strm = strm.rest
    return Stream(strm.first, compute_rest)

def testOddStreams():
    print("-----OddStreams:")
    oddS = oddStream(streamRandoms(1,1,100))
    myList = []
    for i in range(0,100):
        myList.append(oddS.first)
        oddS = oddS.rest
    print(myList)


def teststreamRandoms():
    print("-----streamRandoms:")
    rStream = streamRandoms(1,1,100)
    myList = []
    for i in range (0,10):
        myList.append(rStream.first)
        rStream = rStream.rest
    print(myList)

def testiMerge():
    print("-----iMerge:")
    squares = iterApply(1,lambda x: x**2)
    triples = iterApply(1,lambda x: x**3)
    print(iMerge(squares,triples,8) == [1, 1, 4, 8, 9, 16, 25, 27])
    print(iMerge(squares,triples,10) == [49, 49, 64, 81, 100, 121, 125, 64, 125, 144])
    print(iMerge(squares,triples,6) == [196, 196, 225, 256, 289, 324]) 

def testiterApply():
    print("-----iterApply:")
    squares = iterApply(1,lambda x: x**2)
    print(squares.__next__() == 1)
    print(squares.__next__() == 4)
    print(squares.__next__() == 9)

def testPalindromes():
    print("-----palindromes:")
    print(palindromes ('cabbbaccab') == ['abbba', 'acca', 'baccab', 'bb', 'bb', 'bbb', 'cabbbac', 'cc'])
    print(palindromes ('bacdcabdbacdc') == ['abdba', 'acdca', 'bacdcab', 'bdb', 'cabdbac', 'cdc', 'cdc', 'cdcabdbacdc', 'dcabdbacd']) 
    print(palindromes ('myracecars') == ['aceca', 'cec', 'racecar']) 

def testnumPaths():
    print("-----numPaths:")
    print(numPaths(2,2,[(2,1)]) == 1)
    print(numPaths(3,3,[(2,3)]) == 3)
    print(numPaths(4,3,[(2,2)]) == 4)
    print(numPaths(10,3,[(2,2),(7,1)]) == 27) 
 
def testlookupVal2():
    L2 = [(0,{"x":0,"y":True,"z":"zero"}),
    (0,{"x":1}),
    (1,{"y":False}),
    (1,{"x":3, "z":"three"}),
    (2,{})]
    print("-----lookupval2:")
    print(lookupVal2(L2,"x") == 1)
    print(lookupVal2(L2,"y") == False)
    print(lookupVal2(L2,"z") == 'zero')
    print(lookupVal2(L2,"t") == None)

def testlookupVal():
    print("-----lookupVal:")
    L1 = [{"x":1,"y":True,"z":"found"},{"x":2},{"y":False}]
    print(lookupVal(L1,"x") == 2)
    print(lookupVal(L1,"y") == False)
    print(lookupVal(L1,"z") == 'found')
    print(lookupVal(L1,"t") == None)

if __name__ == '__main__':
    testaddDict()
    testaddDictN()
    testlookupVal()
    testlookupVal2()
    testnumPaths()
    testPalindromes()
    testiterApply()
    testiMerge()
    teststreamRandoms()
    testOddStreams()
