# save this file in same file as R script or have it open

def getCommonNames(list1, *args):
  return_list = []
  listOfLists = []
  for x in args:
    listOfLists.append(x)
  for eachEntry in list1:
    notFound = False
    for eachList in listOfLists:
      if not(eachEntry in eachList):
        notFound = True
    if not(notFound):
      return_list.append(eachEntry)
  return return_list

def filterData(names, keys, values):
  filteredNames = getCommonNames(names, keys)
  dataList = values[:]
  for i in range(len(values)):
    if not (keys[i] in filteredNames):
      dataList[i] = "delete" #don't shrink the list as you iterate!
  while ("delete" in dataList):
    dataList.remove("delete")
  return dataList

def getNewData(allNames, countryNames, countryData):
  new_list = []
  dictionary = {}
  for i in range(len(countryNames)):
    dictionary[countryNames[i]] = countryData[i]
  for name in allNames:
    if name in dictionary.keys():
      new_list.append(dictionary[name])
    else:
      new_list.append("N/A")
  return new_list
