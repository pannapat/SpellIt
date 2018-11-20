# content of test_sample.py
import subprocess as subprocess
import os
import requests


def func(x):
    return x + 1

def output():
	subprocess.call(["cd", "Testing/testScripts"])
	subprocess.call(["./hello.sh"])

def getLanguages():
  	URL = "http://104.248.116.233:5000/language-list"
  	PARAMS = { }
  	# language_raw = requests.post(url = URL, params = PARAMS)
  	# language_list_json= language_raw.json()
  	language_list_json={ "languages" : ["English, Test"]}
  	language_list = language_list_json["languages"]
  	return language_list
	
def getAffixes(language_list):
	URL = "http://104.248.116.233:5000/get-affix"
	number_of_languages = 0
	for index, lang in enumerate(language_list):
		PARAMS = {'language_name':lang}
		raw_affix = requests.post(url = URL, params = PARAMS)
		affix_data = raw_affix.json()
		
		affix_file = open("out"+index+".aff","w+")
		affix_file.write(affix_data["affix_file"])
		affix_file.close()
		
		dic_file = open("out"+index+".dic","w+")
		dic_file.write(affix_data["dic_file"])
		dic_file.close()

		number_of_languages+=1

	return number_of_languages


def call_hunspell(number_of_languages):
	
	passed = 0
	failed = 0
	fileNum = 1
	
	os.chdir("Testing/testScripts")

	while fileNum <= number_of_languages:
		inputFile = "words"+str(fileNum)+".txt"
		outputFile = "output"+str(fileNum)+".txt"
		hunspell_cmd = subprocess.Popen(["cat", inputFile], stdout=subprocess.PIPE)
		hunspell_output = subprocess.Popen(['hunspell', '-d',"./out","-l"], 
			stdin=hunspell_cmd.stdout, stdout=subprocess.PIPE)
		expected_output= subprocess.Popen(["cat", outputFile], stdout=subprocess.PIPE)
		
		if hunspell_output.stdout.read().strip() == expected_output.stdout.read().strip():
			passed=passed+1
		else:
			failed=failed+1

		fileNum=fileNum+1
	# print "tests passed: ",passed
	# print "tests failed: ",failed

	return(passed,failed)

def test_answer():
    assert func(3) == 4
    #number_of_languages = getAffixes(getLanguages())
    passed,failed = call_hunspell(4)

test_answer()