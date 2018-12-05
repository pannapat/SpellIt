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
  	language_raw = requests.post(url = URL)
  	language_list_json= language_raw.json()
  	language_list = language_list_json["languages"]
  	return language_list
	
def getAffixes(language_list):
	baseURL = "http://104.248.116.233:5000/"
	paradigmURL = baseURL + "paradigm-list"
	affixURL = baseURL + "get-affix"
	rootURL = baseURL + "root-word-list"
	wordURL = baseURL + "word-form-list"
	number_of_languages = 0
	for index, lang in enumerate(language_list):
		LANG_PARAMS = {"language_name": lang}
		print LANG_PARAMS

		raw_paradigms = requests.post(url = paradigmURL, json= LANG_PARAMS)
		paradigms_list_json = raw_paradigms.json()
		paradigms_list = paradigms_list_json["paradigms"]

		words_file = open("Testing/testScripts/words"+str(index)+".txt","w+")
		for paradigm in paradigms_list:
			ROOT_PARAMS = {"paradigm_name": paradigm}
			raw_roots = requests.post(url = rootURL, json = ROOT_PARAMS)
			roots_list_json = raw_roots.json()
			roots_list = roots_list_json["paradigm_roots"]
			for root in roots_list:
				WORD_PARAMS = {"paradigm_root": root}
				raw_words = requests.post(url = wordURL, json = WORD_PARAMS)
				word_list_json = raw_words.json()
				word_list = word_list_json["word_data"]
				for key, value in word_list.items():
					print key + ": " + value
					words_file.write(value)

		words_file.close()
 
		raw_affix = requests.post(url = affixURL, json = LANG_PARAMS)
		affix_data = raw_affix.json()
		
		affix_file = open("Testing/testScripts/out"+str(index)+".aff","w+")
		affix_file.write(affix_data["affix_file"])
		affix_file.close()
		
		dic_file = open("Testing/testScripts/out"+str(index)+".dic","w+")
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
		#outputFile = "output"+str(fileNum)+".txt"
		hunspell_cmd = subprocess.Popen(["cat", inputFile], stdout=subprocess.PIPE)
		affix_name = "out" + str(fileNum)
		hunspell_output = subprocess.Popen(['hunspell', '-d',affix_name,"-l"], 
			stdin=hunspell_cmd.stdout, stdout=subprocess.PIPE)
		#expected_output= subprocess.Popen(["cat", outputFile], stdout=subprocess.PIPE)
		expected_output = " "
		
		if hunspell_output.stdout.read().strip() == expected_output.strip():
			passed=passed+1
		else:
			failed=failed+1

		fileNum=fileNum+1
	# print "tests passed: ",passed
	# print "tests failed: ",failed

	return(passed,failed)

def test_answer():
    assert func(3) == 4
    language_list =  getLanguages()
    print(language_list)
    number_of_languages = getAffixes(language_list)
    passed,failed = call_hunspell(number_of_languages)

test_answer()