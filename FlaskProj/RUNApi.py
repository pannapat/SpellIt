# -*- coding: utf-8 -*-
import os
from flask import Flask, request, json
from flask_restful import Resource, Api
from flask_cors import CORS
from User import User
from subprocess import call

app = Flask(__name__)

CORS(app)

api = Api(app)



user = User()
user.validate_user('admin', 'admin')


#   Expected request object -> {language_name: English}
#   Possible result -> True if request is successful or {Error: Request failed due to (reason)}
class SaveLanguage(Resource):

    def post(self):

        request_data = json.loads(request.data.decode())
        lang_name = request_data['language_name']

        user.set_user_language(lang_name)

#   Expected request object -> {paradigm_name: Noun1, slots: [root, present form, past form, future form]}
#   Possible result -> True if request is successful or {Error: Request failed due to (reason)}
class SaveParadigm(Resource):

    def post(self):

        request_data = json.loads(request.data.decode())
        paradigm_name = request_data['paradigm_name']
        paradigm_skeleton = request_data['slots']

        user.set_user_paradigm(paradigm_name, paradigm_skeleton)

#   Expected request object -> {root_word: run, words: [ran, running, runs]}
#   Possible result -> True if request is successful or {Error: Request failed due to (reason)}
class SaveParadigmWords(Resource):

    def post(self):

        request_data = json.loads(request.data.decode())
        root_word = request_data['root_word']
        word_forms = request_data['words']

        user.set_user_paradigm_words(root_word, word_forms)

#   Expected request object -> Null
#   Possible result -> List of all the languages user have, if request is successful or if it fails -> {Error: Request failed due to (reason)}
#   Response Object -> {languages: [English, French, German, Italian]}
class GetLanguages(Resource):

    def post(self):

        user_languages = user.get_user_languages()
        resp_obj = {'languages': user_languages}

        return resp_obj

#   Expected request object -> {language_name: English}
#   Possible result -> List of all the paradigms user have, if request is successful or if it fails -> {Error: Request failed due to (reason)}
#   Response Object -> {paradigms: [Noun1, Verb1, Noun2, Verb2, Adjective3]}
class GetParadigms(Resource):

    def post(self):
        request_data = json.loads(request.data.decode())
        selected_language = request_data['language_name']

        user_paradigms = user.get_user_paradigms(selected_language)
        resp_obj = {'paradigms': user_paradigms}

        return resp_obj

#   Expected request object -> {paradigm_name: Noun1}
#   Possible result -> List of all the words in a paradigm, if request is successful or if it fails -> {Error: Request failed due to (reason)}
#   Response Object -> {paradigm_roots: [run, jump, build, kill, fly, eat]}
class GetParadigmWords(Resource):

    def post(self):
        request_data = json.loads(request.data.decode())
        selected_paradigm = request_data['paradigm_name']
        user_paradigm_words = user.get_user_paradigm_words(selected_paradigm)
        resp_obj = {'paradigm_roots': user_paradigm_words}

        return resp_obj

#   Expected request object -> {paradigm_root: run}
#   Possible result -> List of all the words forms mapped onto a selected paradigm, if request is successful or if it fails -> {Error: Request failed due to (reason)}
#   Response Object -> {word_data: {root: run, plural: runs, continous: running}}
#   PS: The skeleton of selected paradigm will map out onto all the words
class GetParadigmWordData(Resource):

    def post(self):
        request_data = json.loads(request.data.decode())
        selected_word = request_data['paradigm_root']
        user_word_data = user.get_user_paradigm_words_data(selected_word)
        resp_obj = {'word_data': user_word_data}

        return resp_obj

class GetParadigmSlots(Resource):

    def post(self):
        request_data = json.loads(request.data.decode())
        selected_paradigm = request_data['paradigm_name']
        paradigm_slots = user.get_user_paradigm_slots(selected_paradigm)

        resp_obj = {'paradigm_slots': paradigm_slots}

        return resp_obj

class GetAffixFile(Resource):

    def post(self):
    affix_dir = "/root/WebtoHunspell/affix-files"
    WEB_TO_HUNSPELL_PATH = 'root/WebtoHunspell'
    API_PATH = '/root/FlaskProj'
    [os.remove(os.path.join(affix_dir,jfile)) for jfile in os.listdir(affix_dir) if jfile.endswith(".json")]
        user.get_user_data_by_language()

        call_list = ["runghc", WEB_TO_HUNSPELL_PATH+"/WebtoHunspell.hs", WEB_TO_HUNSPELL_PATH+ "/affix-files/"]
        call(call_list)

        with open(API_PATH + '/out.aff', 'r') as affix_file:
            af = affix_file.read()

        with open(API_PATH + '/out.dic', 'r') as dic_file:

            dic = dic_file.read()

        return {'affix_file': af, 'dic_file': dic}

class DeleteWord(Resource):

    def post(self):
        request_data = json.loads(request.data.decode())
        root_word = request_data['root_word']
        user.delete_word(root_word)

class DeleteParadigm(Resource):

    def post(self):
        request_data = json.loads(request.data.decode())
        paradigm = request_data['paradigm_name']
        user.delete_paradigm(paradigm)

class DeleteLanguage(Resource):

    def post(self):
        request_data = json.loads(request.data.decode())
        lang_name = request_data['language_name']
        user.delete_language(lang_name)


api.add_resource(DeleteLanguage, '/delete-language')
api.add_resource(DeleteWord, '/delete-paradigm-word')
api.add_resource(DeleteParadigm, '/delete-paradigm')

api.add_resource(GetAffixFile, '/get-affix')
api.add_resource(GetParadigmSlots, '/paradigm-slots')
api.add_resource(GetParadigmWordData, '/word-form-list')
api.add_resource(GetParadigmWords,'/root-word-list')
api.add_resource(GetParadigms,'/paradigm-list')
api.add_resource(GetLanguages,'/language-list')

api.add_resource(SaveParadigmWords,'/add-paradigm-words')
api.add_resource(SaveParadigm,'/add-paradigm')
api.add_resource(SaveLanguage,'/add-language')

if __name__ == '__main__':
    app.run(debug=True, host='0.0.0.0',threaded = True)