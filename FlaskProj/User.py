from CouchAPI import CouchAPI
import json


class User():
    def __init__(self):
        self.username = ''
        self.paradigm_name = ''
        self.lang_name = ''
        self.user_data = {}
        self.lang_data = {}
        self.paradigm_data = {}
        self.word = 'Words'
        self.skeleton = 'Skeleton'
        self.couch = CouchAPI('Administrator', 'password', 'localhost')
        pass

    #      admin:{English:
    #                   {Noun:
    #                           Words:{walk:
    #                                   [walks, walking, walked],
    #                                  run:
    #                                   [runner, running, runs]
    #                           }

    #                           Skeleton:[root, singular, plural]
    #                           }
    #                       },
    #          }




    def validate_user(self, username, password):
        self.couch.open_bucket('auth')
        result = self.couch.authenticate(username, password)
        if result == True:
            self.username = username
            self.couch.open_bucket('data')
            try:
                self.user_data = self.couch.retrieve_data(self.username).value
            except:
                self.set_user(username)
        else:
            return {'Error': 'User not found'}

    ################################################################    GETTERS     ############################################################


    def get_user_languages(self):
        return list(self.user_data.keys())

    def get_user_paradigms(self, lang_name):
        self.lang_name = lang_name
        self.lang_data = self.user_data[self.lang_name]
        return list(self.lang_data.keys())



    def get_user_paradigm_words(self, paradigm_name):
        self.paradigm_name = paradigm_name
        self.paradigm_data = self.lang_data[paradigm_name]
        return list(self.paradigm_data[self.word].keys())



    def get_user_paradigm_words_data(self, word_name):
        word_forms = self.paradigm_data[self.word][word_name]
        paradigm_skeleton = self.paradigm_data[self.skeleton]
        word_data = self.mapper(word_name, word_forms, paradigm_skeleton)
        return word_data



    def get_user_paradigm_slots(self, paradigm_name):
        self.paradigm_name = paradigm_name
        self.paradigm_data = self.lang_data[paradigm_name]
        return self.paradigm_data[self.skeleton]
########################################################################        AFFIX FILE HELPERS      #############################################3

    def get_user_paradigms_aff_helper(self, lang_name):
        lang_data = self.user_data[lang_name]
        return list(lang_data.keys())

    def get_user_paradigm_words_aff_helper(self, paradigm_name):
        paradigm_data = self.lang_data[paradigm_name]
        return list(paradigm_data[self.word].keys())

    def get_user_paradigm_words_data_aff_helper(self, word_name, paradigm_name):
        paradigm_data = self.lang_data[paradigm_name]
        word_forms = paradigm_data[self.word][word_name]
        paradigm_skeleton = paradigm_data[self.skeleton]
        word_data = self.mapper(word_name, word_forms, paradigm_skeleton)
        return word_data

    def get_user_paradigm_slots_aff_helper(self, paradigm_name):
        paradigm_data = self.lang_data[paradigm_name]
        return paradigm_data[self.skeleton]

########################################################################        AFFIX FILE HELPERS END      #############################################3


    def get_user_data_by_language(self):
        paradigm_names = self.get_user_paradigms_aff_helper(self.lang_name)
        for paradigm in paradigm_names:
            paradigm_data = {}
            paradigm_data['paradigm_name'] = paradigm

            all_paradigm_words = self.get_user_paradigm_words_aff_helper(paradigm)

            words_data = []
            for word in all_paradigm_words:
                words_data.append(self.get_user_paradigm_words_data_aff_helper(word, paradigm))

            paradigm_data['words'] = words_data
            paradigm_data['slots'] = self.get_user_paradigm_slots_aff_helper(paradigm)
            # response_obj.append(paradigm_data)

            with open('/root/WebtoHunspell/affix-files/'+paradigm+'.json', 'w') as outfile:
                json.dump(paradigm_data, outfile)

    def mapper(self, root, forms, skeleton):
        resp = {}
        for x in range(1, len(skeleton)):
            resp[skeleton[x]] = forms[x-1]

        resp['root'] = root

        return resp

    ################################################################    SETTERS     ############################################################

    def set_user(self, username):
        self.couch.store_data(username, {})

    # Input Params lang_name -> string:English   |    DB Structure -> admin:{English: {}}
    def set_user_language(self, lang_name):

        if lang_name in self.user_data:
            return {'Error': 'Language already exists'}

        self.user_data[lang_name] = {}
        self.save_data()
        return True

    # Input Params   paradigm_name -> string:Noun       ,   paradigm_skeleton -> list:[root, singular, plural]  |    DB Structure -> admin:{English: {Noun: {Word: {}, Skeleton: [root, singular, plural]}}}
    def set_user_paradigm(self, paradigm_name, paradigm_skeleton):

        if paradigm_name in self.lang_data:
            return {'Error': 'Paradigm name already exists'}

        self.user_data[self.lang_name][paradigm_name] = {'Words': {}, 'Skeleton': paradigm_skeleton}
        self.save_data()
        return True

    # Input Params   paradigm_name -> string:Noun       ,   paradigm_skeleton -> list:[root, singular, plural]  |    DB Structure -> admin:{English: {Noun: {Word: {run: [runner, running, runs]},
    #                                                                                                                                       Skeleton: [root, singular, plural]}}}
    def set_user_paradigm_words(self, root_word, word_forms):

        if root_word in self.paradigm_data:
            return {'Error': 'Word already exits'}

        self.user_data[self.lang_name][self.paradigm_name][self.word][root_word] = word_forms
        self.save_data()
        return True

    def save_data(self):
        self.couch.store_data(self.username, self.user_data)


# user = User()
# user.validate_user('admin', 'admin')
# user.get_user_paradigms('English')
# user.get_user_paradigm_words('Verb')
# print(user.get_user_paradigm_words_data('jump'))
# print(user.get_user_data_by_language())
# print(user.set_user_language('English'))
# print(user.get_user_languages())
# print(user.get_user_paradigms('English'))
# print(user.set_user_paradigm('Verb', ['root', 'adjective', 'past', 'present']))
# print(user.get_user_paradigm_slots('Verb'))
# print(user.get_user_paradigm_words('Verb'))
# print(user.set_user_paradigm_words('jump', ['jumping','jumps', 'jumper']))
# print(user.get_user_paradigm_words_data('jump'))
