from CouchAPI import CouchAPI
class User():

    def __init__(self):
        self.username = None
        self.paradigm_name = None
        self.lang_name = None
        self.user_data = None
        self.lang_data = None

        self.word = 'Word'
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

    #                           Skeleton: {Noun:
    #                               [root, singular, plural]
    #                           }
    #                       },
    #          }




    def validate_user(self, username, password):
        self.couch.open_bucket('auth')
        result = self.couch.authenticate(username, password)
        if result == True:
            self.username = username
            self.user_data = self.couch.retrieve_data(self.username).value
            self.couch.open_bucket('data')
        else:
            return {'Error': 'User not found'}

    ################################################################    GETTERS     ############################################################


    def get_user_languages(self):
        return  self.user_data.keys()

    def get_user_paradigms(self, lang_name):
        self.lang_name = lang_name
        self.lang_data = self.user_data[lang_name]
        pass

    def get_user_paradigm_words(self, paradigm_name):
        self.paradigm_name = paradigm_name
        self.paradigm_data = self.lang_data[paradigm_name]
        pass

    def get_user_paradigm_words_data(self):
        pass

    def mapper(self):
        pass


    ################################################################    SETTERS     ############################################################

    #Input Params lang_name -> string:English   |    DB Structure -> admin:{English: {}}
    def set_user_language(self, lang_name):

        if lang_name in self.user_data:
            return {'Error': 'Language already exists'}

        self.user_data[lang_name] = {}
        self.save_data()


    #Input Params   paradigm_name -> string:Noun       ,   paradigm_skeleton -> list:[root, singular, plural]  |    DB Structure -> admin:{English: {Noun: {Word: {}, Skeleton: [root, singular, plural]}}}
    def set_user_paradigm(self, paradigm_name, paradigm_skeleton):

        if paradigm_name in self.lang_data:
            return {'Error': 'Paradigm name already exists'}

        self.user_data[self.lang_name][paradigm_name] = {'Words': {}, 'Skeleton': paradigm_skeleton}
        self.save_data()

    # Input Params   paradigm_name -> string:Noun       ,   paradigm_skeleton -> list:[root, singular, plural]  |    DB Structure -> admin:{English: {Noun: {Word: {run: [runner, running, runs]},
    #                                                                                                                                       Skeleton: [root, singular, plural]}}}
    def set_user_paradigm_words(self, root_word, word_forms):

        if root_word in self.paradigm_data:
            return {'Error': 'Word already exits'}

        self.user_data[self.lang_name][self.paradigm_name][self.word][root_word] = word_forms
        self.save_data()

    def save_data(self):
        self.couch.store_data(self.username, self.user_data)














