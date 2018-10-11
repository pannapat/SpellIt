from CouchAPI import CouchAPI
class User():

    def __init__(self):
        self.username = None
        self.paradigm_name = None
        self.lang_name = None
        self.user_data = None
        self.word = 'Word'
        self.skeleton = 'Skeleton'
        self.couch = CouchAPI('Administrator', 'password', 'localhost')
        self.couch.open_bucket('data')
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
        else:
            return {'Error': 'User not found'}

    ################################################################    GETTERS     ############################################################
    def get_user_languages(self):
        return  self.user_data.keys()

    def get_user_paradigms(self, lang_name):
        self.lang_name = lang_name
        pass

    def get_user_paradigm_words(self):
        pass

    def get_user_paradigm_words_data(self):
        pass

    def mapper(self):
        pass


    ################################################################    SETTERS     ############################################################
    def set_user_language(self):

        pass

    def set_user_paradigm(self):
        pass

    def set_user_paradigm_words(self):
        pass

    def set_user_paradigm_words_data(self):
        pass